(ns org.nfrac.cppnx.share
  (:require [org.nfrac.cppnx.core :as cppnx :refer [remap]]
            [cljsjs.oauthio]
            [clojure.string :as str]
            [cljs.reader :refer [read-string]]
            [clojure.walk :refer [postwalk]]
            [goog.Uri]
            [goog.Uri.QueryData]))

(def node-type-encode
  {:linear :l
   :gaussian :g
   :sine :s
   :sigmoid :S
   :sawtooth :w})

(def node-type-decode
  (zipmap (vals node-type-encode) (keys node-type-encode)))

(defn keywordize-syms
  [m]
  (postwalk (fn [x] (if (symbol? x) (keyword (name x)) x)) m))

(defn symbolize-kws
  [m]
  (postwalk (fn [x] (if (keyword? x) (symbol (name x)) x)) m))

(defn cppn->uristr
  [cppn]
  (-> cppn
      (update :nodes (fn [m] (remap #(or (node-type-encode %) %) m)))
      (symbolize-kws)
      (pr-str)
      (str/replace #",? " "_")))

(defn uristr->cppn
  [s]
  (-> s
      (str/replace #"_" " ")
      (read-string)
      (keywordize-syms)
      (update :nodes (fn [m] (remap #(or (node-type-decode %) %) m)))))

(defn get-uri-cppn
  []
  (let [uri (goog.Uri. (.-location js/window))
        qd (.getQueryData uri)
        tc (.get qd "cppn")]
    (when tc
      (uristr->cppn tc))))

(defn uri-with-cppn
  [cppn*]
  (let [cppn (dissoc cppn* :inputs :outputs) ;; inferred from :domain
        uri (goog.Uri. (.-location js/window))
        qd (goog.Uri.QueryData/createFromMap
            #js {:cppn (cppn->uristr cppn)})]
    (.setQueryData uri qd)
    (str uri)))

(defonce init-oauth
  (.initialize js/OAuth "G8cnIzClY3nbnOniOAQzkHvMcfE"))

(defn data-uri->blob
  [data-uri]
  (let [[intro blah] (str/split data-uri #"," 2)
        bytestr (if (re-find #"base64" intro)
                  (.atob js/window blah)
                  (.decodeURI blah))
        [mime _] (str/split intro #";" 2)
        ia (js/Uint8Array. (.-length bytestr))]
    (doseq [i (range (.-length bytestr))]
      (aset ia i (.charCodeAt bytestr i)))
    (js/Blob. (array ia) #js {:type mime})))

(def post-status-url "/1.1/statuses/update.json")
(def post-media-url "https://upload.twitter.com/1.1/media/upload.json")

(defn tweet-uri [info]
  (str "https://twitter.com/" (:screen-name info) "/status/" (:id-str info)))

(defn parse-tweet-uri [uri]
  (let [[_ a b] (re-find #"twitter.com/(.+)/status/([0-9]+)$" uri)]
    (when (and a b)
      {:screen-name a, :id-str b})))

(defn ensure-contains-handle
  [text reply-to]
  (if-let [handle (:screen-name reply-to)]
    (if (or (str/blank? handle)
            (str/includes? text (str "@" handle))
            (>= (.-length (str handle text)) 113))
      text
      (str "@" handle " " (str/trim text)))
    text))

(defn tweet!
  "On success, calls tweet-callback with info on tweet just posted,
  {:id-str, :screen-name}"
  [cppn img-data text reply-to-uri tweet-callback
   include-url? include-img?]
  (let [reply-to (when reply-to-uri (parse-tweet-uri reply-to-uri))
        twitext (ensure-contains-handle text reply-to)]
    (->
     (.popup js/OAuth "twitter" #js {:cache true})
     (.done
      (fn [twitter]
        (let [payload (doto (js/FormData.)
                        (.append "media" (data-uri->blob img-data)))]
          (-> twitter
              (.post post-media-url
                     #js {:data payload
                          :cache false
                          :processData false
                          :contentType false})
              (.done
               (fn [data]
                 (println "uploaded!" (js->clj data))
                 (let [data (js->clj data)
                       mid (get data "media_id_string")
                       tweet (clj->js
                              (cond->
                               {:status (if include-url?
                                          (str twitext " " (uri-with-cppn cppn))
                                          twitext)}
                               include-img?
                               (assoc :media_ids mid)
                               reply-to
                               (assoc :in_reply_to_status_id (:id-str reply-to))))]
                   (-> twitter
                       (.post post-status-url
                              #js {:data tweet})
                       (.done
                        (fn [data]
                          (println "tweeted!" (js->clj data))
                          (let [data (js->clj data)
                                m {:id-str (get data "id_str")
                                   :screen-name (get-in data ["user" "screen_name"])}]
                            (tweet-callback m))))
                       (.fail
                        (fn [err]
                          (.alert js/window "Tweet failed, see js console.")))))))
              (.fail
               (fn [err]
                 (.alert js/window "Upload failed, see js console.")))))))
     (.fail
       (fn [err]
         (.alert js/window "OAuth failed, see js console."))))))
