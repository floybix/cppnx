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
  [cppn]
  (let [uri (goog.Uri. (.-location js/window))
        qd (goog.Uri.QueryData/createFromMap
            #js {:cppn (cppn->uristr cppn)})]
    (.setQueryData uri qd)
    (str uri)))

(defonce init-oauth
  (.initialize js/OAuth "tX7bPIbYnjeE5Y2ZrdToloUbE"))

(defn data-uri->blob
  [data-uri]
  (let [[intro blah] (str/split data-uri #"," 2)
        bytestr (if (re-matches #"base64" intro)
                  (.atob js/window blah)
                  (.decodeURI blah))
        [mime _] (str/split intro #";" 2)
        ia (js/Uint8Array. (.-length bytestr))]
    (doseq [i (range (.-length bytestr))]
      (aset ia i (.charCodeAt bytestr i)))
    (js/Blob. (array ia) #js {:type mime})))

(def post-tweet-url
  "/1.1/statuses/update.json")

(defn tweet!
  [cppn]
  (let [params #js {} #_{:cache true}]
    (-> (.popup js/OAuth "twitter" params)
        (.done
         (fn [twitter]
           (let [tweet #js {:status (str "Hello world."
                                         (uri-with-cppn cppn))}]
             ;; media_ids
             (-> twitter
                 (.post post-tweet-url
                        #js {:data tweet})
                 (.done
                  (fn [data]
                    (println "tweeted!" (js->clj data))))
                 (.fail
                  (fn [err]
                    (println "tweet failed:" (js->clj err))))))))
        (.fail
         (fn [err]
           (println "oauth failed:" (js->clj err)))))))
