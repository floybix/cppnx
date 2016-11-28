(ns org.nfrac.cppnx.share
  (:require [org.nfrac.cppnx.core :as cppnx :refer [remap]]
            [cljsjs.oauthio]
            [clojure.string :as str]
            [cljs.reader :refer [read-string]]
            [clojure.walk :refer [postwalk]]
            [goog.Uri]
            [goog.Uri.QueryData]))

(comment
 js/OAuth)

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
            (clj->js {"cppn" (cppn->uristr cppn)}))]
    (.setQueryData uri qd)
    (str uri)))
