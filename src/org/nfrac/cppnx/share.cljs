(ns org.nfrac.cppnx.share
  (:require [cljsjs.oauthio]
            [clojure.string :as str]
            [cljs.reader :refer [read-string]]
            [clojure.walk]
            [goog.Uri]
            [goog.Uri.QueryData]))

(comment
 js/OAuth)

(defn keywordize-syms
  [m]
  (clojure.walk/postwalk (fn [x] (if (symbol? x) (keyword (name x)) x)) m))

(defn symbolize-kws
  [m]
  (clojure.walk/postwalk (fn [x] (if (keyword? x) (symbol (name x)) x)) m))

(defn cppn->uristr
  [cppn]
  (-> cppn
      (symbolize-kws)
      (pr-str)
      (str/replace #",? " "_")))

(defn uristr->cppn
  [s]
  (-> s
      (str/replace #"_" " ")
      (read-string)
      (keywordize-syms)))

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
