(ns org.nfrac.cppnx.compile-webgl
  (:require [org.nfrac.cppnx.core :as cppnx]
            [clojure.string :as str]))

(def hsv2rgb-glsl
  "
vec3 hsv2rgb(vec3 c)
{
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}")

(def xytod-glsl
  "
float xytod(float x, float y) {
  return sqrt((x * x) + (y * y));
}
")

(def node-fns-glsl
  "
float sine(float x) {
  return sin(x * (3.1415 * 2.0));
}
float gaussian(float x) {
  return exp(- pow(x * 2.5, 2.0));
}
float sigmoid(float x) {
  return 1.0 / (1.0 + exp(x * -4.9));
}
float sawtooth(float x) {
  return mod(x, 1.0);
}
float tanh(float x) {
  float e = exp(2.0 * x);
  return (e - 1.0) / (e + 1.0);
}
")

(defn cppn-glsl-assigns
  "Returns a list of local assignments as [name expr] string pairs to be
  included in a glsl function. Assumes all :input syms are defined.
  Weights: w-exprs should be a map from edge [to from] to glsl string."
  [cppn w-exprs]
  (let [strata (cppnx/cppn-strata cppn)
        zerod? (or (:zerod cppn) #{})
        sorted-nids (apply concat (rest strata))]
    (->>
     sorted-nids
     (map (fn [nid]
           (let [node-type (get-in cppn [:nodes nid] :linear)
                 deps (keys (get-in cppn [:edges nid]))
                 sum (->> deps
                          (map (fn [k]
                                 (let [w (get w-exprs [nid k])]
                                   (str "(" w " * " (name k) ")"))))
                          (str/join " + "))
                 expr (cond
                        (zerod? nid)
                        "0.0"
                        (= :linear node-type)
                        sum
                        :else
                        (str (name node-type) "(" sum ")"))]
             [(name nid) expr]))))))

(defn assigns->glsl
  [assigns]
  (->> assigns
       (map (fn [[id expr]]
              (str "float " id " = " expr ";")))
       (str/join \newline)))
