(ns org.nfrac.cppnx.core
  (:require [gamma.api :as g]
            [org.nfrac.cppnx.util.algo-graph :as graph]
            [clojure.spec :as s]
            [clojure.spec.impl.gen :as gen]))

;; network topology is defined by dependencies between nodes.
;; so all non-input nodes must have at least one input edge.
;; and cycles are not allowed.
(def example-cppn
  {:inputs [:bias :x :y :d]
   :nodes {3 :gaussian
           4 :linear}
   :edges {3 {:y 1.0}
           4 {3 0.5
              :y -1.0}}
   :out-nodes {:h :d
               :s 4
               :v 3}
   :topology-hash 0})

(def all-node-types
  #{:linear :sine :gaussian :sigmoid :sawtooth})

(s/def ::node-id (-> any? (s/with-gen #(s/gen ident?))))
(s/def ::inputs (s/coll-of ::node-id, :min-count 1, :distinct true))
(s/def ::nodes (s/map-of ::node-id all-node-types))
(s/def ::weight (s/double-in :min -100 :max 100 :NaN? false))
(s/def ::node-edges (s/map-of ::node-id ::weight))
(s/def ::edges (s/map-of ::node-id ::node-edges))
(s/def ::out-nodes (s/map-of keyword? ::node-id))
(s/def ::topology-hash int?)

(s/def ::cppn
  (s/keys :req-un [::inputs
                   ::nodes
                   ::edges
                   ::out-nodes
                   ::topology-hash]))

;;; cppns

(defmulti node-glsl
  (fn [type sum weight]
    type))

(defmethod node-glsl :linear
  [_ sum weight]
  (g/div sum weight))

(defmethod node-glsl :sine
  [_ sum _]
  (g/sin (g/* sum (* 3.14 2))))

(defmethod node-glsl :gaussian
  [_ sum _]
  (-> (g/* sum 2.5)
      (g/pow 2.0)
      (g/* -1.0)
      (g/exp)
      (g/* 2.0)
      (g/- 1.0)))

(defmethod node-glsl :sigmoid
  [_ sum _]
  (->
   (g/div
    1.0
    (-> (g/* sum -4.9)
        (g/exp)
        (g/+ 1.0)
        (g/div 1.0)))
   (g/* 2.0)
   (g/- 1.0)))

(defmethod node-glsl :sawtooth
  [_ sum _]
  (-> (g/fract sum)
      (g/* 2.0)
      (g/- 1.0)))

;;; cppn wrangling

(defn mutate-add-node
  [cppn])

(defn mutate-add-conn
  [cppn])

(defn mutate-rewire-conn
  [cppn])

(defn mutate-rewire-output
  [cppn])

(defn mutate-remove-unused
  [cppn])

(defn randomise-weights
  [cppn])

(defn init-weights-tour
  [cppn]
  {:phases
   :wavelengths})

(defn step-weights-tour
  [tour])

(defn with-weights-tour
  [cppn tour])



(defn remap
  "Transforms a map `m` applying function `f` to each value."
  [f m]
  (into (or (empty m) {})
        (map (fn [[k v]] [k (f v)]))
        m))

(defn cppn-strata
  [cppn]
  (let [gr (graph/directed-graph (concat (:inputs cppn) (keys (:nodes cppn)))
                                 (remap keys (:edges cppn)))]
    (graph/dependency-list gr)))

(defn build-cppn-vals
  [cppn in-exprs]
  (let [strata (cppn-strata cppn)
        sorted-nids (apply concat (rest strata))
        node-exprs (reduce (fn [m nid]
                             (let [node-type (get-in cppn [:nodes nid])
                                   ideps (get-in cppn [:edges nid])
                                   sum (->> ideps
                                            (map (fn [[from-id w]]
                                                   (g/* w (get m from-id))))
                                            (reduce g/+))
                                   sumw (reduce g/+ (map g/abs (vals ideps)))
                                   expr (node-glsl node-type sum sumw)]
                                (assoc m nid expr)))
                           in-exprs
                           ;; topological sort
                           sorted-nids)]
    (remap node-exprs (:out-nodes cppn))))
