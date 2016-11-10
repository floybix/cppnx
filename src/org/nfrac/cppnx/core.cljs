(ns org.nfrac.cppnx.core
  (:require [gamma.api :as g]
            [org.nfrac.cppnx.util.algo-graph :as graph]))

;;; cppns

(def all-node-types
  #{:linear :sine :gaussian})

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

;; network topology is defined by dependencies between nodes.
;; so all non-input nodes must have at least one input edge.
;; and cycles are not allowed.
(def example-cppn
  {:inputs [:bias :x :y :d]
   :nodes {3 :gaussian
           4 :linear}
   :deps {3 {:y 1.0}
          4 {3 0.5
             :y -1.0}}
   :out-nodes {:h :d
               :s 4
               :v 3}
   :topology-hash 0})

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
                                 (remap keys (:deps cppn)))]
    (graph/dependency-list gr)))

(defn build-cppn-vals
  [cppn in-exprs]
  (let [strata (cppn-strata cppn)
        sorted-nids (apply concat (rest strata))
        node-exprs (reduce (fn [m nid]
                             (let [node-type (get-in cppn [:nodes nid])
                                   ideps (get-in cppn [:deps nid])
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
