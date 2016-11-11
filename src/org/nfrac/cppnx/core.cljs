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
   :nodes {1 :linear
           3 :gaussian
           4 :linear}
   :edges {1 {:d 1.0}
           3 {:y 1.0}
           4 {3 0.5
              :y -1.0}}
   :out-nodes {:h 1
               :s 4
               :v 3}
   :topology-hash 0})

(def all-node-types
  #{:linear :sine :gaussian :sigmoid :sawtooth})

(s/def ::node-id (-> any? (s/with-gen #(s/gen ident?))))
(s/def ::inputs (s/coll-of ::node-id, :min-count 1, :distinct true))
(s/def ::nodes (s/map-of ::node-id all-node-types, :min-count 1))
(s/def ::weight (s/double-in :min -100 :max 100 :NaN? false))
(s/def ::node-edges (s/map-of ::node-id ::weight))
(s/def ::edges (s/map-of ::node-id ::node-edges, :min-count 1))
(s/def ::out-nodes (s/map-of keyword? ::node-id, :min-count 1))
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

(defn remap
  "Transforms a map `m` applying function `f` to each value."
  [f m]
  (into (or (empty m) {})
        (map (fn [[k v]] [k (f v)]))
        m))

(defn cppn-graph
  [cppn]
  (graph/directed-graph (concat (:inputs cppn) (keys (:nodes cppn)))
                        (remap keys (:edges cppn))))

(defn cppn-strata
  [cppn]
  (graph/dependency-list (cppn-graph cppn)))

(defn downstream
  "Returns the collection of downstream nodes including self."
  [cppn node-id]
  (let [gr (cppn-graph cppn)]
    (-> (graph/reverse-graph gr)
        (graph/transitive-closure)
        (graph/add-loops)
        (graph/get-neighbors node-id))))

(defn mutate-append-node
  [cppn]
  (let [types all-node-types
        type (rand-nth (seq types))
        id (keyword (gensym "node"))
        [output onode] (rand-nth (seq (:out-nodes cppn)))]
    (-> cppn
        (update :nodes assoc id type)
        (update :edges assoc id {onode 1.0})
        (update :out-nodes assoc output id)
        (update :topology-hash inc))))

(defn mutate-add-conn
  [cppn]
  (let [[to-node to-edges] (rand-nth (seq (:edges cppn)))
        candidates (remove (into (set (keys to-edges))
                                 (downstream cppn to-node))
                           (concat (keys (:nodes cppn)) (:inputs cppn)))]
    (if (seq candidates)
      (-> cppn
          (assoc-in [:edges to-node (rand-nth (seq candidates))] 1.0)
          (update :topology-hash inc))
      cppn)))

(defn mutate-rewire-conn
  [cppn]
  (let [[to-node to-edges] (rand-nth (seq (:edges cppn)))
        [rm-from w] (rand-nth (seq to-edges))
        candidates (remove (into (set (keys to-edges))
                                 (downstream cppn to-node))
                           (concat (keys (:nodes cppn)) (:inputs cppn)))]
    (if (seq candidates)
      (-> cppn
          (update-in [:edges to-node] dissoc rm-from)
          (assoc-in [:edges to-node (rand-nth (seq candidates))] w)
          (update :topology-hash inc))
      cppn)))

(defn mutate-rewire-output
  [cppn]
  (let [[output onode] (rand-nth (seq (:out-nodes cppn)))
        node (rand-nth (keys (dissoc (:nodes cppn) onode)))]
    (-> cppn
        (assoc-in [:out-nodes output] node)
        (update :topology-hash inc))))

(defn mutate-remove-unused
  [cppn])

(defn rand-skew
  [max power]
  (-> (rand (Math/pow max (/ 1 power)))
      (Math/pow power)))

(defn rand-sign [] (if (pos? (rand-int 2)) 1 -1))

(defn randomise-weights
  [cppn]
  (let [edge-list (for [[from m] (:edges cppn)
                        [to w] m]
                    [from to])]
    (reduce (fn [cppn edge]
              (assoc-in cppn (into [:edges] edge)
                        (* (rand-skew 10 2) (rand-sign))))
            cppn
            edge-list)))

(def waypoints
  (let [xs [0.2 0.5 1.0 2.5 10.0]]
    (vec (concat xs (map - xs)))))

(defn smooth-step
  [z]
  (let [z (-> z (max 0.0) (min 1.0))]
    (* z z (- 3 (* 2 z)))))

(defn smoother-step
  [z]
  (let [z (-> z (max 0.0) (min 1.0))]
    (* z z z (+ 10 (* z (- (* z 6) 15))))))

(defn interp
  [from to z]
  (+ from (* z (- to from))))

(defn init-1d-motion
  [cppn]
  (let [edge-list (for [[from m] (:edges cppn)
                        [to w] m]
                    [from to])
        selected-edge (rand-nth edge-list)
        origin-w (get-in (:edges cppn) selected-edge)
        target-w (rand-nth waypoints)]
    {:edge selected-edge
     :origin-w origin-w
     :target-w target-w
     :value origin-w
     :time-fraction 0.0}))

(defn step-1d-motion
  [motion time-inc]
  (let [{:keys [origin-w target-w time-fraction]} motion
        new-time (+ time-fraction time-inc)]
    (assoc motion
           :value (interp origin-w target-w (smooth-step new-time))
           :time-fraction new-time
           :done? (>= new-time 1.0))))

(defn init-isolated-weights-tour
  [cppn]
  (assoc cppn
         :tour [(init-1d-motion cppn)]))

(defn init-pair-weights-tour
  [cppn]
  (assoc cppn
         :tour [(init-1d-motion cppn)
                (assoc (init-1d-motion cppn) :done? true)]))

(defn apply-tour
  [cppn tour]
  (reduce (fn [cppn motion]
            (assoc-in cppn (into [:edges] (:edge motion))
                      (:value motion)))
          cppn
          tour))

(defn step-weights-tour
  [cppn dt]
  (let [concurrency (count (:tour cppn))
        ;; replace any completed motions with new random ones
        ongoing (remove :done? (:tour cppn))
        ntour (reduce (fn [tour i]
                        (let [m (init-1d-motion cppn)]
                          ;; make sure not moving the same parameter twice
                          (if (contains? (set (map :edge tour)) (:edge m))
                            (recur tour i)
                            (conj tour m))))
                      ongoing
                      (range (- concurrency (count ongoing))))
        tour (map #(step-1d-motion % dt) ntour)]
    (-> (apply-tour cppn tour)
        (assoc :tour tour))))

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
