(ns org.nfrac.cppnx.core
  (:require [gamma.api :as g]
            [org.nfrac.cppnx.util.algo-graph :as graph]
            [clojure.spec :as s]
            [clojure.spec.impl.gen :as gen]))

;; network topology is defined by dependencies between nodes.
;; so all non-input nodes must have at least one input edge.
;; and cycles are not allowed.
(def example-cppn
  {:inputs #{:bias :x :y :d}
   :outputs #{:h :s :v}
   :nodes {:na :linear
           :nb :gaussian
           :nc :sine
           :nd :sigmoid}
   :edges {:na {:d 1.0}
           :nb {:y 1.0}
           :nc {:bias 1.0}
           :nd {:nc 0.1}
           :h {:na 1.0}
           :s {:nb 0.5
               :y -1.0}
           :v {:nb 1.0}}
   :topology-hash 0})

(def all-node-types
  #{:linear :sine :gaussian :sigmoid :sawtooth})

(s/def ::node-id (-> any? (s/with-gen #(s/gen ident?))))
(s/def ::inputs (s/coll-of ::node-id, :min-count 1, :kind set?))
(s/def ::outputs (s/coll-of ::node-id, :min-count 1, :kind set?))
(s/def ::nodes (s/map-of ::node-id all-node-types, :min-count 1))
(s/def ::weight (s/double-in :min -100 :max 100 :NaN? false))
(s/def ::node-edges (s/map-of ::node-id ::weight))
(s/def ::edges (s/map-of ::node-id ::node-edges, :min-count 1))
(s/def ::topology-hash int?)

(s/def ::cppn
  (s/keys :req-un [::inputs
                   ::outputs
                   ::nodes
                   ::edges
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
  (graph/directed-graph (concat (:inputs cppn)
                                (keys (:nodes cppn))
                                (:outputs cppn))
                        (remap keys (:edges cppn))))

(defn cppn-graph-no-outputs
  [cppn]
  (graph/directed-graph (concat (:inputs cppn)
                                (keys (:nodes cppn)))
                        (remap keys (apply dissoc (:edges cppn) (:outputs cppn)))))

(defn cppn-strata
  "A list of sets. The first set contains the inputs, the last
  the outputs."
  [cppn]
  (concat (graph/dependency-list (cppn-graph-no-outputs cppn))
          [(:outputs cppn)]))

(defn downstream
  "Returns the collection of all downstream nodes including self."
  [cppn node-id]
  (-> (cppn-graph cppn)
      (graph/reverse-graph)
      (graph/transitive-closure)
      (graph/add-loops)
      (graph/get-neighbors node-id)))

(defn mutate-append-node
  [cppn]
  (let [types all-node-types
        type (rand-nth (seq types))
        id (keyword (gensym "node"))
        to1 (rand-nth (seq (:outputs cppn)))
        [from1 w1] (rand-nth (seq (get-in cppn [:edges to1])))]
    (-> cppn
        (update :nodes assoc id type)
        (update-in [:edges to1] dissoc from1)
        (update :edges assoc id {from1 w1})
        (update :edges assoc to1 {id 1.0})
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

(defn delete-node
  [cppn node]
  (let [above (keys (get-in cppn [:edges node]))
        below (-> (cppn-graph cppn)
                  (graph/reverse-graph)
                  (graph/get-neighbors node))]
    (->
     (reduce (fn [m below-node]
               (let [w (get-in m [:edges below-node node])]
                 (update-in m [:edges below-node]
                            #(-> (merge (zipmap above (repeat w)) %)
                                 (dissoc node)))))
             cppn
             below)
     (update :edges dissoc node)
     (update :nodes dissoc node))))

(defn link-nodes
  "Attempt to link node a -> b,
   but if that would be cyclic, link b -> a instead."
  [cppn node-a node-b]
  (if (or (contains? (:outputs cppn) node-a)
          (contains? (:inputs cppn) node-b)
          (contains? (downstream cppn node-b) node-a))
    (assoc-in cppn [:edges node-a node-b] 1.0)
    (assoc-in cppn [:edges node-b node-a] 1.0)))

(defn rand-skew
  [max power]
  (-> (rand (Math/pow max (/ 1 power)))
      (Math/pow power)))

(defn rand-sign [] (if (pos? (rand-int 2)) 1 -1))

(defn rand-weight
  []
  (* (rand-skew 12 2.5) (rand-sign)))

(defn randomise-weights
  [cppn]
  (let [edge-list (for [[from m] (:edges cppn)
                        [to w] m]
                    [from to])]
    (reduce (fn [cppn edge]
              (assoc-in cppn (into [:edges] edge)
                        (rand-weight)))
            cppn
            edge-list)))

;;; weight-space tours

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
        target-w (rand-weight)]
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
                             (let [node-type (get-in cppn [:nodes nid] :linear)
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
    node-exprs))
