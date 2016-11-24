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
   :nodes {:init :gaussian}
   :edges {:init {:d 1.0
                  :y 1.0}
           :h {:init 1.0}
           :s {:init 0.5
               :x -1.0}
           :v {:init 1.0}}})

(def all-node-types
  #{:linear :sine :gaussian :sigmoid :sawtooth})

(s/def ::node-id (-> any? (s/with-gen #(s/gen ident?))))
(s/def ::inputs (s/coll-of ::node-id, :min-count 1, :kind set?))
(s/def ::outputs (s/coll-of ::node-id, :min-count 1, :kind set?))
(s/def ::nodes (s/map-of ::node-id all-node-types, :min-count 1))
(s/def ::weight (s/double-in :min -100 :max 100 :NaN? false))
(s/def ::node-edges (s/map-of ::node-id ::weight))
(s/def ::edges (s/map-of ::node-id ::node-edges, :min-count 1))

(s/def ::cppn
  (s/keys :req-un [::inputs
                   ::outputs
                   ::nodes
                   ::edges]))

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
        (g/+ 1.0)))
   (g/* 2.0)
   (g/- 1.0)))

(defmethod node-glsl :sawtooth
  [_ sum _]
  (let [a (g/fract sum)
        ;; could use just 'fract' but let's do a little smoothing
        peak 0.975
        truncd (g/min a peak)
        over (g/- a truncd)]
    (-> (g/- truncd (g/* over (/ peak (- 1.0 peak))))
        (g/* 2.0)
        (g/- 1.0))))

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
  "A list of sets. The first set contains the only inputs, the last only the
  outputs."
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

(defn edge-list
  [cppn]
  (sort
   (for [[to m] (:edges cppn)
         from (keys m)]
    [to from])))

(defn cppn-weights
  [cppn]
  (mapv #(get-in (:edges cppn) %) (edge-list cppn)))

(defn set-cppn-weights
  [cppn ws]
  (reduce (fn [cppn [[to from] w]]
            (assoc-in cppn [:edges to from] w))
          cppn
          (map vector (edge-list cppn) ws)))

(defn use-indices
  [cppn use-node]
  (keep-indexed (fn [i [to from]]
                  (when (or (nil? use-node)
                            (= to use-node))
                        i))
                (edge-list cppn)))

(defn mutate-append-node
  [cppn]
  (let [types all-node-types
        type (rand-nth (seq types))
        id (keyword (gensym "n"))
        to1 (rand-nth (seq (:outputs cppn)))
        [from1 w1] (rand-nth (seq (get-in cppn [:edges to1])))]
    (-> cppn
        (update :nodes assoc id type)
        (update :edges assoc id {from1 w1})
        (update-in [:edges to1] dissoc from1)
        (update-in [:edges to1] assoc id 1.0))))

(defn mutate-add-conn
  [cppn]
  (let [[to-node to-edges] (rand-nth (seq (:edges cppn)))
        candidates (remove (into (set (keys to-edges))
                                 (downstream cppn to-node))
                           (concat (keys (:nodes cppn)) (:inputs cppn)))]
    (if (seq candidates)
      (-> cppn
          (assoc-in [:edges to-node (rand-nth (seq candidates))] 1.0))
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
          (assoc-in [:edges to-node (rand-nth (seq candidates))] w))
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

(defn remove-edge
  [cppn from to]
  (update-in cppn [:edges from]
             (fn [m]
               (let [m (dissoc m to)]
                 ;; ensure all nodes have at least one input
                 (if (empty? m)
                   (assoc m (rand-nth (seq (:outputs cppn))) 1.0)
                   m)))))

(defn rand-skew
  [max power]
  (-> (rand (Math/pow max (/ 1 power)))
      (Math/pow power)))

(defn rand-sign [] (if (pos? (rand-int 2)) 1 -1))

(defn interp
  [from to z]
  (+ from (* z (- to from))))

(defn rand-weight
  [from-w perturbation]
  (println "perturbation" perturbation from-w)
  (let [global-w (* (rand-skew 12 3) (rand-sign))
        locally (+ from-w (* perturbation 0.5 global-w))
        globally (interp from-w global-w (* perturbation perturbation))]
    (+ (* perturbation globally)
       (* (- 1.0 perturbation) locally))))

(defn randomise-weights
  [cppn perturbation use-node]
  (let [ws (cppn-weights cppn)
        use-is (use-indices cppn use-node)
        new-ws (reduce (fn [ws i]
                         (assoc ws i (rand-weight (nth ws i) perturbation)))
                       ws
                       use-is)]
    (set-cppn-weights cppn new-ws)))

;;; weight-space tours

(defn smooth-step
  [z]
  (let [z (-> z (max 0.0) (min 1.0))]
    (* z z (- 3 (* 2 z)))))

(defn smoother-step
  [z]
  (let [z (-> z (max 0.0) (min 1.0))]
    (* z z z (+ 10 (* z (- (* z 6) 15))))))

(defn motion
  [index from-val to-val]
  {:index index
   :from-val from-val
   :to-val to-val})

(defn rand-motion
  [ws index perturbation]
  (let [from-val (nth ws index)
        to-val (rand-weight from-val perturbation)]
    (motion index from-val to-val)))

(defn motion-at
  [motion time-frac]
  (let [{:keys [from-val to-val]} motion]
    (interp from-val to-val (smooth-step time-frac))))

(defn init-weights-tour
  [cppn concurrency perturbation use-node]
  (let [ws (cppn-weights cppn)
        use-is (use-indices cppn use-node)
        is (take concurrency (shuffle use-is))]
    {:weights ws
     :use-indices use-is
     :perturbation perturbation
     :motion-frac 0.0
     :motions (mapv #(rand-motion ws % perturbation) is)
     :waypoints (list ws)}))

(defn apply-motions
  [ws motions motion-frac]
  (reduce (fn [ws m]
            (assoc ws (:index m) (motion-at m motion-frac)))
          ws
          motions))

(defn step-weights-tour
  [tour dt]
  (let [concurrency (count (:motions tour))
        perturbation (:perturbation tour)]
    (if (>= (:motion-frac tour) 1.0)
      ;; record waypoint and choose new motions
      (let [ws (:weights tour)
            is (take concurrency (shuffle (:use-indices tour)))]
        (-> tour
            (update :waypoints conj (:weights tour))
            (assoc :motion-frac 0.0)
            (assoc :motions (mapv #(rand-motion ws % perturbation) is))))
      ;; just a step
      (let [t (+ (:motion-frac tour) dt)
            ws (apply-motions (:weights tour) (:motions tour) t)]
        (-> tour
            (assoc :weights ws)
            (assoc :motion-frac t))))))

(defn build-cppn-vals
  [cppn in-exprs w-exprs]
  (let [strata (cppn-strata cppn)
        sorted-nids (apply concat (rest strata))
        node-exprs (reduce (fn [m nid]
                             (let [node-type (get-in cppn [:nodes nid] :linear)
                                   deps (keys (get-in cppn [:edges nid]))
                                   sum (->> deps
                                            (map (fn [k]
                                                   (let [w (get w-exprs [nid k])]
                                                     (g/* w (get m k)))))
                                            (reduce g/+))
                                   sumw (->> deps
                                             (map (fn [k]
                                                    (g/abs (get w-exprs [nid k]))))
                                             (reduce g/+))
                                   expr (node-glsl node-type sum sumw)]
                                (assoc m nid expr)))
                           in-exprs
                           ;; topological sort
                           sorted-nids)]
    node-exprs))
