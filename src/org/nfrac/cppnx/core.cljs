(ns org.nfrac.cppnx.core
  (:require [org.nfrac.cppnx.util.algo-graph :as graph]
            [clojure.spec :as s]
            [clojure.spec.impl.gen :as gen]))

;; network topology is defined by dependencies between nodes.
;; so all non-input nodes must have at least one input edge.
;; and cycles are not allowed.
(def example-cppn
  {:inputs #{:bias :x :y :d}
   :outputs #{:h :s :v}
   :nodes {:i0 :gaussian}
   :edges {:i0 {:d 1.0
                :y 1.0}
           :h {:i0 1.0}
           :s {:i0 0.5
               :x -1.0}
           :v {:i0 1.0}}})

(def all-node-types
  #{:linear :sine :gaussian :sigmoid :sawtooth})

(def auto-node-types
  (disj all-node-types :sawtooth))

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

(def node-prefixes (seq "nmpqrstbcdefgh"))

(defn gen-node-id
  "Scan through integer range until find usable id.
  Instead of gensym so as to keep the ids small < 100
  and avoid collisions with externally provided nodes."
  [cppn]
  (loop [pres node-prefixes
         i 0]
    (let [pre (first pres)
          k (keyword (str pre i))]
      (if (>= i 100)
        (recur (rest pres) 0)
        (if (contains? (:nodes cppn) k)
          (recur pres (inc i))
          k)))))

(defn mutate-append-node
  [cppn]
  (let [type (rand-nth (seq auto-node-types))
        id (gen-node-id cppn)
        to1 (rand-nth (seq (:outputs cppn)))
        [from1 w1] (rand-nth (seq (get-in cppn [:edges to1])))]
    (-> cppn
        (update :nodes assoc id type)
        (update :edges assoc id {from1 w1})
        (update-in [:edges to1] dissoc from1)
        (update-in [:edges to1] assoc id 1.0))))

(defn mutate-add-conn-to
  [cppn to-node]
  (let [to-edges (get (:edges cppn) to-node)
        candidates (remove (into (set (keys to-edges))
                                 (downstream cppn to-node))
                           (concat (keys (:nodes cppn)) (:inputs cppn)))
        w (- (rand (* 3.0 2)) 3.0)]
    (if (seq candidates)
      (-> cppn
          (assoc-in [:edges to-node (rand-nth (seq candidates))] w))
      cppn)))

(defn mutate-add-conn
  [cppn]
  (mutate-add-conn-to cppn (rand-nth (keys (:edges cppn)))))

(defn mutate-rewire-conn
  [cppn]
  (let [[to-node to-edges] (rand-nth (seq (:edges cppn)))
        [rm-from old-w] (rand-nth (seq to-edges))
        cppn2 (mutate-add-conn-to cppn to-node)
        new-from (-> (apply dissoc (get-in cppn2 [:edges to-node])
                            (keys to-edges))
                     keys first)]
    (if new-from
      (-> cppn2
          (update-in [:edges to-node] dissoc rm-from)
          (assoc-in [:edges to-node new-from] old-w))
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
  (update-in cppn [:edges to]
             (fn [m]
               (let [m (dissoc m from)]
                 ;; ensure all nodes have at least one input
                 (if (empty? m)
                   (assoc m (rand-nth (seq (:inputs cppn))) 1.0)
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
  (let [global-w (* (rand-skew 8 3) (rand-sign))
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

(defn round-to [x places]
  (let [factor (Math/pow 10 places)]
    (-> (Math/round (* x factor))
        (/ factor))))

(defn trunc-precision
  [cppn places]
  (set-cppn-weights cppn (->> (cppn-weights cppn)
                              (map #(round-to % places)))))
