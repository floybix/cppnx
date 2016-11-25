(ns org.nfrac.cppnx.compile-clj
  (:require [org.nfrac.cppnx.core :as cppnx]))

(def fn-exprs
  {:sine
   '(defn sine [x]
      (Math/sin (* x 3.1415 2)))
   :gaussian
   '(defn gaussian [x]
      (-> (* x 2.5)
          (Math/pow 2.0)
          (* -1.0)
          (Math/exp)
          (* 2.0)
          (- 1.0)))
   :sigmoid
   '(defn sigmoid [x]
      (-> (/ 1.0
             (-> (* x -4.9) (Math/exp) (+ 1.0)))
          (* 2.0)
          (- 1.0)))
   :sawtooth
   '(defn sawtooth [x]
      (-> (- x (int x))
          (* 2.0)
          (- 1.0)))})

(defn build-cppn-code
  [cppn]
  (let [sym (comp symbol name)
        strata (cppnx/cppn-strata cppn)
        sorted-nids (apply concat (rest strata))
        w-exprs (zipmap (cppnx/edge-list cppn)
                        (map #(list 'w %) (range)))
        assigns (map (fn [nid]
                       (let [node-type (get-in cppn [:nodes nid] :linear)
                             deps (keys (get-in cppn [:edges nid]))
                             sum (->> deps
                                      (map (fn [k]
                                             (let [w (get w-exprs [nid k])]
                                               (list '* w (sym k)))))
                                      (apply list '+))
                             sumw (->> deps
                                       (map (fn [k]
                                              (let [w (get w-exprs [nid k])]
                                                (list 'abs w))))
                                       (apply list '+))
                             expr (if (= node-type :linear)
                                    (list '/ sum sumw)
                                    (list (sym node-type) sum))]
                         [(sym nid) expr]))
                     sorted-nids)
        out-exprs (into {} (map (fn [nid] [nid (sym nid)])
                                (sort (:outputs cppn))))]
    (-> (vec (vals fn-exprs))
        (conj (list 'def 'ws (cppnx/cppn-weights cppn)))
        (conj
         (list 'defn 'this-cppn
               (into '[ws] (map sym (sort (:inputs cppn))))
               (list 'let
                     (vec (apply concat assigns))
                     out-exprs))))))