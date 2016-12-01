(ns org.nfrac.cppnx.compile-clj
  (:require [org.nfrac.cppnx.core :as cppnx]))

(def fn-exprs
 '[
   (defn sine [x]
     (Math/sin (* x 3.1415 2)))

   (defn gaussian [x]
     (-> (* x 2.5)
         (Math/pow 2.0)
         (* -1.0)
         (Math/exp)))

   (defn sigmoid [x]
     (/ 1.0
        (-> (* x -4.9) (Math/exp) (+ 1.0))))

   (defn sawtooth [x]
     (mod x 1.0))

   (defn abs [x] (if (neg? x) (- x) x))

   (defn to-0-1 [x] (abs (Math/tanh x)))

   (defn xy->d [x y]
     (Math/sqrt (+ (* x x) (* y y))))])

(defn build-cppn-code
  [cppn]
  (let [sym (comp symbol name)
        strata (cppnx/cppn-strata cppn)
        zerod? (:zerod cppn #{})
        sorted-nids (apply concat (rest strata))
        w-exprs (zipmap (cppnx/edge-list cppn)
                        (map #(list 'w %) (range)))
        assigns (map (fn [nid]
                       (let [node-type (get-in cppn [:nodes nid] :linear)
                             deps (keys (get-in cppn [:edges nid]))
                             adds (->> deps
                                       (map (fn [k]
                                              (let [w (get w-exprs [nid k])]
                                                (list '* w (sym k))))))
                             sum (if (> (count deps) 1)
                                   (apply list '+ adds)
                                   (first adds))
                             expr (cond
                                    (zerod? nid)
                                    0.0
                                    (= :linear node-type)
                                    sum
                                    :else
                                    (list (sym node-type) sum))]
                         [(sym nid) expr]))
                     sorted-nids)
        out-exprs (into {} (map (fn [nid]
                                  [nid (list 'to-0-1 (sym nid))])
                                (sort (:outputs cppn))))
        in-syms (map sym (sort (disj (:inputs cppn) :d)))
        assigns (if (contains? (:inputs cppn) :d)
                  (conj assigns '[d (xy->d x y)])
                  assigns)]
    (-> fn-exprs
        (conj (list 'def 'w (cppnx/cppn-weights cppn)))
        (conj
         (list 'defn 'this-cppn
               (into '[w] in-syms)
               (list 'let
                     (vec (apply concat assigns))
                     out-exprs))))))
