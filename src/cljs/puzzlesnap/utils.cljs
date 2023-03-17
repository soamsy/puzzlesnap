(ns puzzlesnap.utils)

(defn deep-merge-with [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(def deep-merge (partial deep-merge-with (fn [& vals] (last vals))))

(defn apply-to-keys
  [m ks f]
  (merge
   m
   (into {} (for [[k v] (select-keys m ks)]
              [k (f v)]))))


(defn swap-keys [m a b]
  (-> m
      (assoc a (b m))
      (assoc b (a m))))