(ns puzzlesnap.grid)

(defn piece-loc [{:keys [piece-width piece-height] :as ldb}
                 {:keys [main-piece-i main-piece-j] :as chunk}
                 [i j]]
  [(* piece-width (- i main-piece-i))
   (* piece-height (- j main-piece-j))])

(defn chunk-center [ldb {:keys [min-coord max-coord] :as chunk}]
  (let [[low-x low-y] (piece-loc ldb chunk min-coord)
        [high-x high-y] (piece-loc ldb chunk (map + [1 1] max-coord))]
    [(/ (+ low-x high-x) 2)
     (/ (+ low-y high-y) 2)]))

(defn rotate-by [[x y] turns]
  (if (zero? turns)
    [x y]
    (rotate-by [(- y) x] (dec turns))))

(defn rotate-around [orig coords turns]
  (mapv + orig (rotate-by (map - coords orig) turns)))

(defn rotated-location [ldb
                         {:keys [loc-x loc-y rotation] :as chunk}
                         [i j]]
  (let [[cx cy] (chunk-center ldb chunk)
        [x y] (piece-loc ldb chunk [i j])]
    (mapv + [loc-x loc-y] (rotate-around [cx cy] [x y] rotation))))