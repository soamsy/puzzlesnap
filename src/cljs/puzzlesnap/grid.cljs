(ns puzzlesnap.grid)

(defn piece-loc [{:keys [piece-width piece-height] :as ldb}
                 {:keys [loc-x loc-y main-piece-i main-piece-j] :as chunk}
                 [i j]]
  [(+ loc-x (* piece-width (- i main-piece-i)))
   (+ loc-y (* piece-height (- j main-piece-j)))])

(defn chunk-center [{:keys [piece-width piece-height] :as ldb} {:keys [main-piece-i main-piece-j piece-grid] :as chunk}]
  (let [locs (map #(piece-loc ldb chunk %) piece-grid)
        start-xs (map first locs)
        xs (concat start-xs (map #(+ piece-width %) start-xs))
        start-ys (map last locs)
        ys (concat start-ys (map #(+ piece-height %) start-ys))
        x-offset (* piece-width (- main-piece-i (apply min (map first piece-grid))))
        y-offset (* piece-height (- main-piece-j (apply min (map second piece-grid))))]
    [(- (/ (- (apply max xs) (apply min xs)) 2) x-offset)
     (- (/ (- (apply max ys) (apply min ys)) 2) y-offset)]))