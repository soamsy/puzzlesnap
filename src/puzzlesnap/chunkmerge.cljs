(ns puzzlesnap.chunkmerge 
  (:require [clojure.set :as set]
            [puzzlesnap.grid :refer [rotate-by rotated-location]]))

(def dirs [[0 -1]
           [0 1]
           [-1 0]
           [1 0]])

(defn detect-neighbor [{:keys [piece-width piece-height] :as db}
                       a-chunk a-piece
                       b-chunk b-piece]
  (let [a-loc (rotated-location db a-chunk a-piece)
        b-loc (rotated-location db b-chunk b-piece)
        [i j] (map - b-piece a-piece)
        [x-exp y-exp] (rotate-by
                       (map * [piece-width piece-height] [i j])
                       (:rotation a-chunk))
        [x-dist y-dist] (map #(js/Math.abs (- %1 %2)) (map - b-loc a-loc) [x-exp y-exp])
        top-bottom-snap? (zero? x-exp)
        [x-tolerance y-tolerance] [(/ piece-width (if top-bottom-snap? 7 10))
                                   (/ piece-height (if top-bottom-snap? 10 7))]]
    (when (and
           (= (:rotation b-chunk) (:rotation a-chunk))
           (not= (:index b-chunk) (:index a-chunk))
           (< x-dist x-tolerance)
           (< y-dist y-tolerance))
      {:chunk b-chunk
       :dist (if top-bottom-snap? y-dist x-dist)})))


(defn offset-chunk-post-merge [db old-chunk new-chunk]
  (let [piece (first (:piece-grid old-chunk))
        [cx-diff cy-diff] (map -
                               (rotated-location db new-chunk piece)
                               (rotated-location db old-chunk piece))]
    (-> new-chunk
        (update :loc-x #(- % cx-diff))
        (update :loc-y #(- % cy-diff)))))

(defn merge-chunks [{:keys [chunks piece->chunk] :as db} dropped-chunk-index]
  (let [dropped-chunk (get chunks dropped-chunk-index)
        piece-grid (:piece-grid dropped-chunk)
        potential-chunks
        (for [[i j :as dropped-piece] piece-grid
              [i-dir j-dir] dirs]
          (let [neighbor-piece [(+ i i-dir) (+ j j-dir)]
                neighbor-chunk (get chunks (piece->chunk neighbor-piece))]
            (when neighbor-chunk
              (detect-neighbor
               db
               dropped-chunk dropped-piece
               neighbor-chunk neighbor-piece))))
        {chosen-chunk :chunk} (first (sort-by :dist (filter identity potential-chunks)))]
    (if chosen-chunk
      (-> db
          (update-in [:chunks (:index chosen-chunk) :piece-grid] (partial set/union piece-grid))
          (update-in [:chunks (:index chosen-chunk) :min-coord] #(mapv min % (:min-coord dropped-chunk)))
          (update-in [:chunks (:index chosen-chunk) :max-coord] #(mapv max % (:max-coord dropped-chunk)))
          (assoc-in [:chunks (:index dropped-chunk) :piece-grid] #{})
          (assoc-in [:piece->chunk] (merge piece->chunk (into {} (map #(vector %1 (:index chosen-chunk)) piece-grid))))
          (update-in [:chunks (:index chosen-chunk)] #(offset-chunk-post-merge db chosen-chunk %))
          (merge-chunks (:index chosen-chunk)))
      db)))