(ns puzzlesnap.model 
  (:require [clojure.set :as set]))

(defn init-chunks
  [{:keys
    [puzzle-width
     puzzle-height
     piece-width
     piece-height] :as cv}]
  (assoc
   cv :chunks
   (into
    []
    (for [i (range puzzle-width)
          j (range puzzle-height)]
      (let [expand-x (* (js/Math.random) 200 (dec (* 2 (/ i (dec puzzle-width)))))
            expand-y (* (js/Math.random) 200 (dec (* 2 (/ j (dec puzzle-height)))))]
        {:loc-x (+ expand-x (* i piece-width))
         :loc-y (+ expand-y (* j piece-height))
         :drag-start-x 0
         :drag-start-y 0
         :drag-dx 0
         :drag-dy 0
         :index (+ (* i puzzle-height) j)
         :piece-grid #{[i j]}
         :main-piece-i i
         :main-piece-j j})))))

(defn init-piece-to-chunk
  [{chunks :chunks :as cv}]
  (assoc cv :piece->chunk
         (into {} (map #(vector [(:main-piece-i %) (:main-piece-j %)] (:index %)) chunks))))

(defn init-chunk-order [{chunks :chunks :as cv}]
  (assoc cv :chunk-order
         (reverse (map :index chunks)))) 


(defn create-tab [piece-length]
  {:tab-length (/ piece-length 4)
   :tab-width (/ piece-length 4)})

(defn init-tabs [{:keys [puzzle-width puzzle-height piece-width piece-height] :as cv}]
  (assoc cv :tabs
         {:vertical (for [i (range puzzle-width)]
                      (for [j (range (dec puzzle-height))]
                        (create-tab piece-width)))
          :horizontal (for [i (range (dec puzzle-width))]
                        (for [j (range puzzle-height)]
                          (create-tab piece-height)))}))

(defn init-puzzle
  [cv] 
  (->
   cv
   (init-chunks)
   (init-piece-to-chunk)
   (init-chunk-order)
   (init-tabs)))

(defn scale-to-image [{:keys [image-width image-height] :as cv}]
  (assoc cv :scale
         (* 0.8
            (js/Math.min
             (/ js/innerWidth image-width)
             (/ js/innerHeight image-height)))))

(defn piece-location [{:keys [left top piece-width piece-height] :as cv}
                      {:keys [loc-x loc-y main-piece-i main-piece-j] :as chunk}
                      [i j]]
  [(+ left loc-x (* piece-width (- i main-piece-i)))
   (+ top loc-y (* piece-height (- j main-piece-j)))])

(defn find-chunk [{:keys [chunks chunk-order scale piece-width piece-height] :as cv} mouse-x mouse-y]
  (letfn [(contains-coords? [{:keys [piece-grid] :as chunk}]
            (some
             (fn [piece]
               (let [[piece-x piece-y] (piece-location cv chunk piece)]
                 (and
                  (< piece-x (/ mouse-x scale) (+ piece-x piece-width))
                  (< piece-y (/ mouse-y scale) (+ piece-y piece-height)))))
             piece-grid))]
    (first (filter contains-coords? (map chunks chunk-order)))))

(def dirs [[0 -1]
           [0 1]
           [-1 0]
           [1 0]])

(defn detect-neighbor [{:keys [piece-width piece-height] :as cv} 
                       dropped-chunk dropped-piece 
                       neighbor-chunk neighbor]
  (let [neighbor-loc (piece-location cv neighbor-chunk neighbor)
        expected-loc (piece-location cv dropped-chunk neighbor)
        [i-dir j-dir] (map - neighbor dropped-piece)
        [x-dist y-dist] (map #(js/Math.abs (- %1 %2)) neighbor-loc expected-loc)
        vertical-snap? (zero? i-dir)
        x-tolerance (/ piece-width (if vertical-snap? 5 12))
        y-tolerance (/ piece-height (if vertical-snap? 12 5))]
    (when (and
           (not= (:index neighbor-chunk) (:index dropped-chunk))
           (< x-dist x-tolerance)
           (< y-dist y-tolerance))
      {:chunk neighbor-chunk
       :dist (if vertical-snap? y-dist x-dist)})))

(defn merge-chunks [{:keys [chunks piece->chunk] :as cv} dropped-chunk-index]
  (let [dropped-chunk (get chunks dropped-chunk-index)
        piece-grid (:piece-grid dropped-chunk)
        potential-chunks
        (for [[i j :as dropped-piece] piece-grid
              [i-dir j-dir] dirs]
          (let [neighbor-piece [(+ i i-dir) (+ j j-dir)]
                neighbor-chunk (get chunks (piece->chunk neighbor-piece))]
            (when neighbor-chunk
              (detect-neighbor
               cv
               dropped-chunk dropped-piece
               neighbor-chunk neighbor-piece))))
        {chosen-chunk :chunk} (first (sort-by :dist (filter identity potential-chunks)))]
    (if chosen-chunk
      (-> cv
          (update-in [:chunks (:index chosen-chunk) :piece-grid] (partial set/union piece-grid))
          (assoc-in [:chunks (:index dropped-chunk) :piece-grid] #{})
          (assoc-in [:piece->chunk] (merge piece->chunk (into {} (map #(vector %1 (:index chosen-chunk)) piece-grid)))))
      cv)))

(defn mouse-down [cv [x y pan?]]
  (merge
   cv
   (if pan?
     {:is-panning true
      :pan-start-x x
      :pan-start-y y}
     (when-let [chunk (find-chunk cv x y)]
       {:drag-chunk (:index chunk)
        :drag-chunk-start-x x
        :drag-chunk-start-y y}))))

(defn mouse-move [{:keys [pan-start-x pan-start-y scale drag-chunk-start-x drag-chunk-start-y] :as cv} [x y]]
  (merge cv
         (cond
           (-> cv :is-panning) {:pan-dx (/ (- x pan-start-x) scale)
                                :pan-dy (/ (- y pan-start-y) scale)}
           (-> cv :drag-chunk) {:drag-chunk-dx (/ (- x drag-chunk-start-x) scale)
                                :drag-chunk-dy (/ (- y drag-chunk-start-y) scale)}
           :else {})))


(defn mouse-up [{:keys [left top pan-dx pan-dy chunks drag-chunk drag-chunk-dx drag-chunk-dy] :as cv}]
  (-> cv
      (merge
       {:is-panning false
        :pan-start-x 0
        :pan-start-y 0
        :left (+ left pan-dx)
        :top (+ top pan-dy)
        :pan-dx 0
        :pan-dy 0
        :chunks (if drag-chunk
                  (-> chunks
                      (update-in [drag-chunk :loc-x] (partial + drag-chunk-dx))
                      (update-in [drag-chunk :loc-y] (partial + drag-chunk-dy)))
                  chunks)
        :drag-chunk-start-x 0
        :drag-chunk-start-y 0
        :drag-chunk-dx 0
        :drag-chunk-dy 0
        :drag-chunk nil})
      (#(if drag-chunk (merge-chunks % drag-chunk) %))))