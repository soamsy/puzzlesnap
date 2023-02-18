(ns puzzlesnap.events
  (:require [clojure.set :as set]
            [puzzlesnap.db :refer [default-db]]
            [puzzlesnap.model :refer [piece-location]]
            [re-frame.core :refer [debug path reg-event-db reg-event-fx trim-v]]))

(reg-event-fx
 :app/initialize
 (fn [_ _] 
   {:db default-db}))

(def canvas-interceptor [debug
                         (path :canvas)
                         trim-v])

(reg-event-db
 :canvas-tick
 canvas-interceptor
 (fn [cv _] (update cv :id inc)))

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
         :main-piece-x i
         :main-piece-y j})))))

(defn init-piece-to-chunk
  [{chunks :chunks :as cv}]
  (assoc cv :piece->chunk
         (into {} (map #(vector [(:main-piece-x %) (:main-piece-y %)] (:index %)) chunks))))

(defn init-chunk-order [{chunks :chunks :as cv}]
  (assoc cv :chunk-order
         (reverse (map :index chunks))))

(defn init-puzzle
  [cv] 
  (->
   cv
   (init-chunks)
   (init-piece-to-chunk)
   (init-chunk-order)))

(defn scale-to-image [{:keys [image-width image-height] :as cv}]
  (assoc cv :scale
         (* 0.8
            (js/Math.min
             (/ js/innerWidth image-width)
             (/ js/innerHeight image-height)))))

(reg-event-db
 :image-loaded
 canvas-interceptor
 (fn [{:keys [puzzle-width puzzle-height image-loaded] :as cv} [img]] 
 (if image-loaded
   (update cv :id inc)
   (-> cv
       (assoc :image-loaded true)
       (assoc :image-width (.-naturalWidth img))
       (assoc :image-height (.-naturalHeight img))
       (assoc :piece-width (/ (.-naturalWidth img) puzzle-width))
       (assoc :piece-height (/ (.-naturalHeight img) puzzle-height))
       (init-puzzle)
       (scale-to-image)))))

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

(reg-event-db
 :mouse-down
 canvas-interceptor
 (fn [cv [x y pan?]]
   (merge
    cv
    (if pan?
      {:is-panning true
       :pan-start-x x
       :pan-start-y y}
      (when-let [chunk (find-chunk cv x y)]
        {:drag-chunk (:index chunk)
         :drag-chunk-start-x x
         :drag-chunk-start-y y})))))

(reg-event-db
 :mouse-move
 (remove #{debug} canvas-interceptor)
 (fn [{:keys [pan-start-x pan-start-y scale drag-chunk-start-x drag-chunk-start-y] :as cv} [x y]]
 (merge cv
        (cond
          (-> cv :is-panning) {:pan-dx (/ (- x pan-start-x) scale)
                               :pan-dy (/ (- y pan-start-y) scale)}
          (-> cv :drag-chunk) {:drag-chunk-dx (/ (- x drag-chunk-start-x) scale)
                               :drag-chunk-dy (/ (- y drag-chunk-start-y) scale)}
          :else {}))))

(def dirs [[0 -1]
           [0 1]
           [-1 0]
           [1 0]])

(defn detect-neighbor [{:keys [piece-width piece-height] :as cv} dropped-chunk piece neighbor-chunk neighbor]
  (let [neighbor-loc (piece-location cv neighbor-chunk neighbor)
        expected-loc (piece-location cv dropped-chunk neighbor)
        [x-dir y-dir] (map - piece neighbor)
        [x-dist y-dist] (map #(js/Math.abs (- %1 %2)) neighbor-loc expected-loc)
        vertical-snap? (zero? x-dir)
        x-tolerance (/ piece-width (if vertical-snap? 5 12))
        y-tolerance (/ piece-height (if vertical-snap? 12 5))]
    (when (and
           (not= (:index neighbor-chunk) (:index dropped-chunk))
           (< x-dist x-tolerance)
           (< y-dist y-tolerance))
      [neighbor-chunk (if vertical-snap? y-dist x-dist)])))

(defn merge-chunks [{:keys [chunks piece->chunk] :as cv} dropped-chunk-index]
  (let [dropped-chunk (get chunks dropped-chunk-index)
        piece-grid (:piece-grid dropped-chunk)
        potential-chunks
        (for [[px py :as piece] piece-grid
              [x-dir y-dir] dirs]
          (let [neighbor [(+ px x-dir) (+ py y-dir)]
                neighbor-chunk (get chunks (piece->chunk neighbor))]
            (when neighbor-chunk
              (detect-neighbor cv dropped-chunk piece neighbor-chunk neighbor))))
        [chosen-chunk _] (first (sort-by #(nth % 1) (filter identity potential-chunks)))]
    (if chosen-chunk
      (-> cv
          (update-in [:chunks (:index chosen-chunk) :piece-grid] (partial set/union piece-grid))
          (assoc-in [:chunks (:index dropped-chunk) :piece-grid] #{})
          (assoc-in [:piece->chunk] (merge piece->chunk (into {} (map #(vector %1 (:index chosen-chunk)) piece-grid)))))
      cv)))

(reg-event-db
 :mouse-up
 canvas-interceptor
 (fn [{:keys [origin-x origin-y pan-dx pan-dy chunks drag-chunk drag-chunk-dx drag-chunk-dy] :as cv} _]
   (-> cv
       (merge
        {:is-panning false
         :pan-start-x 0
         :pan-start-y 0
         :origin-x (+ origin-x pan-dx)
         :origin-y (+ origin-y pan-dy)
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
       (#(if drag-chunk
          (merge-chunks % drag-chunk)
          %)))))

(reg-event-db
 :mouse-wheel
 canvas-interceptor
 (fn [{:keys [scale origin-x origin-y] :as cv} [x y deltaY]]
   (let [increment (if (< 0 deltaY) -0.1 0.1)
         new-scale (js/Math.max 0.2 (+ scale increment))
         ratio (/ new-scale scale)
         new-x (* x ratio)
         new-y (* y ratio)]
     (merge 
      cv
      {:scale new-scale
       :origin-x (+ origin-x (/ (- x new-x) new-scale))
       :origin-y (+ origin-y (/ (- y new-y) new-scale))}))))

(reg-event-db
  :common/set-error
  (fn [db [_ error]]
    (assoc db :common/error error)))