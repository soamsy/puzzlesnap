(ns puzzlesnap.events
  (:require [ajax.core :as ajax]
            [puzzlesnap.db :refer [default-db]]
            [re-frame.core :refer [after debug dispatch inject-cofx path
                                   reg-cofx reg-event-db reg-event-fx reg-fx trim-v]]))

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
   (into []
    (flatten
     (for [i (range puzzle-width)]
       (for [j (range puzzle-height)]
         (let [expand-x (* (js/Math.random) 200 (dec (* 2 (/ i (dec puzzle-width)))))
               expand-y (* (js/Math.random) 200 (dec (* 2 (/ j (dec puzzle-height)))))]
           {:loc-x (+ expand-x (* i piece-width))
            :loc-y (+ expand-y (* j piece-height))
            :drag-start-x 0
            :drag-start-y 0
            :drag-dx 0
            :drag-dy 0
            :index (+ (* i puzzle-height) j)
            :piece-grid #{{:x i :y j}}
            :main-piece-x i
            :main-piece-y j})))))))

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
 (fn [{:keys [puzzle-width puzzle-height] :as cv} [img]] 
   (-> cv
       (assoc :image-loaded true)
       (assoc :image-width (.-naturalWidth img))
       (assoc :image-height (.-naturalHeight img))
       (assoc :piece-width (/ (.-naturalWidth img) puzzle-width))
       (assoc :piece-height (/ (.-naturalHeight img) puzzle-height))
       (init-puzzle)
       (scale-to-image))))

(defn find-chunk [{:keys [chunks chunk-order origin-x origin-y scale piece-width piece-height]} mouse-x mouse-y]
  (letfn [(chunk-contains [{:keys [piece-grid loc-x loc-y main-piece-x main-piece-y]}]
            (some
             (fn [{px :x py :y}]
               (let [chunk-x (+ origin-x loc-x)
                     chunk-y (+ origin-y loc-y)
                     piece-x (+ chunk-x (* piece-width (- px main-piece-x)))
                     piece-y (+ chunk-y (* piece-height (- py main-piece-y)))]
                 (and
                  (< (* scale piece-x) mouse-x (* scale (+ piece-x piece-width)))
                  (< (* scale piece-y) mouse-y (* scale (+ piece-y piece-height))))))
             piece-grid))]
    (first (filter chunk-contains (map chunks chunk-order)))))

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
 canvas-interceptor
 (fn [{:keys [pan-start-x pan-start-y scale] :as cv} [x y]]
 (merge cv
        (cond
          (-> cv :is-panning) {:pan-dx (/ (- x pan-start-x) scale)
                               :pan-dy (/ (- y pan-start-y) scale)}
          (-> cv :drag-chunk) {:drag-chunk-dx (/ (- x (:drag-chunk-start-x cv)) scale)
                               :drag-chunk-dy (/ (- y (:drag-chunk-start-y cv)) scale)}
          :else {}))))

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
         :drag-chunk nil}))))

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