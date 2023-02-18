(ns puzzlesnap.events
  (:require [puzzlesnap.db :refer [default-db]]
            [puzzlesnap.model :refer [init-puzzle mouse-down mouse-move
                                      mouse-up scale-to-image]]
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

(reg-event-db
 :mouse-down
 canvas-interceptor
 mouse-down)

(reg-event-db
 :mouse-move
 (remove #{debug} canvas-interceptor)
 mouse-move)

(reg-event-db
 :mouse-up
 canvas-interceptor
 (fn [cv _] (mouse-up cv)))

(reg-event-db
 :mouse-wheel
 canvas-interceptor
 (fn [{:keys [scale left top] :as cv} [x y deltaY]]
   (let [increment (if (< 0 deltaY) -0.1 0.1)
         new-scale (js/Math.max 0.2 (+ scale increment))
         ratio (/ new-scale scale)
         new-x (* x ratio)
         new-y (* y ratio)]
     (merge
      cv
      {:scale new-scale
       :left (+ left (/ (- x new-x) new-scale))
       :top (+ top (/ (- y new-y) new-scale))}))))

(reg-event-db
  :common/set-error
  (fn [db [_ error]]
    (assoc db :common/error error)))