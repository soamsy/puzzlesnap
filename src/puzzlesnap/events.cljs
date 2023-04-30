(ns puzzlesnap.events
  (:require [puzzlesnap.db :refer [default-db]]
            [puzzlesnap.initroom :refer [init-puzzle]]
            [puzzlesnap.model :refer [mouse-down mouse-move mouse-up]]
            [re-frame.core :refer [debug path reg-event-db reg-event-fx reg-fx
                                   trim-v]]))

(reg-event-fx
  :app/initialize
  (fn [_ _]
    {:db default-db}))

(def update-interceptor [debug trim-v])

(reg-event-db
  :local-tick
  update-interceptor
  (fn [db _] (update db :tick-id inc)))

(defn init-image-state [db puzzle-width puzzle-height img]
  (-> db
      (assoc :image-loaded true)
      (assoc :image-width (.-naturalWidth img))
      (assoc :image-height (.-naturalHeight img))
      (assoc :piece-width (/ (.-naturalWidth img) puzzle-width))
      (assoc :piece-height (/ (.-naturalHeight img) puzzle-height))
      (assoc :scale (* 0.8
                       (js/Math.min
                         (/ js/innerWidth (.-naturalWidth img))
                         (/ js/innerHeight (.-naturalHeight img)))))))

(reg-event-db
  :image-loaded
  update-interceptor
  (fn [{:keys [puzzle-width puzzle-height image-loaded] :as db} [img]]
    (if image-loaded
      (update db :tick-id inc)
      (-> db
          (init-image-state puzzle-width puzzle-height img)
          (init-puzzle)))))

(reg-event-fx
  :mouse-down
  update-interceptor
  (fn [{db :db} [x y buttons]]
    (mouse-down db x y buttons)))

(reg-event-db
  :mouse-move
  (remove #{debug} update-interceptor)
  mouse-move)

(reg-event-fx
  :mouse-up
  update-interceptor
  (fn [{db :db} [button]]
    (mouse-up db button)))

(reg-fx
  :play-sound
  (fn [_]
    (let [el (-> js/document (.getElementById "snap-sound"))]
      (set! (.-volume el) 0.7)
      (.play el))))

(reg-event-db
  :mouse-wheel
  update-interceptor
  (fn [{:keys [scale left top] :as db} [x y deltaY]]
    (let [scale-factor (if (< 0 deltaY) 0.85 1.15)
          new-scale (js/Math.min 1.5 (js/Math.max 0.4 (* scale scale-factor)))
          ratio (/ new-scale scale)
          new-x (* x ratio)
          new-y (* y ratio)]
      (merge
        db
        {:scale new-scale
         :left (+ left (/ (- x new-x) new-scale))
         :top (+ top (/ (- y new-y) new-scale))}))))

(reg-event-fx
  :rotate-piece
  update-interceptor
  (fn [{db :db} [chunk-index]]
    (let [chunk (get-in db [:chunks chunk-index])
          angle (get-in db [:rotations chunk-index])
          target-angle (* (/ js/Math.PI 2) (get chunk :rotation))
          diff (mod (- target-angle angle) (* 2 js/Math.PI))
          tick (/ js/Math.PI 16)
          should-stop (< diff tick)]
      (if should-stop
        {:db (assoc-in db [:rotations chunk-index] target-angle)}
        {:db (assoc-in db [:rotations chunk-index] (+ angle (if (< (+ js/Math.PI 0.001) diff) (- tick) tick)))
         :dispatch-later {:ms 12 :dispatch [:rotate-piece chunk-index]}}))))