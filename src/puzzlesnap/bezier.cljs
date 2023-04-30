(ns puzzlesnap.bezier
  (:require [puzzlesnap.utils :refer [apply-to-keys swap-keys]]))

(defn bezier-to  [path ref-x ref-y {:keys [cx1 cy1]} {:keys [x y cx2 cy2]}]
  (.bezierCurveTo path
                  (+ ref-x cx1)
                  (+ ref-y cy1)
                  (+ ref-x cx2)
                  (+ ref-y cy2)
                  (+ ref-x x)
                  (+ ref-y y)))

(defn draw-bezier  [path curve ref-x ref-y]
  (doseq [[prev curr] (partition 2 1 curve)]
    (bezier-to path ref-x ref-y prev curr)))

(defn draw-continuous-bezier
  [path curve ref-x ref-y]
  (-> path (.lineTo (+ ref-x (get-in curve [0 :x]))
                   (+ ref-y (get-in curve [0 :y]))))
  (draw-bezier path curve ref-x ref-y))

(defn reverse-curve-x [curve shift-x]
  (mapv
   #(-> %
        (swap-keys :cy1 :cy2)
        (swap-keys :cx1 :cx2)
        (apply-to-keys [:x :cx1 :cx2] (fn [x] (- x shift-x))))
   (reverse curve)))

(defn reverse-curve-y [curve shift-y]
  (mapv
   #(-> %
        (swap-keys :cy1 :cy2)
        (swap-keys :cx1 :cx2)
        (apply-to-keys [:y :cy1 :cy2] (fn [y] (- y shift-y))))
   (reverse curve)))

(defn get-piece-beziers
  [top-bottom left-right [i j] [piece-width piece-height]]
  [(get-in top-bottom [i j])
   (reverse-curve-x (get-in top-bottom [i (inc j)]) piece-width)
   (reverse-curve-y (get-in left-right [i j]) piece-height)
   (get-in left-right [(inc i) j])])

(defn create-piece-path
  [{:keys [piece-width piece-height] 
   {:keys [left-right top-bottom]} :tabs
    :as db}
   [i j]]
  (let [[top bottom left right] (get-piece-beziers top-bottom left-right [i j] [piece-width piece-height])
        path (js/Path2D.)]
    (.moveTo path (get-in top [0 :x]) (get-in top [0 :y]))
    (draw-continuous-bezier path top 0 0)
    (draw-continuous-bezier path right piece-width 0)
    (draw-continuous-bezier path bottom piece-width piece-height)
    (draw-continuous-bezier path left 0 piece-height)
    (.closePath path)
    path))