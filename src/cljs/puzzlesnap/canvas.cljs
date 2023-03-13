(ns puzzlesnap.canvas
  (:require [puzzlesnap.grid :refer [chunk-center piece-loc]]))

(defn get-dimensions [node]
  (let [r (.getBoundingClientRect node)]
    [(.-width r) (.-height r)]))

(defn image-loaded? [image]
  (and (.-complete image) (< 0 (.-naturalHeight image))))

(defn bezier-to [ctx ref-x ref-y cx1 cy1 cx2 cy2 x y]
  (.bezierCurveTo ctx
                  (+ ref-x cx1)
                  (+ ref-y cy1)
                  (+ ref-x cx2)
                  (+ ref-y cy2)
                  (+ ref-x x)
                  (+ ref-y y)))

(defn bezier-normal  [ctx ref-x ref-y {:keys [cx1 cy1]} {:keys [x y cx2 cy2]}]
  (bezier-to ctx ref-x ref-y cx1 cy1 cx2 cy2 x y))

(defn bezier-reverse [ctx ref-x ref-y {:keys [cx2 cy2]} {:keys [x y cx1 cy1]}]
  (bezier-to ctx ref-x ref-y cx2 cy2 cx1 cy1 x y))

(defn create-piece-path
  [ctx
   {{:keys [piece-width piece-height]} :local
    {{:keys [left-right top-bottom]} :tabs} :global :as db}
   [i j]
   [tx ty]]
  (let [top (some-> top-bottom (get i) (get j))
        [ix iy] [(get-in top [0 :x]) (get-in top [0 :y])]
        bottom (some-> top-bottom (get i) (get (inc j)))
        left (some-> left-right (get i) (get j))
        right (some-> left-right (get (inc i)) (get j))]
    (-> ctx .beginPath)
    (-> ctx (.moveTo (+ tx ix) (+ ty iy)))
    (doseq [[prev curr] (partition 2 1 top)]
      (bezier-normal ctx tx ty prev curr))

    (doseq [[prev curr] (partition 2 1 right)]
      (bezier-normal ctx (+ tx piece-width) ty prev curr))

    (doseq [[prev curr] (partition 2 1 (reverse bottom))]
      (bezier-reverse ctx tx (+ ty piece-height) prev curr))

    (doseq [[prev curr] (partition 2 1 (reverse left))]
      (bezier-reverse ctx tx ty prev curr))
    (-> ctx .closePath)))

(defn draw-piece
  [ctx image
   {{piece-width :piece-width
     piece-height :piece-height :as ldb} :local :as db}
   chunk
   [i j :as piece]]
  (let [[tx ty] (piece-loc ldb chunk piece)
        [sx sy] [(* i piece-width) (* j piece-height)]
        [bx by] [(/ piece-width 2) (/ piece-height 2)]]
    (create-piece-path ctx db piece [tx ty])
    (doto ctx
      .stroke
      .save
      .clip
      (.drawImage
       image
       (- sx bx) (- sy by) (+ piece-width (* 2 bx)) (+ piece-height (* 2 by))
       (- tx bx) (- ty by) (+ piece-width (* 2 bx)) (+ piece-height (* 2 by)))
      .restore)))

(defn handle-chunk
  [ctx
   {{:keys [pan-dx pan-dy rotations] :as ldb} :local
    {:keys [draggers] :as sdb} :shared :as db}
   {:keys [piece-grid index loc-x loc-y] :as chunk}
   piece-handler]
   (when (not (empty? piece-grid))
     (let [drag (first (filter #(= (:drag-chunk %) index) (vals draggers)))
           [drag-dx drag-dy] (if drag
                               [(:drag-chunk-dx drag) (:drag-chunk-dy drag)]
                               [0 0])
           [cx cy] (chunk-center ldb chunk)]
       (.save ctx)
       (.translate ctx (+ loc-x cx pan-dx drag-dx) (+ loc-y cy pan-dy drag-dy))
       (.rotate ctx (get rotations index))
       (.translate ctx (- 0 loc-x cx) (- 0 loc-y cy))
       (let [result (doall (for [piece piece-grid] (piece-handler chunk piece)))]
         (.restore ctx)
         result))))

(defn handle-chunks
  [ctx
   {{:keys [left top scale] :as ldb} :local
    {:keys [chunks chunk-order] :as sdb} :shared :as db}
   piece-handler]
  (doto ctx
    (.save)
    (.scale scale scale)
    (.translate left top))
  (let
   [result (doall
            (for [i chunk-order]
              (let [chunk (get chunks i)]
                (handle-chunk ctx db chunk piece-handler))))]
    (.restore ctx)
    result))

(defn find-chunk-by-point
  [ctx
   {ldb :local :as db}
   [x y]]
  (letfn
   [(trace
      [chunk piece]
      (create-piece-path ctx db piece (piece-loc ldb chunk piece))
      (when (.isPointInPath ctx x y)
        chunk))]
    (last (remove nil? (flatten (handle-chunks ctx db trace))))))

(defn update-canvas
  [db
   canvasNode
   image]
  (when (and canvasNode image (image-loaded? image))
    (let [ctx (-> canvasNode (.getContext "2d"))
          [w h] (get-dimensions (.-parentNode canvasNode))]
      (when (not= w (.-width canvasNode))
        (set! (.-width canvasNode) w))
      (when (not= h (.-height canvasNode))
        (set! (.-height canvasNode) h))
      (set! (.-lineJoin ctx) "round")
      (set! (.-lineWidth ctx) 0.01)
      (.clearRect ctx 0 0 w h)
      (handle-chunks ctx db #(draw-piece ctx image db %1 %2)))))