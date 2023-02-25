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
         (vec (reverse (map :index chunks)))))

(defn rand-range [start end]
  (+ start (* (- end start) (js/Math.random))))

(defn rand-close-to-middle [start end]
  (let [buffer (/ (- end start) 4)]
    (rand-range (+ start buffer) (- end buffer))))

(defn vec2d
  [x y f]
  (vec (for [i (range x)] (vec (for [j (range y)] (f i j))))))

(defn swap-xs-ys
  [{:keys [x y cx1 cy1 cx2 cy2]}]
  {:x (+ y) :y x
   :cx1 (+ cy1) :cy1 cx1
   :cx2 (+ cy2) :cy2 cx2})

(defn translate-bezier
  [{:keys [x y cx1 cy1 cx2 cy2]} [sx sy]]
  {:x (+ x sx) :y (+ y sy)
   :cx1 (+ cx1 sx) :cy1 (+ cy1 sy)
   :cx2 (+ cx2 sx) :cy2 (+ cy2 sy)})

(defn make-bezier-point [[x y]]
  {:x x :y y
   :cx1 x :cy1 y
   :cx2 x :cy2 y})

(defn wobble [n radius]
  (rand-range (- n radius) (+ n radius)))

(defn make-tab-xs []
  (let [middle (rand-range 0.4 0.6)
        radius (rand-range 0.08 0.14)
        lside (wobble (- middle radius) 0.02)
        rside (wobble (+ middle radius) 0.02)
        lfloor (rand-close-to-middle 0 lside)
        rfloor (rand-close-to-middle rside 1.0)]
    [0.0 lfloor lside middle rside rfloor 1.0]))

(defn create-tab [scaler-x scaler-y [start-offset-x start-offset-y] [end-offset-x end-offset-y]]
  (let [negate (if (< 0.5 (js/Math.random)) 1 -1)
        flip-y (partial * negate)
        xs (make-tab-xs)
        ys [0.0 0.0 0.1 0.25 0.1 0.0 0.0]
        cx2s (mapv + xs [0.0 -0.1 0.05 -0.1 0.05 -0.1 0.0])
        cy2s (mapv + ys [0.0 0.0 -0.1 0.0 0.1 0.0 0.0])
        cx1s (mapv + xs (map - xs cx2s))
        cy1s (mapv + ys (map - ys cy2s))
        diff-x (- (+ end-offset-x scaler-x) start-offset-x)
        diff-y (- end-offset-y start-offset-y)
        slope (/ diff-y diff-x)
        calc-x (fn [x] (+ start-offset-x (* x diff-x)))
        calc-y (fn [x y] (+ (* y scaler-y) start-offset-y (* x diff-x slope)))]
        (mapv (fn [x y cx1 cy1 cx2 cy2]
                {:x (calc-x x) :y (calc-y x (flip-y y))
                 :cx1 (calc-x cx1) :cy1 (calc-y cx1 (flip-y cy1))
                 :cx2 (calc-x cx2) :cy2 (calc-y cx2 (flip-y cy2))})
              xs ys cx1s cy1s cx2s cy2s)))

(defn init-tabs
  [{:keys [puzzle-width puzzle-height piece-width piece-height] :as cv}]
  (let [corners (vec2d (inc puzzle-width) (inc puzzle-height)
                       (fn [i j]
                         (let [limit-x (/ piece-width 4)
                               limit-y (/ piece-height 4)]
                           [(if (< 0 i puzzle-width)
                              (- (* (js/Math.random) limit-x) (/ limit-x 2))
                              0)
                            (if (< 0 j puzzle-height)
                              (- (* (js/Math.random) limit-y) (/ limit-y 2))
                              0)])))]
    (assoc cv :tabs
           {:top-bottom (vec2d puzzle-width (inc puzzle-height)
                               (fn [i j]
                                 (let [start-offset (some-> corners (get i) (get j))
                                       end-offset (some-> corners (get (inc i)) (get j))]
                                   (if (< 0 j puzzle-height)
                                     (-> (create-tab piece-width piece-height start-offset end-offset))
                                     [(translate-bezier (make-bezier-point [0 0]) start-offset)
                                      (translate-bezier (make-bezier-point [piece-width 0]) end-offset)]))))
            :left-right (vec2d (inc puzzle-width) puzzle-height
                               (fn [i j]
                                 (let [[sx sy] (some-> corners (get i) (get j))
                                       [ex ey] (some-> corners (get i) (get (inc j)))]
                                   (if (< 0 i puzzle-width)
                                     (-> (mapv swap-xs-ys (create-tab piece-height piece-width [sy sx] [ey ex])))
                                     [(translate-bezier (make-bezier-point [0 0]) [sx sy])
                                      (translate-bezier (make-bezier-point [0 piece-height]) [ex ey])]))))})))

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

(defn piece-location [{:keys [piece-width piece-height] :as cv}
                      {:keys [loc-x loc-y main-piece-i main-piece-j] :as chunk}
                      [i j]]
  [(+ loc-x (* piece-width (- i main-piece-i)))
   (+ loc-y (* piece-height (- j main-piece-j)))])

(defn translate-mouse-coords [{:keys [left top scale]} [x y]]
  [(- (/ x scale) left) (- (/ y scale) top)])

(defn find-chunk [{:keys [chunks chunk-order piece-width piece-height] :as cv} [mx my]]
  (letfn [(contains-coords? [{:keys [piece-grid] :as chunk}]
            (some
             (fn [piece]
               (let [[piece-x piece-y] (piece-location cv chunk piece)]
                 (and
                  (< piece-x mx (+ piece-x piece-width))
                  (< piece-y my (+ piece-y piece-height)))))
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
        top-bottom-snap? (zero? i-dir)
        x-tolerance (/ piece-width (if top-bottom-snap? 5 12))
        y-tolerance (/ piece-height (if top-bottom-snap? 12 5))]
    (when (and
           (not= (:index neighbor-chunk) (:index dropped-chunk))
           (< x-dist x-tolerance)
           (< y-dist y-tolerance))
      {:chunk neighbor-chunk
       :dist (if top-bottom-snap? y-dist x-dist)})))

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

(defn mouse-down [{:keys [chunk-order] :as cv} [x y pan?]]
  (merge
   cv
   (if pan?
     {:is-panning true
      :pan-start-x x
      :pan-start-y y}
     (when-let [chunk (find-chunk cv (translate-mouse-coords cv [x y]))]
       {:drag-chunk (:index chunk)
        :drag-chunk-start-x x
        :drag-chunk-start-y y
        :chunk-order (let [i (.indexOf chunk-order (:index chunk))]
                       (vec
                        (concat
                         (subvec chunk-order 0 i)
                         (subvec chunk-order (inc i) (count chunk-order))
                         [(nth chunk-order i)])))}))))

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