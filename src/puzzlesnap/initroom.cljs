(ns puzzlesnap.initroom 
  (:require [puzzlesnap.bezier :refer [create-piece-path]]))

(defn rand-range [start end]
  (+ start (* (- end start) (js/Math.random))))

(defn rand-close-to-middle [start end]
  (let [buffer (/ (- end start) 4)]
    (rand-range (+ start buffer) (- end buffer))))

(defn wobble [n radius]
  (rand-range (- n radius) (+ n radius)))

(defn vec2d
  [x y f]
  (vec (for [i (range x)] (vec (for [j (range y)] (f i j))))))

(defn swap-xs-ys
  [{:keys [x y cx1 cy1 cx2 cy2]}]
  {:x y :y x
   :cx1 cy1 :cy1 cx1
   :cx2 cy2 :cy2 cx2})

(defn translate-bezier
  [{:keys [x y cx1 cy1 cx2 cy2]} [sx sy]]
  {:x (+ x sx) :y (+ y sy)
   :cx1 (+ cx1 sx) :cy1 (+ cy1 sy)
   :cx2 (+ cx2 sx) :cy2 (+ cy2 sy)})

(defn make-bezier-point [[x y]]
  {:x x :y y
   :cx1 x :cy1 y
   :cx2 x :cy2 y})

(defn init-chunks
  [sdb
   {{:keys [puzzle-width puzzle-height]} :global
    {:keys [piece-width piece-height]} :local :as db}]
  (assoc
   sdb :chunks
   (into
    []
    (for [i (range puzzle-width)
          j (range puzzle-height)]
      (let [expand-x (* (js/Math.random) 200 (dec (* 2 (/ (inc i) puzzle-width))))
            expand-y (* (js/Math.random) 200 (dec (* 2 (/ (inc j) puzzle-height))))]
        {:loc-x (+ expand-x (* i piece-width))
         :loc-y (+ expand-y (* j piece-height))
         :drag-start-x 0
         :drag-start-y 0
         :drag-dx 0
         :drag-dy 0
         :index (+ (* i puzzle-height) j)
         :piece-grid #{[i j]}
         :main-piece-i i
         :main-piece-j j
         :min-coord [i j]
         :max-coord [i j]
         :rotation 0})))))

(defn init-piece-to-chunk
  [{chunks :chunks :as sdb}]
  (assoc sdb :piece->chunk
         (into {} (map #(vector [(:main-piece-i %) (:main-piece-j %)] (:index %)) chunks))))

(defn init-chunk-order [{chunks :chunks :as sdb}]
  (assoc sdb :chunk-order
         (vec (reverse (map :index chunks)))))

(defn make-tab-xs []
  (let [middle (rand-range 0.42 0.58)
        radius (rand-range 0.10 0.13)
        lside (wobble (- middle radius) 0.02)
        rside (wobble (+ middle radius) 0.02)
        lfloor (rand-close-to-middle 0 lside)
        rfloor (rand-close-to-middle rside 1.0)]
    [0.0 lfloor lside middle rside rfloor 1.0]))

(defn create-tab [scaler-x scaler-y [start-offset-x start-offset-y] [end-offset-x end-offset-y]]
  (let [negate (if (< 0.5 (js/Math.random)) 1 -1)
        flip-y (partial * negate)
        xs (make-tab-xs)
        ys [0.0 (wobble 0.0 0.02) (wobble 0.1 0.03) (wobble 0.27 0.03) (wobble 0.1 0.03) (wobble 0.0 0.02) 0.0]
        cx2s (mapv + xs [0.0 (wobble -0.1 0.01) (wobble 0.05 0.01) (wobble -0.1 0.01) (wobble 0.05 0.01) (wobble -0.1 0.01) 0.0])
        cy2s (mapv + ys [0.0 0.0 (wobble -0.12 0.01) 0.0 (wobble 0.12 0.01) 0.0 0.0])
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
  [{{:keys [puzzle-width puzzle-height]} :global
    {:keys [piece-width piece-height]} :local :as db}]
  (let [corners (vec2d (inc puzzle-width) (inc puzzle-height)
                       (fn [i j]
                         (let [limit-x (/ piece-width 12)
                               limit-y (/ piece-height 12)]
                           [(if (< 0 i puzzle-width)
                              (rand-range (- limit-x) limit-x)
                              0)
                            (if (< 0 j puzzle-height)
                              (rand-range (- limit-y) limit-y)
                              0)])))]
    (assoc-in db [:global :tabs]
              {:top-bottom (vec2d puzzle-width (inc puzzle-height)
                                  (fn [i j]
                                    (let [start-offset (get-in corners [i j])
                                          end-offset (get-in corners [(inc i) j])]
                                      (if (< 0 j puzzle-height)
                                        (create-tab piece-width piece-height start-offset end-offset)
                                        [(translate-bezier (make-bezier-point [0 0]) start-offset)
                                         (translate-bezier (make-bezier-point [piece-width 0]) end-offset)]))))
               :left-right (vec2d (inc puzzle-width) puzzle-height
                                  (fn [i j]
                                    (let [start-offset (get-in corners [i j])
                                          end-offset (get-in corners [i (inc j)])]
                                      (if (< 0 i puzzle-width)
                                        (mapv swap-xs-ys (create-tab piece-height piece-width (reverse start-offset) (reverse end-offset)))
                                        [(translate-bezier (make-bezier-point [0 0]) start-offset)
                                         (translate-bezier (make-bezier-point [0 piece-height]) end-offset)]))))})))

(defn init-piece-paths
  [ldb {{:keys [piece->chunk]} :shared :as db}]
  (assoc ldb :paths
         (into {} (for [piece (keys piece->chunk)]
                    [piece (create-piece-path db piece)]))))

(defn init-puzzle
  [db]
  (->
   db
   (update :shared init-chunks db)
   (update :shared init-piece-to-chunk)
   (update :shared init-chunk-order)
   (init-tabs)
   (#(assoc-in % [:local :rotations] (vec (repeat (count (get-in % [:shared :chunks])) 0))))
   (#(update % :local init-piece-paths %))))