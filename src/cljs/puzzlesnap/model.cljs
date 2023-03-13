(ns puzzlesnap.model
  (:require [clojure.set :as set]
            [puzzlesnap.canvas :refer [find-chunk-by-point]]
            [puzzlesnap.grid :refer [chunk-center]]))

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
         :rotation 0})))))

(defn init-piece-to-chunk
  [{chunks :chunks :as sdb}]
  (assoc sdb :piece->chunk
         (into {} (map #(vector [(:main-piece-i %) (:main-piece-j %)] (:index %)) chunks))))

(defn init-chunk-order [{chunks :chunks :as sdb}]
  (assoc sdb :chunk-order
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

(defn init-puzzle
  [db]
  (->
   db
   (update :shared init-chunks db)
   (update :shared init-piece-to-chunk)
   (update :shared init-chunk-order)
   (init-tabs)
   (#(assoc-in % [:local :rotations] (vec (repeat (count (get-in % [:shared :chunks])) 0))))))
   

(defn rotate-by [[x y] turns]
  (if (zero? turns)
    [x y]
    (rotate-by [(- y) x] (dec turns))))

(defn rotate-around [orig coords turns]
  (mapv + orig (rotate-by (map - coords orig) turns)))

(defn piece-location [{:keys [piece-width piece-height] :as ldb}
                      {:keys [loc-x loc-y main-piece-i main-piece-j rotation] :as chunk}
                      [i j]]
  (let [[cx cy] (chunk-center ldb chunk)
        [x y] [(+  (* piece-width (- i main-piece-i)))
               (+  (* piece-height (- j main-piece-j)))]]
    (mapv + [loc-x loc-y] (rotate-around [cx cy] [x y] rotation))))

(defn find-chunk [db [mx my]]
  (let [canvas (js/document.getElementById "canvas-inner")
        ctx (.getContext canvas "2d")]
    (find-chunk-by-point ctx db [mx my])))

(def dirs [[0 -1]
           [0 1]
           [-1 0]
           [1 0]])

(defn detect-neighbor [{:keys [piece-width piece-height] :as ldb}
                       a-chunk a-piece
                       b-chunk b-piece]
  (let [a-loc (piece-location ldb a-chunk a-piece)
        b-loc (piece-location ldb b-chunk b-piece)
        [i j] (map - b-piece a-piece)
        [x-exp y-exp] (rotate-by
                       (map * [piece-width piece-height] [i j])
                       (:rotation a-chunk))
        [x-dist y-dist] (map #(js/Math.abs (- %1 %2)) (map - b-loc a-loc) [x-exp y-exp])
        top-bottom-snap? (zero? x-exp)
        x-tolerance (/ piece-width (if top-bottom-snap? 5 12))
        y-tolerance (/ piece-height (if top-bottom-snap? 12 5))]
    (when (and
           (= (:rotation b-chunk) (:rotation a-chunk))
           (not= (:index b-chunk) (:index a-chunk))
           (< x-dist x-tolerance)
           (< y-dist y-tolerance))
      {:chunk b-chunk
       :dist (if top-bottom-snap? y-dist x-dist)})))


(defn offset-chunk-post-merge [ldb old-chunk new-chunk]
  (let [piece (first (:piece-grid old-chunk))
        [cx-diff cy-diff] (map -
                               (piece-location ldb new-chunk piece)
                               (piece-location ldb old-chunk piece))]
    (-> new-chunk
        (update :loc-x #(- % cx-diff))
        (update :loc-y #(- % cy-diff)))))

(defn merge-chunks [{:keys [chunks piece->chunk] :as sdb} ldb dropped-chunk-index]
  (let [dropped-chunk (get chunks dropped-chunk-index)
        piece-grid (:piece-grid dropped-chunk)
        potential-chunks
        (for [[i j :as dropped-piece] piece-grid
              [i-dir j-dir] dirs]
          (let [neighbor-piece [(+ i i-dir) (+ j j-dir)]
                neighbor-chunk (get chunks (piece->chunk neighbor-piece))]
            (when neighbor-chunk
              (detect-neighbor
               ldb
               dropped-chunk dropped-piece
               neighbor-chunk neighbor-piece))))
        {chosen-chunk :chunk} (first (sort-by :dist (filter identity potential-chunks)))]
    (if chosen-chunk
      (-> sdb
          (update-in [:chunks (:index chosen-chunk) :piece-grid] (partial set/union piece-grid))
          (assoc-in [:chunks (:index dropped-chunk) :piece-grid] #{})
          (assoc-in [:piece->chunk] (merge piece->chunk (into {} (map #(vector %1 (:index chosen-chunk)) piece-grid))))
          (update-in [:chunks (:index chosen-chunk)] #(offset-chunk-post-merge ldb chosen-chunk %))
          (merge-chunks ldb (:index chosen-chunk)))
      sdb)))

(defn deep-merge-with [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(def deep-merge (partial deep-merge-with (fn [& vals] (last vals))))

(defn mouse-down [{{:keys [chunk-order] :as sdb} :shared
                   {:keys [id] :as udb} :user :as db}
                  [x y pan?]]
  (->
   (deep-merge
    db
    (if pan?
      {:local {:is-panning true
               :has-panned false
               :pan-start-x x
               :pan-start-y y}}
      (when-let [chunk (find-chunk db [x y])]
        {:shared
         {:draggers {id {:drag-chunk (:index chunk)
                         :drag-chunk-start-x x
                         :drag-chunk-start-y y
                         :dragged false}}
          :chunk-order (let [i (.indexOf chunk-order (:index chunk))]
                         (vec
                          (concat
                           (subvec chunk-order 0 i)
                           (subvec chunk-order (inc i) (count chunk-order))
                           [(nth chunk-order i)])))}})))))

(defn mouse-move [{{:keys [is-panning pan-start-x pan-start-y scale] :as ldb} :local
                   {:keys [draggers] :as sdb} :shared
                   {:keys [id] :as udb} :user :as db} [x y]]
  (let [{:keys [drag-chunk-start-x drag-chunk-start-y]} (get draggers id)]
    (cond
      is-panning (update-in
                  db
                  [:local]
                  #(merge % {:pan-dx (/ (- x pan-start-x) scale)
                             :pan-dy (/ (- y pan-start-y) scale)
                             :has-panned true}))
      (get draggers id) (update-in
                         db
                         [:shared :draggers id]
                         #(merge % {:drag-chunk-dx (/ (- x drag-chunk-start-x) scale)
                                    :drag-chunk-dy (/ (- y drag-chunk-start-y) scale)
                                    :dragged true}))
      :else db)))


(defn finish-pan [{{:keys [left top pan-dx pan-dy] :as ldb} :local :as  db}]
  (-> db
      (deep-merge
       {:local {:is-panning false
                :pan-start-x 0
                :pan-start-y 0
                :left (+ left pan-dx)
                :top (+ top pan-dy)
                :pan-dx 0
                :pan-dy 0}})))


(defn finish-drag [{ldb :local
                    {:keys [id] :as udb} :user :as db}
                   drag-chunk
                   drag-chunk-dx
                   drag-chunk-dy] 
  (let
   [new-db (-> db
               (update-in [:shared :draggers] #(dissoc % id))
               (update-in [:shared :chunks drag-chunk :loc-x] (partial + drag-chunk-dx))
               (update-in [:shared :chunks drag-chunk :loc-y] (partial + drag-chunk-dy))
               ((fn [db] (update db :shared #(merge-chunks % ldb drag-chunk)))))
    before (get-in db [:shared :piece->chunk])
    after (get-in new-db [:shared :piece->chunk])]
   (merge 
    {:db new-db}
    (if (not= before after) {:play-sound []} {}))))

(defn rotate-chunk [db chunk-index direction]
  {:db (update-in db [:shared :chunks chunk-index :rotation] #(mod (+ % direction) 4))
   :dispatch [:rotate-piece chunk-index]})

(defn rotate-x-y [db x y direction]
  (if-let [c (find-chunk db [x y])]
    (rotate-chunk db (:index c) direction)
    {:db db}))

(defn mouse-up [{{:keys [has-panned pan-start-x pan-start-y] :as ldb} :local
                 {:keys [draggers] :as sdb} :shared
                 {:keys [id] :as udb} :user :as db}
                right-click?]
  (let [{:keys [drag-chunk drag-chunk-dx drag-chunk-dy dragged]} (get draggers id)
        clean-db (assoc-in (finish-pan db) [:shared :draggers] {})]
    (cond (and drag-chunk (not dragged)) (rotate-chunk clean-db drag-chunk (if right-click? 1 -1))
          drag-chunk (finish-drag clean-db drag-chunk drag-chunk-dx drag-chunk-dy)
          (and (not has-panned) right-click?) (rotate-x-y clean-db pan-start-x pan-start-y 1)
          :else {:db clean-db})))

