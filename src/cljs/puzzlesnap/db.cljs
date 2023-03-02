(ns puzzlesnap.db)

(def default-db
  {:user {:id "userId1" :name "Cantaloupe"}
   :global {:draw-size 100
            :image-uri "/img/puzzle.jpg"
            :puzzle-width 2
            :puzzle-height 2}
   :local {:id 0
           :left 100
           :top 100
           :is-panning false
           :pan-start-x 0
           :pan-start-y 0
           :pan-dx 0
           :pan-dy 0
           :scale 0.5
           :image-loaded false
           :image-width 0
           :image-height 0}
   :shared {}})

#_(def db-proposal
  {:user
   {:id "userId1" :name "Cantaloupe"}
   :global
   {:image-uri "/img/puzzle.jpg"
    :puzzle-width 2
    :puzzle-height 1
    :tabs {:top-bottom [[[{:x   0 :y   0 :cx1 0 :cy1 0 :cx2 0 :cy2 0}
                          {:x   415.43898855449333 :y   0 :cx1 415.43898855449333 :cy1 0 :cx2 415.43898855449333 :cy2 0}]
                         [{:x   0 :y   0 :cx1 0 :cy1 0 :cx2 0 :cy2 0}
                          {:x   424.2833064772323 :y   0 :cx1 424.2833064772323 :cy1 0 :cx2 424.2833064772323 :cy2 0}]]
                        [[{:x   15.43898855449332 :y   0 :cx1 15.43898855449332 :cy1 0 :cx2 15.43898855449332 :cy2 0}
                          {:x   400 :y   0 :cx1 400 :cy1 0 :cx2 400 :cy2 0}]
                         [{:x   24.28330647723233 :y   0 :cx1 24.28330647723233 :cy1 0 :cx2 24.28330647723233 :cy2 0}
                          {:x   400 :y   0 :cx1 400 :cy1 0 :cx2 400 :cy2 0}]]]
           :left-right [[[{:x   0 :y   0 :cx1 0 :cy1 0 :cx2 0 :cy2 0}
                          {:x   0 :y   566 :cx1 0 :cy1 566 :cx2 0 :cy2 566}]]
                        [[{:x   15.43898855449332 :y   0 :cx1 15.43898855449332 :cy1 0 :cx2 15.43898855449332 :cy2 0}
                        ; ...
                          ]]
                        [[{:x   0 :y   0 :cx1 0 :cy1 0 :cx2 0 :cy2 0}
                          {:x   0 :y   566 :cx1 0 :cy1 566 :cx2 0 :cy2 566}]]]}}
   :shared {:chunk-order        [1 0]
            :piece->chunk       {[0 0] 0
                                 [1 0] 1}
            :chunks             [{:main-piece-i 0
                                  :main-piece-j 0
                                  :index        0
                                  :loc-x        -38.73024503379106
                                  :loc-y        100
                                  :piece-grid   #{[0 0]}}
                                 {:main-piece-i 1
                                  :main-piece-j 0
                                  :index        1
                                  :loc-x        436.08335962047397
                                  :loc-y        100
                                  :piece-grid   #{[1 0]}}]
            :users [{:id "userId1" :name "Cantaloupe"}]
            :draggers {"userId1" {:drag-chunk 2
                                  :drag-chunk-start-x 0
                                  :drag-chunk-start-y 0
                                  :drag-chunk-dx 0
                                  :drag-chunk-dy 0}}}
   :local {:id 0
           :left 100
           :top 100
           :is-panning false
           :pan-start-x 0
           :pan-start-y 0
           :pan-dx 0
           :pan-dy 0
           :scale 0.5
           :piece-width 100
           :piece-height 200
           :image-loaded false
           :image-height 566
           :image-width 800}})