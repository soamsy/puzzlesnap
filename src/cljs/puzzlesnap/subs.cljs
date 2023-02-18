(ns puzzlesnap.subs 
  (:require [re-frame.core :as rf]))

;;subscriptions
(rf/reg-sub
 :canvas
 #(get-in % [:canvas]))

(rf/reg-sub
 :canvas/image-uri
 #(get-in % [:canvas :image-uri]))