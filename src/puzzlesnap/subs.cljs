(ns puzzlesnap.subs 
  (:require [re-frame.core :as rf]))

;;subscriptions
(rf/reg-sub
 :db
 identity)

(rf/reg-sub
 :image-uri
 #(get-in % [:image-uri]))