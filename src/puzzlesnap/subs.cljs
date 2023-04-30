(ns puzzlesnap.subs 
  (:require [re-frame.core :as rf]))

;;subscriptions
(rf/reg-sub
 :db
 identity)

(rf/reg-sub
 :global
 #(get-in % [:global]))

(rf/reg-sub
 :global/image-uri
 #(get-in % [:global :image-uri]))