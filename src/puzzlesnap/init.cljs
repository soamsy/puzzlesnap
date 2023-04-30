(ns puzzlesnap.init
  (:require 
    [puzzlesnap.events]
    [puzzlesnap.subs]
    [puzzlesnap.views :refer [page]]
    [re-frame.core :as rf]
    [reagent.dom :as rdom]))

(defn ^:dev/after-load mount-root []
  (rf/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [#'page] root-el)))

(defn init! []
  (rf/dispatch [:app/initialize])
  (mount-root))
