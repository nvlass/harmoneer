(ns harmoneer.core
  (:require [reagent.dom.client :as rdc]
            [re-frame.core :as rf]
            [re-frame.db :refer [app-db]]
            [clojure.string :as str]
            [harmoneer.events]
            [harmoneer.subs]
            [harmoneer.views :as views]
            [harmoneer.audio :as audio]
            [harmoneer.storage :as storage]))

(defonce root (atom nil))

(defn ^:dev/after-load mount []
  (rf/clear-subscription-cache!)
  (when-not @root
    (reset! root (rdc/create-root (js/document.getElementById "app"))))
  (rdc/render @root [views/app]))

(def captured #{"ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight" " " "Tab" "Backspace"})

(defn- on-key [e]
  (let [tag (some-> (.-target e) .-tagName str/lower-case)
        k (.-key e)
        mod (or (.-metaKey e) (.-ctrlKey e))]
    (when-not (or (#{"input" "select" "textarea"} tag) (.-altKey e)
                  (and (= tag "button") (#{" " "Enter"} k)))
      (when (and (captured k) (not mod) (= :write (:view @app-db)) (:exercise @app-db)) (.preventDefault e))
      (when (and mod (#{"z" "y"} (str/lower-case k))) (.preventDefault e))
      (rf/dispatch [:key (if mod (str/lower-case k) k) {:shift (.-shiftKey e) :mod mod}]))))

(defn init []
  (rf/dispatch-sync [:init])
  (rf/dispatch-sync [:resize (.-innerWidth js/window)])
  (storage/load-all! [:settings :exercise] #(rf/dispatch [:storage/loaded %]))
  (audio/install-unlock!)
  (audio/load! #(rf/dispatch [:audio/loaded %]))
  (.addEventListener js/document "keydown" on-key)
  (.addEventListener js/window "resize" #(rf/dispatch [:resize (.-innerWidth js/window)]))
  (when (and (not ^boolean goog.DEBUG) (.-serviceWorker js/navigator))
    (.register (.-serviceWorker js/navigator) "sw.js"))
  (mount))
