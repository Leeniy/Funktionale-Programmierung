(ns apples.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]))

(declare apples main-screen)
(def speed 14)

(defn- get-direction []
  (cond
    (key-pressed? :dpad-left) :left
    (key-pressed? :dpad-right) :right))

(defn- update-player-position [{:keys [player?] :as entity}]
  (if player?
    (let [direction (get-direction)
          new-x (case direction
                  :right (+ (:x entity) speed)
                  :left (- (:x entity) speed))]
      (when (not= (:direction entity) direction)
        (texture! entity :flip true false))
      (assoc entity :x new-x :direction direction))
    entity))

(defn- update-hit-box [{:keys [player? apple?] :as entity}]
  (if (or player? apple?)
    (assoc entity :hit-box (rectangle (:x entity) (:y entity) (:width entity) (:height entity)))
    entity))

(defn- remove-touched-apples [entities]
  (if-let [apples (filter #(contains? % :apple?) entities)]
    (let [player (some #(when (:player? %) %) entities)
          touched-apples (filter #(rectangle! (:hit-box player) :overlaps (:hit-box %)) apples)]
      (remove (set touched-apples) entities))
    entities))

(defn- move-player [entities]
  (->> entities
       (map (fn [entity]
              (->> entity
                   (update-player-position)
                   (update-hit-box))))
       remove-touched-apples))

(defn- spawn-apple []
  (let [x (+ 50 (rand-int 1400))
        y (+ 50 (rand-int 30))]
    (assoc (texture "apple.png") :x x, :y y, :width 50, :height 65, :apple? true)))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (add-timer! screen :spawn-apple 1 2)
    (let [background (texture "apple_orchard.png")
          player (assoc (texture "cow.png") :x 50, :y 50, :width 400, :height 350, :player? true, :direction :left)]
      [background player]))

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-key-down
  (fn [screen entities]
    (cond
     (key-pressed? :r) (app! :post-runnable #(set-screen! apples main-screen))
     (get-direction) (move-player entities)
     :else entities))

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :spawn-apple (conj entities (spawn-apple)))))

(defgame apples-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))