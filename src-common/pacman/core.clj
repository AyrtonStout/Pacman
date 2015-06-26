(ns pacman.core
  (:use [play-clj.repl])
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.ui :refer :all]))

(def background (atom nil))
(def key-pressed (atom {:up    false
                        :down  false
                        :right false
                        :left  false}))

(defn within?
  [num low high]
  (and (>= num low) (<= num high)))

(defn get-color-at [x y]
  (pixmap! @background :get-pixel x (- (game :height) y)))

(defn corridor?
  [x y]
  (within? (get-color-at x y) 255 1119215))

(defn valid-move-forward
  "Returns true if the next pixel in front of the entity is not a wall"
  [entity]
  (case (:direction entity)
    :down (corridor? (+ (:x entity) 13) (- (:y entity) 1))
    :up (corridor? (+ (:x entity) 13) (- (+ (:height entity) (:y entity)) -1))
    :right (corridor? (+ (:x entity) (:width entity) 1) (+ (:y entity) 13))
    :left (corridor? (- (:x entity) 2) (+ (:y entity) 13))
    :none false))

(defn valid-turn
  [entity direction]
  (case direction
    :down (and
            (every? #(corridor? (+ (:x entity) %) (- (:y entity) 11)) (range 0 (:width entity)))
            (corridor? (:x entity) (- (:y entity) 3))
            (corridor? (+ (:x entity) (:width entity)) (- (:y entity) 3)))
    :up (and
          (every? #(corridor? (+ (:x entity) %) (+ (:y entity) (:height entity) 11)) (range 0 (:width entity)))
          (corridor? (:x entity) (+ (:y entity) (:height entity) 3))
          (corridor? (+ (:x entity) (:width entity)) (+ (:y entity) (:height entity) 3)))
    :right (and
             (every? #(corridor? (+ (:x entity) (:width entity) 13) (+ (:y entity) %)) (range 0 (:height entity)))
             (corridor? (+ (:x entity) (:width entity) 2) (:y entity))
             (corridor? (+ (:x entity) (:width entity) 2) (+ (:y entity) (:height entity))))
    :left (and
            (every? #(corridor? (- (:x entity) 13) (+ (:y entity) %)) (range 0 (:height entity)))
            (corridor? (- (:x entity) 3) (:y entity))
            (corridor? (- (:x entity) 3) (+ (:y entity) (:height entity))))
    false))

(defn move-entity
  "When called without parameters, moves the entity forward 1px"
  ([entity]
   (move-entity entity (get entity :direction)))
  ([entity direction]
   (if (valid-move-forward entity)
     (case direction
       :down (assoc entity :y (dec (:y entity)))
       :up (assoc entity :y (inc (:y entity)))
       :right (assoc entity :x (inc (:x entity)))
       :left (assoc entity :x (dec (:x entity)))
       nil)
     (assoc entity :direction :none))))

(defn change-direction
  [pacman]
  (let [direction (get (clojure.set/map-invert @key-pressed) true)]
    (if (valid-turn pacman direction)
      (case direction
        :down (-> pacman
                  (assoc :direction direction)
                  (assoc :angle 270))
        :up (-> pacman
                (assoc :direction direction)
                (assoc :angle 90))
        :right (-> pacman
                   (assoc :direction direction)
                   (assoc :angle 0))
        :left (-> pacman
                  (assoc :direction direction)
                  (assoc :angle 180)))
      pacman)))

(defn move-AI
  [entity]
  (if (= (:direction entity) :none)
    (case (rand-int 4)
      0 (if (valid-turn entity :down)
          (assoc entity :direction :down)
          entity)
      1 (if (valid-turn entity :up)
          (assoc entity :direction :up)
          entity)
      2 (if (valid-turn entity :right)
          (assoc entity :direction :right)
          entity)
      3 (if (valid-turn entity :left)
          (assoc entity :direction :left)
          entity))
    (move-entity entity)))


(defscreen main-screen
           :on-show
           (fn [screen entities]
             (add-timer! screen :systick 1 0.0111111111)
             (add-timer! screen :animation 1 0.075)
             (update! screen :renderer (stage))
             (reset! background (pixmap "background.png"))

             (let [background (assoc (texture "background.png") :type :ui, :x 0 :y 0)
                   pacman (assoc (texture "pacman-sheet.png") :id :pacman, :type :player, :x 208, :y 206, :width 26, :height 26, :direction :right)
                   blinky (assoc (texture "blinky.png") :id :blinky, :type :ghost, :x 75, :y 350, :width 26, :height 26, :direction :none)
                   pinky (assoc (texture "pinky.png") :id :blinky, :type :ghost, :x 135, :y 350, :width 26, :height 26, :direction :none)
                   inky (assoc (texture "inky.png") :id :blinky, :type :ghost, :x 195, :y 350, :width 26, :height 26, :direction :none)
                   clyde (assoc (texture "clyde.png") :id :blinky, :type :ghost, :x 255, :y 350, :width 26, :height 26, :direction :none)
                   score-text (assoc (label "Score: 1337" (color :white)) :type :ui, :x 35, :y 484)]
               [background pacman blinky pinky inky clyde score-text]))

           :on-render
           (fn [screen entities]
             (clear!)
             (render! screen entities))

           ;;Pac-Man is always the 1st indexed entity
           :on-key-down
           (fn [screen entities]
             (cond
               (= (:key screen) (key-code :dpad-up))
               (swap! key-pressed #(assoc-in % [:up] true))
               (= (:key screen) (key-code :dpad-down))
               (swap! key-pressed #(assoc-in % [:down] true))
               (= (:key screen) (key-code :dpad-right))
               (swap! key-pressed #(assoc-in % [:right] true))
               (= (:key screen) (key-code :dpad-left))
               (swap! key-pressed #(assoc-in % [:left] true)))
             entities)

           :on-key-up
           (fn [screen entities]
             (cond
               (= (:key screen) (key-code :dpad-up))
               (swap! key-pressed #(assoc-in % [:up] false))
               (= (:key screen) (key-code :dpad-down))
               (swap! key-pressed #(assoc-in % [:down] false))
               (= (:key screen) (key-code :dpad-right))
               (swap! key-pressed #(assoc-in % [:right] false))
               (= (:key screen) (key-code :dpad-left))
               (swap! key-pressed #(assoc-in % [:left] false)))
             entities)

           :on-touch-down
           (fn [screen entities]
             (cond
               (> (game :y) (* (game :height) (/ 2 3)))
               (println "up")
               (< (game :y) (/ (game :height) 3))
               (println "down")
               (> (game :x) (* (game :width) (/ 2 3)))
               (println "right")
               (< (game :x) (/ (game :width) 3))
               (println "left")))

           :on-timer
           (fn [screen entities]
             (case (:id screen)
               :systick
               (reduce (fn [coll e]
                         (conj coll
                               (case (:type e)
                                 :ghost (move-AI e)
                                 :player (move-entity (change-direction e))
                                 e)))
                       []
                       entities)
               :animation
               (if (not= (:direction (second entities)) :none)
                 (let [pacman (second entities)]
                   (texture! pacman :set-region-x (mod (+ (texture! (second entities) :get-region-x) 32) 96))
                   (texture! pacman :set-region-width 32)
                   )
                 entities)
               )))

(defgame pacman-game
         :on-create
         (fn [this]
           (set-screen! this main-screen)))


