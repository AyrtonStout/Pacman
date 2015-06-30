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
  (within? (get-color-at x y) 0 1119215))

(defn valid-move-forward
  "Returns true if the next pixel in front of the entity is not a wall"
  [entity]
  (case (:direction entity)
    :down (corridor? (+ (:x entity) 13) (- (:y entity) 1))
    :up (corridor? (+ (:x entity) 13) (- (+ (:height entity) (:y entity)) -1))
    :right (corridor? (+ (:x entity) (:width entity) 1) (+ (:y entity) 13))
    :left (corridor? (- (:x entity) 2) (+ (:y entity) 13))
    :none false))

(defn spawn-clones
  [entities1]
  (loop [entities entities1 i (dec (count entities1))]
    (if (< i 0)
      entities
      (let [curr (nth entities i)]
        (cond
          (= (:x curr) 0) (recur (conj entities (assoc (texture (:object curr)) :id (:id curr), :type (:type curr), :x (- (game :width) 1), :y (:y curr), :width 26, :height 26, :direction :left, :cooldown 10)) (dec i))
          (= (:x curr) (game :width)) (recur entities (dec i))
          :else (recur entities (dec i)))))))

(defn valid-turn
  [entity direction]
  (case direction
    :down (and
            (every? #(corridor? (:x entity) (- (:y entity) %)) (range 0 20))
            (every? #(corridor? (+ (:x entity) (:width entity)) (- (:y entity) %)) (range 0 20))
            (every? #(corridor? (+ (:x entity) (/ (:width entity) 2)) (- (:y entity) %)) (range 0 20)))
    :up (and
          (every? #(corridor? (:x entity) (+ (:y entity) (:height entity) %)) (range 0 20))
          (every? #(corridor? (+ (:x entity) (:width entity)) (+ (:y entity) (:height entity) %)) (range 0 20))
          (every? #(corridor? (+ (:x entity) (/ (:width entity) 2)) (+ (:y entity) (:height entity) %)) (range 0 20)))
    :right (and
             (every? #(corridor? (+ (:x entity) (:width entity) %) (:y entity)) (range 0 20))
             (every? #(corridor? (+ (:x entity) (:width entity) %) (+ (:y entity) (:height entity))) (range 0 20))
             (every? #(corridor? (+ (:x entity) (:width entity) %) (+ (:y entity) (/ (:height entity) 2))) (range 0 20)))
    :left (and
            (every? #(corridor? (- (:x entity) %) (:y entity)) (range 0 20))
            (every? #(corridor? (- (:x entity) %) (+ (:y entity) (:height entity))) (range 0 20))
            (every? #(corridor? (- (:x entity) %) (+ (:y entity) (/ (:height entity) 2))) (range 0 20)))
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
  [ghost]
  (if (<= (:cooldown ghost) 0)
    (let [directions (atom [])]
      (do
        (when (and (valid-turn ghost :down) (not= (:direction ghost) :up))
          (swap! directions conj :down))
        (when (and (valid-turn ghost :up) (not= (:direction ghost) :down))
          (swap! directions conj :up))
        (when (and (valid-turn ghost :left) (not= (:direction ghost) :right))
          (swap! directions conj :left))
        (when (and (valid-turn ghost :right) (not= (:direction ghost) :left))
          (swap! directions conj :right))
        (when (valid-move-forward ghost)
          (swap! directions conj (:direction ghost)))

        (if (> (count @directions) 0)
          (let [index (rand-int (count @directions))]
            (if (not= (nth @directions index) (:direction ghost))
              (-> ghost
                  (assoc :direction (nth @directions index))
                  (assoc :cooldown 10)
                  (move-entity))
              (move-entity ghost)))
          ghost)))
    (if (valid-move-forward ghost)
      (-> ghost
          (assoc :cooldown (dec (:cooldown ghost)))
          (move-entity))
      (assoc ghost :cooldown (dec (:cooldown ghost))))))

(defscreen main-screen
           :on-show
           (fn [screen entities]
             (add-timer! screen :systick 1 0.01)
             (add-timer! screen :animation 1 0.075)
             (update! screen :renderer (stage))
             (reset! background (pixmap "background.png"))

             (let [background (assoc (texture "background.png") :type :ui, :x 1 :y 0)
                   pacman (assoc (texture "pacman-sheet.png") :id :pacman, :type :player, :x 208, :y 206, :width 26, :height 26, :direction :right)
                   ;(texture! pacman :set-region-x 64)
                   ;(texture! pacman :set-region-width 32)
                   blinky (assoc (texture "blinky.png") :id :blinky, :type :ghost, :x 75, :y 350, :width 26, :height 26, :direction :none, :cooldown 0)
                   pinky (assoc (texture "pinky.png") :id :pinky, :type :ghost, :x 135, :y 350, :width 26, :height 26, :direction :none, :cooldown 0)
                   inky (assoc (texture "inky.png") :id :inky, :type :ghost, :x 195, :y 350, :width 26, :height 26, :direction :none, :cooldown 0)
                   clyde (assoc (texture "clyde.png") :id :clyde, :type :ghost, :x 255, :y 350, :width 26, :height 26, :direction :none, :cooldown 0)
                   score-text (assoc (label "Score: 1337" (color :white)) :type :ui, :x 35, :y 484)]
               [background pacman blinky pinky inky clyde score-text]))

           :on-render
           (fn [screen entities]
             (clear!)
             (render! screen entities))

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
               (let [new-entities (spawn-clones entities)]
                 (reduce (fn [coll e]
                           (conj coll
                                 (case (:type e)
                                   :ghost (move-AI e)
                                   :player (move-entity (change-direction e))
                                   e)))
                         []
                         new-entities))
               :animation
               (reduce (fn [coll e]
                         (conj coll
                              (case (:type e)
                                 :ghost (identity e)
                                 :player (if (not= (:direction e) :none)
                                           (do
                                             (texture! e :set-region-x (mod (+ (texture! e :get-region-x) 32) 96))
                                             (texture! e :set-region-width 32)
                                             e)
                                           e)
                                 :ui (identity e)
                                 e)))
                       []
                       entities))))

(defgame pacman-game
         :on-create
         (fn [this]
           (set-screen! this main-screen)))

