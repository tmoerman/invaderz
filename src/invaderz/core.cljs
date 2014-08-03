(ns invaderz.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [cemerick.pprng :as rng]))

;; PARAMETERS

(def square-px 10)
(def invader-size 5)
(def matrix-size 10)
(def rng (rng/rng))

(def max-invader-id
  (dec (Math/pow 2 (* invader-size (Math/ceil (/ invader-size 2))))))

;; GENERATE

(defn leading-zeros [length dna]
  (concat (repeat (- length (count dna)) 0) dna))

(defn to-invader-dna [length invader-id]
  (->> (.toString invader-id 2)
       (map js/parseInt)
       (leading-zeros length)))

(defn unfold [chromosome]
  (->> (take (dec (count chromosome)) chromosome)
       (reverse)
       (concat chromosome)))

(defn unfold-all [width dna]
  (->> dna
       (partition width)
       (map unfold)))

(defn spawn-invader [size invader-id]
  (let [width  (Math/ceil (/ size 2))
        height size
        length (* width height)]
    (->> invader-id
         (to-invader-dna length)
         (unfold-all width))))

;; DRAW INSTRUCTIONS

(defn to-draw-instructions
  [invader]
  (->> invader
       (map-indexed (fn [y row]
                      (->> row
                           (map-indexed (fn [x i]
                                          {:y y
                                           :x x
                                           :draw i})))))
       (flatten)))

;; SETUP

(defn setup []
  (q/smooth)
  (q/frame-rate 10)
  {:invaders {}})

;; UPDATE

(defn random-coords [matrix-size]
  [(rng/int rng matrix-size)
   (rng/int rng matrix-size)])

(defn random-invader-id [max-invader-id]
  (rng/int rng max-invader-id))

(defn random-rgb []
  [(rng/int rng 255) (rng/int rng 255) (rng/int rng 255)])

(defn update [state]
  (let [{:keys [invaders]} state
        rand-x ()
        rand-y ()]
    {:invaders
      (assoc invaders
        (random-coords matrix-size) {:rgb (random-rgb)
                                     :id (random-invader-id max-invader-id)})}))

;; DRAW

(defn draw-invader [square-px translation-coords rgb invader]
  (q/with-translation (->> translation-coords
                           (map (partial * square-px (+ invader-size 2)))
                           (map (partial + square-px)))

    (->> invader
         (to-draw-instructions)
         (map (fn [{:keys [x y draw]} row]
                (if (= draw 1)
                  (do
                    (apply q/fill rgb)
                    (q/rect (* x square-px)
                            (* y square-px)
                            square-px
                            square-px)))))
         dorun)))

(spawn-invader 5 999)



(defn draw [state]
  (q/background 20)
  (q/no-stroke)
  (->> (:invaders state)
       (seq)
       (map (fn [[translation-coords {:keys [id rgb]}]]
                (->> (spawn-invader invader-size id)
                     (draw-invader square-px translation-coords rgb))))
       (dorun)))

;; SKETCH

(q/defsketch invaderz
  :host "invaderz"
  :size [700 700]
  :setup setup
  :update update
  :draw draw
  :middleware [m/fun-mode])
