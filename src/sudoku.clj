(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (cond
   (= 0 (value-at board coord)) false
   :else true))

(defn row-values [board coord]
  (set (get board (first coord))))


(defn col-values [board coord]
 (set (map #(value-at board [% (last coord)]) (range 9))))

(defn coord-pairs [coord]
(for [x coord y coord] [x y]))

(defn block-topleft [[r c]]
  [(* 3 (int (/ r 3))) (* 3 (int (/ c 3)))])


(defn block-values [board [r c]]
  (let [[top left] (block-topleft [r c])
        values (for [row (range top (+ top 3))
                     col (range left (+ left 3))]
                 (value-at board [row col]))]
    (set values)))

(defn valid-values-for [board coord]
  (cond
   (has-value? board coord) #{}
   :else (let [used-values (set/union (row-values board coord)
                                      (col-values board coord)
                                      (block-values board coord))]
           (set/difference all-values used-values))))


(defn filled? [board]
  (every? (fn [coord] (has-value? board coord)) (coord-pairs (range 9))))

(defn rows [board]
  (vec (map (fn [row] (set row)) board)))

(defn valid-rows? [board]
  (every? true? (for [n (range 9)]
                  (= (get (rows board) n) all-values))))

(defn cols [board]
 (vec (for [col (range 9)]
         (set (map (fn [row] (get row col)) board)))))

(defn valid-cols? [board]
  (every? true? (for [n (range 9)]
                  (= (get (cols board) n) all-values))))


(defn blocks [board]
  (vec (for [col [0 3 6]
             row [0 3 6]]
         (block-values board [col row]))))

(defn valid-blocks? [board]
  (every? true? (for [n (range 9)]
                  (= (get (blocks board) n) all-values))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))


(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (for [row (range 9)
                     col (range 9)]
                 [row col])]
    (some (fn [coord] (if (has-value? board coord)
                        false
                        coord))
          coords)))

(defn solve [board]
  (let [empty-point (find-empty-point board)]
    (if (not empty-point)
      (if (valid-solution? board)
        board
        [])
      (some not-empty (for [value (valid-values-for board empty-point)]
                           (solve (set-value-at board empty-point value)))))))


