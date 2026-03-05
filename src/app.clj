(ns app
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

;; IO
(defn read-file [path]
  (slurp (io/file path)))

;; Shape loading
(defn split-shapes [raw]
  "Split a raw shapes file into individual ASCII shapes"
  (->> (str/split raw #"\n\s*\n")
       (map str/trim)
       (remove str/blank?)))

(defn parse-shape [shape-str]
  "Parse an ASCII shape into a set of coordinates"
  (let [lines (str/split-lines shape-str)]
    (->> lines
         (map-indexed
          (fn [r line]
            (->> line
                 (map-indexed
                  (fn [c ch]
                    (when (= ch \#)
                      [r c])))
                 (remove nil?))))
         (apply concat)
         set)))

;; Shape mutations
(defn normalize-shape [shape]
  "Shift shape coordinates to the [0 0] position"
  (let [min-r (apply min (map first shape))
        min-c (apply min (map second shape))]
    (->> shape
         (map (fn [[r c]]
                [(- r min-r)
                 (- c min-c)])))))

(defn rotate-point [[r c]]
  "Rotate a single point 90 deg around the origin"
  [c (- r)])

(defn reflect-point [[r c]]
  "Reflect a point across the vertical axis"
  [r (- c)])

(defn rotate-shape [shape]
  "Rotate an entire shape 90 deg and normalize it"
  (->> shape
       (map rotate-point)
       set
       normalize-shape))

(defn reflect-shape [shape]
  "Mirror an entire shape and normalize it"
  (->> shape
       (map reflect-point)
       set
       normalize-shape))

(defn rotations [shape]
  "Generate the four rotation of a shape"
  (take 4 (iterate rotate-shape shape)))

(defn orientations [shape]
  "Generate all rotations and reflections of a shape"
  (->> (concat
        (rotations shape)
        (rotations (reflect-shape shape)))
       set))

(defn build-pieces [shapes]
  "Convert raw shapes into solver pieces"
  (->> shapes
       (map-indexed
        (fn [idx shape]
          {:name (keyword (str "p" idx))
           :orientations (orientations (normalize-shape shape))}))
       vec))

;; Board
(defn parse-token [token]
  "Convert a calendar token into an internal keyword

Examples:
\"#\"   -> :blocked
\"Jan\" -> :jan
"
  (if (= token "*")
    :blocked
    (keyword (str/lower-case token))))

(defn parse-board [path]
  "Parse the ASCII calendar board into a matrix of tokens"
  (->> path
       read-file
       str/split-lines
       (map (fn [line]
              (-> line
                  (str/trim)
                  (str/split #"\s+")
                  (->> (map parse-token)
                       vec))))
       vec))

(defn prepare-board [board month day weekday]
  "Prepare the board for solving a specific date

:blocked stays :blocked
selected cells become :target
all other cells become nil

nil represents a free cell that piece may occupy."
  (mapv
   (fn [row]
     (mapv
      (fn [cell]
        (cond
          (= cell :blocked) :blocked
          (or (= cell month)
              (= cell day)
              (= cell weekday)) :target
          :else nil))
      row))
   board))

;; Solving
(defn count-free [board]
  "Count the number of free cells on the board"
  (count
   (for [row board
         cell row
         :when (nil? cell)]
     cell)))

(defn first-empty-cell [board]
  "Find the first free cell in the board"
  (first
   (for [r (range (count board))
         c (range (count (first board)))
         :when (nil? (get-in board [r c]))]
     [r c])))

(defn in-bounds? [board [r c]]
  "Check whether a coordinate lies within board bounds"
  (and (<= 0 r)
       (< r (count board))
       (<= 0 c)
       (< c (count (first board)))))

(defn can-place? [board shape anchor]
  "Check whether a shape can be placed at a given anchor position"
  (every?
   (fn [[r c]]
     (let [pos [(+ (first anchor) r)
                (+ (second anchor) c)]]
       (and (in-bounds? board pos)
            (nil? (get-in board pos)))))
   shape))

(defn place-shape [board shape anchor label]
  "Returns a new board with a placed shape"
  (reduce
   (fn [b [r c]]
     (let [pos [(+ (first anchor) r)
                (+ (second anchor) c)]]
       (assoc-in b pos label)))
   board
   shape))

(defn solve [board pieces depth]
  "Recursive backtracking solver

Algo:
1. Find a free cell
2. Try each remaining piece
3. Try each orientation
4. If placement is valid, recurse"
  ;; (println "depth:" depth "pieces left:" (count pieces))
  (if (empty? pieces)
    board
    (if-let [cell (first-empty-cell board)]
      (some
       (fn [piece]
         (some
          (fn [shape]
            (when (can-place? board shape cell)
              (solve
               (place-shape board shape cell (:name piece))
               (remove #(= % piece) pieces)
               (inc depth))))
          (:orientations piece)))
       pieces)
      board)))

;; Output
(def reset "\u001B[0m")
(def piece-colors
  ["\u001B[31m"  ;; red
   "\u001B[32m"  ;; green
   "\u001B[33m"  ;; yellow
   "\u001B[34m"  ;; blue
   "\u001B[35m"  ;; magenta
   "\u001B[36m"  ;; cyan
   "\u001B[91m"  ;; bright red
   "\u001B[92m"  ;; bright green
   "\u001B[93m"  ;; bright yellow
   "\u001B[94m"  ;; bright blue
   ])
(def misc-colors {:blocked "\u001B[90m"  ;; gray
                  :target  "\u001B[97m"  ;; white
                  })

(defn piece-color [cell]
  "Select a terminal color for a piece by its name"
  (let [n (Integer/parseInt (subs (name cell) 1))]
    (nth piece-colors (mod n (count piece-colors)))))

(defn render-cell [cell]
  "Convert a board cell into a colored terminal string"
  (cond
    (= cell :blocked) (str (misc-colors :blocked) "#" reset)
    (= cell :target) (str (misc-colors :target) "." reset)
    (nil? cell) " "
    :else
    (str (piece-color cell) "█" reset)))

(defn print-board [board]
  (doseq [row board]
    (println
     (apply str
            (map (fn [cell]
                   (str (render-cell cell) ""))
                 row)))))

;; Date utils
(defn parse-date [s]
  (java.time.LocalDate/parse s))

(defn resolve-date []
  (if (empty? *command-line-args*)
    (java.time.LocalDate/now)
    (parse-date (first *command-line-args*))))

;; Init
(defn main []
  (defn load-shapes [path]
    (->> path
         read-file
         split-shapes
         (map parse-shape)))

  (def shapes (load-shapes "resources/shapes.txt"))
  (def pieces (build-pieces shapes))
  (def board (parse-board "resources/board.txt"))

  (def date
    (let [date (resolve-date)]
      [(-> date .getMonth str/lower-case (subs 0 3) keyword)
       (-> date .getDayOfMonth str keyword)
       (-> date .getDayOfWeek str/lower-case (subs 0 2) keyword)]))
  (println "Solving calendar puzzle for" date)

  (def prepared-board (apply prepare-board board date))

  (time (def result (solve prepared-board pieces 0)))

  (when result
    (print-board result)))

(main)