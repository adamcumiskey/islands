(ns islands.core
  (:gen-class))

(def gen-rand-node
  "Generates a random 0 or 1
  Adjust the modulus to change frequency of 1 values"
  #(if (= (mod (rand-int 30) 3) 0) 1 0))

(defn gen-row [n gen-node-fn]
  "Generate a row"
  (vec (take n (repeatedly gen-node-fn))))

(defn gen-grid [n m gen-node-fn]
  "Generate a grid"
  (vec (take n (repeatedly #(gen-row m gen-node-fn)))))

(defn get-node [grid x y]
  "Return the element at x y"
  ((grid y) x))

(defn is-land? [grid x y]
  "Check for land. 
  Handling nil here greatly simplifies the `mark-island` method"
  (if (or (nil? x) (nil? y))
    false
    (= 1 (get-node grid x y))))

(defn mark [grid x y value]
  "Set the element at x y"
  (assoc grid y (assoc (grid y) x value)))

(defn mark-land [grid x y]
  (mark grid x y -1))

; print functions
(defn node-str [node]
  "Tiles used to represent nodes in the print functions
  1 is land
  0 is ocean
  -1 is visited"
  (cond
    (= node 1) "❎"
    (= node 0) "🚹"
    (= node -1) "🚼"))

(defn row-str [row]
  "Return a row as a string"
  (clojure.string/join (interpose " " (map (fn [node] (node-str node)) row))))

(defn grid-str [grid]
  "Return a grid as a string"
  (clojure.string/join (interpose "\n" (map (fn [row] (row-str row)) grid))))

(defn print-grid [grid]
  "Pretty-print a grid"
  (println (grid-str grid)))

(defn adjacent-nodes [grid x y]
  "returns a set of all the indecies surrounding a given index"
  (let [max-x (count (grid 0))
        max-y (count grid)
        x-1 (- x 1)
        x+1 (+ x 1)
        y-1 (- y 1)
        y+1 (+ y 1)]
    (set
     (remove nil?
             (for [i (range x-1 (+ x+1 1))
                   j (range y-1 (+ y+1 1))]
               (if (and (>= i 0)
                        (< i max-x)
                        (>= j 0)
                        (< j max-y)
                        (or (not= i x) (not= j y)))
                 (list i j)))))))

(defn mark-island [grid x y]
  "Return a grid with the landmass connected to (x y) as visited"
  (if-not (is-land? grid x y)
    grid
    (loop [x x
           y y
           queue (adjacent-nodes grid x y)
           visited #{}
           grid grid]
      (if (and (empty? queue) (or (nil? x) (nil? y))) ; Return when the queue is empty and x y is invalid
        grid
        (let [next-x (first (first queue))
              next-y (second (first queue))
              visited (clojure.set/union #{(list x y)} visited)] ; mark the current x y as visited
          (if (is-land? grid x y)
            (recur next-x
                   next-y
                   ; If a match is found, add all the adjacent nodes to the queue.
                   (-> (set (rest queue))
                       (clojure.set/union (adjacent-nodes grid x y))
                       (clojure.set/difference visited))
                   visited
                   (mark-land grid x y))
            (recur next-x
                   next-y
                   (set (rest queue))
                   visited
                   grid)))))))

(defn count-islands [grid]
  "Return the total number of islands in the ocean"
  (let [max-x (count (grid 0))
        max-y (count grid)]
    (loop [grid grid
           x 0
           y 0
           n 0]
      (cond (>= y max-y) n
            (>= x max-x) (recur grid 0 (inc y) n)
            (is-land? grid x y) (recur (mark-island grid x y) (inc x) y (inc n))
            :else (recur grid (inc x) y n)))))
