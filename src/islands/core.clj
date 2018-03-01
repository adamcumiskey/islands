(ns islands.core
  (:gen-class))

(def gen-rand-node
  "Generates a random 0 or 1
  Adjust the modulus to change frequency of 1 values"
  #(if (= (mod (rand-int 30) 3) 0) 1 0))

(defn gen-row [n gen-node-fn]
  (vec (take n (repeatedly gen-node-fn))))

(defn gen-grid
  "Generate a grid"
  ([n] (gen-grid n n))
  ([n m] (gen-grid n m gen-rand-node))
  ([n m gen-node-fn]
   (vec (take n (repeatedly #(gen-row m gen-node-fn))))))

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
  (if-not (is-land? grid x y)
    grid
    (let [result (reduce (fn [s n] (mark-island s (first n) (second n)))
                         (mark-land grid x y)
                         (adjacent-nodes grid x y))]
      result)))

(defn count-islands [grid]
  "Return the total number of islands in the grid"
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
