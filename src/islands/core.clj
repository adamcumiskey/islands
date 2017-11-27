(ns islands.core
  (:gen-class))

; generate data
(def gen-rand-node
  #(if (= (mod (rand-int 30) 3) 0) 1 0))

(defn gen-row [n gen-node-fn]
  (vec (take n (repeatedly gen-node-fn))))

(defn gen-grid [n m gen-node-fn]
  (vec (take n (repeatedly #(gen-row m gen-node-fn)))))

; check for land
(defn is-land? [grid x y]
  ; handling nil here greatly simplifies the `mark-island` method
  (if (or (nil? x) (nil? y))
    false
    (= 1 (get-node grid x y))))

; getter
(defn get-node [grid x y]
  ((grid y) x))

; setter
(defn mark [grid x y value]
  (assoc grid y (assoc (grid y) x value)))

(defn mark-land [grid x y]
  (mark grid x y -1))

; print functions
(defn node-str [node]
  (cond 
   (= node 1) "❎"
   (= node 0) "🚹"
   (= node -1) "🚼"))

(defn row-str [row] 
  (clojure.string/join (interpose " " (map (fn [node] (node-str node)) row))))

(defn grid-str [grid]
  (clojure.string/join (interpose "\n" (map (fn [row] (row-str row)) grid))))

(defn print-grid [grid]
  (println (grid-str grid)))

; returns a set of all the indecies surrounding a given index
(defn adjacent-nodes [grid x y]
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

; return a grid with the landmass connected to (x y) as visited
(defn mark-island [grid x y]
  (if-not (is-land? grid x y) 
    grid
    (loop [x x
           y y 
           queue (adjacent-nodes grid x y)
           visited #{} 
           grid grid]
      (if (and (empty? queue) (or (nil? x) (nil? y)))
        grid
        (let [next-x (first (first queue))
              next-y (second (first queue))
              visited (clojure.set/union #{(list x y)} visited)]
          (if (is-land? grid x y) 
            (recur next-x 
                   next-y 
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

; return the total number of islands in the ocean
(defn count-islands [grid]
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

