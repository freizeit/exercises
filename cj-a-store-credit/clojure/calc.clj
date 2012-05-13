(ns calc
  (:import (java.io FileReader BufferedReader)))


(defn sum-prices
  "Sums up two prices and ignores their indices."
  [[_ p1] [_ p2]] (+ p1 p2))


(defn find-match
  "Finds two prices whose sum equals the credit and returns these as a vector.
   Otherwise returns nil."
  [c ps]
  (if (< (count ps) 2)  ;; at least two prices are needed for a solution
    nil ;; no solution
    (let [fp (first ps) rps (rest ps)
          remainder (seq (drop-while #(not (= (sum-prices fp %) c)) rps))]
      (if (not (nil? remainder))
          ;; we have a solution, return the 1-based indices
          (let  [[i1 _] fp [i2 _] (first remainder)]
            [(inc i1) (inc i2)])
          ;; no solution for this price, try the remaining ones
          (recur c rps)))))


(defn solve
    "Finds a solution for a block of 3 lines that hold the store credit,
     the number of items in the store and the prices for the latter
     respectively."
    [idx bo3]
    ;; line 1 and 3 hold the store credit and the item prices respectively.
    (let [[l1 _ l3] bo3
          credit (. Integer parseInt l1)  ;; convert to int
          ;; split the string with the prices and convert the latter to int
          prices (map #(. Integer parseInt %) (re-seq #"\d+" l3))
          ;; each price needs an index since only the latter is to appear in
          ;; the solution
          solution (find-match credit (map-indexed vector prices))]
      (if (nil? solution)
        (format "Case #%d: no solution found" idx)
        (let [[i1 i2] solution]
          (format "Case #%d: %d %d" idx i1 i2)))
    ))


(defn vsolve
    "Finds a solution for a block of 3 lines that hold the store credit,
     the number of items in the store and the prices for the latter
     respectively."
    [[idx bo3]] (solve idx bo3))
