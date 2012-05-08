(ns ex1
  (:import (java.io FileReader BufferedReader)))

(defn find-match
  "Finds two prices whose sum equals the credit and returns these as a vector.
   Otherwise nil is returned."
  [c ps]
  (if (< (count ps) 2)  ;; we need to have at least two prices
    nil ;; no solution
    (let [fp (first ps) rps (rest ps)
          remainder (seq (drop-while #(not (= (+ fp %) c)) rps))]
      (if (not (nil? remainder))
          [fp (first remainder)]  ;; we have a solution
          (recur c rps)))))       ;; drop the 1st price and try again

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
          solution (find-match credit prices)]
      (println "idx     : " idx)
      (println "credit  : " credit)
      (println "prices  : " prices)
      (println "solution: " solution))
    )

(defn -main [path]
  (let [_fr (new FileReader path)
        br (rest (line-seq (new BufferedReader _fr)))] ;; ignore 1st line
    (doall (map solve (iterate inc 1) (partition 3 br))))
    (shutdown-agents)
  )
