(ns seq
  (require calc)
  (:import (java.io FileReader BufferedReader)))


(defn -main [path]
  (let [_fr (new FileReader path)
        br (rest (line-seq (new BufferedReader _fr))) ;; ignore 1st line
    ;; iterate through the lines in blocks of 3; also pass a 1-based task index
    ;; to the solve() function. The index will be used to format the result.
        output (map calc/solve (iterate inc 1) (partition 3 br))]
    (loop [lines output]
      (if (not (nil? lines))
        (let [h (first lines) t (rest lines)]
          (println h)
          (recur (seq t))))))
    (shutdown-agents)
  )
