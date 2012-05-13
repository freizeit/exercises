(ns fpar
  (require calc)
  (:import (java.io FileReader BufferedReader)))


(defn -main [path]
  (let [_fr (new FileReader path)
        br (rest (line-seq (new BufferedReader _fr))) ;; ignore 1st line
    ;; iterate through the lines in blocks of 3; also pass a 1-based task index
    ;; to the solve() function. The index will be used to format the result.
        fs (for [args (map vector (iterate inc 1) (partition 3 br))] (future (calc/vsolve args)))]
    (loop [outstanding (doall fs)]
      (if (not (nil? outstanding))
        (let [[done, not-done] (map seq (split-with future-done? outstanding))]
          ;;(println "done     : " done)
          ;;(println "not-done : " not-done)
          (doseq [line done] (println @line))
          (recur not-done)))))
    (shutdown-agents)
  )
