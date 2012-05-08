(ns ex1
  (:import (java.util.concurrent LinkedBlockingQueue))
  (:import (java.io FileReader BufferedReader)))

(defn calc [vs q]
  (let [result (reduce + vs)]
    ;; (println result)
    (. q put result)
    result))

(defn process-input [path]
  (let [fr (new FileReader path)
        br (rest (seq (line-seq (new BufferedReader fr))))]
    (take 3 br))
  )

(defn -main [path]
  (let [q (new LinkedBlockingQueue)
        results (pmap calc [(range 10 15) (range 20 25)] [q q])]
    (println q path)
    (println (process-input path)))
    (shutdown-agents)
  )
