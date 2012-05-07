(ns ex1
  (:import (java.util.concurrent LinkedBlockingQueue)))

(defn calc [vs q]
  (let [result (reduce + vs)]
    ;; (println result)
    (. q put result)
    result))

(defn -main [path]
  (let [q (new LinkedBlockingQueue)
        results (pmap calc [(range 10 15) (range 20 25)] [q q])]
    (println q path))
    (. System exit 0)
  )
