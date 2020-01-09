(ns clojush.pushgp.selection.lwlexicase
  (:use [clojush random]))

(defn shuffle-cases-dup
  [pop argmap]
  (if (= (:sort-meta-errors-for-lexicase argmap) :random)
    (lshuffle (range (count (:errors (first pop)))))
    (let [num-all-errors (count (:errors (first pop))) ;; will included meta-errors, added in select
          num-meta-errors (count (:meta-errors (first pop)))
          num-true-errors (- num-all-errors num-meta-errors)
          true-error-indices (range num-true-errors)
          meta-error-indices (map #(+ % num-true-errors)
                                  (range num-meta-errors))]
      (case (:sort-meta-errors-for-lexicase argmap)
        :first (concat (lshuffle meta-error-indices)
                       (lshuffle true-error-indices))
        :last (concat (lshuffle true-error-indices)
                      (lshuffle meta-error-indices))))))

(defn lwlexicase-selection
  "Returns an individual that never stands out as the worst on the fitness cases when considered one at a
  time in random order."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle-cases-dup pop argmap)]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))
	max-err-for-case (apply max (map #(nth % (first cases))
                                             (map :errors survivors)))]
	(if max-err-for-case == min-err-for-case)
		(rest cases)
        	(recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               		(rest cases))))))


