(defn matrixMultiplyScalar [matrixA scalar]
  (if (seq matrixA)
    (conj
      (matrixMultiplyScalar (rest matrixA) scalar)
      (map (fn [arg] (* arg scalar)) (first matrixA)))))



(defn multiplyScalar [array scalar]
  (map * (repeat (count array) scalar) array))


(defn getArity [x y]
  (loop [arity 0
         obj x]
    ;(println "obj: " obj "\n")
    (if (and (not (seq? obj))  (not (vector? obj)))
      arity
      (recur (inc arity) (first obj)))))

(defn vectorMultiplyScalar [v scalar]
  (map * v (cycle [scalar])))
