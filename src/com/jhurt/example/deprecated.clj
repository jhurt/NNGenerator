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



(import
  '(java.io StringReader PushbackReader))

(deftype Y [a b])

(defn serialize [x]
  (binding [*print-dup* false] (pr-str x)))

(defn deserialize [x]
  (let [r (new PushbackReader (new StringReader x))]
    (read r)))

(def y (Y "a" "b"))

(deserialize (serialize y)) 
