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
    (if (and (not (seq? obj)) (not (vector? obj)))
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

(defn solveLockers [numberOfLockers]
  (loop [i 2
         lockers (take numberOfLockers (repeat 0))]
    (if (= numberOfLockers i)
      lockers
      (recur (inc i)
        (map
          (fn [x] (if (= 0 (mod i x))
            (if (= x 0) 1 0)
            x))
          lockers)))))

(defn toggleLockers [lockers n]
  (loop [i 0 y []]
    (if (= (count lockers) (count y))
      y
      (let [ind (+ 1 i)
            cur-val (nth lockers i)
            new-val (if (= 0 (mod ind n)) (if (= 0 cur-val) 1 0) cur-val)]
        (recur (inc i) (conj y new-val))))))

(defn solveLockers [numberOfLockers]
  (loop [i 2
         lockers (take numberOfLockers (repeat 0))]
    (if (= numberOfLockers i)
      lockers
      (recur (inc i) (toggleLockers lockers i)))))

(solveLockers 1000)


; Functions shared b/w Matrix and Vector
(defmulti getArityMulti (fn [a x] (class x)))

(defmethod getArityMulti clojure.lang.ISeq [a x]
  (getArityMulti (inc a) (first x)))

(defmethod getArityMulti clojure.lang.IPersistentVector [a x]
  (getArityMulti (inc a) (first x)))

(defmethod getArityMulti :default [a x] a)

(defn getArity [x dummy] (getArityMulti 0 x))

(defmulti multiplyScalar getArity)

(defmethod multiplyScalar 3 [x scalar]
  (loop [x x
         r []]
    (if-not (seq x) r (recur (rest x) (conj r (multiplyScalar (first x) scalar))))))

(defmethod multiplyScalar 2 [matrixA scalar]
  (if (seq matrixA)
    (conj
      (multiplyScalar (rest matrixA) scalar)
      (map (fn [arg] (* arg scalar)) (first matrixA)))))

(defmethod multiplyScalar 1 [array scalar]
  (map * (repeat (count array) scalar) array))

(defmethod multiplyScalar :default [x scalar]
  (* x scalar))

