(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc b e]
                 (if (<= e 0)
                   acc
                   (recur (* acc b) b (dec e))))]
    ; sanitize and optimize. Fractional exp is not handled
    (cond
      (not (integer? exp)) "E: Fractional exp not supported, yet"
      (zero? base) (if (zero? exp) "E: Division by zero" 0)
      (>= exp 0) (helper 1 base exp)
      (neg? exp) (/ (helper 1 base (- exp)))
      :else "E: Invalid arguments")))

(defn last-element [a-seq]
  (let [helper (fn [prev s]
                 (if (empty? s)
                   prev
                   (recur (first s) (rest s))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
                 (cond
                   (and (empty? s1) (empty? s2)) true
                   (or (empty? s1) (empty? s2)) false
                   (not (== (first s1) (first s2))) false
                   :else (recur (rest s1) (rest s2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [i 0
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) i
      :else (recur (inc i) (rest s)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         s a-seq]
    (if (empty? s)
      (/ sum n)
      (recur (+ sum (first s)) (inc n) (rest s)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
      (if (contains? a-set elem)
            (disj a-set elem)
            (conj a-set elem)))]
    (loop [acc #{}
           s a-seq]
      (if (empty? s)
        acc
        (recur (toggle acc (first s)) (rest s))))))

(defn fast-fibo [n]
  (if (zero? n)
    0
    (loop [i n
           a 0
           b 1]
      (if (zero? i)
      a
      (recur (dec i) b (+ a b))))))

(defn cut-at-repetition [a-seq]
  (loop [acc [] 
         s a-seq]
    (if (or (contains? (set acc) (first s)) (empty? s))
      acc
      (recur (conj acc (first s)) (rest s)))))

