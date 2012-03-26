; Solution for Natural Language Processing (NLP)
; problem 2 for Intro Into AI class Oct-Dec 2011
; http://www.ai-class.com
;
; Answer videos to the programming problems are now up on YouTube:
; http://www.youtube.com/watch?v=LhPsH2S3bQo
; http://www.youtube.com/watch?v=3sbjaDJkdZ4
;
; Author: Vitaly Peressada
;

(ns aiclass.nlp2
  (:use 
    [clojure.math.combinatorics]
    [clojure.string :only [lower-case trim join]]
    [clojure.pprint :only [pprint]]
  )
)

(def 
  ^{:doc "Shredded text"}
  ST
"|de|  | f|Cl|nf|ed|au| i|ti|  |ma|ha|or|nn|ou| S|on|nd|on|
|ry|  |is|th|is| b|eo|as|  |  |f |wh| o|ic| t|, |  |he|h |
|ab|  |la|pr|od|ge|ob| m|an|  |s |is|el|ti|ng|il|d |ua|c |
|he|  |ea|of|ho| m| t|et|ha|  | t|od|ds|e |ki| c|t |ng|br|
|wo|m,|to|yo|hi|ve|u | t|ob|  |pr|d |s |us| s|ul|le|ol|e |
| t|ca| t|wi| M|d |th|\"A|ma|l |he| p|at|ap|it|he|ti|le|er|
|ry|d |un|Th|\" |io|eo|n,|is|  |bl|f |pu|Co|ic| o|he|at|mm|
|hi|  |  |in|  |  | t|  |  |  |  |ye|  |ar|  |s |  |  |. |")

(def
  ^{:doc "Number of rows"}
  ROW-COUNT
  (int 8))

(def
  ^{:doc "Number of separators"}
  SEP-COUNT
  (int 20))

(def ^{:doc "Sentence end chars."}
  SEN-END 
  #{\! \. \?})

(defn read-words
  "Reads 2K+ most frequent english words
  from words.txt which could be in the current dir
  or in location specified by system property."
  []
  (-> (slurp (System/getProperty "AICLASS_WORDS" "words.txt"))
             (.split "\\n")
             seq
             set))
(def 
  ^{:doc "2K+ most frequent english words"}
  WORDS (read-words))

(defn cell-hint
  "Returns appropriate meta for a pair of chars (cell)"
  [#^String v] 
  (let [l (first v)
        r (last v)
        is-space? (fn [c] (= (int c) 32))]
    (when (and (not (nil? l)) (not (nil? r)))
      (cond
        (and (is-space? l) (is-space? r)) {:e true} 
        ;left char is in sentence end set; rigth char is space
        (and (contains? SEN-END l) (is-space? r)) {:se true} 
        (is-space? l) {:lw true}
        (is-space? r) {:rw true}
        (not (and (is-space? l) (is-space? r))) {:w true}
        :else (throw (IllegalArgumentException. (format "Invalid input %s" v))) 
        ))))

(defn pair-in-dict? 
  "Used for 1-2 letter words."
  [l r]
  (let [s (trim (apply str (flatten [l r])))]
    (contains? WORDS s)))

(defn poss?
  "Checks if two cells are compatible"
  [[l r]]
  (let [lm (meta l) 
        rm (meta r)]
    (cond
      (and (:rw lm) (:e rm)) false
      (and (:rw lm) (:lw rm)) false
      (and (:lw lm) (:se rm)) false
      ; two letter words should be in dictionary
      (and (:lw lm) (:rw rm)) (pair-in-dict? l r)
      ; one letter words in the middle of the sentence
      (and (:rw lm) (:rw rm)) (pair-in-dict? l r)
      :else true)
    ))

(defn poss-pairs?
  "Checks if sequence of pairs are compatible"
  [pairs]
  (not 
    (nil?
      (loop [s pairs]
        (if (nil? (first s))
          true
          (if (poss? (first s))
            (recur (rest s))))))))

(defn ltrim-to-next-word
  "Using cell meta drops cells from the left 
  until space is found (:lw or :rw)"
  [s]
    (if-let [i (first s)]
      (cond 
        ; no break is found
        (nil? i) s
        ; include current item
        (:lw (meta i)) s
        ; don't include current item
        (:rw (meta i)) (rest s)
        :else (recur (rest s)))))

(defn rtrim-to-prev-word
  "Using cell meta drops cells from the right 
  until space is found (:lw or :rw)"
  [s]
  (when-not (empty? s)
    (loop [i (last s) indx (count s)]
      (cond 
        ; no break is found
        (nil? i) s
        ; don't include current item
        (:lw (meta i)) (take (dec indx) s)
        ; include current item
        (:rw (meta i)) (take indx s)
        :else (recur (last (take (dec indx) s)) (dec indx))))))

(defn trim-to-words
  "Selective applies left or right trim to prev word"
  [s & {:keys [l r] :or {l true r true}}] 
  (let [ts (if l (ltrim-to-next-word s) s)]
    (if r (rtrim-to-prev-word ts) ts)))

(defn word-in-dict?
  "Checks a dictionary for a word.
  Drops two common endings 's' and 'ed'."
  [^String w]
  (let [ends-fn? (fn [w e]
                 (if (> (count w) 3)
                   (cond
                     (.endsWith w e) 
                       (contains? WORDS (apply str (take (- (count w) (count e)) w)))
                     :else false)
                   false))
        ends-with-s? #(ends-fn? % "s")
        ends-with-ed? #(ends-fn? % "ed")]
    (or (contains? WORDS w) (ends-with-s? w) (ends-with-ed? w))))

(defn get-words-score
  "Calculates words score based on custom scale derived
  from word's length"
  [s & {:keys [l r] :or {l true r true}}]
  (let [flt-punct-fn (fn [s]
                       (apply str
                              (filter
                                #(not (contains? SEN-END %))
                                s)))
        wrds (-> (apply str (flatten (trim-to-words s :l l :r r))) 
               flt-punct-fn
               (.split ,,, " ")
               seq)
        scr-lngth-fn (fn [w]
                        (cond
                          (= (count w) 1) 0.1
                          (= (count w) 2) 0.2
                          (= (count w) 3) 0.4
                          ; asign higher weight to words with 4+ letters
                          :else (* 2 (count w))))]
    (reduce
      (fn [v i]
        (if (word-in-dict? (lower-case i))
          (+ v (scr-lngth-fn i))
          v))
      0.0
      wrds)))

(defn st-to-model
  "Converts shredded text into model with clues."
  [#^String st row-count sep-count]
  (let [cell-to-scr-fn (fn [m]
                         (cond
                           (:w m) 1.0
                           (:rw m) 0.75
                           (:lw m) 0.75
                           (:se m) 0.25
                           (:e m) -1.0
                           :else 0.0))
        lft-scr-fn (fn [s]
                     (reduce +
                             (map 
                               #(cell-to-scr-fn (meta %)) 
                               s)))
        tm (partition row-count
            (apply interleave
              (map
                 #(rest %)
                 (partition sep-count
                    (map
                      #(with-meta (vector %) (cell-hint %))
                      (seq (. (. st replace "\n" "") split "\\|")))))))
        upd-fl-fn (fn [v indx nbr]
                    ; :ii initial index vs. :fi after we done
                    (update-in v [indx] #(with-meta % (assoc (meta %) :ii nbr))))
        mk-clmn-fn (fn [s nbr]
                (-> s
                  vec
                  (upd-fl-fn ,,, 0 nbr)
                  (upd-fl-fn ,,, (dec (count s)) nbr)
                  (with-meta ,,, {:ii nbr :ls (lft-scr-fn s)})))]
        (vec (map
               #(mk-clmn-fn %1 %2)
               tm
               (range)))))

(defn update-fi
  "Updates final index meta"
  ([v idx] (update-fi v idx
                        (inc 
                          (apply max 
                             (map #(-> % meta (get ,,, :fi -1)) v)))))
  ([v idx fi]
    (update-in v [idx] #(with-meta % (assoc (meta %) :fi fi)))))

(defn get-n-sorted
  "Generic way to get n sorted items by item function i-fn
  and sort function s-fn"
  ([s i-fn s-fn] (get-n-sorted s 1 i-fn s-fn))
  ([s n i-fn s-fn]
   (take n (sort-by i-fn s-fn s))))

(defn get-n-left
  "Gets n columns in asc order of left score"
  ([s] (get-n-left s (count s)))
  ([s n] (get-n-sorted s n #(-> % meta :ls) #(> %1 %2))))

(defn get-n-unfi
  "Gets n columns without :fi meta in asc order of left score"
  ([s] (get-n-unfi s 1))
  ([s n] (get-n-sorted 
           (filter
             #(nil? (-> % meta :fi))
             s)
           n 
           #(-> % meta :ls) 
           #(> %1 %2))))

(defn get-by-ls-range
  "Gets columns by left score inclusive range"
  [s l u]
  (filter
    #(and 
       (>= (-> % meta :ls) l)
       (<= (-> % meta :ls) u))
    s))

(defn process-last-row
  "Gets the last row and puts it in the right order"
  [m]
  (let [clmns-with-data
        (filter
          #(and
             (nil? (:e (-> % last meta)))
             (nil? (:se (-> % last meta))))
          m)
        guess-clmns (get-n-left clmns-with-data)
        lst-row 
          (map
            #(last %)
            guess-clmns)
        all-perms (permutations lst-row)
        fltrd-perms (filter
                      #(poss-pairs? (partition 2 1 %))
                      all-perms)
        scores (reduce
                 (fn [v i]
                   (conj v 
                         (vector
                          (get-words-score i :l false :r false)
                          i)))
                 []
                 fltrd-perms)
        max-score (apply max (map first scores))
        maxes (reduce
                (fn [v i]
                  (if (= max-score (first i))
                    (cons (last i) v)
                    v))
                []
                scores)
        ; to break ties we use column's left score as a hint
        maxes-sent-scores (apply hash-map
                                 (reduce
                                   (fn [v i]
                                     (concat v
                                          (vector
                                           (-> i first meta :ii m meta :ls)
                                           i)))
                                   []
                                   maxes))
        ordered-row 
          (-> (apply max (map first maxes-sent-scores)) maxes-sent-scores vec)
        ; update column's meta data :fi (final index) 
        upd-m
          (loop [um m idx 0]
            (if (= (count ordered-row) idx)
              um
              (recur 
                (update-fi um (-> (ordered-row idx) meta :ii) idx)
                (inc idx))))
        ; find a first dot in the last column and update it's column fi to max + 1
        dot-clmn-lst-row
          (first (filter #(not (nil? (-> % last meta :se))) upd-m))]
    (update-fi upd-m (-> dot-clmn-lst-row meta :ii))
    ))

(defn get-fi-sorted
  "Sorts on :fi meta in asc order"
  [v]
  (vec
    (sort-by
      #(-> % meta :fi)
      #(> %2 %1)
      (filter
        #(not (nil? (-> % meta :fi)))
        v))))

(defn cols-to-rows
  "Flips columns into rows."
  [v]
  (let [col-cnt (count (first v))
        l2-f 
        (fn [v idx]
          (vec (map #(nth % idx) v)))]
    (vec (map
      #(l2-f v %)
      (range col-cnt)))))

(defn calc-cum-score
  "Calculates cumulative score. v must be in row-wise form."
  [v]
  (reduce
    +
    (map #(get-words-score %) v)))


(defn update-fi-multi
  "Updates :fi meta data for a sequence of columns"
  [in-model in-cols]
  (loop [um in-model cols in-cols]
    (if (empty? cols)
      um
      (recur 
        (update-fi um (-> (first cols) meta :ii))
        (rest cols)))))

(defn -main
  "Main function"
  []
  (let [
        ; step 1 - find correct order for the last row.
        im (st-to-model ST ROW-COUNT SEP-COUNT)
        um (process-last-row im)
        s1 (get-fi-sorted um)

        ; step 2 - find a middle part based on column's left score
        ; somewhat arbitrary range with left score from 5.0 to 6.0
        s2-cnt (- (count im) (count s1))
        no-fi (get-n-unfi um s2-cnt)
        s2 (get-by-ls-range no-fi 5.0 6.0)
        s2-p (permutations s2) 
        s2-pf (filter
                  (fn [i] 
                    (every?
                      #(poss-pairs? (partition 2 1 %))
                    (cols-to-rows i)))
                  s2-p)
        scores (reduce
                 (fn [v i]
                   (conj v 
                         (vector
                            (calc-cum-score (cols-to-rows i))
                            i)))
                 []
                 s2-pf)
        s2-top5 (map
                  last
                  (get-n-sorted scores 5 first #(and (> (+ %1 %2) 0.0) (> %1 %2))))
        ; compbine each candidate with step 1 solution,
        ; filter them and take one with highest score
        s2-sol (first 
                 (filter
                  (fn [i] 
                    (every?
                      #(poss-pairs? (partition 2 1 %))
                    (cols-to-rows (concat s1 i))))
                  s2-top5))
        um2 (update-fi-multi um s2-sol)

        ; step 3 - get unprocessed columns 
        ; and use left score index which "happens" to be 
        ; in right order
        s3-cnt (- (count im) (+ (count s1) (count (first s2-top5))))
        no-fi2 (get-n-unfi um2 s3-cnt)
        um3 (update-fi-multi um2 no-fi2)
        answer (apply str (flatten (cols-to-rows (get-fi-sorted um3))))]

    ; remove consequitive spaces
    (join " "
      (filter
         #(not (empty? %))
         (seq (.split answer " "))))))
