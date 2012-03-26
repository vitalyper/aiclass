; Solution for Natural Language Processing (NLP)
; problem 1 for Intro Into AI class Oct-Dec 2011
; http://www.ai-class.com
;
; Author: Vitaly Peressada

(ns aiclass.nlp1)

(def 
  #^{:doc "Copied from web plus some vi macro magic"} 
  EN-FREQ-DIST 
  [[\a	8.167] [\b	1.492] [\c	2.782] [\d	4.253] [\e	12.702]	
  [\f	2.228] [\g	2.015] [\h	6.094] [\i	6.966] [\j	0.153]	
  [\k	0.772] [\l	4.025] [\m	2.406] [\n	6.749] [\o	7.507]	
  [\p	1.929] [\q	0.095] [\r	5.987] [\s	6.327] [\t	9.056]	
  [\u	2.758] [\v	0.978] [\w	2.360] [\x	0.150] [\y	1.974]	
  [\z	0.074]])

(def 
  #^{:doc "Encrypted text lowercased"}
  ENCRYPTED
  (. "Esp qtcde nzyqpcpynp zy esp ezatn zq Lcetqtntlw Tyepwwtrpynp hld spwo le Olcexzfes Nzwwprp ty estd jplc" toLowerCase))

(defn get-top-n
  "Gets top n most frequent letters from a sequence with items [char percent|count]. 
  Defaults to five."
  ([freq] (get-top-n freq 5))
  ([freq n]
   (take n
         (sort-by 
           last
           (fn [f s] (compare s f))
            freq))))

(defn get-cnt-map
  "Returns map of char occurences. Ignores spaces."
  [input]
  (reduce 
    (fn [m c]
       (if (contains? m c)
         (update-in m [c] inc)
         (assoc m c 1)))
    {}
    (filter 
      #(not (Character/isWhitespace %))
      input)))

(defn get-cipher-shift
  "Calculates shift btw cipher and english chars."
   [#^Character ciph #^Character plain]
   (let [shft (- (int ciph) (int plain))]
     (if (pos? shft)
       shft
       (+ shft 26))))

(defn transpose-caeser
  "Calculates single plain text char from encrypted using 
  supplied shift"
  [#^Character c shift]
  (let [no (- (int c) shift)]
    (cond 
      (Character/isWhitespace c) c
      (< no 97) (char (+ no 26))
      :else (char no))))

(defn decrypt-caeser
  "Decrypts input using shift"
  [#^String input shift]
  (apply str
         (map
           #(transpose-caeser % shift)
           input)))

(defn decrypt-using-top
  "Decrypts text using cipher and english chars.
  Prompts user and returns true if user wants to continue"
  [#^Character chr-cipher #^Character chr-plain #^String cipher]
  (let [#^Number shift (get-cipher-shift chr-cipher chr-plain)
        dec-txt (decrypt-caeser cipher shift)]
    (printf "Encrypted char '%c', english char '%c', shift %d:%n\"%s\"%n" 
            chr-cipher chr-plain shift dec-txt)
    (println "Doesn't look right, give it another try? y")
    (= (. (read-line) toLowerCase) "y")))

(defn -main
  "Main function. Calculats top 5 most frequent 
  encrypted and english chars, interleaves them
  and loops until user confirms the decrypted text."
  []
  (let [top-encr 
          (map
            first
            (get-top-n (get-cnt-map ENCRYPTED)))
        top-eng
          (map
            first
            (get-top-n EN-FREQ-DIST))
        top-chars
           (partition 2 
             (mapcat
               #(interleave top-encr (repeatedly (count top-eng) (fn [] %)))
               top-eng))]
    (loop [sq top-chars continue true]
      (if continue
        (recur (rest sq) 
               (decrypt-using-top 
                 (first (first sq)) 
                 (last (first sq)) 
                 ENCRYPTED))))))

