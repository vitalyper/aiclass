Clojure solutions to Intro into AI class Oct-Dec 2011 
Natural Language Procesing (NLP) problems.

## Problem 1
Given a encrypted with Caesar cipher text, decrypt it using better than brute force approach.

The encrypted text is

```
Esp qtcde nzyqpcpynp zy esp ezatn zq Lcetqtntlw Tyepwwtrpynp hld spwo le Olcexzfes Nzwwprp ty estd jplc
```

### Solution
1. Given occurence frequencies for English letters descend sort them by frequency of occurences.
2. Count occurences in decrytped text, descend sort them by counts.
3. For first five top occurences in decrtypted text calculate shift, decrypt the text and
prompt user to accept decrypted text or to try again.

It happens that English letter frequency distribution holds for our encrypted text and we get an answer on the first try. But the solution is iterative and we should get an answer within less than brute force attempts.

We could of used most frequent words here to eliminate iterative prompting but 
having [E]valuate [P]rompt [R]read [L]oop illustrates how one can have naive REPL like
behavior.

## Problem 2
Restore "two characters shredded" text.

```
|de|  | f|Cl|nf|ed|au| i|ti|  |ma|ha|or|nn|ou| S|on|nd|on|
|ry|  |is|th|is| b|eo|as|  |  |f |wh| o|ic| t|, |  |he|h |
|ab|  |la|pr|od|ge|ob| m|an|  |s |is|el|ti|ng|il|d |ua|c |
|he|  |ea|of|ho| m| t|et|ha|  | t|od|ds|e |ki| c|t |ng|br|
|wo|m,|to|yo|hi|ve|u | t|ob|  |pr|d |s |us| s|ul|le|ol|e |
| t|ca| t|wi| M|d |th|"A|ma|l |he| p|at|ap|it|he|ti|le|er|
|ry|d |un|Th|" |io|eo|n,|is|  |bl|f |pu|Co|ic| o|he|at|mm|
|hi|  |  |in|  |  | t|  |  |  |  |ye|  |ar|  |s |  |  |. |
```

### Solution
A bit more challenging problem ;-) Will use the following properties from our problem domain.

Based on a cell content assign context deducible metadata. For example, " t" will have :rw (right word), "s " :lw (left 
word). Here is complete case analysis:

```clj
  (cond
    (and (is-space? l) (is-space? r)) {:e true} 
    ;left char is in sentence end set; rigth char is space
    (and (contains? SEN-END l) (is-space? r)) {:se true} 
    (is-space? l) {:lw true}
    (is-space? r) {:rw true}
    (not (and (is-space? l) (is-space? r))) {:w true}
    :else (throw (IllegalArgumentException. (format "Invalid input %s" v))))
```

English is left-to-right language, hence we can deduce that columns with higher "full" score (based on cell meta from 1 above) columns will be to the left.

```clj
(cond
    (:w m) 1.0
    (:rw m) 0.75
    (:lw m) 0.75
    (:se m) 0.25
    (:e m) -1.0
    :else 0.0))
```

Also, to filter possible words we will use words.txt with 2K+ most frequest English words. Actually scientific most frequesnt words would help more but the one we have turned out to be sufficient.

With a typical end user machine brute force all permutatoins would take some time ;-) so plan of attack is to divide and conquer.

1. Take advantage of the last row where only few cells are populated. Generate permutations, filter out permutations where cell meta contradicts (see poss?), calculate word score and use column's left score to break ties.
2. Using left score [5.0, 6.0] get middle columns. Once again generate permutations, filter out contradictions, calculate word score, take top 5, combine with step 1, calculate word score again, filter out contradictions and take one with the highest score.
3. Remaing columns happen to be in the left score correct descending order.

## To run
1. Clone the poject.
2. cd into aiclass
3. lein compile
4. lein repl

```clj
(require '[aiclass.nlp1 :as nlp1])
(require '[aiclass.nlp2 :as nlp2])
(nlp1/-main)
(nlp2/-main)
```
