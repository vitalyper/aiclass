(defproject aiclass "0.0.1"
  :description "Solutions to Natural Language Processing (NLP) Intro into AI Stanford Oct-Dec 2011 class"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/math.combinatorics "0.0.2"]]
  :aot ^{:skip-aot true} [aiclass.nlp1 aiclass.nlp2] 
)
            
