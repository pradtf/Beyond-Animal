;;; stocks examination
;; <p class="Mt(15px) Lh(1.6)" data-reactid="217">

;; input stocklist and keywords

(require '[tech.ml.dataset :as ds])

(def stocks (ds/->dataset "/home/exchanges/AMEX.txt"))


;; get the descriptions from the yahoo finance profiles of a stock
;; composing mkurl, fetch, pull-description into get-description

(defn mkurl
  "creates a url on finance.yahoo using stock symbol"
  [sym]
  (str "https://finance.yahoo.com/quote/" sym "/profile"))

(defn fetch-url
  "gets the html from the url provided"
  [url]
  (html/html-resource (java.net.URL. url)))

(defn pull-description
  "gets the 3rd item from finance.yahoo profile of a stock"
  [txt]
  (nth (map html/text
            (html/select txt [:p]))
       2))

(def get-descriptions ;from page given url
  (comp pull-description fetch-url mkurl))


;; pullout the sentence containing desired phrase from txt (of the description)
;; note we need join-regex to combine pure regex with text phrase
;; TODO find way to handle sentences which have abbreviations like Inc. in it

(defn join-regex
  "joins regex strings: patterns -> str -> re-pattern"
  [& patterns]
  (re-pattern (apply str
                     (map #(str %) patterns))))

(defn pull-sentence
  [phrase txt]
  (let [sentence (re-find (join-regex
                           #"[\s\w+,'\"-]+" ;accepts all \s, \w+, comma, quotes till senend
                           phrase
                           #"[\s\w+\W]+?[.!?]") ;same till senend of previous
                          txt)]
    (if sentence
      (str/triml sentence))))

(def stocks ["ALXN" "BYND" "SPCE"])

(def all-descriptions (map #(get-descriptions %) stocks))
(def all-words ["therapeutic" "convenience store" "was founded"])



(pull-sentence "founded" txt)

(map #(pull-sentence % %2) all-words all-descriptions)

(pull-sentence "variouss"
               (nth all-descriptions 0))

(for [word all-words
      desc all-descriptions]
  (pull-sentence word desc))

;; => ("develops and commercializes various therapeutic products." nil nil nil "The company sells its products through grocery, mass merchandiser, club and convenience store, natural retailer channels, direct to consumer, restaurants, foodservice outlets, and schools." nil "The company was founded in 1992 and is headquartered in Boston, Massachusetts." "was founded in 2009 and is headquartered in El Segundo, California." "was founded in 2017 and is headquartered in Las Cruces, New Mexico.")

;; TODO
;; Inc. problem
