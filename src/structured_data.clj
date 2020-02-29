(ns structured-data)

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})

(def books [cities, wild-seed, embassytown, little-schemer])

(defn do-a-thing 
  [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff 
  [v]
  (let [first-element (get v 0)
        third-element (get v 2)]
    (+ first-element third-element)))

(defn cutify 
  [v]
  (conj v "<3"))

(defn spiff-destructuring 
  [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width 
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))
    

(defn height 
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? 
  [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area 
  [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? 
  [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? 
  [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))


(defn title-length 
  [book]
  (count (:title book)))

(defn author-count 
  [book]
  (count (:author book)))

(defn multiple-authors? 
  [book]
  (> 1 (author-count book)))

(defn add-author 
  [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors)))

(defn alive? 
  [author]
  (not (contains? author :death-year)))

(defn element-lengths 
  [collection]
  (map count collection))

(defn second-elements 
  [collection]
  (let [get-second-element (fn [x] (get x 1))]
    (map get-second-element collection)))

(defn titles 
  [books]
  (map :title books))


(defn monotonic? 
  [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars 
  [n]
  (apply str (repeat n "*")))


(defn toggle 
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? 
  [a-seq]
  (let [a-set (set a-seq)]
    (< (count a-set) (count a-seq))))

(defn old-book->new-book 
  [book]
  (assoc {} :authors (set :authors book)))

(defn has-author? 
  [book author]
  (contains? (:authors book) author))


(defn authors 
  [books]
  (let [authors (map :authors books)]
       (apply clojure.set/union authors)))  

(defn all-author-names 
  [books]
  (set (:name (authors books))))

(defn author->string 
  [author]
  (let [name (:name author)
        birth-year (when (:birth-year author)
                     (str " (" (:birth-year author) " - " (:death-year author) ")"))]
    (str name birth-year)))


(defn authors->string 
  [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string 
  [book]
  (str (:title book) " written by " (authors->string (:authors book))))

(book->string little-schemer)


(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
