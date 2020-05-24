;; num-map.clj
;; Calculations over maps with numeric values.
;; AndrewJ 2019-09-21

(ns ttr.num-map)

(defn map-min 
  "Return the minimum value in a numeric map."
  [h]
  (apply min (vals h)))

(defn map-sum
  "Sum the values of the numeric map."
  [h]
  (apply + (vals h)))

(def map-union merge-with)

(defn map-add
  "Add matching values from two numeric mapes."
  [h1 h2]
  (map-union + h1 h2))

(defn map-sub
  "Subtract matching values from two numeric mapes."
  [h1 h2]
  (map-union - h1 h2))

(defn map-enumerate
  "For each pair [k v] in a numeric map, add v copies of k, and concatenate into a single list."
  [h]
  (reduce-kv
   (fn [m k v]
     (into m (repeat v k))) [] h))

(defn map-collect
  "Collect a list into a numeric map of counts."
  [lst]
  (into {} (map (fn [[k v]] [k (count v)])
                (group-by identity lst))))

; From Racket...
;(defn map-intersection [f h1 h2]
;         ;#:combine/key [combine/key (λ (k x y) (* x y))]
;         ;h1 h2)
;  (for/map ([k (in-list (intersection (map-keys h1) (map-keys h2)))])
;            (values k (combine/key k (map-ref h1 k) (map-ref h2 k)))))

;(define (map-union
;         #:combine [combine #f]
;         #:combine/key [combine/key
;                        (if combine
;                          (lambda (k x y) (combine x y))
;                          (map-duplicate-error 'map-union))]
;         one . rest)
;  (for*/fold ([one one]) ([two (in-list rest)] [(k v) (in-map two)])
;    (map-set one k (if (map-has-key? one k)
;                        (combine/key k (map-ref one k) v)
;                        v))))

;; The End