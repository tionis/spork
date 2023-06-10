###
### data.janet 
###
### Compare data structures using `diff`.
###

(varfn diff [])

(defn- atom-diff
  [a b]
  (if (= a b) @[nil nil a] @[a b nil]))

(defn- in? [x ds]
  (if (index-of x ds)
    true
    false))

(defn- safe-in [ds n]
  (if (in? (type ds) [:array :tuple])
    (if (<= n (dec (length ds)))
      (in ds n)
      nil)
    (in ds n)))

(defn- vectorize [m]
  (unless (or (nil? m) (empty? m))
    (when (in? (type m) [:array :tuple :table :struct])
      (reduce
        (fn [result [k v]] (put result k v))
        (array/new-filled (max ;(keys m)))
        (pairs m)))))

(defn- diff-associative-key [a b k]
  (let [va (safe-in a k)
        vb (safe-in b k)
        [a* b* ab] (diff va vb)
        in-a (in? k (keys a))
        in-b (in? k (keys b))
        same (and in-a in-b
                  (or (not (nil? ab))
                      (and (nil? va) (nil? vb))))]
    [(when (and in-a (or (not (nil? a*)) (not same))) {k a*})
     (when (and in-b (or (not (nil? b*)) (not same))) {k b*})
     (when same {k ab})]))

(defn- diff-associative [a b ks]
  (reduce
    (fn [diff1 diff2]
      (map |(if (empty? $) nil $)
           (map |(merge (or $0 {}) (or $1 {})) diff1 diff2)))
    [nil nil nil]
    (map
      (partial diff-associative-key a b)
      ks)))

(defn- diff-sequential [a b]
  (map vectorize (diff-associative
                   (if (array? a) a (array ;a))
                   (if (array? b) b (array ;b))
                   (range (max (length a) (length b))))))

(defn- diff-similar [kind a b]
  (cond
    (in? kind [:array :tuple]) (diff-sequential a b)
    (in? kind [:table :struct]) (diff-associative a b (distinct (array/concat (keys a) (keys b))))
    (atom-diff a b)))

(defn- categorize [x]
  (cond
    (in? (type x) [:array :tuple]) :sequence
    (in? (type x) [:table :struct]) :associative
    :atom))

(varfn diff
  ``` 
  Compares a and b recursively. Returns an array of 
  [things-only-in-a things-only-in-b things-in-both].   
  ```
  [a b]
  (if (= a b)
    @[nil nil (cond (tuple? a) (array ;a)
                (struct? a) (struct/to-table a) a)]
    (if (= (categorize a) (categorize b))
      (diff-similar (type a) a b)
      (atom-diff a b))))

(defn- diff-to-changes [ds &opt ops path]
  (default ops @[])
  (default path [])
  (def category (categorize ds))
  (case category
    :sequence    (eachk key ds (diff-to-changes (ds key) ops [;path [category key]]))
    :associative (eachk key ds (diff-to-changes (ds key) ops [;path [category key]]))
    :atom (if ds (array/push ops [;path [:atom ds]])))
  ops)

(defn changeset/from-diff
  "create changeset to transform a to b from the output of data/diff"
  [diff-arr]
  (def rm-ops (diff-to-changes (diff-arr 0)))
  (def put-ops (diff-to-changes (diff-arr 1)))
  # TODO deduplicate rm-ops
  # TODO also merge datastructures that are deleted in full into one change
  # TODO merge rm-ops with (:atom nil) into put-ops and return them
  {:rm (freeze rm-ops)
   :put (freeze put-ops)})

(defn changeset/apply [ds changeset]
  (def x (thaw ds))
  (each op (ds :rm))
  (each op (ds :rm))
  x)

(def a @[@[1 2 "arr-val"] 2 4 {1 2 4 [1 2 3]}])
(def b @[@[1 2] 2 3 @{1 2 2 3}])
(def diff-arr (diff a b))
(def changeset (changeset/from-diff diff-arr))
(def should {:put [[[:sequence 2] [:atom 3]]
                   [[:sequence 3] [:associative 2] [:atom 3]]]
             :rm [[[:sequence 0] [:sequence 2] [:atom "arr-val"]]
                  [[:sequence 2] [:atom 4]]
                  [[:sequence 3] [:associative 4] [:sequence 0] [:atom 1]]
                  [[:sequence 3] [:associative 4] [:sequence 1] [:atom 2]]
                  [[:sequence 3] [:associative 4] [:sequence 2] [:atom 3]]]})
(assert (deep= should changeset))
(assert (deep= (freeze b) (changeset/apply a changeset)))
