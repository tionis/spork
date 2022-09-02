###
### csv.janet
###
### A csv parser and encoder.
###

(defn get-csv-grammar [separator]
  (peg/compile
   '{:comma separator
     :space " "
     :space? (any :space)
     :comma? (at-most 1 :comma)
     :cr "\r"
     :lf "\n"
     :nl (+ (* :cr :lf)
            :cr :lf)
     :dquote "\""
     :dquote? (? "\"")
     :d_dquote (* :dquote :dquote)
     :textdata (+ (<- (some (if-not (+ :dquote :comma :nl) 1)))
                  (* :dquote
                     (<- (some (+ (if :d_dquote 2)
                                  (if-not :dquote 1))))
                     :dquote))
     :empty_field 0
     :field (accumulate (+ (* :space? :textdata :space?)
                           :empty_field))
     :row (* :field
             (any (* :comma :field))
             (+ :nl 0))
     :main (some (group :row))}))

# TODO add to grammar
# - escape chars
# - newlines in quoted stuff
# - remove quotes and spaces

(defn- csv-parse [x &opt separator]
  (default separator ",")
  (peg/match (get-csv-grammar separator) x))

(defn- process-header
  "Process the output of csv/decode into an array of struct using the first element as header."
  [ary]
  (let [header (map keyword (first ary))
        data   (array/slice ary 1)]
    (map (fn [row] (zipcoll header row))
         data)))

(defn decode
  "Decode a csv encoded string."
  [x &named separator header]
  (let [data (csv-parse x separator)]
    (if header
      (process-header data)
      data)))

(defn- field-to-csv
  [field]
  "escape strings for csv"
  (if (and (not= nil field)
           (or (string/find "\"" field)
               (string/find "\n" field)
               (string/find " " field)))
    (->> (string/replace-all "\"" "\"\"" field)
         (string/format "\"%s\""))
    (if (= nil field)
      ""
      field)))

(defn- is-list?
  [data]
  (or (array? data)
      (struct? data)))

(defn- row-to-csv
  [row]
  (let [data (if (not (is-list? row))
               (values row)
               row)]
    (map field-to-csv
         data)))

(defn- to-array-of-array
  [data]
  (let [ary @[]]
    (when (not (is-list? (first data)))
      (array/push ary (-> (first data)
                          keys)))
    (each row data
      (array/push ary (row-to-csv row)))
    ary))

(defn encode
  "Encode an array of arrays of strings into a csv."
  [x &opt separator]
  (default separator ",")
  (string/join (map (fn [row] (string/join row separator))
                 (to-array-of-array data))
             "\r\n"))
