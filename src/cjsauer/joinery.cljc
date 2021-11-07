(ns cjsauer.joinery)

(defprotocol Joinery
  (is-join? [this v])
  (join [this m v]))

(deftype TableIdentJoinery []
  Joinery
  (is-join? [_ v] (and (vector? v)
                    (= 2 (count v))
                    (keyword? (first v))))
  (join [_ table v] (get-in table v)))

(declare joined-map? table-join -jm-equiv)

(defn- map-entry
  [k v]
  #?(:clj (clojure.lang.MapEntry. k v)
     :cljs (cljs.core/MapEntry. k v nil)))

(deftype JoinedMap [joinery root m]
  #?@(:clj
      [clojure.lang.MapEquivalence

       clojure.lang.IPersistentMap
       (assoc [_ k v]
         (JoinedMap. joinery root (.assoc m k v)))
       (assocEx [_ k v]
         (JoinedMap. joinery root (.assocEx m k v)))
       (without [_ k]
         (JoinedMap. joinery root (.without m k)))

       java.lang.Iterable
       (iterator [_]
         (.iterator m))

       clojure.lang.Associative
       (containsKey [_ k]
         (.containsKey m k))
       (entryAt [_ k]
         (when-let [[k v] (find m k)]
           (map-entry k (table-join joinery root v))))

       clojure.lang.Counted
       (count [_] (.count m))

       clojure.lang.IPersistentCollection
       (cons [_ o]
         (JoinedMap. joinery root (.cons m o)))
       (empty [_]
         (JoinedMap. joinery root (.empty m)))
       (equiv [this o]
         (or (identical? this o)
           (if (joined-map? o)
             (.equiv (.m o) m)
             (.equiv o this))))

       clojure.lang.Seqable
       (seq [_]
         (map (fn [[k v]]
                (let [v' (table-join joinery root v)]
                  (map-entry k v')))
           (seq m)))

       clojure.lang.ILookup
       (valAt [this k]
         (.valAt this k nil))
       (valAt [_ k not-found]
         (let [v (get m k not-found)]
           (table-join joinery root v)))

       java.util.Map
       (get [this k]
         (.valAt this k))
       (isEmpty [this]
         (empty? this))
       (size [this]
         (count this))
       (keySet [_]
         (set (keys m)))
       (put [_ _ _]
         (throw (UnsupportedOperationException.)))
       (putAll [_ _]
         (throw (UnsupportedOperationException.)))
       (clear [_]
         (throw (UnsupportedOperationException.)))
       (remove [_ _]
         (throw (UnsupportedOperationException.)))
       (values [this]
         (->> this seq (map second)))
       (entrySet [this]
         (->> this seq set))

       clojure.lang.IObj
       (withMeta [_ mta]
         (JoinedMap. joinery root (.withMeta m mta)))
       (meta [_]
         (meta m))

       Object
       (hashCode [this]
         (hash (into {} this)))
       (equals [this o]
         (.equiv this o))
       (toString [this]
         (str (into {} this)))])

  #?@(:cljs
      [Object
       (toString [this]
         (str (into {} this)))
       (equiv [this o]
         (-equiv this o))
       (keys [this]
         (es6-iterator (keys this)))
       (entries [this]
         (es6-entries-iterator (seq this)))
       (values [this]
         (es6-iterator (vals this)))
       (has [this k]
         (contains? this k))
       (get [this k not-found]
         (-lookup this k not-found))
       (forEach [this f]
         (doseq [[k v] this]
           (f v k)))

       ICloneable
       (-clone [_] (JoinedMap. joinery root m))

       IWithMeta
       (-with-meta
        [_ new-meta]
        (JoinedMap. joinery root (-with-meta m new-meta)))

       IMeta
       (-meta [_] (meta m))

       ICollection
       (-conj
        [_ entry]
        (JoinedMap. joinery root (conj m entry)))

       IEmptyableCollection
       (-empty
        [_]
        (JoinedMap. joinery root (empty m)))

       IEquiv
       (-equiv
        [this ^js o]
        (or (identical? this o)
          (if (joined-map? o)
            (-equiv (.-m o) m)
            (-equiv o this))))

       IHash
       (-hash
        [this]
        (hash (into {} this)))

       IIterable
       (-iterator
        [this]
        (-iterator (into {} this)))

       ISeqable
       (-seq
        [_]
        (map (fn [[k v]]
               (let [v' (table-join joinery root v)]
                 (map-entry k v')))
          (seq m)))

       IAssociative
       (-assoc
        [_ k v]
        (JoinedMap. joinery root (-assoc m k v)))
       (-contains-key?
        [_ k]
        (contains? m k))

       IFind
       (-find
        [_ k]
        (when-let [[k v] (find m k)]
          (map-entry k (table-join joinery root v))))

       IMap
       (-dissoc
        [_ k]
        (JoinedMap. joinery root (dissoc m k)))

       ICounted
       (-count [_] (count m))

       ILookup
       (-lookup
        [this k]
        (-lookup this k nil))
       (-lookup
        [_ k not-found]
        (let [v (get m k not-found)]
          (table-join joinery root  v)))])

  ;;
  )

(defn- table-join
  [joinery root v]
  (cond
    (joined-map? v)
    v
    ;;
    (is-join? joinery v)
    (JoinedMap. joinery root (join joinery root v))
    ;;
    (map? v)
    (JoinedMap. joinery root v)
    ;;
    (coll? v)
    (into (empty v)
      (map (fn [x]
             (if (is-join? joinery x)
               (JoinedMap. joinery root (join joinery root x))
               x)))
      v)
    ;;
    :else v))

(defn joined-map?
  [o]
  (instance? JoinedMap o))

(defn joined-map
  ([m]
    (joined-map (TableIdentJoinery.) m))
  ([joinery m]
    (->JoinedMap joinery m m)))

(defn unwrap
  [^JoinedMap jm]
  #?(:clj (.m jm)
     :cljs (.-m jm)))

#?(:clj
   (defmethod print-method JoinedMap [jm writer]
     (.write writer (str (into {} jm)))))