(ns cjsauer.joinery)

(defprotocol Joinery
  "Description of normalized data source traversal"
  (is-join? [this v] "Returns true if the given value v represents a logical join")
  (join [this db v] "Returns the result of following the link that is v within the normalized data source db"))

(deftype TableIdentJoinery []
  Joinery
  (is-join? [_ v] (and (vector? v)
                    (= 2 (count v))
                    (keyword? (first v))))
  (join [_ table v] (get-in table v)))

(declare joined-map? table-join -jm-equiv unwrap)

(defn- map-entry
  [k v]
  #?(:clj (clojure.lang.MapEntry. k v)
     :cljs (cljs.core/MapEntry. k v nil)))

#?(:clj (defn throw-arity [] (throw (clojure.lang.ArityException 1 (str `JoinedMap)))))

(deftype JoinedMap [joinery db m]
  #?@(:clj
      [clojure.lang.MapEquivalence

       clojure.lang.IPersistentMap
       (assoc [_ k v]
         (JoinedMap. joinery db (.assoc m k v)))
       (assocEx [_ k v]
         (JoinedMap. joinery db (.assocEx m k v)))
       (without [_ k]
         (JoinedMap. joinery db (.without m k)))

       clojure.lang.IMapIterable
       (keyIterator [_]
         (.iterator (keys m)))
       (valIterator [_]
         (.iterator (map #(table-join joinery db %) (vals m))))

       java.lang.Iterable
       (iterator [this]
         (.iterator (seq this)))

       clojure.lang.Associative
       (containsKey [_ k]
         (.containsKey m k))
       (entryAt [_ k]
         (when-let [[k v] (find m k)]
           (map-entry k (table-join joinery db v))))

       clojure.lang.Counted
       (count [_] (.count m))

       clojure.lang.IPersistentCollection
       (cons [_ o]
         (JoinedMap. joinery db (.cons m o)))
       (empty [_]
         (JoinedMap. joinery db (.empty m)))
       (equiv [this o]
         (or (identical? this o)
           (if (joined-map? o)
             (.equiv (.m o) m)
             (.equiv o this))))

       clojure.lang.Seqable
       (seq [_]
         (map (fn [[k v]]
                (let [v' (table-join joinery db v)]
                  (map-entry k v')))
           (seq m)))

       clojure.lang.ILookup
       (valAt [this k]
         (.valAt this k nil))
       (valAt [_ k not-found]
         (let [v (get m k not-found)]
           (table-join joinery db v)))

       clojure.lang.IReduce
       (reduce [this f]
         (reduce f (seq this)))

       clojure.lang.IReduceInit
       (reduce [this f init]
         (reduce-kv (fn [r k v]
                      (f r (map-entry k v)))
           init this))

       clojure.core.protocols.IKVReduce
       (kv-reduce [this f init]
         (reduce (fn [r [k v]]
                   (f r k v))
           init (seq this)))

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
         (JoinedMap. joinery db (.withMeta m mta)))
       (meta [_]
         (meta m))

       clojure.lang.IHashEq
       (hasheq [_]
         (.hasheq m))

       clojure.lang.IFn
       (invoke [_this] (throw-arity))
       (invoke [this k] (get this k))
       (invoke [_this _k _] (throw-arity))
       (invoke [_this _k _ _] (throw-arity))
       (invoke [_this _k _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _ _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _] (throw-arity))
       (invoke [_this _k _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _] (throw-arity))

       Object
       (hashCode [_]
         (hash m))
       (equals [this o]
         (.equiv this o))
       (toString [_]
         (str m))])

  #?@(:cljs
      [Object
       (toString [this]
         (pr-str* this))
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
       (-clone [_] (JoinedMap. joinery db m))

       IWithMeta
       (-with-meta
        [_ new-meta]
        (JoinedMap. joinery db (-with-meta m new-meta)))

       IMeta
       (-meta [_] (meta m))

       ICollection
       (-conj
        [_ entry]
        (JoinedMap. joinery db (conj m entry)))

       IEmptyableCollection
       (-empty
        [_]
        (JoinedMap. joinery db (empty m)))

       IEquiv
       (-equiv
        [this ^js o]
        (or (identical? this o)
          (if (joined-map? o)
            (-equiv (.-m o) m)
            (-equiv o this))))

       IHash
       (-hash
        [_]
        (hash m))

       IIterable
       (-iterator
        [this]
        (-iterator (into {} this)))

       ISeqable
       (-seq
        [_]
        (map (fn [[k v]]
               (let [v' (table-join joinery db v)]
                 (map-entry k v')))
          (seq m)))

       IAssociative
       (-assoc
        [_ k v]
        (JoinedMap. joinery db (-assoc m k v)))
       (-contains-key?
        [_ k]
        (contains? m k))

       IFind
       (-find
        [_ k]
        (when-let [[k v] (find m k)]
          (map-entry k (table-join joinery db v))))

       IMap
       (-dissoc
        [_ k]
        (JoinedMap. joinery db (dissoc m k)))

       ICounted
       (-count [_] (count m))

       ILookup
       (-lookup
        [this k]
        (-lookup this k nil))
       (-lookup
        [_ k not-found]
        (let [v (get m k not-found)]
          (table-join joinery db  v)))

       IReduce
       (-reduce
        [this f]
        (reduce f (seq this)))
       (-reduce
        [this f init]
        (reduce-kv (fn [r k v]
                     (f r (map-entry k v)))
          init this))

       IKVReduce
       (-kv-reduce
        [this f init]
        (reduce (fn [r [k v]]
                  (f r k v))
          init (seq this)))

       IFn
       (-invoke
        [this k]
        (get this k))
       (-invoke
        [this k not-found]
        (get this k not-found))

       IPrintWithWriter
       (-pr-writer
        [_ writer opts]
        (print-map m pr-writer writer opts))])

  ;;
  )

(defn- table-join
  [joinery db v]
  (cond
    (joined-map? v)
    v
    ;;
    (is-join? joinery v)
    (JoinedMap. joinery db (join joinery db v))
    ;;
    (map? v)
    (JoinedMap. joinery db v)
    ;;
    (coll? v)
    (into (empty v)
      (map (fn [x]
             (if (is-join? joinery x)
               (JoinedMap. joinery db (join joinery db x))
               x)))
      v)
    ;;
    :else v))

(defn joined-map?
  "Returns true if the given object is an instance of JoinedMap"
  [o]
  (instance? JoinedMap o))

(defn joined-map
  "Create a new joined-map using the given `db` as the normalize data source.
  Optionally takes a starting entity as the second arg that will become the
  current position of the joined-map.
  Optionally takes a third argument that is an implementation of the 
  cjsauer.joinery/Joinery protocol to customize the behavior of joins."
  ([db]
    (->JoinedMap (TableIdentJoinery.) db db))
  ([db e]
    (->JoinedMap (TableIdentJoinery.) db e))
  ([db e joinery]
    (->JoinedMap joinery db e)))

(defn unwrap
  "Given a JoinedMap instance, returns the plain underlying hash-map that this
  joined map wraps."
  [^JoinedMap jm]
  #?(:clj (.m jm)
     :cljs (.-m jm)))

#?(:clj
   (defmethod print-method JoinedMap [jm writer]
     (.write writer (str jm))))