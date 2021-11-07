(ns cjsauer.joinery
  (:require [clojure.pprint :refer [simple-dispatch]]))

(defprotocol Joinery
  (is-join? [this v])
  (join [this m v]))

(deftype TableIdentJoinery []
  Joinery
  (is-join? [_ v] (and (vector? v)
                       (= 2 (count v))
                       (keyword? (first v))))
  (join [_ table v] (get-in table v)))

(declare joined-map? table-join)

(deftype JoinedMap [joinery root m]
  clojure.lang.MapEquivalence

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
    (let [[k v] (.entryAt m k)
          v' (table-join joinery root v)]
      (clojure.lang.MapEntry. k v')))

  clojure.lang.Counted
  (count [_] (.count m))

  clojure.lang.IPersistentCollection
  (cons [_ o]
    (JoinedMap. joinery root (.cons m o)))
  (empty [_]
    (JoinedMap. joinery root (.empty m)))
  (equiv [this o]
    (if (isa? (class o) JoinedMap)
      (.equiv (.m o) m)
      (.equiv o this)))

  clojure.lang.Seqable
  (seq [_]
    (map (fn [[k v]]
           (let [v' (table-join joinery root v)]
             (clojure.lang.MapEntry. k v')))
         (.seq m)))

  clojure.lang.ILookup
  (valAt [_ k]
    (let [v (.valAt m k)]
      (table-join joinery root v)))
  (valAt [this k not-found]
    (if-let [v (.valAt this k)]
      v
      not-found))

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
    (clojure.lang.Util/hash (into {} this)))

  (equals [this o]
    (or (identical? this o)
        (.equiv this o)))

  (toString [this]
    (str (into {} this)))

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
  (isa? (class o) JoinedMap))

(defn joined-map
  ([m]
   (joined-map (TableIdentJoinery.) m))
  ([joinery m]
   (->JoinedMap joinery m m)))

(defn unwrap
  [^JoinedMap jm]
  (.m jm))

(do
  ;; setup-printing

  (defn print-joined-map [jm writer]
    (.write writer (str (.m jm))))

  (defmethod print-method JoinedMap [jm writer]
    (print-joined-map jm writer))

  (defmethod print-dup JoinedMap [jm writer]
    (print-joined-map jm writer))

  (.addMethod simple-dispatch JoinedMap (fn [jm] (print-joined-map jm *out*))))