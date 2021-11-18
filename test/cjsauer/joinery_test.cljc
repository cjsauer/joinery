(ns cjsauer.joinery-test
  (:require [clojure.test :refer [deftest testing is]]
            [cjsauer.joinery :refer [joined-map joined-map? unwrap]]))

(def db
  {:person/id {1 {:person/name "Calvin"
                  :person/friends [[:person/id 2]]
                  :person/pet [:pet/id 1]}
               2 {:person/name "Derek"
                  :person/friends [[:person/id 1]]}}
   :pet/id    {1 {:pet/name "Malcolm"
                  :pet/species :dog
                  :pet/owner [:person/id 1]}}})

(def jm (joined-map db))

(deftest joined-map-test
  (testing "containment"
    (is (true? (contains? jm :person/id)))
    (is (false? (contains? jm :foobar))))

  (testing "count"
    (is (= 2 (count jm))))

  (testing "equality"
    (is (= jm jm))
    (is (= (empty jm) {}))
    (is (= {} (empty jm)))
    (is (= {:a 1} (joined-map {:a 1})))
    (is (= (joined-map {:a 1}) {:a 1}))
    (is (not= db jm))
    (is (not= jm db)))

  (testing "lookup"
    (let [c  (get-in jm [:person/id 1])
          d  (get-in jm [:person/id 2])
          m  (get-in jm [:person/id 1 :person/pet])
          fs (get-in jm [:person/id 1 :person/friends])]
      (is (true? (joined-map? c)))
      (is (true? (joined-map? m)))
      (is (true? (every? joined-map? fs)))
      (is (= c (get-in c [:person/pet :pet/owner])))
      (is (= "Calvin" (:person/name c)))
      (is (= "Malcolm" (:pet/name m)))
      (is (= d (first fs)))
      ;; testing a "loop"
      (is (= c (get-in jm [:person/id 1 :person/pet :pet/owner])))))

  (testing "find"
    (let [c (get-in jm [:person/id 1])]
      (is (= [:person/name "Calvin"]
             (find c :person/name)))
      (is (nil? (find c :foobar)))))

  (testing "assoc"
    (let [c (get-in jm [:person/id 1])
          c' (assoc c :person/occupation "hacker")]
      (is (joined-map? c'))
      (is (= "hacker" (:person/occupation c')))
      (is (= c (get-in c' [:person/pet :pet/owner])))))

  (testing "meta"
    (let [jm' (with-meta jm {:foo "bar"})]
      (is (= {:foo "bar"} (meta jm')))))

  (testing "keys"
    (let [c (get-in jm [:person/id 1])]
      (is (= #{:person/id :pet/id}
             (set (keys jm))))
      (is (= #{1 2}
             (set (keys (:person/id jm)))))
      (is (= #{:person/name :person/friends :person/pet}
             (set (keys c))))))

  (testing "vals"
    (let [c (get-in jm [:person/id 1])]
      (is (every? joined-map? (vals jm)))
      (is (= #{"Calvin"
               [(joined-map #:person{:friends [[:person/id 1]], :name "Derek"})]
               (joined-map #:pet{:name "Malcolm", :owner [:person/id 1], :species :dog})}
             (set (vals c))))
      (is (every? joined-map? (reduce conj [] (vals jm))))))

  (testing "unwrap"
    (is (= db (unwrap jm))
      (= (get-in db [:person/id 1])
         (unwrap (get-in jm [:person/id 1])))))

  (testing "customize starting entity"
    (let [c   (get-in jm [:person/id 1])
          e   {:current/person [:person/id 1]}
          jm' (joined-map db e)]
      (is (= c (:current/person jm')))))

  (testing "to string"
    (is (= (str db) (str jm)))
    (is (= (str (get-in db [:person/id 1]))
           (str (get-in jm [:person/id 1]))))
    (is (= (str [(get-in db [:person/id 2])])
           (str (get-in jm [:person/id 1 :person/friends])))))

  (testing "hashing"
    (is (= (hash db) (hash jm)))
    (is (= (hash (get-in db [:person/id 1]))
           (hash (get-in jm [:person/id 1]))))
    (is (= (hash [(get-in db [:person/id 2])])
           (hash (get-in jm [:person/id 1 :person/friends])))))

  (testing "invoke"
    (is (= (joined-map (db :person/id))
           (jm :person/id))))

  (testing "reduce"
    (is (= (seq jm)
           (reduce conj [] jm)))
    (is (= (seq jm)
           (reduce-kv (fn [r k v] (conj r [k v])) [] jm))))

  #?(:clj (testing "Java iterator"
            (let [i (.iterator (get-in jm [:person/id 1]))
                  _ (.next i)
                  friends (val (.next i))]
              (is (= (get-in jm [:person/id 2])
                    (first friends)))))))