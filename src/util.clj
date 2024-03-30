(ns util
  (:refer-clojure :exclude [declare])
  (:require [clojure.core :as clj]
            [clojure.set :as set]))

(def rules (atom {}))

(defn declare [key context]
  (if (namespace key)
    (swap! rules
           (fn [m]
             (if (contains? m key)
               (throw (ex-info "redeclared rule" {:key key}))
               (assoc m key {:context (set context)}))))
    (throw (ex-info "missing namespace" {:key key}))))

(defn define [key function]
  (swap! rules
         (fn [m]
           (if-let [v (get m key)]
             (if (contains? v :function)
               (throw (ex-info "redefined rule" {:key key}))
               (assoc m key (assoc v :function function)))
             (throw (ex-info "missing rule" {:key key}))))))

(defn defexception [key condition function]
  (swap! rules
         (fn [m]
           (if-let [v (get m key)]
             (assoc m key
                    (update v :exceptions (fnil conj [])
                            {:condition condition
                             :function function}))
             (throw (ex-info "missing rule" {:key key}))))))

(defn with-key [db key m]
  (let [expanded (into {} (map (fn [[c v]] [[key c] v]) m))]
    (swap! db merge expanded)))

(defn with-context [db context m]
  (let [expanded (into {} (map (fn [[k v]] [[k context] v]) m))]
    (swap! db merge expanded)))

(clj/declare query)

(defn evaluate [db key context function exceptions]
  (let [q (fn
            ([key] (query db key context))
            ([key additional]
             (query db key (merge context additional))))
        o (fn []
            (if function
              (function q context)
              (throw (ex-info "missing value"
                              {:key key :context context}))))
        e (->> exceptions
               (map (fn [m]
                      (assoc m :applies
                             ((:condition m) q context))))
               (filter :applies)
               vec)]
    (if-let [[{f :function} & more] (seq e)]
      (if (seq more)
        (throw (ex-info "multiple exceptions"
                        {:key key :context context}))
        (f q context o))
      (o))))

(defn query [db key context]
  (if-let [{ks :context f :function e :exceptions} (get @rules key)]
    (let [c (select-keys context ks)
          d (set/difference ks (set (keys c)))]
      (if (seq d)
        (throw (ex-info "missing context"
                        {:key key
                         :context ks
                         :given context
                         :missing d}))
        (let [k [key c]
              m @db
              v (if (contains? m k)
                  (get m k)
                  (evaluate db key c f e))]
          (swap! db assoc k v)
          v)))
    (throw (ex-info "missing rule" {:key key}))))
