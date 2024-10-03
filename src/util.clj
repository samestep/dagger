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

(defn default [key value]
  (swap! rules
         (fn [m]
           (if-let [v (get m key)]
             (if (contains? v :default)
               (throw (ex-info "redefined default" {:key key}))
               (assoc m key (assoc v :default value)))
             (throw (ex-info "missing rule" {:key key}))))))

(defn condition [key context]
  (declare key context)
  (default key false))

(defn define [key function]
  (swap! rules
         (fn [m]
           (if-let [v (get m key)]
             (if (contains? v :function)
               (throw (ex-info "redefined rule" {:key key}))
               (assoc m key (assoc v :function function)))
             (throw (ex-info "missing rule" {:key key}))))))

(defn defcase [key condition function]
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
                              {:kind :missing :key key :context context}))))
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
  (if-let [info (get @rules key)]
    (let [{ks :context f :function e :exceptions} info
          c (select-keys context ks)
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
                  (try
                    (evaluate db key c f e)
                    (catch Exception ex
                      (if (and (= :missing (:kind (ex-data ex)))
                               (contains? info :default))
                        (:default info)
                        (throw ex)))))]
          (swap! db assoc k v)
          v)))
    (throw (ex-info "missing rule" {:key key}))))
