(ns sara.common
  (:refer-clojure :exclude [declare])
  (:require [util :refer [condition declare define]]))

(declare ::birth-date [:person])

(declare ::age [:year :person])
(define ::age
  (fn [? {:keys [year]}]
    (let [[y] (? ::birth-date)]
      (- year y))))

(declare ::death-year [:person])

(declare ::spouse [:year :person])

(condition ::married [:year :person])
