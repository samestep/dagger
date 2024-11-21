(ns irc.excerpt-test
  (:require [clojure.test :refer [deftest is]]
            [irc.excerpt :as irc]
            [util :refer [query with-context with-key]]))

(deftest example-3
;; Charlie and Diane are married and filing taxes jointly for the year 2018. Charlie was born on 3/15/1985 and Diane was born on 8/22/1953. In 2018, Charlie and Diane’s adjusted gross income was $185,000. Charlie and Diane do not itemize their deductions and do not qualify for any deductions other than the standard deduction.
  (let [db (atom {})]
    (let [year 2018]
      (with-key db ::irc/married
        {{:year year :person "Charlie"} true
         {:year year :person "Diane"} true})
      (with-key db ::irc/spouse
        {{:year year :person "Charlie"} "Diane"
         {:year year :person "Diane"} "Charlie"})
      (with-key db ::irc/filing-status
        {{:year year :person "Charlie"} :married-filing-jointly
         {:year year :person "Diane"} :married-filing-jointly})
      (with-key db ::irc/birth-date
        {{:person "Charlie"} [1985 3 15]
         {:person "Diane"} [1953 8 22]})
      (with-key db ::irc/adjusted-gross-income
        {{:year year :person "Charlie"} 185000
         {:year year :person "Diane"} 185000})
      (is (query db ::irc/section-63-f-1-b {:year year :person "Charlie"}))
      (is (not (query db ::irc/section-63-f-1-a {:year year :person "Charlie"}))))))
