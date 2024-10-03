(ns sara.section2-test
  (:require [clojure.test :refer [deftest is]]
            [sara.common :as common]
            [sara.section2 :as s2]
            [util :refer [query with-context with-key]]))

(deftest a-1-a-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014.
  (let [db (atom {})]
    (with-context db {:year 2014 :person "Bob"}
      {::common/spouse "Alice"})
    (with-key db ::common/death-date
      {{:person "Alice"} [2014 7 9]})
    ;; Section 2(a)(1)(A) applies to Bob in 2014. Contradiction
    (let [section-a-1-a
          (query db ::s2/section-a-1-a {:person "Bob" :year 2014})]
      (is (not section-a-1-a)))))

(deftest a-1-a-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014.
  (let [db (atom {})]
    (with-context db {:year 2015 :person "Bob"}
      {::common/spouse "Alice"})
    (with-key db ::common/death-date
      {{:person "Alice"} [2014 7 9]})
    ;; Section 2(a)(1)(A) applies to Bob in 2015. Entailment
    (let [section-a-1-a
          (query db ::s2/section-a-1-a {:person "Bob" :year 2015})]
      (is section-a-1-a))))

(deftest a-1-b-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice has a child, Charlie, born October 9th, 2000. Alice died on July 9th, 2021. From 2011 to 2024, Bob furnished the costs of maintaining the home where he and Charlie lived during that time. In 2020, Charlie filed a joint return with his spouse whom he married on Dec 1st, 2019. Charlie earned $312489 in 2020.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(a)(1)(B) applies to Bob in 2020. Contradiction
    (let [section-a-1-b
          (query db ::s2/section-a-1-b {:person "Bob" :year 2020})]
      (is (not section-a-1-b)))))

(deftest a-1-b-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice and Bob have a child, Charlie, born October 9th, 2000. Alice died on July 9th, 2014. From 2004 to 2017, Bob furnished the costs of maintaining the home where he and Charlie lived during that time. From 2014 to 2017, Bob was entitled to a deduction for Charlie under section 151. Bob's income in 2016 was $553252.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(a)(1)(B) applies to Bob in 2016. Entailment
    (let [section-a-1-b
          (query db ::s2/section-a-1-b {:person "Bob" :year 2016})]
      (is section-a-1-b))))

(deftest a-2-a-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014. Bob married Charlie on September 14th, 2015.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(a)(2)(A) applies to Bob in 2014. Contradiction
    (let [section-a-2-a
          (query db ::s2/section-a-2-a {:person "Bob" :year 2014})]
      (is (not section-a-2-a)))))

(deftest a-2-a-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014. Bob married Charlie on September 14th, 2015.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(a)(2)(A) applies to Bob in 2015. Entailment
    (let [section-a-2-a
          (query db ::s2/section-a-2-a {:person "Bob" :year 2015})]
      (is section-a-2-a))))

(deftest a-2-b-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014. Alice was a nonresident alien since March 4th, 1990.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(a)(2)(B) applies to Bob in 2014. Contradiction
    (let [section-a-2-b
          (query db ::s2/section-a-2-b {:person "Bob" :year 2014})]
      (is (not section-a-2-b)))))

(deftest a-2-b-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(a)(2)(B) applies to Bob in 2014. Entailment
    (let [section-a-2-b
          (query db ::s2/section-a-2-b {:person "Bob" :year 2014})]
      (is section-a-2-b))))

(deftest b-1-a-i-i-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice and Bob have a child, Charlie, born October 9th, 2000. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and Charlie lived during that time. Charlie married Dan on Feb 14th, 2018.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1)(A)(i)(I) applies to Charlie in 2017. Contradiction
    (let [section-b-1-a-i-i
          (query db ::s2/section-b-1-a-i-i {:person "Charlie" :year 2017})]
      (is (not section-b-1-a-i-i)))))

(deftest b-1-a-i-i-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice and Bob have a child, Charlie, born October 9th, 2000. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and Charlie lived during that time. Charlie married Dan on Feb 14th, 2017.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1)(A)(i)(I) applies to Charlie in 2017. Entailment
    (let [section-b-1-a-i-i
          (query db ::s2/section-b-1-a-i-i {:person "Charlie" :year 2017})]
      (is section-b-1-a-i-i))))

(deftest b-1-a-i-ii-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice and Bob have a child, Charlie, born October 9th, 2000. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and Charlie lived during that time. Charlie married Dan on Feb 14th, 2018.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1)(A)(i)(II) applies to Bob with Charlie as the qualifying child in 2018. Contradiction
    (let [section-b-1-a-i-ii
          (query db ::s2/section-b-1-a-i-ii
                 {:person "Bob" :child "Charlie" :year 2018})]
      (is (not section-b-1-a-i-ii)))))

(deftest b-1-a-i-ii-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice and Bob have a child, Charlie, born October 9th, 2000. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and Charlie lived during that time. Charlie married Dan on Feb 14th, 2018. Section 152(b)(2) applies to Charlie as the dependent and Bob as the taxpayer for 2018.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1)(A)(i)(II) applies to Bob with Charlie as the qualifying child in 2018. Entailment
    (let [section-b-1-a-i-ii
          (query db ::s2/section-b-1-a-i-ii
                 {:person "Bob" :child "Charlie" :year 2018})]
      (is section-b-1-a-i-ii))))

(deftest b-1-a-i-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice and Bob have a child, Charlie, born October 9th, 2000. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and Charlie lived during that time. Charlie married Dan on Feb 14th, 2018. Section 152(b)(2) applies to Bob as the dependent and Charlie as the taxpayer for 2018.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1)(A)(i) applies to Bob in 2018. Contradiction
    (let [section-b-1-a-i
          (query db ::s2/section-b-1-a-i {:person "Bob" :year 2018})]
      (is (not section-b-1-a-i)))))

(deftest b-1-a-i-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice and Bob have a child, Charlie, born October 9th, 2000. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and Charlie lived during that time.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1)(A)(i) applies to Charlie in 2018. Entailment
    (let [section-b-1-a-i
          (query db ::s2/section-b-1-a-i {:person "Charlie" :year 2018})]
      (is section-b-1-a-i))))

(deftest b-1-a-ii-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Bob has a brother, Charlie, born October 9th, 2000. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and Charlie lived during that time. In 2017, Charlie earned $312489. In 2017, Charlie filed a joint return with his spouse whom he married on Dec 1st, 2016.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1)(A)(ii) applies to Charlie as the dependent in 2017. Contradiction
    (let [section-b-1-a-ii
          (query db ::s2/section-b-1-a-ii {:person "Charlie" :year 2017})]
      (is (not section-b-1-a-ii)))))

(deftest b-1-a-ii-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Bob has a brother, Charlie, born October 9th, 2000. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and Charlie lived during that time. From 2015 to 2019, Bob was entitled to a deduction for Charlie under section 151(c). In 2017, Bob earned $5254312.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1)(A)(ii) applies to Charlie as the dependent in 2017. Entailment
    (let [section-b-1-a-ii
          (query db ::s2/section-b-1-a-ii {:person "Charlie" :year 2017})]
      (is section-b-1-a-ii))))

(deftest b-1-a-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice and Bob have a child, Charlie, born October 9th, 2000. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished 40% of the costs of maintaining the home where he and Charlie lived during that time. Charlie is not the dependent of Bob under section 152(b)(2).
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1)(A) applies to Bob in 2018. Contradiction
    (let [section-b-1-a
          (query db ::s2/section-b-1-a {:person "Bob" :year 2018})]
      (is (not section-b-1-a)))))

(deftest b-1-a-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice and Bob have a child, Charlie, born October 9th, 2000. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and Charlie lived during that time. Charlie was a qualifying child of Bob under section 152(c) from 2004 to 2019.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1)(A) applies to Bob in 2018. Entailment
    (let [section-b-1-a
          (query db ::s2/section-b-1-a {:person "Bob" :year 2018})]
      (is section-b-1-a))))

(deftest b-1-b-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice and Bob have a child, Charlie, born October 9th, 2000. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and Charlie lived during that time. Bob is entitled to a deduction for Charlie under section 151(c) for the years 2015 to 2019.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1)(B) applies to Bob in 2018. Contradiction
    (let [section-b-1-b
          (query db ::s2/section-b-1-b {:person "Bob" :year 2018})]
      (is (not section-b-1-b)))))

(deftest b-1-b-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and and his father Charlie lived during that time. Bob is entitled to a deduction for Charlie under section 151(c) for the years 2015 to 2019.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1)(B) applies to Bob in 2018. Entailment
    (let [section-b-1-b
          (query db ::s2/section-b-1-b {:person "Bob" :year 2018})]
      (is section-b-1-b))))

(deftest b-1-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and his son Charlie lived during that time. Bob is entitled to a deduction for Charlie under section 151(c) for the years 2015 to 2019.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1) applies to Bob in 2016. Contradiction
    (let [section-b-1 (query db ::s2/section-b-1 {:person "Bob" :year 2016})]
      (is (not section-b-1)))))

(deftest b-1-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and and his father Charlie lived during that time. Bob is entitled to a deduction for Charlie under section 151(c) for the years 2015 to 2019.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(1) applies to Bob in 2018. Entailment
    (let [section-b-1 (query db ::s2/section-b-1 {:person "Bob" :year 2018})]
      (is section-b-1))))

(deftest b-2-a-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice and Bob were legally separated under a decree of separate maintenance on July 9th, 2014.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(2)(A) applies to Alice and Bob in 2010. Contradiction
    (let [section-b-2-a
          (query db ::s2/section-b-2-a
                 {:person "Alice" :spouse "Bob" :year 2010})]
      (is (not section-b-2-a)))))

(deftest b-2-a-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice and Bob were legally separated under a decree of separate maintenance on July 9th, 2014.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(2)(A) applies to Alice and Bob in 2018. Entailment
    (let [section-b-2-a
          (query db ::s2/section-b-2-a
                 {:person "Alice" :spouse "Bob" :year 2018})]
      (is section-b-2-a))))

(deftest b-2-b-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice was a nonresident alien until July 9th, 2014.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(2)(B) applies to Bob in 2015. Contradiction
    (let [section-b-2-b
          (query db ::s2/section-b-2-b {:person "Bob" :year 2015})]
      (is (not section-b-2-b)))))

(deftest b-2-b-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice was a nonresident alien until July 9th, 2014.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(2)(B) applies to Bob in 2013. Entailment
    (let [section-b-2-b
          (query db ::s2/section-b-2-b {:person "Bob" :year 2013})]
      (is section-b-2-b))))

(deftest b-2-c-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice was a nonresident alien. Alice died on July 9th, 2014.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(2)(C) applies to Bob in 2014. Contradiction
    (let [section-b-2-c
          (query db ::s2/section-b-2-c {:person "Bob" :year 2014})]
      (is (not section-b-2-c)))))

(deftest b-2-c-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(2)(C) applies to Bob in 2014. Entailment
    (let [section-b-2-c
          (query db ::s2/section-b-2-c {:person "Bob" :year 2014})]
      (is section-b-2-c))))

(deftest b-3-a-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and and his father Charlie lived during that time. Bob is entitled to a deduction for Charlie under section 151(c) for the years 2015 to 2019. Bob was a nonresident alien until Feb 12, 2018.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(3)(A) applies to Bob in 2019. Contradiction
    (let [section-b-3-a
          (query db ::s2/section-b-3-a {:person "Bob" :year 2019})]
      (is (not section-b-3-a)))))

(deftest b-3-a-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and and his father Charlie lived during that time. Bob is entitled to a deduction for Charlie under section 151(c) for the years 2015 to 2019. Bob was a nonresident alien until Feb 12, 2018.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(3)(A) applies to Bob in 2018. Entailment
    (let [section-b-3-a
          (query db ::s2/section-b-3-a {:person "Bob" :year 2018})]
      (is section-b-3-a))))

(deftest b-3-b-neg
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014. From 2004 to 2019, Bob furnished the costs of maintaining the home where he and and his father Charlie lived during that time. Bob is entitled to a deduction for Charlie under section 151(c) for the years 2015 to 2019.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(3)(B) applies to Bob in 2018. Contradiction
    (let [section-b-3-b
          (query db ::s2/section-b-3-b {:person "Bob" :year 2018})]
      (is (not section-b-3-b)))))

(deftest b-3-b-pos
  ;; Alice and Bob got married on Feb 3rd, 1992. Alice died on July 9th, 2014. From 2015 to 2019, Bob furnished the costs of maintaining the home where he and and his friend Charlie lived during that time. Charlie is a dependent of Bob under section 152(d)(2)(H) for the years 2015 to 2019. Bob earned $300000 every year from 2015 to 2019.
  (let [db (atom {})]
    (with-context db {}
      {})
    ;; Section 2(b)(3)(B) applies to Bob as the taxpayer and Charlie as the individual in 2018. Entailment
    (let [section-b-3-b
          (query db ::s2/section-b-3-b {:person "Bob" :year 2018})]
      (is section-b-3-b))))
