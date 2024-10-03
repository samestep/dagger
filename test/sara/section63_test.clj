(ns sara.section63-test
  (:require [clojure.test :refer [deftest is]]
            [sara.section2 :as s2]
            [sara.section63 :as s63]
            [util :refer [query with-context with-key]]))

(deftest a-neg
  ;; In 2017, Alice was paid $33200. She is allowed a deduction under section 63(c) of $2000 and deductions of $4000 under section 151 for the year 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/personal-exemptions 2000
       ::s63/itemized-deductions 4000
       ::s63/itemize true})
    ;; Under section 63(a), Alice's taxable income in 2017 is equal to $27200. Contradiction
    (let [taxable-income
          (query db ::s63/taxable-income {:person "Alice" :year 2017})]
      (is (not= taxable-income 31200)))))

(deftest a-pos
  ;; In 2017, Alice was paid $33200. She is allowed deductions under section 151 of $2000 for the year 2017. She is allowed an itemized deduction of $4252 in 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/personal-exemptions 2000
       ::s63/itemized-deductions 4252
       ::s63/itemize true})
    ;; Under section 63(a), Alice's taxable income in 2017 is equal to $26948. Entailment
    (let [taxable-income
          (query db ::s63/taxable-income {:person "Alice" :year 2017})]
      (is (= taxable-income 26948)))))

(deftest b-neg
  ;; In 2017, Alice was paid $33200. She is allowed a deduction under section 63(c)(1) of $2000 for the year 2017, and no deduction under section 151. Alice takes the standard deduction.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/standard-deduction 2000
       ::s63/personal-exemptions 0
       ::s63/itemize false})
    ;; Under section 63(b), Alice's taxable income in 2017 is equal to $31400. Contradiction
    (let [taxable-income
          (query db ::s63/taxable-income {:person "Alice" :year 2017})]
      (is (not= taxable-income 31400)))))

(deftest b-pos
  ;; In 2017, Alice was paid $33200. She is allowed a deduction under section 63(c)(1) of $2000 for the year 2017, and no deduction under section 151. Alice decides not to itemize her deductions.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/standard-deduction 2000
       ::s63/personal-exemptions 0
       ::s63/itemize false})
    ;; Under section 63(b), Alice's taxable income in 2017 is equal to $31200. Entailment
    (let [taxable-income
          (query db ::s63/taxable-income {:person "Alice" :year 2017})]
      (is (= taxable-income 31200)))))

(deftest c-1-neg
  ;; In 2017, Alice was paid $33200. For the year 2017, Alice is allowed a basic standard deduction under section 63(c)(2) of $2000 and an additional standard deduction of $3000 under section 63(c)(3) for the year 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/basic-standard-deduction 2000
       ::s63/additional-standard-deduction 3000})
    ;; Under section 63(c)(1), Alice's standard deduction in 2017 is equal to $4000. Contradiction
    (let [standard-deduction
          (query db ::s63/standard-deduction {:person "Alice" :year 2017})]
      (is (not= standard-deduction 4000)))))

(deftest c-1-pos
  ;; In 2017, Alice was paid $33200. For the year 2017, Alice is allowed a basic standard deduction under section 63(c)(2) of $2000 and an additional standard deduction of $3000 under section 63(c)(3) for the year 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/basic-standard-deduction 2000
       ::s63/additional-standard-deduction 3000})
    ;; Under section 63(c)(1), Alice's standard deduction in 2017 is equal to $5000. Entailment
    (let [standard-deduction
          (query db ::s63/standard-deduction {:person "Alice" :year 2017})]
      (is (= standard-deduction 5000)))))

(deftest c-2-a-i-neg
  ;; In 2017, Alice was paid $33200 in remuneration. Alice and Bob have been married since Feb 3rd, 2017. Alice and Bob file separate returns in 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/filing-status :married-filing-separately})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"
       ::s63/filing-status :married-filing-separately})
    ;; Section 63(c)(2)(A)(i) applies to Alice in 2017. Contradiction
    (let [section-c-2-a-i
          (query db ::s63/section-c-2-a-i {:person "Alice" :year 2017})]
      (is (not section-c-2-a-i)))))

(deftest c-2-a-i-pos
  ;; In 2017, Alice was paid $33200 in remuneration. Alice and Bob have been married since Feb 3rd, 2017, and they file a joint return for 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/filing-status :married-filing-jointly})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"
       ::s63/filing-status :married-filing-jointly})
    ;; Section 63(c)(2)(A)(i) applies to Alice in 2017. Entailment
    (let [section-c-2-a-i
          (query db ::s63/section-c-2-a-i {:person "Alice" :year 2017})]
      (is section-c-2-a-i))))

(deftest c-2-a-ii-neg
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017, and they file a joint return for 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/filing-status :married-filing-jointly})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"
       ::s63/filing-status :married-filing-jointly})
    ;; Section 63(c)(2)(A)(ii) applies to Alice in 2017. Contradiction
    (let [section-c-2-a-ii
          (query db ::s63/section-c-2-a-ii {:person "Alice" :year 2017})]
      (is (not section-c-2-a-ii)))))

(deftest c-2-a-ii-pos
  ;; In 2017, Alice was paid $33200. Alice is a surviving spouse for 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s2/surviving-spouse true})
    ;; Section 63(c)(2)(A)(ii) applies to Alice in 2017. Entailment
    (let [section-c-2-a-ii
          (query db ::s63/section-c-2-a-ii {:person "Alice" :year 2017})]
      (is section-c-2-a-ii))))

(deftest c-2-b-neg
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017, and they file a joint return for 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/filing-status :married-filing-jointly})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"
       ::s63/filing-status :married-filing-jointly})
    ;; Under section 63(c)(2)(B), Alice's basic standard deduction in 2017 is equal to $4400. Contradiction
    (let [basic-standard-deduction
          (query db ::s63/basic-standard-deduction
                 {:person "Alice" :year 2017})]
      (is (not= basic-standard-deduction 4400)))))

(deftest c-2-b-pos
  ;; In 2017, Alice was paid $33200. Alice is a head of household for 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/filing-status :head-of-household})
    ;; Under section 63(c)(2)(B), Alice's basic standard deduction in 2017 is equal to $4400. Entailment
    (let [basic-standard-deduction
          (query db ::s63/basic-standard-deduction
                 {:person "Alice" :year 2017})]
      (is (= basic-standard-deduction 4400)))))

(deftest c-2-c-neg
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017, and they file a joint return for 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/filing-status :married-filing-jointly})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"
       ::s63/filing-status :married-filing-jointly})
    ;; Under section 63(c)(2)(C), Alice's basic standard deduction in 2017 is equal to $4400. Contradiction
    (let [basic-standard-deduction
          (query db ::s63/basic-standard-deduction
                 {:person "Alice" :year 2017})]
      (is (not= basic-standard-deduction 4400)))))

(deftest c-2-c-pos
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Alice and Bob file separate returns.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/filing-status :married-filing-separately})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"
       ::s63/filing-status :married-filing-separately})
    ;; Under section 63(c)(2)(C), Alice's basic standard deduction in 2017 is equal to $3000. Entailment
    (let [basic-standard-deduction
          (query db ::s63/basic-standard-deduction
                 {:person "Alice" :year 2017})]
      (is (= basic-standard-deduction 3000)))))

(deftest c-3-neg
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Alice is entitled to an additional standard deduction of $600 each for herself and for Bob, under section 63(f)(1)(A) and 63(f)(1)(B), respectively.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/paragraph-1 (* 600 2)})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"})
    ;; Under section 63(c)(3), Alice's additional standard deduction in 2017 is equal to $300. Contradiction
    (let [additional-standard-deduction
          (query db ::s63/additional-standard-deduction
                 {:person "Alice" :year 2017})]
      (is (not= additional-standard-deduction 300)))))

(deftest c-3-pos
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Alice is entitled to an additional standard deduction of $600 each for herself and for Bob, under section 63(f)(1)(A) and 63(f)(1)(B), respectively.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/paragraph-1 (* 600 2)})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"})
    ;; Under section 63(c)(3), Alice's additional standard deduction in 2017 is equal to $1200. Entailment
    (let [additional-standard-deduction
          (query db ::s63/additional-standard-deduction
                 {:person "Alice" :year 2017})]
      (is (= additional-standard-deduction 1200)))))

(deftest c-5-neg
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Bob earned $10 in 2017. Alice and Bob file separate returns. Alice is not entitled to a deduction for Bob under section 151.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/filing-status :married-filing-separately})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/gross-income 10
       ::s63/married true
       ::s63/spouse "Alice"
       ::s63/filing-status :married-filing-separately
       ::s63/dependent false})
    ;; Section 63(c)(5) applies to Bob's basic standard deduction in 2017. Contradiction
    (let [section-c-5 (query db ::s63/section-c-5 {:person "Bob" :year 2017})]
      (is (not section-c-5)))))

(deftest c-5-pos
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Alice is entitled to a deduction for Bob under section 151(b). Bob had no gross income in 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ;; the rest of this information is not explicit in the text
       ::s63/filing-status :married-filing-jointly})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/gross-income 0
       ::s63/married true
       ::s63/spouse "Alice"
       ::s63/dependent true
       ;; the rest of this information is not explicit in the text
       ::s63/filing-status :married-filing-jointly})
    ;; Under section 63(c)(5), Bob's basic standard deduction in 2017 is equal to at most $500. Entailment
    (let [basic-standard-deduction
          (query db ::s63/basic-standard-deduction {:person "Bob" :year 2017})]
      (is (= basic-standard-deduction 500)))))

(deftest c-6-a-neg
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Bob and Alice file a joint return for 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/filing-status :married-filing-jointly})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"
       ::s63/filing-status :married-filing-jointly})
    ;; Section 63(c)(6)(A) applies to Alice for 2017. Contradiction
    (let [section-c-6-a
          (query db ::s63/section-c-6-a {:person "Alice" :year 2017})]
      (is (not section-c-6-a)))))

(deftest c-6-a-pos
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Bob is allowed an itemized deduction of $4324. Alice and Bob file separate returns.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/filing-status :married-filing-separately})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"
       ::s63/itemize true
       ::s63/itemized-deductions 4324
       ::s63/filing-status :married-filing-separately})
    ;; Section 63(c)(6)(A) applies to Alice for 2017. Entailment
    (let [section-c-6-a
          (query db ::s63/section-c-6-a {:person "Alice" :year 2017})]
      (is section-c-6-a))))

(deftest c-6-b-neg
  ;; In 2017, Alice was paid $33200. Alice and Bob got married on Feb 3rd, 2017. Alice was a nonresident alien from August 23rd, 2015 to September 15th, 2016.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/nonresident-alien false})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"})
    ;; Section 63(c)(6)(B) applies to Alice for 2017. Contradiction
    (let [section-c-6-b
          (query db ::s63/section-c-6-b {:person "Alice" :year 2017})]
      (is (not section-c-6-b)))))

(deftest c-6-b-pos
  ;; In 2017, Alice was paid $33200. Alice and Bob got married on Feb 3rd, 2017. Alice was a nonresident alien from August 23rd, 2016 to September 15th, 2018.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/nonresident-alien true})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"})
    ;; Section 63(c)(6)(B) applies to Alice for 2017. Entailment
    (let [section-c-6-b
          (query db ::s63/section-c-6-b {:person "Alice" :year 2017})]
      (is section-c-6-b))))

(deftest c-6-d-neg
  ;; From 1973 to 2019, the Walter Brown Family Trust II was considered to be a business trust.
  (let [db (atom {})]
    (with-context db {:year 2021 :person "Walter Brown Family Trust II"}
      {::s63/estate-or-trust false})
    ;; Section 63(c)(6)(D) applies to the Walter Brown Family Trust II for 2021. Contradiction
    (let [section-c-6-d
          (query db ::s63/section-c-6-d
                 {:person "Walter Brown Family Trust II" :year 2021})]
      (is (not section-c-6-d)))))

(deftest c-6-d-pos
  ;; From 1973 to 2019, the Walter Brown Family Trust II was considered to be a business trust.
  (let [db (atom {})]
    (with-context db {:year 1999 :person "Walter Brown Family Trust II"}
      {::s63/estate-or-trust true})
    ;; Section 63(c)(6)(D) applies to the Walter Brown Family Trust II for 1999. Entailment
    (let [section-c-6-d
          (query db ::s63/section-c-6-d
                 {:person "Walter Brown Family Trust II" :year 1999})]
      (is section-c-6-d))))

(deftest c-7-i-neg
  ;; In 2019, Alice was paid $33200. Alice is a head of household for 2019.
  (let [db (atom {})]
    (with-context db {:year 2019 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/filing-status :head-of-household})
    ;; Under section 63(c)(7)(i), Alice's basic standard deduction in 2019 is equal to $4400. Contradiction
    (let [basic-standard-deduction
          (query db ::s63/basic-standard-deduction
                 {:person "Alice" :year 2019})]
      (is (not= basic-standard-deduction 4400)))))

(deftest c-7-i-pos
  ;; In 2019, Alice was paid $33200. Alice is a head of household for 2019.
  (let [db (atom {})]
    (with-context db {:year 2019 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/filing-status :head-of-household})
    ;; Under section 63(c)(7)(i), Alice's basic standard deduction in 2019 is equal to $18000. Entailment
    (let [basic-standard-deduction
          (query db ::s63/basic-standard-deduction
                 {:person "Alice" :year 2019})]
      (is (= basic-standard-deduction 18000)))))

(deftest c-7-ii-neg
  ;; In 2019, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2019.  Alice and Bob file separate returns in 2019.
  (let [db (atom {})]
    (with-context db {:year 2019 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/filing-status :married-filing-separately})
    (with-context db {:year 2019 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"
       ::s63/filing-status :married-filing-separately})
    ;; Under section 63(c)(7)(i), Alice's basic standard deduction in 2019 is equal to $4400. Contradiction
    (let [basic-standard-deduction
          (query db ::s63/basic-standard-deduction
                 {:person "Alice" :year 2019})]
      (is (not= basic-standard-deduction 4400)))))

(deftest c-7-ii-pos
  ;; In 2019, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2019. Alice and Bob file separate returns in 2019.
  (let [db (atom {})]
    (with-context db {:year 2019 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/filing-status :married-filing-separately})
    (with-context db {:year 2019 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"
       ::s63/filing-status :married-filing-separately})
    ;; Under section 63(c)(7)(ii), Alice's basic standard deduction in 2019 is equal to $12000. Entailment
    (let [basic-standard-deduction
          (query db ::s63/basic-standard-deduction
                 {:person "Alice" :year 2019})]
      (is (= basic-standard-deduction 12000)))))

(deftest f-1-a-neg
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Alice was born March 2nd, 1950 and Bob was born March 3rd, 1955.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"})
    (with-key db ::s63/birth-date
      {{:person "Alice"} [1950 3 2]
       {:person "Bob"} [1955 3 3]})
    ;; Section 63(f)(1)(A) applies to Bob in 2017. Contradiction
    (let [section-f-1-a
          (query db ::s63/section-f-1-a {:person "Bob" :year 2017})]
      (is (not section-f-1-a)))))

(deftest f-1-a-pos
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Alice was born March 2nd, 1950 and Bob was born March 3rd, 1955.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"})
    (with-key db ::s63/birth-date
      {{:person "Alice"} [1950 3 2]
       {:person "Bob"} [1955 3 3]})
    ;; Section 63(f)(1)(A) applies to Alice in 2017. Entailment
    (let [section-f-1-a
          (query db ::s63/section-f-1-a {:person "Alice" :year 2017})]
      (is section-f-1-a))))

(deftest f-1-b-neg
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Alice was born March 2nd, 1950 and Bob was born March 3rd, 1955. In addition, Alice is allowed an exemption for Bob under section 151(b) for the year 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/additional-exemption-for-spouse true})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"})
    (with-key db ::s63/birth-date
      {{:person "Alice"} [1950 3 2]
       {:person "Bob"} [1955 3 3]})
    ;; Section 63(f)(1)(B) applies to Alice with Bob as the spouse in 2017. Contradiction
    (let [section-f-1-b
          (query db ::s63/section-f-1-b {:person "Alice" :year 2017})]
      (is (not section-f-1-b)))))

(deftest f-1-b-pos
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Alice was born March 2nd, 1950 and Bob was born March 3rd, 1955. In addition, Bob is allowed an exemption for Alice under section 151(b) for the year 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"
       ::s63/additional-exemption-for-spouse true})
    (with-key db ::s63/birth-date
      {{:person "Alice"} [1950 3 2]
       {:person "Bob"} [1955 3 3]})
    ;; Section 63(f)(1)(B) applies to Bob with Alice as the spouse in 2017. Entailment
    (let [section-f-1-b
          (query db ::s63/section-f-1-b {:person "Bob" :year 2017})]
      (is section-f-1-b))))

(deftest f-2-a-neg
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Alice and Bob file separate returns in 2017. Alice has been blind since April 19, 2015.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/blind true})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"})
    ;; Section 63(f)(2)(A) applies to Bob in 2017. Contradiction
    (let [section-f-2-a
          (query db ::s63/section-f-2-a {:person "Bob" :year 2017})]
      (is (not section-f-2-a)))))

(deftest f-2-a-pos
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Alice has been blind since March 20, 2016.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/blind true})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"})
    ;; Section 63(f)(2)(A) applies to Alice in 2017. Entailment
    (let [section-f-2-a
          (query db ::s63/section-f-2-a {:person "Alice" :year 2017})]
      (is section-f-2-a))))

(deftest f-2-b-neg
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Alice has been blind since October 4, 2013. In addition, Alice is allowed an exemption for Bob under section 151(b) for the year 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/blind true
       ::s63/additional-exemption-for-spouse true})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"})
    ;; Section 63(f)(2)(B) applies to Alice in 2017. Contradiction
    (let [section-f-2-b
          (query db ::s63/section-f-2-b {:person "Alice" :year 2017})]
      (is (not section-f-2-b)))))

(deftest f-2-b-pos
  ;; In 2017, Alice was paid $33200. Alice and Bob have been married since Feb 3rd, 2017. Alice has been blind since Feb 28, 2014. In addition, Bob is allowed an exemption for Alice under section 151(b) for the year 2017.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200
       ::s63/married true
       ::s63/spouse "Bob"
       ::s63/blind true})
    (with-context db {:year 2017 :person "Bob"}
      {::s63/married true
       ::s63/spouse "Alice"
       ::s63/additional-exemption-for-spouse true})
    ;; Section 63(f)(2)(B) applies to Bob in 2017. Entailment
    (let [section-f-2-b
          (query db ::s63/section-f-2-b {:person "Bob" :year 2017})]
      (is section-f-2-b))))

(deftest f-3-neg
  ;; In 2017, Alice was paid $33200. Alice was born March 2nd, 1950 and Bob was born March 3rd, 1955.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200})
    (with-key db ::s63/birth-date
      {{:person "Alice"} [1950 3 2]
       {:person "Bob"} [1955 3 3]})
    ;; Under section 63(f)(3), Alice's additional standard deduction in 2017 is equal to $600. Contradiction
    (let [additional-standard-deduction
          (query db ::s63/additional-standard-deduction
                 {:person "Alice" :year 2017})]
      (is (not= additional-standard-deduction 600)))))

(deftest f-3-pos
  ;; In 2017, Alice was paid $33200. Alice was born March 2nd, 1950 and Bob was born March 3rd, 1955.
  (let [db (atom {})]
    (with-context db {:year 2017 :person "Alice"}
      {::s63/gross-income 33200})
    (with-key db ::s63/birth-date
      {{:person "Alice"} [1950 3 2]
       {:person "Bob"} [1955 3 3]})
    ;; Under section 63(f)(3), Alice's additional standard deduction in 2017 is equal to $750. Entailment
    (let [additional-standard-deduction
          (query db ::s63/additional-standard-deduction
                 {:person "Alice" :year 2017})]
      (is (= additional-standard-deduction 750)))))
