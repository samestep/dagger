(ns sara.section1-test
  (:require [clojure.test :refer [deftest is]]
            [sara.section1 :as s1]
            [util :refer [query with-context]]))

(deftest a-1-i-pos
  ;; Alice is married under section 7703 for the year 2017. Alice files a joint return with her spouse for 2017. Alice's and her spouse's taxable income for the year 2017 is $17330.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-jointly
       ::s1/taxable-income 17330})
    ;; Alice and her spouse have to pay $2600 in taxes for the year 2017 under section 1(a)(i). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 2600 tax)))))

(deftest a-1-ii-pos
  ;; Alice is married under section 7703 for the year 2017. Alice files a joint return with her spouse for 2017. Alice's and her spouse's taxable income for the year 2017 is $42876.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-jointly
       ::s1/taxable-income 42876})
    ;; Alice and her spouse have to pay $7208 in taxes for the year 2017 under section 1(a)(ii). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 7208 tax)))))

(deftest a-1-iii-pos
  ;; Alice is married under section 7703 for the year 2017. Alice files a joint return with her spouse for 2017. Alice's and her spouse's taxable income for the year 2017 is $103272.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-jointly
       ::s1/taxable-income 103272})
    ;; Alice and her spouse have to pay $24543 in taxes for the year 2017 under section 1(a)(iii). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 24543 tax)))))

(deftest a-1-iv-pos
  ;; Alice is married under section 7703 for the year 2017. Alice files a joint return with her spouse for 2017. Alice's and her spouse's taxable income for the year 2017 is $164612.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-jointly
       ::s1/taxable-income 164612})
    ;; Alice and her spouse have to pay $44789 in taxes for the year 2017 under section 1(a)(iv). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 44789 tax)))))

(deftest a-1-v-pos
  ;; Alice is married under section 7703 for the year 2017. Alice files a joint return with her spouse for 2017. Alice's and her spouse's taxable income for the year 2017 is $684642.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-jointly
       ::s1/taxable-income 684642})
    ;; Alice and her spouse have to pay $247647 in taxes for the year 2017 under section 1(a)(v). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 247647 tax)))))

(deftest a-1-neg
  ;; Alice is a head of household for the year 2017. Alice's taxable income for the year 2017 is $97407.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :head-of-household
       ::s1/taxable-income 97407
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice and her spouse have to pay $247647 in taxes for the year 2017 under section 1(a)(iv). Contradiction
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (not= 247647 tax)))))

(deftest a-1-pos
  ;; Alice is married under section 7703 for the year 2017. Alice files a joint return with her spouse for 2017. Alice's and her husband's taxable income for the year 2017 is $17330.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-jointly
       ::s1/taxable-income 17330})
    ;; Alice and her husband have to pay $2600 in taxes for the year 2017 under section 1(a). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 2600 tax)))))

(deftest a-2-i-pos
  ;; Alice is a surviving spouse for the year 2017. Alice's taxable income for the year 2017 is $25561.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/surviving-spouse true
       ::s1/taxable-income 25561
       ;; the rest of this information is not explicit in the text
       ::s1/filing-status :single})
    ;; Alice has to pay $3834 in taxes for the year 2017 under section 1(a)(i). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 3834 tax)))))

(deftest a-2-ii-pos
  ;; Alice is a surviving spouse for the year 2017. Alice's taxable income for the year 2017 is $70117.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/surviving-spouse true
       ::s1/taxable-income 70117
       ;; the rest of this information is not explicit in the text
       ::s1/filing-status :single})
    ;; Alice has to pay $14836 in taxes for the year 2017 under section 1(a)(ii). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 14836 tax)))))

(deftest a-2-iii-pos
  ;; Alice is a surviving spouse for the year 2017. Alice's taxable income for the year 2017 is $95129.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/surviving-spouse true
       ::s1/taxable-income 95129
       ;; the rest of this information is not explicit in the text
       ::s1/filing-status :single})
    ;; Alice has to pay $22018 in taxes for the year 2017 under section 1(a)(iii). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 22018 tax)))))

(deftest a-2-iv-pos
  ;; Alice is a surviving spouse for the year 2017. Alice's taxable income for the year 2017 is $236422.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/surviving-spouse true
       ::s1/taxable-income 236422
       ;; the rest of this information is not explicit in the text
       ::s1/filing-status :single})
    ;; Alice has to pay $70640 in taxes for the year 2017 under section 1(a)(iv). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 70640 tax)))))

(deftest a-2-v-pos
  ;; Alice is a surviving spouse for the year 2017. Alice's taxable income for the year 2017 is $615572.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/surviving-spouse true
       ::s1/taxable-income 615572
       ;; the rest of this information is not explicit in the text
       ::s1/filing-status :single})
    ;; Alice has to pay $220295 in taxes for the year 2017 under section 1(a)(v). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 220295 tax)))))

(deftest a-2-pos
  ;; Alice is a surviving spouse for the year 2017. Alice's taxable income for the year 2017 is $70117.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/surviving-spouse true
       ::s1/taxable-income 70117
       ;; the rest of this information is not explicit in the text
       ::s1/filing-status :single})
    ;; Alice has to pay $14836 in taxes for the year 2017 under section 1(a). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 14836 tax)))))

(deftest b-i-pos
  ;; Alice is a head of household for the year 2017. Alice's taxable income for the year 2017 is $9560.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :head-of-household
       ::s1/taxable-income 9560
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $1434 in taxes for the year 2017 under section 1(b)(i). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 1434 tax)))))

(deftest b-ii-pos
  ;; Alice is a head of household for the year 2017. Alice's taxable income for the year 2017 is $54775.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :head-of-household
       ::s1/taxable-income 54775
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $11489 in taxes for the year 2017 under section 1(b)(ii). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 11489 tax)))))

(deftest b-iii-pos
  ;; Alice is a head of household for the year 2017. Alice's taxable income for the year 2017 is $97407.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :head-of-household
       ::s1/taxable-income 97407
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $24056 in taxes for the year 2017 under section 1(b)(iii). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 24056 tax)))))

(deftest b-iv-pos
  ;; Alice is a head of household for the year 2017. Alice's taxable income for the year 2017 is $194512.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :head-of-household
       ::s1/taxable-income 194512
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $57509 in taxes for the year 2017 under section 1(b)(iv). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 57509 tax)))))

(deftest b-v-pos
  ;; Alice is a head of household for the year 2017. Alice's taxable income for the year 2017 is $1172980.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :head-of-household
       ::s1/taxable-income 1172980
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $442985 in taxes for the year 2017 under section 1(b)(v). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 442985 tax)))))

(deftest b-pos
  ;; Alice is a head of household for the year 2017. Alice's taxable income for the year 2017 is $97407.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :head-of-household
       ::s1/taxable-income 97407
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $24056 in taxes for the year 2017 under section 1(b). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 24056 tax)))))

(deftest c-i-pos
  ;; Alice's taxable income for the year 2017 is $7748. In 2017, Alice is not married, is not a surviving spouse, and is not a head of household.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :single
       ::s1/taxable-income 7748
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $1162 in taxes for the year 2017 under section 1(c)(i). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 1162 tax)))))

(deftest c-ii-pos
  ;; Alice's taxable income for the year 2017 is $22895. Alice is not married, is not a surviving spouse, and is not a head of household in 2017.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :single
       ::s1/taxable-income 22895
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $3538 in taxes for the year 2017 under section 1(c)(ii). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 3538 tax)))))

(deftest c-iii-pos
  ;; Alice's taxable income for the year 2017 is $102268. Alice is not married, is not a surviving spouse, and is not a head of household in 2017.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :single
       ::s1/taxable-income 102268
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $27225 in taxes for the year 2017 under section 1(c)(iii). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 27225 tax)))))

(deftest c-iv-pos
  ;; Alice's taxable income for the year 2017 is $210204. In 2017, Alice is not married, is not a surviving spouse, and is not a head of household.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :single
       ::s1/taxable-income 210204
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $65445 in taxes for the year 2017 under section 1(c)(iv). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 65445 tax)))))

(deftest c-v-pos
  ;; Alice's taxable income for the year 2017 is $718791. Alice is not married, is not a surviving spouse, and is not a head of household in 2017.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :single
       ::s1/taxable-income 718791
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $265413 in taxes for the year 2017 under section 1(c)(v). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 265413 tax)))))

(deftest c-pos
  ;; Alice's taxable income for the year 2017 is $210204. Alice is not married, is not a surviving spouse, and is not a head of household in 2017.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :single
       ::s1/taxable-income 210204
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $65445 in taxes for the year 2017 under section 1(c). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 65445 tax)))))

(deftest d-i-pos
  ;; Alice is married under section 7703 for the year 2017. Alice's taxable income for the year 2017 is $6662. Alice files taxes separately in 2017.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-separately
       ::s1/taxable-income 6662
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $999 in taxes for the year 2017 under section 1(d)(i). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 999 tax)))))

(deftest d-ii-pos
  ;; Alice is married under section 7703 for the year 2017. Alice's taxable income for the year 2017 is $28864. Alice files a separate return.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-separately
       ::s1/taxable-income 28864
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $5683 in taxes for the year 2017 under section 1(d)(ii). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 5683 tax)))))

(deftest d-iii-pos
  ;; Alice is married under section 7703 for the year 2017. Alice's taxable income for the year 2017 is $67285. Alice files a separate return.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-separately
       ::s1/taxable-income 67285
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $17123 in taxes for the year 2017 under section 1(d)(iii). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 17123 tax)))))

(deftest d-iv-pos
  ;; Alice is married under section 7703 for the year 2017. Alice's taxable income for the year 2017 is $113580. Alice files a separate return.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-separately
       ::s1/taxable-income 113580
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $33653 in taxes for the year 2017 under section 1(d)(iv). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 33653 tax)))))

(deftest d-v-neg
  ;; Alice is married under section 7703 for the year 2017. Alice's taxable income for the year 2017 is $554313. Alice files a separate return.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-separately
       ::s1/taxable-income 554313
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $20772 in taxes for the year 2017 under section 1(d)(v). Contradiction
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (not= 20772 tax)))))

(deftest d-v-pos
  ;; Alice is married under section 7703 for the year 2017. Alice's taxable income for the year 2017 is $554313. Alice files a separate return.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-separately
       ::s1/taxable-income 554313
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $207772 in taxes for the year 2017 under section 1(d)(v). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 207772 tax)))))

(deftest d-pos
  ;; Alice is married under section 7703 for the year 2017. Alice's taxable income for the year 2017 is $554313. Alice files a separate return.
  (let [db (atom {})]
    (with-context db {:return :alice}
      {::s1/year 2017
       ::s1/filing-status :married-filing-separately
       ::s1/taxable-income 554313
       ;; the rest of this information is not explicit in the text
       ::s1/surviving-spouse false})
    ;; Alice has to pay $207772 in taxes for the year 2017 under section 1(d). Entailment
    (let [tax (query db ::s1/tax {:return :alice})]
      (is (= 207772 tax)))))
