(ns sara.section1
  (:refer-clojure :exclude [declare])
  (:require [clojure.math :refer [round]]
            [sara.section2 :as s2]
            [util :refer [condition declare defcase define]]))

;; §1. Tax imposed

(declare ::year [:return])

(declare ::person [:return])

(declare ::filing-status [:return])

(declare ::tax [:return])

(declare ::tax-unrounded [:return])

(define ::tax
  (fn [? _]
    (round (? ::tax-unrounded))))

;; (a) Married individuals filing joint returns and surviving spouses
;;
;; There is hereby imposed on the taxable income of-
;;
;;     (1) every married individual (as defined in section 7703) who makes a single return jointly with his spouse, and
;;
;;     (2) every surviving spouse (as defined in section 2(a)),
;;
;; a tax determined in accordance with the following:
;;
;;     (i) 15% of taxable income if the taxable income is not over $36,900;
;;     (ii) $5,535, plus 28% of the excess over $36,900 if the taxable income is over $36,900 but not over $89,150;
;;     (iii) $20,165, plus 31% of the excess over $89,150 if the taxable income is over $89,150 but not over $140,000;
;;     (iv) $35,928.50, plus 36% of the excess over $140,000 if the taxable income is over $140,000 but not over $250,000;
;;     (v) $75,528.50, plus 39.6% of the excess over $250,000 if the taxable income is over $250,000.

(declare ::taxable-income [:return])

(condition ::surviving-spouse [:return])
(define ::surviving-spouse
  (fn [? _]
    (? ::s2/surviving-spouse
       {:year (? ::year) :person (? ::person)})))

(defcase ::tax-unrounded
  (fn [? _]
    (or (= (? ::filing-status) :married-filing-jointly) (? ::surviving-spouse)))
  (fn [? _ _]
    (let [taxable-income (? ::taxable-income)]
      (cond
        (<= taxable-income 36900)
        (* 15/100 taxable-income)

        (<= taxable-income 89150)
        (+ 5535 (* 28/100 (- taxable-income 36900)))

        (<= taxable-income 140000)
        (+ 20165 (* 31/100 (- taxable-income 89150)))

        (<= taxable-income 250000)
        (+ 35928 50/100 (* 36/100 (- taxable-income 140000)))

        :else
        (+ 75528 50/100 (* 396/1000 (- taxable-income 250000)))))))

;; (b) Heads of households
;;
;; There is hereby imposed on the taxable income of every head of a household (as defined in section 2(b)) a tax determined in accordance with the following:
;;
;;     (i) 15% of taxable income if the taxable income is not over $29,600;
;;     (ii) $4,440, plus 28% of the excess over $29,600 if the taxable income is over $29,600 but not over $76,400;
;;     (iii) $17,544, plus 31% of the excess over $76,400 if the taxable income is over $76,400 but not over $127,500;
;;     (iv) $33,385, plus 36% of the excess over $127,500 if the taxable income is over $127,500 but not over $250,000;
;;     (v) $77,485, plus 39.6% of the excess over $250,000 if the taxable income is over $250,000.

(defcase ::tax-unrounded
  (fn [? _]
    (= (? ::filing-status) :head-of-household))
  (fn [? _ _]
    (let [taxable-income (? ::taxable-income)]
      (cond
        (<= taxable-income 29600)
        (* 15/100 taxable-income)

        (<= taxable-income 76400)
        (+ 4440 (* 28/100 (- taxable-income 29600)))

        (<= taxable-income 127500)
        (+ 17544 (* 31/100 (- taxable-income 76400)))

        (<= taxable-income 250000)
        (+ 33385 (* 36/100 (- taxable-income 127500)))

        :else
        (+ 77485 (* 396/1000 (- taxable-income 250000)))))))

;; (c) Unmarried individuals (other than surviving spouses and heads of households)
;;
;; There is hereby imposed on the taxable income of every individual (other than a surviving spouse as defined in section 2(a) or the head of a household as defined in section 2(b)) who is not a married individual (as defined in section 7703) a tax determined in accordance with the following:
;;
;;     (i) 15% of taxable income if the taxable income is not over $22,100;
;;     (ii) $3,315, plus 28% of the excess over $22,100 if the taxable income is over $22,100 but not over $53,500;
;;     (iii) $12,107, plus 31% of the excess over $53,500 if the taxable income is over $53,500 but not over $115,000;
;;     (iv) $31,172, plus 36% of the excess over $115,000 if the taxable income is over $115,000 but not over $250,000;
;;     (v) $79,772, plus 39.6% of the excess over $250,000 if the taxable income is over $250,000.

(defcase ::tax-unrounded
  (fn [? _]
    (and (= (? ::filing-status) :single) (not (? ::surviving-spouse))))
  (fn [? _ _]
    (let [taxable-income (? ::taxable-income)]
      (cond
        (<= taxable-income 22100)
        (* 15/100 taxable-income)

        (<= taxable-income 53500)
        (+ 3315 (* 28/100 (- taxable-income 22100)))

        (<= taxable-income 115000)
        (+ 12107 (* 31/100 (- taxable-income 53500)))

        (<= taxable-income 250000)
        (+ 31172 (* 36/100 (- taxable-income 115000)))

        :else
        (+ 79772 (* 396/1000 (- taxable-income 250000)))))))

;; (d) Married individuals filing separate returns
;;
;; There is hereby imposed on the taxable income of every married individual (as defined in section 7703) who does not make a single return jointly with his spouse, a tax determined in accordance with the following:
;;
;;     (i) 15% of taxable income if the taxable income is not over $18,450;
;;     (ii) $2,767.50, plus 28% of the excess over $18,450 if the taxable income is over $18,450 but not over $44,575;
;;     (iii) $10,082.50, plus 31% of the excess over $44,575 if the taxable income is over $44,575 but not over $70,000;
;;     (iv) $17,964.25, plus 36% of the excess over $70,000 if the taxable income is over $70,000 but not over $125,000;
;;     (v) $37,764.25, plus 39.6% of the excess over $125,000 if the taxable income is over $125,000

(defcase ::tax-unrounded
  (fn [? _]
    (= (? ::filing-status) :married-filing-separately))
  (fn [? _ _]
    (let [taxable-income (? ::taxable-income)]
      (cond
        (<= taxable-income 18450)
        (* 15/100 taxable-income)

        (<= taxable-income 44575)
        (+ 2767 50/100 (* 28/100 (- taxable-income 18450)))

        (<= taxable-income 70000)
        (+ 10082 50/100 (* 31/100 (- taxable-income 44575)))

        (<= taxable-income 125000)
        (+ 17964 25/100 (* 36/100 (- taxable-income 70000)))

        :else
        (+ 37764 25/100 (* 396/1000 (- taxable-income 125000)))))))
