(ns sara.section63
  (:refer-clojure :exclude [declare])
  (:require [util :refer [declare defcase define]]))

(declare ::filing-status [:year :person])

;; §63. Taxable income defined
;;
;; (a) In general
;;
;; Except as provided in subsection (b), for purposes of this subtitle, the term "taxable income" means gross income minus the deductions allowed by this chapter (other than the standard deduction).

(declare ::gross-income [:year :person])

(declare ::deductions [:year :person])

(declare ::taxable-income [:year :person])
(define ::taxable-income
  (fn [? _]
    (- (? ::gross-income) (? ::deductions))))

;; (b) Individuals who do not itemize their deductions
;;
;; In the case of an individual who does not elect to itemize his deductions for the taxable year, for purposes of this subtitle, the term "taxable income" means adjusted gross income, minus-
;;
;;     (1) the standard deduction, and
;;
;;     (2) the deduction for personal exemptions provided in section 151.

(declare ::itemize [:year :person])

(declare ::adjusted-gross-income [:year :person])
(define ::adjusted-gross-income
  (fn [? _]
    (? ::gross-income)))

(declare ::standard-deduction [:year :person])

(declare ::personal-exemptions [:year :person])

(defcase ::taxable-income
  (fn [? _]
    (not (? ::itemize)))
  (fn [? _ _]
    (- (? ::adjusted-gross-income)
       (? ::standard-deduction)
       (? ::personal-exemptions))))

;; (c) Standard deduction
;;
;; For purposes of this subtitle-
;;
;;     (1) In general
;;
;;     Except as otherwise provided in this subsection, the term "standard deduction" means the sum of-
;;
;;         (A) the basic standard deduction, and
;;
;;         (B) the additional standard deduction.

(declare ::basic-standard-deduction [:year :person])

(declare ::additional-standard-deduction [:year :person])

(define ::standard-deduction
  (fn [? _]
    (+ (? ::basic-standard-deduction)
       (? ::additional-standard-deduction))))

;;     (2) Basic standard deduction
;;
;;     For purposes of paragraph (1), the basic standard deduction is-
;;
;;         (A) 200 percent of the dollar amount in effect under subparagraph (C) for the taxable year in the case of-
;;
;;             (i) a joint return, or
;;
;;             (ii) a surviving spouse (as defined in section 2(a)),
;;
;;         (B) $4,400 in the case of a head of household (as defined in section 2(b)), or
;;
;;         (C) $3,000 in any other case.

(declare ::section-c-2-a-i [:year :person])
(define ::section-c-2-a-i
  (fn [? _]
    (= (? ::filing-status) :married-filing-jointly)))

(declare ::section-c-2-a-ii [:year :person])
(define ::section-c-2-a-ii
  (fn [? _]
    (? ::surviving-spouse)))

(declare ::subparagraph-c-2-b [:year])
(define ::subparagraph-c-2-b
  (fn [_ _]
    4400))

(declare ::subparagraph-c-2-c [:year])
(define ::subparagraph-c-2-c
  (fn [_ _]
    3000))

(declare ::surviving-spouse [:year :person])

(define ::basic-standard-deduction
  (fn [? _]
    (cond
      (or (? ::section-c-2-a-i) (? ::section-c-2-a-ii)) (* 2 (? ::subparagraph-c-2-c))
      (= (? ::filing-status) :head-of-household) (? ::subparagraph-c-2-b)
      :else (? ::subparagraph-c-2-c))))

;;     (3) Additional standard deduction for aged and blind
;;
;;     For purposes of paragraph (1), the additional standard deduction is the sum of each additional amount to which the taxpayer is entitled under subsection (f).

(declare ::additional-amounts [:year :person])

(define ::additional-standard-deduction
  (fn [? _]
    (reduce + (? ::additional-amounts))))

;;     (5) Limitation on basic standard deduction in the case of certain dependents
;;
;;     In the case of an individual with respect to whom a deduction under section 151 is allowable to another taxpayer for a taxable year beginning in the calendar year in which the individual's taxable year begins, the basic standard deduction applicable to such individual for such individual's taxable year shall not exceed the greater of-
;;
;;         (A) $500, or
;;
;;         (B) the sum of $250 and such individual's earned income.

(declare ::dependent [:year :person])

(declare ::section-c-5 [:year :person])
(define ::section-c-5
  (fn [? _]
    (? ::dependent)))

(declare ::earned-income [:year :person])
(define ::earned-income
  (fn [? _]
    (? ::gross-income)))

(defcase ::basic-standard-deduction
  (fn [? _]
    (? ::section-c-5))
  (fn [? _ original]
    (min (original) (max 500 (+ 250 (? ::earned-income))))))

;;     (6) Certain individuals, etc., not eligible for standard deduction
;;
;;     In the case of-
;;
;;         (A) a married individual filing a separate return where either spouse itemizes deductions,
;;
;;         (B) a nonresident alien individual, or
;;
;;         (D) an estate or trust, common trust fund, or partnership,
;;
;;     the standard deduction shall be zero.

(declare ::spouse [:year :person])

(declare ::section-c-6-a [:year :person])
(define ::section-c-6-a
  (fn [? _]
    (and (= (? ::filing-status) :married-filing-separately)
         (or (? ::itemize) (? ::itemize {:person (? ::spouse)})))))

(declare ::nonresident-alien [:year :person])

(declare ::section-c-6-b [:year :person])
(define ::section-c-6-b
  (fn [? _]
    (? ::nonresident-alien)))

(declare ::estate-or-trust [:year :person])

(declare ::common-trust-fund [:year :person])

(declare ::partnership [:year :person])

(declare ::section-c-6-d [:year :person])
(define ::section-c-6-d
  (fn [? _]
    (or (? ::estate-or-trust) (? ::common-trust-fund) (? ::partnership))))

(defcase ::standard-deduction
  (fn [? _]
    (or (? ::section-c-6-a) (? ::section-c-6-b) (? ::section-c-6-d)))
  (fn [_ _ _]
    0))

;;     (7) Special rules for taxable years 2018 through 2025
;;
;;     In the case of a taxable year beginning after December 31, 2017, and before January 1, 2026-
;;
;;     Paragraph (2) shall be applied-
;;
;;         (i) by substituting "$18,000" for "$4,400" in subparagraph (B), and
;;
;;         (ii) by substituting "$12,000" for "$3,000" in subparagraph (C).

(declare ::special-rules [:year])
(define ::special-rules
  (fn [_ {:keys [year]}]
    (<= 2018 year 2025)))

(defcase ::subparagraph-c-2-b
  (fn [? _]
    (? ::special-rules))
  (fn [_ _ _]
    18000))

(defcase ::subparagraph-c-2-c
  (fn [? _]
    (? ::special-rules))
  (fn [_ _ _]
    12000))

;; (d) Itemized deductions
;;
;; For purposes of this subtitle, the term "itemized deductions" means the deductions allowable under this chapter other than-
;;
;;     (1) the deductions allowable in arriving at adjusted gross income, and
;;
;;     (2) the deduction for personal exemptions provided by section 151.

(declare ::itemized-deductions [:year :person])

(define ::deductions
  (fn [? _]
    (+ (? ::itemized-deductions) (? ::personal-exemptions))))

;; (f) Aged or blind additional amounts

(declare ::bonus [:year :person])
(define ::bonus
  (fn [_ _]
    600))

(defn for-each [x bools]
  (* x (count (filter identity bools))))

(declare ::paragraph-1 [:year :person])

(declare ::paragraph-2 [:year :person])

(define ::additional-amounts
  (fn [? _]
    [(? ::paragraph-1) (? ::paragraph-2)]))

;;     (1) Additional amounts for the aged
;;
;;     The taxpayer shall be entitled to an additional amount of $600-
;;
;;         (A) for himself if he has attained age 65 before the close of his taxable year, and
;;
;;         (B) for the spouse of the taxpayer if the spouse has attained age 65 before the close of the taxable year and an additional exemption is allowable to the taxpayer for such spouse under section 151(b).

(declare ::birth-date [:person])

(declare ::age [:year :person])
(define ::age
  (fn [? {:keys [year]}]
    (let [[y] (? ::birth-date)]
      (- year y))))

(declare ::section-f-1-a [:year :person])
(define ::section-f-1-a
  (fn [? _]
    (>= (? ::age) 65)))

(declare ::additional-exemption-for-spouse [:year :person])

(declare ::section-f-1-b [:year :person])
(define ::section-f-1-b
  (fn [? _]
    (and (? ::married)
         (>= (? ::age {:person (? ::spouse)}) 65)
         (? ::additional-exemption-for-spouse))))

(define ::paragraph-1
  (fn [? _]
    (for-each (? ::bonus) [(? ::section-f-1-a) (? ::section-f-1-b)])))

;;     (2) Additional amount for blind
;;
;;     The taxpayer shall be entitled to an additional amount of $600-
;;
;;         (A) for himself if he is blind at the close of the taxable year, and
;;
;;         (B) for the spouse of the taxpayer if the spouse is blind as of the close of the taxable year and an additional exemption is allowable to the taxpayer for such spouse under section 151(b).
;;
;;     For purposes of subparagraph (B), if the spouse dies during the taxable year the determination of whether such spouse is blind shall be made as of the time of such death.

(declare ::blind [:year :person])

(declare ::section-f-2-a [:year :person])
(define ::section-f-2-a
  (fn [? _]
    (? ::blind)))

(declare ::section-f-2-b [:year :person])
(define ::section-f-2-b
  (fn [? _]
    (and (? ::married)
         (? ::blind {:person (? ::spouse)})
         (? ::additional-exemption-for-spouse))))

(define ::paragraph-2
  (fn [? _]
    (for-each (? ::bonus) [(? ::section-f-2-a) (? ::section-f-2-b)])))

;;     (3) Higher amount for certain unmarried individuals
;;
;;     In the case of an individual who is not married and is not a surviving spouse, paragraphs (1) and (2) shall be applied by substituting "$750" for "$600".

(declare ::married [:year :person])

(defcase ::bonus
  (fn [? _]
    (and (not (? ::married)) (not (? ::surviving-spouse))))
  (fn [_ _ _]
    750))

;; (g) Marital status
;;
;; For purposes of this section, marital status shall be determined under section 7703.
