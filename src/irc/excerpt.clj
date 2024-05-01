(ns irc.excerpt
  (:refer-clojure :exclude [declare])
  (:require [util :refer [declare defcase define]]))

;; https://arxiv.org/abs/2404.09868

(defn for-each [x bools]
  (* x (count (filter identity bools))))

(declare ::birth-date [:person])

(declare ::age [:year :person])
(define ::age
  (fn [? {:keys [year]}]
    (let [[y] (? ::birth-date)]
      (- year y))))

(declare ::married [:year :person])

(declare ::spouse [:year :person])

(declare ::adjusted-gross-income [:year :person])
(define ::adjusted-gross-income
  (fn [? _]
    (? ::gross-income)))

(declare ::filing-status [:year :person])

(declare ::dependent [:year :person])

(declare ::surviving-spouse [:year :person])

;; §63. Taxable income defined
;;
;; ...

(declare ::taxable-income [:year :person])

;; (b) Individuals who do not itemize their deductions
;;
;;   In the case of an individual who does not elect to itemize his deductions for the taxable year, for purposes of this subtitle, the term “taxable income” means adjusted gross income, minus—
;;
;;   (1) the standard deduction,
;;
;;   ...

(define ::taxable-income
  (fn [? _]
    (- (? ::adjusted-gross-income) (? ::standard-deduction))))

;; (c) Standard deduction

(declare ::standard-deduction [:year :person])

;;     (1) In general
;;
;;     Except as otherwise provided in this subsection, the term "standard deduction" means the sum of-
;;
;;         (A) the basic standard deduction, and
;;
;;         (B) the additional standard deduction.

(define ::standard-deduction
  (fn [? _]
    (+ (? ::basic-standard-deduction) (? ::additional-standard-deduction))))

;;     (2) Basic standard deduction

(declare ::basic-standard-deduction [:year :person])

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

(declare ::section-63-c-2-b [:year])
(define ::section-63-c-2-b
  (fn [_ _]
    4400))

(declare ::section-63-c-2-c [:year])
(define ::section-63-c-2-c
  (fn [_ _]
    3000))

(define ::basic-standard-deduction
  (fn [? _]
    (cond
      (or (= (? ::filing-status) :married-filing-jointly)
          (? ::surviving-spouse))
      (* 2 (? ::section-63-c-2-c))

      (= (? ::filing-status) :head-of-household)
      (? ::section-63-c-2-b)

      :else
      (? ::section-63-c-2-c))))

;;     (3) Additional standard deduction for aged and blind

(declare ::additional-standard-deduction [:year :person])

;;     For purposes of paragraph (1), the additional standard deduction is the sum of each additional amount to which the taxpayer is entitled under subsection (f).

(define ::additional-standard-deduction
  (fn [? _]
    (? ::additional-aged)))

;;     (4) Adjustments for inflation
;;
;;         In the case of any taxable year beginning in a calendar year after 1988, each dollar amount contained in paragraph (2)(B), (2)(C), or (5) or subsection (f) shall be increased by an amount equal to—
;;
;;         ...
;;
;;     (7) Special rules for taxable years 2018 through 2025
;;
;;     In the case of a taxable year beginning after December 31, 2017, and before January 1, 2026-

(declare ::section-63-c-7 [:year])
(define ::section-63-c-7
  (fn [_ {:keys [year]}]
    (<= 2018 year 2025)))

;;     (A) Increase in standard deduction
;;
;;         Paragraph (2) shall be applied—
;;
;;         (i) by substituting “$18,000” for “$4,400” in subparagraph (B), and
;;
;;         (ii) by substituting “$12,000” for “$3,000” in subparagraph (C).

(defcase ::section-63-c-2-b
  (fn [? _]
    (? ::section-63-c-7))
  (fn [_ _ _]
    18000))

(defcase ::section-63-c-2-c
  (fn [? _]
    (? ::section-63-c-7))
  (fn [_ _ _]
    12000))

;;     (B) Adjustment for inflation
;;
;;         (i) In general
;;
;;         Paragraph (4) shall not apply to the dollar amounts contained in paragraphs (2)(B) and (2)(C).
;;
;;         (ii) Adjustment of increased amounts
;;
;;             In the case of a taxable year beginning after 2018, the $18,000 and $12,000 amounts in subparagraph (A) shall each be increased by an amount equal to—
;;
;;             (I) such dollar amount, multiplied by
;;
;;             (II) the cost-of-living adjustment determined under section 1(f)(3) for the calendar year in which the taxable year begins, determined by substituting “2017” for “2016” in subparagraph (A)(ii) thereof.
;;
;;             If any increase under this clause is not a multiple of $50, such increase shall be rounded to the next lowest multiple of $50.
;;
;; ...
;;
;; (f) Aged or blind additional amounts
;;
;;   (1) Additional amounts for the aged

(declare ::additional-aged [:year :person])

;;     The taxpayer shall be entitled to an additional amount of $600—
;;
;;     (A) for himself if he has attained age 65 before the close of his taxable year, and
;;
;;     (B) for the spouse of the taxpayer if the spouse has attained age 65 before the close of the taxable year and an additional exemption is allowable to the taxpayer for such spouse under section 151(b).
;;
;;   ...

(declare ::section-63-f-1-a [:year :person])
(define ::section-63-f-1-a
  (fn [? _]
    (>= (? ::age) 65)))

(declare ::section-63-f-1-b [:year :person])
(define ::section-63-f-1-b
  (fn [? _]
    (and (? ::married)
         (>= (? ::age {:person (? ::spouse)}) 65)
         (? ::exemption-for-spouse))))

(define ::additional-aged
  (fn [? _]
    (for-each 600 [(? ::section-63-f-1-a) (? ::section-63-f-1-b)])))

;; §1. Tax imposed
;;
;; ...
;;
;; (f) Adjustments in tax tables so that inflation will not result in tax increases
;;
;;     ...
;;
;;     (3) Cost-of-living adjustment

(declare ::cost-of-living-adjustment [:year])

;;         For purposes of this subsection—
;;
;;         (A) In general
;;
;;             The cost-of-living adjustment for any calendar year is the percentage (if any) by which—
;;
;;             (i) the C-CPI-U for the preceding calendar year, exceeds
;;
;;             (ii) the CPI for calendar year 2016, multiplied by the amount determined under subparagraph (B).

(define ::cost-of-living-adjustment
  (fn [? {:keys [:year]}]
    (/ (? ::c-cpi-u {:year (dec year)})
       (* (? ::cpi {:year 2016}) (? ::section-1-f-3-b)))))

;;         (B) Amount determined
;;
;;             The amount determined under this clause is the amount obtained by dividing—
;;
;;             (i) the C-CPI-U for calendar year 2016, by
;;
;;             (ii) the CPI for calendar year 2016.

(declare ::section-1-f-3-b [])
(define ::section-1-f-3-b
  (fn [? _]
    (/ (? ::c-cpi-u {:year 2016})
       (? ::cpi {:year 2016}))))

;;         (C) Special rule for adjustments with a base year after 2016
;;
;;         For purposes of any provision of this title which provides for the substitution of a year after 2016 for “2016” in subparagraph (A)(ii), subparagraph (A) shall be applied by substituting “the C-CPI-U for calendar year 2016” for “the CPI for calendar year 2016” and all that follows in clause (ii) thereof.
;;
;;     (4) CPI for any calendar year
;;
;;     For purposes of paragraph (3), the CPI for any calendar year is the average of the Consumer Price Index as of the close of the 12-month period ending on August 31 of such calendar year.

(declare ::cpi [:year])
(define ::cpi
  (fn [? _]
    (? ::c-cpi-u)))

;;     (6) C-CPI-U
;;
;;         ...
;;
;;         (B) Determination for calendar year
;;
;;         The C-CPI-U for any calendar year is the average of the C-CPI-U as of the close of the 12-month period ending on August 31 of such calendar year.
;;
;;         ...

(declare ::c-cpi-u [:year])
(define ::c-cpi-u
  (fn [_ {:keys [year]}]
    (/ (case year
         2017 1382
         2018 1410
         2019 1432
         2020 1448
         2021 1491
         2022 1599
         2023 1683
         2024 1725)
       10)))

;; §151. Allowance of deductions for personal exemptions
;;
;;     (a) Allowance of deductions
;;
;;     In the case of an individual, the exemptions provided by this section shall be allowed as deductions in computing taxable income.
;;
;;     (b) Taxpayer and spouse
;;
;;     An exemption of the exemption amount for the taxpayer; and an additional exemption of the exemption amount for the spouse of the taxpayer if a joint return is not made by the taxpayer and his spouse, and if the spouse, for the calendar year in which the taxable year of the taxpayer begins, has no gross income and is not the dependent of another taxpayer.
;;
;;     ...

(declare ::exemption-for-spouse [:year :person])
(define ::exemption-for-spouse
  (fn [? _]
    (let [spouse (? ::spouse)]
      (and (not= (? ::filing-status) :married-filing-jointly)
           (zero? (? ::gross-income {:person spouse}))
           (not (? ::dependent {:person spouse}))))))
