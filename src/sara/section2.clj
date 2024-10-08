(ns sara.section2
  (:refer-clojure :exclude [declare])
  (:require [sara.common :as common]
            [util :refer [condition define]]))

;; §2. Definitions and special rules
;;
;; (a) Definition of surviving spouse
;;
;;     (1) In general
;;
;;     For purposes of section 1, the term "surviving spouse" means a taxpayer-
;;
;;         (A) whose spouse died during either of the two years immediately preceding the taxable year, and
;;
;;         (B) who maintains as his home a household which constitutes for the taxable year the principal place of abode (as a member of such household) of a dependent (i) who (within the meaning of section 152) is a son, stepson, daughter, or stepdaughter of the taxpayer, and (ii) with respect to whom the taxpayer is entitled to a deduction for the taxable year under section 151.
;;
;;     For purposes of this paragraph, an individual shall be considered as maintaining a household only if over half of the cost of maintaining the household during the taxable year is furnished by such individual.

(condition ::surviving-spouse [:year :person])
(define ::surviving-spouse
  (fn [? {:keys [year]}]
    (and (contains? #{(- year 1) (- year 2)}
                    (? ::common/death-year {:person (? ::common/spouse)}))
         :todo)))

;;     (2) Limitations
;;
;;     Notwithstanding paragraph (1), for purposes of section 1 a taxpayer shall not be considered to be a surviving spouse-
;;
;;         (A) if the taxpayer has remarried at any time before the close of the taxable year, or
;;
;;         (B) unless, for the taxpayer's taxable year during which his spouse died, a joint return could have been made. A husband and wife may make a single return jointly of income taxes, even though one of the spouses has neither gross income nor deductions, except that no joint return shall be made if either the husband or wife at any time during the taxable year is a nonresident alien.

;; (defcase ::surviving-spouse
;;   (fn [_ _]
;;     :todo)
;;   (fn [_ _ _]
;;     :todo))

;; (b) Definition of head of household
;;
;;     (1) In general
;;
;;     An individual shall be considered a head of a household if, and only if, such individual is not married at the close of his taxable year, is not a surviving spouse (as defined in subsection (a)), and either-
;;
;;         (A) maintains as his home a household which constitutes for more than one-half of such taxable year the principal place of abode, as a member of such household, of-
;;
;;             (i) a qualifying child of the individual (as defined in section 152(c)), but not if such child-
;;
;;                 (I) is married at the close of the taxpayer's taxable year, and
;;
;;                 (II) is not a dependent of such individual by reason of section 152(b)(2) or
;;
;;             (ii) any other person who is a dependent of the taxpayer, if the taxpayer is entitled to a deduction for the taxable year for such person under section 151, or
;;
;;         (B) maintains a household which constitutes for such taxable year the principal place of abode of the father or mother of the taxpayer, if the taxpayer is entitled to a deduction for the taxable year for such father or mother under section 151.
;;
;;     For purposes of this paragraph, an individual shall be considered as maintaining a household only if over half of the cost of maintaining the household during the taxable year is furnished by such individual.
;;
;;     (2) Determination of status
;;
;;     Notwithstanding paragraph (1),
;;
;;         (A) an individual who is legally separated from his spouse under a decree of divorce or of separate maintenance shall not be considered as married;
;;
;;         (B) a taxpayer shall be considered as not married at the close of his taxable year if at any time during the taxable year his spouse is a nonresident alien; and
;;
;;         (C) a taxpayer shall be considered as married at the close of his taxable year if his spouse (other than a spouse described in subparagraph (B)) died during the taxable year.
;;
;;     (3) Limitations
;;
;;     Notwithstanding paragraph (1), for purposes of this subtitle a taxpayer shall not be considered to be a head of a household-
;;
;;         (A) if at any time during the taxable year he is a nonresident alien; or
;;
;;         (B) by reason of an individual who would not be a dependent for the taxable year but for subparagraph (H) of section 152(d)(2).
