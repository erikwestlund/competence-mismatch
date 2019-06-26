cd "C:\Users\Erik\Desktop\competence-mismatch"
use "data\students_mice_mlm.dta", clear

mi import flong, m(mi_m) id(mi_id)

log using mice-multilevel-model-results.smcl, replace

* Intercept interpretation: log odds of college application for a hypothetical student who is:
*   * male
*   * white
*   * born in us to us parents
*   * parents never attended college
*   * lives in suburban area
*   Who matched the sample mean on all student-level continuous covariates, i.e.:
*   * math-reading difference
*   * math-reading composite
*   * family income
*
* Each of the above apply only to models where those coefficients are estimated.

********************************************************************************
* MODEL 1
********************************************************************************
* Sample: All 10th graders
* Y:      Submitted at least 1 application to a postsecondary institution
* X:      MV difference, Composite, controls
* Group:  High School
********************************************************************************

*** ANOVA with Random Effects
** Model 1a
* Estimate random intercept
mi estimate, or esampvaryok: xtlogit ever_applied_to_college, i(sch_id) re

*** ANCOVA with Random Effects
** Models 1b-c
* Student-level measures where intercepts vary by high school

** Model 1b
* Math-reading difference and composite
mi estimate, or esampvaryok: xtlogit ever_applied_to_college test_mr_diff_c test_composite_c, i(sch_id) re

* Model 1c
* Math-reading difference and composite plus demographic and family background controls
mi estimate, or esampvaryok: xtlogit ever_applied_to_college c.test_mr_diff_c test_composite_c b2.female b1.race c.family_income_log_centered i.parents_ed b2.parents_expect_college b1.generational_status b1.urbanicity, i(sch_id) re or

*** Intercepts-as-outcomes
** Model 1d
* Model 1c
* Math-reading difference and composite plus demographic and family background controls plus high school controls
mi estimate, or esampvaryok: xtlogit ever_applied_to_college c.test_mr_diff_c test_composite_c b2.female b1.race c.family_income_log_centered i.parents_ed b2.parents_expect_college b1.generational_status b1.urbanicity c.hs_10g_frl_c c.hs_perc_ap_c c.hs_attend_4yr_int_c c.hs_10g_remedial_math_c, i(sch_id) re or



********************************************************************************
* MODEL 2
********************************************************************************
* Sample: All 10th graders who submitted at least one application to any school
* Y:      Submitted at least 1 application to a selective college (Defined as Most or Highly Competitive in Barron's Guide)
* X:      MV difference, Composite, Controls (Student + High School)
* Group:  High School
********************************************************************************

*** ANOVA with Random Effects
** Model 1a
* Estimate random intercept
mi estimate, or esampvaryok: xtlogit applied_selective_college if ever_applied_to_college==1, i(sch_id) re

*** ANCOVA with Random Effects
** Models 1b-c
* Student-level measures where intercepts vary by high school

** Model 1b
* Math-reading difference and composite
mi estimate, or esampvaryok: xtlogit applied_selective_college test_mr_diff_c test_composite_c if ever_applied_to_college==1, i(sch_id) re

* Model 1c
* Math-reading difference and composite plus demographic and family background controls
mi estimate, or esampvaryok: xtlogit applied_selective_college c.test_mr_diff_c test_composite_c b2.female b1.race c.family_income_log_centered i.parents_ed b2.parents_expect_college b1.generational_status b1.urbanicity if ever_applied_to_college==1, i(sch_id) re or

*** Intercepts-as-outcomes
** Model 1d
* Model 1c
* Math-reading difference and composite plus demographic and family background controls plus high school controls
mi estimate, or esampvaryok: xtlogit applied_selective_college c.test_mr_diff_c test_composite_c b2.female b1.race c.family_income_log_centered i.parents_ed b2.parents_expect_college b1.generational_status b1.urbanicity hs_10g_frl_c hs_perc_ap_c hs_attend_4yr_int_c hs_10g_remedial_math_c if ever_applied_to_college==1, i(sch_id) re or


********************************************************************************
* MODEL 3
********************************************************************************
* Sample: All 10th graders who submitted at least one application to any school
* Y:      Accepted to at least 1 application to a selective college (Defined as Most or Highly Competitive in Barron's Guide)
* X:      MV difference, Composite, Controls (Student + High School)
* Group:  High School
********************************************************************************

*** ANOVA with Random Effects
** Model 1a
* Estimate random intercept
mi estimate, or esampvaryok: xtlogit accepted_selective_college if ever_applied_to_college==1, i(sch_id) re

*** ANCOVA with Random Effects
** Models 1b-c
* Student-level measures where intercepts vary by high school

** Model 1b
* Math-reading difference and composite
mi estimate, or esampvaryok: xtlogit accepted_selective_college test_mr_diff_c test_composite_c if ever_applied_to_college==1, i(sch_id) re

* Model 1c
* Math-reading difference and composite plus demographic and family background controls
mi estimate, or esampvaryok: xtlogit accepted_selective_college c.test_mr_diff_c test_composite_c b2.female b1.race c.family_income_log_centered i.parents_ed b2.parents_expect_college b1.generational_status b1.urbanicity if ever_applied_to_college==1, i(sch_id) re or

*** Intercepts-as-outcomes
** Model 1d
* Model 1c
* Math-reading difference and composite plus demographic and family background controls plus high school controls
mi estimate, or esampvaryok: xtlogit accepted_selective_college c.test_mr_diff_c test_composite_c b2.female b1.race c.family_income_log_centered i.parents_ed b2.parents_expect_college b1.generational_status b1.urbanicity hs_10g_frl_c hs_perc_ap_c hs_attend_4yr_int_c hs_10g_remedial_math_c if ever_applied_to_college==1, i(sch_id) re or


********************************************************************************
* MODEL 4
********************************************************************************
* Sample: All 10th graders
* Y:      Earned a BA within 6 years
* X:      MV difference, Composite, Controls (Student + High School)
* Group:  High School
********************************************************************************

*** ANOVA with Random Effects
** Model 1a
* Estimate random intercept
mi estimate, or esampvaryok: xtlogit earned_ba_within_six_years if ever_applied_to_college==1, i(sch_id) re

*** ANCOVA with Random Effects
** Models 1b-c
* Student-level measures where intercepts vary by high school

** Model 1b
* Math-reading difference and composite
mi estimate, or esampvaryok: xtlogit earned_ba_within_six_years test_mr_diff_c test_composite_c, i(sch_id) re

* Model 1c
* Math-reading difference and composite plus demographic and family background controls
mi estimate, or esampvaryok: xtlogit earned_ba_within_six_years c.test_mr_diff_c test_composite_c b2.female b1.race c.family_income_log_centered i.parents_ed b2.parents_expect_college b1.generational_status b1.urbanicity, i(sch_id) re or

*** Intercepts-as-outcomes
** Model 1d
* Model 1c
* Math-reading difference and composite plus demographic and family background controls plus high school controls
mi estimate, or esampvaryok: xtlogit earned_ba_within_six_years c.test_mr_diff_c test_composite_c b2.female b1.race c.family_income_log_centered i.parents_ed b2.parents_expect_college b1.generational_status b1.urbanicity hs_10g_frl_c hs_perc_ap_c hs_attend_4yr_int_c hs_10g_remedial_math_c, i(sch_id) re or


* Predicted probabilities
mi estimate, or esampvaryok saving(miest, replace): xtlogit earned_ba_within_six_years c.test_mr_diff_c test_composite_c b2.female b1.race c.family_income_log_centered i.parents_ed b2.parents_expect_college b1.generational_status b1.urbanicity hs_10g_frl_c hs_perc_ap_c hs_attend_4yr_int_c hs_10g_remedial_math_c, i(sch_id) re or

mi predictnl prob_degree = predict(pr) using miest
sum prob_degree if m90_r90==1 
sum prob_degree if m90_r60==1
sum prob_degree if m75_r75==1
sum prob_degree if m60_r90==1
sum prob_degree if m50_r50==1
sum prob_degree if m50_r20==1
sum prob_degree if m35_r35==1
sum prob_degree if m20_r50==1

log close
