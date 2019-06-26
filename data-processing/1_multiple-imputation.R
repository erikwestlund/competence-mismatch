###############################################################################
# Impute missing  data
###############################################################################
#
# This is a complex survey.  As such, imputation requires judgment in places.
# The biggest problem is dealing with legitimate skips.
# For example, it doesn't make sense to ask a student if they applied to a very selective college
# if they did not apply to any college at all.
#
# Simply imputing all data will ignore these aspects of the data and impute unreasonable values in many cases.
# There are no straightforward ways around this.
#
#
# This paper discusses these matters:  http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2891890/
#
# I'm following their advice.
#
# For example, they have data on surgery and cancer. Consider these 5 questions:
# (i) have you had surgery for your cancer?
# (ii) how many surgeries have you had for your cancer?
# (iii) how involved were you in the decision about surgery?
# (iv) have you had any problems with your self-care?
# (v) how satisfied are you with your communication with your doctor?
#
# Questions i and ii were skipped for patients w/o surgery, but these can reasonably be imputed to zero.
# Question (iii) does not make sense if you haven't had surgery yet.
# They argue that imputing 1 and 2 to positive values (by not forcing them to 0) can bias imputation
# results for (iv) because high-surgery patients might have probelms with self-care.  So they keep them 0.
# On the other, they suggest forcing (iii) to 0 (or a single value), they would not accurately assess the relationships
# between (iii) and (v), which are related. So even if iii makes no sense for those without surgery, they still impute it.
#
# After the imputation procedure, they manually put "skips" back in where necessary.
#
# They acknowledge this process is not perfect, but they find that it performs fairly well in simulations.
#
# For me, this involves coding "0," for example to "applied to a very selective college" for students who did not apply to any college at all.
# These fixes are all done after imputation runs.
#
# With respect to choosing prediction covariates, They suggest using variables with R^2=0.01 (or R=0.10) as predictors. This removes a lot of junk. (See section 4.3)
# This is easily done using the mice package.

## Missing data patterns
md_check <- students %>%
  left_join(
    select(
      high_schools,
      sch_id,
      school_percent_10th_grade_free_reduced_lunch,
      school_percent_student_body_in_ap,
      school_percent_attend_four_year_college_2003_integer,
      school_percent_10th_grade_remedial_math
    ),
    by="sch_id"
  ) %>% 
  select(
  ever_apply = ever_applied_to_college,
  appl_sel = applied_to_barrons_very_competitive_plus,
  acc_sel = accepted_to_barrons_very_competitive_plus,
  earn_ba = earned_ba_within_six_years,
  math = standardized_test_score_math,
  reading = standardized_test_score_reading,
  income = family_income_1000,
  female,
  race,
  par_exp_coll = parents_expect_college_for_children,
  par_high_ed = parents_highest_level_of_education,
  hs_frl = school_percent_10th_grade_free_reduced_lunch,
  hs_perc_ap = school_percent_student_body_in_ap,
  hs_atnd_4yr = school_percent_attend_four_year_college_2003_integer
  )

md.pattern(md_check)

png(
  file="graphics/missing-data.png",
  width=2400,
  height=2400
    )
aggr(md_check, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(md_check), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
dev.off()

# Level 1 measures
imp_complete_no_predict_vars_l1 <- c(
  "stu_id",
  "strat_id",
  "psu",
  "f1sch_id",
  "f1univ1",
  "f1univ2a",
  "f1univ2b",
  "f2univ1",
  "f2univ_p",
  "f3univ",
  "f3univg10",
  "f3univg12",
  "g10cohrt",
  "g12cohrt",
  "bystuwt",
  "byexpwt",
  "family_income_log"
)

imp_complete_no_predict_vars_l1_method <-  c(
  "",
  "",
  "", 
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  ""
)


imp_complete_predict_vars_l1 <- c(
  "family_income_1000"
)

imp_complete_predict_vars_l1_method <- c(
  "2l.contextual.pmm"
)

imp_incomplete_vars_l1 <- c(
  "ever_applied_to_college",
  "applied_to_highly_selective_college_carnegie",
  "applied_to_barrons_very_competitive_plus",
  "accepted_to_barrons_very_competitive_plus",
  "attempted_ba",
  "transferred_colleges",
  "earned_ba_within_six_years",
  "female",
  "parents_expect_college_for_children",
  "grades_very_important",
  "race",
  "generational_status",
  "parents_highest_level_of_education",
  "ses",
  "hs_gpa",
  "sat_score_math_highest_vs_act_converted",
  "sat_score_verbal_highest_vs_act_converted",
  "standardized_test_score_reading",
  "standardized_test_score_math",
  "self_efficacy_math",
  "self_efficacy_english",
  "ec_hours_integer"
)

imp_incomplete_vars_l1_method <- c(
  "logreg",
  "logreg",  
  "logreg",
  "logreg",
  "logreg",
  "logreg",
  "logreg",
  "logreg",
  "logreg",  
  "logreg",
  "polyreg",
  "polyreg",
  "polr",
  "2l.contextual.pmm",
  "2l.contextual.pmm",
  "2l.contextual.pmm",
  "2l.contextual.pmm",
  "2l.contextual.pmm",
  "2l.contextual.pmm",
  "2l.contextual.pmm",
  "2l.contextual.pmm",
  "2l.contextual.pmm"
)


## level 2 measure (i.e., school level)
imp_complete_no_predict_vars_l2 <- c(
  "sch_id",
  "urbanicity",
  "geographic_region"
)

imp_complete_no_predict_vars_l2_method <- c(
  "", 
  "", 
  ""
  )

imp_complete_predict_vars_l2 <- c("school_control")
imp_complete_predict_vars_l2_method <- c("polyreg")

imp_incomplete_vars_l2 <- c(
  "school_percent_10th_grade_free_reduced_lunch",
  "school_percent_student_body_in_ap",
  "school_percent_10th_grade_remedial_math",
  "school_percent_10th_grade_college_prep_program",
  "school_percent_attend_four_year_college_2003_integer",
  "school_percent_attend_two_year_college_2003_integer",
  "school_percent_attend_college_app_program_integer",
  "school_percent_attend_financial_aid_program_integer",
  "school_percent_attend_sat_course_integer",
  "school_percent_attend_college_fair_integer"
)

imp_incomplete_vars_l2_method <- c(
  "2lonly.pmm",
  "2lonly.pmm",
  "2lonly.pmm",
  "2lonly.pmm",
  "2lonly.pmm",
  "2lonly.pmm",
  "2lonly.pmm",
  "2lonly.pmm",
  "2lonly.pmm",
  "2lonly.pmm"
)

imp_vars <- c(
  imp_complete_no_predict_vars_l1,
  imp_complete_predict_vars_l1,
  imp_incomplete_vars_l1,
  imp_complete_no_predict_vars_l2,
  imp_complete_predict_vars_l2,
  imp_incomplete_vars_l2
)

imp_vars_method <- c(
  imp_complete_no_predict_vars_l1_method,
  imp_complete_predict_vars_l1_method,
  imp_incomplete_vars_l1_method,
  imp_complete_no_predict_vars_l2_method,
  imp_complete_predict_vars_l2_method,
  imp_incomplete_vars_l2_method
)


students_imp <- students %>%
  left_join(
    select(
      high_schools,
      sch_id,
      geographic_region,
      urbanicity,
      school_control,
      school_percent_10th_grade_free_reduced_lunch,
      school_percent_student_body_in_ap,
      school_percent_10th_grade_remedial_math,
      school_percent_10th_grade_college_prep_program,
      school_percent_attend_four_year_college_2003_integer,
      school_percent_attend_two_year_college_2003_integer,
      school_percent_attend_college_app_program_integer,
      school_percent_attend_financial_aid_program_integer,
      school_percent_attend_sat_course_integer,
      school_percent_attend_college_fair_integer
    ),
    by = "sch_id",
  ) %>%
  select(imp_vars)

# add the composites as missing, as these will be logically imputed
students_imp.ext <- cbind(students_imp)
imp_vars_method.ext <- c(imp_vars_method)


# all vars
students_imp.ext_vars <- colnames(students_imp.ext)

ncol(students_imp.ext)
length(imp_vars_method.ext)

# dry run to get some extractable info
ini <- mice(students_imp.ext, m = 2, method = imp_vars_method.ext, maxit = 0, seed = 092084)

meth <- ini$meth

# group must be -2 for all 2level imputations
level_1_2l_imp_vars <- c(
  "ses",
  "hs_gpa",
  "sat_score_math_highest_vs_act_converted",
  "sat_score_verbal_highest_vs_act_converted",
  "standardized_test_score_reading",
  "standardized_test_score_math",
  "self_efficacy_math",
  "self_efficacy_english",
  "ec_hours_integer"  
)
level_2_var <- c(
  "school_percent_10th_grade_free_reduced_lunch",
  "school_percent_student_body_in_ap",
  "school_percent_10th_grade_remedial_math",
  "school_percent_10th_grade_college_prep_program",
  "school_percent_attend_four_year_college_2003_integer",
  "school_percent_attend_two_year_college_2003_integer",
  "school_percent_attend_college_app_program_integer",
  "school_percent_attend_financial_aid_program_integer",
  "school_percent_attend_sat_course_integer",
  "school_percent_attend_college_fair_integer"
)

# Quick prediction matrix.
# this returns a matrix that uses variables with a correlation of at least 0.1 to predict.  this allows us not to overdo it
# see: http://www.stefvanbuuren.nl/mi/docs/Utrecht-15MayCourse%20handout.pdf

# additional variables that should not be used as predictors
# all the variables that aren't related to education (in complete_no_predict vectors)
# degree earning status, as well as all follow-up 12 measures related to degree earning
vars_no_predict <- c(
  imp_complete_no_predict_vars_l1,
  imp_complete_no_predict_vars_l2
)

# Variables to always include in prediction:
# Dependent variables
# SES measures: income, education. Race/ethnicity measures: race/genstatus
# We know these are important all the way through the college-going process
# vars that will be in our full model, as well
vars_always_predict <- c(
  "ever_applied_to_college",
  "applied_to_highly_selective_college_carnegie",
  "applied_to_barrons_very_competitive_plus",
  "accepted_to_barrons_very_competitive_plus",
  "transferred_colleges",  
  "earned_ba_within_six_years",
  "family_income_1000",
  "ses",
  "race",
  "generational_status",
  "female",
  "parents_highest_level_of_education",
  "sat_score_math_highest_vs_act_converted",
  "sat_score_verbal_highest_vs_act_converted",
  "standardized_test_score_reading",
  "standardized_test_score_math"
)

# 0.10 corr = R^2 of 0.01 -- recommended in above paper on MICE in common survey
pred <- quickpred(students_imp.ext, exclude=vars_no_predict, include=vars_always_predict, mincor=0.10, method='pearson')

# With continuous variables, we want to account for the clustering of data
# Use a custom function in the miceadds pacakage called 2l.contextual.pmm
# This uses linear regression with predictive mean matching, but creates cluster effects using the
# level two variables within each cluster.
# Author of mice suggested in above handout that pmm + context effects performs well
pred[c(level_1_2l_imp_vars, level_2_var), 'sch_id'] <- -2

# Include level 2 contextual effects for vars with 2l.contextual.pmm
pred[level_1_2l_imp_vars, level_2_var] <- 2

imp <- mice(students_imp.ext, m=10, method=meth, pred=pred, seed=092084, maxit=3)

# Restore "skips" where necessary

# Save and transform
saveRDS(imp, 'data/student_mice_mids.rds')
data <- complete(imp, "long", inc=T)
saveRDS(data, 'data/student_mice_stacked.rds')

# 
# # Good
# plot(imp, c('sat_score_math_highest_vs_act_converted', 'sat_score_verbal_highest_vs_act_converted'))
# 
# # Might need some more imputation for math
# plot(imp, c('standardized_test_score_math', 'standardized_test_score_reading', 'hs_gpa'))
# 
# # Math could use some work again
# plot(imp, c('self_efficacy_math', 'self_efficacy_english', 'ec_hours_integer'))
