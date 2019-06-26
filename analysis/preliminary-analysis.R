source("bootstrap.R")

##############################################################################
# Earned BA Model
##############################################################################

# Standardized Test Scores from ELS02
earned_ba_standardized_test_score_model <- 
  glm(earned_ba ~ standardized_test_score_math_reading_difference + standardized_test_score_composite+ ses + female + race + parents_expect_college_for_children + urbanicity,
  data = students,
  family = binomial(link = "logit")
)

# Standardized Test Scores from ELS02 -- math and verbal scores instead of difference + composite
earned_ba_standardized_test_score_model_math_verbal <- 
  glm(earned_ba ~ standardized_test_score_math + standardized_test_score_reading + ses + female + race + parents_expect_college_for_children + urbanicity,
      data = students,
      family = binomial(link = "logit")
  )

summary(earned_ba_standardized_test_score_model_math_verbal)
exp(cbind(coef(earned_ba_standardized_test_score_model_math_verbal)))

# SAT Scores
earned_ba_sat_score_model <- 
  glm(earned_ba ~ sat_score_math_verbal_difference + sat_score_composite_highest_vs_act_converted + ses + female + race + parents_expect_college_for_children + urbanicity,
      data = students,
      family = binomial(link = "logit")
  )

summary(earned_ba_sat_score_model)
exp(cbind(coef(earned_ba_sat_score_model)))


##############################################################################
# Transferred Model
##############################################################################

# Standardized Test Scores from ELS02
transferred_colleges_standardized_test_score_model <- 
  glm(transferred_colleges ~ standardized_test_score_math_reading_difference + standardized_test_score_composite+ ses + female + race + parents_expect_college_for_children + urbanicity,
  data = students,
  family = binomial(link = "logit")
)

summary(transferred_colleges_standardized_test_score_model)
exp(cbind(coef(transferred_colleges_standardized_test_score_model)))

# SAT Scores
transferred_colleges_sat_score_model <- 
  glm(transferred_colleges ~ sat_score_math_verbal_difference + sat_score_composite_highest_vs_act_converted + ses + female + race + parents_expect_college_for_children + urbanicity,
      data = students,
      family = binomial(link = "logit")
  )

summary(transferred_colleges_sat_score_model)
exp(cbind(coef(transferred_colleges_sat_score_model)))


##############################################################################
# Applied to Highly Selective College Model
##############################################################################

# Standardized Test Scores from ELS02
applied_to_highly_selective_college_standardized_test_score_model <- 
  glm(applied_to_highly_selective_college_carnegie ~ standardized_test_score_math_reading_difference + standardized_test_score_composite+ ses + female + race + parents_expect_college_for_children + urbanicity,
  data = students,
  family = binomial(link = "logit")
)

summary(applied_to_highly_selective_college_standardized_test_score_model)
exp(cbind(coef(applied_to_highly_selective_college_standardized_test_score_model)))

# Standardized Test Scores from ELS02 -- math and verbal scores instead of difference + composite
applied_to_highly_selective_college_standardized_test_score_model_math_verbal <- 
  glm(applied_to_highly_selective_college_carnegie ~ standardized_test_score_math + standardized_test_score_reading + ses + female + race + parents_expect_college_for_children + urbanicity,
  data = students,
  family = binomial(link = "logit")
)

summary(applied_to_highly_selective_college_standardized_test_score_model_math_verbal)
exp(cbind(coef(applied_to_highly_selective_college_standardized_test_score_model_math_verbal)))

# SAT Scores
applied_to_highly_selective_college_sat_score_model <- 
  glm(applied_to_highly_selective_college_carnegie ~ sat_score_math_verbal_difference + sat_score_composite_highest_vs_act_converted + ses + female + race + parents_expect_college_for_children + urbanicity,
      data = students,
      family = binomial(link = "logit")
  )

summary(applied_to_highly_selective_college_sat_score_model)
exp(cbind(coef(applied_to_highly_selective_college_sat_score_model)))


##############################################################################
# Applied to Highly Selective College Model -- using Barron's
##############################################################################

# Standardized Test Scores from ELS02
applied_to_highly_selective_college_standardized_test_score_model_barrons <- 
  glm(applied_to_barrons_very_competitive_plus ~ standardized_test_score_math_reading_difference + standardized_test_score_composite+ ses + female + race + female + parents_expect_college_for_children + urbanicity,
      data = students,
      family = binomial(link = "logit")
  )

summary(applied_to_highly_selective_college_standardized_test_score_model_barrons)
exp(cbind(coef(applied_to_highly_selective_college_standardized_test_score_model_barrons)))


##############################################################################
# Earned BA from Barron's College, given attended
##############################################################################

# Standardized Test Scores from ELS02
finished_ba_at_barrons_highly_competitive_college <- 
  glm(earned_ba ~ standardized_test_score_math_reading_difference + standardized_test_score_composite+ ses + female + race + female + parents_expect_college_for_children + urbanicity,
      data = students %>% filter(attended_barrons_very_competitive_plus_first_real_college == 1),
      family = binomial(link = "logit")
  )

summary(finished_ba_at_barrons_highly_competitive_college)
exp(cbind(coef(finished_ba_at_barrons_highly_competitive_college)))

##############################################################################
# Applied to Highly Selective College Model -- using Barron's
##############################################################################

# Standardized Test Scores from ELS02
applied_to_highly_selective_college_standardized_test_score_model_barrons <- 
  glm(applied_to_barrons_very_competitive_plus ~ standardized_test_score_math_reading_difference + standardized_test_score_composite+ ses + female + race + female + parents_expect_college_for_children + urbanicity,
      data = students,
      family = binomial(link = "logit")
  )

summary(applied_to_highly_selective_college_standardized_test_score_model_barrons)
exp(cbind(coef(applied_to_highly_selective_college_standardized_test_score_model_barrons)))


##############################################################################
# Grades Are Very Important Model
##############################################################################

# Standardized Test Scores from ELS02
grades_very_important_standardized_test_score_model <- 
  glm(grades_very_important ~ standardized_test_score_math_reading_difference + standardized_test_score_composite+ ses + female + race + parents_expect_college_for_children + urbanicity,
  data = students,
  family = binomial(link = "logit")
)

summary(grades_very_important_standardized_test_score_model)
exp(cbind(coef(grades_very_important_standardized_test_score_model)))

# SAT Scores
grades_very_important_sat_score_model <- 
  glm(grades_very_important ~ sat_score_math_verbal_difference + sat_score_composite_highest_vs_act_converted + ses + female + race + parents_expect_college_for_children + urbanicity,
      data = students,
      family = binomial(link = "logit")
  )

summary(grades_very_important_sat_score_model)
exp(cbind(coef(grades_very_important_sat_score_model)))

