source("bootstrap.R")

##############################################################################
# Earned BA Model
##############################################################################

# Standardized Test Scores from ELS02
earned_ba_standardized_test_score_model <- 
  glm(earned_ba ~ standardized_test_score_math_reading_difference + standardized_test_score_composite+ ses + race + parents_expect_college_for_children + urbanicity,
  data = students,
  family = binomial(link = "logit")
)

summary(earned_ba_standardized_test_score_model)
exp(cbind(coef(earned_ba_standardized_test_score_model)))

# SAT Scores
earned_ba_sat_score_model <- 
  glm(earned_ba ~ sat_score_math_verbal_difference + sat_score_composite_highest_vs_act_converted + ses + race + parents_expect_college_for_children + urbanicity,
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
  glm(transferred_colleges ~ standardized_test_score_math_reading_difference + standardized_test_score_composite+ ses + race + parents_expect_college_for_children + urbanicity,
  data = students,
  family = binomial(link = "logit")
)

summary(transferred_colleges_standardized_test_score_model)
exp(cbind(coef(transferred_colleges_standardized_test_score_model)))

# SAT Scores
transferred_colleges_sat_score_model <- 
  glm(transferred_colleges ~ sat_score_math_verbal_difference + sat_score_composite_highest_vs_act_converted + ses + race + parents_expect_college_for_children + urbanicity,
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
  glm(applied_to_highly_selective_college ~ standardized_test_score_math_reading_difference + standardized_test_score_composite+ ses + race + parents_expect_college_for_children + urbanicity,
  data = students,
  family = binomial(link = "logit")
)

summary(applied_to_highly_selective_college_standardized_test_score_model)
exp(cbind(coef(applied_to_highly_selective_college_standardized_test_score_model)))

# SAT Scores
applied_to_highly_selective_college_sat_score_model <- 
  glm(applied_to_highly_selective_college ~ sat_score_math_verbal_difference + sat_score_composite_highest_vs_act_converted + ses + race + parents_expect_college_for_children + urbanicity,
      data = students,
      family = binomial(link = "logit")
  )

summary(applied_to_highly_selective_college_sat_score_model)
exp(cbind(coef(applied_to_highly_selective_college_sat_score_model)))


##############################################################################
# Grades Are Very Important Model
##############################################################################

# Standardized Test Scores from ELS02
grades_very_important_standardized_test_score_model <- 
  glm(grades_very_important ~ standardized_test_score_math_reading_difference + standardized_test_score_composite+ ses + race + parents_expect_college_for_children + urbanicity,
  data = students,
  family = binomial(link = "logit")
)

summary(grades_very_important_standardized_test_score_model)
exp(cbind(coef(grades_very_important_standardized_test_score_model)))

# SAT Scores
grades_very_important_sat_score_model <- 
  glm(grades_very_important ~ sat_score_math_verbal_difference + sat_score_composite_highest_vs_act_converted + ses + race + parents_expect_college_for_children + urbanicity,
      data = students,
      family = binomial(link = "logit")
  )

summary(grades_very_important_sat_score_model)
exp(cbind(coef(grades_very_important_sat_score_model)))

