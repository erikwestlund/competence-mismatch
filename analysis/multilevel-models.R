
# Sample: All 10th graders
# Y:      Submitted at least 1 application to a postsecondary institution
# X:      MV difference, Composite, controls
# Group:  High School

### Model 1a
## ANOVA with Random Effects
# Estimate random intercept
model_1a <-
  glmer(
    ever_applied_to_college ~
      (1 | sch_id),
    data = students,
    family = binomial(link = "logit")
  )

summary(model_1a)

### Models 1a-c
## ANCOVA with Random Effects
#
# Student-level measures where intercepts vary by high school
#
# Intercept interpretation: log odds of college application for a hypothetical student who is:
#   * male
#   * white
#   * born in us to us parents
#   * parents never attended college
#   * lives in suburban area
#   Who matched the sample mean on all student-level continuous covariates, i.e.:
#   * math-reading difference
#   * math-reading composite
#   * family income
#
# Each of the above apply only to models where those coefficients are estimated.

### Model 1b
# Math-reading difference and composite

model_1b <-
  glmer(
    ever_applied_to_college ~
      standardized_test_score_math_reading_difference_centered + 
      standardized_test_score_composite_centered +
      (1 | sch_id),
    data = students,
    family = binomial(link = "logit"),
    
  )

summary(model_1b)

# Add in demographics & family background information
model_1c <-
  glmer(
    ever_applied_to_college ~
      standardized_test_score_math_reading_difference_centered + 
      standardized_test_score_composite_centered +
      female +
      race +
      family_income_log_centered + 
      parents_highest_level_of_education + 
      generational_status +
      urbanicity +
      (1 | sch_id),
    data = students,
    family = binomial(link = "logit"),
    
  )

summary(model_1c)



tt <- getME(model_1b, "theta")
ll <- getME(model_1b, "lower")

min(tt[ll==0])
derivs1 <- model_1b@optinfo$derivs
grad1 <- with(derivs1, solve(Hessian, gradient))

max(abs(grad1))
max(pmin(abs(grad1), abs(derivs1$gradient)))

ss <- getME(model_1b,c("theta", "fixef"))
model_1b_2 <- update(model_1b, start=ss, control=glmerControl(optCtrl=list(maxfun=2e4)))
model_1b_3 <- update(model_1b,
                     start=ss,
                     control=glmerControl(
                       optimizer="bobyqa",
                       optCtrl=list(maxfun=2e5)
                     )
)

model_1 <-
  glmer(
    ever_applied_to_college ~
      standardized_test_score_math_reading_difference_centered +
      standardized_test_score_composite_centered +
      ses +
      female +
      race +
      parents_expect_college_for_children +
      urbanicity,
    data = students,
    family = binomial(link = "logit")
  )
