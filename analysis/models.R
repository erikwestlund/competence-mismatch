
## Model 1
# Sample: All 10th graders
# Y:      Submitted at least 1 application to a postsecondary institution
# X:      MV difference, Composite, controls

model_1 <-
  glm(
    ever_applied_to_college ~
    standardized_test_score_math_reading_difference +
      standardized_test_score_composite +
      ses +
      female +
      race +
      parents_expect_college_for_children +
      urbanicity,
    data = students,
    family = binomial(link = "logit")
  )

# model_1_re <-
#   glmer(
#     ever_applied_to_college ~
#       standardized_test_score_math_reading_difference +
#       standardized_test_score_composite +
#       ses +
#       female +
#       race +
#       parents_expect_college_for_children +
#       urbanicity +
#       (1 | sch_id),
#
#     data = students,
#     family = binomial(link = "logit")
#   )
#
#
# summary(model_1_re)

model_1_ix <-
  glm(
    ever_applied_to_college ~
    standardized_test_score_math_reading_difference *
      standardized_test_score_composite +
      ses +
      female +
      race +
      parents_expect_college_for_children +
      urbanicity,
    data = students,
    family = binomial(link = "logit")
  )

summary(model_1_ix)

model_1_ix_lrtest <- lrtest(model_1_ix, model_1)


## Model 2
# Sample: All 10th graders who submitted at least one application to any school
# Y:      Submitted at least 1 application to a selective college (Defined as Most or Highly Competitive in Barron's Guide)
# X:      MV difference, Composite, controls

model_2 <-
  glm(
    applied_to_barrons_very_competitive_plus ~
    standardized_test_score_math_reading_difference +
      standardized_test_score_composite +
      ses +
      female +
      race +
      parents_expect_college_for_children +
      urbanicity,
    data = students %>% filter(ever_applied_to_college == 1),
    family = binomial(link = "logit")
  )

summary(model_2)

model_2_ix <-
  glm(
    applied_to_barrons_very_competitive_plus ~
    standardized_test_score_math_reading_difference *
      standardized_test_score_composite +
      ses +
      female +
      race +
      parents_expect_college_for_children +
      urbanicity,
    data = students %>% filter(ever_applied_to_college == 1),
    family = binomial(link = "logit")
  )

summary(model_2_ix)

model_2_ix_lrtest <- lrtest(model_2_ix, model_2)




## Model 3
# Sample: All 10th graders who submitted at least one selective college
# Y:      Accepted to at least 1 application to a selective college (Defined as Most or Highly Competitive in Barron's Guide)
# X:      MV difference, Composite, controls

model_3 <-
  glm(
    accepted_to_barrons_very_competitive_plus ~
    standardized_test_score_math_reading_difference +
      standardized_test_score_composite +
      ses +
      female +
      race +
      parents_expect_college_for_children +
      urbanicity,
    data = students %>% filter(applied_to_barrons_very_competitive_plus == 1),
    family = binomial(link = "logit")
  )

summary(model_3)


## Model 4
# Sample: All 10th graders who earned a BA within six years.
# Y:      Accepted to at least 1 application to a college
# X:      MV difference, Composite, controls

model_4_data <- students %>%
  filter(ever_applied_to_college == 1) %>%
  select(
    earned_ba_within_six_years,
    standardized_test_score_math_reading_difference,
    standardized_test_score_composite,
    ses,
    female,
    race,
    parents_expect_college_for_children,
    urbanicity,
    standardized_test_score_m90_r90,
    standardized_test_score_m90_r60,
    standardized_test_score_m75_r75,
    standardized_test_score_m60_r90,
    standardized_test_score_m50_r50,
    standardized_test_score_m50_r20,
    standardized_test_score_m35_r35,
    standardized_test_score_m20_r50
  ) %>%
  filter(complete.cases(.))


model_4 <-
  glm(
    earned_ba_within_six_years ~
    standardized_test_score_math_reading_difference +
      standardized_test_score_composite +
      ses +
      female +
      race +
      parents_expect_college_for_children +
      urbanicity,
    data = model_4_data,
    family = binomial(link = "logit")
  )

summary(model_4)


model_4_ix <-
  glm(
    earned_ba_within_six_years ~
    standardized_test_score_math_reading_difference *
      standardized_test_score_composite +
      ses +
      female +
      race +
      parents_expect_college_for_children +
      urbanicity,
    data = model_4_data,
    family = binomial(link = "logit")
  )

summary(model_4_ix)

model_4_ix_lrtest <- lrtest(model_4_ix, model_4)


# Solve at interesting configurations
model_4_logit <- predict(model_4)
model_4_prob <- exp(model_4_logit) / (1 + exp(model_4_logit))

model_4_data$preds <- model_4_prob

prob_matrix <- matrix(nrow = 8, ncol = 1)
colnames(prob_matrix) <- c("p(earn degree six years)")
rownames(prob_matrix) <- c("m90_r90", "m90_r60", "m75_r75", "m60_r90", "m50_r50", "m50_r20", "m35_r35", "m20_r50")

prob_matrix[1] <- model_4_data %>%
  filter(standardized_test_score_m90_r90 == 1) %>%
  select(preds) %>%
  first() %>%
  mean()
prob_matrix[2] <- model_4_data %>%
  filter(standardized_test_score_m90_r60 == 1) %>%
  select(preds) %>%
  first() %>%
  mean()
prob_matrix[3] <- model_4_data %>%
  filter(standardized_test_score_m75_r75 == 1) %>%
  select(preds) %>%
  first() %>%
  mean()
prob_matrix[4] <- model_4_data %>%
  filter(standardized_test_score_m60_r90 == 1) %>%
  select(preds) %>%
  first() %>%
  mean()
prob_matrix[5] <- model_4_data %>%
  filter(standardized_test_score_m50_r50 == 1) %>%
  select(preds) %>%
  first() %>%
  mean()
prob_matrix[6] <- model_4_data %>%
  filter(standardized_test_score_m50_r20 == 1) %>%
  select(preds) %>%
  first() %>%
  mean()
prob_matrix[7] <- model_4_data %>%
  filter(standardized_test_score_m35_r35 == 1) %>%
  select(preds) %>%
  first() %>%
  mean()
prob_matrix[8] <- model_4_data %>%
  filter(standardized_test_score_m20_r50 == 1) %>%
  select(preds) %>%
  first() %>%
  mean()


# Marginal Effects
cplot_dat <- cplot(model_4, x = "standardized_test_score_composite", dx = "standardized_test_score_math_reading_difference", what = "effect", draw = FALSE)

cplot <-
  ggplot(cplot_dat, aes(x = xvals)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray90") +
  geom_line(aes(y = yvals)) +
  xlab("Composite Standardized Test Score") +
  ylab("Average Marginal Effect of Math-Reading Difference") +
  ggtitle("Average Marginal Effect of Math-Reading Difference, Given Composite Test Score") +
  theme_bw()
