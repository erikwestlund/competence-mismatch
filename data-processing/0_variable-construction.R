raw_els02_by_f3_pets <- readRDS('data/els02_r/els02_by_f3_pets.rds')

students <- tibble(
  # Study Design Variables
  stu_id = raw_els02_by_f3_pets$STU_ID,
  sch_id = raw_els02_by_f3_pets$SCH_ID,
  strat_id = raw_els02_by_f3_pets$STRAT_ID,
  psu = raw_els02_by_f3_pets$PSU,
  f1sch_id = raw_els02_by_f3_pets$F1SCH_ID,
  f1univ1 = raw_els02_by_f3_pets$F1UNIV1,
  f1univ2a = raw_els02_by_f3_pets$F1UNIV2A,
  f1univ2b = raw_els02_by_f3_pets$F1UNIV2B,
  f2univ1 = raw_els02_by_f3_pets$F2UNIV1,
  f2univ_p = raw_els02_by_f3_pets$F2UNIV_P,
  f3univ = raw_els02_by_f3_pets$F3UNIV,
  f3univg10 = raw_els02_by_f3_pets$F3UNIVG10,
  f3univg12 = raw_els02_by_f3_pets$F3UNIVG12,
  g10cohrt = raw_els02_by_f3_pets$G10COHRT,
  g12cohrt = raw_els02_by_f3_pets$G12COHRT,
  bystuwt = raw_els02_by_f3_pets$BYSTUWT,
  byexpwt = raw_els02_by_f3_pets$BYEXPWT,
)

##############################################################################
# Demographics
##############################################################################
students$female <- raw_els02_by_f3_pets$BYSEX %>%
  recode("Male" = 0, "Female" = 1) %>%
  replace_missing()

students$ses <- raw_els02_by_f3_pets$BYSES1 %>% replace_missing()
students$race <- raw_els02_by_f3_pets$BYRACE %>% replace_missing() %>%
  recode(
    "Amer. Indian/Alaska Native, non-Hispanic" = "native american",
    "Asian, Hawaii/Pac. Islander,non-Hispanic" = "asian",
    "Black or African American, non-Hispanic" = "black",
    "Hispanic, no race specified" = "hispanic",
    "Hispanic, race specified" = "hispanic",
    "More than one race, non-Hispanic" = "multiracial",
    "White, non-Hispanic" = "white, non-hispanic"
  )

students$urbanicity <- raw_els02_by_f3_pets$BYURBAN %>% 
  replace_missing() %>%
  recode(
    "Suburban" = "suburban",
    "Urban" = "urban",
    "Rural" = "rural"
  )

##############################################################################
# Educational Expectations & Outcomes
##############################################################################

students$parents_expect_college_for_children <- raw_els02_by_f3_pets$BYSTEXP %>%
  replace_missing() %>%
  recode(
    "Less than high school graduation" = 0,
    "High school graduation or GED only" = 0,
    "Attend or complete 2-year college/school" = 0,
    "Attend college, 4-year degree incomplete" = 0,
    "Graduate from college" = 1,
    "Obtain Master's degree or equivalent" = 1,
    "Obtain PhD, MD, or other advanced degree" = 1
  )

students$earned_ba <- raw_els02_by_f3_pets$F3ATTAINMENT %>%
  replace_missing() %>%
  recode(
    "No HS credential, no PS attendance" = 0,
    "HS credential, no PS attendance" = 0,
    "Some PS attendance, no PS credential" = 0,
    "Undergraduate certificate" = 0,
    "Associates degree" = 0,
    "Bachelors degree" = 1,
    "Post-Baccalaureate certificate" = 1,
    "Masters degree" = 1,
    "Post-Masters certificate" = 1,
    "Doctoral degree" = 1
  )

students$earned_ba <- raw_els02_by_f3_pets$F3ATTAINMENT %>%
  replace_missing() %>%
  recode(
    "No HS credential, no PS attendance" = 0,
    "HS credential, no PS attendance" = 0,
    "Some PS attendance, no PS credential" = 0,
    "Undergraduate certificate" = 0,
    "Associates degree" = 0,
    "Bachelors degree" = 1,
    "Post-Baccalaureate certificate" = 1,
    "Masters degree" = 1,
    "Post-Masters certificate" = 1,
    "Doctoral degree" = 1
  )


students$grades_very_important <- raw_els02_by_f3_pets$BYS37 %>%
  replace_missing() %>%
  recode(
    "Not important" = 0,
    "Somewhat important" = 0,
    "Important" = 0,
    "Very important" = 1,
  )


students$applied_to_highly_selective_college <- raw_els02_by_f3_pets$F2PSAPSL %>%
  replace_missing() %>%
  recode(
    "Highly selective, 4-yr institution" = 1,
    "Moderately selective, 4-yr institution" = 0,
    "Inclusive, 4-yr institution" = 0,
    "Selectivity not classified, 4-yr inst" = 0,
    "Selectivity not classified, 2-yr inst" = 0,
    "Selectivity not classified,less than 2yr" = 0
  )

students$transferred_colleges <- raw_els02_by_f3_pets$F2SWITCH %>%
  replace_missing() %>%
  recode(
    "Did not transfer or switch" = 0,
    "Transferred or switched" = 1,
  )

##############################################################################
# Test Scores
##############################################################################

# Standardized scores administered to ELS02 sample
students$standardized_test_score_math <- raw_els02_by_f3_pets$BYTXMSTD %>% replace_missing()
students$standardized_test_score_reading <- raw_els02_by_f3_pets$BYTXRSTD %>% replace_missing()
students$standardized_test_score_composite <- students$standardized_test_score_math + students$standardized_test_score_reading

# SAT Scores
students$sat_score_math <- raw_els02_by_f3_pets$TXSATM %>% replace_missing()
students$sat_score_verbal <- raw_els02_by_f3_pets$TXSATV %>% replace_missing()
students$sat_score_composite <- students$sat_score_math + students$sat_score_verbal

# ACT Scores
students$act_score_math <- raw_els02_by_f3_pets$TXACTM %>% replace_missing()
students$act_score_english <- raw_els02_by_f3_pets$TXACTE %>% replace_missing()
students$act_score_reading <- raw_els02_by_f3_pets$TXACTR %>% replace_missing()
students$act_score_composite <- raw_els02_by_f3_pets$TXACTC %>% replace_missing()

# Convert most recent ACT scores to SAT scores
students$act_score_english_plus_reading <- students$act_score_english + students$act_score_reading
students$sat_score_verbal_from_act_conversion <- students$act_score_english_plus_reading %>% convert_act_english_reading_to_sat_verbal()
students$sat_score_math_from_act_conversion <- raw_els02_by_f3_pets$TXACTM %>%
  replace_missing() %>%
  convert_act_math_to_sat_math()


# Highest SAT scores, when considering conversions from the ACT
students$sat_score_math_highest_vs_act_converted <- mapply(
  get_highest_sat_score_with_act_conversions,
  students$sat_score_math,
  students$sat_score_math_from_act_conversion
)

students$sat_score_verbal_highest_vs_act_converted <- mapply(
  get_highest_sat_score_with_act_conversions,
  students$sat_score_verbal,
  students$sat_score_verbal_from_act_conversion
)

students$sat_score_composite_highest_vs_act_converted <- students$sat_score_math_highest_vs_act_converted + students$sat_score_verbal_highest_vs_act_converted


# Calculate percentile ranks of test scores
students$standardized_test_score_math_percent_rank <- percent_rank(students$standardized_test_score_math) * 100
students$standardized_test_score_reading_percent_rank <- percent_rank(students$standardized_test_score_reading) * 100
students$sat_score_math_from_act_conversion_percent_rank <- percent_rank(students$sat_score_math_highest_vs_act_converted) * 100
students$sat_score_verbal_from_act_conversion_percent_rank <- percent_rank(students$sat_score_verbal_highest_vs_act_converted) * 100

# Calculate difference betweeh math and reading/verbal percent ranks
students$standardized_test_score_math_reading_difference <- students$standardized_test_score_math_percent_rank - students$standardized_test_score_reading_percent_rank
students$sat_score_math_verbal_difference <- students$sat_score_math_from_act_conversion_percent_rank - students$sat_score_verbal_from_act_conversion_percent_rank

# Create categorical versions of math-verbal differences
students$standardized_test_score_math_reading_difference_categorical <- cut(students$standardized_test_score_math_reading_difference,
  breaks = c(-Inf, -25, -10, -5, 5, 10, 25, Inf),
  labels = c("reading+ 25", "reading+ 11-24", "reading+ 6-1", "math/reading+/-5", "math+ 6-10", "math+ 11-24", "math+ 25")
)

students$sat_score_math_reading_difference_categorical <- cut(students$sat_score_math_verbal_difference,
  breaks = c(-Inf, -25, -10, -5, 5, 10, 25, Inf),
  labels = c("verbal+ 25", "verbal+ 11-24", "verbal+ 6-1", "math/verbal+/-5", "math+ 6-10", "math+ 11-24", "math+ 25")
)