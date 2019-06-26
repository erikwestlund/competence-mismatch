mi_orig <- readRDS('data/student_mice_mids.rds')
mi_data <- readRDS('data/student_mice_stacked.rds')

mi_processed <- mi_data


###############################################################################
# Restore logically necessary skips
###############################################################################

# If you never applied to any non-open admissions college, you can't have applied to a selective college.
mi_processed$applied_to_highly_selective_college_carnegie[mi_processed$ever_applied_to_college=='no'] <- 'no'
mi_processed$applied_to_barrons_very_competitive_plus[mi_processed$ever_applied_to_college=='no'] <- 'no'

# If you never applid to a highly selective college, you cannot have been admitted to one.
mi_processed$accepted_to_barrons_very_competitive_plus[mi_processed$applied_to_barrons_very_competitive_plus=='no'] <- 'no'

# If you never attempted a BA, you cannot have earned one.
mi_processed$earned_ba_within_six_years[mi_processed$attempted_ba=='no'] <- 'no'


###############################################################################
# Post-imputation variable preparation
###############################################################################

# Create composites
mi_processed$standardized_test_score_composite <- mi_processed$standardized_test_score_math + mi_processed$standardized_test_score_reading
mi_processed$sat_score_composite_highest_vs_act_converted <- mi_processed$sat_score_math_highest_vs_act_converted + mi_processed$sat_score_verbal_highest_vs_act_converted

## Create math-reading difference measures
# Calculate percentile ranks of test scores. This must be done within each imputation.
mi_processed <- mi_processed %>% 
  group_by(.imp) %>%
  mutate(
    standardized_test_score_math_percent_rank = percent_rank(standardized_test_score_math)*100,
    standardized_test_score_reading_percent_rank = percent_rank(standardized_test_score_reading)*100,
    sat_score_math_from_act_conversion_percent_rank = percent_rank(sat_score_math_highest_vs_act_converted)*100,
    sat_score_verbal_from_act_conversion_percent_rank = percent_rank(sat_score_verbal_highest_vs_act_converted)*100,
  )

# Subtract the reading/verbal score from the math score to get the percentile difference
mi_processed$standardized_test_score_math_reading_difference <- mi_processed$standardized_test_score_math_percent_rank - mi_processed$standardized_test_score_reading_percent_rank
mi_processed$sat_score_math_verbal_difference <- mi_processed$sat_score_math_from_act_conversion_percent_rank - mi_processed$sat_score_verbal_from_act_conversion_percent_rank

# Create categorical versions of math-verbal differences
mi_processed <- mi_processed %>%
  group_by(.imp) %>%
  mutate(
    standardized_test_score_math_reading_difference_categorical = cut(standardized_test_score_math_reading_difference,
      breaks = c(-Inf, -25, -10, -5, 5, 10, 25, Inf),
      labels = c("reading+ 25", "reading+ 11-24", "reading+ 6-1", "math/reading+/-5", "math+ 6-10", "math+ 11-24", "math+ 25"),
    ),
    sat_score_math_reading_difference_categorical = cut(sat_score_math_verbal_difference,
      breaks = c(-Inf, -25, -10, -5, 5, 10, 25, Inf),
      labels = c("verbal+ 25", "verbal+ 11-24", "verbal+ 6-1", "math/verbal+/-5", "math+ 6-10", "math+ 11-24", "math+ 25")
    )
  )

# Specific combinations of M-V
mi_processed$standardized_test_score_m90_r90 <- ifelse(
  mi_processed$standardized_test_score_math_percent_rank >= 87.5 & mi_processed$standardized_test_score_math_percent_rank <= 92.5 &
    mi_processed$standardized_test_score_reading_percent_rank >= 87.5 & mi_processed$standardized_test_score_reading_percent_rank <= 92.5,
  1,
  0
)

mi_processed$standardized_test_score_m90_r60 <- ifelse(
  mi_processed$standardized_test_score_math_percent_rank >= 87.5 & mi_processed$standardized_test_score_math_percent_rank <= 92.5 &
    mi_processed$standardized_test_score_reading_percent_rank >= 57.5 & mi_processed$standardized_test_score_reading_percent_rank <= 62.5,
  1,
  0
)

mi_processed$standardized_test_score_m75_r75 <- ifelse(
  mi_processed$standardized_test_score_math_percent_rank >= 72.5 & mi_processed$standardized_test_score_math_percent_rank <= 77.5 &
    mi_processed$standardized_test_score_reading_percent_rank >= 72.5 & mi_processed$standardized_test_score_reading_percent_rank <= 77.5,
  1,
  0
)

mi_processed$standardized_test_score_m60_r90 <- ifelse(
  mi_processed$standardized_test_score_math_percent_rank >= 57.5 & mi_processed$standardized_test_score_math_percent_rank <= 62.5,
  mi_processed$standardized_test_score_reading_percent_rank >= 87.5 & mi_processed$standardized_test_score_reading_percent_rank <= 92.5 &
    1,
  0
)

mi_processed$standardized_test_score_m50_r50 <- ifelse(
  mi_processed$standardized_test_score_math_percent_rank >= 47.5 & mi_processed$standardized_test_score_math_percent_rank <= 52.5 &
    mi_processed$standardized_test_score_reading_percent_rank >= 47.5 & mi_processed$standardized_test_score_reading_percent_rank <= 52.5,
  1,
  0
)

mi_processed$standardized_test_score_m50_r20 <- ifelse(
  mi_processed$standardized_test_score_math_percent_rank >= 47.5 & mi_processed$standardized_test_score_math_percent_rank <= 52.5 &
    mi_processed$standardized_test_score_reading_percent_rank >= 17.5 & mi_processed$standardized_test_score_reading_percent_rank <= 22.5,
  1,
  0
)

mi_processed$standardized_test_score_m35_r35 <- ifelse(
  mi_processed$standardized_test_score_math_percent_rank >= 32.5 & mi_processed$standardized_test_score_math_percent_rank <= 37.5 &
    mi_processed$standardized_test_score_reading_percent_rank >= 32.5 & mi_processed$standardized_test_score_reading_percent_rank <= 37.5,
  1,
  0
)

mi_processed$standardized_test_score_m20_r50 <- ifelse(
  mi_processed$standardized_test_score_math_percent_rank >= 17.5 & mi_processed$standardized_test_score_math_percent_rank <= 22.5,
  mi_processed$standardized_test_score_reading_percent_rank >= 47.5 & mi_processed$standardized_test_score_reading_percent_rank <= 52.5 &
    1,
  0
)

###############################################################################
# Center variables for multi-level models
###############################################################################

mi_processed <- mi_processed %>% 
  group_by(.imp) %>%
  mutate(
    # Level 1 (student)
    standardized_test_score_math_centered = scale(standardized_test_score_math, center=TRUE, scale=FALSE),
    standardized_test_score_reading_centered = scale(standardized_test_score_reading, center=TRUE, scale=FALSE),
    standardized_test_score_math_reading_difference_centered = scale(standardized_test_score_math_reading_difference, center=TRUE, scale=FALSE),
    standardized_test_score_composite_centered = scale(standardized_test_score_composite, center=TRUE, scale=FALSE),
    ses_centered = scale(ses, center=TRUE, scale=FALSE),
    family_income_1000_centered = scale(family_income_1000, center=TRUE, scale=FALSE),
    family_income_log_centered = scale(family_income_log, center=TRUE, scale=FALSE),
    
    # Level 2 (high school)
    school_percent_10th_grade_free_reduced_lunch_centered = scale(school_percent_10th_grade_free_reduced_lunch, center=TRUE, scale=FALSE),
    school_percent_student_body_in_ap_centered = scale(school_percent_student_body_in_ap, center=TRUE, scale=FALSE),
    school_percent_attend_four_year_college_2003_integer_centered = scale(school_percent_attend_four_year_college_2003_integer, center=TRUE, scale=FALSE),
    school_percent_10th_grade_remedial_math_centered = scale(school_percent_10th_grade_remedial_math, center=TRUE, scale=FALSE)   
  )

###############################################################################
# Save data for analysis in Stata
###############################################################################

# Recode dependent variables to be integers because Stata does not cooperate with
# factors for the dependent variable.
mi_processed$ever_applied_to_college <- mi_processed$ever_applied_to_college %>%
  recode(
    "yes" = 1,
    "no" = 0
  )

mi_processed$applied_to_barrons_very_competitive_plus <- mi_processed$applied_to_barrons_very_competitive_plus %>%
  recode(
    "yes" = 1,
    "no" = 0
  )

mi_processed$accepted_to_barrons_very_competitive_plus <- mi_processed$accepted_to_barrons_very_competitive_plus %>%
  recode(
    "yes" = 1,
    "no" = 0
  )

mi_processed$earned_ba_within_six_years <- mi_processed$earned_ba_within_six_years %>%
  recode(
    "yes" = 1,
    "no" = 0
  )

save.dta13(
  select(
    mi_processed,
    stu_id,
    sch_id,
    ever_applied_to_college,
    applied_selective_college = applied_to_barrons_very_competitive_plus,
    accepted_selective_college = accepted_to_barrons_very_competitive_plus,
    earned_ba_within_six_years,
    test_mr_diff_c = standardized_test_score_math_reading_difference_centered,
    test_composite_c = standardized_test_score_composite_centered,
    ses_centered,
    family_income_1000_centered,
    family_income_log_centered,
    hs_10g_frl_c = school_percent_10th_grade_free_reduced_lunch_centered,
    hs_perc_ap_c = school_percent_student_body_in_ap_centered,
    hs_attend_4yr_int_c = school_percent_attend_four_year_college_2003_integer_centered,
    hs_10g_remedial_math_c = school_percent_10th_grade_remedial_math_centered,
    female,
    race,
    parents_expect_college = parents_expect_college_for_children,
    parents_ed = parents_highest_level_of_education,
    generational_status,
    urbanicity,
    m90_r90 = standardized_test_score_m90_r90,
    m90_r60 = standardized_test_score_m90_r60,
    m75_r75 = standardized_test_score_m75_r75,
    m60_r90 = standardized_test_score_m60_r90,
    m50_r50 = standardized_test_score_m50_r50,
    m50_r20 = standardized_test_score_m50_r20,
    m35_r35 = standardized_test_score_m35_r35,
    m20_r50 = standardized_test_score_m20_r50,
    mi_m = .imp,
    mi_id = .id
  ),
  "data/students_mice_mlm.dta"
)
