##############################################################################
# Raw Data
##############################################################################
#
# ELS02 - Restricted
#
# For the main student file, we want to drop any students who were code as
# missing from the possibility of having applied to a college.
#   Of 16197, 359 cases (2%) were "Survey component/legitimate skip,"  meaning:
#   1) the component was not administered based on their status (e.g., transfer
#     students did not receive certain items on the in-school survey), 
#   2) the sample member was not a part of the study at the time of
#     administration (e.g., first follow-up freshened sample members were by
#     definition not eligible for the base-year survey), or 
#   3) the sample member was not capable of completing the survey component (e.g.,
#       students who were questionnaire-ineligible due to a language barrier
#     or disability at the time of the survey).
#
#  108 cases (< 1%) were "Item legitimate skip/NA", denoting "questiontions
#    that are not administered based on routing logic; i.e., the items are not
#    applicable based on responses to prior questions.
#
# Every other student will have been a 10th grade who is capable of applying for a 
# college. Drop these now to make later variable construction only reference possible cases.
raw_els02_by_f3_pets <- readRDS("data/els02_r/els02_by_f3_pets.rds") %>% as_tibble() %>% 
  filter(
    F2NAPP2P != "Survey component legitimate skip/NA",
    F2NAPP2P != "Item legitimate skip/NA",
  )

raw_els02_f2_student_institution <- readRDS("data/els02_r/els02_f2_student_institution.rds") %>% as_tibble() 
raw_els02_f3_student_institution <- readRDS("data/els02_r/els02_f3_student_institution.rds") %>% as_tibble()
raw_els02_high_school <- readRDS("data/els02_r/els02_high_school.rds") %>% as_tibble()

# Barron's Competitiveness Indices - Restricted
raw_barrons04 <- read_excel(
  "data/barrons/NCES-BARRONS ADMISSIONS COMPETITIVE INDEX_July-2015_NCES 2016-333.xls",
  sheet = "NCES-Barrons 2004"
) %>% as_tibble()

# IPEDS Data
raw_ipeds04_institutional_characteristics <- read.csv("data/ipeds/hd2004.csv") %>% as_tibble()
raw_ipeds04_test_scores <- read.csv("data/ipeds/ic2004.csv") %>% as_tibble()


##############################################################################
# Baseline Data
##############################################################################

# Student data file using ELS02 BY-F3 data
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
  f2napp2p = raw_els02_by_f3_pets$F2NAPP2P 
)  %>% replace_missing()

# High school file
high_schools <- tibble(
  sch_id = raw_els02_high_school$SCH_ID,
  strat_id = raw_els02_high_school$STRAT_ID,
  psu = raw_els02_high_school$PSU,
) %>% replace_missing()

# College data file using IPEDS institutional characteristics file from 2004
colleges <- tibble(
  unitid = raw_ipeds04_institutional_characteristics$unitid,
  opeid = raw_ipeds04_institutional_characteristics$opeid,
  carnegie = raw_ipeds04_institutional_characteristics$carnegie,
  name = raw_ipeds04_institutional_characteristics$instnm,
  state = raw_ipeds04_institutional_characteristics$stabbr,
  zip = raw_ipeds04_institutional_characteristics$zip,
) %>% replace_missing()


# Student-college data file
student_colleges_f2 <- tibble(
  stu_id = raw_els02_f2_student_institution$STU_ID,
  order = raw_els02_f2_student_institution$F2IORDER,
  unitid = raw_els02_f2_student_institution$F2IIPED
)

##############################################################################
# Prepare College File
##############################################################################

colleges$sector <- raw_ipeds04_institutional_characteristics$sector %>%
  replace_missing() %>%
  recode(
    "0" = "administrative unit",
    "1" = "public, 4-year",
    "2" = "private not-for-profit, 4-year",
    "3" = "private for-profit, 4-year",
    "4" = "public, 2-year",
    "5" = "private not-for-profit, 2-year",
    "6" = "private for-profit, 2-year",
    "7" = "public, < 2-year",
    "8" = "private not-for-profit, < 2-year",
    "9" = "private for-profit, < 2-year",
    "99" = "sector unknown (not active)"
  )

colleges$level <- raw_ipeds04_institutional_characteristics$iclevel %>%
  replace_missing() %>%
  recode(
    "1" = "four or more years",
    "2" = "at least 2 but less than 4 years",
    "3" = "less than 2 years (below associate)",
  )

colleges$ugoffer <- raw_ipeds04_institutional_characteristics$ugoffer %>%
  replace_missing() %>%
  recode(
    "1" = "undergraduate degree or certificate offering",
    "2" = "no undergraduate offering",
  )

colleges$degree_offering <- raw_ipeds04_institutional_characteristics$deggrant %>%
  replace_missing() %>%
  recode(
    "1" = "degree-granting",
    "2" = "nondegree-granting, primarily postsecondary",
  )

colleges$locale <- raw_ipeds04_institutional_characteristics$locale %>%
  replace_missing() %>%
  recode(
    "1" = "large city",
    "2" = "mid-size city",
    "3" = "urban fringe of large city",
    "4" = "urban fringe of mid-size city",
    "5" = "large town",
    "6" = "small town",
    "7" = "rural",
    "9" = "not assigned",
  )

# Get Barron's Competitiveness Indices
colleges <- colleges %>%
  left_join(
    select(raw_barrons04,
      UNITID_04,
      barrons04_competitiveness_index = BARRONS04,
      barrons04_competitiveness_index_plus = BARRONS04P
    ),
    by = c("unitid" = "UNITID_04"),
  )

# Get admissions & test score data
colleges <- colleges %>%
  left_join(
    select(raw_ipeds04_test_scores,
      unitid,
      open_admissions = openadmp,
      applied_men = applcnm,
      applied_women = applcnw,
      admit_men = admssnm,
      admit_women = admssnw,
      sat_verbal_25 = satvr25,
      sat_verbal_75 = satvr75,
      sat_math_25 = satmt25,
      sat_math_75 = satmt75,
    ),
    by = c("unitid" = "unitid"),
  ) %>%
  replace_missing()

colleges$open_admissions <- colleges$open_admissions %>%
  recode(
    `1` = 1,
    `2` = 0,
  )

colleges <- colleges %>%
  mutate(
    admit_rate = (admit_men + admit_women) / (applied_men + applied_women)
  )

##############################################################################
# Prepare Student-College File
##############################################################################

student_colleges_f2 <- student_colleges_f2 %>%
  left_join(
    select(raw_barrons04,
      MERGEID_04,
      barrons04_competitiveness_index = BARRONS04,
      barrons04_competitiveness_index_plus = BARRONS04P
    ),
    by = c("unitid" = "MERGEID_04"),
  )

student_colleges_f2$carnegie_selectivity <- raw_els02_f2_student_institution$F2ISELC %>%
  replace_missing()

student_colleges_f2$state <- raw_els02_f2_student_institution$F2ISTATE %>%
  replace_missing()

student_colleges_f2$level <- raw_els02_f2_student_institution$F2ILEVEL %>%
  replace_missing()

student_colleges_f2$control <- raw_els02_f2_student_institution$F2ICNTRL %>%
  replace_missing()

student_colleges_f2$sector <- raw_els02_f2_student_institution$F2ISECTR %>%
  replace_missing()

student_colleges_f2$open_admissions <- raw_els02_f2_student_institution$F2IOPNAP %>%
  replace_missing()

student_colleges_f2$applied <- raw_els02_f2_student_institution$F2IAPPLY %>%
  replace_missing()

student_colleges_f2$accepted <- raw_els02_f2_student_institution$F2IACCPT %>%
  replace_missing()

student_colleges_f2$attended <- raw_els02_f2_student_institution$F2IATTND %>%
  replace_missing()


##############################################################################
# Student High School Characteristics
##############################################################################

high_schools$geographic_region <- raw_els02_high_school$BYREGION %>%
  replace_missing() %>%
  recode(
    "Northeast" = "northeast",
    "Midwest" = "midwest", 
    "South" = "south",
    "West" = "west"
  ) %>%
  fct_relevel("northeast")

high_schools$urbanicity <- raw_els02_high_school$BYURBAN %>%
  replace_missing() %>%
  recode(
    "Suburban" = "suburban",
    "Urban" = "urban",
    "Rural" = "rural"
  ) %>%
  fct_relevel("suburban")


high_schools$school_control <- raw_els02_high_school$BYSCTRL %>%
  replace_missing()

# Percentages not needing recoding
high_schools$school_percent_10th_grade_free_reduced_lunch <- raw_els02_high_school$BYA21 %>%
  replace_missing()

high_schools$school_percent_student_body_in_ap <- raw_els02_high_school$F1A22F %>%
  replace_missing()

high_schools$school_percent_10th_grade_remedial_math <- raw_els02_high_school$BYA14J %>%
  replace_missing()

high_schools$school_percent_10th_grade_college_prep_program <- raw_els02_high_school$BYA14B %>%
  replace_missing()

# Percentages in surveys with categorical response. Convert to random number within boundaries.
high_schools$school_percent_attend_four_year_college_2003_categorical <- raw_els02_high_school$F1A19A %>%
  replace_missing()

high_schools$school_percent_attend_four_year_college_2003_integer <- mapply(
  convert_percent_categorical_in_high_school_survey_data_to_random_percent_within_boundaries,
  high_schools$school_percent_attend_four_year_college_2003_categorical
)

high_schools$school_percent_attend_two_year_college_2003_categorical <- raw_els02_high_school$F1A19B %>%
  replace_missing()

high_schools$school_percent_attend_two_year_college_2003_integer <- mapply(
  convert_percent_categorical_in_high_school_survey_data_to_random_percent_within_boundaries,
  high_schools$school_percent_attend_two_year_college_2003_categorical
)

high_schools$school_percent_attend_college_app_program_categorical <- raw_els02_high_school$F1A20A %>%
  replace_missing()

high_schools$school_percent_attend_college_app_program_integer <- mapply(
  convert_percent_categorical_in_high_school_survey_data_to_random_percent_within_boundaries,
  high_schools$school_percent_attend_college_app_program_categorical
)

high_schools$school_percent_attend_financial_aid_program_categorical <- raw_els02_high_school$F1A20B %>%
  replace_missing()

high_schools$school_percent_attend_financial_aid_program_integer <- mapply(
  convert_percent_categorical_in_high_school_survey_data_to_random_percent_within_boundaries,
  high_schools$school_percent_attend_financial_aid_program_categorical
)

high_schools$school_percent_attend_sat_course_categorical <- raw_els02_high_school$F1A20C %>%
  replace_missing()

high_schools$school_percent_attend_sat_course_integer <- mapply(
  convert_percent_categorical_in_high_school_survey_data_to_random_percent_within_boundaries,
  high_schools$school_percent_attend_sat_course_categorical
)

high_schools$school_percent_attend_college_fair_categorical <- raw_els02_high_school$F1A20C %>%
  replace_missing()

high_schools$school_percent_attend_college_fair_integer <- mapply(
  convert_percent_categorical_in_high_school_survey_data_to_random_percent_within_boundaries,
  high_schools$school_percent_attend_college_fair_categorical
)


########################################################################
# Student Demographics
##############################################################################

students$female <- raw_els02_by_f3_pets$BYSEX %>%
  recode(
    "Male" = "male",
    "Female" = "female"
    ) %>%
  replace_missing()%>%
  fct_relevel("female")

students$ses <- raw_els02_by_f3_pets$BYSES1 %>% replace_missing()

students$parents_highest_level_of_education <- raw_els02_by_f3_pets$BYPARED %>%
  replace_missing() %>%
  recode(
    "Graduated from 2-year school" = "two-year degree",
    "Graduated from college" = "four-year degree",
    "Completed Master's degree or equivalent" = "advanced degree",
    "Completed PhD, MD, other advanced degree" = "advanced degree",
   .default = "no four-year degree"
  ) %>%
  fct_relevel("no four-year degree")

students$family_income_1000 <- mapply(
  convert_income_to_number_with_random_amount_in_boundary,
  raw_els02_by_f3_pets$BYINCOME %>% replace_missing() 
)/1000

# Add $1 dollar so log doesn' return NA.
students$family_income_log <- log(students$family_income_1000 + 0.001)

students$race <- raw_els02_by_f3_pets$BYRACE %>%
  replace_missing() %>%
  recode(
    "Amer. Indian/Alaska Native, non-Hispanic" = "native american",
    "Asian, Hawaii/Pac. Islander,non-Hispanic" = "asian",
    "Black or African American, non-Hispanic" = "black",
    "Hispanic, no race specified" = "hispanic",
    "Hispanic, race specified" = "hispanic",
    "More than one race, non-Hispanic" = "multiracial",
    "White, non-Hispanic" = "white, non-hispanic"
  ) %>%
  fct_relevel("white, non-hispanic")


students$generational_status = raw_els02_by_f3_pets$BYGNSTAT %>%
  replace_missing() %>%
  recode(
    "SM and mother both born in US" = "born in us to us mother",
    "SM born in US; mother born in PR/non-US" = "born in us to mother born outside us",
    "SM born in Puerto Rico or non-US country" = "born outside us",
  ) %>%
  fct_relevel("born in us to us mother")


##############################################################################
# Educational Expectations & Outcomes
##############################################################################

### Dependent variables in models

## Ever Applied to College
# This variable uses the revised version of F2NAPPLY.
students$ever_applied_to_college <- raw_els02_by_f3_pets$F2NAPP2P %>%
  replace_missing() %>%
  recode(
    "0 - did not apply" = "no",
    .default = "yes"
  ) %>% 
  fct_relevel("yes")

## Applied to highly selective college
students$first_real_college_link <- raw_els02_by_f3_pets$F2PS1 %>%
  replace_missing() %>%
  as.character()

students$applied_to_highly_selective_college_carnegie <- raw_els02_by_f3_pets$F2PSAPSL %>%
  replace_missing() %>%
  recode(
    "Highly selective, 4-yr institution" = "yes",
    "Moderately selective, 4-yr institution" = "no",
    "Inclusive, 4-yr institution" = "no",
    "Selectivity not classified, 4-yr inst" = "no",
    "Selectivity not classified, 2-yr inst" = "no",
    "Selectivity not classified,less than 2yr" = "no"
  ) %>% 
  fct_relevel("yes")

students$application_count_to_barrons_very_competitive_plus <- mapply(
  application_count_to_barrons_level_or_more_competitive,
  students$stu_id,
  2
)

students$applied_to_barrons_very_competitive_plus <- students$application_count_to_barrons_very_competitive_plus %>%
  recode(
    `0` = "no",
    .default = "yes"
  ) %>% 
  fct_relevel("yes")

students$acceptance_count_to_barrons_very_competitive_plus <- mapply(
  acceptance_count_to_barrons_level_or_more_competitive,
  students$stu_id,
  2
)

students$accepted_to_barrons_very_competitive_plus <- students$acceptance_count_to_barrons_very_competitive_plus %>%
  recode(
    `0` = 0,
    .default = 1
  ) %>% 
  recode(
    `0` = "no",
    `1` = "yes"
  ) %>% 
  fct_relevel("yes")

students$attended_barrons_very_competitive_plus_first_real_college <- mapply(
  attended_barrons_level_or_more_competitive,
  students$stu_id,
  students$first_real_college_link,
  2
) %>% 
  recode(
    `0` = "no",
    `1` = "yes"
  ) %>% 
  fct_relevel("yes")

students$transferred_colleges <- raw_els02_by_f3_pets$F2SWITCH %>%
  replace_missing() %>%
  recode(
    "Did not transfer or switch" = "no",
    "Transferred or switched" = "yes",
  ) %>% 
  fct_relevel("yes")

## Earned BA within six years of enrollment
students$attendeded_first_college_four_year_full_time <- ifelse(
  raw_els02_by_f3_pets$F2PS1LVL == "Four or more years" & raw_els02_by_f3_pets$F2PS1FTP == "Full-time or mainly full-time",
  1,
  0
) %>% replace_missing()

students$time_to_degree_months <- raw_els02_by_f3_pets$F3PS2BA %>%
  replace_missing()

students$earned_ba <- raw_els02_by_f3_pets$F3ATTAINMENT %>%
  replace_missing() %>%
  recode(
    "No HS credential, no PS attendance" = "no",
    "HS credential, no PS attendance" = "no",
    "Some PS attendance, no PS credential" = "no",
    "Undergraduate certificate" = "no",
    "Associates degree" = "no",
    "Bachelors degree" = "yes",
    "Post-Baccalaureate certificate" = "yes",
    "Masters degree" = "yes",
    "Post-Masters certificate" = "yes",
    "Doctoral degree" = "yes"
  ) %>% 
  fct_relevel("yes")

students$earned_ba_within_six_years <- ifelse(
  students$earned_ba == 'yes' & students$time_to_degree_months <= 72,
  "yes",
  "no"
) %>%
  fct_relevel("yes")


# Other college attendance related measures
students$attempted_ba <- raw_els02_by_f3_pets$F2EDLEVL %>%
  replace_missing() %>%
  recode(
    "Enrolled in 4-year college or university" = "yes",
    .default = "no"
  ) %>% 
  fct_relevel("yes")

students$ba_status <- raw_els02_by_f3_pets$F3ATTAINMENT %>%
  replace_missing() %>%
  recode(
    "No HS credential, no PS attendance" = "did not try for four-year college",
    "HS credential, no PS attendance" = "did not try for four-year college",
    "Some PS attendance, no PS credential" = "did not try for four-year college",
    "Undergraduate certificate" = "did not try for four-year college",
    "Associates degree" = "did not try for four-year college",
    "Bachelors degree" = "earned BA",
    "Post-Baccalaureate certificate" = "earned BA",
    "Masters degree" = "earned BA",
    "Post-Masters certificate" = "earned BA",
    "Doctoral degree" = "earned BA"
  )

students$ba_status <- fct_expand(students$ba_status, "tried for BA but did not earn")
students$ba_status[students$attempted_ba == 'yes' & students$earned_ba == 'no'] <- "tried for BA but did not earn"

# Measures related to academic expectations/performance/experiences
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
  ) %>%
  recode(
    `0` = "no expectation of college",
    `1` = "expects college"
  ) %>% fct_relevel("expects college")

students$hs_gpa <- raw_els02_by_f3_pets$F1RGP %>%
  replace_missing()

students$self_efficacy_math <- raw_els02_by_f3_pets$BYMATHSE %>%
  replace_missing()

students$self_efficacy_english <- raw_els02_by_f3_pets$BYENGLSE %>%
  replace_missing()

students$ec_hours <- raw_els02_by_f3_pets$F1S27 %>%
  replace_missing() 

students$ec_hours_integer <- mapply(
  convert_percent_categorical_extracurricular_hours_to_integer,
  students$ec_hours
)

students$grades_very_important <- raw_els02_by_f3_pets$BYS37 %>%
  replace_missing() %>%
  recode(
    "Not important" = "no",
    "Somewhat important" = "no",
    "Important" = "no",
    "Very important" = "yes",
  ) %>%
  fct_relevel("yes")


##############################################################################
# Test Scores
##############################################################################

# Standardized scores administered to ELS02 sample
students$standardized_test_score_math <- raw_els02_by_f3_pets$BYTXMSTD %>% replace_missing()
students$standardized_test_score_reading <- raw_els02_by_f3_pets$BYTXRSTD %>% replace_missing()

# SAT Scores
students$sat_score_math <- raw_els02_by_f3_pets$TXSATM %>% replace_missing()
students$sat_score_verbal <- raw_els02_by_f3_pets$TXSATV %>% replace_missing()

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


## Note: any test score conversion, processing, etc.
# will be done after multiple imputation for missing data
