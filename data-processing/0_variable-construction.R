##############################################################################
# Raw Data
##############################################################################

# ELS02 - Restricted
raw_els02_by_f3_pets <- readRDS("data/els02_r/els02_by_f3_pets.rds") %>% as_tibble
raw_els02_f2_student_institution <- readRDS("data/els02_r/els02_f2_student_institution.rds") %>% as_tibble
raw_els02_f3_student_institution <- readRDS("data/els02_r/els02_f3_student_institution.rds") %>% as_tibble
raw_els02_high_school <- readRDS("data/els02_r/els02_high_school.rds") %>% as_tibble

# Barron's Competitiveness Indices - Restricted
raw_barrons04 <- read_excel(
  "data/barrons/NCES-BARRONS ADMISSIONS COMPETITIVE INDEX_July-2015_NCES 2016-333.xls",
  sheet = "NCES-Barrons 2004"
) %>% as_tibble

# IPEDS Data
raw_ipeds04_institutional_characteristics <- read.csv("data/ipeds/hd2004.csv") %>% as_tibble
raw_ipeds04_test_scores <- read.csv("data/ipeds/ic2004.csv") %>% as_tibble


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
    "1"= "four or more years",
    "2" = "at least 2 but less than 4 years",
    "3"= "less than 2 years (below associate)",
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
    "1"= "degree-granting",
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
  ) %>% replace_missing()

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
# Student Demographics
##############################################################################
students$female <- raw_els02_by_f3_pets$BYSEX %>%
  recode("Male" = 0, "Female" = 1) %>%
  replace_missing()

students$ses <- raw_els02_by_f3_pets$BYSES1 %>% replace_missing()
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

students$first_real_college_link <- raw_els02_by_f3_pets %>% 
  filter(
    STU_ID == student_id
  ) %>%
  select(F2PS1) %>%
  first %>%
  as.character

students$attendeded_first_college_four_year_full_time <- raw_els02_by_f3_pets$F2PS1LVL == "Four or more years" & 
  raw_els02_by_f3_pets$F2PS1FTP == "Full-time or mainly full-time" %>%
  replace_missing()

students$time_to_degree_months <- raw_els02_by_f3_pets$F3PS2BA %>% 
  replace_missing()

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

students$attempted_ba <- raw_els02_by_f3_pets$F2EDLEVL %>%
  replace_missing() %>%
  recode(
    "Enrolled in 4-year college or university" = 1,
    .default = 0
  )


students$ba_status <- raw_els02_by_f3_pets$F3ATTAINMENT %>%
  replace_missing() %>%
  recode(
    "No HS credential, no PS attendance" = "Did not try for four-year college",
    "HS credential, no PS attendance" =  "Did not try for four-year college",
    "Some PS attendance, no PS credential" = "Did not try for four-year college",
    "Undergraduate certificate" = "Did not try for four-year college",
    "Associates degree" =  "Did not try for four-year college",
    "Bachelors degree" = "Earned",
    "Post-Baccalaureate certificate" = "Earned",
    "Masters degree" = "Earned",
    "Post-Masters certificate" = "Earned",
    "Doctoral degree" = "Earned",
  )


students$ba_status <- fct_expand(students$ba_status, "Tried for BA but did not earn")

students$ba_status[students$attempted_ba ==1 & students$earned_ba == 0] = "Tried for BA but did not earn"


students$grades_very_important <- raw_els02_by_f3_pets$BYS37 %>%
  replace_missing() %>%
  recode(
    "Not important" = 0,
    "Somewhat important" = 0,
    "Important" = 0,
    "Very important" = 1,
  )

students$applied_to_highly_selective_college_carnegie <- raw_els02_by_f3_pets$F2PSAPSL %>%
  replace_missing() %>%
  recode(
    "Highly selective, 4-yr institution" = 1,
    "Moderately selective, 4-yr institution" = 0,
    "Inclusive, 4-yr institution" = 0,
    "Selectivity not classified, 4-yr inst" = 0,
    "Selectivity not classified, 2-yr inst" = 0,
    "Selectivity not classified,less than 2yr" = 0
  )

students$applied_to_barrons_very_competitive_plus <- mapply(
  applied_barrons_level_or_more_competitive,
  students$stu_id,
  2
)

students$accepted_to_barrons_very_competitive_plus <- mapply(
  accepted_barrons_level_or_more_competitive,
  students$stu_id,
  2
)

students$attended_barrons_very_competitive_plus_first_real_college <- mapply(
  attended_barrons_level_or_more_competitive,
  students$stu_id,
  students$first_real_college_link,
  2
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
