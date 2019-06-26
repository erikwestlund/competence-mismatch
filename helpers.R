##############################################################################
# R Helpers
##############################################################################

'%!in%' <- function(x,y)!('%in%' (x,y))

##############################################################################
# ELS02 provides data with the following missing codes:
#
#   -1: "Don't know" represents respondents who indicated that they didn't
# know the answer to the question.
#
# -2: "Refused" represents respondents who indicated that they refused
# to answer the question
#
# -3: "Item legitimate skip/NA" is filled for questions that are not
# administered based on routing logic; i.e., the items are not
# applicable based on responses to prior questions.
#
# -4: "Nonrespondent" is filled for all variables across the entire
# questionnaire when a sample member did not respond to the
# questionnaire.
#
# -5: "Out of range" represents questionnaire respondents who
# reported values that are out of range.
#
# -6: "Multiple response" represents hard copy questionnaire respondents
# who reported more than one response for an item that requires
# only one response.
#
# -7: "Partial interview-breakoff" is filled for questions that are not
# answered because the respondent does not wish to continue the
# interview or they have run out of time.  This also includes particular
# items that are not included on an abbreviated version questionnaire.
#
# -8: "Survey component legitimate skip/NA" is filled for all items
# within a survey component for sample members who were not administered
# that component by design for one of the following reasons:  1) the
# component was not administered based on their status (e.g., transfer
# students did not receive certain items on the in-school survey), 2)
# the sample member was not a part of the study at the time of
# administration (e.g., first follow-up freshened sample members were by
# definition not eligible for the base-year survey), or 3) the sample
# member was not capable of completing the survey component (e.g.,
# students who were questionnaire-ineligible due to a language barrier
# or disability at the time of the survey).
#
# -9: "Missing" is filled for questions that are not answered when the
# routing suggests that a response should have been provided.
##############################################################################

is_missing <- function(x) {
  return(x %in% -1:-9)
}

replace_missing <- function(.data) {
  data <-  .data %>%
      na_if(-1) %>%
      na_if(-2) %>%
      na_if(-3) %>%
      na_if(-4) %>%
      na_if(-5) %>%
      na_if(-6) %>%
      na_if(-7) %>%
      na_if(-8) %>%
      na_if(-9) %>%
      na_if("Don't know") %>%
      na_if("Refused") %>%
      na_if("Item legitimate skip/NA") %>%
      na_if("Nonrespondent") %>%
      na_if("Out of range") %>%
      na_if("Multiple response") %>%
      na_if("Partial interview-breakoff") %>%
      na_if("Survey component legitimate skip/NA") %>%
      na_if("Missing") %>%
      na_if("Not administered; abbreviated interview or breakoff")
  
  
  if(is.factor(data)) {
    return(
      data %>% droplevels
    )
  }
  
  return(data)
}


##############################################################################
# Generate random income between provided boundaries
##############################################################################

convert_income_to_number_with_random_amount_in_boundary <- function(x){
  if(is.na(x)) {
    return(NA)
  } else if(x == "None") {
    return(0)
  } else if(x == "$1,000 or less") {
    return(sample(1:1000, 1))
  } else if(x == "$1,001-$5,000") {
    return(sample(1001:5000, 1))
  } else if(x == "$5,001-$10,000") {
    return(sample(5001:10000, 1))
  } else if(x == "$10,001-$15,000") {
    return(sample(10001:15000, 1))
  } else if(x == "$15,001-$20,000") {
    return(sample(15001:20000, 1))
  } else if(x == "$20,001-$25,000") {
    return(sample(20001:25000, 1))
  } else if(x == "$25,001-$35,000") {
    return(sample(25001:35000, 1))
  } else if(x == "$35,001-$50,000") {
    return(sample(35001:50000, 1))
  } else if(x == "$50,001-$75,000") {
    return(sample(50001:75000, 1))
  } else if(x == "$75,001-$100,000") {
    return(sample(75001:100000, 1))
  } else if(x == "$100,001-$200,000") {
    return(sample(100001:200000, 1))
  } else if(x == "$200,001 or more") {
    return(200001)
  }
}

##############################################################################
# Generate percent categorical of extra-curricular hours to
# an integer within the provided boundaries
##############################################################################

convert_percent_categorical_extracurricular_hours_to_integer <- function(x){
  if(is.na(x)) {
    return(NA)
  } 
  else if(x == "None") {
    return(0)
  }
  else if(x == "Less than 1 hour/week") {
    return(sample(0:1, 1))
  }
  else if(x == "1-4 hours") {
    return(sample(1:4, 1))
  }
  else if(x == "5-9 hours") {
    return(sample(5:9, 1))
  }
  else if(x == "10-14 hours") {
    return(sample(10:14, 1))
  }
  else if(x == "15-19 hours") {
    return(sample(15:19, 1))
  }
  else if(x == "20-24 hours") {
    return(sample(20:24, 1))
  }
  else if(x == "25 or more hours/week") {
    return(25)
  }
}

##############################################################################
# Generate percent categorical in high school survey data to
# an integer within the provided boundaries
##############################################################################

convert_percent_categorical_in_high_school_survey_data_to_random_percent_within_boundaries <- function(x){
  if(is.na(x)) {
    return(NA)
  } else if(x == "None") {
    return(0)
  } else if(x == "1-10 percent") {
    return(sample(1:10, 1))
  } else if(x == "11-24 percent") {
    return(sample(11:24, 1))
  } else if(x == "25-49 percent") {
    return(sample(25:49, 1))
  } else if(x == "50-74 percent") {
    return(sample(50:74, 1))
  } else if(x == "75-100 percent") {
    return(sample(75:100, 1))
  }
}


##############################################################################
# Convert ACT and SAT scores to their respective equivalents
##############################################################################
# Note:
#   The goal is to get the highest sat math/verbal scores
#   whether those are the original sat scores, or
#   sat scores converted from the act scores
#
#   ACT math to SAT math conversion tables are available from ets.org
#   ACT english and ACT reading can be summed and converted to sat verbal
#   ets.org study is from 1999 but the values are very close to those from
#   the college board/act (https://www.act.org/aap/concordance/pdf/report.pdf)
##############################################################################

##############################################################################
# ACT to SAT
##############################################################################

##############################################################################
# Convert ACT math to SAT math
# see: https://www.ets.org/Media/Research/pdf/RR-99-02-Dorans.pdf
##############################################################################
##############################################################################
convert_act_math_to_sat_math <- function(x) {
  case_when(
    x == 36 ~ 800,
    x == 36 ~ 800,
    x == 35 ~ 790,
    x == 34 ~ 780,
    x == 33 ~ 740,
    x == 32 ~ 720,
    x == 31 ~ 700,
    x == 30 ~ 680,
    x == 29 ~ 650,
    x == 28 ~ 640,
    x == 27 ~ 620,
    x == 26 ~ 600,
    x == 25 ~ 580,
    x == 24 ~ 560,
    x == 23 ~ 540,
    x == 22 ~ 520,
    x == 21 ~ 500,
    x == 20 ~ 480,
    x == 19 ~ 460,
    x == 18 ~ 440,
    x == 17 ~ 420,
    x == 16 ~ 390,
    x == 15 ~ 360,
    x == 14 ~ 330,
    x == 13 ~ 290,
    x == 12 ~ 250,
    x == 11 ~ 220,
    x <= 10 ~ 200
  )
}

##############################################################################
# Convert ACT Reading & English score to SAT verbal
# see: https://www.ets.org/Media/Research/pdf/RR-99-02-Dorans.pdf
##############################################################################

convert_act_english_reading_to_sat_verbal <- function(x) {
  case_when(
    x == 72 ~ 800,
    x == 71 ~ 800,
    x == 70 ~ 790,
    x == 69 ~ 770,
    x == 68 ~ 750,
    x == 67 ~ 740,
    x == 66 ~ 720,
    x == 65 ~ 710,
    x == 64 ~ 690,
    x == 63 ~ 680,
    x == 62 ~ 670,
    x == 61 ~ 660,
    x == 60 ~ 650,
    x == 59 ~ 640,
    x == 58 ~ 630,
    x == 57 ~ 620,
    x == 56 ~ 620,
    x == 55 ~ 610,
    x == 54 ~ 600,
    x == 53 ~ 590,
    x == 52 ~ 580,
    x == 51 ~ 570,
    x == 50 ~ 560,
    x == 49 ~ 560,
    x == 48 ~ 550,
    x == 47 ~ 540,
    x == 46 ~ 530,
    x == 45 ~ 520,
    x == 44 ~ 510,
    x == 43 ~ 510,
    x == 42 ~ 500,
    x == 41 ~ 490,
    x == 40 ~ 480,
    x == 39 ~ 470,
    x == 38 ~ 460,
    x == 37 ~ 450,
    x == 36 ~ 440,
    x == 35 ~ 430,
    x == 34 ~ 430,
    x == 33 ~ 420,
    x == 32 ~ 410,
    x == 31 ~ 400,
    x == 30 ~ 390,
    x == 29 ~ 370,
    x == 28 ~ 360,
    x == 27 ~ 350,
    x == 26 ~ 340,
    x == 25 ~ 330,
    x == 24 ~ 310,
    x == 23 ~ 300,
    x == 22 ~ 280,
    x == 21 ~ 260,
    x == 20 ~ 240,
    x == 19 ~ 210,
    x <= 18 ~ 200,
  )
}

##############################################################################
# Get highest SAT score, considering converted ACT scores
##############################################################################

get_highest_sat_score_with_act_conversions <- function(sat, act) {
  if (is.na(act) && is.na(sat)) {
    return(NA)
  }

  if (is.na(act) && !is.na(sat)) {
    return(sat)
  }

  if (is.na(sat) && !is.na(act)) {
    return(act)
  }

  return(
    ifelse(sat >= act, sat, act)
  )
}


##############################################################################
# Check whether applied/accepted/attended Barron's selectivity as or
# more competitive than value provided
#
# 1 = Most competitive
# 6 = Noncompetitive
##############################################################################

application_count_to_barrons_level_or_more_competitive <- function(student_id, comp_level) {
  
    if(is.na(students$ever_applied_to_college[students$stu_id == student_id])) {
      return(NA)
    }
  
    college_records <- student_colleges_f2 %>%
    filter(
      !is.na(barrons04_competitiveness_index),
      stu_id == student_id,
      applied == "Yes",
      barrons04_competitiveness_index <= comp_level
    )
  
  return(
    nrow(college_records)
  )
}

acceptance_count_to_barrons_level_or_more_competitive <- function(student_id, comp_level) {
  
  if(is.na(students$ever_applied_to_college[students$stu_id == student_id])) {
    return(NA)
  }
  
  college_records <- student_colleges_f2 %>%
    filter(
      !is.na(barrons04_competitiveness_index),
      stu_id == student_id,
      accepted == "Yes",
      barrons04_competitiveness_index <= comp_level
    )
  
  return(
    nrow(college_records)
  )
}

attended_barrons_level_or_more_competitive <- function(student_id, first_real_college, comp_level) {
  
  if(is.na(students$ever_applied_to_college[students$stu_id == student_id])) {
    return(NA)
  }
  
  college_records <- student_colleges_f2 %>%
    filter(
      !is.na(barrons04_competitiveness_index),
      stu_id == student_id,
      order == first_real_college,
      attended == "Yes",
      barrons04_competitiveness_index <= comp_level
    )
  
  return(
    ifelse(nrow(college_records) >= 1, 1, 0)
  )
}

##############################################################################
# Calculate a percent rank within an imputation.
##############################################################################

get_percent_rank_by_imputation <- function(variable, data) {
  imps <- max(data$.imp)
  
  percent_rank(students[variable][students$.imp]) * 100
  
  mi_processed %>% 
    group_by(.imp) %>%
    mutate(rank = percent_rank(sat_score_math_highest_vs_act_converted))
  
}

##############################################################################
# Get Math/Reading composite at provided percent ranks. Useful
# For estimating marginal effects
##############################################################################

get_math_reading_composite_at_percent_ranks <- function(m_percent, r_percent) {
  m_l_bound <- m_percent - 1
  m_r_bound <- m_percent + 1
  r_l_bound <- r_percent - 1
  r_r_bound <- r_percent + 1
  
  m <- summary(students$standardized_test_score_math[students$standardized_test_score_math_percent_rank >= m_l_bound & students$standardized_test_score_math_percent_rank<=m_r_bound])["Mean"][[1]]
  r <- summary(students$standardized_test_score_reading[students$standardized_test_score_reading_percent_rank >= r_l_bound & students$standardized_test_score_reading_percent_rank<=r_r_bound])["Mean"][[1]]
  c <- m+r
  
  return(c)
}
