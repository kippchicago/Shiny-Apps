# load and save data

library(silounloadr)
library(lubridate)
library(here)
library(tidyverse)
library(googleCloudStorageR)
library(readr)
library(rlang)
library(dplyr)
library(rlang)
library(purrr)

gcs_global_bucket('idea_sqrp_ontrack')


#### Tables ####

schools <- tribble(~"schoolid", ~"schoolabbreviation",
                   78102, "KAP",
                   7810, "KAMS",
                   400146, "KAC",
                   400163, "KBCP",
                   4001802, "KOP",
                   400180, "KOA",
                   4001632, "KBP",
                   4001462, "KACP")

students <- get_powerschool("students") %>%
  select(studentid = id,
         student_number,
         first_name,
         last_name,
         grade_level,
         home_room,
         enroll_status,
         schoolid) %>%
  collect()

#calculate first year
current_first_year <- calc_academic_year(lubridate::today(),
                                         format = "first_year")

current_short_year <- calc_academic_year(lubridate::today(),
                                         format = "short")
#calculate term ID
ps_termid <- calc_ps_termid(current_first_year)

year_id <- ps_termid/100

terms <- get_powerschool("terms") %>%
  filter(id >= ps_termid) %>%
  select(id,
         abbreviation,
         firstday,
         lastday) %>%
  collect()  %>%
  unique()

identify_quarter <- function(date) {
  case_when(
    date >= terms$firstday[1] & date < terms$firstday[3] ~ "Q1",
    date >= terms$firstday[3] & date < terms$firstday[4] ~ "Q2",
    date >= terms$firstday[4] & date < terms$firstday[5] ~ "Q3",
    date >= terms$firstday[5] & date <= terms$lastday[5] ~ "Q4",
    TRUE ~ "not in SY")
}

current_q_dates <- terms %>%
  filter(grepl("Q", abbreviation)) %>%
  mutate(q_number = stringr::str_extract(abbreviation, "\\d")) %>%
  group_by(q_number,
           firstday,
           lastday) %>%
  summarise() %>%
  as.data.frame() %>%
  mutate(q_interval = lubridate::interval(firstday, lastday))

q_dates_no_interval <- current_q_dates %>%
  select(-q_interval)

first_school_day <- q_dates_no_interval %>%
  filter(q_number == 1) %>%
  select(firstday)

last_school_day <- q_dates_no_interval %>%
  dplyr::filter(q_number == 4) %>%
  select(lastday)

first_day <- terms %>%
  filter(abbreviation == current_short_year) %>%
  pull(firstday)

cc <- get_powerschool("cc") %>%
  filter(termid == ps_termid |
           termid == (-1*ps_termid)) %>%
  collect()

students_sections <- students %>%
  filter(enroll_status == 0) %>%
  left_join(cc %>%
              select(dateenrolled,
                     dateleft,
                     sectionid,
                     course_number,
                     studentid),
            by = "studentid") %>%
  filter(dateenrolled >= first_school_day$firstday) %>%
  mutate(status = if_else(sectionid < 0, "Previous Class", "Current Class"))


enrollments <- get_powerschool("ps_enrollment_all") %>%
  select(studentid,
         schoolid,
         entrydate,
         exitdate,
         exitcode,
         yearid) %>%
  collect()

dna_students <- enrollments %>%
  filter(yearid == ps_termid/100) %>%
  mutate(exitdate = as_date(exitdate)) %>%
  group_by(schoolid,
           studentid) %>%
  filter(exitdate == min(exitdate)) %>%
  mutate(dna = exitdate == first_day |
           exitcode == 99) #first day of school


gradebooks <- get_illuminate("gradebooks", schema = "gradebook") %>%
  select(academic_year,
         gradebook_name,
         active,
         is_deleted,
         gradebook_id,
         created_by) %>%
  filter(academic_year == 2020,
         active,
         !is_deleted) %>%
  collect()

# Gradebook Sections
gradebook_sections_0 <- get_illuminate("gradebook_section_course_aff", schema = "gradebook") %>%
  select(date_sync = `_fivetran_synced`,
         gradebook_id,
         ill_sec_id = section_id,
         user_id) %>%
  collect()

gradebook_sections <- gradebook_sections_0 %>%
  group_by(gradebook_id,
           user_id) %>%
  filter(date_sync == max(date_sync)) %>%
  select(-date_sync)

# Illuminate sections on Powerschool
illuminate_sec <- get_illuminate("sections",
                                 "public") %>%
  select(ill_sec_id = section_id,
         ps_sec_id = local_section_id) %>%
  collect()

# Illuminate student IDs to join with PS students table
ill_students <- get_illuminate("students", "public") %>%
  select(student_id,
         local_student_id) %>%
  collect()

# Scores
overall_grades <- get_illuminate("overall_score_cache", schema = "gradebook") %>%
  filter(timeframe_end_date <= "2020-06-20 10:00:00.000000 UTC",
         timeframe_start_date >= "2019-08-10 10:00:00.000000 UTC") %>%
  select(gradebook_id,
         calculated_at,
         mark,
         percentage,
         student_id,
         timeframe_end_date,
         timeframe_start_date) %>%
  collect(n= Inf)

#filter by max calculated_at day/time
overall_grades_recent <- overall_grades %>%
  group_by(gradebook_id,
           student_id,
           timeframe_start_date,
           timeframe_end_date) %>%
  filter(calculated_at == max(calculated_at))

grade_scale <- tribble(
  ~mark, ~points,
  "A+",   4.0,
  "A",    4.0,
  "A-",   4.0,
  "B+",   3.0,
  "B",    3.0,
  "B-",   3.0,
  "C+",   2.0,
  "C",    2.0,
  "C-",   2.0,
  "D+",   1.0,
  "D",    1.0,
  "D-",   1.0,
  "F",    0.0
)
# need grade_scale to calculate primary GPA for core classes

#### GPA ####

# Function to bring in last quarters GPAs from saved file

read_gpa_csv <- function(csv_file, Qnumber) {
  
  # csv_file <- gcs_get_object(csv_file)  # first part already does this before testing if file is imported or not
  
  Qvalue_gpa <- paste0("Q", match.call()[3], "_gpa")
  
  q_gpa <- read_csv(csv_file) %>%
    select(student_number = student_id,
           gpa)
  
  sped_file_name <- paste0("Q", Qnumber, "_sped_gpa_idea.csv")    # DL students have different GPA
  
  sped_file <- gcs_get_object(sped_file_name)
  
  q_gpa_sped <- read_csv(sped_file) %>%
    select(student_number = student_id,
           gpa_sped = gpa)
  
  primary_gpa <- read_csv(csv_file) %>%                           # 3/4th students at KAP need GPA calculated with only core subjects
    filter(course_school %in% c("kop", "kap")) %>%
    select(student_number = student_id,
           ela_grade = `ela grade`,
           math_grade = `math grade`,
           science_grade = `science grade`,
           lit_centers_grade = `literacy centers grade`,
           ss_grade = `social studies grade`) %>%
    gather(course, mark, -student_number) %>%
    left_join(grade_scale, by = "mark") %>%
    select(-mark) %>%
    spread(course, points) %>%
    rowwise() %>%
    mutate(primary_gpa = mean(c(ela_grade, lit_centers_grade, math_grade, science_grade, ss_grade), na.rm = TRUE)) %>%
    select(student_number, primary_gpa)
  
  q_gpa_total <- q_gpa %>%
    left_join(q_gpa_sped , by = c("student_number")) %>%
    mutate(q_gpa_total = if_else(is.na(gpa_sped), gpa, gpa_sped))  %>%
    select(-c(gpa_sped, gpa)) %>%
    left_join(primary_gpa , by = c("student_number")) %>%
    mutate(q_gpa_total = if_else(is.na(primary_gpa), q_gpa_total, primary_gpa)) %>%
    select(student_number,
           gpa = q_gpa_total) %>%
    mutate(quarter = sprintf("Q%s", Qnumber))
  # mutate(!!quo_name(Qvalue_gpa) := q_gpa_total) %>%            # rename and select after combining with sped and primary
  # select(-q_gpa_total)
  
  # return
  q_gpa_total
  
}

# Function to bring in current quarters GPAs from Illuminate

gradebook_function <- function(Qnumber) {
  
  gradebook_grades <- overall_grades_recent %>%
    filter(!is.na(mark)) %>%
    left_join(gradebooks,
              by = "gradebook_id") %>%
    left_join(gradebook_sections %>%
                unique(),
              by = c("gradebook_id",
                     "created_by" = "user_id")) %>%
    left_join(illuminate_sec,
              by = "ill_sec_id") %>%
    left_join(ill_students %>%
                mutate(student_number = as.double(local_student_id)),
              by = "student_id") %>%
    left_join(students, by = "student_number") %>%
    filter(grade_level > 2,
           enroll_status == 0) %>%
    left_join(schools,
              by = "schoolid") %>%
    mutate(ps_sec_id = as.integer(ps_sec_id)) %>%
    left_join(students_sections %>%
                mutate(abs_sec_id = abs(sectionid)) %>%
                select(student_number,
                       # grade_level,
                       abs_sec_id,
                       status,
                       dateleft),
              by = c("student_number",
                     "ps_sec_id" = "abs_sec_id")) %>%
    left_join(dna_students %>%
                select(student_number = studentid,
                       schoolid,
                       dna,
                       dna_exitdate = exitdate),
              by = c("student_number",
                     "schoolid")) %>%
    mutate(status0 = status,
           status = if_else(is.na(status) & dna, "Previous Class", status0)) %>%
    filter(enroll_status == 0,
           active) %>%
    select(-c(dateleft, dna, dna_exitdate, status0)) %>%
    left_join(grade_scale, by = c("mark")) %>%
    mutate(gradebook_name = tolower(gradebook_name)) %>%
    filter(grepl("math|ela|reading|science|studies|literacy|lit|ss|social", gradebook_name),
           !grepl("choice|guided", gradebook_name)) %>%
    ungroup() %>%
    mutate(timeframe_start_date = ymd(timeframe_start_date),
           timeframe_end_date = ymd(timeframe_end_date)) %>%
    mutate(quarter = case_when(
      timeframe_start_date == ymd(current_q_dates$firstday[1]) & timeframe_end_date == ymd(current_q_dates$lastday[1]) ~ 1,
      timeframe_start_date == ymd(current_q_dates$firstday[2]) & timeframe_end_date == ymd(current_q_dates$lastday[2]) ~ 2,
      timeframe_start_date == ymd(current_q_dates$firstday[3]) & timeframe_end_date == ymd(current_q_dates$lastday[3]) ~ 3,
      timeframe_start_date == ymd(current_q_dates$firstday[4]) & timeframe_end_date == ymd(current_q_dates$lastday[4]) ~ 4,
      TRUE ~ 2900
    )) %>%
    filter(quarter == Qnumber)
  
  # group by Quarter and calc gpa
  
  gpa_q <- gradebook_grades %>%
    filter(status == "Current Class") %>%
    group_by(student_number, quarter) %>%
    summarize(gpa = mean(points)) %>%
    mutate(quarter = sprintf("Q%s", quarter))
  # spread(key = quarter, value = gpa) %>%
  # mutate(!!quo_name(Qvalue) := !!quo_name(value)) %>%
  # select(student_number,
  #        !!quo_name(Qvalue))
  
  return(gpa_q)
  
}

# Conditional function that loads in files if they exist on Idea or pulls from Illuminate
# if there's an error in loading the files (because current quarter)

file_or_gradebook_conditional <- function(Q_number) {
  file_name <- sprintf("Q%s_gpa_idea.csv", Q_number)
  
  q_file <- try(gcs_get_object(file_name))
  
  if("raw" == class(q_file)) {
    q_gpa <- read_gpa_csv(q_file, Q_number)     # function to pull grades from idea
  } else {
    q_gpa <- gradebook_function(Q_number)       # function to pull grades from Illuminate if error in try()
  }
  
  q_gpa                                      # return this
  
}

# Map for each quarter

all_q_gpas <- 1:4 %>%
  map_df(file_or_gradebook_conditional)


all_q_gpas_wide <- all_q_gpas %>%
  mutate(gpa = round(gpa, 2)) %>%
  spread(quarter, gpa) %>%
  mutate(y_avg_gpa = rowMeans(.[,-1])) %>%
  mutate(y_avg_gpa = round(y_avg_gpa, 2))


#### Attendance ####

attendance <- get_powerschool("attendance") %>%
  dplyr::filter(att_date >= lubridate::ymd("2019-08-19")) %>%
  dplyr::filter(att_date <= lubridate::today()) %>%
  dplyr::filter(att_mode_code == "ATT_ModeDaily") %>%
  collect()

# get attendance code table
attendance_code <- get_powerschool("attendance_code") %>%
  dplyr::mutate(att_code = if_else(att_code == "true", "T", att_code)) %>% #
  collect()

#combine attendance and attendance code tables
attendance_complete <- attendance %>%
  right_join(attendance_code %>%
               select(attendance_codeid = id,
                      att_code),
             by = "attendance_codeid")

sy <-silounloadr::calc_academic_year(today(), format = 'firstyear')

ps_sy_termid <- silounloadr::calc_ps_termid(sy) %>%
  str_extract("\\d{2}") %>%
  as.integer()

membership <- silounloadr::get_powerschool("ps_membership_reg") %>%
  filter(yearid >= ps_sy_termid) %>%
  select(studentid,
         schoolid,
         date = calendardate,
         enrolled = studentmembership,
         grade_level,
         attendance = ATT_CalcCntPresentAbsent) %>%
  collect()


# combine membership with attendance complete table
member_att <- membership  %>%
  left_join(attendance_complete %>%
              select(studentid,
                     att_date,
                     att_code
                     #presence_status_cd
              ),
            by =c("studentid",
                  "date" = "att_date"))

# Identify whether each att_code is enrolled, present, absent, or tardy for each student
# for each day
attend_student <- member_att %>%
  filter(date >= lubridate::ymd(first_day)) %>%
  mutate(enrolled0 = 1,
         enrolled = if_else(att_code == "D" & !is.na(att_code), 0, enrolled0),
         present0 = ifelse(is.na(att_code) | att_code == "", 1, 0),
         present1 = ifelse(att_code %in%  c("A", "S"), 0, present0),
         present2 = ifelse(att_code == "H", 0.5, present1),
         present3 = ifelse(att_code %in% c("T", "E", "L", "I"), 1, present2),
         present = ifelse(is.na(present2), 1, present3),
         absent = (1 - present)*enrolled,
         tardy = ifelse(att_code %in% "T", 1, 0)) %>%
  left_join(students %>%
              select(studentid,
                     student_number,
                     first_name,
                     last_name,
                     home_room),
            by="studentid") %>%
  inner_join(schools, by=c("schoolid")) %>%
  select(studentid,
         student_number,
         first_name,
         last_name,
         grade_level,
         schoolid,
         #schoolname,
         schoolabbreviation,
         home_room,
         date,
         att_code,
         enrolled,
         present,
         absent,
         tardy)


# summarize this data for every student
attend_school_grade_student <- attend_student %>%
  dplyr::filter(date <= lubridate::ymd(today())) %>%
  group_by(schoolabbreviation, grade_level, student_number, first_name, last_name) %>%
  summarize(enrolled = sum(enrolled),
            present = sum(present),
            absent = sum(absent),
            tardy = sum(tardy)) %>%
  arrange(schoolabbreviation,
          grade_level) %>%
  filter(enrolled >= 30)

attend_school_grade_student <- attend_school_grade_student %>%
  mutate(rate = (present/enrolled)*100,
         rate = round(rate, 1), # add % sign to rate
         quarter_absent = absent,
         quarter_tardy = tardy)

#### Final Table ####

student_attend_gpa <- all_q_gpas_wide %>%
  left_join(students %>%
              select(student_number,
                     first_name,
                     last_name,
                     grade_level,
                     home_room,
                     schoolid), by = "student_number") %>%
  left_join(schools, by = "schoolid") %>%
  left_join(attend_school_grade_student %>% ungroup() %>%
              select(student_number,
                     rate),
            by = "student_number")

#### SQRP Table Calculations ####

track_student <- student_attend_gpa %>%
  mutate(gpa_cat = case_when(y_avg_gpa >= 0 & y_avg_gpa < 2 ~ "Below 2",
                             y_avg_gpa >= 2 & y_avg_gpa <= 2.49 ~ "2.0 < 2.5",
                             y_avg_gpa >= 2.5 & y_avg_gpa <= 2.99 ~ "2.5 < 3.0",
                             y_avg_gpa >= 3 & y_avg_gpa <= 3.49 ~ "3.0 < 3.5",
                             y_avg_gpa >= 3.5 ~ "3.5 - 4.0")) %>%
  mutate(att_cat = case_when(rate < 85 ~ "< 85 ADA",
                             rate >= 85 & rate < 87.5 ~ ">= 85 and < 87.5 ADA",
                             rate >= 87.5 & rate < 90 ~ ">= 87.5 and < 90 ADA",
                             rate >= 90 & rate < 92.5 ~ ">= 90 and < 92.5 ADA",
                             rate >= 92.5 & rate < 95 ~ ">= 92.5 and < 95 ADA",
                             rate >= 95 & rate < 97.5 ~ ">= 95 and < 97.5 ADA",
                             rate >= 97.5 ~ ">= 97.5 ADA")) %>%
  mutate(current_bucket = case_when(
    gpa_cat == "Below 2" & rate < 85 ~ "Less than 2 GPA and less than 85 ADA",
    gpa_cat == "Below 2" & rate >= 85 & rate < 87.5 ~ "Less than 2 GPA and 85-87.49 ADA",
    gpa_cat == "Below 2" & rate >= 87.5 & rate < 90 ~ "Less than 2 GPA and 87.5-89.9 ADA",
    gpa_cat == "Below 2" & rate >= 90 & rate < 92.5 ~ "Less than 2 GPA and 90-92.49 ADA",
    gpa_cat == "Below 2" & rate >= 92.5 & rate < 95 ~ "Less than 2 GPA and 92.5-94.9 ADA",
    gpa_cat == "Below 2" & rate >= 95 & rate < 97.5 ~ "Less than 2 GPA and 95-97.49 ADA",
    gpa_cat == "Below 2" & rate >= 97.5 ~ "Less than 2 GPA and greater than 97.5 ADA",
    gpa_cat == "2.0 < 2.5" & rate < 85 ~ "2.0 to 2.5 GPA and less than 85 ADA",
    gpa_cat == "2.0 < 2.5" & rate >= 85 & rate < 87.5 ~ "2.0 to 2.5 GPA and 85-87.49 ADA",
    gpa_cat == "2.0 < 2.5" & rate >= 87.5 & rate < 90 ~ "2.0 to 2.5 GPA and 87.5-89.9 ADA",
    gpa_cat == "2.0 < 2.5" & rate >= 90 & rate < 92.5 ~ "2.0 to 2.5 GPA and 90-92.49 ADA",
    gpa_cat == "2.0 < 2.5" & rate >= 92.5 & rate < 95 ~ "2.0 to 2.5 GPA and 92.5-94.9 ADA",
    gpa_cat == "2.0 < 2.5" & rate >= 95 & rate < 97.5 ~ "2.0 to 2.5 GPA and 95-97.49 ADA",
    gpa_cat == "2.0 < 2.5" & rate >= 97.5 ~ "2.0 to 2.5 GPA and greater than 97.5 ADA",
    gpa_cat == "2.5 < 3.0" & rate < 85 ~ "2.5 to 3.0 GPA and less than 85 ADA",
    gpa_cat == "2.5 < 3.0" & rate >= 85 & rate < 87.5 ~ "2.5 to 3.0 GPA and 85-87.49 ADA",
    gpa_cat == "2.5 < 3.0" & rate >= 87.5 & rate < 90 ~ "2.5 to 3.0 GPA and 87.5-89.9 ADA",
    gpa_cat == "2.5 < 3.0" & rate >= 90 & rate < 92.5 ~ "2.5 to 3.0 GPA and 90-92.49 ADA",
    gpa_cat == "2.5 < 3.0" & rate >= 92.5 & rate < 95 ~ "2.5 to 3.0 GPA and 92.5-94.9 ADA",
    gpa_cat == "2.5 < 3.0" & rate >= 95 & rate < 97.5 ~ "2.5 to 3.0 GPA and 95-97.49 ADA",
    gpa_cat == "2.5 < 3.0" & rate >= 97.5 ~ "2.5 to 3.0 GPA and greater than 97.5 ADA",
    gpa_cat == "3.0 < 3.5" & rate < 85 ~ "3.0 to 3.5 GPA and less than 85 ADA",
    gpa_cat == "3.0 < 3.5" & rate >= 85 & rate < 87.5 ~ "3.0 to 3.5 GPA and 85-87.49 ADA",
    gpa_cat == "3.0 < 3.5" & rate >= 87.5 & rate < 90 ~ "3.0 to 3.5 GPA and 87.5-89.9 ADA",
    gpa_cat == "3.0 < 3.5" & rate >= 90 & rate < 92.5 ~ "3.0 to 3.5 GPA and 90-92.49 ADA",
    gpa_cat == "3.0 < 3.5" & rate >= 92.5 & rate < 95 ~ "3.0 to 3.5 GPA and 92.5-94.9 ADA",
    gpa_cat == "3.0 < 3.5" & rate >= 95 & rate < 97.5 ~ "3.0 to 3.5 GPA and 95-97.49 ADA",
    gpa_cat == "3.0 < 3.5" & rate >= 97.5 ~ "3.0 to 3.5 GPA and greater than 97.5 ADA",
    gpa_cat == "3.5 - 4.0" & rate < 85 ~ "Greater than 3.5 GPA and less than 85 ADA",
    gpa_cat == "3.5 - 4.0" & rate >= 85 & rate < 87.5 ~ "Greater than 3.5 GPA and 85-87.49 ADA",
    gpa_cat == "3.5 - 4.0" & rate >= 87.5 & rate < 90 ~ "Greater than 3.5 GPA and 87.5-89.9 ADA",
    gpa_cat == "3.5 - 4.0" & rate >= 90 & rate < 92.5 ~ "Greater than 3.5 GPA and 90-92.49 ADA",
    gpa_cat == "3.5 - 4.0" & rate >= 92.5 & rate < 95 ~ "Greater than 3.5 GPA and 92.5-94.9 ADA",
    gpa_cat == "3.5 - 4.0" & rate >= 95 & rate < 97.5 ~ "Greater than 3.5 GPA and 95-97.49 ADA",
    gpa_cat == "3.5 - 4.0" & rate >= 97.5 ~ "Greater than 3.5 GPA and greater than 97.5 ADA")) %>%
  mutate(current_track_status = case_when(
    current_bucket == "Less than 2 GPA and less than 85 ADA" ~ "Off-Track",
    current_bucket == "Less than 2 GPA and 85-87.49 ADA" ~ "Far from On-Track",
    current_bucket == "Less than 2 GPA and 87.5-89.9 ADA" ~ "Far from On-Track",
    current_bucket == "Less than 2 GPA and 90-92.49 ADA" ~ "Far from On-Track",
    current_bucket == "Less than 2 GPA and 92.5-94.9 ADA" ~ "Far from On-Track",
    current_bucket == "Less than 2 GPA and 95-97.49 ADA" ~ "Far from On-Track",
    current_bucket == "Less than 2 GPA and greater than 97.5 ADA" ~ "Far from On-Track",
    current_bucket == "2.0 to 2.5 GPA and less than 85 ADA" ~ "Off-Track",
    current_bucket == "2.0 to 2.5 GPA and 85-87.49 ADA" ~ "Far from On-Track",
    current_bucket == "2.0 to 2.5 GPA and 87.5-89.9 ADA" ~ "Far from On-Track",
    current_bucket == "2.0 to 2.5 GPA and 90-92.49 ADA" ~ "Far from On-Track",
    current_bucket == "2.0 to 2.5 GPA and 92.5-94.9 ADA" ~ "Near On-Track",
    current_bucket == "2.0 to 2.5 GPA and 95-97.49 ADA" ~ "Near On-Track",
    current_bucket == "2.0 to 2.5 GPA and greater than 97.5 ADA" ~ "Almost On-Track",
    current_bucket == "2.5 to 3.0 GPA and less than 85 ADA" ~ "Off-Track",
    current_bucket == "2.5 to 3.0 GPA and 85-87.49 ADA" ~ "Far from On-Track",
    current_bucket == "2.5 to 3.0 GPA and 87.5-89.9 ADA" ~ "Near On-Track",
    current_bucket == "2.5 to 3.0 GPA and 90-92.49 ADA" ~ "Near On-Track",
    current_bucket == "2.5 to 3.0 GPA and 92.5-94.9 ADA" ~ "Almost On-Track",
    current_bucket == "2.5 to 3.0 GPA and 95-97.49 ADA"  ~ "Almost On-Track",
    current_bucket == "2.5 to 3.0 GPA and greater than 97.5 ADA" ~ "On-Track",
    current_bucket == "3.0 to 3.5 GPA and less than 85 ADA" ~ "Off-Track",
    current_bucket == "3.0 to 3.5 GPA and 85-87.49 ADA" ~ "Far from On-Track",
    current_bucket == "3.0 to 3.5 GPA and 87.5-89.9 ADA" ~ "Almost On-Track",
    current_bucket == "3.0 to 3.5 GPA and 90-92.49 ADA"~ "Almost On-Track",
    current_bucket == "3.0 to 3.5 GPA and 92.5-94.9 ADA" ~ "Almost On-Track",
    current_bucket == "3.0 to 3.5 GPA and 95-97.49 ADA" ~ "On-Track",
    current_bucket == "3.0 to 3.5 GPA and greater than 97.5 ADA" ~ "On-Track",
    current_bucket == "Greater than 3.5 GPA and less than 85 ADA" ~ "Off-Track",
    current_bucket == "Greater than 3.5 GPA and 85-87.49 ADA" ~ "Far from On-Track",
    current_bucket == "Greater than 3.5 GPA and 87.5-89.9 ADA" ~ "Almost On-Track",
    current_bucket == "Greater than 3.5 GPA and 90-92.49 ADA" ~ "On-Track",
    current_bucket == "Greater than 3.5 GPA and 92.5-94.9 ADA" ~ "On-Track",
    current_bucket == "Greater than 3.5 GPA and 95-97.49 ADA" ~ "On-Track",
    current_bucket == "Greater than 3.5 GPA and greater than 97.5 ADA" ~ "On-Track")) %>%
  mutate(points = case_when(current_track_status == "On-Track" ~ 5,
                            current_track_status == "Almost On-Track" ~ 4,
                            current_track_status == "Near On-Track" ~ 3,
                            current_track_status == "Far from On-Track" ~ 2,
                            current_track_status == "Off-Track" ~ 1)) %>%
  rename(ADA = rate,
         attendance_bucket = att_cat,
         gpa_bucket = gpa_cat) %>%
  filter(!is.na(current_track_status))


# save all the different files the app needs

gcs_save(track_student, file= "track_student.rda")
