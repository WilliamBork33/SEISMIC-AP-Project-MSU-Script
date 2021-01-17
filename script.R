######################################################################################################################
## PART 0. STARTUP                                                                                                  ##
## Note: Exact syntax for these steps is likely to vary based on institution-specific variable naming conventions   ##
##       See WG1-P4 GitHub repository for complete examples of institution-specific data cleaning code:             ##
##        - https://github.com/seismic2020/WG1-P4                                                                   ##
######################################################################################################################

##################################################
## PREPARE ENVIRONMENT                          ##
##################################################
## Get working directory to see which directory (folder) I'm working out of.
#getwd()

## Set working directory to whichever directory (folder) I need to be working in (if needed)
#setwd()

## Load desired R packages
library(tidyverse)

##################################################
## IMPORT DESIRED SIS TABLES                    ##
##################################################
## Import data
SISPLVT <- read.csv("../sis-data/dbo_SISPLVT.csv")
SISAPRSA <- read.csv("../sis-data/dbo_SISAPRSA.csv")
SISPETHN <- read.csv("../sis-data/dbo_SISPETHN.csv")
SISAGPA <- read.csv("../sis-data/dbo_SISAGPA.csv")
SISEADR <- read.csv("../sis-data/dbo_SISEADR.csv")
SISATST <- read.csv("../sis-data/dbo_SISATST.csv")
SISPCRS <- read.csv("../sis-data/dbo_SISPCRS.csv")
SISPMJR <- read.csv("../sis-data/dbo_SISPMJR.csv")


#################################################################
## SELECT DESIRED COLUMNS TO KEEP OR DROP IN EACH DATA FRAME ##
#################################################################
## Select desired columns from original data frame; also drops unneeded columns
SISPLVT <- select(SISPLVT,
                  Pid,
                  Term_Seq_Id,
                  Student_Level_Code,
                  System_Rgstn_Status,
                  Term_Code,
                  Primary_Major_Code,
                  Primary_Lvl_Flag,
                  Lvl_Entry_Status,
                  Term_Classification,
                  Cum_Gpa_Credits,
                  Cum_Grd_Pt_Avg)

## Select desired columns from original data frame; also drops unneeded columns
SISAPRSA <- select(SISAPRSA,
                   Pid,
                   Ctzn_Code,
                   Gndr_Flag,
                   Ethnic_Code,
                   Spcl_Qual_Code_1,
                   Spcl_Qual_Code_2,
                   Spcl_Qual_Code_3,
                   Spcl_Qual_Code_4,
                   Spcl_Qual_Code_5,
                   Spcl_Qual_Code_6,
                   Spcl_Qual_Code_7,
                   Spcl_Qual_Code_8,
                   Spcl_Qual_Code_9,
                   Spcl_Qual_Code_10)

## Select desired columns from original data frame; also drops unneeded columns
SISPETHN <- select(SISPETHN,
                   Pid,
                   Ethnic_Code1,
                   Ethnic_Code2,
                   Ethnic_Code3,
                   Ethnic_Code4,
                   Ethnic_Code5,
                   Ethnic_Code6,
                   Ethnic_Code7,
                   Ethnic_Code8,
                   Ethnic_Code9,
                   Ethnic_Code10,
                   Ipeds_Flag)

## Select desired columns from original data frame; also drops unneeded columns
SISAGPA <- select(SISAGPA,
                  Pid,
                  Entity_Id,
                  Hs_Gpa_Type_Code,
                  Hs_Gpa)

## Select desired columns from original data frame; also drops unneeded columns
SISEADR <- select(SISEADR,
                  Entity_Id,
                  Cntry_Code)

## Select desired columns from original data frame; also drops unneeded columns
SISATST <- select(SISATST,
                  Pid,
                  Atst_Type,
                  Atst_Score_1,
                  Atst_Score_2,
                  Atst_Score_3,
                  Atst_Score_4,
                  Atst_Score_5,
                  Atst_Score_6,
                  Atst_Score_7,
                  Atst_Score_8,
                  Atst_Score_9,
                  Atst_Score_10)

## Select desired columns from original data frame; also drops unneeded columns
SISPCRS <- select(SISPCRS,
                  Pid,
                  Term_Seq_Id,
                  Subj_Code,
                  Crse_Code,
                  Grade_Code,
                  Repeat_Status_Flag,
                  Sctn_Crdt_Hours,
                  Term_Code)

## Select desired columns from original data frame; also drops unneeded columns
SISPMJR <- select(SISPMJR,
                  Pid,
                  Major_Code)


##########################################################################################
## FILTER ROWS FOR TARGET SEMESTERS FTIAC & STATUS THEN CREATE NEW FILTERED DATA FRAME  ##
##########################################################################################
## Filter SISPLVT for target semesters (FALL 2013 - SUMMER 2019) by Term_Seq_Id
SISPLVT_FILTERED <- SISPLVT %>%
  filter(Term_Seq_Id == 1134 | Term_Seq_Id == 1136 | Term_Seq_Id == 1142 | #2013-14: Fall, Spring, Summer
         Term_Seq_Id == 1144 | Term_Seq_Id == 1146 | Term_Seq_Id == 1152 | #2014-15: Fall, Spring, Summer
         Term_Seq_Id == 1154 | Term_Seq_Id == 1156 | Term_Seq_Id == 1162 | #2015-16: Fall, Spring, Summer
         Term_Seq_Id == 1164 | Term_Seq_Id == 1166 | Term_Seq_Id == 1172 | #2016-17: Fall, Spring, Summer
         Term_Seq_Id == 1174 | Term_Seq_Id == 1176 | Term_Seq_Id == 1182 | #2017-18: Fall, Spring, Summer
         Term_Seq_Id == 1184 | Term_Seq_Id == 1186 | Term_Seq_Id == 1192 | #2018-19: Fall, Spring, Summer
         Term_Seq_Id == 1194                                               #2019-xx: Fall, xx,      xx
         ) %>%
  ## Then filter for FTIAC students (First Time in Any College) using Sarah Castle's code
  filter(System_Rgstn_Status == "R" |
         System_Rgstn_Status == "E" |
         System_Rgstn_Status == "W" |
         System_Rgstn_Status == "C"
         ) %>%
  filter(Term_Classification == "NEW" |
         Term_Classification == "CONT"
         ) %>%
  filter(Student_Level_Code == "UN" &
         Primary_Lvl_Flag == "Y" &
         Lvl_Entry_Status == "FRST")


###########################################################################################################
## FILTER ROWS FOR EXAMS, HS GPA, AND COURSES THAT RECIEVED A GRADE THEN CREATE NEW FILTERED DATA FRAMES ##
###########################################################################################################
## Filter SISATST for students who took desired AP, SAT, and ACT exams by Atst_Type
SISATST_FILTERED <- SISATST %>%
  filter(Atst_Type == "AP20" |
         Atst_Type == "AP25" |
         Atst_Type == "AP80" |
         Atst_Type == "AP82" |
         Atst_Type == "SAT"  |
         Atst_Type == "SATR" |
         Atst_Type == "SATW" |
         Atst_Type == "SATX" |
         Atst_Type == "ACT5" |
         Atst_Type == "ACT6")

## AP Exam Type Key #####################################################
## AP20 = "Biology Score"
## AP25 = "Chemistry Score"
## AP80 = "Physics C Mechanics Score"
## AP82 = "Physics C Elec& Mag Score"
## Source - https://admissions.msu.edu/documents/MSU_AP_Equivalencies.pdf
#########################################################################

## Filter by Atst_Type and highest Atst_Score_1 for each AP test; create new column for AP scores
df_ap <- SISATST_FILTERED %>% 
  filter(Atst_Type == "AP20" |
         Atst_Type == "AP25" |
         Atst_Type == "AP80" |
         Atst_Type == "AP82") %>%
  group_by(Pid, Atst_Type) %>%
  slice(which.max(Atst_Score_1)) %>%
  mutate(apscore = as.numeric(Atst_Score_1)) %>%
  select(Pid, Atst_Type, apscore)

## Filter by Atst_Type and highest Atst_Score_1 for each SAT test; create new column for SAT English scores
df_sat_englsr <- SISATST_FILTERED %>% 
  filter(Atst_Type == "SAT"  |
         Atst_Type == "SATR" |
         Atst_Type == "SATW" |
         Atst_Type == "SATX") %>%
  group_by(Pid) %>%
  slice(which.max(Atst_Score_1)) %>%
  mutate(sat_englsr = as.numeric(Atst_Score_1)) %>%
  select(Pid, Atst_Type, sat_englsr)

## Filter by Atst_Type and highest Atst_Score_1 for each SAT test; create new column for SAT Math scores
df_sat_mathsr <- SISATST_FILTERED %>% 
  filter(Atst_Type == "SAT"  |
           Atst_Type == "SATR" |
           Atst_Type == "SATW" |
           Atst_Type == "SATX") %>%
  group_by(Pid) %>%
  slice(which.max(Atst_Score_2)) %>%
  mutate(sat_mathsr = as.numeric(Atst_Score_2)) %>%
  select(Pid, Atst_Type, sat_mathsr)

## Join the SAT English and SAT Math data frames by Pid; select desired columns; rename a single column back to original name
df_sat <- full_join(df_sat_englsr, df_sat_mathsr, by = "Pid") %>%
  rename(Atst_Type_Engl = Atst_Type.x, Atst_Type_Math = Atst_Type.y)

## Filter for ACT5 or ACT6
df_act <- SISATST_FILTERED %>%
  filter(Atst_Type == "ACT5" |
           Atst_Type == "ACT6") %>%
  group_by(Pid) %>%
  select(Pid, Atst_Type, Atst_Score_1, Atst_Score_2, Atst_Score_3, Atst_Score_4, Atst_Score_5)

## Create new column for ACT English Score
df_act <- df_act %>%
  mutate(act_englsr = if_else(Atst_Type == "ACT5", as.numeric(as.character(Atst_Score_1)), as.numeric(as.character(Atst_Score_1))))

## Create new column for ACT Math Score
df_act <- df_act %>%
  mutate(act_mathsr = if_else(Atst_Type == "ACT5", as.numeric(as.character(Atst_Score_4)), as.numeric(as.character(Atst_Score_2))))

## Convert to ACT English Scores to SAT English Scores; creates new columns with these converted scores; this takes a few minutes to run
df_act <- df_act %>%
  mutate(converted_englsr = if_else(act_englsr == 36, 800,
                            if_else(act_englsr == 35, 780,
                            if_else(act_englsr == 34, 760,
                            if_else(act_englsr == 33, 740,
                            if_else(act_englsr == 32, 720,
                            if_else(act_englsr == 31, 710,
                            if_else(act_englsr == 30, 700,
                            if_else(act_englsr == 29, 680,
                            if_else(act_englsr == 28, 660,
                            if_else(act_englsr == 27, 640,
                            if_else(act_englsr == 26, 610,
                            if_else(act_englsr == 25, 590,
                            if_else(act_englsr == 24, 580,
                            if_else(act_englsr == 23, 560,
                            if_else(act_englsr == 22, 540,
                            if_else(act_englsr == 21, 530,
                            if_else(act_englsr == 20, 520,
                            if_else(act_englsr == 19, 510,
                            if_else(act_englsr == 18, 500,
                            if_else(act_englsr == 17, 470,
                            if_else(act_englsr == 16, 430,
                            if_else(act_englsr == 15, 400,
                            if_else(act_englsr == 14, 360,
                            if_else(act_englsr == 13, 330,
                            if_else(act_englsr == 12, 310,
                            if_else(act_englsr == 11, 280,
                            if_else(act_englsr == 10, 260, NaN
                            ))))))))))))))))))))))))))))

## Convert to ACT Math scores to SAT Math scores; creates new columns with these converted scores; this takes a few minutes to run
df_act <- df_act %>%
  mutate(converted_mathsr = if_else(act_mathsr == 36, 800,
                            if_else(act_mathsr == 35, 780,
                            if_else(act_mathsr == 34, 760,
                            if_else(act_mathsr == 33, 740,
                            if_else(act_mathsr == 32, 720,
                            if_else(act_mathsr == 31, 710,
                            if_else(act_mathsr == 30, 700,
                            if_else(act_mathsr == 29, 680,
                            if_else(act_mathsr == 28, 660,
                            if_else(act_mathsr == 27, 640,
                            if_else(act_mathsr == 26, 610,
                            if_else(act_mathsr == 25, 590,
                            if_else(act_mathsr == 24, 580,
                            if_else(act_mathsr == 23, 560,
                            if_else(act_mathsr == 22, 540,
                            if_else(act_mathsr == 21, 530,
                            if_else(act_mathsr == 20, 520,
                            if_else(act_mathsr == 19, 510,
                            if_else(act_mathsr == 18, 500,
                            if_else(act_mathsr == 17, 470,
                            if_else(act_mathsr == 16, 430,
                            if_else(act_mathsr == 15, 400,
                            if_else(act_mathsr == 14, 360,
                            if_else(act_mathsr == 13, 330,
                            if_else(act_mathsr == 12, 310,
                            if_else(act_mathsr == 11, 280,
                            if_else(act_mathsr == 10, 260, NaN
                            ))))))))))))))))))))))))))))

## Select desired columns from data frame; also drops unneeded columns; also reorders columns
df_act <- df_act %>%
  select(Pid, Atst_Type, act_englsr, act_mathsr, converted_englsr, converted_mathsr)

## Join ACT score data frame with SAT score data frame
df_act_sat <- full_join(df_act, df_sat, by = "Pid") %>%
  distinct()

## Find top converted score
df_top_converted_englsr <- df_act_sat %>%  
  group_by(Pid) %>%
  slice(which.max(converted_englsr)) %>%
  mutate(converted_englsr = converted_englsr) %>%
  select(Pid, converted_englsr)

## Find top converted score
df_top_converted_mathsr <- df_act_sat %>%
  group_by(Pid) %>%
  slice(which.max(converted_mathsr)) %>%
  mutate(converted_mathsr = converted_mathsr) %>%
  select(Pid, converted_mathsr)

## Find top SAT score
df_top_sat_englsr <- df_act_sat %>%  
  group_by(Pid) %>%
  slice(which.max(sat_englsr)) %>%
  mutate(sat_englsr = sat_englsr) %>%
  select(Pid, sat_englsr)

## Find top SAT score
df_top_sat_mathsr <- df_act_sat %>%
  group_by(Pid) %>%
  slice(which.max(sat_mathsr)) %>%
  mutate(sat_mathsr = sat_mathsr) %>%
  select(Pid, sat_mathsr)

## Join top score data frames for both converted ACT and normal SAT scores
df_top_converted_combined <- full_join(df_top_converted_englsr, df_top_converted_mathsr, by = "Pid")
df_top_sat_combined <- full_join(df_top_sat_englsr, df_top_sat_mathsr, by = "Pid")
df_top_act_sat_combined <- full_join(df_top_converted_combined, df_top_sat_combined, by = "Pid")

## Convert all "NA" to 0 to numerically compare converted SAT scores to normal SAT scores
df_top_act_sat_combined <- df_top_act_sat_combined %>%
  mutate(converted_englsr = ifelse(is.na(converted_englsr), 0, converted_englsr)) %>%
  mutate(converted_mathsr = ifelse(is.na(converted_mathsr), 0, converted_mathsr)) %>%
  mutate(sat_englsr = ifelse(is.na(sat_englsr), 0, sat_englsr)) %>%
  mutate(sat_mathsr = ifelse(is.na(sat_mathsr), 0, sat_mathsr))

## Create column for top English SAT score by comparing converted and real SAT scores and choosing highest score
df_top_act_sat_combined <- df_top_act_sat_combined %>%
  mutate(englsr = ifelse(converted_englsr > sat_englsr, converted_englsr, sat_englsr))

## Create column for top Math SAT score by comparing converted and real SAT scores and choosing highest score 
df_top_act_sat_combined <- df_top_act_sat_combined %>%
  mutate(mathsr = ifelse(converted_mathsr > sat_mathsr, converted_mathsr, sat_mathsr))

## Select desired columns from data frame; also drops unneeded columns; also reorders columns
ACT_SAT_SCORES <- df_top_act_sat_combined %>%
  select(Pid,
         englsr,
         mathsr) %>%
  distinct()

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## STILL NEED TO CHECK ON "NORMALINZING" GPAS TO 4.0 SCALE                                               ##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## Join SISAGPA and SISEADR to put Cntry_Code column together with other SISAGPRA columns; create new data frame
SISAGPA_FILTERED <- left_join(x = SISAGPA, y = SISEADR, by = "Entity_Id")

## Filter SISAGPA_FILTERED for only HSRP GPA (high school reported GPA); add column high school country code; remove duplicates
SISAGPA_FILTERED <- SISAGPA_FILTERED %>%
  filter(Hs_Gpa_Type_Code == "HSRP") %>%
  group_by(Pid) %>%
  slice(which.max(Hs_Gpa)) %>%
  mutate(Us_Hs = if_else(Cntry_Code == "US", 1, 0)) %>%
  distinct()

## Filter SISAPRSA for ONLY U.S. citizens
SISAPRSA_FILTERED <- SISAPRSA %>%
  filter(Ctzn_Code == "CTZN")

## Filter SISPCRS for students who received a course grade by Term_Seq_Id from FS2013 to US2019
## Then filter for grades in only biology, chemistry, and physics
SISPCRS_FILTERED <- SISPCRS %>%
  filter(Term_Seq_Id == 1134 | Term_Seq_Id == 1136 | Term_Seq_Id == 1142 | #2013-14: Fall, Spring, Summer
         Term_Seq_Id == 1144 | Term_Seq_Id == 1146 | Term_Seq_Id == 1152 | #2014-15: Fall, Spring, Summer
         Term_Seq_Id == 1154 | Term_Seq_Id == 1156 | Term_Seq_Id == 1162 | #2015-16: Fall, Spring, Summer
         Term_Seq_Id == 1164 | Term_Seq_Id == 1166 | Term_Seq_Id == 1172 | #2016-17: Fall, Spring, Summer
         Term_Seq_Id == 1174 | Term_Seq_Id == 1176 | Term_Seq_Id == 1182 | #2017-18: Fall, Spring, Summer
         Term_Seq_Id == 1184 | Term_Seq_Id == 1186 | Term_Seq_Id == 1192 | #2018-19: Fall, Spring, Summer
         Term_Seq_Id == 1194                                               #2019-xx: Fall, xx,      xx
         ) %>%
  filter(Subj_Code == "BS"  |
         Subj_Code == "CEM" |
         Subj_Code == "PHY")

#############################################################################
## JOIN TOGETHER FILTERED DATA FRAMES TO CREATE THE FULL SAMPLE DATA FRAME ##
#############################################################################

## Join SISPLVT_FILTERED with SISAPRSA_FILTERED by Pid to create data frame for FTIAC (FS13-FS19) AND domestic + Gender/Ethnicity
PARTIAL_SAMPLE <- inner_join(x = SISPLVT_FILTERED, y = SISAPRSA_FILTERED, by = "Pid")
## Join PARTIAL_SAMPLE with ACT_SAT_SCORES by Pid to create data frame for FTIAC (FS13-FS19) AND domestic + Gender/Ethnicity + ACT/SAT scores
PARTIAL_SAMPLE <- left_join(x = PARTIAL_SAMPLE, y = ACT_SAT_SCORES, by = "Pid")
## Join PARTIAL_SAMPLE with SISAGPA_FILTERED by Pid to create data frame for FTIAC (FS13-FS19) AND domestic + Gender/Ethnicity + ACT/SAT scores + HS GPAs
PARTIAL_SAMPLE <- left_join(x = PARTIAL_SAMPLE, y = SISAGPA_FILTERED, by = "Pid")
## Join PARTIAL_SAMPLE with df_ap by Pid to create data frame for FTIAC (FS13-FS19) AND domestic + Gender/Ethnicity + ACT/SAT scores + HS GPAs + AP scores
PARTIAL_SAMPLE <- left_join(x = PARTIAL_SAMPLE, y = df_ap, by = "Pid")
## Join PARTIAL_SAMPLE with SISPCRS_FILTERED by Pid to create data frame for FTIAC (FS13-FS19) AND domestic + Gender/Ethnicity + ACT/SAT scores + HS GPAs + AP scores + Gender/Ethnicity + Course Grades
PARTIAL_SAMPLE <- left_join(x = PARTIAL_SAMPLE, y = SISPCRS_FILTERED, by = "Pid")
## Join PARTIAL_SAMPLE with SISPMJR by Pid to create data frame for FTIAC (FS13-FS19) AND domestic + Gender/Ethnicity + ACT/SAT scores + HS GPAs + AP scores + Gender/Ethnicity + Course Grades + Major
PARTIAL_SAMPLE <- left_join(x = PARTIAL_SAMPLE, y = SISPMJR, by = "Pid")

## Re-filter to remove the PIDs that got joined but have NAs for Subj_Code (because it is a non-BS, CEM, or PHY course)
PARTIAL_SAMPLE <- PARTIAL_SAMPLE %>%
  filter(Subj_Code == "BS"  |
         Subj_Code == "CEM" |
         Subj_Code == "PHY")

## Create FULL_SAMPLE data frame by selecting desired columns from PARTIAL_SAMPLE; also drops unneeded columns; also reorders columns
FULL_SAMPLE <- PARTIAL_SAMPLE %>%
  select(Pid,
         Term_Code.x,
         Term_Seq_Id.x,
         Gndr_Flag,
         Ctzn_Code,
         englsr,
         mathsr,
         Hs_Gpa,
         Us_Hs,
         Ethnic_Code,
         Spcl_Qual_Code_1,
         Spcl_Qual_Code_2,
         Spcl_Qual_Code_3,
         Spcl_Qual_Code_4,
         Spcl_Qual_Code_5,
         Spcl_Qual_Code_6,
         Spcl_Qual_Code_7,
         Spcl_Qual_Code_8,
         Spcl_Qual_Code_9,
         Spcl_Qual_Code_10,
         Atst_Type,
         apscore,
         Subj_Code,
         Crse_Code,
         Grade_Code,
         Repeat_Status_Flag,
         Term_Code.y,
         Term_Seq_Id.y,
         Cum_Gpa_Credits,
         Cum_Grd_Pt_Avg,
         Sctn_Crdt_Hours,
         FIRST_TERM_SEQ_ID,
         TERM_CODE,
         AIDYR,
         PELL_ELIG)

## Rename certain columns from MSU variable names to SEISMIC variable names
FULL_SAMPLE <- FULL_SAMPLE %>%
  rename(st_id = Pid,
         ethniccode = Ethnic_Code,
         female = Gndr_Flag,
         international = Ctzn_Code,
         semester = Term_Code.x,
         cohort = Term_Seq_Id.x,
         hsgpa = Hs_Gpa,
         us_hs = Us_Hs,
         crs_sbj = Subj_Code,
         crs_catalog = Crse_Code,
         numgrade = Grade_Code,
         crs_retake = Repeat_Status_Flag,
         crs_term = Term_Code.y,
         crs_termcd = Term_Seq_Id.y)

## Keep distinct rows by removing duplicates
FULL_SAMPLE <- distinct(FULL_SAMPLE)

## Inspect FULL_SAMPLE data frame
#view(FULL_SAMPLE)

## Write table to .csv file
write.csv(FULL_SAMPLE, file = "FULL_SAMPLE.csv", row.names = FALSE)


###############################################################################################################
## PART 1. CLEAN STUDENT LEVEL VARIABLES                                                                     ##
## A. Rename and generate/recode student level variables as needed to match common SEISMIC AP variable names ##
## Note: Student level data should contain unique rows per student                                           ##
###############################################################################################################
## Create new student level variable data frame copied from FULL_SAMPLE
df_std <- FULL_SAMPLE

## Select desired columns from data frame; also drops unneeded columns
df_std <- df_std %>%
  select(st_id,
         semester,
         cohort,
         female,
         international,
         hsgpa,
         us_hs,
         ethniccode,
         Spcl_Qual_Code_1,
         Spcl_Qual_Code_2,
         Spcl_Qual_Code_3,
         Spcl_Qual_Code_4,
         Spcl_Qual_Code_5,
         Spcl_Qual_Code_6,
         Spcl_Qual_Code_7,
         Spcl_Qual_Code_8,
         Spcl_Qual_Code_9,
         Spcl_Qual_Code_10,
         englsr,
         mathsr)

## Create a column for transfer status
## MSU already filtered out transfer students so just assign everyone with 0 to indicate non-transfer student
df_std <- mutate(df_std, transfer = 0)

## Create columns for lowincomeflag using PELL_ELIG values
df_std <- df_std %>% 
  mutate(lowincomeflag = if_else(PELL_ELIG == "Y", 1, 0))

## Preserving this numerical cohort value for Becky to get financial info by recreating term_seq_id variable mapped to cohort (which was originally term_seq_id)
df_std <- df_std %>%
  mutate(term_seq_id = cohort)

## Change cohort from a semester code (i.e. 1134) to a number for the year (i.e. 2013)
df_std <- mutate(df_std, cohort = if_else(cohort == 1134, 2013, #2013: Fall
                                  if_else(cohort == 1136, 2013, #2014: Spring
                                  if_else(cohort == 1142, 2013, #2014: Summer
                                          
                                  if_else(cohort == 1144, 2014, #2014: Fall
                                  if_else(cohort == 1146, 2014, #2015: Spring
                                  if_else(cohort == 1152, 2014, #2015: Summer
                                          
                                  if_else(cohort == 1154, 2015, #2015: Fall
                                  if_else(cohort == 1156, 2015, #2016: Spring
                                  if_else(cohort == 1162, 2015, #2016: Summer
                                  
                                  if_else(cohort == 1164, 2016, #2016: Fall
                                  if_else(cohort == 1166, 2016, #2017: Spring
                                  if_else(cohort == 1172, 2016, #2017: Summer
                                          
                                  if_else(cohort == 1174, 2017, #2017: Fall
                                  if_else(cohort == 1176, 2017, #2018: Spring
                                  if_else(cohort == 1182, 2017, #2018: Summer
                                          
                                  if_else(cohort == 1184, 2018, #2018: Fall
                                  if_else(cohort == 1186, 2018, #2019: Spring
                                  if_else(cohort == 1192, 2018, #2019: Summer
                                          
                                  if_else(cohort == 1194, 2019, 2222 #2019: Fall
                                  ))))))))))))))))))))

## Create a new column for firstgen
## Check for "FGEN" in each Spcl_Qual_Code column then assign value of 1 if "FGEN" is present in any of these special code columns
df_std <- mutate(df_std, firstgen = if_else(Spcl_Qual_Code_1 == "FGEN" |
                                            Spcl_Qual_Code_2 == "FGEN" |
                                            Spcl_Qual_Code_3 == "FGEN" |
                                            Spcl_Qual_Code_4 == "FGEN" |
                                            Spcl_Qual_Code_5 == "FGEN" |
                                            Spcl_Qual_Code_6 == "FGEN" |
                                            Spcl_Qual_Code_7 == "FGEN" |
                                            Spcl_Qual_Code_8 == "FGEN" |
                                            Spcl_Qual_Code_9 == "FGEN" |
                                            Spcl_Qual_Code_10 == "FGEN", 1, 0))

## Change gender from "F" to 1 and "M" to 0
df_std <- mutate(df_std, female = if_else(female == "F", 1, 0))

## Assign U.S. citizens a value of 0 and all others a value of 1 for international student
## MSU already filtered out international students; everyone is already CTZN and will be assigned with 0 to indicate domestic (non-international) student
df_std <- mutate(df_std, international = if_else(international == "CTZN", 0, 1))

## MSU to SEISMIC Ethnic Code Key #######################################
## MSU                                | SEISMIC
## 1 = white                          | white = 6
## 2 = black                          | black = 4
## 3 = [n/a]                          | [n/a]
## 4 = [n/a]                          | [n/a]
## 5 = Amer. Indian                   | Amer. Indian/Alaska Native = 2
## 6 = Asian/P.I.                     | Native HW/P.I. = 5
## 7 = Other                          | [n/a]
## 8 = Blank                          | [n/a]
## 9 = Not requested                  | [n/a]
## 10 = HW./P.I.                      | Native HW/P.I. = 5
## 11 = Asian                         | Asian = 3
## H = Hispanic (#3 or #4)            | Hispanic/Latino = 1
## M = Multiple (2+ codes; exclude H) | [n/a]
########################################################################

## Map MSU ethnicity codes onto SEISMIC ethnicity codes
## Note I'm using ifelse instead of if_else because if_else throws an error of - Error: `false` must be a character vector, not a double vector
df_std <- mutate(df_std, ethniccode = ifelse(ethniccode == 1, 6, 
                                      ifelse(ethniccode == 2, 4, 
                                      ifelse(ethniccode == 5, 2, 
                                      ifelse(ethniccode == 6, 5,
                                      ifelse(ethniccode == 7, "Other",
                                      ifelse(ethniccode == 8, "Blank",
                                      ifelse(ethniccode == 9, "Not Requested",
                                      ifelse(ethniccode == 10, 5,
                                      ifelse(ethniccode == 11, 3,
                                      ifelse(ethniccode == "H", 1,
                                      ifelse(ethniccode == "M", "M", NaN))))))))))))

## SEISMIC URM Code Key ####################################################################################################################
## 0 = Student only self-identifies as White
## 1 = Student's self-identity includes Latino or Hispanic, Black or African American, American Indian, Alaska Native, or Pacific Islander 
## 2 = Asian/Asian American only or Asian/Asian American and White only
## 3 = Two or more unresolvable/catch-all unknown.
############################################################################################################################################

## Create the URM group using SEISMIC ethnicity codes
## (1 = Hispanic; 2 = American Indian/Native Alaskan; 4 = Black; 5 = Native HW/PI)
URM <- c(1, 2, 4, 5)

## Create column for ethniccode_cat variables
df_std <- mutate(df_std, ethniccode_cat = if_else(ethniccode == 6, 0,       ## 0 = White
                                          if_else(ethniccode %in% URM, 1,   ## 1 = URM 
                                          if_else(ethniccode == 3, 2, 3)))) ## 2 = Asian; 3 = "Two or more unresolved"

## Create a URM binary variable: 1 = URM; 0 = Not URM
df_std <- mutate(df_std, urm = if_else(ethniccode_cat == 1, 1, 0))

## Select desired columns from data frame; also drops unneeded columns; also reorders columns
df_std <- df_std %>%
  select(st_id,
         firstgen,
         ethniccode,
         ethniccode_cat,
         urm,
         female,
         lowincomeflag,
         transfer,
         international,
         us_hs,
         cohort,
         term_seq_id,
         englsr,
         mathsr,
         hsgpa)

## Keep distinct rows by removing duplicates
df_std <- distinct(df_std)

## Write data frame to .csv file
write.csv(df_std, file = "df_std.csv", row.names = FALSE)


##############################################################################################################
## PART 2. CLEAN COURSE LEVEL VARIABLES                                                                     ##
## A. Rename and generate/recode course level variables as needed to match common SEISMIC AP variable names ##
## Note: Course level data will likely contain multiple rows for each course, per student                   ##
##############################################################################################################
## Create new student level variable dataset copied from FULL_SAMPLE
df_crs <- FULL_SAMPLE

## Select desired columns from data frame; also drops unneeded columns
df_crs <- df_crs %>%
  select(st_id,
       cohort,
       crs_sbj,
       crs_catalog,
       numgrade,
       crs_retake,
       crs_term,
       crs_termcd
       )

## Create course name variable
df_crs <- unite(df_crs, crs_name, crs_sbj, crs_catalog, remove = FALSE, na.rm = FALSE)

## Find if the course was a withdrawal ("W") or not and assign a value of 1 if true
df_crs <- df_crs %>%
  mutate(numgrade_w = ifelse(numgrade == "W", 1, 0))

## Change the non-numeric grades to "NA" so that numgrade variable becomes type numeric
df_crs$numgrade <- na_if(df_crs$numgrade, "W")  # Withdrawn
df_crs$numgrade <- na_if(df_crs$numgrade, "V")  # Visitor
df_crs$numgrade <- na_if(df_crs$numgrade, "U")  # Unfinished Work
df_crs$numgrade <- na_if(df_crs$numgrade, "I")  # Incomplete
df_crs$numgrade <- na_if(df_crs$numgrade, "ET") # Extension

## Find if the course was a summer course or not and assign a value of 1 if true
df_crs <- df_crs %>%
  mutate(summer_crs = ifelse(str_detect(df_crs$crs_term, "US"), 1, 0))

## Create crs_semester variable to hold the crs_term characters (they are a quicker visual reference than a 4 digit code)
## Then remap crs_term to be the Fall Academic year instead of individual semester codes to match project convention
df_crs <- df_crs %>%
  mutate(crs_semester = crs_term) %>%
  mutate(crs_term = if_else(crs_termcd == 1134, 2013,
                    if_else(crs_termcd == 1136 | crs_termcd == 1142 | crs_termcd == 1144, 2014,
                    if_else(crs_termcd == 1146 | crs_termcd == 1152 | crs_termcd == 1154, 2015,
                    if_else(crs_termcd == 1156 | crs_termcd == 1162 | crs_termcd == 1164, 2016,
                    if_else(crs_termcd == 1166 | crs_termcd == 1172 | crs_termcd == 1174, 2017,
                    if_else(crs_termcd == 1176 | crs_termcd == 1182 | crs_termcd == 1184, 2018,
                    if_else(crs_termcd == 1186 | crs_termcd == 1192 | crs_termcd == 1194, 2019, NaN
                    ))))))))

df_crs <- df_crs %>%
  mutate(enrl_from_cohort = if_else(crs_termcd - cohort == 0, 0.33, # Fall
                            if_else(crs_termcd - cohort == 2, 0.66, # Spring
                            if_else(crs_termcd - cohort == 4, 1.00, # Summer
                            if_else(crs_termcd - cohort == 6, 0.66, # Spring
                            if_else(crs_termcd - cohort == 8, 1.00, # Summer
                                    
                            if_else(crs_termcd - cohort == 10, 1.33,
                            if_else(crs_termcd - cohort == 12, 1.66,
                            if_else(crs_termcd - cohort == 14, 2.00,
                            if_else(crs_termcd - cohort == 16, 1.66,
                            if_else(crs_termcd - cohort == 18, 2.00,
                                    
                            if_else(crs_termcd - cohort == 20, 2.33,
                            if_else(crs_termcd - cohort == 22, 2.66,
                            if_else(crs_termcd - cohort == 24, 3.00,
                            if_else(crs_termcd - cohort == 26, 2.66,
                            if_else(crs_termcd - cohort == 28, 3.00,
                                    
                            if_else(crs_termcd - cohort == 30, 3.33, 
                            if_else(crs_termcd - cohort == 32, 3.66,
                            if_else(crs_termcd - cohort == 34, 4.00,
                            if_else(crs_termcd - cohort == 36, 3.66,
                            if_else(crs_termcd - cohort == 38, 4.00,
                                    
                            if_else(crs_termcd - cohort == 40, 4.33,
                            if_else(crs_termcd - cohort == 42, 4.66,
                            if_else(crs_termcd - cohort == 44, 5.00,
                            if_else(crs_termcd - cohort == 46, 4.66,
                            if_else(crs_termcd - cohort == 48, 5.00,
                                    
                            if_else(crs_termcd - cohort == 50, 5.33,
                            if_else(crs_termcd - cohort == 52, 5.66,
                            if_else(crs_termcd - cohort == 54, 6.00,
                            if_else(crs_termcd - cohort == 56, 5.66,
                            if_else(crs_termcd - cohort == 58, 6.00,
                            NaN)))))))))))))))))))))))))))))))

## Keep distinct rows by removing duplicates
df_crs <- distinct(df_crs)

## Select desired columns from data frame; also drops unneeded columns; also reorders columns
df_crs <- df_crs %>%
  select(st_id,
         crs_sbj,
         crs_catalog,
         crs_name,
         numgrade,
         numgrade_w,
         crs_retake,
         crs_term,
         crs_termcd,
         crs_semester,
         enrl_from_cohort
         )

## Write data frame to .csv file
write.csv(df_crs, file = "df_crs.csv", row.names = FALSE)


###################################################################################################
## PART 2. CLEAN COURSE LEVEL VARIABLES                                                          ##
## B. For each subject course (1 and 2), create data frame of only first time taking that course ##
## Note: This step selects down to only a single row per course, per student                     ##
###################################################################################################
## First Biology course (bio1)
df_crs_bio1 <- df_crs %>%
  filter(crs_sbj == "BS" & (crs_catalog == "161")) %>%
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group = TRUE) %>%
  mutate(crs_retake_num = row_number() - 1) %>%   ## Create variable to count each time course was taken by a student
  filter(crs_retake_num == 0)                     ## Filters for just the first time course was taken (i.e. was not a retake)

## Second Biology course (bio2)
df_crs_bio2 <- df_crs %>%
  filter(crs_sbj == "BS" & (crs_catalog == "162")) %>%
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group = TRUE) %>%
  mutate(crs_retake_num = row_number() - 1) %>%   ## Create variable to count each time course was taken by a student
  filter(crs_retake_num == 0)                     ## Filters for just the first time course was taken (i.e. was not a retake)

## First Chemistry course (chem1)
df_crs_chem1 <- df_crs %>%
  filter(crs_sbj == "CEM" & (crs_catalog == "141")) %>%
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group = TRUE) %>%
  mutate(crs_retake_num = row_number() - 1) %>%   ## Create variable to count each time course was taken by a student
  filter(crs_retake_num == 0)                     ## Filters for just the first time course was taken (i.e. was not a retake)

## Second Chemistry course (chem2)
df_crs_chem2 <- df_crs %>%
  filter(crs_sbj == "CEM" & (crs_catalog == "142")) %>%
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group = TRUE) %>%
  mutate(crs_retake_num = row_number() - 1) %>%   ## Create variable to count each time course was taken by a student
  filter(crs_retake_num == 0)                     ## Filters for just the first time course was taken (i.e. was not a retake)

## First Physics course (phys1)
df_crs_phys1 <- df_crs %>%
  filter(crs_sbj == "PHY" & (crs_catalog == "183")) %>%
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group = TRUE) %>%
  mutate(crs_retake_num = row_number() - 1) %>%   ## Create variable to count each time course was taken by a student
  filter(crs_retake_num == 0)                     ## Filters for just the first time course was taken (i.e. was not a retake)

## Second Physics course (phys2)
df_crs_phys2 <- df_crs %>%
  filter(crs_sbj == "PHY" & (crs_catalog == "184")) %>%
  group_by(st_id, crs_catalog) %>% 
  arrange(crs_term, .by_group = TRUE) %>%
  mutate(crs_retake_num = row_number() - 1) %>%   ## create variable to count each time course was taken by a student
  filter(crs_retake_num == 0)                     ## Filters for just the first time course was taken (i.e. was not a retake)

## Write data frames to .csv file
write.csv(df_crs_bio1,  file = "df_crs_bio1.csv",  row.names = FALSE)
write.csv(df_crs_bio2,  file = "df_crs_bio2.csv",  row.names = FALSE)
write.csv(df_crs_chem1, file = "df_crs_chem1.csv", row.names = FALSE)
write.csv(df_crs_chem2, file = "df_crs_chem2.csv", row.names = FALSE)
write.csv(df_crs_phys1, file = "df_crs_phys1.csv", row.names = FALSE)
write.csv(df_crs_phys2, file = "df_crs_phys2.csv", row.names = FALSE)


####################################################################################################################################
## PART 3. CLEAN AP LEVEL VARIABLES                                                                                               ##
## B.  For each AP subject, rename and generate/recode course level variables as needed to match common SEISMIC AP variable names ##
## Note: Taking highest (max) AP score received; single row per AP exam, per student                                              ##
####################################################################################################################################
## Create new student level variable data frame copied from FULL_SAMPLE
df_ap_bio <- FULL_SAMPLE

## Select desired columns from data frame; also drops unneeded columns; also reorders columns
df_ap_bio <- df_ap_bio %>%
  select(st_id,
         cohort,
         Atst_Type,
         apscore,
         crs_sbj,
         crs_catalog,
         numgrade)

## Create aptaker variable by checking if Atst_Type is NA (which means student didn't take an AP exam)
df_ap_bio <- df_ap_bio %>%
  mutate(aptaker = ifelse(is.na(Atst_Type), 0, 1))

## Create eligible_to_skip variable based on if AP exam score was high enough to earn credit; Non-AP test takers are computed as "NA" which is changed to 0 in the next step
df_ap_bio <- df_ap_bio %>%
  mutate(eligible_to_skip = ifelse(Atst_Type == "AP20" & as.numeric(as.character(apscore)) >= 4, 1, 0))

## Change eligible_to_skip = NA to eligible_to_skip = 0; this combines all Non-AP takers with AP takers who scored below credit earning threshold
df_ap_bio <- df_ap_bio %>%
  mutate(eligible_to_skip = ifelse(is.na(eligible_to_skip), 0, eligible_to_skip))

## Change the non-numeric grades to "NA" so that numgrade variable becomes type numeric
df_ap_bio$numgrade <- na_if(df_ap_bio$numgrade, "W")  # Withdrawn
df_ap_bio$numgrade <- na_if(df_ap_bio$numgrade, "V")  # Visitor
df_ap_bio$numgrade <- na_if(df_ap_bio$numgrade, "U")  # Unfinished Work
df_ap_bio$numgrade <- na_if(df_ap_bio$numgrade, "I")  # Incomplete
df_ap_bio$numgrade <- na_if(df_ap_bio$numgrade, "ET") # Extension

## Create tookcourse variable for who took first course (BS 161) in sequence AND didn't withdraw from course (i.e. got a grade)
df_ap_bio <- df_ap_bio %>%
  mutate(tookcourse = ifelse(crs_sbj == "BS" & crs_catalog == "161" & !is.na(numgrade), 1, 0))

## Create tookcourse_2 variable for who took second course (BS 162) in sequence AND didn't withdraw from course (i.e. got a grade)
df_ap_bio <- df_ap_bio %>%
  mutate(tookcourse_2 = ifelse(crs_sbj == "BS" & crs_catalog == "162" & !is.na(numgrade), 1, 0))

## Create apyear using semester code (i.e. 1134) changed to a number for the year (i.e. 2013); apyear = matriculation year
df_ap_bio <- mutate(df_ap_bio, apyear = if_else(cohort == 1134, 2013, #2013: Fall
                                        if_else(cohort == 1136, 2013, #2014: Spring
                                        if_else(cohort == 1142, 2013, #2014: Summer
                                                
                                        if_else(cohort == 1144, 2014, #2014: Fall
                                        if_else(cohort == 1146, 2014, #2015: Spring
                                        if_else(cohort == 1152, 2014, #2015: Summer
                                                
                                        if_else(cohort == 1154, 2015, #2015: Fall
                                        if_else(cohort == 1156, 2015, #2016: Spring
                                        if_else(cohort == 1162, 2015, #2016: Summer
                                                
                                        if_else(cohort == 1164, 2016, #2016: Fall
                                        if_else(cohort == 1166, 2016, #2017: Spring
                                        if_else(cohort == 1172, 2016, #2017: Summer
                                                
                                        if_else(cohort == 1174, 2017, #2017: Fall
                                        if_else(cohort == 1176, 2017, #2018: Spring
                                        if_else(cohort == 1182, 2017, #2018: Summer
                                                
                                        if_else(cohort == 1184, 2018, #2018: Fall
                                        if_else(cohort == 1186, 2018, #2019: Spring
                                        if_else(cohort == 1192, 2018, #2019: Summer
                                        
                                        if_else(cohort == 1194, 2019, 2222 #2019: Fall
                                        ))))))))))))))))))))

## Filter out non-bio AP exams, for this bio specific data frame, by including only AP20 and SAT exams
 df_ap_bio <- df_ap_bio %>%
   filter(Atst_Type == "AP20" | is.na(Atst_Type))

## Making sure tookcourse and tookcourse_2 variables show correct so distinct() can work correctly
 df_ap_bio <- df_ap_bio %>%
   group_by(st_id) %>%
   mutate(tookcourse = max(tookcourse)) %>%
   mutate(tookcourse_2 = max(tookcourse_2))

## Create apscore_full variable used in shared analysis code; apscore_full is apscore but with a 0 score if student didn't take an AP exam (rather than an "NA")
df_ap_bio <- df_ap_bio %>%
  mutate(apscore_full = ifelse(is.na(apscore), 0, apscore))

## Select desired columns from data frame; also drops unneeded columns; also reorders columns
df_ap_bio <- df_ap_bio %>%
  select(st_id,
         aptaker,
         Atst_Type,
         eligible_to_skip,
         tookcourse,
         tookcourse_2,
         apyear,
         apscore,
         apscore_full)

## Keep distinct rows by removing duplicates
df_ap_bio <- distinct(df_ap_bio)

## Write data frame to .csv file
write.csv(df_ap_bio, file = "df_ap_bio.csv", row.names = FALSE)

####################################################################################################################################

## Create new student level variable data frame copied from FULL_SAMPLE
df_ap_chem <- FULL_SAMPLE

## Select desired columns from data frame; also drops unneeded columns; also reorders columns
df_ap_chem <- df_ap_chem %>%
  select(st_id,
         cohort,
         Atst_Type,
         apscore,
         crs_sbj,
         crs_catalog,
         numgrade)

## Create aptaker variable by checking if Atst_Type is NA (which means student didn't take an AP exam)
df_ap_chem <- df_ap_chem %>%
  mutate(aptaker = ifelse(is.na(Atst_Type), 0, 1))

## Create eligible_to_skip variable based on if AP exam score was high enough to earn credit; Non-AP test takers are computed as "NA" which is changed to 0 in the next step
df_ap_chem <- df_ap_chem %>%
  mutate(eligible_to_skip = ifelse(Atst_Type == "AP25" & as.numeric(as.character(apscore)) >= 3, 1, 0))

## Change eligible_to_skip = NA to eligible_to_skip = 0; this combines all Non-AP takers with AP takers who scored below credit earning threshold
df_ap_chem <- df_ap_chem %>%
  mutate(eligible_to_skip = ifelse(is.na(eligible_to_skip), 0, eligible_to_skip))

## Change the non-numeric grades to "NA" so that numgrade variable becomes type numeric
df_ap_chem$numgrade <- na_if(df_ap_chem$numgrade, "W")  # Withdrawn
df_ap_chem$numgrade <- na_if(df_ap_chem$numgrade, "V")  # Visitor
df_ap_chem$numgrade <- na_if(df_ap_chem$numgrade, "U")  # Unfinished Work
df_ap_chem$numgrade <- na_if(df_ap_chem$numgrade, "I")  # Incomplete
df_ap_chem$numgrade <- na_if(df_ap_chem$numgrade, "ET") # Extension

## Create tookcourse variable for who took first course (CEM 141) in sequence AND didn't withdraw from course (i.e. got a grade)
df_ap_chem <- df_ap_chem %>%
  mutate(tookcourse = ifelse(crs_sbj == "CEM" & crs_catalog == "141" & !is.na(numgrade), 1, 0))

## Create tookcourse_2 variable for who took second course (CEM 142) in sequence AND didn't withdraw from course (i.e. got a grade)
df_ap_chem <- df_ap_chem %>%
  mutate(tookcourse_2 = ifelse(crs_sbj == "CEM" & crs_catalog == "142" & !is.na(numgrade), 1, 0))

## Create apyear using semester code (i.e. 1134) changed to a number for the year (i.e. 2013); apyear = matriculation year
df_ap_chem <- mutate(df_ap_chem, apyear = if_else(cohort == 1134, 2013, #2013: Fall
                                          if_else(cohort == 1136, 2013, #2014: Spring
                                          if_else(cohort == 1142, 2013, #2014: Summer
                                          
                                          if_else(cohort == 1144, 2014, #2014: Fall
                                          if_else(cohort == 1146, 2014, #2015: Spring
                                          if_else(cohort == 1152, 2014, #2015: Summer
                                          
                                          if_else(cohort == 1154, 2015, #2015: Fall
                                          if_else(cohort == 1156, 2015, #2016: Spring
                                          if_else(cohort == 1162, 2015, #2016: Summer
                                          
                                          if_else(cohort == 1164, 2016, #2016: Fall
                                          if_else(cohort == 1166, 2016, #2017: Spring
                                          if_else(cohort == 1172, 2016, #2017: Summer
                                          
                                          if_else(cohort == 1174, 2017, #2017: Fall
                                          if_else(cohort == 1176, 2017, #2018: Spring
                                          if_else(cohort == 1182, 2017, #2018: Summer
                                          
                                          if_else(cohort == 1184, 2018, #2018: Fall
                                          if_else(cohort == 1186, 2018, #2019: Spring
                                          if_else(cohort == 1192, 2018, #2019: Summer
                                                  
                                          if_else(cohort == 1194, 2019, 2222 #2019: Fall
                                          ))))))))))))))))))))

## Filter out non-chem AP exams, for this chem specific data frame, by including only AP25 and SAT exams
 df_ap_chem <- df_ap_chem %>%
   filter(Atst_Type == "AP25" | is.na(Atst_Type))

## Making sure tookcourse and tookcourse_2 variables show correct so distinct() can work correctly
df_ap_chem <- df_ap_chem %>%
  group_by(st_id) %>%
  mutate(tookcourse = max(tookcourse)) %>%
  mutate(tookcourse_2 = max(tookcourse_2))

## Create apscore_full variable used in shared analysis code; apscore_full is apscore but with a 0 score if student didn't take an AP exam (rather than an "NA")
df_ap_chem <- df_ap_chem %>%
  mutate(apscore_full = ifelse(is.na(apscore), 0, apscore))

## Select desired columns from data frame; also drops unneeded columns; also reorders columns
df_ap_chem <- df_ap_chem %>%
  select(st_id,
         aptaker,
         Atst_Type,
         eligible_to_skip,
         tookcourse,
         tookcourse_2,
         apyear,
         apscore,
         apscore_full)

## Keep distinct rows by removing duplicates
df_ap_chem <- distinct(df_ap_chem)

## Write data frame to .csv file
write.csv(df_ap_chem, file = "df_ap_chem.csv", row.names = FALSE)

####################################################################################################################################

## Create new student level variable data frame copied from FULL_SAMPLE
df_ap_phys <- FULL_SAMPLE

## Select desired columns from data frame; also drops unneeded columns; also reorders columns
df_ap_phys <- df_ap_phys %>%
  select(st_id,
         cohort,
         Atst_Type,
         apscore,
         crs_sbj,
         crs_catalog,
         numgrade)

## Create aptaker variable by checking if Atst_Type is NA (which means student didn't take an AP exam)
df_ap_phys <- df_ap_phys %>%
  mutate(aptaker = ifelse(is.na(Atst_Type), 0, 1))

## Create eligible_to_skip variable based on if AP exam score was high enough to earn credit; Non-AP test takers are computed as "NA" which is changed to 0 in the next step
df_ap_phys <- df_ap_phys %>%
  mutate(eligible_to_skip = ifelse(Atst_Type == "AP80" & as.numeric(as.character(apscore)) >= 4, 1,
                            ifelse(Atst_Type == "AP82" & as.numeric(as.character(apscore)) >= 4, 1, 0)))

## Change eligible_to_skip = NA to eligible_to_skip = 0; this combines all Non-AP takers with AP takers who scored below credit earning threshold
df_ap_phys <- df_ap_phys %>%
  mutate(eligible_to_skip = ifelse(is.na(eligible_to_skip), 0, eligible_to_skip))

## Change the non-numeric grades to "NA" so that numgrade variable becomes type numeric
df_ap_phys$numgrade <- na_if(df_ap_phys$numgrade, "W")  # Withdrawn
df_ap_phys$numgrade <- na_if(df_ap_phys$numgrade, "V")  # Visitor
df_ap_phys$numgrade <- na_if(df_ap_phys$numgrade, "U")  # Unfinished Work
df_ap_phys$numgrade <- na_if(df_ap_phys$numgrade, "I")  # Incomplete
df_ap_phys$numgrade <- na_if(df_ap_phys$numgrade, "ET") # Extension

## Create tookcourse variable for who took first course (PHY 183) in sequence AND didn't withdraw from course (i.e. got a grade)
df_ap_phys <- df_ap_phys %>%
  mutate(tookcourse = ifelse(crs_sbj == "PHY" & crs_catalog == "183" & !is.na(numgrade), 1, 0))

## Create tookcourse_2 variable for who took second course (PHY 184) in sequence AND didn't withdraw from course (i.e. got a grade)
df_ap_phys <- df_ap_phys %>%
  mutate(tookcourse_2 = ifelse(crs_sbj == "PHY" & crs_catalog == "184" & !is.na(numgrade), 1, 0))

## Create apyear using semester code (i.e. 1134) changed to a number for the year (i.e. 2013); apyear = matriculation year
df_ap_phys <- mutate(df_ap_phys, apyear = if_else(cohort == 1134, 2013, #2013: Fall
                                          if_else(cohort == 1136, 2013, #2014: Spring
                                          if_else(cohort == 1142, 2013, #2014: Summer
                                          
                                          if_else(cohort == 1144, 2014, #2014: Fall
                                          if_else(cohort == 1146, 2014, #2015: Spring
                                          if_else(cohort == 1152, 2014, #2015: Summer
                                          
                                          if_else(cohort == 1154, 2015, #2015: Fall
                                          if_else(cohort == 1156, 2015, #2016: Spring
                                          if_else(cohort == 1162, 2015, #2016: Summer
                                          
                                          if_else(cohort == 1164, 2016, #2016: Fall
                                          if_else(cohort == 1166, 2016, #2017: Spring
                                          if_else(cohort == 1172, 2016, #2017: Summer
                                          
                                          if_else(cohort == 1174, 2017, #2017: Fall
                                          if_else(cohort == 1176, 2017, #2018: Spring
                                          if_else(cohort == 1182, 2017, #2018: Summer
                                          
                                          if_else(cohort == 1184, 2018, #2018: Fall
                                          if_else(cohort == 1186, 2018, #2019: Spring
                                          if_else(cohort == 1192, 2018, #2019: Summer
                                                  
                                          if_else(cohort == 1194, 2019, 2222 #2019: Fall
                                          ))))))))))))))))))))

## Filter out non-phy AP exams, for this phys specific data frame, by including only AP80, AP82, and SAT exams
 df_ap_phys <- df_ap_phys %>%
   filter(Atst_Type == "AP80" | Atst_Type == "AP82" | is.na(Atst_Type))

## Making sure tookcourse and tookcourse_2 variables show correct so distinct() can work correctly
df_ap_phys <- df_ap_phys %>%
  group_by(st_id) %>%
  mutate(tookcourse = max(tookcourse)) %>%
  mutate(tookcourse_2 = max(tookcourse_2))

## Create apscore_full variable used in shared analysis code; apscore_full is apscore but with a 0 score if student didn't take an AP exam (rather than an "NA")
df_ap_phys <- df_ap_phys %>%
  mutate(apscore_full = ifelse(is.na(apscore), 0, apscore))

## Select desired columns from data frame; also drops unneeded columns; also reorders columns
df_ap_phys <- df_ap_phys %>%
  select(st_id,
         aptaker,
         Atst_Type,
         eligible_to_skip,
         tookcourse,
         tookcourse_2,
         apyear,
         apscore,
         apscore_full)

## Keep distinct rows by removing duplicates
df_ap_phys <- distinct(df_ap_phys)

## Write data frame to .csv file
write.csv(df_ap_phys, file = "df_ap_phys.csv", row.names = FALSE)


###########################################################################################################################
## PART 4. CREATE STACKED DATASET                                                                                        ##
## A. Join previously generated dataframes (Student, Course1, Course2, and AP) for each course subject (BIO, CHEM, PHYS) ##
## Note: Include new variable: “discipline” as flag for each subject BIO CHEM PHYS                                       ##
###########################################################################################################################
## Import data
# df_std <- read.csv("df_std.csv")
# df_crs <- read.csv("df_crs.csv")
# 
# df_crs_bio1 <- read.csv("df_crs_bio1.csv")
# df_crs_bio2 <- read.csv("df_crs_bio2.csv")
# df_ap_bio <- read.csv("df_ap_bio.csv")
# 
# df_crs_chem1 <- read.csv("df_crs_chem1.csv")
# df_crs_chem2 <- read.csv("df_crs_chem2.csv")
# df_ap_chem <- read.csv("df_ap_chem.csv")
# 
# df_crs_phys1 <- read.csv("df_crs_phys1.csv")
# df_crs_phys2 <- read.csv("df_crs_phys2.csv")
# df_ap_phys <- read.csv("df_ap_phys.csv")

## Join previously generated data frames (Student, Course1, Course2, and AP) for each course subject (BIO, CHEM, PHYS)
df_bio <- df_std %>%
  right_join(df_crs_bio2, by = "st_id") %>%
  full_join(df_crs_bio1, by = "st_id") %>%
  full_join(df_ap_bio, by = "st_id") %>%
  mutate(discipline = "BIO") %>%
  mutate(skipped_course = ifelse(tookcourse == 0 & tookcourse_2 == 1 & eligible_to_skip == 1, 1, 0)) %>% ## Wondering about ppl who were not skip_eligible (No passing AP score) but still skipped 1st course somehow
  select(discipline,
         st_id:hsgpa,
         crs_sbj.x:crs_retake_num.x,
         crs_sbj.y:crs_retake_num.y,
         apyear,
         aptaker,
         Atst_Type,
         apscore,
         apscore_full,
         eligible_to_skip, 
         tookcourse,
         tookcourse_2,
         skipped_course)

## Join previously generated data frames (Student, Course1, Course2, and AP) for each course subject (BIO, CHEM, PHYS)
df_chem <- df_std %>%
  right_join(df_crs_chem2, by = "st_id") %>%
  full_join(df_crs_chem1, by = "st_id") %>%
  full_join(df_ap_chem, by = "st_id") %>%
  mutate(discipline = "CHEM") %>%
  mutate(skipped_course = ifelse(tookcourse == 0 & tookcourse_2 == 1 & eligible_to_skip == 1, 1, 0)) %>%
  select(discipline,
         st_id:hsgpa,
         crs_sbj.x:crs_retake_num.x,
         crs_sbj.y:crs_retake_num.y, 
         apyear,
         aptaker,
         Atst_Type,
         apscore,
         apscore_full,
         eligible_to_skip, 
         tookcourse,
         tookcourse_2,
         skipped_course)

## Join previously generated data frames (Student, Course1, Course2, and AP) for each course subject (BIO, CHEM, PHYS)
df_phys <- df_std %>%
  right_join(df_crs_phys2, by = "st_id") %>%
  full_join(df_crs_phys1, by = "st_id") %>%
  full_join(df_ap_phys, by = "st_id") %>%
  mutate(discipline = "PHYS") %>%
  mutate(skipped_course = ifelse(tookcourse == 0 & tookcourse_2 == 1 & eligible_to_skip == 1, 1, 0)) %>%
  select(discipline,
         st_id:hsgpa,
         crs_sbj.x:crs_retake_num.x,
         crs_sbj.y:crs_retake_num.y,
         apyear,
         aptaker,
         Atst_Type,
         apscore,
         apscore_full,
         eligible_to_skip,
         tookcourse,
         tookcourse_2,
         skipped_course)

#########################################################################################################################################
## PART 4. CREATE STACKED DATASET                                                                                                      ##
## B. Stack complete dataframes for each course subject (BIO, CHEM, PHYS), including "discipline" indicator variable                   ##
## Note: Should end up with dataset structured like this Example Dataset                                                               ##
##  https://docs.google.com/spreadsheets/d/1Sj5kaFNGUkBhRoOH3cIPm-97UEBZmcFkbKGjzBbKWc0/edit?usp=drive_open&ouid=118183464940790632947 ##
#########################################################################################################################################
# Stacked dataframe with Bio, Chem, Phys
df_clean <- rbind(df_bio, df_chem, df_phys)

## Rename these variables to match what is needed in SharedAnalysis_r2.R file
df_clean <- df_clean %>%
  rename_at(vars(ends_with(".x")), 
          ~(str_replace(., ".x", "_2"))) %>%
  rename_at(vars(ends_with(".y")), 
          ~(str_replace(., ".y", ""))) %>%
  mutate(apyear = if_else(discipline == "CHEM", cohort - 1, cohort)) %>%
  distinct()
  
## Write data frame to csv
write.csv(df_clean, file = "df_clean.csv", row.names = FALSE)


###############################################################################
## DATA ANALYSIS                                                             ##
## GO TO SharedAnalysis_r2.R for this.                                       ## 
###############################################################################
