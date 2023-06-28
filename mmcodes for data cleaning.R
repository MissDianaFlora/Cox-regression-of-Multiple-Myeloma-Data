#Importing raw MM data in CSV format
library(readr)
mmdata <- read_csv("MMdata.csv")
View(mmdata)

#---------------------------------------------------------------------DATA CLEANING--------------------------------------------------------------------------------------------
#packages for Data cleaning
library(dplyr)
library(tidyr)
library(tidyselect)
library(lubridate)
library(tidyverse)

#-------------------------Clean the diagnosis date data so that we can subset the data set to remain only with data variables we need------------------------------------------
#------------------------------------------changing date variables into date format-------------------------------------------------------------------------------------------
mmdata$E_Date = as.Date(mmdata$E_Date, format = "%m/%d/%Y")
mmdata$Iv_Date=as.Date(mmdata$Iv_Date, format = "%m/%d/%Y")
mmdata$`Death Date`=as.Date(mmdata$`Death Date`, format = "%m/%d/%Y")
mmdata$Date_diagnosis= as.Date(mmdata$Date_diagnosis, format = "%m/%d/%Y")
mmdata$Date_of_birth= as.Date(mmdata$Date_of_birth,format = "%d/%m%Y")
mmdata$Date_of_birth <- as.Date(paste(mmdata$Year_of_birth, "01", "01", sep = "-"))

#---------------------------------------------------------------------checking for missing dates data--------------------------------------------------------------------------
which(is.na(mmdata$Iv_Date))
which(is.na(mmdata$E_Date))
which(is.na(mmdata$Encounter_Type))
which(is.na(mmdata$Date_diagnosis))
which(is.na(mmdata$`Death Date`))

#--------------------------------------------------------------------replacing the missing initial encounter date value-------------------------------------------------------
mmdata=mmdata%>%
  group_by(Hosp_ID)%>%
  mutate(Iv_Date=ifelse(is.na(Iv_Date), min(E_Date, na.rm = TRUE), Iv_Date))%>%
  mutate(Iv_Date= as.Date(Iv_Date, origin = "1970-01-01"))

#-----------replacing missing diagnosis date in the other encounters since data is longitudinal and diagnosis date is the same for each patient across the encounters---------
mmdata=mmdata %>%
  group_by(Hosp_ID) %>%
  mutate(Date_diagnosis = ifelse(is.na(Date_diagnosis), min(Date_diagnosis, na.rm = TRUE), Date_diagnosis)) %>%
  mutate(Date_diagnosis = as.Date(Date_diagnosis, origin = "1970-01-01"))
summary(is.na(mmdata$Date_diagnosis))
 

#---------------replacing diagnosis date for a patient with missing diagnosis date, with the date from the file that was not entered------------------------------------------
newdate = as.Date("2015-05-27")
mmdata=mmdata%>%
  group_by %>%
  mutate(Date_diagnosis = ifelse(Hosp_ID == 397477, newdate,Date_diagnosis)) %>%
  ungroup()
mmdata$Date_diagnosis= as.Date(mmdata$Date_diagnosis, origin = "1970-01-01")


#------------------------------checking patients with different diagnosis dates for one patient-------------------------------------------------------------------------------
mmdata <- mmdata %>%
  group_by(Hosp_ID)

#--------------------------------------------------------Filter patients with a unique diagnosis date--------------------------------------------------------------------------
unique_diagnosis_patients <- mmdata %>%
  filter(n_distinct(Date_diagnosis) == 1) %>%
  distinct(Hosp_ID) %>%
  ungroup()

#-------------------------------------------Filter patients without a unique diagnosis date-----------------------------------------------------------------------------------
different_diagnosis_patients <- mmdata %>%
  filter(n_distinct(Date_diagnosis) > 1) %>%
  distinct(Hosp_ID) %>%
  ungroup()

#extract to word for the data entry team to update the correct date
write.csv(different_diagnosis_patients, "differentdiagnosis date.csv", row.names = FALSE)
#---------------------------replace diagnosis date for those with different diagnosis date with the minimum diagnosis date of each patient-------------------------------------
mmdata=mmdata %>%
  group_by(Hosp_ID) %>%
  mutate(Date_diagnosis = ifelse(!is.na(Date_diagnosis), min(Date_diagnosis, na.rm = TRUE), Date_diagnosis)) %>%
  mutate(Date_diagnosis = as.Date(Date_diagnosis, origin = "1970-01-01"))
summary(is.na(mmdata$Date_diagnosis))


#----------------------------------subset data to remain with data for patients diagnosed between 01/01/2020 to 31/12/2022----------------------------------------------------
mmdata <- mmdata %>%
  filter(Date_diagnosis >= as.Date("2020-01-01") & Date_diagnosis <= as.Date("2022-12-31"))

#--------------------------------------------dropping variables that will not be involved in the analysis---------------------------------------------------------------------
mmdata= mmdata%>%
  select(-StartTreatment, -ContinueTreatment, -ChemoReinitiated, -StartTreatment_Supportivemed, -ContinueTreatment_SupportiveMed, -Chemo_SupportiveMed,
         -Occupation, -patID,-Visit_Type,-Vitals,-BMA,-BSA,-Diagnosis,-MM_Subtype,-Plasmacytoma_Subtype, -XRay,-CTScan,-MRI,-Ultrasound,
         -Spep, -Spep_date,-Upep,-Upep_date,-BMA,-BMA_date,-SFLC,-SFLC_date,-LiverFunction_test,-LFT_date,-CBC,-CBC_date,-RenalFunction_date, -RenalFunctionAlbumin,
         -LabTest_Ordered,-OtherLabTests,-ImagingTestsOrdered,-OtherImagingTests,-NextVisit_date,-SurvivalStatus,-DeceasedDate, -CompletedBy, -Year_of_birth, -Occupation)

#--------------------------------------------Continue with replacing the missing data------------------------------------------------------------------------------------------
#--------------------------------------------replace missing encounter type---------------------------------------------------------------------------------------------------
mmdata=mmdata%>%
  group_by(Hosp_ID)%>%
  mutate(Encounter_Type= ifelse(is.na(Encounter_Type), "Return", Encounter_Type))

#---------------------------------------------------------Checking for duplicate Encounter date---------------------------------------------------------------------------------------------
duplicates <- mmdata %>%
  group_by(Hosp_ID, Pat_Name) %>%
  filter(duplicated(E_Date))


#----------------------------------------------------------------------------remove duplicate encounter date--------------------------------------------------------------------------------
mmdata <- mmdata %>%
  group_by(Hosp_ID, Pat_Name) %>%
  distinct(E_Date, .keep_all = TRUE)

#-----------------------------------------------Checking if the duplicates have been removed--------------------------------------------------------------------------------
duplicate1 = mmdata%>%
  arrange(Pat_Name, E_Date) %>%
  group_by(Hosp_ID, Pat_Name)%>%
  filter(duplicated(E_Date))

#-------------------------------------------------checking duplicates of initial encounter in each patient------------------------------------------------
duplicates2 <- mmdata %>%
  group_by(Hosp_ID, Pat_Name) %>%
  filter(duplicated(Encounter_Type) & Encounter_Type == "Initial")


#------------------------------------------------------merging the different deceased formats---------------------------------------------------------------------------------
mmdata$Survival[mmdata$Survival == "deceased" | mmdata$Survival == "DECEASED"] = "Deceased"

#---------------------create a new survival variable with 0 for alive/ltfu patients and 1 for deceased patients----------------------------------------------------------------
mmdata$survival_status <- ifelse(mmdata$Survival == "Alive" | mmdata$Survival == "LTFU", 0, 1)
table(mmdata$survival_status)

#-------------------------------creating a new variable that counts number of encounters per patient---------------------------------------------------------------------------------- 
mmdata=mmdata%>%
  arrange(Hosp_ID, E_Date) %>%
  group_by(Hosp_ID) %>%
  mutate(record_count = row_number())

#--------------------------------------latest encounter in patients--------------------------------------------------------------------------------------------------------
mmdata=mmdata %>%
  group_by(Hosp_ID) %>%
  mutate(latest_encounterdate =  max(E_Date, na.rm = TRUE)) %>%
  mutate(latest_encounterdate = as.Date(latest_encounterdate, origin = "1970-01-01"))

#--------------------------------------replacing last encounter with death date for those with death date-----------------------------------------------------------------
mmdata$latest_encounterdate = ifelse (!is.na(mmdata$`Death Date`), mmdata$`Death Date`, mmdata$latest_encounterdate)
mmdata$latest_encounterdate = as.Date(mmdata$latest_encounterdate, origin = "1970-01-01")


#----------------------------calculate age using DOB and Date of Diagnosis----------------------------------------------
library(lubridate)
mmdata= mmdata%>%
  mutate(age = interval(Date_of_birth, Date_diagnosis) %/% years(1))

#-------------------------------------------------CRAB features---------------------------------------------------------------------------------------------------
#renal failure serum creatinine >177, Replacing creatinine value with values at least up-to 3 encounters if it's missing in initial
#mmdata=mmdata%>%
  #group_by(Hosp_ID) %>%
  #arrange(E_Date) %>%
  #mutate(creatinine = ifelse(is.na(`Serum_creatinine_mmol/l`) & Encounter_Type == "Initial",
                             #ifelse(!is.na(`Serum_creatinine_mmol/l`[Encounter_Type == "Return"][1]), `Serum_creatinine_mmol/l`[Encounter_Type== "Return"][1],
                                    #ifelse(!is.na(`Serum_creatinine_mmol/l`[Encounter_Type== "Return"][2]), `Serum_creatinine_mmol/l`[Encounter_Type == "Return"][2],
                                           #ifelse(!is.na(`Serum_creatinine_mmol/l`[Encounter_Type == "Return"][3]), `Serum_creatinine_mmol/l`[Encounter_Type == "Return"][3],
                                                  #ifelse(!is.na(`Serum_creatinine_mmol/l`[Encounter_Type == "Return"][4]), `Serum_creatinine_mmol/l`[Encounter_Type== "Return"][4], NA)))),
                             #`Serum_creatinine_mmol/l`)) %>%
  #ungroup()
#-------------------------------------------------------Export the data that has the new creatinine variable----------------------------------------------------
#write.csv(mmdata, "creatinine.csv", row.names = FALSE)

#grouping the creatinine variable
#mmdata <- mmdata %>%
  #mutate(renalfailure = ifelse(Encounter_Type == "Initial" & creatinine > 177, "> 177",
                               #ifelse(Encounter_Type == "Initial" & creatinine <= 177, "<= 177", NA)))
#write.csv(mmdata, "renal failure.csv", row.names = FALSE)
#--------------------------------------------------------------replace creatinine with most recent value---------------------------------------------------------------------------
mmdata <- mmdata %>%
  arrange(Hosp_ID, E_Date) %>%
  group_by(Hosp_ID) %>%
  mutate(creatinine = if_else(Encounter_Type == "Initial" & is.na(`Serum_creatinine_mmol/l`) & difftime(E_Date, lag(E_Date), units = "days") < 7, lead(`Serum_creatinine_mmol/l`),
                                             `Serum_creatinine_mmol/l`)) %>%ungroup()

#-----------------------------------------grouping creatinine to get renal failure----------------------------------------------------------------------------------
mmdata = mmdata%>%
  mutate(renalfailure = ifelse(creatinine > 177, "> 177", ifelse(creatinine <= 177, "<=177", NA)))
table(mmdata$Encounter_Type, mmdata$renalfailure)

#----------------------------------------------------------------------------------Anemia---------------------------------------------------------------------------
mmdata = mmdata%>%
  mutate(anemia = ifelse(`HgB_g/dl` < 10, "< 10", ifelse(`HgB_g/dl` >= 10, ">=10", NA)))
table(mmdata$Encounter_Type, mmdata$anemia)

#-----------------------------------------------------------------------corrected calcium----------------------------------------------------------------------------
mmdata <- mmdata %>%
  mutate(corrected_calcium = ifelse(`SerumAlbumin_g/l` < 40, `SerumCalcium_unmol/l` + 0.02 * (40 - `SerumAlbumin_g/l`),
                                    ifelse(`SerumAlbumin_g/l` > 45, `SerumCalcium_unmol/l` - 0.02 * (`SerumAlbumin_g/l` - 45), `SerumCalcium_unmol/l`)))

write.csv(mmdata, "mmdata1.csv", row.names = FALSE)

#--------------------------------------------------------------------------Hypercalcemia-----------------------------------------------------------------------------
mmdata = mmdata%>%
  mutate(hypercalcemia = ifelse(corrected_calcium > 2.75, "Yes", "No"))
table(mmdata$Encounter_Type, mmdata$hypercalcemia)


#--------------------------------------------------------------------CHIEF COMPLAINTS------------------------------------------------------------------------------
#-----------------------------------------------------replacing the codes with the categories---------------------------------------------------------------------
mmdata$chief_complaint <- gsub("14", "None", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("13", "Others", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("12", "Swelling", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("11", "Bleeding", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("10", "Paralysis", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("9", "Vomiting", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("8", "Nausea", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("7", "Numbness", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("6", "Weight Loss", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("5", "Decreased urine output", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("4", "Frequent Infections", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("3", "Fatigue", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("2", "Diziness", mmdata$chief_complaint)
mmdata$chief_complaint <- gsub("1", "Bone Pain", mmdata$chief_complaint)


#--------------------------------CHIEF COMPLAINTS------------------------------------------
#creating chief complaints variables
mmdata = mmdata %>% mutate(`Bone Pain` = ifelse(grepl("(?i)bone pain|Bone Pain|Bone pain", chief_complaint), "Yes", "No"))
mmdata = mmdata %>% mutate(`Fatigue` = ifelse(grepl("(?i)fatigue|fatique|Fatigue|Fatique", chief_complaint), "Yes", "No"))
mmdata = mmdata %>% mutate(`Frequent Infections` = ifelse(grepl("(?i)frequent infections", chief_complaint), "Yes", "No"))
mmdata = mmdata %>% mutate(`Decreased Urine Output` = ifelse(grepl("(?i)decreased urine output|Decreased Urine Output", chief_complaint), "Yes", "No"))
mmdata = mmdata %>% mutate(`Weight Loss` = ifelse(grepl("(?i)weight loss|Weight Loss", chief_complaint), "Yes", "No"))
mmdata = mmdata %>% mutate(`Numbness` = ifelse(grepl("(?i)Numbness", chief_complaint), "Yes", "No"))
mmdata = mmdata %>% mutate(`Nausea` = ifelse(grepl("(?i)nausea|Nausea", chief_complaint), "Yes", "No"))
mmdata = mmdata %>% mutate(`Vomiting` = ifelse(grepl("(?i)vomiting|vommiting|Vomiting|Vommiting", chief_complaint), "Yes", "No"))
mmdata = mmdata %>% mutate(`Paralysis` = ifelse(grepl("(?i)Paralysis|paralysis", chief_complaint), "Yes", "No"))
mmdata = mmdata %>% mutate(`Bleeding` = ifelse(grepl("(?i)Bleeding|bleeding", chief_complaint), "Yes", "No"))
mmdata = mmdata %>% mutate(`Swelling` = ifelse(grepl("(?i)swelling|Swelling", chief_complaint), "Yes", "No"))
mmdata = mmdata %>% mutate(`Others chiefcomplaint` = ifelse(grepl("(?i)others|Others|Other|other", chief_complaint), "Yes", "No"))
mmdata = mmdata %>% mutate(`Diziness` = ifelse(grepl("(?i)diziness|Diziness|Dizziness|dizziness", chief_complaint), "Yes", "No"))

#-------------------------------------------------------------------------summarize the chief complaints----------------------------------------------------------
table(mmdata$Vomiting)
table(mmdata$`Bone Pain`)
table(mmdata$Fatigue)
table(mmdata$`Frequent Infections`)
table(mmdata$`Weight Loss`)
table(mmdata$Numbness)
table(mmdata$Nausea)
table(mmdata$Paralysis)
table(mmdata$Bleeding)
table(mmdata$Swelling)
table(mmdata$Diziness)
table(mmdata$`Others chiefcomplaint`)

#----------------------------------------------------------------------------pain site variable---------------------------------------------------------------------
mmdata$Pain_site <- gsub("5", "None", mmdata$Pain_site)
mmdata$Pain_site = gsub("4", "Upper Limb", mmdata$Pain_site)
mmdata$Pain_site = gsub("3", "Lower Limb", mmdata$Pain_site)
mmdata$Pain_site = gsub("2", "Back", mmdata$Pain_site)
mmdata$Pain_site = gsub("1", "Chest", mmdata$Pain_site)

#---------------------------------------------------------------creating new pain site variables-------------------------------------------------------------------- 
mmdata = mmdata %>% mutate(`Back` = ifelse(grepl("(?i)Back pain|back pain|Back Pain|Back", Pain_site), "Yes", "No"))
mmdata = mmdata %>% mutate(`Chest` = ifelse(grepl("(?i)chest|Chest|Chest pain|Chest Pain", Pain_site), "Yes", "No"))
mmdata = mmdata %>% mutate(`Lower Limb` = ifelse(grepl("(?i)Lower Limb|Lower limb|Lower limp|Lower Limp", Pain_site), "Yes", "No"))
mmdata = mmdata %>% mutate(`Upper Limb` = ifelse(grepl("(?i)Upper Limb|Upper limb|Upper limp|Upper Limp", Pain_site), "Yes", "No"))
mmdata = mmdata %>% mutate(`no pain site` = ifelse(grepl("(?i)Upper Limb|Upper limb|Upper limp|Upper Limp", Pain_site), "Yes", "No"))


#-----------------------------------------tables for the new pain site variables-----------------------------------------------------------------------------------
table(mmdata$Back)
table(mmdata$Chest)
table(mmdata$`Lower Limb`)
table(mmdata$`Upper Limb`)


#---------------------------------------------------------------------signs and symptoms-----------------------------------------------------------------------------
mmdata$Signs_Symptoms<- gsub("5", "None", mmdata$Signs_Symptoms)
mmdata$Signs_Symptoms = gsub("4", "Others", mmdata$Signs_Symptoms)
mmdata$Signs_Symptoms = gsub("3", "Fever", mmdata$Signs_Symptoms)
mmdata$Signs_Symptoms= gsub("2", "Bone Tenderness", mmdata$Signs_Symptoms)
mmdata$Signs_Symptoms = gsub("1", "Pallor", mmdata$Signs_Symptoms)

#----------------------------------------------------creating signs and symptoms variables--------------------------------------------------------------------------
mmdata = mmdata %>% mutate(`Pallor` = ifelse(grepl("(?i)pallor|Pallor", Signs_Symptoms), "Yes", "No"))
mmdata = mmdata %>% mutate(`Bone Tenderness` = ifelse(grepl("(?i)Bone Tenderness|Bone tenderness", Signs_Symptoms), "Yes", "No"))
mmdata = mmdata %>% mutate(`Fever` = ifelse(grepl("(?i)Fever|fever", Signs_Symptoms), "Yes", "No"))
mmdata = mmdata %>% mutate(`Otherssignsymptom` = ifelse(grepl("(?i)Others|other", Signs_Symptoms), "Yes", "No"))
mmdata = mmdata %>% mutate(`No symptom` = ifelse(grepl("(?i)none|None", Signs_Symptoms), "Yes", "No"))

#-----------------------------------------tables for the sign and symptoms variables----------------------------------------------------------------------------------
table(mmdata$Pallor)
table(mmdata$`Bone Tenderness`)
table(mmdata$Fever)
table(mmdata$Otherssignsymptom)


#----------------------------------------------------Bleeding site cleaning----------------------------------------------------------------------------------------
mmdata$Bleeding_Site[mmdata$Bleeding_Site == "NOSE" | mmdata$Bleeding_Site == "NOSE."] = "Nose"
mmdata$Bleeding_Site[mmdata$Bleeding_Site == "MOUTH"] = "Mouth"
mmdata$Bleeding_Site[mmdata$Bleeding_Site == "GUMS"] = "Gums"
table(mmdata$Bleeding_Site)


#------------------------------------------------------------------Swelling site cleaning-------------------------------------------------------------------
mmdata$Swelling_Site[mmdata$Swelling_Site == "Bilateral swelling"| mmdata$Swelling_Site == "billateral"|
                       mmdata$Swelling_Site == "bilateral"| mmdata$Swelling_Site == "BILATERAL LL"|
                       mmdata$Swelling_Site == "BILLATERAL"| mmdata$Swelling_Site == "bilateral lower limb swelling"| 
                       mmdata$Swelling_Site == "BILATERAL LOWER LIMBS"] = "Bilateral Swelling" 
mmdata$Swelling_Site[mmdata$Swelling_Site == "lower limb"|mmdata$Swelling_Site == "lower limbs"|
                       mmdata$Swelling_Site == "LOWER LIMB"|mmdata$Swelling_Site == "LL"|
                       mmdata$Swelling_Site == "lower limb sweolling"|
                       mmdata$Swelling_Site == "left lower limb"] = "Lower Limb"
mmdata$Swelling_Site[mmdata$Swelling_Site == "abdomen"|mmdata$Swelling_Site == "ABDOMINAL"|
                       mmdata$Swelling_Site == "abdominal"] = "Abdominal"
mmdata$Swelling_Site[mmdata$Swelling_Site == "BACK"|mmdata$Swelling_Site == "back"] = "Back"
mmdata$Swelling_Site[mmdata$Swelling_Site == "CHEST RT SIDE"|mmdata$Swelling_Site == "chest wall"]= "Chest area"
mmdata$Swelling_Site[mmdata$Swelling_Site == "EYE"|mmdata$Swelling_Site == "eye"] = "Eye"
mmdata$Swelling_Site[mmdata$Swelling_Site == "face" |mmdata$Swelling_Site == "FACE"]= "Face"



#---------------------------------------------------------------------VITALS------------------------------------------------------------------------------------
#-----------------------ECOG PERFORMANCE-------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------Convert to categorical--------------------------------------------------------------------------------
mmdata$Ecog_Index <- factor(mmdata$Ecog_index, levels = c(0,1, 2, 3,4), labels = c("Normal", "Symptomatic but ambulatory", 
                                                                                   "Bedridden <50%", "Bedridden >50%",
                                                                                   "bedridden 100%"))

#-------------------------------------------------Check the class and levels of categorical_values---------------------------------------------------------------
class(mmdata$Ecog_index)
levels(mmdata$Ecog_index)
table(mmdata$Ecog_index)
table(mmdata$Ecog_index1)


#--------------------------------------------------------------------------spep replace with the most recent--------------------------------------------------------------------------------
mmdata <- mmdata %>%
  arrange(Hosp_ID, E_Date) %>%
  group_by(Hosp_ID) %>%
  mutate(spep= if_else(Encounter_Type == "Initial" & is.na(`Mcomponent_g/l`) & difftime(E_Date, lag(E_Date), units = "days") < 14, lead(`Mcomponent_g/l`), `Mcomponent_g/l`)) %>%
  ungroup()



#---------------------------------------------------cleaning the treatment variables--------------------------------------------------------------------
#---------------------------------------------------------start treatment-----------------------------------------------------------------------------
table(mmdata$StartTreatment_regimen)
mmdata$StartTreatment_regimen[mmdata$StartTreatment_regimen == 1]= "VTD"
mmdata$StartTreatment_regimen[mmdata$StartTreatment_regimen == 15]= "Others"
mmdata$StartTreatment_regimen[mmdata$StartTreatment_regimen == 2]= "TD"
mmdata$StartTreatment_regimen[mmdata$StartTreatment_regimen == 3]= "VRD"
mmdata$StartTreatment_regimen[mmdata$StartTreatment_regimen == 4]= "VCD"
mmdata$StartTreatment_regimen[mmdata$StartTreatment_regimen == 5]= "VD"
mmdata$StartTreatment_regimen[mmdata$StartTreatment_regimen == 6]= "VMD"
mmdata$StartTreatment_regimen[mmdata$StartTreatment_regimen == 7]= "RD"
mmdata$StartTreatment_regimen[mmdata$StartTreatment_regimen == "2;7"]= "TD"
mmdata$StartTreatment_regimen[mmdata$StartTreatment_regimen == "VTD, VTD"]= "VTD"
table(mmdata$StartTreatment_regimen)

#continue regimen'
table(mmdata$ContinueTreatment_Regimen)
mmdata$ContinueTreatment_Regimen[mmdata$ContinueTreatment_Regimen == 1]= "VTD"
mmdata$ContinueTreatment_Regimen[mmdata$ContinueTreatment_Regimen == 15]= "Others"
mmdata$ContinueTreatment_Regimen[mmdata$ContinueTreatment_Regimen == 2]= "TD"
mmdata$ContinueTreatment_Regimen[mmdata$ContinueTreatment_Regimen == 3]= "VRD"
mmdata$ContinueTreatment_Regimen[mmdata$ContinueTreatment_Regimen == 4]= "VCD"
mmdata$ContinueTreatment_Regimen[mmdata$ContinueTreatment_Regimen == 5]= "VD"
mmdata$ContinueTreatment_Regimen[mmdata$ContinueTreatment_Regimen == 6]= "VMD"
mmdata$ContinueTreatment_Regimen[mmdata$ContinueTreatment_Regimen == 7]= "RD"
mmdata$ContinueTreatment_Regimen[mmdata$ContinueTreatment_Regimen == "2;7"]= "TD"
mmdata$ContinueTreatment_Regimen[mmdata$ContinueTreatment_Regimen == "VTD, VTD"]= "VTD"
table(mmdata$ContinueTreatment_Regimen)


#chemo re initiated
mmdata$Chemo_Regimen[mmdata$Chemo_Regimen == 1]= "VTD"
mmdata$Chemo_Regimen[mmdata$Chemo_Regimen == 15]= "Others"
mmdata$Chemo_Regimen[mmdata$Chemo_Regimen == 2]= "TD"
mmdata$Chemo_Regimen[mmdata$Chemo_Regimen == 3]= "VRD"
mmdata$Chemo_Regimen[mmdata$Chemo_Regimen == 4]= "VCD"
mmdata$Chemo_Regimen[mmdata$Chemo_Regimen == 5]= "VD"
mmdata$Chemo_Regimen[mmdata$Chemo_Regimen == 6]= "VMD"
mmdata$Chemo_Regimen[mmdata$Chemo_Regimen == 7]= "RD"
mmdata$Chemo_Regimen[mmdata$Chemo_Regimen == "2;7"]= "TD"
mmdata$Chemo_Regimen[mmdata$Chemo_Regimen == "VTD, VTD"]= "VTD"
table(mmdata$Chemo_Regimen)

#-------------------filling the missing treatment variable with what is in the next encounters--------------------------------------------------
#merging treatment columns start, continue and chemo initiated columns
mmdata=mmdata %>%
  group_by(Hosp_ID) %>%
  arrange(E_Date) %>%
  mutate(Treatment = StartTreatment_regimen)
table(mmdata$Treatment)


#---------------------------------------merge treatment with continue treatment------------------------------------------------------------------
mmdata = mmdata%>%
  group_by(Hosp_ID)%>%
  arrange(E_Date)%>%
  mutate(Treatment = ifelse(is.na(Treatment),ContinueTreatment_Regimen, Treatment))
table(mmdata$Treatment)

#---------------------------------------merge treatment with chemo regimen and order the variables in my preferred order-----------------------------------------------------------------------
mmdata = mmdata%>%
  group_by(Hosp_ID)%>%
  arrange(E_Date)%>%
  mutate(Treatment = ifelse(is.na(Treatment),Chemo_Regimen, Treatment))%>%
  select(Iv_Date, Hosp_ID, Date_diagnosis, E_Date, Treatment, StartTreatment_regimen, ContinueTreatment_Regimen, Chemo_Regimen,Treatment, 
         Ecog_index, Ecog_Index, `Mcomponent_g/l`, spep, `Serum_creatinine_mmol/l`, creatinine, `HgB_g/dl`, anemia, `SerumAlbumin_g/l`, `SerumCalcium_unmol/l`,
          corrected_calcium,  everything())


mmdata=mmdata%>%
  group_by(Hosp_ID)%>%
  arrange(E_Date)%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("to VTD", ManagementPlan_Notes), "VTD", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("restart on VTD", ManagementPlan_Notes), "VTD", NA), Treatment)) %>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("re initiated on VD", ManagementPlan_Notes), "VD", NA), Treatment)) %>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("ON MAINTENANCE Cychlophosphamide", ManagementPlan_Notes), "C",NA), Treatment)) %>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("CT THALIDOMIDE.", ManagementPlan_Notes), "T", NA), Treatment)) %>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("CT LENALIDOMIDE.", ManagementPlan_Notes), "R", NA), Treatment)) %>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("CONTINUE MAITENANCE LENALIDOMIDE", ManagementPlan_Notes), "R", NA), Treatment)) %>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("CONTINUE BORTEZOMIB", ManagementPlan_Notes), "V", NA), Treatment)) %>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("To continue with zolendronic Acid", ManagementPlan_Notes), "Zolendronic acid", NA), Treatment)) %>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("CAME FOR INJ ZOLENDRONIC", ManagementPlan_Notes), "Zolendronic acid", NA), Treatment)) %>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("on monthly zolendronic", ManagementPlan_Notes), "Zolendronic acid", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("on lenalidomide", ManagementPlan_Notes), "R", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("on maintenance thalidex", ManagementPlan_Notes), "TD", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("strat bortezomib as maintenance", ManagementPlan_Notes), "V", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("CONTINUE WITH BORTEZOMIB", ManagementPlan_Notes), "V", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("continue bortezomib maintenance.", ManagementPlan_Notes), "V", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("on maintenance lenalidomide", ManagementPlan_Notes), "R", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("on maintenance therapy of lenalidomide", ManagementPlan_Notes), "R", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("CT MAINTENANCE WITH LENALIDOMIDE.", ManagementPlan_Notes), "R", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("maintenance of lenalidomide", ManagementPlan_Notes), "R", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("on maintenance RD", ManagementPlan_Notes), "RD", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("to lenalidomide", ManagementPlan_Notes), "R", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("on TD", ManagementPlan_Notes), "TD", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("of thalidomide", ManagementPlan_Notes), "T", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("with lenalidomide", ManagementPlan_Notes), "R", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("On thalidomide", ManagementPlan_Notes), "T", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("of lenalidomide", ManagementPlan_Notes), "R", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("of thalidomide", ManagementPlan_Notes), "T", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("CONTINUE THALIDOMIDE MAINTENANCE", ManagementPlan_Notes), "T", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("with thalidomide", ManagementPlan_Notes), "T", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("maintenance thalidomide", ManagementPlan_Notes), "T", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("on VTD", ManagementPlan_Notes), "VTD", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("CONTINUE THALIDOMIDE MAINTENANCE", ManagementPlan_Notes), "T", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("TO VRD", ManagementPlan_Notes), "VRD", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("STARTED ON RADIOTHERAPY", ManagementPlan_Notes), "Radiotherapy", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("DIED BEFORE TREATMENT.", ManagementPlan_Notes), "Died before treatment", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("TO RD", ManagementPlan_Notes), "RD", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("TO START TREATMENT,VTD", ManagementPlan_Notes), "VTD", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("ON MAINTENANCE OF RD", ManagementPlan_Notes), "RD", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("with lenalidomide maintenance", ManagementPlan_Notes), "R", NA), Treatment))%>%
  mutate(Treatment = ifelse(is.na(Treatment), ifelse(grepl("TO RD", ManagementPlan_Notes), "RD", NA), Treatment))
  table(mmdata$Treatment)
  
  
#----------------------------------------------------------------------cycle cleaning-----------------------------------------------------------------------------------------
  mmdata=mmdata %>%
    group_by(Hosp_ID) %>%
    arrange(E_Date) %>%
    mutate(cycle = StartTreatment_Cycle)
table(mmdata$StartTreatment_Cycle)
  
  
  #---------------------------------------merge cycle with continue treatment cycle-----------------------------------------------------------------------------------
mmdata = mmdata%>%
    group_by(Hosp_ID)%>%
    arrange(E_Date)%>%
    mutate(cycle = ifelse(is.na(cycle),ContinueTreatment_Cycle, cycle))
table(mmdata$cycle)
  
  #---------------------------------------merge cycle with chemo re initiated cycle-----------------------------------------------------------------------
  mmdata = mmdata%>%
    group_by(Hosp_ID)%>%
    arrange(E_Date)%>%
    mutate(cycle = ifelse(is.na(cycle),Chemo_Cycle, cycle))%>%
  select(Iv_Date, Hosp_ID, Date_diagnosis, E_Date, Treatment, StartTreatment_regimen, ContinueTreatment_Regimen, Chemo_Regimen,Treatment, 
         Ecog_index, Ecog_Index, `Mcomponent_g/l`, spep, `Serum_creatinine_mmol/l`, creatinine, `HgB_g/dl`, anemia, `SerumAlbumin_g/l`, `SerumCalcium_unmol/l`,
         corrected_calcium,StartTreatment_Cycle, ContinueTreatment_Cycle, Chemo_Cycle, cycle,  everything())

table(mmdata$cycle)
  

#--------------------------------------------------Remission cleaning-----------------------------------------------------------------------------------
#remission
#----------------------------------------renaming the remission codes with actual words they represent------------------------------------------------------
table(mmdata$Remission_plan)
mmdata$Remission_plan <- ifelse(mmdata$Remission_plan == 1, "Start Drug Holiday", mmdata$Remission_plan)
mmdata$Remission_plan <- ifelse(mmdata$Remission_plan == 2, "Continue Drug Holiday", mmdata$Remission_plan)
mmdata$Remission_plan <- ifelse(mmdata$Remission_plan == 3, "Start Maintenance Therapy", mmdata$Remission_plan)
mmdata$Remission_plan <- ifelse(mmdata$Remission_plan == 4, "Continue Maintenance", mmdata$Remission_plan)

#-------------------------------------merging the format of the remission plan texts------------------------------------------------------------
mmdata$Remission_plan[mmdata$Remission_plan == "Continue Drug Holiday"] = "Drug Holiday"
mmdata$Remission_plan[mmdata$Remission_plan == "Continue Maintenance"] = "On Maintenance Therapy"
mmdata$Remission_plan[mmdata$Remission_plan == "Continue Maintenance Therapy"] ="On Maintenance Therapy"
mmdata$Remission_plan[mmdata$Remission_plan == "CONTINUE WITHMAINTENANCE"] = "On Maintenance Therapy"
mmdata$Remission_plan[mmdata$Remission_plan == "Start Drug Holiday"] = "Drug Holiday"
mmdata$Remission_plan[mmdata$Remission_plan == "Start Maintanance Therapy"] ="On Maintenance Therapy"
mmdata$Remission_plan[mmdata$Remission_plan == "Start Maintenance Therapy"] ="On Maintenance Therapy"
table(mmdata$Remission_plan)

#-------------------------------------------fill missing remission plan from management notes-----------------------------------------------------
mmdata <- mmdata %>%
  group_by(Hosp_ID) %>%
  arrange(E_Date) %>%
  mutate(Remission_plan = ifelse(is.na(Remission_plan) &
                                   (grepl("start remission|on maintenance|for maintenance|ON THALIDEX MAINTENANCE|
                                          ON MIANTENANCE|Thalidomide maintanance|ON TD MAINTENANCE|on maintenance cychlophosphamide|
                                          CONTINUE WITH MAINTENANCE THERAPY|START MAINTANANCE|CONTINUE MAINTANANCE|LENALIDOMIDE MAINTENANCE|
                                          CONTINUE WITH MAINTENANCE|CONTINUE MAINTENANCE|CONTINUE WITH MAINTENANCE|CONTINUE WITH THALIDOMIDE MAINTENANCE|
                                          IN REMISSION|start maintenance|maintenance|MAITAINANCE|MAINTANCE|mainance|ON MAINTANANCE|MAITENANCE|with remission", 
                                          ManagementPlan_Notes, ignore.case = TRUE)), "On Maintenance Therapy", Remission_plan))
table(mmdata$Remission_plan)
#-------------------------------------------------------patients on remission-------------------------------------------------------------------------
#---------------------------------------merging those with remission date but on remission is false------------------------------------------------
table(mmdata$OnRemission)
mmdata = mmdata%>%
  group_by(Hosp_ID)%>%
  arrange(E_Date)%>%
  mutate(OnRemission = ifelse(!OnRemission & !is.na(Remission_date), TRUE, OnRemission))

#-----------------------------------------merge remission with remission plan---------------------------------------------------------------------
mmdata= mmdata%>%
  group_by(Hosp_ID)%>%
  arrange(E_Date)%>%
  mutate(OnRemission = ifelse(!OnRemission & (!is.na(Remission_plan)| Remission_plan != "stop all medications"), TRUE, OnRemission))

#---------------------------------filter data to remain only with initial data---------------------------------------------------------------------------
initial_data <- subset(mmdata, Encounter_Type == "Initial")


#-------------------------------------------setting data for survival analysis and cox regression analysis---------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(survival)
library(survminer)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condSURV)
library(ranger)
library(ggfortify)

#----------------------------------------------------survival time in days----------------------------------------------------
initial_data$survivaldays <- round(as.numeric(difftime(initial_data$latest_encounterdate, initial_data$Date_diagnosis, units = "days"))/1, 1)
table(initial_data$survivaldays)
summary(initial_data$survivaldays)

#---------------------------------------------------------survival time in months---------------------------------
initial_data$survivalmonths <- round(as.numeric(difftime(initial_data$latest_encounterdate, initial_data$Date_diagnosis, units = "days"))/ 30.5, 1)
table(initial_data$survivalmonths)
summary(initial_data$survivalmonths)

#-------------------------------------------------survival time in years-------------------------------------------------------
initial_data$survivalyears <- round(as.numeric(difftime(initial_data$latest_encounterdate, initial_data$Date_diagnosis, units = "days"))/ 365.25, 1)
table(initial_data$survivalyears)
summary(initial_data$survivalyears)

#-------------------------------------------------checking for survival times that are less than 0--------------------------------------------------
missing = initial_data%>%
  filter(survivaldays <0)


#----------------------------------------------------Kaplan Meier analysis-------------------------------------------------------
allfit = survfit(Surv(survivalmonths, survival_status)~1, data = initial_data)
summary(allfit)
autoplot(allfit)
str(allfit)

#----------------------------------------------SUMMARIZING THE SURVIVAL KAPLAN MEIER ESTIMATES---------------------------------------------------
summary(initial_data$survivalmonths)
summary(survfit(Surv(survivalmonths, survival_status)~1, data = initial_data), times = 40.1)


#-----------------------------------------------computing the cox regression-------------------------------------------------------------
mm.cox <- coxph(Surv(survivalmonths, survival_status)~ Ecog_Index  + `Bone Pain` + 
                  creatinine + Treatment + anemia + renalfailure + hypercalcemia,  data =  initial_data)
mm.cox

#----------------------------------------------------Testing the assumptions of the cox regression using statistical--------------------------------
test.phmm = cox.zph(mm.cox)

#----------------------------------------------------testing the cox assumptions using a graphical diagnostics
ggcoxzph(test.phmm)






