#  This file is part of CEOsys Recommendation Checker.
#
#  Copyright (c) 2021 CEOsys project team <https://covid-evidenz.de>.
#
#  CEOsys Recommendation Checker is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  CEOsys Recommendation Checker is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with CEOsys Recommendation Checker.  If not, see <https://www.gnu.org/licenses/>.

# 2Do
# Token generieen 
# load_dot_env() ?
# rm(list = ls())


library(httr)
library(dplyr)
library(dotenv)
library(stringr)
library(readr)

act.wd <- getwd()
setwd("D:/GitHub/user-interface")                                               # Nach lokalem Speicherort ändern
files <- list.files(path = "data/")

patient.dat<-read_csv(paste0("data/", "recommendation_person_data_202301061125.csv" )) %>% arrange(person_id) %>% 
  mutate(ward = case_when(person_id <= 30 ~ "ward1", 
                          person_id <= 60 ~ "ward2",
                          person_id >= 61 ~ "ward3"))  %>% 
  mutate(icu_day = 33)
age<-data_frame(
  person_id = 0:max(patient.dat$person_id),
  age= sample(30:100,max(patient.dat$person_id)+1, replace=TRUE))
patient.dat<-patient.dat %>%  left_join(age, by = "person_id") 


recommendation.dat<-read_csv(paste0("data/", "recommendation_result_202301061125.csv" ))
cohort_definition <- read_csv(paste0("data/","cohort_definition_202301061125.csv"))
recommendation_run<-read_csv(paste0("data/", "recommendation_run_202301061125.csv" ))

setwd(act.wd)



#load_dot_env()
#GET(url='http://localhost:5004/run')
#base_url <- "http://localhost:5002"
#GET(url = paste0(Sys.getenv("COMPARATOR_SERVER"), "/run"))
#base_url <- Sys.getenv("UI_BACKEND_SERVER")
#req <- POST(url = paste0(base_url, "/token"), body = list(grant_type = "password", username = Sys.getenv("UI_BACKEND_USERNAME"), password = Sys.getenv("UI_BACKEND_PASSWORD")))
#auth.token <- content(req)$access_token
#req <- GET(paste0(base_url, "/recommendation/list"), add_headers("Authorization" = paste("Bearer", auth.token)))
#recommendations <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8")) %>% as.data.frame()
#parse_datetime <- function(s) {
#  return(s %>% str_replace("(\\d\\d):(\\d\\d)$", "\\1\\2") %>% str_replace("\\.\\d{6}", "") %>% as.POSIXct(format = "%Y-%m-%dT%H:%M:%S%z"))
#}

recommendations <- cohort_definition %>% 
  select(cohort_definition_id,recommendation_url) %>%  
  rename(id = cohort_definition_id, title = recommendation_url ) %>% 
  mutate(text = "text")

rec_results<-recommendation.dat %>% 
  filter(is.na(criterion_name) & is.na(recommendation_plan_name)) %>% 
  arrange(person_id)

patients <- patient.dat

#req <- GET(paste0(base_url, "/patients/list"), add_headers("Authorization" = paste("Bearer", auth.token)))
#patients$age <- floor(age_calc(as.Date(patients$birth_date), units = "years"))
#patients$icu_day <- as.numeric(floor(age_calc(as.Date(patients$admission_hospitalisation), units = "days")))


load_recommendation_variables <- function(recommendation_id) {
  #req <- GET(paste0(base_url, "/recommendation/variables/", recommendation_id), add_headers("Authorization" = paste("Bearer", auth.token)))
  #recommendation_variables <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8"))
  
  recommendation_variables <-  patients[patients$recommendation_run_id==recommendation_id,] %>% 
    distinct(parameter_concept_id,  .keep_all = TRUE) %>% 
    select(cohort_category, criterion_name)  %>%
    rename(type = cohort_category, variable_name = criterion_name )

  return(recommendation_variables)
}


load_recommendation_results <- function(recommendation_id) {
  #req <- GET(paste0(base_url, "/recommendation/get/", recommendation_id), add_headers("Authorization" = paste("Bearer", auth.token)))
  #recommendation_results <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8"))
  #gl_summary <- recommendation_results[["summary"]]
  #gl_details <- recommendation_results[["detail"]]
  
  patient_results <- patients[patients$recommendation_run_id==recommendation_id,] %>%
    select(person_id, ward, age, icu_day)
  
  gl_summary_rec<-rec_results[rec_results$recommendation_run_id==recommendation_id,] %>% arrange(person_id)
  
  gl_summary <- data_frame( 
  person_id = unique(gl_summary_rec$person_id),
  valid_exposure = unique(gl_summary_rec$person_id) %in% gl_summary_rec$person_id[gl_summary_rec$cohort_category == "POPULATION_INTERVENTION"],
  valid_population = unique(gl_summary_rec$person_id) %in% gl_summary_rec$person_id[gl_summary_rec$cohort_category == "POPULATION"],
  valid_treatment = unique(gl_summary_rec$person_id) %in% gl_summary_rec$person_id[gl_summary_rec$cohort_category == "POPULATION"] )

  patient_results <- patient_results %>% inner_join(gl_summary, by = "person_id")
  
  return(patient_results)
}



load_patient <- function(patient_id, recommendation_id) {
  if (is.null(patient_id) | length(patient_id) == 0) {
    return(NULL)
  }
  
  # Use token to fetch the actual data.
  #req <- GET(paste0(base_url, "/patient/get/?patient_id=", patient_id, "&recommendation_id=", recommendation_id), add_headers("Authorization" = paste("Bearer", auth.token)))
  #patientdata <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8"))
  
  patientdata<- patients[patients$recommendation_run_id==recommendation_id & patients$person_id==patient_id & patients$domain_id=="Measurement",] %>%
    select(criterion_name, value_as_number, start_datetime, end_datetime, person_id) %>%
    rename(variable_name=criterion_name, value=value_as_number, datetime=start_datetime, datetime_end=end_datetime)
  
  patientdata <- patientdata %>% arrange(criterion_name, start_datetime)

  return(patientdata)
}
