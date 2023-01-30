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
library(httr)
library(dotenv)
library(stringr)
library(readr)
library(dplyr)
library(lubridate)
# library(urltools)


base_url <- "http://localhost:8001"
# base_url <- Sys.getenv("UI_BACKEND_SERVER")

rec_map <- list(
  "recommendation_url" = c(
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/no-therapeutic-anticoagulation",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/sepsis/recommendation/ventilation-plan-ards-tidal-volume",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/ventilation-plan-ards-tidal-volume",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/covid19-ventilation-plan-ards-peep",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/prophylactic-anticoagulation",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/therapeutic-anticoagulation",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/covid19-abdominal-positioning-ards"
  ),
  "short" = c("No ACT", "Sepsis/Tidal", "C19/Tidal", "PEEP", "p-ACT", "t-ACT", "Proning")
) %>% as_tibble()

load_recommendations <- function() {
  req <- GET(paste0(base_url, "/recommendation/list"))
  recommendations <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8")) %>%
    as_tibble() %>%
    inner_join(rec_map, by = "recommendation_url")
  return(recommendations)
}

recommendations <- load_recommendations()


summarize_category <- function(categories) {
  if ("population_intervention" %in% categories) {
    return("PI")
  } else if ("population" %in% categories) {
    return("P")
  } else if ("intervention" %in% categories) {
    return("I")
  } else {
    return("o")
  }
}

load_patient_list <- function(selected_recommendation_urls, start_datetime, end_datetime) {
  patients <- tibble()

  if (is.null(selected_recommendation_urls)) {
    return(patients)
  }

  for (recommendation_url in selected_recommendation_urls) {
    req <- GET(paste0(base_url, "/patient/list/?recommendation_url=", URLencode(recommendation_url), "&start_datetime=", URLencode(as.character(start_datetime)), "&end_datetime=", URLencode(as.character(end_datetime))))

    data <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8"))
    run_id <- data$run_id
    pat_data <- data$data %>%
      as_tibble() %>%
      mutate(run_id = run_id, url = recommendation_url)
    patients <- bind_rows(patients, pat_data)
  }

  run_ids <- patients %>% distinct(run_id, url)

  patients <- patients %>%
    # make up a ward
    mutate(ward = sprintf("ITS %02d", (person_id %% 3) + 1)) %>%
    inner_join(rec_map %>% rename(url = recommendation_url), by = "url") %>%
    pivot_wider(id_cols = c("person_id", "ward"), names_from = "short", values_from = "cohort_category", values_fn = summarize_category) %>%
    arrange(person_id)

  return(list(patients = patients, run_id = run_ids))
}

load_recommendation_variables <- function(recommendation_url) {
  req <- GET(paste0(base_url, "/recommendation/criteria/?recommendation_url=", URLencode(recommendation_url)))
  data <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8"))
  criteria <- data$criterion %>%
    as_tibble() %>%
    rename(type = cohort_category, variable_name = concept_name, criterion_name = unique_name)

  return(criteria)
}

load_data <- function(person_id, run_id, criterion_name) {
  if (is.null(person_id) | length(person_id) == 0) {
    return(NULL)
  }

  req <- GET(paste0(base_url, "/patient/data/?person_id=", URLencode(as.character(person_id)), "&run_id=", URLencode(as.character(run_id)), "&criterion_name=", URLencode(criterion_name)))

  if (req$status_code != 200) {
    stop("Error encountered during load_data")
  }

  patientdata <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8")) %>% as_tibble()

  patientdata <- patientdata %>%
    rename(datetime = start_datetime) %>%
    arrange(datetime)



  if (("value_as_number" %in% names(patientdata))) {
    patientdata <- patientdata %>%
      rename(value = value_as_number)
  } else if ("drug_dose_as_number" %in% names(patientdata)) {
    patientdata <- patientdata %>%
      rename(value = drug_dose_as_number)
  } else {
    patientdata <- patientdata %>%
      mutate(value = TRUE)
  }

  if (!("end_datetime" %in% names(patientdata))) {
    patientdata <- patientdata %>%
      mutate(end_datetime = datetime)
  }

  if (nrow(patientdata) > 0) {
    patientdata <- patientdata %>%
      mutate(datetime = parse_datetime(datetime)) %>%
      mutate(end_datetime = parse_datetime(end_datetime))
  }

  return(patientdata)
}
