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

OFFLINE <- nchar(Sys.getenv('OFFLINE')) > 0
OFFLINE_PATH = 'offline-data/'

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


COLUMN_SUFFIXES <- c(".data", ".days", ".comment")

read_offline_data <- function(name) {
  return(read_csv(paste0(OFFLINE_PATH, name, '.csv'), show_col_types = FALSE))
}


expand_colnames <- function(colnames, suffixes = COLUMN_SUFFIXES) {
  #' Expand Column Names with Custom Suffixes
  #'
  #' This function takes a character vector of column names and a character vector
  #' of suffixes, and expands the column names by appending each suffix to each
  #' column name. The expanded column names are returned in a specific order,
  #' with each group of suffixes appearing together in the sequence.
  #'
  #' @param colnames A character vector containing the original column names
  #' @param suffixes A character vector containing the custom suffixes to be
  #'        appended to the column names
  #'
  #' @return A character vector containing the expanded column names in the
  #'         specified order, with each group of suffixes appearing together
  #'         in the sequence
  #'
  #' @examples
  #' ## Expand a vector of column names with custom suffixes
  #' original_colnames <- c("A", "B")
  #' custom_suffixes <- c(".data", ".days", ".comment")
  #' expanded_colnames <- expand_colnames(original_colnames, custom_suffixes)
  #' print(expanded_colnames)

  ordered_colnames <- unname(unlist(lapply(colnames, function(x) sapply(suffixes, function(s) paste0(x, s)))))

  return(ordered_colnames)
}


load_recommendations <- function() {
  #' Load Recommendations
  #'
  #' This function retrieves a list of recommendations from a specified endpoint and converts the response into a tibble data frame.
  #' The response is joined with a pre-defined mapping data frame 'rec_map', by the column 'recommendation_url'.
  #'
  #' @return A tibble data frame containing the recommendations with added columns from the mapping data frame.
  #' @export
  
  if(!OFFLINE) {
    req <- GET(paste0(base_url, "/recommendation/list"))
    
    recommendations <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8")) %>%
      as_tibble() %>%
      inner_join(rec_map, by = "recommendation_url")
  } else {
    recommendations <- read_offline_data("recommendations")
  }

  #write_csv(recommendations, "data-new/recommendations.csv")
  
  return(recommendations)
}

recommendations <- load_recommendations()


summarize_category <- function(categories) {
  #' Summarize categories
  #'
  #' This function takes a character vector of categories and returns a summarized representation based on the presence of 'population_intervention', 'population', or 'intervention' categories.
  #'
  #' @param categories A character vector of categories.
  #' @return A character string representation of the summarized category: "PI" for 'population_intervention', "P" for 'population', "I" for 'intervention', and "o" for other categories.
  #' @examples
  #' categories <- c("population_intervention", "other", "another")
  #' summarize_category(categories) # returns "PI"
  #'
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
  #' Load a list of patients based on selected recommendations and time period
  #'
  #' This function loads a list of patients based on selected recommendations and time period.
  #'
  #' @param selected_recommendation_urls character vector of recommendation urls to be used
  #' @param start_datetime Datetime for the start of the time period
  #' @param end_datetime Datetime for the end of the time period
  #'
  #' @return A list containing patients data in tibble format and run_ids in tibble format.
  #'
  #' @examples
  #' result <- load_patient_list(c("recommendation1","recommendation2"), "2021-01-01", "2021-01-31")
  #' patients <- result$patients
  #' run_ids <- result$run_id
  #'

  patients <- tibble()
  rec_short_names <- rec_map$short

  if (is.null(selected_recommendation_urls)) {
    return(patients)
  }

  if (!OFFLINE) {
    for (recommendation_url in selected_recommendation_urls) {
      req <- GET(paste0(base_url, "/patient/list/?recommendation_url=", URLencode(recommendation_url), "&start_datetime=", URLencode(as.character(start_datetime)), "&end_datetime=", URLencode(as.character(end_datetime))))
      
      data <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8"))
      run_id <- data$run_id
      pat_data <- data$data %>%
        as_tibble() %>%
        mutate(run_id = run_id, url = recommendation_url)
      patients <- bind_rows(patients, pat_data)
    }
  } else {
    patients <- read_offline_data("patients")
  }

  #write_csv(patients, "data-new/patients.csv")

  run_ids <- patients %>% distinct(run_id, url)

  if (nrow(patients) > 0) {
    patients <- patients %>%
      # make up a ward TODO: should be real ward
      mutate(Ward = as.factor(sprintf("ITS %02d", (person_id %% 3) + 1))) %>%
      inner_join(rec_map %>% rename(url = recommendation_url), by = "url") %>%
      pivot_wider(id_cols = c("person_id", "Ward"), names_from = "short", values_from = "cohort_category", values_fn = summarize_category) %>%
      arrange(person_id) %>%
      mutate(Patient = person_id)
  } else {
    # no patients received - return a valid tibble but without rows
    patients <- bind_cols(
      tibble(Patient = character(), person_id = character(), Ward = character(), .rows = 0),
      tibble(!!!rec_short_names, .rows = 0, .name_repair = ~rec_short_names)
    )
  }


  # make up comment data
  t_comment <- patients %>%
    mutate_at(all_of(rec_short_names), ~ runif(nrow(patients)) > 0.5)

  # make up day data
  generate_random_strings <- function(x, n) {
    random_strings <- sapply(1:x, function(i) {
      random_digits <- sample(0:2, n, replace = TRUE)
      random_string <- paste0(random_digits, collapse = "")
      return(random_string)
    })
    return(random_strings)
  }

  n_days <- 10
  t_days <- patients %>%
    mutate_at(vars(all_of(rec_short_names)), ~ generate_random_strings(length(.), n_days))

  # determine percentage data
  percentage <- function(input_vector) {
    result <- sapply(input_vector, function(input_string) {
      split <- strsplit(input_string, "")[[1]]

      # Count the occurrences of 1 and 2
      count_1 <- sum(split == "1")
      count_2 <- sum(split == "2")

      # Divide the number of 2s by the sum of the number of 1s and 2s
      result <- round(count_2 / (count_1 + count_2) * 100, 2)

      return(result)
    })

    return(result)
  }

  t_percentage <- t_days %>% mutate(across(all_of(rec_short_names), percentage))

  # combine days, percentage and comment data into a single tibble
  t_percentage <- t_percentage %>% rename_with(~ paste0(., ".data"))
  t_days <- t_days %>% rename_with(~ paste0(., ".days"))
  t_comment <- t_comment %>% rename_with(~ paste0(., ".comment"))

  # Combine tibbles
  combined_tibble <- bind_cols(t_percentage, t_days, t_comment)

  # Generate the desired order of column names
  expanded_colnames <- expand_colnames(rec_short_names)

  # Reorder columns
  ordered_tibble <- combined_tibble %>% select(all_of(expanded_colnames))

  result <- bind_cols(patients %>% select(-all_of(rec_short_names)), ordered_tibble)
  return(list(patients = result, run_id = run_ids))
}

# t_days<-load_patient_list(rec_map$recommendation_url, start_datetime="2023-01-01", end_datetime="2023-04-03")



load_recommendation_variables <- function(recommendation_url) {
  #' Load Recommendation Variables
  #'
  #' The function `load_recommendation_variables()` retrieves the criteria information for a specified recommendation URL.
  #'
  #' @param recommendation_url Character string of the recommendation URL
  #'
  #' @return A tibble with columns `type`, `variable_name`, and `criterion_name`
  #'
  #' @examples
  #' criteria <- load_recommendation_variables("www.example.com/recommendation/1234")

  if(!OFFLINE) {
    req <- GET(paste0(base_url, "/recommendation/criteria/?recommendation_url=", URLencode(recommendation_url)))
    data <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8"))
    
    criteria <- data$criterion %>%
      as_tibble() %>%
      rename(type = cohort_category, variable_name = concept_name, criterion_name = unique_name)  
  } else {
    criteria <- read_offline_data("criteria") %>% filter({{recommendation_url}} == recommendation_url)
  }
  

  #library(cli)
  #rec_hash = hash_md5(recommendation_url)
  #write_csv(criteria %>% mutate(recommendation_url=recommendation_url), glue("data-new/criteria-{rec_hash}.csv"))
  
  return(criteria)
}

load_data <- function(person_id, run_id, criterion_name, start_date, end_date) {
  #' Load patient data based on person_id, run_id, and criterion_name
  #'
  #' @param person_id character string identifying a person
  #' @param run_id character string identifying a run
  #' @param criterion_name character string identifying a criterion
  #' @param min_dt datetime of the beginning of the observation window
  #' @param max_dt datetime of the end of the observation window
  #'
  #' @return a tibble with patient data, arranged by datetime. Columns may include:
  #'   - `datetime`: start datetime of the patient data
  #'   - `end_datetime`: end datetime of the patient data (defaults to `datetime` if not present)
  #'   - `value`: value of the patient data, renamed from `value_as_number` or `drug_dose_as_number` if present
  #'
  #' @examples
  #' patientdata <- load_data("12345", "run1", "criterion_a")
  #'
  #' @export
  #'
  if (is.null(person_id) | length(person_id) == 0) {
    return(NULL)
  }

  if(OFFLINE) {
    tbl <- generate_tibble(person_id, start_date, end_date, criterion_name)
    return(tbl)
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
      mutate(end_datetime = coalesce(end_datetime, datetime)) %>%
      mutate(datetime = parse_datetime(datetime)) %>%
      mutate(end_datetime = parse_datetime(end_datetime))
  }

  
  #write_csv(patientdata, glue("data-new/patientdata-{person_id}.csv"))
  
  return(patientdata)
}


generate_tibbleX <- function(person_id, start_date, end_date, criterion_name){

  # seed the random number generator
  md5_hash = digest::digest(criterion_name, "md5")
  set.seed(as.integer(person_id) + strtoi(substr(md5_hash, 1, 5), 16))
  
  # initialize the parameter_concept_id to 0
  parameter_concept_id <- 0
  
  # determine the type of the criterion and number of entries
  if (any(startsWith(criterion_name, c("Measurement_", "TidalVolumePerIdealBodyWeight_", "ConceptCriterion_")))) {
    num_entries <- sample(24:24*60, 1) # measurements: once per day to once per hour
  } else if (startsWith(criterion_name, "DrugExposure_")){
    num_entries <- sample(0:2, 1) # drug exposures: 0 to 2 times per day
  } else if (any(startsWith(criterion_name, c("ConditionOccurrence_", "ProcedureOccurrence_", "VisitOccurrence_")))) {
    num_entries <- sample(0:1, 1) # occurrences: 0 to 1 times per day
  } else (
    stop("No entry found for criterion_name = ", criterion_name)
  )
  
  # generate datetime and end_datetime
  datetimes <- sample(seq(as.POSIXct(start_date), as.POSIXct(end_date), by="min"), num_entries)
  end_datetimes <- datetimes + dminutes(sample(0:60, num_entries, replace = TRUE)) # random duration for occurrences
  
  # generate values
  if (any(startsWith(criterion_name, c("Measurement_", "TidalVolumePerIdealBodyWeight_", "ConceptCriterion_")))) {
    if (grepl("aPTT", criterion_name)) values <- runif(num_entries, 20, 40) # adjust as per clinically plausible values
    else if (grepl("Tidal-volume", criterion_name)) values <- runif(num_entries, 5, 10)
    else if (grepl("D-dimer", criterion_name)) values <- runif(num_entries, 0, 0.5)
    else if (grepl("PEEP", criterion_name)) values <- runif(num_entries, 5, 20)
    else if (grepl("Body-weight", criterion_name)) values <- runif(num_entries, 50, 100)
    else if (grepl("Inhaled-oxygen-concentration", criterion_name)) values <- runif(num_entries, 21, 100)
    else if (grepl("Horowitz-index", criterion_name)) values <- runif(num_entries, 200, 500)
    else if (grepl("Pressure-max", criterion_name)) values <- runif(num_entries, 10, 30)
  } else if (startsWith(criterion_name, "DrugExposure_")){
    values <- runif(num_entries, 0.1, 1.0) # adjust as per clinically plausible values
  } else if (any(startsWith(criterion_name, c("ConditionOccurrence_", "ProcedureOccurrence_", "VisitOccurrence_")))) {
    values <- rep(1, num_entries)
  }
  
  # create a tibble
  df <- tibble(
    person_id = rep(person_id, num_entries),
    parameter_concept_id = rep(parameter_concept_id, num_entries),
    datetime = datetimes,
    end_datetime = end_datetimes,
    value = values
  )
  
  return(df)
}

generate_tibble <- function(person_id, start_date, end_date, criterion_name){
  
  # seed the random number generator
  md5_hash = digest::digest(criterion_name, "md5")
  set.seed(as.integer(person_id) + strtoi(substr(md5_hash, 1, 5), 16))
  
  # initialize the parameter_concept_id to 0
  parameter_concept_id <- 0
  
  # calculate the number of days in the observation period
  num_days <- as.integer(difftime(end_date, start_date, units = "days"))
  
  type_range <- TRUE
  
  # determine the type of the criterion and number of entries per day
  if (any(startsWith(criterion_name, c("Measurement_", "TidalVolumePerIdealBodyWeight_", "ConceptCriterion_")))) {
    type_range <- FALSE
    if (grepl("aPTT", criterion_name))  entries_per_day <- sample(4:6, 1) # aPTT: 4-6 times per day
    else if (grepl("Tidal-volume", criterion_name)) entries_per_day <- sample(24:24*4, 1) # Tidal volume: 1-2 times per day
    else if (grepl("D-dimer", criterion_name)) entries_per_day <- sample(1:2, 1) # D-dimer: 1-2 times per day
    else if (grepl("PEEP", criterion_name)) entries_per_day <- sample(24:24*4, 1) # PEEP: 1-4 times per day
    else if (grepl("Body-weight", criterion_name)) entries_per_day <- 1/num_days # Body weight: once per day
    else if (grepl("Inhaled-oxygen-concentration", criterion_name)) entries_per_day <- sample(24:24*4, 1) # Inhaled oxygen concentration: 1-2 times per day
    else if (grepl("Horowitz-index", criterion_name)) entries_per_day <- sample(24:24*4, 1) # Horowitz index: 1-2 times per day
    else if (grepl("Pressure-max", criterion_name)) entries_per_day <- sample(24:24*4, 1) # Maximal pressure during respiration: 1-4 times per day
  
  } else if (startsWith(criterion_name, "DrugExposure_")){
    entries_per_day <- sample(0:2, 1) # drug exposures: 0 to 2 times per day
  } else if (any(startsWith(criterion_name, c("ProcedureOccurrence_")))) {
    entries_per_day <- sample(0:1, 1) # occurrences: 0 to 1 times per day
  } else if (any(startsWith(criterion_name, c("ConditionOccurrence_", "VisitOccurrence_")))) {
    entries_per_day <- sample(0:1, 1) / num_days # occurrences: 0 to 1 times per stay
  } else {
    stop("No entry found for criterion_name = ", criterion_name)
  }
  
  # calculate the total number of entries
  num_entries <- num_days * entries_per_day
  
  # generate datetime and end_datetime
  datetimes <- sample(seq(as.POSIXct(start_date), as.POSIXct(end_date), by="min"), num_entries)
  if(num_entries == 1) {
    sample_range <- 300:3000
  } else {
    sample_range <- 5:720
  }
  
  end_datetimes <- datetimes
  
  if (type_range) {
    end_datetimes <- end_datetimes + as.difftime(sample(sample_range, num_entries, replace = TRUE), units = "mins") # random duration for occurrences
  }

  # generate values
  if (any(startsWith(criterion_name, c("Measurement_", "TidalVolumePerIdealBodyWeight_", "ConceptCriterion_")))) {
    if (grepl("aPTT", criterion_name)) values <- runif(num_entries, 20, 40) # adjust as per clinically plausible values
    else if (grepl("Tidal-volume", criterion_name)) values <- runif(num_entries, 5, 10)
    else if (grepl("D-dimer", criterion_name)) values <- runif(num_entries, 0, 0.5)
    else if (grepl("PEEP", criterion_name)) values <- runif(num_entries, 5, 20)
    else if (grepl("Body-weight", criterion_name)) values <- runif(num_entries, 50, 100)
    else if (grepl("Inhaled-oxygen-concentration", criterion_name)) values <- runif(num_entries, 21, 100)
    else if (grepl("Horowitz-index", criterion_name)) values <- runif(num_entries, 200, 500)
    else if (grepl("Pressure-max", criterion_name)) values <- runif(num_entries, 10, 30)
  } else if (startsWith(criterion_name, "DrugExposure_")){
    values <- runif(num_entries, 0.1, 1.0) # adjust as per clinically plausible values
  } else if (any(startsWith(criterion_name, c("ConditionOccurrence_", "ProcedureOccurrence_", "VisitOccurrence_")))) {
    values <- rep(1, num_entries)
  }
  
  # create a tibble
  df <- tibble(
    person_id = rep(person_id, num_entries),
    parameter_concept_id = rep(parameter_concept_id, num_entries),
    datetime = datetimes,
    end_datetime = end_datetimes,
    value = values
  ) %>% arrange(datetime)

  return(df)
}

