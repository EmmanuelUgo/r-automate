library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(janitor)

googledrive::drive_auth(
  path = gargle::secret_decrypt_json(
    "inst/googledrive_encrpyt.json",
    "GOOGLEDRIVE_KEY"
  )
)

googlesheets4::gs4_auth(
  path = gargle::secret_decrypt_json(
    "inst/googledrive_encrpyt.json",
    "GOOGLEDRIVE_KEY"
  )
)

sheet_id <- "https://docs.google.com/spreadsheets/d/1QJbm6ZxDlo4Zl3IAo-Kh_JlTe1TsZD4rEgXDMHBpUQo/"

survey_response <- read_sheet(sheet_id, sheet = 1)
survey_code <- read_sheet(sheet_id, sheet = 2)

## Transform and clean data
srv_data <- survey_response %>%
  set_names(survey_code$label) %>%
  select(-"timestamp") %>%
  mutate(across(
    -c("instructor"),
    \(x) case_when(
      x == "Strongly disagree" ~ 1,
      x == "Disagree" ~ 2,
      x == "Neutral" ~ 3,
      x == "Agree" ~ 4,
      x == "Strongly agree" ~ 5
    )
  ))

## KPI calculation

get_KPI <- function(data, cols) {
  data %>%
    select(starts_with(cols)) %>%
    pivot_longer(cols = everything(), names_to = "dim") %>%
    group_by(.data$dim) %>%
    summarize(dim_avg = mean(.data$value, na.rm = T)) %>%
    pull(.data$dim_avg) %>%
    mean() %>%
    round(2)
}

kpi_tbl <-
  srv_data %>%
  select(starts_with("instructor")) %>%
  nest_by(instructor) %>%
  mutate(kpi = get_KPI(data, cols = "instructor_")) %>%
  select(-"data") %>%
  ungroup() %>%
  mutate(instruct_id = make_clean_names(paste0("kpi_ind_", instructor))) %>%
  select("instruct_id", "kpi") %>%
  pivot_wider(names_from = "instruct_id", values_from = "kpi") %>%
  mutate(
    timestamp = Sys.time(),
    kpi_instructor = get_KPI(srv_data, cols = "instructor_"),
    kpi_content = get_KPI(srv_data, cols = "content_"),
    kpi_overall = get_KPI(srv_data, cols = "overall_")
  ) %>%
  select("timestamp",
         "kpi_overall",
         "kpi_content",
         "kpi_instructor",
         starts_with("kpi_ind_"))



sheet_append(ss = sheet_id, data = kpi_tbl, sheet = "metrics")
