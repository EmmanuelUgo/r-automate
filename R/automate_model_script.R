

# Introduction ------------------------------------------------------------

## For this script, we want to pull in data from googlesheet,
## Apply the model on the data and save the results back to googlesheet.
## Since this would work in an automated fashion, we don't want to repredict on
## already predicted data so we need to somehow mark it.


## Load necessary libraries

library(googlesheets4)
library(readr)
library(dplyr)
library(parsnip)
library(workflows)
library(emayili)
library(tidyr)

## googlesheet De-auth setup
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

## Helper function to help format googlesheet (0.456 -> 45%)
format_prediction_to_percent <- function(sheet){
  x <- gs4_get(sheet)

  # determine targeted sheet ---------------------------------------------------
  range_spec <- googlesheets4:::as_range_spec(
    LETTERS[x$sheets$grid_columns[2]],
    sheet = 2,
    sheets_df = x$sheets, nr_df = x$named_ranges
  )

  # form request ---------------------------------------------------------------
  range_req <- googlesheets4:::as_GridRange(range_spec)
  cell_req <- list(
    userEnteredFormat = list(
      numberFormat = list(
        type = "NUMBER",
        pattern = "0%"
      )
    )
  )
  field_req <- "userEnteredFormat.numberFormat"

  req <- request_generate(
    "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = as_sheets_id(sheet),
      requests = list(
        repeatCell = list(
          range = range_req,
          cell = cell_req,
          fields = field_req
        )
      )
    )
  )
  resp_raw <- request_make(req)
  gargle::response_process(resp_raw)
}

## Load prediction model
readmit_model <- read_rds("inst/final_readmission_model.rds")

## Load googleshee where we store patient data
sheet_id <- "https://docs.google.com/spreadsheets/d/1vDQD7KyvDzJkuz-ZQBZ1EF4KMPNTrQwYhHRrvRt0xvo/"

## Read in data

## The first sheet is where the raw data is
patient_data <- read_sheet(sheet_id, sheet = 1)

email_patient_df <- patient_data |>
  select("patient_id", "email")

patient_data <- patient_data |> select(-"email")

## The second sheet is the process result
patient_data_with_pred <- read_sheet(sheet_id, sheet = 2)

## It's possible to have everything in one sheet tho, i felt it would be better to
## separate things but let me know if we should do that.

## We need to script the first process.
## The first time we would run this, there isn't any predicted result in the database,
## so we need to put in some data there.

## The code below would be commented out because we only need to run it once. (for the first time)
## So when you want to start everything again, you need to run this.

# pred_data <-
#   augment(readmit_model, patient_data) |>
#   select(all_of(colnames(patient_data)), "Prob Readmit" = ".pred_Yes")

## After predicting on the first batch, we need to save that result in the new sheet

# sheet_write(ss = sheet_id, data = pred_data, sheet = "Patient Data with Prediction")

## Now we've saved the first batch, how do we handle new data??

## Get patients that haven't been predicted

## The next time we want to work on new data, we have to filter out those that have been
## predicted on first

new_patients <- patient_data |> anti_join(patient_data_with_pred |> select("patient_id"))

### to find changes in the raw data

patient_with_pred <-
  patient_data |>
  inner_join(patient_data_with_pred |> select("patient_id")) |>
  pull(patient_id)

patient_raw_long <-
  patient_data |>
  filter(patient_id %in% patient_with_pred) |>
  mutate(across(-"patient_id", as.character)) |>
  pivot_longer(cols = -"patient_id", values_to = "value_raw")

patient__with_pred_long <-
  patient_data_with_pred |>
  filter(patient_id %in% patient_with_pred) |>
  mutate(across(-"patient_id", as.character)) |>
  pivot_longer(cols = -"patient_id", values_to = "value_pred")

id_with_change <-
  patient_raw_long |>
  left_join(patient__with_pred_long) |>
  filter(value_raw != value_pred) |>
  distinct(patient_id) |>
  pull()

if(length(id_with_change) > 0) {
  ## When there's a change in previously predicted data,
  ## Add it to new prediction data and remove old record.

  new_patients <-
    patient_data |>
    filter(patient_id %in% id_with_change) |>
    bind_rows(new_patients)

  patient_data_with_pred <-
    patient_data_with_pred |>
    filter(!patient_id %in% id_with_change)
}


## Since the automation task would run every 30 minutes, we put a conditional statement
## Process the data and send an email if we have new data.

if(nrow(new_patients) > 0) {

  ## Apply model on new patient data
  pred_data <-
    augment(readmit_model, new_patients) |>
    select(all_of(colnames(new_patients)), "Prob Readmit" = ".pred_Yes")

  ## Instead of overwriting the data, we need to append new data

  ## Before appending, let's confirm that the column names are still the same.
  ## they should be, but it best practice to confirm.


  if(!all(colnames(patient_data_with_pred) == colnames(pred_data))) {
    #waldo::compare(colnames(patient_data_with_pred), colnames(pred_data))
    stop("Prediction data and prediction database don't match")
  }


  if(length(id_with_change) > 0) {
    data_to_send <-
      patient_data_with_pred |>
      bind_rows(pred_data) |>
      arrange(patient_id)

    sheet_write(ss = sheet_id,
                data = data_to_send,
                sheet = "Patient Data with Prediction")
  } else {
    sheet_append(ss = sheet_id,
                 data = pred_data,
                 sheet = "Patient Data with Prediction")
  }


  ## Format the text col (i.e 0.564 -> 56%)
  format_prediction_to_percent(sheet_id)
}


if(nrow(new_patients) > 0) {

  pred_data <- left_join(pred_data, email_patient_df)

  for (i in unique(pred_data$email)) {

    email_data <- pred_data |>
      filter(.data$email == i)

    ## Prepare email message
    smtp <- emayili::server(
      host = "smtp.gmail.com",
      port = 465,
      username = Sys.getenv("GMAIL_USERNAME"),
      password = Sys.getenv("GMAIL_PASSWORD")
    )

    emayili <- envelope() %>%
      from("favour879@gmail.com") %>%
      to(i) %>%
      subject("New Readmission Prediction") %>%
      emayili::render(
        input = "R/email_content_for_patient.Rmd",
        params =  list(
          patient_id = email_data$patient_id,
          prob = email_data$`Prob Readmit`
        ),
        squish = F,
        include_css = "bootstrap"
      )

    smtp(emayili, verbose = TRUE)
  }

}

