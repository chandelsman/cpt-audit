# load libraries
library(tidyverse)
library(lubridate)
library(here)

# Set parameters
params <- 
  tibble(
    dt_start = as.POSIXct("2023-08-01 00:00:01"),
    dt_end = as.POSIXct("2023-08-31 11:59:59"),
    path_id = "D"
  )

# Import raw case data
cases_raw <- 
  list.files(path = here("data"), 
             pattern = "\\d{4}-\\d{2}_audit-100",
             full.names = TRUE) |> 
  sapply(readxl::read_excel, simplify = FALSE) |> 
  bind_rows()

# Import pathologist ID key
pathologists <- readxl::read_excel(here("data", "pathologist-key.xlsx"))

# Clean case data
cases_clean <-
  cases_raw |>
  mutate(original_release =
           as.POSIXct(
             parse_date_time(original_release, c("mdy HM", "mdy HMS"))),
         pathologist = str_extract(pathologist, "^([^,]*)")) |>
  left_join(pathologists, by = "pathologist") |> 
  filter(status == "Completed",
         code == params$path_id,
         original_release >= params$dt_start &
           original_release <= params$dt_end) |> 
  arrange(original_release) |> 
  slice(100:199)

# Write data to a spreadsheet
writexl::write_xlsx(cases_clean,
                    here::here(
                      "output",
                      paste0(
                        year(params$dt_start),
                        "-",
                        if_else(month(params$dt_start) < 10, "0", ""),
                        month(params$dt_start),
                        "_Audit100",
                        "_Pathologist-",
                        params$path_id,
                        ".xlsx"
                      )
                    ))
