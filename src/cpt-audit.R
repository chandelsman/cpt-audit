# load libraries
library(tidyverse)
library(here)

######################
### SET START DATE ###
######################
st_date <- date("2023-07-01")

####################
### SET END DATE ###
####################
end_date <- date("2023-09-30")

# make sampling function to include at least one accession when n < 10
# sample_up <- function(.data, frac) {
#   sample_n(.data, ceiling({{frac}} * n()) )
# }

# Import raw data
cases_raw <- 
  list.files(path = here("data"), 
             pattern = "\\d{4}\\D\\d",
             full.names = TRUE) |> 
  sapply(readxl::read_excel, simplify = FALSE) |> 
  bind_rows()

# Generate list of pathologists to include in audit
pathologists <-
  cases_raw |>
  filter(
    !str_detect(PATHOLOGIST, "^\\[x\\]"),
    !str_detect(PATHOLOGIST, "^Admin"),
    !str_detect(PATHOLOGIST, "^Caufield"),
    !str_detect(PATHOLOGIST, "^Cullen"),
    !str_detect(PATHOLOGIST, "^Leidy"),
    !str_detect(PATHOLOGIST, "^Lillis"),
    !str_detect(PATHOLOGIST, "^Sbicca"),
    !str_detect(PATHOLOGIST, "^Stearns"),
    !str_detect(PATHOLOGIST, "^Vogt"),
    !str_detect(PATHOLOGIST, "^Hoover"),
    !str_detect(PATHOLOGIST, "^Hibbert"),
    !str_detect(PATHOLOGIST, "^Shipman"),
    !str_detect(PATHOLOGIST, "Pathologist, Test")
  ) |>
  select(PATHOLOGIST) |> 
  distinct() |> 
  as_vector()

# Clean data and select 1% of cases per pathologist
cases_clean <-
  cases_raw |>
  mutate(Create = date(parse_date_time(Create, c("mdy HM", "mdy HMS")))) |>
  filter(
    Create >= st_date &
      Create <= end_date,!str_detect(PATHOLOGIST, "^\\[x\\]"),
    PATHOLOGIST %in% pathologists
  ) |>
  select(PATHOLOGIST, `RESULT ID`, Create, CPTS) |>
  group_by(PATHOLOGIST) |>
  sample_n(10)
# sample_up(0.01)

# Export results to review
writexl::write_xlsx(cases_clean,
                    here("output",
                         paste0(
                           year(st_date),
                           "q", quarter(st_date),
                           "-coding-audit.xlsx"
                         )))
  
                    