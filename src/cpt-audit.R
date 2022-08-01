# load libraries
library(tidyverse)
library(lubridate)
library(here)

### SET START DATE ###
st_date <- date("2022-01-01")

### SET END DATE ###
end_date <- date("2022-03-31")

# make sampling function to include at least one accession when n < 10
# sample_up <- function(.data, frac) {
#   sample_n(.data, ceiling({{frac}} * n()) )
# }

# Import raw data
cases_raw <- 
  list.files(path = here("data"), 
             pattern = "\\d{4}\\D\\d",
             full.names = TRUE) %>% 
  sapply(readxl::read_excel, simplify = FALSE) %>% 
  bind_rows()

# Clean data and select 1% of cases per pathologist
cases_clean <- 
  cases_raw %>% 
  mutate(Create = date(mdy_hm(Create))) %>% 
  filter(Create >= st_date & Create <= end_date,  
         !is.na(PATHOLOGIST), 
         PATHOLOGIST != "Admin, Summit",
         !str_detect(PATHOLOGIST, "^\\[x\\]")) %>% 
  select(PATHOLOGIST, `RESULT ID`, Create, CPTS) %>% 
  group_by(PATHOLOGIST) %>% 
  sample_n(3)
  # sample_up(0.01)

# Export results to review
writexl::write_xlsx(cases_clean,
                    here("output",
                         paste0(
                           year(st_date),
                           "q", quarter(st_date),
                           "-coding-audit.xlsx"
                         )))
  
                    