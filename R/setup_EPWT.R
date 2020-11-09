library(here)
library(readxl)
library(data.table)
raw_data <- readxl::read_xlsx(
  here("data/raw/EPWT6.0.xlsx"), sheet = 1, skip = 75, 
  .name_repair = "universal")
data.table::fwrite(raw_data, file = here("data/tidy/EPWT6.csv"))
