# create the log file
sink("question_3_tlg/01_create_ae_summary_table.log", split = TRUE)
cat("Running 01_create_ae_summary_table.R\n")
cat("Execution started at:", Sys.time(), "\n\n")

# load the required packages into R
library(dplyr)
library(gtsummary)
library(gt)

# load data into R
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# avoid counting multiple events
adae_y <- adae %>%
  filter(TRTEMFL == "Y") %>%
  distinct(USUBJID, ACTARM, AETERM) 

# make the table
table1 <- adae_y %>%
  select(AETERM, ACTARM) %>%
  tbl_summary(
    by = ACTARM,
    statistic = all_categorical() ~ "{n} ({p}%)",
    percent = "column",
    sort = all_categorical() ~ "frequency"
  ) %>%
  add_overall(last = TRUE) %>%  # adds Total column
  modify_header(label = "**Treatment-emergent adverse events**") %>%
  bold_labels()

# save the summary table as "ae_summary_table.html"
table1 %>%
  as_gt() %>%
  gtsave("question_3_tlg/table1.html")

cat("\nExecution completed at:", Sys.time(), "\n")
sink()
