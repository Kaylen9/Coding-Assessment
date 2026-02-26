# create the log file
sink("question_1_sdtm/01_create_ds_domain.log", split = TRUE)
cat("01_create_ds_domain.R\n")
cat("Execution started at:", Sys.time(), "\n\n")

# load the required packages
library(sdtm.oak)
library(tidyverse)
library(dplyr)
library(readr)

# load the raw data into R
ds_raw <- pharmaverseraw::ds_raw

# create oak_id_vars
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# create a variable DSCAT: If IT.DSDECOD = Randomized then DSCAT = "PROTOCOL MILESTONE" else DSCAT = "DISPOSITION EVENT"
ds_raw <- ds_raw %>%
  mutate(
    DSCAT = if_else(
      IT.DSDECOD == "Randomized",
      "PROTOCOL MILESTONE",
      "DISPOSITION EVENT"
    )
  )

# Select relevant variables and prepare for creating DS domain
ds <- ds_raw %>%
  mutate(
    SUBJID = substr(PATNUM, 5, 8)
  ) %>%
  select(oak_id, raw_source, patient_number, SUBJID)

# download the Study controlled terminology from the github
study_ct <- read.csv("https://raw.githubusercontent.com/pharmaverse/examples/refs/heads/main/metadata/sdtm_ct.csv")

# Create the variable "VISITNUM"
ds <- ds %>% assign_ct(
  raw_dat = ds_raw,
  raw_var = "INSTANCE",
  tgt_var = "VISITNUM",
  ct_spec = study_ct,
  ct_clst = "VISITNUM",
  id_vars = oak_id_vars()) 

# Create the variable "VISIT"
ds <- ds %>% assign_ct(
  raw_dat = ds_raw,
  raw_var = "INSTANCE",
  tgt_var = "VISIT",
  ct_spec = study_ct,
  ct_clst = "VISIT",
  id_vars = oak_id_vars()
) 

# Map the value in DSDTCOL & DSTMCOL to DSDTC in ISO8601 format
ds <- ds %>% assign_datetime(
  raw_dat = ds_raw,
  raw_var = c("DSDTCOL", "DSTMCOL"),
  tgt_var = "DSDTC",
  raw_fmt = list(
    c("m-d-y"),        
    c("H:M", "H:M:S") 
  ),
  raw_unk = c("UNK", "UN", "--", "-", ".", ""),
  id_vars = oak_id_vars()
)

# Create the variable "DSSEQ"
ds <- ds %>%
  group_by(patient_number) %>%
  arrange(DSDTC, .by_group = TRUE) %>%
  mutate(DSSEQ = row_number()) %>%
  ungroup()

# If OTHERSP is not null, then map the value in OTHERSP to DSTERM
ds <- ds %>% assign_no_ct(
  raw_dat = ds_raw,
  raw_var = "OTHERSP",
  tgt_var = "DSTERM",
  id_vars = oak_id_vars()
)

# If OTHERSP is not null then map the value in OTHERSP to DSDECOD
ds <- ds %>% assign_ct(
  raw_dat = ds_raw,
  raw_var = "OTHERSP",
  tgt_var = "DSDECOD",
  ct_spec = study_ct,
  ct_clst = "C66727",
  id_vars = oak_id_vars()
)

# Map the value in IT.DSSDAT to DSSTDTC in ISO8601 format
ds <- ds %>% assign_datetime(
  raw_dat = ds_raw,
  raw_var = "IT.DSSTDAT",
  tgt_var = "DSSTDTC",
  raw_fmt = list(c("m-d-y")),
  raw_unk = c("UNK", "UN", "", "--"),
  id_vars = oak_id_vars()
)

# Map the DSCAT from the raw data to the DS domain
ds <- ds %>% assign_no_ct(
  raw_dat = ds_raw,
  raw_var = "DSCAT",
  tgt_var = "DSCAT",
  id_vars = oak_id_vars()
)

# If OTHERSP is not null then map DSCAT = OTHER EVENT
ds$DSCAT[!is.na(ds$DSDECOD)] <- "OTHER EVENT"

## If OTHERSP is null then map the value in IT.DSTERM to DSTERM
ds <- ds %>% assign_no_ct(
  raw_dat = ds_raw,
  raw_var = "IT.DSTERM",
  tgt_var = "IT.DSTERM",
  id_vars = oak_id_vars()
)
ds <- ds %>% mutate(DSTERM = ifelse(is.na(DSTERM), `IT.DSTERM`, DSTERM))

# If OTHERSP is null then map the value in IT.DSDECOD to DSDECOD 
ds <- ds %>% assign_ct(
  raw_dat = ds_raw,
  raw_var = "IT.DSDECOD",
  tgt_var = "IT.DSDECOD",
  ct_spec = study_ct,
  ct_clst = "C66727",
  id_vars = oak_id_vars()
)
ds <- ds %>% mutate(DSDECOD = ifelse(is.na(DSDECOD), `IT.DSDECOD`, DSDECOD))

# get the variable "RFSTDTC", which is the reference Start Date/time for the subject in ISO 8601 character format
dm_raw <- pharmaverseraw::dm_raw
ec_raw <- pharmaverseraw::ec_raw
dm_raw <- dm_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "dm_raw"
  )
ec_raw <- ec_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ec_raw"
  )
ref_date_conf_df <- tibble::tribble(
  ~raw_dataset_name, ~date_var,     ~time_var,      ~dformat,      ~tformat, ~sdtm_var_name,
  "ec_raw",       "IT.ECSTDAT", NA_character_, "dd-mmm-yyyy", NA_character_,     "RFXSTDTC",
  "ec_raw",       "IT.ECENDAT", NA_character_, "dd-mmm-yyyy", NA_character_,     "RFXENDTC",
  "ec_raw",       "IT.ECSTDAT", NA_character_, "dd-mmm-yyyy", NA_character_,      "RFSTDTC",
  "ec_raw",       "IT.ECENDAT", NA_character_, "dd-mmm-yyyy", NA_character_,      "RFENDTC",
  "dm_raw",            "IC_DT", NA_character_,  "mm/dd/yyyy", NA_character_,      "RFICDTC",
  "ds_raw",          "DSDTCOL",     "DSTMCOL",  "mm-dd-yyyy",         "H:M",     "RFPENDTC",
  "ds_raw",          "DEATHDT", NA_character_,  "mm/dd/yyyy", NA_character_,       "DTHDTC"
)

dm <- dm_raw %>%
  mutate(
    SUBJID = substr(PATNUM, 5, 8)
  ) %>%
  select(oak_id, raw_source, patient_number, SUBJID)
dm <- dm %>%
  # Derive RFSTDTC using oak_cal_ref_dates
  oak_cal_ref_dates(
    ds_in = .,
    der_var = "RFSTDTC",
    min_max = "min",
    ref_date_config_df = ref_date_conf_df,
    raw_source = list(
      ec_raw = ec_raw,
      ds_raw = ds_raw,
      dm_raw = dm_raw
    )
  )

# create the variable "DSSTDY"
ds <- ds %>%
  left_join(
    dm %>% select(patient_number, RFSTDTC),
    by = "patient_number"
  ) %>%
  mutate(
    # Convert ISO8601 strings to Date
    DSSTDTC_date = as.Date(substr(as.character(DSSTDTC), 1, 10)),
    RFSTDTC_date = as.Date(substr(as.character(RFSTDTC), 1, 10)),
    DSSTDY = ifelse(
      !is.na(DSSTDTC_date) & !is.na(RFSTDTC_date),
      ifelse(
        DSSTDTC_date >= RFSTDTC_date,
        as.integer(DSSTDTC_date - RFSTDTC_date) + 1L,
        as.integer(DSSTDTC_date - RFSTDTC_date)
      ),
      NA_integer_
    )
  ) 

# create variables "STUDYID", "DOMAIN", and "USUBJID" and select relevant variables
ds <- ds %>%
  mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
  ) %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", "VISITNUM", "VISIT", "DSDTC",
    "DSSTDTC", "DSSTDY")


write_csv(ds, "question_1_sdtm/ds.csv")

cat("\nExecution completed at:", Sys.time(), "\n")
sink()








