# create the log file
sink("question_2_adam/create_adsl.log", split = TRUE)
cat("create_adsl.R\n")
cat("Execution started at:", Sys.time(), "\n\n")

# load the required packages into R
library(admiral)
library(dplyr)

# start by assigning pharmaversesdtm::dm to an adsl object
dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs
dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)
adsl <- dm %>% select(-DOMAIN)

# regroup the age variable 
agegr1_lookup <- exprs(
  ~condition, ~AGEGR9, ~AGEGR9N,
  AGE < 18, "<18", 1,
  between(AGE, 18, 50), "18-50", 2,
  AGE > 50, ">50", 3
)
adsl_cat <- derive_vars_cat(
  dataset = adsl,
  definition = agegr1_lookup
)

# create the variables "TRTSDTM" and "TRTSTMF"
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )
adsl <- adsl %>%
  # Treatment Start Datetime
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) 

# Set to "Y" if [DM.ARM] not equal to missing Else set to "N"
adsl <- adsl %>% mutate(ITTFL = ifelse(!is.na(ARM) & ARM != "", "Y", "N"))

# create the variable "LSTAVLDT"
vs_ext <- vs %>%
  derive_vars_dt(dtc = VSDTC, new_vars_prefix = "VS") %>%
  filter(!is.na(VSDT) & !(is.na(VSSTRESN) & is.na(VSSTRESC)))

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = vs_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(LSTVS = VSDT),
    order = exprs(VSDT),
    mode = "last"
  )

ae_ext <- ae %>%
  derive_vars_dt(dtc = AESTDTC, new_vars_prefix = "AEST") %>%
  filter(!is.na(AESTDT))

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ae_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(LSTAE = AESTDT),
    order = exprs(AESTDT),
    mode = "last"
  )

ds_ext <- ds %>%
  derive_vars_dt(dtc = DSSTDTC, new_vars_prefix = "DSST") %>%
  filter(!is.na(DSSTDT))

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(LSTDS = DSSTDT),
    order = exprs(DSSTDT),
    mode = "last"
  )

# Treatment candidate: use TRTEDTM (last exposure), take datepart
adsl <- adsl %>%
  mutate(LSTTRT = as.Date(TRTSDTM))

adsl <- adsl %>%
  mutate(
    LSTAVLDT = pmax(LSTVS, LSTAE, LSTDS, LSTTRT, na.rm = TRUE)
  )

cat("\nExecution completed at:", Sys.time(), "\n")
sink()





