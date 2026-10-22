# Field reference for THIP's yearly inpatient tables -------------------------
#
# Writes data/thip_fields.rda. Descriptions are written for this package from
# THIP's published data dictionaries (the 2024 "Data Structure Description" and
# the 2023 field list); they are not a copy of them. The fields in those
# dictionaries are listed, plus `sdx_index`, which the notebook platform provides
# alongside the documented diagnosis columns.
#
# Run from the package root: Rscript data-raw/thip_fields.R

row <- function(field, group, description, format, codes = "") {
  data.frame(field = field, group = group, description = description,
             format = format, codes = codes)
}
numbered <- function(stem, n, group, description, format) {
  row(paste0(stem, seq_len(n)), group,
      sprintf(description, seq_len(n)), format)
}

dischs_codes <- paste(
  "1 complete recovery; 2 improved; 3 not improved; 4 normal delivery;",
  "5 not delivered; 6 normal newborn discharged with mother;",
  "7 normal newborn discharged separately; 8 stillbirth; 9 dead")
discht_codes <- paste(
  "1 with approval; 2 against advice; 3 escaped; 4 transferred; 5 other;",
  "8 dead, autopsy; 9 dead, no autopsy")

thip_fields <- rbind(
  row("tran_id", "Identifiers", "Key of one inpatient record (hashed)", "text"),
  row("pid", "Identifiers",
      "Key of one person, hashed from their national ID, alien card or passport number",
      "text"),
  row("sex", "Person", "Sex", "code", "1 male; 2 female"),
  row("marry_status", "Person", "Marital status", "code",
      "1 single; 2 married; 3 widowed or divorced; 4 other"),
  row("age_y", "Person", "Age in whole years", "whole number as text"),
  row("weight", "Person",
      "Weight at admission in kilograms; required for newborns", "number as text"),
  row("dateadm", "Admission and discharge", "Admission date", "YYYYMMDD"),
  row("datedsc", "Admission and discharge", "Discharge date", "YYYYMMDD"),
  row("timeadm", "Admission and discharge", "Admission time", "HHMM"),
  row("timedsc", "Admission and discharge", "Discharge time", "HHMM"),
  row("los", "Admission and discharge", "Length of stay in days", "whole number as text"),
  row("mth", "Admission and discharge", "Admission month", "month number"),
  row("g_year", "Admission and discharge",
      "Fiscal year in the Buddhist era (2567 = October 2023 to September 2024)", "YYYY"),
  row("dischs", "Admission and discharge",
      "Discharge status: the patient's condition at discharge", "code", dischs_codes),
  row("discht", "Admission and discharge",
      "Discharge type: how the patient left hospital", "code", discht_codes),
  row("drg_nhso", "Payment group", "Diagnosis-related group used for payment", "code"),
  row("rw_nhso", "Payment group", "Relative weight of the diagnosis-related group",
      "decimal as text"),
  row("adjrw_nhso", "Payment group",
      "Relative weight adjusted for the actual length of stay", "decimal as text"),
  row("pdx", "Diagnoses", "Principal diagnosis for this admission",
      "ICD-10 code (Thai ICD-10-TM) without a dot"),
  numbered("sdx", 20, "Diagnoses", "Secondary diagnosis %d",
           "ICD-10 code (Thai ICD-10-TM) without a dot"),
  row("sdx_index", "Diagnoses",
      "Secondary diagnoses sdx1-sdx20 joined by spaces (not in the dictionary)",
      "codes separated by spaces"),
  numbered("proc", 21, "Procedures", "Procedure %d", "ICD-9-CM code without a dot"),
  row("death_date", "Death", "Date of death, from death data linked to the person",
      "YYYY-MM-DD"),
  row("dcause_oth", "Death", "Cause of death 1", "text"),
  row("dcause_oth1", "Death", "Cause of death 2", "text"),
  row("dcause_oth2", "Death", "Cause of death 3", "text"),
  row("health_region", "Geography", "Health region", "two digits, 01 to 13"),
  row("superdistrict", "Geography",
      "Super-district of the treating facility: a group of districts in one province",
      "code")
)
rownames(thip_fields) <- NULL
stopifnot(!anyDuplicated(thip_fields$field), nrow(thip_fields) == 67L)

save(thip_fields, file = "data/thip_fields.rda", compress = "xz")
