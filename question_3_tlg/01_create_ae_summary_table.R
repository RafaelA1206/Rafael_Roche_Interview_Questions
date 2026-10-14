
# Program : 01_create_ae_summary_table.R
# Purpose : Summary table of treatment-emergent AEs by SOC and term
# Inputs  : pharmaverseadam::adae, pharmaverseadam::adsl
# Output  : question_3_tlg/ae_summary_table.html (and .pdf)

#Question 3: TLG - Adverse Events Reporting
#Create a summary table of treatment-emergent adverse events (TEAEs).

library(gtsummary)
library(pharmaverseadam)
library(gt)
library(dplyr)

#data needed

adae <-pharmaverseadam::adae
adsl<-pharmaverseadam::adsl


#Since we are dealing with a safety output, we should keep only safety records on ADSL.
#For ADAE , we should only keep Trt emergent records +. safety population =Y 

adsl<-adsl%>% filter(SAFFL=="Y")
adae<-adae%>%filter(SAFFL=="Y" ,TRTEMFL=="Y")


# Checks
count(adsl, ACTARM)                      # Big_N number for trt cols
nrow(adae)                               # number of TEAE records
n_distinct(adae$USUBJID)                 # subjects with at least one TEAE
count(adae, ACTARM)

##1.Create Table
# id = USUBJID: each subject counted once per SOC/term row (subjects only)
# denominator = adsl: percentages use the safety opulation N per ACTARM
# "..ard_hierarchical_overall.." is gtsummary's reserved name for the "Any TEAE" row, its basically a local parameter callout
# add_overall(): Total column; sort_hierarchical(): descending frequency


table_teae <- adae %>%
  tbl_hierarchical(
    variables= c(AESOC,AETERM),
    by = ACTARM,
    id= USUBJID,
    denominator =adsl,
    overall_row = TRUE,
    label = list( "..ard_hierarchical_overall.." ~ "Any treatment-emergent adverse event", AESOC ~ "Primary System Organ Class", AETERM ~ "Reported Term" )
  ) %>%
  add_overall(last = TRUE, col_label = "**Total**  \nN = {N}") %>%
  sort_hierarchical()

#Just to check the output
#table_teae



table_teae_gt <- table_teae %>%
  modify_caption("**Table 1. Summary of Treatment-Emergent Adverse Events by Primary System Organ Class and Reported Term**") %>%
  as_gt() %>%
  gt::tab_source_note(
    "Safety population (SAFFL = 'Y'). TEAEs: TRTEMFL = 'Y'. Subjects are counted once per SOC and once per term. Percentages based on N per actual treatment arm (ACTARM). Sorted by descending frequency."
  )

# Save as HTML (required output) and PDF (PDF needs webshot2 + Google Chrome)
gt::gtsave(table_teae_gt, "question_3_tlg/ae_summary_table.html")
gt::gtsave(table_teae_gt, "question_3_tlg/ae_summary_table.pdf")



sessionInfo()






