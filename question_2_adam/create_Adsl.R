

# Program : create_ßADSL.R
# Purpose : Create ADaM ADSL dataset using admiral package
# Inputs : pharmaversesdtm::dm, vs, ex, ds, ae
# Output : question_2_adam/adsl.rds, adsl.csv

#Question 2: ADaM ADSL Dataset Creation

#packages needed
library(admiral)
library(pharmaversesdtm)
library(dplyr)
library(lubridate)
library(stringr)




dm<-convert_blanks_to_na(pharmaversesdtm::dm)
vs<-convert_blanks_to_na(pharmaversesdtm::vs)
ex<-convert_blanks_to_na(pharmaversesdtm::ex)
ds<-convert_blanks_to_na(pharmaversesdtm::ds)
ae<-convert_blanks_to_na(pharmaversesdtm::ae)



#TASK: Derive these following variables : AGEGR9 & AGEGR9N ,TRTSDTM & TRTSTMF ,ITTFL, LSTAVLDT

#DM is the basis for ADSL , should be one row per subject
adsl1 <- dm %>%
  select(-DOMAIN)
nrow(adsl1)
n_distinct(adsl1$USUBJID)   # should equal nrow(adsl)


# AGEGR9/AGEGR9N lookup; between() is inclusive, so age 50 falls in "18 - 50"
agegr9_cat<-exprs(
  ~condition, ~AGEGR9, ~AGEGR9N,
  AGE <18, "<18", 1 ,
  between(AGE,18,50), "18 - 50", 2,
  AGE>50, ">50",  3
)
# No subjects under 18 in this study, so "<18" does not appear


adsl <-adsl1 %>% derive_vars_cat(definition=agegr9_cat)

###Checks

count(adsl, AGEGR9, AGEGR9N)
adsl %>% group_by(AGEGR9) %>% summarise(min = min(AGE), max = max(AGE))



#### GET TRT START DATES AND END DATEs
# highest_imputation = "h": only the time is imputed, never the date,
# so incomplete dates get NA and are excluded below
# ignore_seconds_flag = TRUE: no TRTSTMF if only seconds are missing
ex_ec<- ex %>%
  derive_vars_dtm(
    dtc=EXSTDTC,
    new_vars_prefix = "EXST", 
    highest_imputation = "h",
    time_imputation = "first", #missing time = 00:00:00 in return
    ignore_seconds_flag = TRUE
  ) %>%
  
  derive_vars_dtm(
    dtc=EXENDTC,
    new_vars_prefix = "EXEN",
    highest_imputation = "h",
    time_imputation = "last", #missing end time = 23:59:59
    ignore_seconds_flag = TRUE
  )
  

# Valid dose per spec: EXDOSE > 0, or EXDOSE = 0 for PLACEBO
# (placebo subjects must count as treated)
  valid_dose <- expr(
    EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))
  )  
  
  
  
adsl2<- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ec,
    filter_add = !!valid_dose & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM=EXSTDTM, TRTSTMF =EXSTTMF) , 
    order = exprs (EXSTDTM,EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID,USUBJID)
  ) %>%
  
  
  derive_vars_merged(
    dataset_add = ex_ec,
    filter_add = !!valid_dose & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM=EXENDTM) , 
    order = exprs (EXENDTM,EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID,USUBJID)
  )
  
  
adsl2 %>% select(USUBJID, ARM, TRTSDTM, TRTSTMF, TRTEDTM) %>% head(10)
count(adsl2, TRTSTMF)
count(adsl2, ARM, has_trtsdtm = !is.na(TRTSDTM))
  



####ITTFL

#Spec mentions  =Y if DM.ARM ^= '' , else N 


adsl3 <- adsl2 %>% mutate(ITTFL=if_else(!is.na(ARM),"Y","N"))

#check
count(adsl3, ARM, ITTFL)



###LAST KNOWN ALIVE DATE(LSTAVLDT)


adsl4 <-adsl3 %>% 
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID,USUBJID),
    events=list(
      event(
        dataset_name = "vs",
        order = exprs(VSDTC,VSSEQ),
        condition = !(is.na(VSSTRESN) & is.na(VSSTRESC)) &
          !is.na(convert_dtc_to_dt(VSDTC,highest_imputation ="n")),
        set_values_to = exprs(
          LSTAVLDT= convert_dtc_to_dt(VSDTC, highest_imputation = "n"),
          seq=VSSEQ
        )
      ),
      event(
        dataset_name = "ae",
        order = exprs (AESTDTC,AESEQ),
        condition = !is.na(convert_dtc_to_dt(AESTDTC,highest_imputation = "n")),
        set_values_to = exprs(LSTAVLDT=convert_dtc_to_dt(AESTDTC,highest_imputation = "n"),
                              seq=AESEQ
                              
        )
      ),
      
      event(dataset_name = "ds",
            order = exprs(DSSTDTC,DSSEQ),
            condition = !is.na(convert_dtc_to_dt(DSSTDTC, highest_imputation = "n")),
            set_values_to = exprs(LSTAVLDT=convert_dtc_to_dt(DSSTDTC,highest_imputation = "n"),
                                  seq=DSSEQ)),
      event( dataset_name = "adsl",
             condition = !is.na(TRTEDTM),
             set_values_to = exprs(
               LSTAVLDT=as.Date(TRTEDTM),
               seq=0
             )
        )
      ),
    
    source_datasets = list(vs=vs,ae=ae,ds=ds,adsl=adsl3),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTAVLDT, seq, event_nr),
    mode = "last",
    new_vars = exprs(LSTAVLDT)
  )

###CHECKS
adsl4 %>% select(USUBJID, ARM, TRTEDTM, LSTAVLDT) %>% head(10)
sum(is.na(adsl4$LSTAVLDT)) #No subjects missing last known alive date, count=0 

adsl4 %>% filter(!is.na(TRTEDTM), LSTAVLDT < as.Date(TRTEDTM)) %>% nrow() # LSTAVLDT is not supposed to be before last trt date, count=0 so all good.

    
dir.create("question_2_adam", showWarnings = FALSE)
saveRDS(adsl4, "question_2_adam/adsl.rds")
write.csv(adsl4, "question_2_adam/adsl.csv", row.names = FALSE, na = "")

sessionInfo()
  
  
  
  
  
  
  
  
  