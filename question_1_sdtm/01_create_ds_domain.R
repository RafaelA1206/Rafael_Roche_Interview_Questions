
# Program : 01_create_ds_domain.R
# Purpose : Create SDTM DS domain using sdtm.oak package
# Inputs  : pharmaverseraw::ds_raw, sdtm_ct.csv, pharmaversesdtm::dm
# Output  : question_1_sdtm/ds.rds, ds.csv

##################### QUESTION 1. ################################
# Create an SDTM Disposition (DS) domain dataset from raw clinical trial data using {sdtm.oak}.
##################################################################


#Variables needed are ::: STUDYID(assign it ), DOMAIN(assing it ), USUBJID(assign it from DM ), DSSEQ, 
#DSTERM[raw.IT.DSTERM], DSDECOD[raw.IT.DSDECOD], DSCAT(use CRF conditions), VISITNUM(use CT), VISIT(use CT), DSDTC, DSSTDTC, DSSTDY



#packages needed
library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)



################
#RAW/DATA NEEDED
################

ds_raw <- pharmaverseraw::ds_raw 
dm<- pharmaversesdtm::dm

#Controlled terminology
study_ct <- read_ct_spec("sdtm_ct.csv")



# generate_oak_id_vars() adds 3 key columns to the raw data:
#   oak_id         - row identifier
#   raw_source     - name of the raw dataset ("ds_raw")
#   patient_number - subject ID taken from PATNUM
# Every {sdtm.oak} mapping below joins on these keys (oak_id_vars())


raw1 <-ds_raw %>% generate_oak_id_vars(pat_var="PATNUM", raw_src ="ds_raw")
head(raw1)
head(ds_raw)

ds<- assign_no_ct(raw_dat =raw1,raw_var="IT.DSTERM",tgt_var = "DSTERM",id_vars = oak_id_vars()) %>%
  #Per CRF: when OTHERSP is populated, DSTERM = OTHERSP (otherwise IT.DSTERM)
  assign_no_ct(raw_dat =condition_add(raw1, !is.na(OTHERSP)),raw_var="OTHERSP",tgt_var = "DSTERM",id_vars = oak_id_vars())  


#CHECKS; for no records lsot , every record has a DSTERM
nrow(ds) == nrow(raw1)
sum(is.na(ds$DSTERM)) 
names(ds)


#DISPOSITION EVENTS
ds1 <- ds %>% assign_ct(raw_dat = condition_add(raw1, is.na(OTHERSP) & IT.DSDECOD != "Randomized"),
                        raw_var = "IT.DSDECOD",
                        tgt_var = "DSDECOD",
                        ct_spec = study_ct,
                        ct_clst="C66727", #this is for completion/Reason for Non completion codelist
                        id_vars =oak_id_vars()
                        ) %>%
  
  hardcode_no_ct(
    raw_dat=condition_add(raw1,IT.DSDECOD %in% "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    tgt_val = "RANDOMIZED",
    id_vars=oak_id_vars() )%>%
  
  assign_no_ct(raw_dat=condition_add(raw1, !is.na(OTHERSP)),
               raw_var = "OTHERSP",
               tgt_var="DSDECOD",
               id_vars = oak_id_vars()
               
  ) %>%
  
  mutate(DSDECOD=toupper(DSDECOD))
    
    
#DSCAT.   
#Per CRF: Randomized we set to PROTOCOL MILESTONE; OTHERSP not null we set to OTHER EVENT,  otherwise we call it as DISPOSITION EVENT

ds2 <-ds1 %>% hardcode_no_ct( raw_dat = condition_add(raw1, IT.DSDECOD %in% "Randomized"), 
                              raw_var = "IT.DSDECOD",
                              tgt_var = "DSCAT",
                              tgt_val= "PROTOCOL MILESTONE",
                              id_vars =oak_id_vars() ) %>%
  
              hardcode_no_ct( raw_dat = condition_add(raw1, !is.na(OTHERSP)),
                              raw_var = "OTHERSP",
                              tgt_var = "DSCAT",
                              tgt_val = "OTHER EVENT",
                              id_vars = oak_id_vars() ) %>%
  
  
              hardcode_no_ct( raw_dat = condition_add(raw1, is.na(OTHERSP) & !IT.DSDECOD %in% "Randomized"),
                              raw_var = "IT.DSDECOD",
                              tgt_var = "DSCAT",
                              tgt_val = "DISPOSITION EVENT",
                              id_vars = oak_id_vars() ) 
            
                            
#CHECKS
count(ds2, DSCAT)
sum(is.na(ds2$DSCAT))


#DATES -- CONVERT INTO ISO 8601 format // IT.DSSTDAT
head(raw1)
ds3 <-ds2 %>% assign_datetime(raw_dat = raw1,
                              raw_var="IT.DSSTDAT",
                              tgt_var = "DSSTDTC",
                              raw_fmt = "mm-dd-yyyy",
                              id_vars = oak_id_vars()) %>%
  
              assign_datetime(raw_dat = raw1,
                              raw_var = c("DSDTCOL","DSTMCOL"),
                              tgt_var = "DSDTC",
                              raw_fmt = c("mm-dd-yyyy", "H:M"),
                              id_vars = oak_id_vars() 
                              )


#Checks
raw1 %>% select(IT.DSSTDAT, DSDTCOL, DSTMCOL) %>% head(10)
ds3  %>% select(DSSTDTC, DSDTC) %>% head(10)


###################
#VISIT and visitnum
###################

ds4 <- ds3 %>%
  assign_ct(
    raw_dat = raw1,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = raw1,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  ) %>%
  
  mutate(
    VISIT=toupper(VISIT),
    VISITNUM= case_when(
      VISIT== "AMBUL ECG REMOVAL" ~"6",
      grepl( "^UNSCHEDULED", VISIT)~sub("UNSCHEDULED ","",VISIT),
      TRUE ~ VISITNUM
    ),
    VISITNUM=as.numeric(VISITNUM)
  )

count(ds4, VISIT, VISITNUM)
sum(is.na(ds4$VISITNUM))


############################
#STUDYID DOMAIN AND usubjid
############################



ds5 <- ds4%>% 
  assign_no_ct( raw_dat =raw1,
                raw_var = "STUDY",
                tgt_var="STUDYID",
                id_vars = oak_id_vars()
                ) %>%
  mutate( DOMAIN="DS",
          USUBJID=paste0 ("01-",patient_number)  #USUBJID is not defined on CRF, we will just follow DM convention from the pharmaversesdtm package.
          )


# Checks
count(ds5, STUDYID, DOMAIN)
head(ds5$USUBJID)
sum(!ds5$USUBJID %in% dm$USUBJID)


#DERIVE DSSTDY, its easier to do it at this step than before since we created USUBJID on the previous step for ds5, otherwise we cannot merge back to DM for the reference dates
ds6 <-ds5 %>% mutate(DSSTDTC=as.character(DSSTDTC),
                     DSDTC=as.character(DSDTC)) %>%
              derive_study_day(dm_domain = dm,
                               tgdt="DSSTDTC",
                               refdt="RFSTDTC",
                               study_day_var = "DSSTDY")

ds7<- ds6 %>%
  derive_seq(tgt_var = "DSSEQ",
             rec_vars = c("USUBJID","DSSTDTC","VISITNUM","DSDECOD")
             )

###CHECKS

ds7 %>% select(USUBJID, DSSEQ, DSDECOD, DSSTDTC, DSSTDY) %>% head(10)
sum(is.na(ds7$DSSTDY)) #We have about 52 missing dates, could be due screening failure, double checking on the step after
sum(duplicated(ds7[c("USUBJID", "DSSEQ")]))

#Checking for screen failures~ we have about 52 patients ####
ds7 %>%
  filter(is.na(DSSTDY)) %>%
  left_join(select(dm, USUBJID, RFSTDTC), by = "USUBJID") %>%
  count(DSDECOD, missing_RFSTDTC = is.na(RFSTDTC))



final_DS<- ds7 %>%
  mutate(DSSTDTC=format (as.Date(DSSTDTC), "%Y-%m-%d")) %>%
  select(STUDYID,DOMAIN,USUBJID,DSSEQ,DSTERM,DSDECOD,DSCAT,VISITNUM,VISIT,DSDTC,DSSTDTC,DSSTDY) %>%
  arrange(USUBJID,DSSEQ)


# Final checks
str(final_DS)                 # 850 obs, 12 variables, DSSTDTC as chr
count(final_DS, DSCAT, DSDECOD)




#CHECK PLUS SAVE LOG 


dir.create("question_1_sdtm", showWarnings = FALSE)
saveRDS(final_DS, "question_1_sdtm/ds.rds")
write.csv(final_DS, "question_1_sdtm/ds.csv", row.names = FALSE, na = "")

