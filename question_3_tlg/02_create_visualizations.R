

# Program : 02_create_visualizations.R
# Purpose : AE visualizations with {ggplot2}
#           Plot 1: AE severity distribution by treatment
#           Plot 2: Top 10 most frequent AEs with 95% CI
# Inputs  : pharmaverseadam::adae, pharmaverseadam::adsl
# Output  : question_3_tlg/plot1_ae_severity_by_treatment.png
#           question_3_tlg/plot2_top10_ae_incidence_ci.png

#################################
# 2. Visualizations using {ggplot2)
#################################

library(pharmaverseadam)
library(dplyr)
library(ggplot2)

#DATA

# Safety population (denominator for Plot 2)
# Plots use all AEs in the safety population (SAFFL = "Y", 1,191 records).
# The plot spec does not specifies  TEAEs, and this matches the example output.
# (The summary table uses TEAEs only, per its own spec.)

adsl <- pharmaverseadam::adsl %>% filter(SAFFL == "Y")
adae_plot <- pharmaverseadam::adae %>% filter(SAFFL == "Y")

#Plot 1: AE severity distribution by treatment (bar chart or heatmap). AE Severity is
#captured in the AESEV variable in pharmaverseadam::adae dataset.

# AESEV as ordered factor so bars stack MILD -> MODERATE -> SEVERE
tox_grades<-adae_plot %>% mutate(AESEV=factor(AESEV, levels=c("MILD","MODERATE","SEVERE"))) %>%
  count(ACTARM,AESEV)

#Check output
#tox_grades



plot1 <-ggplot(tox_grades, aes(x=ACTARM,y=n, fill=AESEV)) + 
  geom_col(width = 0.6,color="white",linewidth=0.5) + 
  scale_fill_manual( values= c(MILD = "#9ECAE1", MODERATE = "#4292C6", SEVERE = "#08306B"),
                     name="Severity/Intensity") +
  labs(
    title= "Adverse Events by Severity",
    subtitle = "Safety population; counts of AE records",
    x = "Actual Treatment Arm",
    y="Count of AEs" ) +
  theme_minimal(base_size=12)+
  theme(panel.grid.major.x = element_blank())



plot1

##Check numbers, make sure you are just using AEs where SAFFL=Y , not trtemfl=Y exclusively.
tox_grades %>% group_by(ACTARM) %>% summarise(total = sum(n))
count(adae_plot, TRTEMFL)
nrow(pharmaverseadam::adae)
count(pharmaverseadam::adae, SAFFL)
pharmaverseadam::adae %>% count(ACTARM)






#Plot 2: Top 10 most frequent AEs (with 95% CI for incidence rates). AEs are captured in
#the AETERM variable in the pharmaverseadam::adae dataset.


N_total <-n_distinct(adsl$USUBJID)

frequent<- adae_plot %>%
  distinct(USUBJID,AETERM) %>%
  count(AETERM,name="n_subj") %>%
  slice_max(n_subj, n=10,with_ties=FALSE) %>%
  rowwise() %>%
  mutate(
    pct= n_subj / N_total * 100,
    lower = binom.test(n_subj, N_total)$conf.int[1] *100,
    upper=binom.test(n_subj,N_total)$conf.int[2]*100
  ) %>%
  ungroup() %>%
  mutate(AETERM=reorder(AETERM,pct))
#Output Check
#frequent


plot2 <- ggplot(frequent, aes(x = pct, y = AETERM)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.25,
                orientation = "y", color = "#4292C6", linewidth = 0.6) +
  geom_point(size = 3, color = "#08306B") +
  labs(
    title    = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", N_total, " subjects; 95% Clopper-Pearson CIs"),
    x        = "Percentage of Patients (%)",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

#Output Check
#plot2  


dir.create("question_3_tlg", showWarnings = FALSE)
ggsave("question_3_tlg/plot1_ae_severity_by_treatment.png", plot1,
       width = 8, height = 5, dpi = 300, bg = "white")
ggsave("question_3_tlg/plot2_top10_ae_incidence_ci.png", plot2,
       width = 8, height = 5, dpi = 300, bg = "white")

sessionInfo()






