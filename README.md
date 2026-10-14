# SR. ADS Programmer: Coding Assessment

Solutions and observations to the Coding assessment: creating an SDTM domain with `{sdtm.oak}`, an ADaM dataset with `{admiral}`, and safety TLGs with `{gtsummary}` and `{ggplot2}`.

Each question has its own folder containing the program, its output(s), and a log file showing the program ran without errors.

## Repository structure

```
.
├── README.md
├── question_1_sdtm/
│   ├── 01_create_ds_domain.R            # SDTM DS domain program ({sdtm.oak})
│   ├── 01_create_ds_domain.log          # Run log (error-free)
│   ├── sdtm_ct.csv                      # Study controlled terminology (input)
│   ├── ds.rds                           # DS dataset (R format)
│   └── ds.csv                           # DS dataset (CSV)
├── question_2_adam/
│   ├── create_adsl.R                    # ADaM ADSL program ({admiral})
│   ├── create_adsl.log                  # Run log (error-free)
│   ├── adsl.rds                         # ADSL dataset (R format)
│   └── adsl.csv                         # ADSL dataset (CSV)
└── question_3_tlg/
    ├── 01_create_ae_summary_table.R     # TEAE summary table ({gtsummary})
    ├── 01_create_ae_summary_table.log
    ├── ae_summary_table.html            # Table output (HTML)
    ├── ae_summary_table.pdf             # Table output (PDF copy)
    ├── 02_create_visualizations.R       # AE plots ({ggplot2})
    ├── 02_create_visualizations.log
    ├── plot1_ae_severity_by_treatment.png
    └── plot2_top10_ae_incidence_ci.png
```

## How to run

1. Clone the repository and open it as an RStudio Project, or set the working directory to the repository root.
2. Install the packages:
   ```r
   install.packages(c("sdtm.oak", "pharmaverseraw", "pharmaversesdtm", "admiral",
                      "pharmaverseadam", "gtsummary", "gt", "ggplot2", "dplyr",
                      "lubridate", "stringr", "webshot2"))
   ```
3. Run any program from the repository root. For example, to reproduce a log:
   ```r
   system("Rscript question_1_sdtm/01_create_ds_domain.R > question_1_sdtm/01_create_ds_domain.log 2>&1")
   ```

All paths are relative to the repository root. Developed with R 4.6.1.

---

## Question 1: SDTM DS domain (`{sdtm.oak}`)

**Input:** `pharmaverseraw::ds_raw`, `question_1_sdtm/sdtm_ct.csv` ([source](https://github.com/pharmaverse/examples/blob/main/metadata/sdtm_ct.csv)), `pharmaversesdtm::dm`  
**Spec:** [mock eCRF with DS programming notes](https://github.com/pharmaverse/pharmaverseraw/blob/main/vignettes/articles/aCRFs/Subject_Disposition_aCRF.pdf)  
**Output:** `ds.csv` / `ds.rds`, 850 records for 306 subjects

**Notes:**
- A few raw values didn't match the CT file only because of case or wording, and "Unscheduled" visits aren't in the CT. Both are handled and explained in the program comments.
- The 52 records without a study day (DSSTDY) are screen failures, who have no reference start date in DM.

## Question 2: ADaM ADSL (`{admiral}`)

**Input:** `pharmaversesdtm::dm, vs, ex, ds, ae`  
**Output:** `adsl.csv` / `adsl.rds`, 306 subjects, with AGEGR9/AGEGR9N, TRTSDTM/TRTSTMF, ITTFL and LSTAVLDT added

**Notes:**
- ITTFL follows the spec as written ("Y" if ARM is not missing). Screen failures have ARM = "Screen Failure" rather than missing, so all subjects are "Y". If the intent was randomized subjects only, I'd confirm with the spec author.

## Question 3: TLGs (`{gtsummary}`, `{ggplot2}`)

**Input:** `pharmaverseadam::adae, adsl`  
**Output:** `ae_summary_table.html` / `.pdf`, `plot1_ae_severity_by_treatment.png`, `plot2_top10_ae_incidence_ci.png`

**Notes:**
- The table uses treatment-emergent AEs only (TRTEMFL = "Y"), as its instructions ask.
- The plots use all AEs in the safety population, since the plot instructions don't restrict to TEAEs and this matches the example output.
