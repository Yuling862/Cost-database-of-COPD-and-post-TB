# ---- COPD and post-TB cost review ----
# Meta-analysis and forest plots
# YL
----------------------
  
# Load packages ----
# if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, here, janitor,flextable,officer,meta)

cost<-read_excel("Data/cost_usd_2021.xlsx")


# Meta analysis ----
# Create a new variable to host both ref_id and subgroup_short
for(i in 1:length(cost$ref_id)){
  cost$ref_subgroup[i]=ifelse(!is.na(cost$subgroup_short[i]),
                              paste0(cost$ref_id[i]," - ",cost$subgroup_short[i]),
                              cost$ref_id[i])
}

## Distribution of some variables ---- 
# persepctive
cost %>% tabyl(perspective)%>%
  flextable() %>%
  set_table_properties(layout = "autofit", width = .8)

# with or without intervention
cost %>% tabyl(intervention_yn)%>%
  flextable() %>%
  set_table_properties(layout = "autofit", width = .8)

# calculation unit
cost %>% tabyl(calculation_unit)%>%
  flextable() %>%
  set_table_properties(layout = "autofit", width = .8)

# 1. per patient (observation or follow-up time or number of events varies)
# "Per acute exacerbation","Per exacerbation","Per month per patient",
# "Per patient","Per patient over 1 year","Per patient over 2 years",
# "Per patient over 24 weeks","Per patient over 3 years","Per patient over 6 months",
# "Per patient per hospitalization","Per patient per month","Per year per patient"

# 2. per hospitalization
# "Per hospitalization","Per hospitalization and per outpatient visit",
# "Per patient per hospitalization"

# 3. per outpatient visit
# "Per hospitalization and per outpatient visit","Per outpatient visit",
# "Per per visit for seeking non-domiciliary treatment"

# 4. lifetime costs
# "Lifetime costs per patient"

# disease
cost %>% tabyl(disease)%>%
  flextable() %>%
  set_table_properties(layout = "autofit", width = .8)

## COPD per patient/event----
### Hospitalization ----
#### Calculation unit ----
t1d <- cost %>% filter(disease=="COPD") %>%
  filter(intervention_yn=="No") %>% 
  filter(!(ref_subgroup=="Satici C, 2018 - The whole study population" | 
             ref_subgroup=="Ture DA, 2021 - With inadequate health literacy" | 
             ref_subgroup=="Ture DA, 2021 - With adequate health literacy" | 
             ref_subgroup=="Hong Y, 2020 - In the ward in the propensity-matched cohort" | 
             ref_subgroup=="Hong Y, 2020 - In the ICU in the propensity-matched cohort"|
             ref_subgroup=="Hong Y, 2020 - In the ICU before matching"|
             calculation_unit=="Per bed-day over 2016-2018"|
             calculation_unit=="Per 100000 patiens in 40 years"|
             calculation_unit=="Lifetime costs per patient"|
             calculation_unit=="Per day"))%>%
  filter(!(ref_subgroup=="Kallaru H, 2015 - Overall: GOLD II, III, IV"))

t1d<-t1d%>%mutate(cal_unit_new=case_when(calculation_unit %in% c("Per acute exacerbation","Per exacerbation",
                                                                   "Per hospitalization","Per hospitalization and per outpatient visit",
                                                                   "Per patient per hospitalization")~"Per event",
                                           calculation_unit %in% c("Per month per patient","Per patient","Per patient over 1 year",
                                                                   "Per patient over 2 years","Per patient over 6 months",
                                                                   "Per patient per month","Per year per patient")~"Per patient",
                                           TRUE~calculation_unit))%>%
  rename(`Calculation unit`=cal_unit_new)

# get SD from mean and CI
t1d$hospitalization_costs_sd[is.na(t1d$hospitalization_costs_sd)] = 1/1.96*(t1d$hospitalization_costs_mean[is.na(t1d$hospitalization_costs_sd)] -
                                                                              t1d$hospitalization_costs_ci_low[is.na(t1d$hospitalization_costs_sd)])

tmp = t1d[, c("ref_subgroup", "income_group","continent","country", "hospitalization_costs_n",
              "hospitalization_costs_mean", "hospitalization_costs_sd",
              "sample_size_patients","hospitalization_costs_median", 
              "hospitalization_costs_iqr_low", "hospitalization_costs_iqr_hi", 
              "hospitalization_costs_range_low", "hospitalization_costs_range_hi",
              "hospitalization_costs_ci_low", "hospitalization_costs_ci_hi","Calculation unit")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))

t1<-metamean(tmp1,
             n=ifelse(is.na(hospitalization_costs_n),sample_size_patients,hospitalization_costs_n), # we get infinitive values for SD
             # n=sample_size_patients,
             mean=hospitalization_costs_mean,
             sd=hospitalization_costs_sd,
             studlab=ref_subgroup,
             median=hospitalization_costs_median,
             q1=hospitalization_costs_iqr_low,
             q3=hospitalization_costs_iqr_hi,
             min=hospitalization_costs_range_low,
             max=hospitalization_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE,
             sm = "MLN",
             subgroup = `Calculation unit`
)

# export the plot with high resolution
png('Output/Forest_plot_log tranformed_1 decimal_08032024/COPD_hosp_unit.png', 
    pointsize=10, width=4000, height=2750,res=400)
forest(t1, 
       # subgroup=T,
       # layout = "JAMA",
       sortvar = TE,
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE, # remove inestimable effects
       fontsize=9
       ,smlab = "",
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()

#### Income level----
t1d <- cost %>% filter(disease=="COPD") %>%
  filter(intervention_yn=="No") %>% 
  filter(!(ref_subgroup=="Satici C, 2018 - The whole study population" | 
             ref_subgroup=="Ture DA, 2021 - With inadequate health literacy" | 
             ref_subgroup=="Ture DA, 2021 - With adequate health literacy" | 
             ref_subgroup=="Hong Y, 2020 - In the ward in the propensity-matched cohort" | 
             ref_subgroup=="Hong Y, 2020 - In the ICU in the propensity-matched cohort"|
             ref_subgroup=="Hong Y, 2020 - In the ICU before matching"|
             calculation_unit=="Per bed-day over 2016-2018"|
             calculation_unit=="Per 100000 patiens in 40 years"|
             calculation_unit=="Lifetime costs per patient"|
             calculation_unit=="Per day"))%>%
  filter(!(ref_subgroup=="Kallaru H, 2015 - Overall: GOLD II, III, IV"))%>%
  rename(`Income group`=income_group)

# get SD from mean and CI
t1d$hospitalization_costs_sd[is.na(t1d$hospitalization_costs_sd)] = 1/1.96*(t1d$hospitalization_costs_mean[is.na(t1d$hospitalization_costs_sd)] -
                                                                              t1d$hospitalization_costs_ci_low[is.na(t1d$hospitalization_costs_sd)])

tmp = t1d[, c("ref_subgroup", "Income group","continent","country", "hospitalization_costs_n",
              "hospitalization_costs_mean", "hospitalization_costs_sd",
              "sample_size_patients","hospitalization_costs_median", 
              "hospitalization_costs_iqr_low", "hospitalization_costs_iqr_hi", 
              "hospitalization_costs_range_low", "hospitalization_costs_range_hi",
              "hospitalization_costs_ci_low", "hospitalization_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))

t1<-metamean(tmp1,
             n=ifelse(is.na(hospitalization_costs_n),sample_size_patients,hospitalization_costs_n), # we get infinitive values for SD
             # n=sample_size_patients,
             mean=hospitalization_costs_mean,
             sd=hospitalization_costs_sd,
             studlab=ref_subgroup,
             median=hospitalization_costs_median,
             q1=hospitalization_costs_iqr_low,
             q3=hospitalization_costs_iqr_hi,
             min=hospitalization_costs_range_low,
             max=hospitalization_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE,
             sm = "MLN",
             subgroup = `Income group`
)

png('Output/Forest_plot_log tranformed_1 decimal_08032024/COPD_hosp_income.png', 
    pointsize=10, width=4000, height=2750,res=400)
forest(t1, 
       # subgroup=T,
       # layout = "JAMA",
       sortvar = TE,
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE, # remove inestimable effects
       fontsize=9
       ,smlab = "",
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()

#### Continent----
t1d <- cost %>% filter(disease=="COPD") %>%
  filter(intervention_yn=="No") %>% 
  filter(!(ref_subgroup=="Satici C, 2018 - The whole study population" | 
             ref_subgroup=="Ture DA, 2021 - With inadequate health literacy" | 
             ref_subgroup=="Ture DA, 2021 - With adequate health literacy" | 
             ref_subgroup=="Hong Y, 2020 - In the ward in the propensity-matched cohort" | 
             ref_subgroup=="Hong Y, 2020 - In the ICU in the propensity-matched cohort"|
             ref_subgroup=="Hong Y, 2020 - In the ICU before matching"|
             calculation_unit=="Per bed-day over 2016-2018"|
             calculation_unit=="Per 100000 patiens in 40 years"|
             calculation_unit=="Lifetime costs per patient"|
             calculation_unit=="Per day"))%>%
  filter(!(ref_subgroup=="Kallaru H, 2015 - Overall: GOLD II, III, IV"))%>%
  rename(Continent = continent)

# get SD from mean and CI
t1d$hospitalization_costs_sd[is.na(t1d$hospitalization_costs_sd)] = 1/1.96*(t1d$hospitalization_costs_mean[is.na(t1d$hospitalization_costs_sd)] -
                                                                              t1d$hospitalization_costs_ci_low[is.na(t1d$hospitalization_costs_sd)])

tmp = t1d[, c("ref_subgroup", "income_group","Continent","country", "hospitalization_costs_n",
              "hospitalization_costs_mean", "hospitalization_costs_sd",
              "sample_size_patients","hospitalization_costs_median", 
              "hospitalization_costs_iqr_low", "hospitalization_costs_iqr_hi", 
              "hospitalization_costs_range_low", "hospitalization_costs_range_hi",
              "hospitalization_costs_ci_low", "hospitalization_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))

t1<-metamean(tmp1,
             n=ifelse(is.na(hospitalization_costs_n),sample_size_patients,hospitalization_costs_n), # we get infinitive values for SD
             # n=sample_size_patients,
             mean=hospitalization_costs_mean,
             sd=hospitalization_costs_sd,
             studlab=ref_subgroup,
             median=hospitalization_costs_median,
             q1=hospitalization_costs_iqr_low,
             q3=hospitalization_costs_iqr_hi,
             min=hospitalization_costs_range_low,
             max=hospitalization_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE,
             sm = "MLN",
             subgroup = Continent
)

# leave out those with mean but without CI
# t1$TE<-ifelse(is.na(t1$lower),NA,t1$TE) #pooled effect was the same
# without CI, mean won't be included in meta analysis!

png('Output/Forest_plot_log tranformed_1 decimal_08032024/COPD_hosp_continent.png', 
    pointsize=10, width=4000, height=2750,res=400)
forest(t1, 
       # subgroup=T,
       # layout = "JAMA",
       sortvar = TE,
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE, # remove inestimable effects
       fontsize=9
       ,smlab = "",
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()

#### Country----
t1d <- cost %>% filter(disease=="COPD") %>%
  filter(intervention_yn=="No") %>% 
  filter(!(ref_subgroup=="Satici C, 2018 - The whole study population" | 
             ref_subgroup=="Ture DA, 2021 - With inadequate health literacy" | 
             ref_subgroup=="Ture DA, 2021 - With adequate health literacy" | 
             ref_subgroup=="Hong Y, 2020 - In the ward in the propensity-matched cohort" | 
             ref_subgroup=="Hong Y, 2020 - In the ICU in the propensity-matched cohort"|
             ref_subgroup=="Hong Y, 2020 - In the ICU before matching"|
             calculation_unit=="Per bed-day over 2016-2018"|
             calculation_unit=="Per 100000 patiens in 40 years"|
             calculation_unit=="Lifetime costs per patient"|
             calculation_unit=="Per day"))%>%
  filter(!(ref_subgroup=="Kallaru H, 2015 - Overall: GOLD II, III, IV"))%>%
  rename(Country = country)
# t1d$Country[t1d$ref_id=="Foo J, 2016"]

# table(t1d$Country)

# get SD from mean and CI
t1d$hospitalization_costs_sd[is.na(t1d$hospitalization_costs_sd)] = 1/1.96*(t1d$hospitalization_costs_mean[is.na(t1d$hospitalization_costs_sd)] -
                                                                              t1d$hospitalization_costs_ci_low[is.na(t1d$hospitalization_costs_sd)])

tmp = t1d[, c("ref_subgroup", "income_group","continent","Country", "hospitalization_costs_n",
              "hospitalization_costs_mean", "hospitalization_costs_sd",
              "sample_size_patients","hospitalization_costs_median", 
              "hospitalization_costs_iqr_low", "hospitalization_costs_iqr_hi", 
              "hospitalization_costs_range_low", "hospitalization_costs_range_hi",
              "hospitalization_costs_ci_low", "hospitalization_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))

t1<-metamean(tmp1,
             n=ifelse(is.na(hospitalization_costs_n),sample_size_patients,hospitalization_costs_n), # we get infinitive values for SD
             # n=sample_size_patients,
             mean=hospitalization_costs_mean,
             sd=hospitalization_costs_sd,
             studlab=ref_subgroup,
             median=hospitalization_costs_median,
             q1=hospitalization_costs_iqr_low,
             q3=hospitalization_costs_iqr_hi,
             min=hospitalization_costs_range_low,
             max=hospitalization_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE,
             sm = "MLN",
             subgroup = Country
)

# leave out those with mean but without CI
# t1$TE<-ifelse(is.na(t1$lower),NA,t1$TE) #pooled effect was the same
# without CI, mean won't be included in meta analysis!

png('Output/Forest_plot_log tranformed_1 decimal_08032024/COPD_hosp_country.png', 
    pointsize=10, width=4000, height=3900,res=400)
forest(t1, 
       # subgroup=T,
       # layout = "JAMA",
       sortvar = TE,
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE, # remove inestimable effects
       fontsize=9
       ,smlab = "",
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()


### Medication costs ----
#### Income level ----
t1d <- cost %>% filter(disease=="COPD") %>%
  filter(intervention_yn=="No") %>%
  filter(!(ref_subgroup=="Akramova EG, 2014 - With comorbidities"|
             ref_subgroup=="Vu TQ, 2019 - Outpatient (GOLD III)"|
             ref_subgroup=="Vu TQ, 2019 - Outpatient (GOLD IV)"
  ))


# get SD from mean and CI
t1d$medication_costs_sd[is.na(t1d$medication_costs_sd)] = 1/1.96*(t1d$medication_costs_mean[is.na(t1d$medication_costs_sd)] -
                                                                            t1d$medication_costs_ci_low[is.na(t1d$medication_costs_sd)])

tmp = t1d[, c("ref_subgroup", "income_group","continent","country", "medication_costs", #`medication_costs_n` doesn't exist.
              "medication_costs_mean", "medication_costs_sd",
              "sample_size_patients","medication_costs_median", 
              "medication_costs_iqr_low", "medication_costs_iqr_hi", 
              "medication_costs_range_low", "medication_costs_range_hi",
              "medication_costs_ci_low", "medication_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))%>%
  rename(`Income group`=income_group)

t1<-metamean(tmp1,
             n=sample_size_patients,
             mean=medication_costs_mean,
             sd=medication_costs_sd,
             studlab=ref_subgroup,
             median=medication_costs_median,
             q1=medication_costs_iqr_low,
             q3=medication_costs_iqr_hi,
             min=medication_costs_range_low,
             max=medication_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE
             sm = "MLN",
             subgroup = `Income group`
)

png('Output/Forest_plot_log tranformed_1 decimal_08032024/COPD_medication_income.png', 
    pointsize=10, width=4400, height=2550,res=400)
forest(t1, 
       # layout = "JAMA",
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE # remove inestimable effects
       ,fontsize=9,
       smlab = "",
       # ,xlim=c(0,3000)
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()


#### Country ----
# only one group has 3 studies, others are with 1 or 2 studies
t1d <- cost %>% filter(disease=="COPD") %>%
  filter(intervention_yn=="No") %>%
  filter(!(ref_subgroup=="Akramova EG, 2014 - With comorbidities"|
             ref_subgroup=="Vu TQ, 2019 - Outpatient (GOLD III)"|
             ref_subgroup=="Vu TQ, 2019 - Outpatient (GOLD IV)"
  ))


# get SD from mean and CI
t1d$medication_costs_sd[is.na(t1d$medication_costs_sd)] = 1/1.96*(t1d$medication_costs_mean[is.na(t1d$medication_costs_sd)] -
                                                                    t1d$medication_costs_ci_low[is.na(t1d$medication_costs_sd)])

tmp = t1d[, c("ref_subgroup", "income_group","continent","country", "medication_costs", #`medication_costs_n` doesn't exist.
              "medication_costs_mean", "medication_costs_sd",
              "sample_size_patients","medication_costs_median", 
              "medication_costs_iqr_low", "medication_costs_iqr_hi", 
              "medication_costs_range_low", "medication_costs_range_hi",
              "medication_costs_ci_low", "medication_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))%>%
  rename(`Country`=country)

t1<-metamean(tmp1,
             n=sample_size_patients,
             mean=medication_costs_mean,
             sd=medication_costs_sd,
             studlab=ref_subgroup,
             median=medication_costs_median,
             q1=medication_costs_iqr_low,
             q3=medication_costs_iqr_hi,
             min=medication_costs_range_low,
             max=medication_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE,
             sm = "MLN",
             subgroup = Country
)

png('Output/Forest_plot_log tranformed_1 decimal_08032024/COPD_medication_country.png', 
    pointsize=10, width=4400, height=3900,res=400)
forest(t1, 
       # layout = "JAMA",
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE # remove inestimable effects
       ,fontsize=9,
       smlab = "",
       # ,xlim=c(0,3000)
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()


### Outpatient costs ----
#### Income level ----
t1d <- cost %>% filter(disease=="COPD") %>%
  filter(intervention_yn=="No") 

# get SD from mean and CI
t1d$medication_costs_sd[is.na(t1d$outpatient_costs_sd)] = 1/1.96*(t1d$outpatient_costs_mean[is.na(t1d$outpatient_costs_sd)] -
                                                                    t1d$outpatient_costs_ci_low[is.na(t1d$outpatient_costs_sd)])
# No outpatient_costs_ci_low
tmp = t1d[, c("ref_subgroup", "income_group","continent","country", "outpatient_costs_n", #`medication_costs_n` doesn't exist.
              "outpatient_costs_mean", "outpatient_costs_sd",
              "sample_size_patients","outpatient_costs_median", 
              "outpatient_costs_iqr_low", "outpatient_costs_iqr_hi", 
              "outpatient_costs_range_low", "outpatient_costs_range_hi",
              "outpatient_costs_ci_low", "outpatient_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))%>%
  rename(`Income group` = income_group)

t1<-metamean(tmp1,
             n=ifelse(is.na(outpatient_costs_n),sample_size_patients,outpatient_costs_n), # we get infinitive values for SD
             # n=sample_size_patients,
             mean=outpatient_costs_mean,
             sd=outpatient_costs_sd,
             studlab=ref_subgroup,
             median=outpatient_costs_median,
             q1=outpatient_costs_iqr_low,
             q3=outpatient_costs_iqr_hi,
             min=outpatient_costs_range_low,
             max=outpatient_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE
             sm = "MLN",
             subgroup = `Income group`
)

png('Output/Forest_plot_log tranformed_1 decimal_08032024/COPD_outpatient_income.png', 
    pointsize=10, width=3400, height=1700,res=400)
forest(t1, 
       # layout = "JAMA",
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE # remove inestimable effects
       ,fontsize=9,
       smlab = "",
       # ,xlim=c(0,3000)
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()

#### Country ----
# Each country has one study
t1d <- cost %>% filter(disease=="COPD") %>%
  filter(intervention_yn=="No") 

# # get SD from mean and CI
# t1d$medication_costs_sd[is.na(t1d$outpatient_costs_sd)] = 1/1.96*(t1d$outpatient_costs_mean[is.na(t1d$outpatient_costs_sd)] -
#                                                                     t1d$outpatient_costs_ci_low[is.na(t1d$outpatient_costs_sd)])
# No outpatient_costs_ci_low
tmp = t1d[, c("ref_subgroup", "income_group","continent","country", "outpatient_costs_n", #`medication_costs_n` doesn't exist.
              "outpatient_costs_mean", "outpatient_costs_sd",
              "sample_size_patients","outpatient_costs_median", 
              "outpatient_costs_iqr_low", "outpatient_costs_iqr_hi", 
              "outpatient_costs_range_low", "outpatient_costs_range_hi",
              "outpatient_costs_ci_low", "outpatient_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))%>%
  rename(Country=country)

t1<-metamean(tmp1,
             n=ifelse(is.na(outpatient_costs_n),sample_size_patients,outpatient_costs_n), # we get infinitive values for SD
             # n=sample_size_patients,
             mean=outpatient_costs_mean,
             sd=outpatient_costs_sd,
             studlab=ref_subgroup,
             median=outpatient_costs_median,
             q1=outpatient_costs_iqr_low,
             q3=outpatient_costs_iqr_hi,
             min=outpatient_costs_range_low,
             max=outpatient_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE
             sm = "MLN",
             subgroup = Country
)

png('Output/Forest_plot_log tranformed_1 decimal_08032024/COPD_outpatient_country.png', 
    pointsize=10, width=3400, height=1700,res=400)
forest(t1, 
       # layout = "JAMA",
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE # remove inestimable effects
       ,fontsize=9,
       smlab = "",
       # ,xlim=c(0,3000)
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()




## AECOPD ----
### Hospitalization ----
#### Calculation unit ----
t1d <- cost %>% filter(disease=="AECOPD") %>%
  filter(intervention_yn=="No") %>%
  filter(!(ref_subgroup=="Pothirat C, 2015 - Managed by internist"|
             ref_subgroup=="Pothirat C, 2015 - Managed by pulmonologist"|
             ref_id=="Reechaipichitkul W, 2014"))%>%
  filter(!(ref_subgroup=="Li F, 2018 - Total study population"|
             ref_subgroup=="Li F, 2018 - Length of stay in hospital <8 days"|
             ref_subgroup=="Li F, 2018 - Length of stay in hospital ≥8 days"|
             ref_subgroup=="Mao X, 2021 - The whole study population"|
             ref_subgroup=="Wang S, 2021 - The whole study population"|
             ref_subgroup=="Zeng Q, 2021 - Eosinophilic group, aged >65"|
             ref_subgroup=="Zeng Q, 2021 - Non-eosinophilic group, aged >65"|
             ref_subgroup=="Zeng Q, 2021 - Eosinophilic group, aged 45-65"|
             ref_subgroup=="Zeng Q, 2021 - Non-eosinophilic group, aged 45-65"|
             ref_subgroup=="Cui Y, 2021b - Eosinophilic (before propensity score matching)" |
             ref_subgroup=="Cui Y, 2021b - Non-eosinophilic (before propensity score matching)" 
  ))%>%
  filter(!(ref_id=="Liang L, 2020a" & calculation_unit=="Per patient"))
  
# table(t1d$perspective)

t1d<-t1d%>%mutate(cal_unit_new=case_when(calculation_unit %in% c("Per exacerbation","Per hospitalization",
                                                             "Per patient per hospitalization") ~ "Per event",
                                         calculation_unit %in% c("Per patient over 6 months","Per year per patient",
                                                             "Per patient") ~ "Per patient",
                                         TRUE ~ calculation_unit))%>%
  rename(`Calculation unit`=cal_unit_new)

# get SD from mean and CI
t1d$hospitalization_costs_sd[is.na(t1d$hospitalization_costs_sd)] = 1/1.96*(t1d$hospitalization_costs_mean[is.na(t1d$hospitalization_costs_sd)] -
                                                                              t1d$hospitalization_costs_ci_low[is.na(t1d$hospitalization_costs_sd)])

tmp = t1d[, c("ref_subgroup", "income_group","continent","country", "hospitalization_costs_n",
              "hospitalization_costs_mean", "hospitalization_costs_sd",
              "sample_size_patients","hospitalization_costs_median", 
              "hospitalization_costs_iqr_low", "hospitalization_costs_iqr_hi", 
              "hospitalization_costs_range_low", "hospitalization_costs_range_hi",
              "hospitalization_costs_ci_low", "hospitalization_costs_ci_hi","Calculation unit")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))| # select studies with contribution to meta-analysis
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))

t1<-metamean(tmp1,
             n=ifelse(is.na(hospitalization_costs_n),sample_size_patients,hospitalization_costs_n), # we get infinitive values for SD
             # n=sample_size_patients,
             mean=hospitalization_costs_mean,
             sd=hospitalization_costs_sd,
             studlab=ref_subgroup,
             median=hospitalization_costs_median,
             q1=hospitalization_costs_iqr_low,
             q3=hospitalization_costs_iqr_hi,
             min=hospitalization_costs_range_low,
             max=hospitalization_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE
             sm = "MLN",
             subgroup = `Calculation unit`
)

png('Output/Forest_plot_log tranformed_1 decimal_08032024/AECOPD_hosp_unit.png', 
    pointsize=10, width=5050, height=5700,res=400)
forest(t1, 
       # subgroup=T,
       # layout = "JAMA",
       sortvar = TE,
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE, # remove inestimable effects
       fontsize=9
       ,smlab = "",
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()

#### Income level----
# same as per event
t1d <- cost %>% filter(disease=="AECOPD") %>%
  filter(intervention_yn=="No") %>%
  filter(!(ref_subgroup=="Pothirat C, 2015 - Managed by internist"|
             ref_subgroup=="Pothirat C, 2015 - Managed by pulmonologist"|
             ref_id=="Reechaipichitkul W, 2014"))%>%
  filter(!(ref_subgroup=="Li F, 2018 - Total study population"|
             ref_subgroup=="Li F, 2018 - Length of stay in hospital <8 days"|
             ref_subgroup=="Li F, 2018 - Length of stay in hospital ≥8 days"|
             ref_subgroup=="Mao X, 2021 - The whole study population"|
             ref_subgroup=="Wang S, 2021 - The whole study population"|
             ref_subgroup=="Zeng Q, 2021 - Eosinophilic group, aged >65"|
             ref_subgroup=="Zeng Q, 2021 - Non-eosinophilic group, aged >65"|
             ref_subgroup=="Zeng Q, 2021 - Eosinophilic group, aged 45-65"|
             ref_subgroup=="Zeng Q, 2021 - Non-eosinophilic group, aged 45-65"|
             ref_subgroup=="Cui Y, 2021b - Eosinophilic (before propensity score matching)" |
             ref_subgroup=="Cui Y, 2021b - Non-eosinophilic (before propensity score matching)" 
  )) %>%
  filter(!(ref_id=="Liang L, 2020a" & calculation_unit=="Per patient")) %>%
  rename(`Income group`=income_group)

# table(t1d$calculation_unit)

# get SD from mean and CI
t1d$hospitalization_costs_sd[is.na(t1d$hospitalization_costs_sd)] = 1/1.96*(t1d$hospitalization_costs_mean[is.na(t1d$hospitalization_costs_sd)] -
                                                                              t1d$hospitalization_costs_ci_low[is.na(t1d$hospitalization_costs_sd)])

tmp = t1d[, c("ref_subgroup", "Income group","continent","country", "hospitalization_costs_n",
              "hospitalization_costs_mean", "hospitalization_costs_sd",
              "sample_size_patients","hospitalization_costs_median", 
              "hospitalization_costs_iqr_low", "hospitalization_costs_iqr_hi", 
              "hospitalization_costs_range_low", "hospitalization_costs_range_hi",
              "hospitalization_costs_ci_low", "hospitalization_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))

t1<-metamean(tmp1,
             # n=ifelse(is.na(hospitalization_costs_n),sample_size_patients,hospitalization_costs_n), # we get infinitive values for SD
             n=sample_size_patients,
             mean=hospitalization_costs_mean,
             sd=hospitalization_costs_sd,
             studlab=ref_subgroup,
             median=hospitalization_costs_median,
             q1=hospitalization_costs_iqr_low,
             q3=hospitalization_costs_iqr_hi,
             min=hospitalization_costs_range_low,
             max=hospitalization_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE,
             sm = "MLN",
             subgroup = `Income group`
)

# leave out those with mean but without CI
# t1$TE<-ifelse(is.na(t1$lower),NA,t1$TE) #pooled effect was the same
# without CI, mean won't be included in meta analysis!

png('Output/Forest_plot_log tranformed_1 decimal_08032024/AECOPD_hosp_income.png', 
    pointsize=10, width=5050, height=5700,res=400)
forest(t1, 
       # subgroup=T,
       # layout = "JAMA",
       sortvar = TE,
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE, # remove inestimable effects
       fontsize=9
       ,smlab = "",
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()

#### Continent----
# same as per event
t1d <- cost %>% filter(disease=="AECOPD") %>%
  filter(intervention_yn=="No") %>%
  filter(!(ref_subgroup=="Pothirat C, 2015 - Managed by internist"|
             ref_subgroup=="Pothirat C, 2015 - Managed by pulmonologist"|
             ref_id=="Reechaipichitkul W, 2014"))%>%
  filter(!(ref_subgroup=="Li F, 2018 - Total study population"|
             ref_subgroup=="Li F, 2018 - Length of stay in hospital <8 days"|
             ref_subgroup=="Li F, 2018 - Length of stay in hospital ≥8 days"|
             ref_subgroup=="Mao X, 2021 - The whole study population"|
             ref_subgroup=="Wang S, 2021 - The whole study population"|
             ref_subgroup=="Zeng Q, 2021 - Eosinophilic group, aged >65"|
             ref_subgroup=="Zeng Q, 2021 - Non-eosinophilic group, aged >65"|
             ref_subgroup=="Zeng Q, 2021 - Eosinophilic group, aged 45-65"|
             ref_subgroup=="Zeng Q, 2021 - Non-eosinophilic group, aged 45-65"|
             ref_subgroup=="Cui Y, 2021b - Eosinophilic (before propensity score matching)" |
             ref_subgroup=="Cui Y, 2021b - Non-eosinophilic (before propensity score matching)" 
  )) %>%
  filter(!(ref_id=="Liang L, 2020a" & calculation_unit=="Per patient")) %>%
  rename(Continent=continent)

# table(t1d$calculation_unit)

# get SD from mean and CI
t1d$hospitalization_costs_sd[is.na(t1d$hospitalization_costs_sd)] = 1/1.96*(t1d$hospitalization_costs_mean[is.na(t1d$hospitalization_costs_sd)] -
                                                                              t1d$hospitalization_costs_ci_low[is.na(t1d$hospitalization_costs_sd)])

tmp = t1d[, c("ref_subgroup", "income_group","Continent","country", "hospitalization_costs_n",
              "hospitalization_costs_mean", "hospitalization_costs_sd",
              "sample_size_patients","hospitalization_costs_median", 
              "hospitalization_costs_iqr_low", "hospitalization_costs_iqr_hi", 
              "hospitalization_costs_range_low", "hospitalization_costs_range_hi",
              "hospitalization_costs_ci_low", "hospitalization_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))

t1<-metamean(tmp1,
             # n=ifelse(is.na(hospitalization_costs_n),sample_size_patients,hospitalization_costs_n), # we get infinitive values for SD
             n=sample_size_patients,
             mean=hospitalization_costs_mean,
             sd=hospitalization_costs_sd,
             studlab=ref_subgroup,
             median=hospitalization_costs_median,
             q1=hospitalization_costs_iqr_low,
             q3=hospitalization_costs_iqr_hi,
             min=hospitalization_costs_range_low,
             max=hospitalization_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE,
             sm = "MLN",
             subgroup = Continent
)

# leave out those with mean but without CI
# t1$TE<-ifelse(is.na(t1$lower),NA,t1$TE) #pooled effect was the same
# without CI, mean won't be included in meta analysis!

png('Output/Forest_plot_log tranformed_1 decimal_08032024/AECOPD_hosp_continent.png', 
    pointsize=10, width=5050, height=5700,res=400)
forest(t1, 
       # subgroup=T,
       # layout = "JAMA",
       sortvar = TE,
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE, # remove inestimable effects
       fontsize=9
       ,smlab = "",
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()

#### Country----
# same as per event
t1d <- cost %>% filter(disease=="AECOPD") %>%
  filter(intervention_yn=="No") %>%
  filter(!(ref_subgroup=="Pothirat C, 2015 - Managed by internist"|
             ref_subgroup=="Pothirat C, 2015 - Managed by pulmonologist"|
             ref_id=="Reechaipichitkul W, 2014"))%>%
  filter(!(ref_subgroup=="Li F, 2018 - Total study population"|
             ref_subgroup=="Li F, 2018 - Length of stay in hospital <8 days"|
             ref_subgroup=="Li F, 2018 - Length of stay in hospital ≥8 days"|
             ref_subgroup=="Mao X, 2021 - The whole study population"|
             ref_subgroup=="Wang S, 2021 - The whole study population"|
             ref_subgroup=="Zeng Q, 2021 - Eosinophilic group, aged >65"|
             ref_subgroup=="Zeng Q, 2021 - Non-eosinophilic group, aged >65"|
             ref_subgroup=="Zeng Q, 2021 - Eosinophilic group, aged 45-65"|
             ref_subgroup=="Zeng Q, 2021 - Non-eosinophilic group, aged 45-65"|
             ref_subgroup=="Cui Y, 2021b - Eosinophilic (before propensity score matching)" |
             ref_subgroup=="Cui Y, 2021b - Non-eosinophilic (before propensity score matching)" 
  )) %>%
  filter(!(ref_id=="Liang L, 2020a" & calculation_unit=="Per patient")) %>%
  rename(Country=country)

# table(t1d$calculation_unit)

# get SD from mean and CI
t1d$hospitalization_costs_sd[is.na(t1d$hospitalization_costs_sd)] = 1/1.96*(t1d$hospitalization_costs_mean[is.na(t1d$hospitalization_costs_sd)] -
                                                                              t1d$hospitalization_costs_ci_low[is.na(t1d$hospitalization_costs_sd)])
tmp = t1d[, c("ref_subgroup", "income_group","continent","Country", "hospitalization_costs_n",
              "hospitalization_costs_mean", "hospitalization_costs_sd",
              "sample_size_patients","hospitalization_costs_median", 
              "hospitalization_costs_iqr_low", "hospitalization_costs_iqr_hi", 
              "hospitalization_costs_range_low", "hospitalization_costs_range_hi",
              "hospitalization_costs_ci_low", "hospitalization_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))


t1<-metamean(tmp1,
             # n=ifelse(is.na(hospitalization_costs_n),sample_size_patients,hospitalization_costs_n), # we get infinitive values for SD
             n=sample_size_patients,
             mean=hospitalization_costs_mean,
             sd=hospitalization_costs_sd,
             studlab=ref_subgroup,
             median=hospitalization_costs_median,
             q1=hospitalization_costs_iqr_low,
             q3=hospitalization_costs_iqr_hi,
             min=hospitalization_costs_range_low,
             max=hospitalization_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE,
             sm = "MLN",
             subgroup = Country
)

# leave out those with mean but without CI
# t1$TE<-ifelse(is.na(t1$lower),NA,t1$TE) #pooled effect was the same
# without CI, mean won't be included in meta analysis!

png('Output/Forest_plot_log tranformed_1 decimal_08032024/AECOPD_hosp_country.png', 
    pointsize=10, width=5050, height=7000,res=400)
forest(t1, 
       # subgroup=T,
       # layout = "JAMA",
       sortvar = TE,
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE, # remove inestimable effects
       fontsize=9
       ,smlab = "",
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()




### Medication costs ----
#### Income level ----
t1d <- cost %>% filter(disease=="AECOPD") %>%
  filter(intervention_yn=="No") %>%
  filter(!(ref_subgroup=="Li F, 2018 - Total study population"|
             ref_subgroup=="Pothirat C, 2015 - Requiring mechanical ventilator managed by pulmonologist"|
             ref_subgroup=="Pothirat C, 2015 - Requiring mechanical ventilator managed by internist"
  ))


# get SD from mean and CI
t1d$medication_costs_sd[is.na(t1d$medication_costs_sd)] = 1/1.96*(t1d$medication_costs_mean[is.na(t1d$medication_costs_sd)] -
                                                                    t1d$medication_costs_ci_low[is.na(t1d$medication_costs_sd)])

tmp = t1d[, c("ref_subgroup", "income_group","continent","country", "medication_costs", #`medication_costs_n` doesn't exist.
              "medication_costs_mean", "medication_costs_sd",
              "sample_size_patients","medication_costs_median", 
              "medication_costs_iqr_low", "medication_costs_iqr_hi", 
              "medication_costs_range_low", "medication_costs_range_hi",
              "medication_costs_ci_low", "medication_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))%>%
  rename(`Income group`=income_group)

t1<-metamean(tmp1,
             n=sample_size_patients,
             mean=medication_costs_mean,
             sd=medication_costs_sd,
             studlab=ref_subgroup,
             median=medication_costs_median,
             q1=medication_costs_iqr_low,
             q3=medication_costs_iqr_hi,
             min=medication_costs_range_low,
             max=medication_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE
             sm = "MLN",
             subgroup = `Income group`
)

png('Output/Forest_plot_log tranformed_1 decimal_08032024/AECOPD_medication_income.png', 
    pointsize=10, width=4800, height=2600,res=400)
forest(t1, 
       # layout = "JAMA",
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE # remove inestimable effects
       ,fontsize=9,
       smlab = "",
       # ,xlim=c(0,3000)
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()


#### Country ----
# only one group has 3 studies, others are with 1 or 2 studies
t1d <- cost %>% filter(disease=="AECOPD") %>%
  filter(intervention_yn=="No") %>%
  filter(!(ref_subgroup=="Li F, 2018 - Total study population"|
             ref_subgroup=="Pothirat C, 2015 - Requiring mechanical ventilator managed by pulmonologist"|
             ref_subgroup=="Pothirat C, 2015 - Requiring mechanical ventilator managed by internist"
  ))


# get SD from mean and CI
t1d$medication_costs_sd[is.na(t1d$medication_costs_sd)] = 1/1.96*(t1d$medication_costs_mean[is.na(t1d$medication_costs_sd)] -
                                                                    t1d$medication_costs_ci_low[is.na(t1d$medication_costs_sd)])

tmp = t1d[, c("ref_subgroup", "income_group","continent","country", "medication_costs", #`medication_costs_n` doesn't exist.
              "medication_costs_mean", "medication_costs_sd",
              "sample_size_patients","medication_costs_median", 
              "medication_costs_iqr_low", "medication_costs_iqr_hi", 
              "medication_costs_range_low", "medication_costs_range_hi",
              "medication_costs_ci_low", "medication_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))%>%
  rename(`Country`=country)

t1<-metamean(tmp1,
             n=sample_size_patients,
             mean=medication_costs_mean,
             sd=medication_costs_sd,
             studlab=ref_subgroup,
             median=medication_costs_median,
             q1=medication_costs_iqr_low,
             q3=medication_costs_iqr_hi,
             min=medication_costs_range_low,
             max=medication_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE
             sm = "MLN",
             subgroup = Country
)

png('Output/Forest_plot_log tranformed_1 decimal_08032024/AECOPD_medication_country.png', 
    pointsize=10, width=4800, height=4250,res=400)
forest(t1, 
       # layout = "JAMA",
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE # remove inestimable effects
       ,fontsize=9,
       smlab = "",
       # ,xlim=c(0,3000)
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)
dev.off()




### Outpatient costs (no entry) ----
#### Income level ----
# Each country has one study
t1d <- cost %>% filter(disease=="AECOPD") %>%
  filter(intervention_yn=="No") 

# get SD from mean and CI
t1d$medication_costs_sd[is.na(t1d$outpatient_costs_sd)] = 1/1.96*(t1d$outpatient_costs_mean[is.na(t1d$outpatient_costs_sd)] -
                                                                    t1d$outpatient_costs_ci_low[is.na(t1d$outpatient_costs_sd)])

tmp = t1d[, c("ref_subgroup", "income_group","continent","country", "outpatient_costs_n", #`medication_costs_n` doesn't exist.
              "outpatient_costs_mean", "outpatient_costs_sd",
              "sample_size_patients","outpatient_costs_median", 
              "outpatient_costs_iqr_low", "outpatient_costs_iqr_hi", 
              "outpatient_costs_range_low", "outpatient_costs_range_hi",
              "outpatient_costs_ci_low", "outpatient_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))%>%
  rename(Country=country)

t1<-metamean(tmp1,
             n=ifelse(is.na(outpatient_costs_n),sample_size_patients,outpatient_costs_n), # we get infinitive values for SD
             # n=sample_size_patients,
             mean=outpatient_costs_mean,
             sd=outpatient_costs_sd,
             studlab=ref_subgroup,
             median=outpatient_costs_median,
             q1=outpatient_costs_iqr_low,
             q3=outpatient_costs_iqr_hi,
             min=outpatient_costs_range_low,
             max=outpatient_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE
             subgroup = Country
)


forest(t1, 
       # layout = "JAMA",
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE # remove inestimable effects
       ,fontsize=9,
       smlab = "",
       # ,xlim=c(0,3000)
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)




#### Country ----
t1d <- cost %>% filter(disease=="AECOPD") %>%
  filter(intervention_yn=="No") 

# get SD from mean and CI
t1d$medication_costs_sd[is.na(t1d$outpatient_costs_sd)] = 1/1.96*(t1d$outpatient_costs_mean[is.na(t1d$outpatient_costs_sd)] -
                                                                    t1d$outpatient_costs_ci_low[is.na(t1d$outpatient_costs_sd)])

tmp = t1d[, c("ref_subgroup", "income_group","continent","country", "outpatient_costs_n", #`medication_costs_n` doesn't exist.
              "outpatient_costs_mean", "outpatient_costs_sd",
              "sample_size_patients","outpatient_costs_median", 
              "outpatient_costs_iqr_low", "outpatient_costs_iqr_hi", 
              "outpatient_costs_range_low", "outpatient_costs_range_hi",
              "outpatient_costs_ci_low", "outpatient_costs_ci_hi")]

tmp1<-tmp%>%filter((!is.na(tmp[,6])&!is.na(tmp[,7]))|
                     ((!is.na(tmp[,8])&!is.na(tmp[,9]))&
                        ((!is.na(tmp[,10])&!is.na(tmp[,11]))|(!is.na(tmp[,12])&!is.na(tmp[,13])))))%>%
  rename(`Income group` = income_group)

t1<-metamean(tmp1,
             n=ifelse(is.na(outpatient_costs_n),sample_size_patients,outpatient_costs_n), # we get infinitive values for SD
             # n=sample_size_patients,
             mean=outpatient_costs_mean,
             sd=outpatient_costs_sd,
             studlab=ref_subgroup,
             median=outpatient_costs_median,
             q1=outpatient_costs_iqr_low,
             q3=outpatient_costs_iqr_hi,
             min=outpatient_costs_range_low,
             max=outpatient_costs_range_hi,
             method.mean = "Luo", #or "Wan"
             method.sd = "Shi", #or "Wan"
             random = TRUE,
             # prediction = TRUE
             subgroup = `Income group`
)


forest(t1, 
       # layout = "JAMA",
       leftcols = c("studlab","mean","sd","n"), #"complab",
       leftlabs = c("Study","Mean","SD","N"), #"Subgroup",
       rightlabs = c("Mean","95% CI","W (common)","W (random)"),
       allstudies = FALSE # remove inestimable effects
       ,fontsize=9,
       smlab = "",
       # ,xlim=c(0,3000)
       digits.mean = 1,
       digits.sd = 1,
       digits = 1
)



