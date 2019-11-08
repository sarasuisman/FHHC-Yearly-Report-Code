---
title: "FHHC Yearly Report (Year 1)"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Load in packages

```{r}
library(prettyR)
library(psych)
library(lubridate)
```

Working directory

```{r}
#setwd("S:/Indiana Research & Evaluation/FHHC Homelessness/Data and QPR/YearlyReports")
#base_redcap = read.csv("base.csv", header = TRUE, na.strings = c(-99, -98, -97, " "))
#month6_redcap = read.csv("month6.csv", header = TRUE, na.strings = c(-99, -98, -97, " "))
#SPARS_data = read.csv("FHHC.csv", header = TRUE, na.strings = c(-99, -98, -97, -1, -4, -5, -7, -9, -2, -6, -8, " "))
```

Matched Pairs in SPARS Data (base and month 6)

```{r}
dim(SPARS_data)
SPARS_data = SPARS_data[order(SPARS_data$ConsumerID),]
head(SPARS_data)
SPARS_data$ConsumerID = gsub("\\D", "",SPARS_data$ConsumerID)
SPARS_data$ConsumerID = as.numeric(SPARS_data$ConsumerID)
SPARS_data$InterviewDate = mdy(SPARS_data$InterviewDate)
dim(SPARS_data)

## SET DATE FOR ALL DATA IN GRANT YEAR (Y1 ended Sept 30)
SPARS_data = subset(SPARS_data, SPARS_data$InterviewDate < "2019/10/01")


SPARS_base = subset(SPARS_data, SPARS_data$InterviewType_07 == 1)
SPARS_month6 = subset(SPARS_data, SPARS_data$InterviewType_07 == 3)
dim(SPARS_month6)
dim(SPARS_base)

SPARS_wide = merge(SPARS_base, SPARS_month6, by = "ConsumerID", all.y = TRUE)
dim(SPARS_wide)
SPARS_wide$ConsumerID == SPARS_month6$ConsumerID
```

Get date from SPARs and put into REDCap month6 data (we don't have a date in REDCap month6 as it currently stands)
```{r}
SPARS_date_base = data.frame(record_id =  SPARS_base$ConsumerID,  InterviewDate = SPARS_base$InterviewDate)

redcap_date_base = merge(base_redcap, SPARS_date_base, by = "record_id", all.y = TRUE)
head(redcap_date_base)
dim(base_redcap)


#CHANGE DATE to correct year and stuff
base_redcap = subset(redcap_date_base, InterviewDate < "2020-01-01")
dim(base_redcap)


#CHANGE "id" column name in month6 to "record_id" so you can merge based on that variable (did it manually because was struggling to rename column with name that already existed)



base_redcap$record_id
month6_redcap$record_id



redcap_data = merge(base_redcap, month6_redcap, by = "record_id", all.y = TRUE)
head(redcap_data)
dim(redcap_data)

```

Obj. A: Reduce mental health symptomatology by 50% for enrollees with SMI per 6-month and discharge follow-ups.
Use PHQ-9 and GAD-7 mean scores
```{r}
#Depression base_redcap
base_redcap_depression = redcap_data[,29:37]
head(base_redcap_depression)
## number of people
dim(base_redcap_depression)
sum(is.na(base_redcap_depression))
base_redcap_depression$PHQ_9_Total = rowSums(base_redcap_depression)
mean(base_redcap_depression$PHQ_9_Total)

## Depression follow-up
month6_redcap_depression = redcap_data[,140:148]
head(month6_redcap_depression)
### number of people
dim(month6_redcap_depression)
sum(is.na(month6_redcap_depression))
month6_redcap_depression$PHQ_9_Total = rowSums(month6_redcap_depression)
mean(month6_redcap_depression$PHQ_9_Total)

#percent change
p_change_phq =  (mean(month6_redcap_depression$PHQ_9_Total)-mean(base_redcap_depression$PHQ_9_Total))/mean(base_redcap_depression$PHQ_9_Total)
p_change_phq


#Anxiety base_redcap

base_redcap_anxiety = redcap_data[,41:47]
head(base_redcap_anxiety)

## number of people
dim(base_redcap_anxiety)
sum(is.na(base_redcap_anxiety))

#base_redcapline mean score
base_redcap_anxiety$Gad_7_Total = rowSums(base_redcap_anxiety)
mean(base_redcap_anxiety$Gad_7_Total)

## Anxiety follow-up
month6_redcap_anxiety = redcap_data[,152:158]
head(month6_redcap_anxiety)

### number of people
dim(month6_redcap_anxiety)
sum(is.na(month6_redcap_anxiety))

#follow-up mean score
month6_redcap_anxiety$Gad_7_Total = rowSums(month6_redcap_anxiety)
mean(month6_redcap_anxiety$Gad_7_Total)

## Final results
dim(base_redcap_anxiety)[1]



anx_results = data.frame(dep_mean_base_redcap = mean(base_redcap_anxiety$Gad_7_Total), dep_mean_month6_redcapmean = mean(month6_redcap_anxiety$Gad_7_Total), n = dim(base_redcap_anxiety)[1], pchange_anx = (mean(month6_redcap_anxiety$Gad_7_Total-mean(base_redcap_anxiety$Gad_7_Total))/mean(base_redcap_anxiety$Gad_7_Total)))
anx_results
pchange_anx = (mean(month6_redcap_anxiety$Gad_7_Total-mean(base_redcap_anxiety$Gad_7_Total))/mean(base_redcap_anxiety$Gad_7_Total))

mental_health_percent_change = data.frame(depression = p_change_phq, anxiety = pchange_anx)
mental_health_percent_change
```

Obj. B: Increase abstinence vs. past 30-day substance use among 70% of enrolled clients with SUD/COD per 6-month
and discharge follow-ups.
Drug/Alc use questions in NOMS

Tobacco_Use
Alcohol_Use
Cannabis_Use

1 = Never
2 = Once or Twice
3 = Weekly
4 = Daily or Almost Daily 
```{r}
#Compare % abstaining at baseline vs. month 6

#TOBACCO USE at BASELINE

describe.factor(SPARS_wide$Tobacco_Use.x)

#TOBACCO USE at MONTH6

describe.factor(SPARS_wide$Tobacco_Use.y)


#AlCOHOL USE at BASELINE

describe.factor(SPARS_wide$Alcohol_Use.x)


#ALCOHOL USE at MONTH6

describe.factor(SPARS_wide$Alcohol_Use.y)


#CANNABIS USE at BASELINE

describe.factor(SPARS_wide$Cannabis_Use.x)


#CANNABIS USE at MONTH6
describe.factor(SPARS_wide$Cannabis_Use.y)
```

Obj. C: Reduce symptoms of trauma by 50% for enrollees who screen positive for trauma-related conditions per 6-
month and discharge follow-ups.
Use PLC-C mean scores
```{r}
#plc base_redcap


base_redcap_plc = redcap_data[,50:66]
head(base_redcap_plc)

## number of people
dim(base_redcap_plc)
sum(is.na(base_redcap_plc))

#base_redcapline mean score
base_redcap_plc$Plc_Total = rowSums(base_redcap_plc)
mean(base_redcap_plc$Plc_Total)

## PLC follow-up
month6_redcap_plc = redcap_data[,161:177]
head(month6_redcap_plc)

### number of people
dim(month6_redcap_plc)
sum(is.na(month6_redcap_plc))

#follow-up mean score
month6_redcap_plc$Plc_Total = rowSums(month6_redcap_plc)
mean(month6_redcap_plc$Plc_Total)

#percent change

p_change_plc = (mean(month6_redcap_plc$Plc_Total)-mean(base_redcap_plc$Plc_Total))/mean(base_redcap_plc$Plc_Total)
p_change_plc


plc_results = data.frame(plc_base = mean(base_redcap_plc$Plc_Total), plc_month6 = mean(month6_redcap_plc$Plc_Total), p_change = p_change_plc)
plc_results
```

Obj. D: By 9/29/2023, reduce service/utilization costs related to SMI/COD issues (inpatient hospitalization,
emergency room visits, etc.) by 50%.
Use ER usage and Hospitalizations
```{r}
#ER Visits
SPARS_wide_ER = data.frame(base_redcap_ER = SPARS_wide$TimesER.x, month6_redcap_ER = SPARS_wide$TimesER.y)
SPARS_wide_ER_complete = na.omit(SPARS_wide_ER)
#number of people
dim(SPARS_wide_ER_complete)
head(SPARS_wide_ER)
#base_redcap
sum(SPARS_wide_ER$base_redcap_ER)
#Follow-up
sum(SPARS_wide_ER$month6_redcap_ER)

SPARS_wide_ER_complete

#Nights Hospital MHC
SPARS_wide_hospital = data.frame(base_redcap_Hosp = SPARS_wide$NightsHospitalMHC.x, month6_redcap_Hosp = SPARS_wide$NightsHospitalMHC.y)
SPARS_wide_hospital_complete = na.omit(SPARS_wide_hospital)

# number of people

dim(SPARS_wide_hospital_complete)
head(SPARS_wide_hospital)

#sum base_redcapline

sum(SPARS_wide_hospital$base_redcap_Hosp)

#sum month 6
sum(SPARS_wide_hospital$month6_redcap_Hosp)



#sum hospitalizations for mental health and ER visits

#base_redcap ER and hosp

base_redcap_er_hosp = sum(SPARS_wide_ER$base_redcap_ER, SPARS_wide_hospital$base_redcap_Hosp)

#follow-up ER and hosp

month6_redcap_er_hosp = sum(SPARS_wide_ER$month6_redcap_ER, SPARS_wide_hospital$month6_redcap_Hosp)

p_change_ER_hosp = (month6_redcap_er_hosp- base_redcap_er_hosp)/base_redcap_er_hosp 
p_change_ER_hosp


ER_hosp = data.frame(base_ER_hosp = base_redcap_er_hosp, month6_ER_hosp = month6_redcap_er_hosp, percentchange = p_change_ER_hosp)
ER_hosp
```

Obj. E: Reduce past 30-day involvement with the criminal justice system (e.g., arrest, re-arrest, re-conviction) among
60% of enrollees with criminal justice histories per 6-month and discharge follow-ups.

Use variable "NumTimesArrested" from SPARS
```{r}
base_arrests = SPARS_wide$NumTimesArrested.x
base_arrests = na.omit(base_arrests)


month6_arrests = SPARS_wide$NumTimesArrested.y
month6_arrests = na.omit(month6_arrests)


SPARS_wide_arrests = data.frame(base_arrests = SPARS_wide$NumTimesArrested.x, month6_arrests = SPARS_wide$NumTimesArrested.y)
SPARS_wide_arrests = na.omit(SPARS_wide_arrests)
sum(is.na(SPARS_wide_arrests))
SPARS_wide_arrests

sum_base_arrests = sum(base_arrests)
sum_month6_arrests = sum(month6_arrests)

p_change_arrests = (sum_month6_arrests - sum_base_arrests)/sum_base_arrests

arrests = data.frame(base_arrests = sum_base_arrests, month6_arrests = sum_month6_arrests, p_change_arrests = p_change_arrests)
arrests
```

Obj. F: Increase client access to treatment/services, including trauma-focused services, by 90% by 9/29/2023.

Use NOMS variables "Svc_CaseManagement", "Svc_MentalHealth", "Svc_TreatmentPlanning" "Svc_TraumaSpecific", "Svc_Transportation", "Svc_Housing", "Svc_Employment", "Svc_Family", "Svc_ChildCare",  (asks if client has received X service since last NOMS interview)
0 = no, 1 = yes
We don't ask about these at baseline, so just show what services people are getting at 6month
```{r}

#we don't ask about services during baseline NOMS!

#report % of people receiving each service

#MONTH6 Trauma Services
describe.factor(SPARS_wide$Svc_CaseManagement.y)
describe.factor(SPARS_wide$Svc_MentalHealth.y)
describe.factor(SPARS_wide$Svc_TreatmentPlanning.y)
describe.factor(SPARS_wide$Svc_TraumaSpecific.y)
describe.factor(SPARS_wide$Svc_Transportation.y)
describe.factor(SPARS_wide$Svc_Housing.y)
describe.factor(SPARS_wide$Svc_Employment.y)
describe.factor(SPARS_wide$Svc_Family.y)
describe.factor(SPARS_wide$Svc_ChildCare.y)
```

Obj. G: Increase recovery capital among 90% of enrollees by 9/29/2023.
Use BARC-10 mean scores
```{r}
#Barc base_redcap
base_redcap_barc = base_redcap[,88:97]
base_redcap_barc = na.omit(base_redcap_barc)
head(base_redcap_barc)

## number of people
dim(base_redcap_barc)
base_redcap_barc = na.omit(base_redcap_barc)
sum(is.na(base_redcap_barc))

#base_redcap mean score
base_redcap_barc$Barc_Total = rowSums(base_redcap_barc)
mean(base_redcap_barc$Barc_Total)
mean_base_barc = mean(base_redcap_barc$Barc_Total)

## Barc follow-up
month6_redcap_barc = month6_redcap[,83:92]
head(month6_redcap_barc)

### number of people
dim(month6_redcap_barc)
month6_redcap_barc = na.omit(month6_redcap_barc)
sum(is.na(month6_redcap_barc))

#follow-up mean score
month6_redcap_barc$Barc_Total = rowSums(month6_redcap_barc)
mean_month6_barc = mean(month6_redcap_barc$Barc_Total)
mean_month6_barc


p_change_barc = (mean_month6_barc-mean_base_barc)/mean_base_barc
p_change_barc

barc = data.frame(base_barc = mean_base_barc, month6_barc = mean_month6_barc, p_change_barc = p_change_barc)
barc
```

Obj. H: Achieve 80% client retention rate per 6-month and discharge follow-ups.
Number of enrollees eligible for follow-up that have been seen for 6 month (based on date calculated --> don't count people that have not yet reached reassessment window)
USE SPARS RATE
```{r}
#currently 41 clients eligible for follow-up, 37 of those seen for follow up

retention_rate = 37/41
retention_rate
```


Goal IV: Increase permanent housing and other services that support recovery for clients.


Obj. A: Provide housing navigation and CES linkage to 100% of enrollees per 6-month and discharge follow-ups.
Use variable "Svc_Housing" from SPARS (asks about housing services since last NOMS interview)
```{r}

describe.factor(SPARS_wide$Svc_Housing.y)

```

Obj. B: Place 80% of enrollees in permanent housing by 9/29/2023.
Use participant tracker --> number housed/number enrolled
```{r}
#housed enrollees divided by total enrollees


housed_enrollees = 38/50
housed_enrollees
```

Obj. C: As appropriate to their ICPs, assist 100% of enrollees to identify/secure employment per 6-month and discharge follow-ups.
Use "did you receive help getting a job" question from REDCap?

Most people say "no" to this, so I'm not sure if employment typically isnt a main concern for our clients or what
```{r}
month6_job_help = redcap_data$did_you_receive_help_getti
month6_job_help = na.omit(month6_job_help)
sum_month6_job_help = sum(month6_job_help)
sum_month6_job_help
```

Obj. D: Increase enrollment in health insurance, Medicaid, VA, SSI/SSDI, other benefit programs by 80% for clients
in need of/eligible for these benefits by 9/29/2023.
Use Benefits Tracking in REDCap
```{r}
#benefit at base, benefit at month 6, then percent change from base to month 6

#MEDICAID OR MEDICARE

#BASELINE

base_medicaid = redcap_data$medicaid_or_medicare.x
base_medicaid = na.omit(base_medicaid)
sum_base_med = sum(base_medicaid)

#MONTH6

month6_medicaid = redcap_data$medicaid_or_medicare.y
month6_medicaid = na.omit(month6_medicaid)
sum_month6_med = sum(month6_medicaid)

p_change_med = (sum_month6_med - sum_base_med)/sum_base_med

medicaid_or_medicare = data.frame(base = sum_base_med, month6 = sum_month6_med, percentchange = p_change_med)

medicaid_or_medicare


#HIP

#BASELINE

base_hip = redcap_data$healthy_indiana_plan_hip.x
base_hip = na.omit(base_hip)
sum_base_hip = sum(base_hip)


#MONTH6
month6_hip = redcap_data$healthy_indiana_plan_hip.y
month6_hip = na.omit(month6_hip)
sum_month6_hip = sum(month6_hip)

p_change_hip = (sum_month6_hip - sum_base_hip)/sum_base_hip

hip = data.frame(base = sum_base_hip, month6 = sum_month6_hip, percentchange = p_change_hip)

hip

#SSI or SSDI

#BASELINE

base_ssi = redcap_data$ssi_or_ssdi.x
base_ssi = na.omit(base_ssi)
sum_base_ssi = sum(base_ssi)

#MONTH 6
month6_ssi = redcap_data$ssi_or_ssdi.y
month6_ssi = na.omit(month6_ssi)
sum_month6_ssi = sum(month6_ssi)


p_change_ssi = (sum_month6_ssi - sum_base_ssi)/sum_base_ssi

ssi_or_ssdi = data.frame(base = sum_base_ssi, month6 = sum_month6_ssi, percentchange = p_change_ssi)

ssi_or_ssdi


#HUD

#BASELINE

base_hud = redcap_data$hud_or_section_8.x
base_hud = na.omit(base_hud)
sum_base_hud = sum(base_hud)

#MONTH6

month6_hud = redcap_data$hud_or_section_8.y
month6_hud = na.omit(month6_hud)
sum_month6_hud = sum(month6_hud)

p_change_hud = (sum_month6_hud - sum_base_hud)/sum_base_hud

hud = data.frame(base = sum_base_hud, month6 = sum_month6_hud, percentchange = p_change_hud)

hud


#SNAP

#BASE

base_snap = redcap_data$snap_food_stamps.x
base_snap = na.omit(base_snap)
sum_base_snap = sum(base_snap)

#MONTH6
month6_snap = redcap_data$snap_food_stamps.y
month6_snap = na.omit(month6_snap)
sum_month6_snap = sum(month6_snap)

p_change_snap = (sum_month6_snap - sum_base_snap)/sum_base_snap

snap = data.frame(base = sum_base_snap, month6 = sum_month6_snap, percentchange = p_change_snap)

snap


#PERCENT CHANGE OF ALL BENEFITS

benefits_percent_change = data.frame(medicaid_medicare = p_change_med, hip = p_change_hip, ssi_ssdi = p_change_ssi, hud = p_change_hud, snap = p_change_snap)

benefits_percent_change
```

Obj. E: Increased social connectedness among 80% of enrollees per 6-month and discharge follow-ups.
Use social relationships section of SPARS data
variables = Friendships, EnjoyPeople, BelongInCommunity, SupportFromFamily, SupportiveFamilyFriends, GenerallyAccomplishGoal

1 = Strong Disagree
2 = Disagree
4 = Agree
5 = Strongly Agree

Is there a better way to look at this data?
```{r}
#mean and then percent change of each variable
#do rowsum in new data set, %change over all

#Baseline

describe.factor(SPARS_wide$Friendships.x)
describe.factor(SPARS_wide$EnjoyPeople.x)
describe.factor(SPARS_wide$BelongInCommunity.x)
describe.factor(SPARS_wide$SupportFromFamily.x)
describe.factor(SPARS_wide$SupportiveFamilyFriends.x)

#Month6 
describe.factor(SPARS_wide$Friendships.y)
describe.factor(SPARS_wide$EnjoyPeople.y)
describe.factor(SPARS_wide$BelongInCommunity.y)
describe.factor(SPARS_wide$SupportFromFamily.y)
describe.factor(SPARS_wide$SupportiveFamilyFriends.y)


```

Obj. F: Improve independent living skills among 80% of enrollees per 6-month and discharge follow-ups.

Use variable "CapableManagingHealthCareNeeds" from SPARS 

" I feel capable of managing my health care needs..."

1 = On my own most of the time
2 = On my own some of the time and with support from
others some of the time
3 = With support from others most of the time
4 = Rarely or never
```{r}
#do same thing as social connectedness part
#OR use generally accomplish what I set out to do

#baseline independent living
describe.factor(SPARS_wide$CapableManagingHealthCareNeeds.x)
#month6 independent living
describe.factor(SPARS_wide$CapableManagingHealthCareNeeds.y)
```

