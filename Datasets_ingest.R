# This file will preprocess the IFR / Hopitalized fits from other papers, then simulate.

# This will be the code to preprocess inputs.

# The final version will need to create a combined posterior estimate over multiple studies.


GBD_Data <- read.csv("IHME-GBD_2017_DATA-bc10ddbb-1.csv")
GBD_Data <- reshape(data=GBD_Data,
                    idvar=c("measure","location",	"sex",	"age",	"cause"),
                    v.names = "val",
                    timevar= 'metric',
                    direction="wide",
                    drop=c("number","rate",'upper','lower')) # Just use percentages.

# levels(GBD_Data$cause) #We definitely don't want all of these. 
#I think we just want to use Cardiovasc, Diabetes, "Chronic respiratory diseases", and "Respiratory infections and tuberculosis" - I need to check with a medical person. -DM
#Note: Other noncommunicable disease == injuries (car, etc.)

# China, Hospitalized.
# https://erj.ersjournals.com/content/55/5/2000547
# The mean age was 48.9 years and 686 (42.7%) patients were female. Severe cases accounted for 16.0% of the study population. 131 (8.2%) patients reached the composite end-points. 399 (25.1%) reported having at least one comorbidity. The most prevalent comorbidity was hypertension (16.9%), followed by diabetes (8.2%). 130 (8.2%) patients reported having two or more comorbidities. After adjusting for age and smoking status, COPD (HR (95% CI) 2.681 (1.424–5.048)), diabetes (1.59 (1.03–2.45)), hypertension (1.58 (1.07–2.32)) and malignancy (3.50 (1.60–7.64)) were risk factors of reaching the composite end-points. The hazard ratio (95% CI) was 1.79 (1.16–2.77) among patients with at least one comorbidity and 2.59 (1.61–4.17) among patients with two or more comorbidities.

require(readxl)

China_ERJ_Comorbidities_Deaths<- read_excel('Study Summary Data/China_ERJ.xls',1)
China_ERJ_Comorbidities_Genders<- read_excel('Study Summary Data/China_ERJ.xls',2)


# JAMA NYC:
# https://jamanetwork.com/journals/jama/article-abstract/2765184
# A total of 5700 patients were included (median age, 63 years [interquartile range {IQR}, 52-75; range, 0-107 years]; 39.7% female) (Table 1). The median time to obtain polymerase chain reaction testing results was 15.4 hours (IQR, 7.8-24.3). The most common comorbidities were hypertension (3026, 56.6%), obesity (1737, 41.7%), and diabetes (1808, 33.8%). The median score on the Charlson Comorbidity Index was 4 points (IQR, 2-6), which corresponds to a 53% estimated 10-year survival and reflects a significant comorbidity burden for these patients. At triage, 1734 patients (30.7%) were febrile, 986 (17.3%) had a respiratory rate greater than 24 breaths/min, and 1584 (27.8%) received supplemental oxygen (Table 2 and Table 3). The first test for COVID-19 was positive in 5517 patients (96.8%), while 183 patients (3.2%) had a negative first test and positive repeat test. The rate of co-infection with another respiratory virus for those tested was 2.1% (42/1996). Discharge disposition by 10-year age intervals of all 5700 study patients is included in Table 4. Length of stay for those who died, were discharged alive, and remained in hospital are presented as well. Among the 3066 patients who remained hospitalized at the final study follow-up date (median age, 65 years [IQR 54-75]), the median follow-up at time of censoring was 4.5 days (IQR, 2.4-8.1). Mortality was 0% (0/20) for male and female patients younger than 20 years. Mortality rates were higher for male compared with female patients at every 10-year age interval older than 20 years.
#Data now in xls. Weird summaries.

NYC_JAMA_Age_Outcome<- read_excel('Study Summary Data/NYC_JAMA.xls',1)
NYC_JAMA_Age_Outcome_resplit<- read_excel('Study Summary Data/NYC_JAMA.xls',2)
NYC_JAMA_Comorbidities<- read_excel('Study Summary Data/NYC_JAMA.xls',3)

# Coborbidity Metaanalysis China:
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7185114/
China_Meta_OR<- read_excel('Study Summary Data/China_Aging_Meta.xls',1)


# IFR Metaanalysis, NBER:
# https://www.medrxiv.org/content/10.1101/2020.07.23.20160895v2
# Several different datasets.
NBER_IFR_Benchmark_Studies <- read_excel('Study Summary Data/NBER_IFR_Meta_Dataset.xlsx',1)
#Places: Belgium. Geneva, Indiana, New York, Spain, Sweden
NBER_IFR_US_Studies <- read_excel('Study Summary Data/NBER_IFR_Meta_Dataset.xlsx',2, skip=1)
names(NBER_IFR_US_Studies)[6:7]<-c('Infect 95_lower','Infect 95_upper')
names(NBER_IFR_US_Studies)[10:11]<-c('IFR_95_lower','IFR_95_upper')


# Now, studies on outcomes per age without comorbidities...


