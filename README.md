# RiskModel
This is a project to model the risk of a human challenge trial, supporting a journal paper in preparation.

## Contents of this repository

Code in this repository is written in the [R statistical language](https://www.r-project.org/), using [RStudio projects](https://rstudio.com/) both to replicate other analyses, and (planned) to simulate IFR and total risk distributions for the purpose of this paper.

(Original/work in progress) workflow is as follows:

1. Probabilities of death, hospitalisation, ICU severity are derived for different age brackets from French and Chinese datasets. These are located in subfolders. There is an analysis script in R, which calls on [Stan](https://mc-stan.org/).
2. The derived probabilities (from Stan) are then saved as .csv files, such as `France_Male_p_death_` etc. in the main folder
3. A study simulation script `Riskmodel_Alpha.R` uses the csv inputs to simulate trials and calculate risk.

## (Eventual) Model Structure
We are building a Bayesian model in MC-Stan, using rStan, to build do a meta-analysis of existing estimates of IFR which accounts for age-structure and corrects for comorbidities in the populations in question. This meta-analysis will be used as an input into the risk model (currently Riskmodel_Alpha.R,) which estimates the risks from a COVID-19 Challenge Study. of a given type.

The current version uses the Salje H. et al 2020 study from 



## General Notes on sources of error

The published literature is still developing rapidly.

The two published papers, in Science and Lancet Inf Dis, are not necessarily reflective of current risk as treatement has been refined and IFR seems to have declines as clinical supprt proptocols have been adapted and improved. This makes them likely to be conservative. They also do not exclude comorbidities, which would be screened out in a HCT.

There is much less understanding of longer term risks of COVID-19, and a far less confident estimate of those impacts is possible. The majority of longer-term sequelae seem to be in hospitalized patients, so we can plausibly use a percentage of hospitalization rate as an upper-bound proxy.



## Input papers

### Published:
- Salje H et al. Estimating the burden of SARS-CoV-2 in France. Science 10 JUL 2020 : 208-211 https://science.sciencemag.org/content/369/6500/208
-- France, Hospitalized only. Likely to undercount nursing home fatality rates.
-- Extracted model from original repository and reran.
-- IFR, Hospitalization, ICU Simulation / probability data extracted. (Now corrected to use the conditional values, per Witold.)

- China (Lancet ID)
-- IFR and Hospitalization rates
-- China plus other early cases
-- Data extraction completed. Model very slow.


- NY, USA (JAMA, Richardson) https://jamanetwork.com/journals/jama/fullarticle/2765184
-- Comorbidities and outcomes conditional on hospitalization.
--- Needs review for incorporation into cormorbidities analysis.
-- From 12 Hospitals in New York, 


### Preprints, reviewed:

Gianluca Rinaldi, Matteo Paradisi. An empirical estimate of the infection fatality rate of COVID-19 from the first Italian outbreak. doi: https://doi.org/10.1101/2020.04.18.20070912
-- Analysis code available, to run.
-- 


### Preprints, not reviewed yet:

Modi C, et al. How deadly is COVID-19? A rigorous analysis of excess mortality and age-dependent fatality rates in Italy. DOI: 10.1101/2020.04.15.20067074v3


- Rinaldi G, Paradisi M. An empirical estimate of the infection fatality rate of COVID-19 from
the first Italian outbreak. medRxiv. 10.1101/2020.04.18.20070912v2
-- Uses total excess mortality to infer IFR


- Perez-Saez, Azman. Serology-informed estimates of SARS-COV-2 infection fatality risk in Geneva, Switzerland.




"Streeck H, Schulte B, Kuemmerer B, Richter E, Hoeller T, Fuhrmann C, et al. Infection fatality
rate of SARS-CoV-2 infection in a German community with a super-spreading event. medRxiv"

Meyerowitz Katz, Merone, "A systematic review and meta-analysis of published research data on COVID-19 infection-fatality rates" medarxhiv

"e: Seroconversion of a city: Longitudinal monitoring of SARS-CoV-2 seroprevalence
2 in New York City  Stadbauer, Krammer"

"Remarkable variability in SARS-CoV-2 antibodies across Brazilian
2 regions: nationwide serological household survey in 27 states -- Hallal, victora"

Estimating The Infection Fatality Rate Among Symptomatic COVID-19 Cases In The United States
