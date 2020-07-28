# RiskModel
This is a project to model the risk of a human challenge trial, supporting a journal paper in preparation.

Code in this repository is written in the [R statistical language](https://www.r-project.org/), using [RStudio projects](https://rstudio.com/) both to replicate other analyses, and (planned) to simulate IFR and total risk distributions for the purpose of this paper.


## General Notes on sources of error
The published literature is still developing rapidly.

The two published papers, in Science and Lancet Inf Dis, are not necessarily reflective of current risk as treatement has been refined and IFR seems to have declines as clinical supprt proptocols have been adapted and improved. This makes them likely to be conservative. They also do not exclude comorbidities, which would be screened out in a HCT.

There is much less understanding of longer term risks of COVID-19, and a far less confident estimate of those impacts is possible. The majority of longer-term sequelae seem to be in hospitalized patients, so we can plausibly use a percentage of hospitalization rate as a proxy.


## Input papers

### Published:
- Salje H et al. Estimating the burden of SARS-CoV-2 in France. Science 10 JUL 2020 : 208-211 https://science.sciencemag.org/content/369/6500/208
-- France, Hospitalized only. Likely to undercount nursing home fatality rates.
-- IFR, Hospitalization, ICU Simulation / probability data extracted.

- China (Lancet ID)
-- China plus other early cases
-- Data extraction in process.

- USA (JAMA, Richardson)
-- (Presently) Excluded because no code for risk analysis is available.


### Preprints, not being reviewed (yet?):

- Rinaldi G, Paradisi M. An empirical estimate of the infection fatality rate of COVID-19 from
the first Italian outbreak. medRxiv. 10.1101/2020.04.18.20070912v2
-- Uses total excess mortality to infer IFR

- Perez-Saez, Azman. Serology-informed estimates of SARS-COV-2 infection fatality risk in Geneva, Switzerland.

Modi C, et al. How deadly is COVID-19? A rigorous analysis of excess mortality and age-dependent fatality rates in Italy. DOI: 10.1101/2020.04.15.20067074v3

"Streeck H, Schulte B, Kuemmerer B, Richter E, Hoeller T, Fuhrmann C, et al. Infection fatality
rate of SARS-CoV-2 infection in a German community with a super-spreading event. medRxiv"

Meyerowitz Katz, Merone, "A systematic review and meta-analysis of published research data on COVID-19 infection-fatality rates" medarxhiv

"e: Seroconversion of a city: Longitudinal monitoring of SARS-CoV-2 seroprevalence
2 in New York City  Stadbauer, Krammer"

"Remarkable variability in SARS-CoV-2 antibodies across Brazilian
2 regions: nationwide serological household survey in 27 states -- Hallal, victora"

Estimating The Infection Fatality Rate Among Symptomatic COVID-19 Cases In The United States