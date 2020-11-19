# RiskModel

This is a project to model the risk of a human challenge trial for COVID-19, supporting a journal paper which is now in submission. This repo comprises (1) a Bayesian model for some of the risks associated with COVID-19 infection and (2) a Shiny app that quantifies risks associated with participation in a challenge trial.

All parts of the repo are in active development and we expect them to change substantially.


## Structure of this repo

1. A Bayesian model of mortality is included in `ifr-model/`. PDF summary of the model and its results is inside that folder. 
   * Analysis is done in R. Stan package is required to run the model.
   * The model is stand-alone (it's sufficient to only copy `ifr-model` folder); all required data is in `ifr-model/data/`
   * To re-run the model and re-generate the outputs you can run the `Rmd` file but with `eval = FALSE` options changed to `TRUE`. By default no models are re-ran.
2. The Shiny app is in `ShinyApp/`
   * The app depends on outputs of `ifr-model/` which define Bayesian posteriors for some risks used by the app.
3. Some additional data extraction is included in `Study Summary Data/`. These datasets are not used either 1 or 2 but are intended for future model extensions.


## Our result and publication

We will update this sections once a pre-print based on this repo is online.

This is the abstract:

> Human Challenge Trials (HCTs) are a potential method to accelerate development of vaccines and therapeutics. However, HCTs for COVID-19  pose ethical and practical challenges, in part due to the unclear and developing risks. In this paper, we introduce an interactive model for exploring some risks of a SARS-COV-2 dosing study, a  prerequisite for any COVID-19 challenge trials. The risk estimates we use are based on a Bayesian evidence synthesis model which can incorporate new data on infection fatality rates (IFRs) to patients, and infer rates of hospitalization. We have also created a web tool to explore risk under different study design parameters and participant scenarios. Finally, we use our model to estimate individual risk, as well as the overall mortality and hospitalization risk in a dosing study.

> Based on the Bayesian model we expect IFR for someone between 20 and 30 years of age to be 17.5 in 100,000, with 95% uncertainty interval from 12.8 to 23.6.  Using this estimate, we find that a simple 50-person dosing trial using younger individuals has a 99.1% (95% CI: 98.8% to 99.4%) probability of no fatalities, and a 92.8% (95% CI: 90.3% to 94.6%) probability of no cases requiring hospitalization. However, this IFR will be reduced in an HCT via screening for comorbidities, as well as providing medical care and aggressive treatment for any cases which occur, so that with stronger assumptions, we project the risk to be as low as 3.1 per 100,000, with a 99.85% (95% CI: 99.7% to 99.9%) chance of no fatalities, and a 98.7% (95% CI:  97.4% to 99.3%) probability of no cases requiring hospitalization.
