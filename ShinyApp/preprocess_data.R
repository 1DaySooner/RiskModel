
#
# This file will preprocess the IFR / Hospitalized fits based on the model and other inputs, then output the csv used in the interactive app.

require(dplyr)

# This will be the code to preprocess inputs.

# App currently uses the mean and the 95th percentile (One-directional), as well as the 95% CI
q=0.95
quants=c((1-q)/2,0.5, q,1 - ((1-q)/2))


# Ideally, structure the data into a single dataframe to pull numbers from. (Done.)
# The final version will need to create a combined posterior estimate over multiple studies. (From IFR model.)

# We will also need to correct for lack of cormorbidities based on studies with that information. (Done in stan IFR model.)

# Final Stan Model Output:
ifr_model_out <- read.csv("../ifr-model/output/bayesian-posteriors-shiny.csv", row.names=1, check.names=FALSE)
#replications, #replications for mean IFR in 20-30 (takes into account heterogeneity) 
#inv_logit(hypermean),   #posterior mean for IFR in 20-30 (mean over included studies)
#theta_min,    #lowest estimated posterior IFR in 20-30 (in this case in Belgium)
#rr_healthy,   #ratio of mortality in healthy to general population
#final_ifr)    #theta_min/rr_healthy


# We use the France study as our basis for estimating relative risk of hospitalization from deaths. (Salje et al,  science.sciencemag.org/cgi/content/full/science.abc3517)
# Load data from files.

Male_probDeath_France <- cbind(gend = "m", outc="death", read.csv("France_Male_p_death_by_age_range.csv", row.names=1, check.names=FALSE))
#Male_probDeath_France$ages <- rownames(Male_probDeath)
Female_probDeath_France <- cbind(gend = "f", outc="death", read.csv("France_Female_p_death_by_age_range.csv", row.names=1, check.names=FALSE))
#Female_probDeath_France$ages <- rownames(Female_probDeath)
Male_probHosp_France <- cbind(gend = "m", outc="hosp", read.csv("France_Male_p_hospitalization_by_age_range.csv", row.names=1, check.names=FALSE))
#Male_probHosp_France$ages <- rownames(Male_probHosp)
Female_probHosp_France <- cbind(gend = "f", outc="hosp", read.csv("France_Female_p_hospitalization_by_age_range.csv", row.names=1, check.names=FALSE))
#Female_probHosp_France$ages <- rownames(Female_probHosp)
#Male_probICU_France <- cbind(gend = "m", outc="ICU", read.csv("France_Male_p_death_by_age_range.csv", row.names=1, check.names=FALSE))
#Male_probICU_France$ages <- rownames(Male_probICU)
#Female_probICU_France <- cbind(gend = "f", outc="ICU", read.csv("France_Female_p_death_by_age_range.csv", row.names=1, check.names=FALSE))
#Female_probICU_France$ages <- rownames(Female_probICU)

# participants <- 10:100

problist = paste0(formatC(c(0:100)),"%")
agelist = c("0 to 19", "20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", "70 to 79", "80 to 100")

all_probs_France <- 
  rbind(
    reshape(Female_probDeath_France, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    reshape(Male_probDeath_France, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    reshape(Male_probHosp_France, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    reshape(Female_probHosp_France, direction='long', v.name="value", varying=problist, times=problist, idvar="ages")#,
    #reshape(Male_probICU_France, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    #reshape(Female_probICU_France, direction='long', v.name="value", varying=problist, times=problist, idvar="ages")
  )


names(all_probs_France)[names(all_probs_France)=='time'] <- "probability"
row.names(all_probs_France) <- NULL
all_probs_France['ages']=agelist[unlist(all_probs_France['ages'])]

rr_death_hosp = reshape(all_probs_France[which(all_probs_France['probability']=='50%'),], direction='wide', v.name='value', timevar='outc', idvar=c('gend','ages'))

rr_death_hosp['probability']=NULL

names(rr_death_hosp)[names(rr_death_hosp) == 'gend'] <- 'Gender'
rr_death_hosp[,'Gender']=toupper(rr_death_hosp[,'Gender'])

rr_death_hosp_B=rr_death_hosp[which(rr_death_hosp['Gender']=='M'),]
rr_death_hosp_B['Gender'] = 'B'
rr_death_hosp_B['value.death'] = (rr_death_hosp[which(rr_death_hosp['Gender']=='M'), 'value.death'] + rr_death_hosp[which(rr_death_hosp['Gender']=='F'), 'value.death']) /2
rr_death_hosp_B['value.hosp'] = (rr_death_hosp[which(rr_death_hosp['Gender']=='M'), 'value.hosp'] + rr_death_hosp[which(rr_death_hosp['Gender']=='F'), 'value.hosp']) / 2

rr_death_hosp = rbind(rr_death_hosp,rr_death_hosp_B)

rr_death_hosp[which(rr_death_hosp['Gender']=='M'),'rr_gend'] <- rr_death_hosp[which(rr_death_hosp['Gender']=='M'), 'value.death']/rr_death_hosp_B['value.death']
rr_death_hosp[which(rr_death_hosp['Gender']=='F'),'rr_gend'] <- rr_death_hosp[which(rr_death_hosp['Gender']=='F'), 'value.death']/rr_death_hosp_B['value.death']
rr_death_hosp[which(rr_death_hosp['Gender']=='B'),'rr_gend'] <- 1

rr_death_hosp['rr_hosp'] <- rr_death_hosp['value.hosp']/rr_death_hosp['value.death']

# Add case, status, gender, ages.
covariates = full_join(data.frame(Gender=c('M','F','B')), data.frame(outc=c('death','hosp')), by=character())
#covariates = full_join(covariates,data.frame(status=c('healthy','all')), by=character())
#covariates = full_join(covariates,, by=character())
# covariates = full_join(covariates,data.frame(ages=agelist), by=character()) <- This doesn't do ages!


rr_death_hosp_age = cbind(rr_death_hosp,rr_age=1)
rr_death_hosp_age[which(rr_death_hosp_age['ages']=='0 to 19'),'rr_age']  <- rr_death_hosp_age[which(rr_death_hosp_age['ages']=='0 to 19'),'value.death']/rr_death_hosp_age[which(rr_death_hosp_age['ages']=='20 to 29'),'value.death']
rr_death_hosp_age[which(rr_death_hosp_age['ages']=='30 to 39'),'rr_age']  <- rr_death_hosp_age[which(rr_death_hosp_age['ages']=='30 to 39'),'value.death']/rr_death_hosp_age[which(rr_death_hosp_age['ages']=='20 to 29'),'value.death']
rr_death_hosp_age[which(rr_death_hosp_age['ages']=='40 to 49'),'rr_age']  <- rr_death_hosp_age[which(rr_death_hosp_age['ages']=='40 to 49'),'value.death']/rr_death_hosp_age[which(rr_death_hosp_age['ages']=='20 to 29'),'value.death']
rr_death_hosp_age[which(rr_death_hosp_age['ages']=='50 to 59'),'rr_age']  <- rr_death_hosp_age[which(rr_death_hosp_age['ages']=='50 to 59'),'value.death']/rr_death_hosp_age[which(rr_death_hosp_age['ages']=='20 to 29'),'value.death']
rr_death_hosp_age[which(rr_death_hosp_age['ages']=='60 to 69'),'rr_age']  <- rr_death_hosp_age[which(rr_death_hosp_age['ages']=='60 to 69'),'value.death']/rr_death_hosp_age[which(rr_death_hosp_age['ages']=='20 to 29'),'value.death']
rr_death_hosp_age[which(rr_death_hosp_age['ages']=='70 to 79'),'rr_age']  <- rr_death_hosp_age[which(rr_death_hosp_age['ages']=='70 to 79'),'value.death']/rr_death_hosp_age[which(rr_death_hosp_age['ages']=='20 to 29'),'value.death']
rr_death_hosp_age[which(rr_death_hosp_age['ages']=='80 to 100'),'rr_age']  <- rr_death_hosp_age[which(rr_death_hosp_age['ages']=='80 to 100'),'value.death']/rr_death_hosp_age[which(rr_death_hosp_age['ages']=='20 to 29'),'value.death']



relative_risk_for_simulation = cbind(full_join(covariates, rr_death_hosp_age, by=c('Gender')), rr=0)
relative_risk_for_simulation['rr'] = relative_risk_for_simulation['rr_gend'] * relative_risk_for_simulation['rr_age']
relative_risk_for_simulation[which(relative_risk_for_simulation['outc']=='hosp'),'rr']= relative_risk_for_simulation[which(relative_risk_for_simulation['outc']=='hosp'),'rr'] * relative_risk_for_simulation[which(relative_risk_for_simulation['outc']=='hosp'),'rr_hosp']

relative_risk_for_simulation[,'gend']=tolower(relative_risk_for_simulation[,'Gender'])
relative_risk_for_simulation[c('value.death','value.hosp','rr_hosp','rr_gend','rr_age','Gender')] = NULL


write.csv(relative_risk_for_simulation, file = "relative_risks.csv")



# Extract the quantiles from the ifr analysis
Base_Quantiles = data_frame(p = quants, ifr_min_25 = quantile(ifr_model_out$ifr_min_25, probs = quants), ifr_min_35 = quantile(ifr_model_out$ifr_min_25, probs = quants), meta_ifr_25 = quantile(ifr_model_out$metaanalysis_ifr_25, probs = quants), meta_ifr_35 = quantile(ifr_model_out$metaanalysis_ifr_35, probs = quants), min_healthy_ifr_25 = quantile(ifr_model_out$min_healthy_ifr_25, probs = quants), min_healthy_ifr_35 = quantile(ifr_model_out$min_healthy_ifr_35, probs = quants), meta_healthy_ifr_25 = quantile(ifr_model_out$metaanalysis_healthy_ifr_25, probs = quants), meta_healthy_ifr_35 = quantile(ifr_model_out$metaanalysis_healthy_ifr_35, probs = quants))


Base_Quantiles = rbind(cbind(Base_Quantiles,ages="20 to 29"),cbind(Base_Quantiles,ages="30 to 39"))
Base_Quantiles['ifr_min'] = Base_Quantiles['ifr_min_25']
Base_Quantiles['ifr_meta'] = Base_Quantiles['meta_ifr_25']
#Base_Quantiles['not_healthy_rr'] = Base_Quantiles['rr_25']
#Base_Quantiles['hypermean_ifr'] = Base_Quantiles['hyper_ifr_25']
Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'ifr_meta'] = Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'meta_ifr_35']
Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'ifr_min'] = Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'ifr_min_35']
Base_Quantiles['ifr_min_25'] = NULL
Base_Quantiles['ifr_min_35'] = NULL
Base_Quantiles['meta_ifr_25'] = NULL
Base_Quantiles['meta_ifr_35'] = NULL

Base_Quantiles['ifr_min_healthy'] = Base_Quantiles['min_healthy_ifr_25']
Base_Quantiles['ifr_meta_healthy'] = Base_Quantiles['meta_healthy_ifr_25']
Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'ifr_meta_healthy'] = Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'meta_healthy_ifr_35']
Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'ifr_min_healthy'] = Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'min_healthy_ifr_35']
Base_Quantiles['min_healthy_ifr_25'] = NULL
Base_Quantiles['min_healthy_ifr_35'] = NULL
Base_Quantiles['meta_healthy_ifr_25'] = NULL
Base_Quantiles['meta_healthy_ifr_35'] = NULL

Base_Healthy_Quantiles = rbind(cbind(Base_Quantiles,case="meta"),cbind(Base_Quantiles,case="best"))
Transformed_Quantiles = rbind(cbind(Base_Healthy_Quantiles,status="healthy"),cbind(Base_Healthy_Quantiles,status="all"))

Transformed_Quantiles[which(Transformed_Quantiles['case']=="best" & Transformed_Quantiles['status']=="healthy"),'probability'] = Transformed_Quantiles[which(Transformed_Quantiles['case']=="best" & Transformed_Quantiles['status']=="healthy"),'ifr_min_healthy']
Transformed_Quantiles[which(Transformed_Quantiles['case']=="meta" & Transformed_Quantiles['status']=="healthy"),'probability'] = Transformed_Quantiles[which(Transformed_Quantiles['case']=="meta" & Transformed_Quantiles['status']=="healthy"),'ifr_meta_healthy']

Transformed_Quantiles[which(Transformed_Quantiles['case']=="best" & Transformed_Quantiles['status']=="all"),'probability'] = Transformed_Quantiles[which(Transformed_Quantiles['case']=="best" & Transformed_Quantiles['status']=="all"),'ifr_min']
Transformed_Quantiles[which(Transformed_Quantiles['case']=="meta" & Transformed_Quantiles['status']=="all"),'probability'] = Transformed_Quantiles[which(Transformed_Quantiles['case']=="meta" & Transformed_Quantiles['status']=="all"),'ifr_meta']

Transformed_Quantiles['ifr_meta'] = NULL
Transformed_Quantiles['ifr_min'] = NULL
Transformed_Quantiles['ifr_meta_healthy'] = NULL
Transformed_Quantiles['ifr_min_healthy'] = NULL

# Now we include the other data sets used to get hospitalization and gender:


# Nongender_Dataset = merge(Base_Quantiles, Quantiles_for_age_gender_rr, by=NULL, all.y=FALSE)

Combined_proto_dataset = inner_join(Gender_dataset, rr_death_hosp, by=c('ages', "Gender"))

Combined_dataset = rbind(cbind(Combined_proto_dataset,Gender='F'),cbind(Combined_proto_dataset,Gender='M'),cbind(Combined_proto_dataset,Gender='B'))


Combined_dataset['value.death'] = NULL
Combined_dataset['value.hosp'] = NULL


Combined_dataset[which(Combined_dataset['Gender']=='M'),'probability']=Combined_dataset[which(Combined_dataset['Gender']=='M'),'probability'] * Combined_dataset[which(Combined_dataset['Gender']=='M'),'rr_gend']

Combined_dataset[which(Combined_dataset['Gender']=='F'),'probability']=Combined_dataset[which(Combined_dataset['Gender']=='F'),'probability'] * Combined_dataset[which(Combined_dataset['Gender']=='F'),'rr_gend']

Final_Dataset = rbind(cbind(Combined_dataset,outc='death'),cbind(Combined_dataset,outc='hosp'))

Final_Dataset[which(Final_Dataset['outc']=='hosp'),'probability'] = Final_Dataset[which(Final_Dataset['outc']=='hosp'),'probability'] * Final_Dataset[which(Final_Dataset['outc']=='hosp'),'rr_hosp']


Final_Dataset[,'gend']=tolower(Final_Dataset[,'Gender'])
Final_Dataset['Gender'] = NULL
Final_Dataset['rr_hosp'] = NULL
Final_Dataset['rr_gend'] = NULL

names(Final_Dataset)[names(Final_Dataset) == 'probability'] <- 'value'
names(Final_Dataset)[names(Final_Dataset) == 'p'] <- 'probability'


write.csv(Final_Dataset, file = "app_dataset.csv")
# Unsure what this is about when I run write.csv
# Warning message:
# Unknown or uninitialised column: `rr_all_B`.

# Copy most recent bayesian_ifr_model.pdf to this directory so it can be included in the app:

# copy the file to the new folder
file.copy('../ifr-model/bayesian_ifr_model.pdf', './')

