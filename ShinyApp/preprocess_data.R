
#
# This file will preprocess the IFR / Hospitalized fits based on the model and other inputs, then output the csv used in the interactive app.

require(dplyr)




# Note: Both provide a fit for each of IFR and Hospitalization by age for all infections (including asymptomatics.) 


# This will be the code to preprocess inputs.

# App currently uses the mean and the 95th percentile. (i.e. One-directional CI), not the 95% CI. All are caluclated. 
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

# rr_Quantiles_20s_Only = data_frame(p = quants, q = quantile(ifr_model_out$rr_healthy, probs = quants))
Base_Quantiles = data_frame(p = quants, rr_25 = quantile(ifr_model_out$rr_healthy_25, probs = quants), rr_30 = quantile(ifr_model_out$rr_healthy_30, probs = quants),  rr_35 = quantile(ifr_model_out$rr_healthy_35, probs = quants), theta_min = quantile(ifr_model_out$theta_min, probs = quants), final_ifr_25 = quantile(ifr_model_out$final_ifr_25, probs = quants), final_ifr_30 = quantile(ifr_model_out$final_ifr_30, probs = quants), final_ifr_35 = quantile(ifr_model_out$final_ifr_35, probs = quants), hyper_ifr_25 = quantile(ifr_model_out$hyperifr_25, probs = quants), hyper_ifr_30 = quantile(ifr_model_out$hyperifr_30, probs = quants), hyper_ifr_35 = quantile(ifr_model_out$hyperifr_35, probs = quants))

Base_Quantiles = rbind(cbind(Base_Quantiles,ages="20 to 29"),cbind(Base_Quantiles,ages="30 to 39"))
Base_Quantiles['final_ifr'] = Base_Quantiles['final_ifr_25']
Base_Quantiles['not_healthy_rr'] = Base_Quantiles['rr_25']
Base_Quantiles['hypermean_ifr'] = Base_Quantiles['hyper_ifr_25']
Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'final_ifr'] = Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'final_ifr_35']
Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'not_healthy_rr'] = Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'rr_35']
Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'hypermean_ifr'] = Base_Quantiles[which(Base_Quantiles['ages']=="30 to 39"),'hyper_ifr_35']
Base_Quantiles['final_ifr_25'] = NULL
Base_Quantiles['final_ifr_30'] = NULL
Base_Quantiles['final_ifr_35'] = NULL
Base_Quantiles['hyper_ifr_25'] = NULL
Base_Quantiles['hyper_ifr_30'] = NULL
Base_Quantiles['hyper_ifr_35'] = NULL
Base_Quantiles['rr_25'] = NULL
Base_Quantiles['rr_30'] = NULL
Base_Quantiles['rr_35'] = NULL


# Add in estimates for metaanalysis hyper_ifr versus best-case.
Split_Quantiles_metaanalysis = rbind(cbind(Base_Quantiles, case="meta"), cbind(Base_Quantiles, case="best"))
Split_Quantiles_metaanalysis[which(Split_Quantiles_metaanalysis['case']=="meta"),'final_ifr']=Split_Quantiles_metaanalysis[which(Split_Quantiles_metaanalysis['case']=="meta"),'hypermean_ifr']
Split_Quantiles_metaanalysis['hypermean_ifr'] = NULL

# Add in estimates for healthy / regular.
Split_Quantiles = rbind(cbind(Split_Quantiles_metaanalysis, status="healthy"), cbind(Split_Quantiles_metaanalysis, status="all"))
Split_Quantiles[which(Split_Quantiles['status']=="all"),'final_ifr']=Split_Quantiles[which(Split_Quantiles['status']=="all"),'final_ifr'] * Split_Quantiles[which(Split_Quantiles['status']=="all"),'not_healthy_rr']
Split_Quantiles['not_healthy_rr'] = NULL

# Now we process the other data sets used to get hospitalization and gender:

# For gender and age ratios, and by healthy/not:
ifr_model_alldata_out <- read.csv("../ifr-model/output/bayesian-posteriors-bygender-alldata.csv", row.names=1, check.names=FALSE)

# Now, construct the dataset needed.
ifr_model_alldata_out$ages <- paste(ifr_model_alldata_out$Age_min, "to",ifr_model_alldata_out$Age_max)
# 
ifr_model_alldata_out$Age_min <- NULL
ifr_model_alldata_out$Age_max <- NULL
# Before reshaping, we need to get the relevant percentiles.

library(tidyr)
Gathered <- gather(ifr_model_alldata_out,key = "subgroup",value = "Val",c("value_como_F", "value_healthy_F", "value_como_M", "value_healthy_M","n_como_F", "n_healthy_F", "n_como_M", "n_healthy_M", "theta_all_B", "theta_all_M", "theta_all_F", "theta_healthy_B", "theta_healthy_M", "theta_healthy_F", "rr_all_B", "rr_all_M", "rr_all_F"))

#out_test = by(Gathered[,'Val'],Gathered[,'subg_by_age'], quantile, probs = quants)
#out_test2 = tapply(Gathered[,'Val'], Gathered[,'subg_by_age'], quantile, probs = quants)


require(tidyverse)


# Recover groups from "subgroups":

Gathered['Gender']="M"

Gathered[c(which(Gathered['subgroup']=="value_como_F"), which(Gathered['subgroup']=="value_healthy_F"),which(Gathered['subgroup']=="n_como_F"),which(Gathered['subgroup']=="n_healthy_F"), which(Gathered['subgroup']=="theta_all_F"),which(Gathered['subgroup']=="theta_healthy_F"), which(Gathered['subgroup']=="rr_all_F")),'Gender']="F"

Gathered[c(which(Gathered['subgroup']=="theta_all_B"),which(Gathered['subgroup']=="theta_healthy_B"),which(Gathered['subgroup']=="rr_all_B")), 'Gender']="B"

Gathered['DataType']="prob"

Gathered[c(which(Gathered['subgroup']=="theta_all_B"),which(Gathered['subgroup']=="theta_healthy_B"),which(Gathered['subgroup']=="theta_all_M"),which(Gathered['subgroup']=="theta_healthy_M"),which(Gathered['subgroup']=="theta_all_F"),which(Gathered['subgroup']=="theta_healthy_F")),'DataType']="Theta"


Gathered[c(which(Gathered['subgroup']=="rr_all_B"), which(Gathered['subgroup']=="rr_all_M"), which(Gathered['subgroup']=="rr_all_F")),'DataType']="relrisk"

Gathered[c(which(Gathered['subgroup']=="n_como_F"), which(Gathered['subgroup']=="n_healthy_F"), which(Gathered['subgroup']=="n_como_M"), which(Gathered['subgroup']=="n_healthy_M")),'DataType']='count'

# Gathered$subg_by_age <- paste0(Gathered$ages,"_",Gathered$subgroup)


# We use the France study as our basis for estimating relative risk of hospitalization from deaths. (Salje et al,  science.sciencemag.org/cgi/content/full/science.abc3517)
# Load data from files.

Male_probDeath_France <- cbind(gend = "m", outc="death", read.csv("France_Male_p_death_by_age_range.csv", row.names=1, check.names=FALSE))
Male_probDeath_France$ages <- rownames(Male_probDeath)
Female_probDeath_France <- cbind(gend = "f", outc="death", read.csv("France_Female_p_death_by_age_range.csv", row.names=1, check.names=FALSE))
Female_probDeath_France$ages <- rownames(Female_probDeath)
Male_probHosp_France <- cbind(gend = "m", outc="hosp", read.csv("France_Male_p_hospitalization_by_age_range.csv", row.names=1, check.names=FALSE))
Male_probHosp_France$ages <- rownames(Male_probHosp)
Female_probHosp_France <- cbind(gend = "f", outc="hosp", read.csv("France_Female_p_hospitalization_by_age_range.csv", row.names=1, check.names=FALSE))
Female_probHosp_France$ages <- rownames(Female_probHosp)
#Male_probICU_France <- cbind(gend = "m", outc="ICU", read.csv("France_Male_p_death_by_age_range.csv", row.names=1, check.names=FALSE))
#Male_probICU_France$ages <- rownames(Male_probICU)
#Female_probICU_France <- cbind(gend = "f", outc="ICU", read.csv("France_Female_p_death_by_age_range.csv", row.names=1, check.names=FALSE))
#Female_probICU_France$ages <- rownames(Female_probICU)

participants <- 10:100

problist = paste0(formatC(c(0:100)),"%")
agelist = c("0 to 19", "20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", "70 to 79", "80 to 100")

all_probs_France <- 
  rbind(
    reshape(Male_probDeath_France, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    reshape(Female_probDeath_France, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    reshape(Male_probHosp_France, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    reshape(Female_probHosp_France, direction='long', v.name="value", varying=problist, times=problist, idvar="ages")#,
    #reshape(Male_probICU_France, direction='long', v.name="value", varying=problist, times=problist, idvar="ages"),
    #reshape(Female_probICU_France, direction='long', v.name="value", varying=problist, times=problist, idvar="ages")
  )


names(all_probs_France)[names(all_probs_France)=='time'] <- "probability"
row.names(all_probs) <- NULL
#all_probs_France['ages']=agelist[unlist(all_probs_France['ages'])]
rr_death_hosp = reshape(all_probs_France[which(all_probs_France['probability']=='50%'),], direction='wide', v.name='value', timevar='outc', idvar=c('gend','ages'))

names(rr_death_hosp)[names(rr_death_hosp) == 'gend'] <- 'Gender'
rr_death_hosp[,'Gender']=toupper(rr_death_hosp[,'Gender'])
rr_death_hosp['Probability']=NULL

rr_death_hosp['rr'] <- rr_death_hosp['value.hosp']/rr_death_hosp['value.death']

rr_death_hosp_both <- rr_death_hosp[which(rr_death_hosp['Gender']=='M'),]
rr_death_hosp_both['Gender']='B'
rr_death_hosp_both['rr_M'] = rr_death_hosp_both['rr']
rr_death_hosp_both['rr_F'] = rr_death_hosp[which(rr_death_hosp['Gender']=='F'),'rr']
rr_death_hosp_both['rr'] = (rr_death_hosp_both['rr_M'] + rr_death_hosp_both['rr_F'])/2
rr_death_hosp_both['rr_M'] = NULL
rr_death_hosp_both['rr_F'] = NULL

rr_death_hosp = rbind(rr_death_hosp,rr_death_hosp_both)


Gathered_for_age_gender_rr = Gathered[which(Gathered['DataType']=='Theta'),] #Theta is the best-case IFR 
Gathered_for_age_gender_rr['subgroup'] <- NULL 
Gathered_for_age_gender_rr['subg_by_age'] <- NULL

Quantiles_for_age_gender_rr = Gathered_for_age_gender_rr %>% group_by(Gender,ages,DataType) %>% 
  do(data_frame(p = quants, q = quantile(.$Val, probs = quants))) %>%
  pivot_wider(names_from = c(Gender), values_from = c(q))

Quantiles_for_age_gender_rr['rr_M'] = Quantiles_for_age_gender_rr['M'] / Quantiles_for_age_gender_rr['B']
Quantiles_for_age_gender_rr['rr_F'] = Quantiles_for_age_gender_rr['F'] / Quantiles_for_age_gender_rr['B']

# Now, we need the dataset with:
# IFRs and Hosp per age / gender.


# Back to the Base Quantiles dataset, to use as the base for making the dataset that gets modified using the other data.

# Nongender_Dataset = merge(Base_Quantiles, Quantiles_for_age_gender_rr, by=NULL, all.y=FALSE)
Nongender_Dataset = inner_join(Split_Quantiles, Quantiles_for_age_gender_rr, by=c('p','ages'))


Gender_dataset = rbind(cbind(Nongender_Dataset,Gender='F'),cbind(Nongender_Dataset,Gender='M'),cbind(Nongender_Dataset,Gender='B'))

Gender_dataset['B'] = NULL
Gender_dataset['F'] = NULL
Gender_dataset['M'] = NULL
#Not useing these age relative risks, since we have estimates already
Gender_dataset['rr_25'] = NULL
Gender_dataset['rr_30'] = NULL
Gender_dataset['rr_35'] = NULL



Gender_dataset[which(Gender_dataset['Gender']=='M'),'final_ifr']=Gender_dataset[which(Gender_dataset['Gender']=='M'),'final_ifr'] * Gender_dataset[which(Gender_dataset['Gender']=='M'),'rr_M']

Gender_dataset[which(Gender_dataset['Gender']=='F'),'final_ifr']=Gender_dataset[which(Gender_dataset['Gender']=='F'),'final_ifr'] * Gender_dataset[which(Gender_dataset['Gender']=='F'),'rr_F']

Gender_dataset['DataType'] = NULL


Hosp_Dataset = inner_join(Gender_dataset, rr_death_hosp[,c("ages","Gender", "rr")], by = c("ages","Gender"), all = FALSE)
names(Hosp_Dataset)[names(Hosp_Dataset) == 'rr'] <- 'rr_hosp'
Hosp_Dataset["final_ifr"] =  Hosp_Dataset["final_ifr"] *  Hosp_Dataset['rr_hosp']
names(Hosp_Dataset)[names(Hosp_Dataset) == "final_ifr"] <- 'value'
Hosp_Dataset['rr_hosp'] <-NULL

names(Gender_dataset)[names(Gender_dataset) == "final_ifr"] <- 'value'

Final_Dataset = rbind(cbind(Gender_dataset,outc='death'),cbind(Hosp_Dataset,outc='hosp'))

Final_Dataset['rr_M'] = NULL
Final_Dataset['rr_F'] = NULL
Final_Dataset['ages.1'] = NULL
Final_Dataset['theta_min'] = NULL

Final_Dataset[,'gend']=tolower(Final_Dataset[,'Gender'])
Final_Dataset['Gender'] = NULL
names(Final_Dataset)[names(Final_Dataset) == "p"] <- 'probability'




write.csv(Final_Dataset, file = "app_dataset.csv")
# Unsure what this is about when I run write.csv
# Warning message:
# Unknown or uninitialised column: `rr_all_B`.

# Copy most recent bayesian_ifr_model.pdf to this directory so it can be included in the app:

# copy the file to the new folder
file.copy('../ifr-model/bayesian_ifr_model.pdf', './')

