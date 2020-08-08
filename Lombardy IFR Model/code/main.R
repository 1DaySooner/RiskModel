##################################################
## Set workind directory and load packages
##################################################
# Set the working directory to the local folder to which the git was downloaded
setwd("/Github/covid_IFR_Lombardy/")
rm(list = ls())
required_packages <- c("OECD", "rjags", "ggplot2", "R2OpenBUGS", "coda", "data.table", 
                       "MCMCvis", "viridis", "ggsci", "RColorBrewer", "stargazer", "countrycode", 
                       "animation", "latex2exp", "readstata13", "jtools", "MCMCglmm", "Smisc")
not_installed <- required_packages[!(required_packages %in% installed.packages()[ , "Package"])]    # Determine missing packages
if(length(not_installed)) install.packages(not_installed)                                           # Install missing packages

suppressWarnings(lapply(required_packages, require, character.only = TRUE))

# Run Bayesian estimations
source("code/bayesianIFRestimate.R")

#########################
## Make Tables
#########################
IFRPlot <- rbindlist(lapply(postTown, function(x) as.data.table(x[,8:14])))
ageRanges <- unique(dataLikelihoodTown$ageRange)
names(IFRPlot) <- ageRanges
IFRPlot <- melt(IFRPlot)
IFRPlot[, value := value*100] # Change to percentage

IFRTable <- as.data.table(MCMCsummary(postTown, params = c("deltaCovid"), digits=4, HPD = T,func = function(x) posterior.mode(x)))
IFRTable[, mode := func] 
IFRTable <- IFRTable*100 # Change to percentage
IFRTable <- IFRTable[, c("mode", "mean", "95%_HPDL",  "95%_HPDU")]
IFRTable$ageRange <- ageRanges

#############################################################
# Overall IFR from full model for various age groups
#############################################################
ageRangeShare <- unique(demographicData[, c("ageRange", "ageRangeShare")]) 
overallIFR <- merge(ageRangeShare, IFRTable, by ="ageRange")
overallIFR <- overallIFR[, c(sum(ageRangeShare*`mode`), sum(ageRangeShare*`mean`), sum(ageRangeShare*`95%_HPDL`), sum(ageRangeShare*`95%_HPDU`))]
sprintf("%.2f", overallIFR)

under60IFR <- merge(ageRangeShare, IFRTable, by ="ageRange")
under60IFR <- under60IFR[ageRange %in% c("0-20", "21-40", "41-50", "51-60"), c(sum(ageRangeShare*`mode`), sum(ageRangeShare*`mean`), sum(ageRangeShare*`95%_HPDL`), sum(ageRangeShare*`95%_HPDU`))/sum(ageRangeShare)]
sprintf("%.2f", under60IFR)

over60IFR <- merge(ageRangeShare, IFRTable, by ="ageRange")
over60IFR <- over60IFR[! ageRange %in% c("0-20", "21-40", "41-50", "51-60"), c(sum(ageRangeShare*`mode`), sum(ageRangeShare*`mean`), sum(ageRangeShare*`95%_HPDL`), sum(ageRangeShare*`95%_HPDU`))/sum(ageRangeShare)]
sprintf("%.2f", over60IFR)

##################################
# Table of demographics and deaths
##################################
table1Data <- dataLikelihoodTown[, c(-1)] # Remove town name
table1Data <-  table1Data[,lapply(.SD, sum, na.rm=TRUE), by= ageRange]
overallDemsDeaths <- table1Data[, c(-1)]
overallDemsDeaths <- overallDemsDeaths[,lapply(.SD, sum, na.rm=TRUE), ]
overallDemsDeaths[, ageRange := "Overall"]
table1Data <- rbind(table1Data, overallDemsDeaths)
stargazer(table1Data, summary = FALSE, rownames = FALSE)
write.csv(table1Data, "output/table1Data.csv")

##################################
# Table of model estimates
##################################
sprintf("%.4f", overallIFR)
sprintf("%.4f", under60IFR)
sprintf("%.4f", over60IFR)
table2Data <- MCMCsummary(postTown, params = c('delta','theta_i', "deltaCovid"), digits=4, HPD = T,func = function(x) posterior.mode(x))
table2Data <- table2Data*100
table2Data <- table2Data[, c(7,1,3,4)]
table2Data <- rbind(overallIFR, under60IFR, over60IFR, table2Data)
table2Data <- round(table2Data,4)
names(table2Data)[1] <- "mode"
stargazer(table2Data, summary = FALSE, digits = 4)
write.csv(table2Data, "output/table2Data.csv")

#########################
## Make figures 
#########################
baseSize <- 20

###############################################################
# Deaths by day in 2015-2019 and 2020
###############################################################
ggplot() + 
  geom_line(data = plotDeaths, aes(x = date, y = meanDailyDeathsBeforeAll, colour = "2015-19 average deaths") , linetype = "dashed") + 
  geom_line(data = plotDeaths, aes(x = date, y = deaths2020All, colour = "2020 deaths")) + 
  scale_color_manual(values = c(
    '2015-19 average deaths' = 'blue',
    '2020 deaths' = 'black')) +
  theme_bw(base_size = baseSize) + 
  xlab("") + 
  ylab("") + 
  geom_hline(yintercept = 0) +  
  theme(legend.position = c(0.2, 0.8)) +
  theme(legend.margin = margin(t = 0, unit='cm')) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = as.Date("2020-02-20"), color = "red") +
  theme(panel.grid.minor = element_blank())

ggsave(filename = "output/deathsByDate.pdf")
ggsave(filename = "../../Users/grinaldi/Dropbox/Apps/Overleaf/covid19 IFR/figures/deathsByDate.pdf")

###############################################################
# Bayesian estimates of infection fatality rates
###############################################################
ggplot(IFRPlot, aes(variable, value)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_sqrt(breaks = c(0,.01, .1, .2, .5, 1, 2, 5, 10), limits = c(0, 15)) +
  theme_bw(base_size = baseSize) +
  xlab("Age Range") + 
  ylab("Infection Fatality Rate (%)") +
  geom_hline(yintercept = 0) +
  theme(panel.grid.minor = element_blank())

ggsave(filename = "output/IFRbyAge.pdf")
ggsave(filename = "../../Users/grinaldi/Dropbox/Apps/Overleaf/covid19 IFR/figures/IFRbyAge.pdf")

###############################################################
# Fatality rates for different assumptions on infection rates
###############################################################
# Construct overall ifr 
overallIFR <- merge(graphDataAll, ageRangeShare, by = "ageRange")
overallIFR <- overallIFR[, list(sum(`2.5%` * ageRangeShare), sum(`50%` * ageRangeShare), sum(`97.5%` * ageRangeShare)), by = prop]
overallIFR[, ageRange := "Overall", ]
names(overallIFR)[2:4] <- c("2.5%", "50%", "97.5%")
overallIFR <- overallIFR[, c("2.5%", "50%", "97.5%", "ageRange", "prop")]
graphDataAll <- rbind(graphDataAll, overallIFR)
graphDataAll[, `2.5%` := 100 * `2.5%`]  # change to percentage
graphDataAll[, `50%` := 100 * `50%`] 
graphDataAll[, `97.5%` := 100 * `97.5%`] 
graphDataAll[, prop := 100 * prop] 
# Custom palette
palCustom <- c( "#4575B4", "#91BFDB", "greenyellow", "khaki", "orange", rev(brewer.pal(n = 7, name = "RdYlBu"))[6:7] ,  "#000000")

ggplot(graphDataAll[prop > 10, ], aes(x = prop)) +
  geom_line(aes(y = `2.5%`, color = ageRange), linetype = 2) +
  geom_line(aes(y = `50%`, color = ageRange), size = 1) +
  geom_line(aes(y = `97.5%`, color = ageRange), linetype = 2) +
  scale_y_sqrt(breaks = c(0,.01, .1, .2, .5, 1, 2, 5, 10, 20), limits = c(0, 30)) +
  theme_bw(base_size = baseSize) +
  xlab("Proportion Infected (%)") + 
  ylab("Infection Fatality Rate (%)") +
  geom_hline(yintercept = 0) +
  theme(panel.grid.minor = element_blank()) + 
  geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`, fill = ageRange), alpha= 0.35)  +
  scale_fill_manual(values = palCustom, name = "Age Range") + 
  scale_color_manual(values = palCustom, name = "Age Range") 
  
ggsave(filename = "output/IFRbyProp.pdf")
ggsave(filename = "../../Users/grinaldi/Dropbox/Apps/Overleaf/covid19 IFR/figures/IFRbyProp.pdf")

###############################################################
# Extrapolation of age specific IFR to get overall by country
###############################################################
source("code/oecdComparison.R")
ggplot(data=popByAgeRange, aes(x= reorder(LOCATION, - overallIFRest), y = overallIFRest)) +
  geom_bar(stat="identity", color = "black", fill = "red") + 
  geom_errorbar(aes(ymin=overallIFRLower, ymax=overallIFRUpper), width=.3,
                position=position_dodge(.9))+ 
  theme_bw(base_size = baseSize) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("Estimated overall IFR") 

ggsave(filename = "output/IFRbyCountry.pdf", width = 15, height = 7)
ggsave(filename = "../../Users/grinaldi/Dropbox/Apps/Overleaf/covid19 IFR/figures/IFRbyCountry.pdf", width = 15, height = 7)
write.csv(popByAgeRange, "output/crossCountryIFREstimates.csv")

##############################################################
# IFR Across countries and Case fatality rates
##############################################################
dataWorld <- fread("/Github/covid_IFR_Lombardy/data/worldometerJune3.csv")
dataWorld[, LOCATION := countrycode(Country, origin = 'country.name', destination = 'iso3c')]
dataWorld <- merge(dataWorld, popByAgeRange, by = "LOCATION")
dataWorld[, cfr := Deaths/Cases]
dataWorld[, overallIFRest := overallIFRest/100]
baseSize <- 20

ggplot(dataWorld[Tests_mln > 20000,], aes(overallIFRest, cfr, label = LOCATION)) + geom_point() + 
  geom_smooth(method =  "lm",data =  dataWorld[Tests_mln > 20000,], color = "gray", formula = 'y ~ x',se = FALSE) + 
  geom_text(color= "Black", nudge_y = .005) + 
  theme_bw(base_size = baseSize) +
  xlab("Estimated IFR") + 
  ylab("Reported CFR")
summ(lm(cfr ~ overallIFRest, dataWorld[Tests_mln > 20000,]),robust =  T)
ggsave(filename = "output/ifrCfr.pdf")
ggsave(filename = "../../Users/grinaldi/Dropbox/Apps/Overleaf/covid19 IFR/figures/ifrCfr.pdf")

##############################################################
# Overall IFR assuming everyone got it and infection rate
##############################################################
sprintf("%.2f", graphDataAll[ageRange == "Overall" & prop == 100, ])

graphDataAllUnder60 <- merge(ageRangeShare, graphDataAll[ageRange %in% c("0-20", "21-40", "41-50", "51-60"),])
graphDataAllUnder60[prop == 10, c(sum(ageRangeShare*`50%`), sum(ageRangeShare*`2.5%`), sum(ageRangeShare*`97.5%`))/sum(ageRangeShare)]

# Overall infection rate of the area
infectionByTown <- cbind(demographicData[, sum(tot2019), by = Denominazione], MCMCsum[8:16,])
names(infectionByTown)[2] <- c("population")
overallInfection <- infectionByTown[, list(sum(population*func)/sum(population), sum(population*mean)/sum(population),  sum(population*`95%_HPDL`)/sum(population), sum(population*`95%_HPDU`)/sum(population))]

###############################################################
# Appendix plot of trace and posterior densities
###############################################################
# diagnostic evaluation of posterior samples
priorDelta <- seq(0,0.1, length.out = 5000)
MCMCtrace(postTown,
          iter = 500,
          params = c("delta"),
          priors = priorDelta,
          main_den = c(TeX("Density $\\delta_{0-20}$"),
                       TeX("Density $\\delta_{21-40}$"),
                       TeX("Density $\\delta_{41-50}$"),
                       TeX("Density $\\delta_{51-60}$"),
                       TeX("Density $\\delta_{61-70}$"),
                       TeX("Density $\\delta_{71-80}$"),
                       TeX("Density $\\delta_{81+}$")),
          main_tr =  c(TeX("Trace $\\delta_{0-20}$"),
                       TeX("Trace $\\delta_{21-40}$"),
                       TeX("Trace $\\delta_{41-50}$"),
                       TeX("Trace $\\delta_{51-60}$"),
                       TeX("Trace $\\delta_{61-70}$"),
                       TeX("Trace $\\delta_{71-80}$"),
                       TeX("Trace $\\delta_{81+}$")),
          filename = "output/MCMCdelta.pdf"
          )

priorDeltaCovid <- seq(0,0.3, length.out = 5000)
MCMCtrace(postTown,
          iter = 500,
          params = c("deltaCovid"),
          priors = priorDeltaCovid,
          main_den = c(TeX("Density $\\delta^{Covid}_{0-20}$"),
                       TeX("Density $\\delta^{Covid}_{21-40}$"),
                       TeX("Density $\\delta^{Covid}_{41-50}$"),
                       TeX("Density $\\delta^{Covid}_{51-60}$"),
                       TeX("Density $\\delta^{Covid}_{61-70}$"),
                       TeX("Density $\\delta^{Covid}_{71-80}$"),
                       TeX("Density $\\theta^{Covid}_{81+}$")),
          main_tr =  c(TeX("Trace $\\delta^{Covid}_{0-20}$"),
                       TeX("Trace $\\delta^{Covid}_{21-40}$"),
                       TeX("Trace $\\delta^{Covid}_{41-50}$"),
                       TeX("Trace $\\delta^{Covid}_{51-60}$"),
                       TeX("Trace $\\delta^{Covid}_{61-70}$"),
                       TeX("Trace $\\delta^{Covid}_{71-80}$"),
                       TeX("Trace $\\theta^{Covid}_{81+}$")),
          filename = "output/MCMCdeltaCovid.pdf")

priorTheta <- qbeta(seq(0,1, length.out = 5000), 3, 2, ncp = 0, lower.tail = TRUE, log.p = FALSE)

MCMCtrace(postTown,
          iter = 500,
          priors = priorTheta,
          params = c("theta_i"),
          main_den = c(TeX("Density $\\theta_{Casalpusterlengo}$"),
                       TeX("Density $\\theta_{Castelgerundo}$"),
                       TeX("Density $\\delta_{Castiglione d'Adda}$"),
                       TeX("Density $\\delta_{Codogno}$"),
                       TeX("Density $\\delta_{Fombio}$"),
                       TeX("Density $\\delta_{Maleo}$"),
                       TeX("Density $\\delta_{San Fiorano}$"),
                       TeX("Density $\\delta_{Somaglia}$"),
                       TeX("Density $\\delta_{Terranova dei Passerini}$")),
          main_tr =  c(TeX("Trace $\\theta_{Casalpusterlengo}$"),
                       TeX("Density $\\theta_{Castelgerundo}$"),
                       TeX("Trace $\\delta_{Castiglione d'Adda}$"),
                       TeX("Trace $\\delta_{Codogno}$"),
                       TeX("Trace $\\delta_{Fombio}$"),
                       TeX("Trace $\\delta_{Maleo}$"),
                       TeX("Trace $\\delta_{San Fiorano}$"),
                       TeX("Trace $\\delta_{Somaglia}$"),
                       TeX("Trace $\\delta_{Terranova dei Passerini}$")),
          filename = "output/MCMCtheta.pdf")

system2(command = "pdfcrop", 
        args    = c("output/MCMCdelta.pdf", 
                    "output/MCMCdelta.pdf")) 

system2(command = "pdfcrop", 
        args    = c("output/MCMCdeltaCovid.pdf", 
                    "output/MCMCdeltaCovid.pdf")) 

system2(command = "pdfcrop", 
        args    = c("output/MCMCtheta.pdf", 
                    "output/MCMCtheta.pdf")) 



