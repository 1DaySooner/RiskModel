## Estimate IFR for other nations using OECD Data

# Uncomment this to get the source data form OECD (slow)
#oecdDem <- get_dataset("HISTPOP", filter = NULL, start_time = NULL,
#            end_time = NULL, pre_formatted = FALSE)
#oecdDemCln <- as.data.table(oecdDem)
#oecdDemCln <- oecdDemCln[obsTime == 2018, ]
#oecdDemCln <- oecdDemCln[SEX == "T", ]
#write.csv(oecdDemCln,"data/oecdDemCln.csv")

oecdDemCln <- fread("data/oecdDemCln.csv")
# One year difference compared to my data unfortunately (20-24 + 25-29 + 30-34 + 35-39 instead of 21-40 in ISTAT data)
oecdDemCln[AGE %in% c("0_4", "05_9", "10_14", "15_19"), ageRange := "0-20"]
oecdDemCln[AGE %in% c("20_24", "25_29", "30_34", "35_39"), ageRange := "21-40"]
oecdDemCln[AGE %in% c("40_44", "45_49"), ageRange := "41-50"]
oecdDemCln[AGE %in% c("50_54", "55_59"), ageRange := "51-60"]
oecdDemCln[AGE %in% c("60_64", "65_69"), ageRange := "61-70"]
oecdDemCln[AGE %in% c("70_74", "75_79"), ageRange := "71-80"]
oecdDemCln[AGE %in% c("80_84", "85_OVER"), ageRange := "81+"]

popByAgeRange <- oecdDemCln[! is.na(ageRange), sum(obsValue), by = c("LOCATION", "ageRange")]
popByAgeRange <- merge(popByAgeRange, oecdDemCln[AGE %in% c("TOTAL"), c("LOCATION", "obsValue")], by = "LOCATION")
popByAgeRange[, share := V1/obsValue]

# Add IFR estimates
ifrEstimates <- table2Data[20:26, c(1,3,4)]
ifrEstimates <- cbind(ifrEstimates, c("0-20", "21-40", "41-50", "51-60", "61-70", "71-80", "81+"))
names(ifrEstimates) <- c("mode","lower","upper","ageRange")

popByAgeRange <- merge(popByAgeRange, ifrEstimates, by = "ageRange")
popByAgeRange[, overallIFRest := sum(mode*share), by = LOCATION]
popByAgeRange[, overallIFRLower := sum(lower*share), by = LOCATION]
popByAgeRange[, overallIFRUpper := sum(upper*share), by = LOCATION]

popByAgeRange <- unique(popByAgeRange[, c("LOCATION", "overallIFRest", "overallIFRLower", "overallIFRUpper")])


