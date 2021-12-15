dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
dataOWD <- dataOWD[dataOWD$iso_code=="ARG", c("date","new_deaths","new_cases")]


distAge <- c(0.001887598,
             0.007639806,
             0.020992082,
             0.057571754,
             0.125386213,
             0.224485133,
             0.266509036,
             0.295528379)

ageGroups <<- c("0-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")

dataOWD$new_deaths <- as.numeric(dataOWD$new_deaths)
dataOWD$new_cases <- as.numeric(dataOWD$new_cases)
dataOWD[is.na(dataOWD)] <- 0

for (i in 1:length(ageGroups)) {
  dataOWD[[paste0("`",ageGroups[i],"_deaths`")]] <- dataOWD$new_deaths * distAge[i]
  dataOWD[[paste0("`",ageGroups[i],"_cases`")]] <- dataOWD$new_cases * distAge[i]
  
}


#writexl::write_xlsx(dataOWD, "C:/Users/Adrian/Desktop/prueba.xlsx")



