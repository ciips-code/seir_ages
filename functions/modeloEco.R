setEcoParameters <- function () {
 param1 <<- 1
 leverEconomics <<- c(1,1,1,1,1,1,1,1,1,1,1,1)
 acumuladoDeImpactoEco <<- 0
 serieTempralFatiga <<- c()
}

getModificadorActividadLaboral <<- function(t, muertesTmenosUno) {
  # acumuladoDeImpactoEco <<- acumuladoDeImpactoEco + 1
  # fatiga = 0.5
  # return(fatiga)
  workingHours$month <- str_pad(workingHours$month,2,"left",0)
  workingHoursValue <- workingHours$workingHours[workingHours$year==substring(fechas_master[t],1,4) &
                                                 workingHours$month==substring(fechas_master[t],6,7)]  
  return(workingHoursValue)
  
}


#getModificadorActividadLaboral(which(fechas_master=="2021-01-01"))

