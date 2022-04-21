setEcoParameters <- function () {
 param1 <<- 1
 leverEconomics <<- c(1,1,1,1,1,1,1,1,1,1,1,1)
 acumuladoDeImpactoEco <<- 0
 serieTempralFatiga <<- c()
}

getModificadorActividadLaboral <<- function(t, muertesTmenosUno) {
  acumuladoDeImpactoEco <<- acumuladoDeImpactoEco + 1
  fatiga = 0.5
  return(fatiga)
}