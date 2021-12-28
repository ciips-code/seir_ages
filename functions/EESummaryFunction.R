runScenario <- function (escenario,
                         country,
                         iso_country,
                         sensEE=F,
                         modifCostosSens=NULL,
                         modifEfectividadSens=NULL,
                         modifCostoEventosSens=NULL,
                         sensScenario=NULL
                         ) {
  
  if (sensEE == F) {
    modifCostosSens <- c(0,0)  
    modifEfectividadSens <- c(0,0) 
    modifCostoEventosSens <- c(0,0) 
    sensScenario <- if (is.null(sensScenario)) {1} 
  } else {
    sensScenario <- if (sensScenario=="low") {1} else {2}
  }
  
  
  
  duracion_inmunidad = 180
  fechaMetaVac = '2021-12-31'
  diasVacunacion = as.numeric(as.Date(fechaMetaVac) - as.Date('2021-01-01'))
  selectedPriority <- "Priority: older -> adults -> young"
  
  if (escenario=="NO_VAC") {
    selectedUptake <- 0
    escenario="BASE"
  } else if (escenario=="BASE") {
    selectedUptake <- EEParams2$uptake$valor[EEParams2$uptake$uptake==escenario]  
  } else if (escenario=="REAL_LIFE") {
    selectedUptake <- EEParams2$uptake$valor[EEParams2$uptake$uptake==iso_country]  
  } else if (escenario=="OPTIMISTA") {
    selectedUptake <- EEParams2$uptake$valor[EEParams2$uptake$uptake==escenario]  
  }
  
  cantidadVacunasTotal = selectedUptake * sum(N)
  ritmoVacunacion = cantidadVacunasTotal / diasVacunacion
  planVacunacionFinalParam <- generaEscenarioSage(selectedUptake, fechaMetaVac, selectedPriority,
                                                  planVacunacionFinal, N, tVacunasCero, as.Date(diaCeroVac))
  planVacunacionFinalParam <- lapply(planVacunacionFinalParam, function(dia) {colnames(dia) <- ageGroups 
  return(dia)})
  planVacunacionFinalParam <<- planVacunacionFinalParam 
  ajuste = (((.6*-1) + 1)/10)+0.3
  trans_prob_param <<- transmission_probability * ajuste
  relaxNpi = FALSE
  relaxGoal = NULL
  contact_matrix_scenario <<- get_custom_matrix(scenario = "Physical distancing + Shielding of older people + Self isolation",
                                                matrix_list = list(
                                                  contact_matrix = contact_matrix,
                                                  contact_matrix_work = contact_matrix_work,
                                                  contact_matrix_home = contact_matrix_home,
                                                  contact_matrix_school = contact_matrix_school,
                                                  contact_matrix_other = contact_matrix_other),
                                                ages= as.numeric(ageGroupsV))
  
  parametroDeBusqueda <- "BASE"
  if (escenario=="OPTIMISTA") {
    parametroDeBusqueda <- "OPTIMISTA"
  } else if (escenario=="REAL_LIFE") {
    parametroDeBusqueda <- iso_country
  }
  efficacy = applyVaccineEfficacy("B2. 100%, 80%, 50%")
  efficacy[[1]][3,] <- rep(EEParams2$eficaciaVacuna$caso[EEParams2$eficaciaVacuna$eficaciaVacuna==parametroDeBusqueda &
                                                           EEParams2$eficaciaVacuna$dosis==1], 
                           length(ageGroupsV)) * (1+modifEfectividadSens[sensScenario])
  efficacy[[1]][4,] <- rep(EEParams2$eficaciaVacuna$caso[EEParams2$eficaciaVacuna$eficaciaVacuna==parametroDeBusqueda &
                                                           EEParams2$eficaciaVacuna$dosis==2], 
                           length(ageGroupsV)) * (1+modifEfectividadSens[sensScenario])
  efficacy[[2]][3,] <- rep(EEParams2$eficaciaVacuna$casoGrave[EEParams2$eficaciaVacuna$eficaciaVacuna==parametroDeBusqueda &
                                                                EEParams2$eficaciaVacuna$dosis==1], 
                           length(ageGroupsV)) * (1+modifEfectividadSens[sensScenario])
  efficacy[[2]][4,] <- rep(EEParams2$eficaciaVacuna$casoGrave[EEParams2$eficaciaVacuna$eficaciaVacuna==parametroDeBusqueda &
                                                                EEParams2$eficaciaVacuna$dosis==2], 
                           length(ageGroupsV)) * (1+modifEfectividadSens[sensScenario])
  efficacy[[3]][3,] <- rep(EEParams2$eficaciaVacuna$casoCritico[EEParams2$eficaciaVacuna$eficaciaVacuna==parametroDeBusqueda &
                                                                  EEParams2$eficaciaVacuna$dosis==1], 
                           length(ageGroupsV)) * (1+modifEfectividadSens[sensScenario])
  efficacy[[3]][4,] <- rep(EEParams2$eficaciaVacuna$casoCritico[EEParams2$eficaciaVacuna$eficaciaVacuna==parametroDeBusqueda &
                                                                  EEParams2$eficaciaVacuna$dosis==2], 
                           length(ageGroupsV)) * (1+modifEfectividadSens[sensScenario])
  efficacy[[4]][3,] <- rep(EEParams2$eficaciaVacuna$muerte[EEParams2$eficaciaVacuna$eficaciaVacuna==parametroDeBusqueda &
                                                             EEParams2$eficaciaVacuna$dosis==1], 
                           length(ageGroupsV)) * (1+modifEfectividadSens[sensScenario])
  efficacy[[4]][4,] <- rep(EEParams2$eficaciaVacuna$muerte[EEParams2$eficaciaVacuna$eficaciaVacuna==parametroDeBusqueda &
                                                             EEParams2$eficaciaVacuna$dosis==2], 
                           length(ageGroupsV)) * (1+modifEfectividadSens[sensScenario])
  tVacunasCero = 303
  ifrProy = ifr_edit[1,]
  if (country == "Argentina") {
    ifrProy = ifrProy * 2.4
  } else if (country == "Peru") {
    ifrProy = ifrProy * 3.55
  } else if (country == "Colombia") {
    ifrProy = ifrProy * 1.8
  } else if (country == "Chile") {
    ifrProy = ifrProy * 1
  } else if (country == "Mexico") {
    ifrProy = ifrProy * 1.8
  } else if (country == "Brazil") {
    ifrProy = ifrProy * 1
  }
  
  ifr_base <<- ifrProy
  proyBaseEE <- seir_ages(dias=diasDeProyeccion,
                          duracionE = periodoPreinfPromedio,
                          duracionIi = duracionMediaInf,
                          porc_gr = porcentajeCasosGraves,
                          porc_cr = porcentajeCasosCriticos,
                          duracionIg = diasHospCasosGraves,
                          duracionIc = diasHospCasosCriticos,
                          ifr = ifrProy,
                          contact_matrix = contact_matrix_scenario,
                          relaxationThreshold = "1.1",
                          contact_matrix_relaxed = contact_matrix_relaxed,
                          transmission_probability = trans_prob_param,
                          N = N,
                          defunciones_reales=def_p,
                          modif_beta=efficacy$modif_beta,
                          modif_porc_gr=efficacy$modif_porc_gr,
                          modif_porc_cr=efficacy$modif_porc_cr,
                          modif_ifr=efficacy$modif_ifr,
                          planVacunacionFinal=planVacunacionFinalParam,
                          selectedPriority=c(8,7,6,5,4,3,2,1),
                          selectedUptake=selectedUptake,
                          ritmoVacunacion=ritmoVacunacion,
                          diasVacunacion=diasVacunacion,
                          immunityStates=immunityStates,
                          ageGroups=ageGroups,
                          paramVac=paramVac_edit,
                          duracion_inmunidad=duracion_inmunidad,
                          tVacunasCero=tVacunasCero,
                          relaxNpi=relaxNpi,
                          relaxGoal=relaxGoal,
                          relaxFactor=0,
                          country=country
  )
  
  fecha <- "2021-12-31"
  tInicio <- which(fechas_master == "2021-01-01")
  tFecha <- which(fechas_master == "2021-12-31")
  costoVacuna <- (1 + modifCostosSens[sensScenario]) * EEParams2$costoVacuna$valor[EEParams2$costoVacuna$costoVacuna==parametroDeBusqueda] 
  costoCasoAsint <- 0
  costoCasoSint <- (1 + modifCostoEventosSens[sensScenario]) * EEParams2$costoEvento$valor[EEParams2$costoEvento$costoEvento=="costoCasoSint" & 
                                                 EEParams2$costoEvento$pais==iso_country]
  costoCasoHosp <- (1 + modifCostoEventosSens[sensScenario]) * EEParams2$costoEvento$valor[EEParams2$costoEvento$costoEvento=="costoCasoHosp" & 
                                                 EEParams2$costoEvento$pais==iso_country]
  costoCasoUtiVent <- (1 + modifCostoEventosSens[sensScenario]) * EEParams2$costoEvento$valor[EEParams2$costoEvento$costoEvento=="costoCasoUtiVent" & 
                                                    EEParams2$costoEvento$pais==iso_country]
  costoCasoUtiNoVent <- (1 + modifCostoEventosSens[sensScenario]) * EEParams2$costoEvento$valor[EEParams2$costoEvento$costoEvento=="costoCasoUtiNoVent" & 
                                                      EEParams2$costoEvento$pais==iso_country]
  porcentajeAsint <- EEParams2$costoEvento$valor[EEParams2$costoEvento$costoEvento=="porcentajeAsint" & 
                                                   EEParams2$costoEvento$pais==iso_country]
  porcentajeUtiVent <- EEParams2$costoEvento$valor[EEParams2$costoEvento$costoEvento=="porcentajeUtiVent" & 
                                                     EEParams2$costoEvento$pais==iso_country]
  
  `AVACs perdidos (d)` <- sum(sapply(proyBaseEE[["ylqd: Years lost Qualy Disc"]][tInicio:tFecha],simplify = T,sum))
  `Casos totales` <- sum(sapply(proyBaseEE[["i: Daily infectious"]][tInicio:tFecha],simplify = T,sum))
  
  `Hospitalizaciones/día` <- sum(sapply(proyBaseEE[["Ig: Infectious (moderate)"]][tInicio:tFecha],simplify = T,sum)) # + sum(sapply(proyBaseEE[["Ic: Infectious (severe)"]][1:tFecha],simplify = T,sum))
  
  `Hospitalizaciones/día en UTI` <- sum(sapply(proyBaseEE[["Ic: Infectious (severe)"]][tInicio:tFecha],simplify = T,sum)) 
  `Muertes` <- sum(sapply(proyBaseEE[["d: Daily deaths"]][tInicio:tFecha],simplify = T,sum)) 
  `Años de vida perdidos (d)` <- sum(sapply(proyBaseEE[["yld: Years lost Disc"]][tInicio:tFecha],simplify = T,sum))
  `Años de vida perdidos` <- sum(sapply(proyBaseEE[["yl: Years lost"]][tInicio:tFecha],simplify = T,sum))
  `AVACs perdidos` <- sum(sapply(proyBaseEE[["ylq: Years lost Qualy"]][tInicio:tFecha],simplify = T,sum))
  `Vacunas aplicadas` <- sum(sapply(proyBaseEE[["vA: Daily vaccinations"]][tInicio:tFecha],simplify = T,sum))
  `Costos Vacunación` <- costoVacuna * sum(sapply(proyBaseEE[["vA: Daily vaccinations"]][tInicio:tFecha],simplify = T,sum))
  `  Casos asintomáticos/dia` <-porcentajeAsint * sum(sapply(proyBaseEE[["i: Daily infectious"]][tInicio:tFecha],simplify = T,sum))
  `  Costo casos asintomático` <- costoCasoAsint * `  Casos asintomáticos/dia`
  `  Casos sintomáticos` <- (1-porcentajeAsint) * sum(sapply(proyBaseEE[["i: Daily infectious"]][tInicio:tFecha],simplify = T,sum))
  `  Costo casos sintomáticos no hospitalizados (ambulatorio)` <- costoCasoSint * `  Casos sintomáticos`  
  `  Casos hospitalizados no UTI/dia` <- sum(sapply(proyBaseEE[["Ig: Infectious (moderate)"]][tInicio:tFecha],simplify = T,sum))
  `  Costo casos hospitalizados no UTI` <- costoCasoHosp * `  Casos hospitalizados no UTI/dia`
  `  Casos hospitalizados UTI/dia (sin respirador)` <- sum(sapply(proyBaseEE[["Ic: Infectious (severe)"]][tInicio:tFecha],simplify = T,sum)) * (1-porcentajeUtiVent)
  `  Costo casos hospitalizados UTI (sin respirador)` <- costoCasoUtiNoVent * `  Casos hospitalizados UTI/dia (sin respirador)`
  `  Casos hospitalizados UTI/dia (con respirador)` <- sum(sapply(proyBaseEE[["Ic: Infectious (severe)"]][tInicio:tFecha],simplify = T,sum)) * porcentajeUtiVent
  `  Costo casos hospitalizados no UTI (con respirador)` <- costoCasoUtiVent * `  Casos hospitalizados UTI/dia (con respirador)`
  `Costo de eventos COVID` <- 
    `  Costo casos asintomático` +
    `  Costo casos sintomáticos no hospitalizados (ambulatorio)` +
    `  Costo casos hospitalizados no UTI` +
    `  Costo casos hospitalizados UTI (sin respirador)` +
    `  Costo casos hospitalizados no UTI (con respirador)`
  `Costos totales` <- `Costo de eventos COVID` + `Costos Vacunación`
  
  
  metrics <- c("Costos totales",
               "AVACs perdidos (d)", 
               "Casos totales", 
               "Hospitalizaciones/día", 
               "Hospitalizaciones/día en UTI",
               "Muertes",
               "Años de vida perdidos (d)", 
               "Años de vida perdidos", 
               "AVACs perdidos", 
               "Vacunas aplicadas", 
               "Costos Vacunación", 
               "Costo de eventos COVID",
               #"  Casos asintomáticos/dia", 
               "  Costo casos asintomático", 
               #"  Casos sintomáticos no hospitalizados", 
               "  Costo casos sintomáticos no hospitalizados (ambulatorio)", 
               #"  Casos hospitalizados no UTI/dia", 
               "  Costo casos hospitalizados no UTI", 
               #"  Casos hospitalizados UTI/dia (sin respirador)", 
               "  Costo casos hospitalizados UTI (sin respirador)", 
               #"  Casos hospitalizados UTI/dia (con respirador)", 
               "  Costo casos hospitalizados no UTI (con respirador)")
  
  values <- c()
  for (i in 1:length(metrics)) {
    values <- c(values,eval(parse(text=paste0("round(","`",metrics,"`",", digits=0)")[i])))
  }                         
  
  EETable <- data.frame(`Metrics`= metrics,
                        Desenlaces = values)
  print(paste(escenario, "costo total", EETable[1,2]))
  # EEAvailable <<- T
  return(EETable)
  
  
}

