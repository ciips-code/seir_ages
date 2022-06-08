ejecutarProyeccionConParametrosUI = function(input, output, session) {
  withProgress(
    message = 'Cargando...', value = 0, {
      print("calibra")
      calibra(input,output,session)
      incProgress(0.1)
      print("actualiza mapa")
      actualizaMapa(input,output,session)
      incProgress(0.1)
      print("actualiza panel")
      actualizaPanel(input,output,session)
      incProgress(0.1)
      incProgress(0.1)
      print("actualiza variables")
      actualizaVariables(input,output,session)
      incProgress(0.1)
      print("actualiza cm")
      actualizaCM(input,output,session)
      incProgress(0.1)
      print("actualiza dttables")
      actualizaDTTables(input,output,session)
      incProgress(0.1)
      print("actualiza parametros")
      actualizaParametros(input,output,session)
      incProgress(0.1)
      print("actualiza proy")
      actualizaProy(input,output,session)
      incProgress(0.1)
      print("actualiza plot")
      actualizaPlot(input,output,session)
      incProgress(0.1)
      print("actualiza tablas")
      actualizaTablas(input,output,session)
    }) 
  
}

calibra <- function (input, output, session) {
  
  ####################################
  # Calibración
  ####################################
  
  if (input$TSP=="Calibracion") {
    porcentajeCasosCriticosCalibrador <<- input$`input-porcentajeCasosCriticosCalibrador`
    porcentajeCasosGravesCalibrador <<- input$`input-porcentajeCasosGravesCalibrador`
    ifrCalibrador <<- input$`input-ifrCalibrador`
    transmission_probabilityCalibrador <<- input$`input-transmission_probabilityCalibrador`
    fechaTransicionOmicron <<- input$`input-fechaTransicionOmicron`
    periodoTransicionOmicron <<- input$`input-periodoTransicionOmicron`
  }
  
  porcentajeCasosGraves <<- porcentajeCasosGraves_base * porcentajeCasosGravesCalibrador
  porcentajeCasosCriticos <<- porcentajeCasosCriticos_base * porcentajeCasosCriticosCalibrador
  ifr <<- ifr_base * ifrCalibrador
  transmission_probability <<- transmission_probability_base * transmission_probabilityCalibrador # 0.68
  
}

actualizaMapa <- function(input, output, session) {
  # Set params
  iso_country <<- if (input$country=="Argentina") {"ARG"} else
    if (input$country=="Peru") {"PER"}
  else if (input$country=="Brazil") {"BRA"} else
    if (input$country=="Colombia") {"COL"} else
      if (input$country=="Mexico") {"MEX"} else
        if (input$country=="Costa Rica") {"CRI"} else
          if (input$country=="Uruguay") {"URY"} else
            if (input$country=="Chile") {"CHL"} else
              if (input$country=="Paraguay") {"PRY"} else
                if (input$country=="Bahamas") {"BHS"} else
                  if (input$country=="Barbados") {"BRB"} else
                    if (input$country=="Belice") {"BLZ"} else
                      if (input$country=="Bolivia") {"BOL"} else
                        if (input$country=="Ecuador") {"ECU"} else
                          if (input$country=="Guatemala") {"GTM"} else
                            if (input$country=="Guyana") {"GUY"} else
                              if (input$country=="Honduras") {"HND"} else
                                if (input$country=="Haiti") {"HTI"} else
                                  if (input$country=="Jamaica") {"JAM"} else
                                    if (input$country=="El Salvador") {"SLV"} else
                                      if (input$country=="Nicaragua") {"NIC"} else
                                        if (input$country=="Panama") {"PAN"} else
                                          if (input$country=="Venezuela") {"VEN"} else
                                            if (input$country=="Suriname") {"SUR"} else
                                              if (input$country=="Trinidad & Tobago") {"TTO"} else
                                                if (input$country=="Republica Dominicana") {"DOM"} 
  
  
  # print(iso_country)
  map_tiles <- subset(OWDSummaryData,metric==if (input$select_map=="Casos") {"totalCasesPerMillon"} else {"totalDeathsPerMillon"}) %>% dplyr::arrange(value)
  map_tiles$tile <- as.numeric(rownames(map_tiles))
  map_tiles$SOV_A3 <- map_tiles$iso_code
  map <- merge(map,map_tiles)
  mapa_pais <- subset(map, iso_code==iso_country)
  pal <- colorBin("YlOrRd", map@data$value)
  
  
  bounds <- subset(map, map@data$SOV_A3 == iso_country) %>% 
    st_bbox() %>% 
    as.character()
  
  mapa_ui_basico <-leaflet(map,
                           options = leafletOptions(attributionControl=FALSE,
                                     zoomControl = FALSE,
                                     zoomControl = FALSE,
                                     minZoom = 1, maxZoom = 8)) %>%
                   addPolygons(stroke = F, 
                               fillOpacity = .5, 
                               smoothFactor = .5, 
                               color = ~pal(value),
                               label = map@data$BBRK_NAME) %>% 
                   leaflet::addLegend("bottomright", 
                                      pal = pal, 
                                      values = ~value, 
                                      opacity = .6, 
                                      title = if (input$select_map=="Casos") {"Casos Acum. </br>c/Mill. hab."} else {"Muertes Acum. </br>c/Mill. hab."}) %>%
                    addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
                    setView(lng = gCentroid(subset(map, ADM0_A3 ==iso_country, byid = T))@bbox[1,1],
                            lat = gCentroid(subset(map, ADM0_A3 ==iso_country, byid = T))@bbox[2,1],
                            zoom = 3) %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
                    
  output$map <- renderLeaflet({
    mapa_ui_basico 
  })
  
}
  
actualizaPanel <- function (input,output,session) {
  output$population <- renderText({
    input$country
    OWDSummaryData$value[OWDSummaryData$iso_code==iso_country & OWDSummaryData$metric=="population"]})         
  output$dailyCases <- renderText({
    input$country
    OWDSummaryData$value[OWDSummaryData$iso_code==iso_country & OWDSummaryData$metric=="dailyCases"]})         
  output$dailyDeaths <- renderText({
    input$country
    OWDSummaryData$value[OWDSummaryData$iso_code==iso_country & OWDSummaryData$metric=="dailyDeaths"]})         
  output$populationOver65 <- renderText({
    input$country
    OWDSummaryData$value[OWDSummaryData$iso_code==iso_country & OWDSummaryData$metric=="populationOver65"]})         
  output$totalCases <- renderText({
    input$country
    OWDSummaryData$value[OWDSummaryData$iso_code==iso_country & OWDSummaryData$metric=="totalCases"]})                 
  output$totalDeaths <- renderText({
    input$country
    OWDSummaryData$value[OWDSummaryData$iso_code==iso_country & OWDSummaryData$metric=="totalDeaths"]})         
  output$lifeExp <- renderText({
    input$country
    OWDSummaryData$value[OWDSummaryData$iso_code==iso_country & OWDSummaryData$metric=="lifeExp"]})         
  output$totalTestPerMillon <- renderText({
    input$country
    OWDSummaryData$value[OWDSummaryData$iso_code==iso_country & OWDSummaryData$metric=="totalTestPerMillon"]})         
  output$dailyTests <- renderText({
    input$country
    OWDSummaryData$value[OWDSummaryData$iso_code==iso_country & OWDSummaryData$metric=="dailyTests"]})   
}

actualizaVariables <- function (input,output,session) {
  capacidadUTI <<- parametros_epi$CamasUCI[parametros_epi$Pais==iso_country]
  if (is.null(input$CC_set)==F) {
    capacidadUTI <<- input$CC_set
  } 
  
  capacidadVent <<- parametros_epi$Respiradores[parametros_epi$Pais==iso_country]
  if (is.null(input$CC_Vent)==F) {
    capacidadVent <<- input$Vent_set
  }
}

actualizaCM <- function (input,output,session) {
  
  if (input$country=="Belice") {
    cm_country <- "Belize"
  } else if (input$country=="Bolivia") {
    cm_country <- "Argentina"
  } else if (input$country=="Republica Dominicana") {
    cm_country <- "Dominican Republic"
  } else if (input$country=="Trinidad & Tobago") {
    cm_country <- "Argentina"
  } else if (input$country=="Venezuela") {
    cm_country <- "Argentina"
  }  else {
    cm_country <- input$country
  }
  
  
  # empirical cm
  if(use_empirical_mc){
    contact_matrix <<- get_empirical_cm(country = cm_country, ages=as.numeric(ageGroupsV), type = "general")
    contact_matrix_home <<- get_empirical_cm(country = cm_country, ages=as.numeric(ageGroupsV), type = "home")
    contact_matrix_school <<- get_empirical_cm(country = cm_country, ages=as.numeric(ageGroupsV), type = "school")
    contact_matrix_work <<- get_empirical_cm(country = cm_country, ages=as.numeric(ageGroupsV), type = "work")
    contact_matrix_other <<- get_empirical_cm(country = cm_country, ages=as.numeric(ageGroupsV), type = "other")
    colnames(contact_matrix) = rownames(contact_matrix) = ageGroups
    colnames(contact_matrix_home) = rownames(contact_matrix_home) = ageGroups
    colnames(contact_matrix_school) = rownames(contact_matrix_school) = ageGroups
    colnames(contact_matrix_work) = rownames(contact_matrix_work) = ageGroups
    colnames(contact_matrix_other) = rownames(contact_matrix_other) = ageGroups
    transmission_probability = transmission_probability
    
    # population data
    load("data/population.RData")
    if (input$country=="Belice") {
      pop_country <- "Belize"
    } else if (input$country=="Trinidad & Tobago") {
      pop_country <- "Argentina"
    } else if (input$country=="Venezuela") {
      pop_country <- "Argentina"
    }  else {
      pop_country <- input$country
    }
    
    
    N <<- population[[pop_country]]
    
    # epi data
    
    load(paste0("data/data", iso_country, ".RData"), envir = .GlobalEnv)
    dataPorEdad <<- formatData(iso_country, ageGroupsV)
    
    #writexl::write_xlsx(data.frame(dataPorEdad$FMTD$def),"argentina.xls")
    
    diaCeroVac <<- min(dataPorEdad$FMTD$vac$fecha)
    tVacunasCero <<-  as.numeric(as.Date(diaCeroVac)-min(dataPorEdad$FMTD$def$fecha))
    vacPre <<- lapply(1:(as.numeric(tVacunasCero)-1), matrix, data=0,
                      nrow=length(immunityStates),
                      ncol=length(ageGroups))
    vacArg <<- lapply(1:nrow(dataPorEdad$FMTD$vac), matrix,  data=0,
                      nrow=length(immunityStates),
                      ncol=length(ageGroups))
    for (t in 1:length(vacArg)) {
      # TODO: Expandir a otras vacunas
      vacArg[[t]][3,]  <<- as.numeric(dataPorEdad$FMTD$vac[t,2:ncol(dataPorEdad$FMTD$vac)])
      vacArg[[t]][4,]  <<- as.numeric(dataPorEdad$FMTD$vac2[t,2:ncol(dataPorEdad$FMTD$vac2)])
      
    }
    vacArg <<- rapply(vacArg, f=function(x) ifelse(is.na(x),0,x), how="replace" )
    promedio <<- round(Reduce("+", vacArg) / length(vacArg),0)
    vacPlan <<- lapply(1:(diasDeProyeccion-length(vacArg)-length(vacPre)), matrix, data=t(promedio),
                       nrow=length(immunityStates),
                       ncol=length(ageGroups))
    
    planVacunacionFinal <<- c(vacPre,vacArg,vacPlan)
    temp <<- lapply(colnames(dataPorEdad$FMTD$def)[-1], function(loopCol) {
      loessCol = paste0(loopCol,'_loess')
      
      dataPorEdad$FMTD$def[loessCol] <<- predict(loess(dataPorEdad$FMTD$def[loopCol][,1]~seq(1,nrow(dataPorEdad$FMTD$def), by=1),span=.4))
      dataPorEdad$FMTD$def[loessCol][dataPorEdad$FMTD$def[loessCol]<0] <- 0
    })
    
    rm(temp)
    loessCols <<- which(colnames(dataPorEdad$FMTD$def) %in% grep("loess",colnames(dataPorEdad$FMTD$def), value = TRUE))
    def_p <<- dataPorEdad$FMTD$def[,loessCols]
    def_p <<- def_p[1:(nrow(def_p)-15),]
    
    fechas_master <<- seq(min(dataPorEdad$FMTD$def$fecha),
                          min(dataPorEdad$FMTD$def$fecha)+diasDeProyeccion-1,by=1)
  }
}

actualizaDTTables <- function (input,output,session) {
  # Update data from DT tables
  if (mbeta_primeraVez==T) {
    mbeta_edit <<- modif_beta
    mbeta_primeraVez<<-F
  } else {
    mbeta_edit[as.numeric(input$mbeta_cell_edit[1]),
               as.numeric(input$mbeta_cell_edit[2])+1] <- as.numeric(input$mbeta_cell_edit[3])
    mbeta_edit <<- mbeta_edit
  }
  if (mgraves_primeraVez==T) {
    mgraves_edit <<- modif_porc_gr
    mgraves_primeraVez<<-F
  } else {
    mgraves_edit[as.numeric(input$mgraves_cell_edit[1]),
                 as.numeric(input$mgraves_cell_edit[2])+1] <- as.numeric(input$mgraves_cell_edit[3])
    mgraves_edit <<- mgraves_edit
  }
  if (mcriticos_primeraVez==T) {
    mcriticos_edit <<- modif_porc_cr
    mcriticos_primeraVez<<-F
  } else {
    mcriticos_edit[as.numeric(input$mcriticos_cell_edit[1]),
                   as.numeric(input$mcriticos_cell_edit[2])+1] <- as.numeric(input$mcriticos_cell_edit[3])
    mcriticos_edit <<- mcriticos_edit
  }
  if (mifr_primeraVez==T) {
    mifr_edit <<- modif_ifr
    mifr_primeraVez<<-F
  } else {
    mifr_edit[as.numeric(input$mifr_cell_edit[1]),
              as.numeric(input$mifr_cell_edit[2])+1] <- as.numeric(input$mifr_cell_edit[3])
    mifr_edit <<- mifr_edit
  }
  if (ifr_primeraVez==T) {
    ifr_edit <- matrix(ifr, 1, length(ageGroups))
    colnames(ifr_edit) = ageGroups
    ifr_edit <<- ifr_edit
    ifr_primeraVez<<-F
  } else {
    ifr_edit[as.numeric(input$ifrt_cell_edit[1]),
             as.numeric(input$ifrt_cell_edit[2])+1] <- as.numeric(input$ifrt_cell_edit[3])
    ifr_edit <<- ifr_edit
  }
  if (transprob_primeraVez==T) {
    transprob_edit <- transmission_probability
    transprob_edit <<- as.matrix(transprob_edit)
    transprob_primeraVez<<-F
  } else {
    transprob_edit[as.numeric(input$transprob_cell_edit[1]),
                   as.numeric(input$transprob_cell_edit[2])] <- as.numeric(input$transprob_cell_edit[3])
    transprob_edit <<- transprob_edit  
  }
  if (paramVac_primeraVez==T) {
    paramVac_edit <- paramVac
    paramVac_edit <<- as.matrix(paramVac_edit)
    paramVac_primeraVez<<-F
  } else {
    paramVac_edit[as.numeric(input$paramVac_cell_edit[1]),
                  as.numeric(input$paramVac_cell_edit[2])] <<- as.numeric(input$paramVac_cell_edit[3])
    paramVac_edit <<- paramVac_edit  
  }
  if (porc_cr_primeraVez==T) {
    porc_cr_edit <- matrix(porcentajeCasosCriticos[1,], 1, length(ageGroups))
    colnames(porc_cr_edit) = ageGroups
    porc_cr_edit <<- porc_cr_edit
    porc_cr_primeraVez<<-F
  } else {
    porc_cr_edit[as.numeric(input$porc_cr_cell_edit[1]),
                 as.numeric(input$porc_cr_cell_edit[2])+1] <- as.numeric(input$porc_cr_cell_edit[3])
    porc_cr_edit <<- porc_cr_edit
    porcentajeCasosCriticos <<- matrix(rep(porc_cr_edit,length(immunityStates)),length(immunityStates),length(ageGroups),byrow=T,dimnames = )
  }
  if (porc_gr_primeraVez==T) {
    porc_gr_edit <- matrix(porcentajeCasosGraves[1,], 1, length(ageGroups))
    colnames(porc_gr_edit) = ageGroups
    porc_gr_edit <<- porc_gr_edit
    porc_gr_primeraVez<<-F
  } else {
    porc_gr_edit[as.numeric(input$porc_gr_cell_edit[1]),
                 as.numeric(input$porc_gr_cell_edit[2])+1] <- as.numeric(input$porc_gr_cell_edit[3])
    porc_gr_edit <<- porc_gr_edit
    porcentajeCasosGraves <<- matrix(rep(porc_gr_edit,length(immunityStates)),length(immunityStates),length(ageGroups),byrow=T,dimnames = )
  }
}

actualizaParametros <- function(input,output,session) {
  duracion_inmunidad = input$duracionInm
  
  
  # N, diaCeroVac, as.Date("2022-01-01"), tVacunasCero, planVacDosis1
  shinyjs::show("vacDateGoal")
  shinyjs::show("vacStrat")
  shinyjs::show("vacEfficacy")
  # shinyjs::show("immunityDuration")
  if (input$vacUptake == "Current uptake") {
    shinyjs::hide("vacDateGoal")
  } else if (input$vacUptake == "No vaccination") {
    shinyjs::hide("vacDateGoal")
    shinyjs::hide("vacStrat")
    shinyjs::hide("vacEfficacy")
    # shinyjs::hide("immunityDuration")
  }
  diasVacunacion <<- as.numeric(as.Date(input$vacDateGoal) - as.Date('2021-01-01'))
  
  # Set parameters from sliders
  if ("uptakeSlider" %in% names(reactiveValuesToList(input))) {
    # customMatrix <<- T
    if (input$uptakeSlider == "0%") {
      updateSelectInput(session, "vacUptake", selected = "No vaccination")
      selectedUptake <<- getUptake("No vaccination")
    } else if (input$uptakeSlider == "20%") {
      updateSelectInput(session, "vacUptake", selected = "Low uptake: 20%")
      selectedUptake <<- getUptake("Low uptake: 20%")
    } else if (input$uptakeSlider == "50%") {
      updateSelectInput(session, "vacUptake", selected = "Mid-range uptake: 50%")
      selectedUptake <<- getUptake("Mid-range uptake: 50%")
    } else if (input$uptakeSlider == "80%") {
      updateSelectInput(session, "vacUptake", selected = "High uptake: 80%")
      selectedUptake <<- getUptake("High uptake: 80%")
    } else if (input$uptakeSlider == "95%") {
      updateSelectInput(session, "vacUptake", selected = "High uptake: 95%")
      selectedUptake <<- getUptake("High uptake: 95%")
    }
    
    if (input$effectivenessSlider == 'Low') {
      updateSelectInput(session, "vacEfficacy", selected = "C2. 80%, 50%, 50%")
      efficacy <<- applyVaccineEfficacy("C2. 80%, 50%, 50%")
    } else if (input$effectivenessSlider  == 'Middle') {
      updateSelectInput(session, "vacEfficacy", selected = "B2. 100%, 80%, 50%")
      efficacy <<- applyVaccineEfficacy("B2. 100%, 80%, 50%")
    } else if (input$effectivenessSlider ==  'High') {
      updateSelectInput(session, "vacEfficacy", selected = "B1. 100%, 80%, 80%")
      efficacy <<- applyVaccineEfficacy("B1. 100%, 80%, 80%")
    }  
  } else {
    selectedUptake <<- getUptake(input$vacUptake)
    efficacy <<- applyVaccineEfficacy(input$vacEfficacy)
  }
  
  selectedPriority <<- getPrioritiesV2(input$vacStrat)
  
  cantidadVacunasTotal <<- selectedUptake * sum(N)
  ritmoVacunacion <<- cantidadVacunasTotal / diasVacunacion
  planVacunacionFinalParam <- generaEscenarioSage(input$vacUptake, input$vacDateGoal, input$vacStrat,
                                                  planVacunacionFinal, N, tVacunasCero, as.Date(diaCeroVac))
  
  planVacunacionFinalParam <- lapply(planVacunacionFinalParam, function(dia) {colnames(dia) <- ageGroups 
  return(dia)})
  
  planVacunacionFinalParam <<- planVacunacionFinalParam 
  
  ajuste <<- (((input$ajusta_beta*-1) + 1)/10)+0.3
  trans_prob_param <<- transprob_edit * ajuste
  
  relaxNpi <<- FALSE
  relaxGoal <<- NULL
  if (input$npiStrat == "cont") {
    shinyjs::hide("relaxationDateGoal")
    shinyjs::hide("relaxationFactor")
  } else {
    shinyjs::show("relaxationDateGoal")
    shinyjs::show("relaxationFactor")
    relaxNpi <<- TRUE
    relaxGoal <<- which(fechas_master == input$relaxationDateGoal)
  }
  # Aplicar el NPI Scenario seleccionado y mandarlo al SEIR
  contact_matrix_scenario <<- get_npi_cm_scenario(scenario = input$npiScenario,
                                                  matrix_list = list(
                                                    contact_matrix = contact_matrix,
                                                    contact_matrix_work = contact_matrix_work,
                                                    contact_matrix_home = contact_matrix_home,
                                                    contact_matrix_school = contact_matrix_school,
                                                    contact_matrix_other = contact_matrix_other),
                                                  ages= as.numeric(ageGroupsV))
  contact_matrix_relaxed <<- get_npi_cm_scenario(scenario = input$npiScenarioRelaxed,
                                                 matrix_list = list(
                                                   contact_matrix = contact_matrix,
                                                   contact_matrix_work = contact_matrix_work,
                                                   contact_matrix_home = contact_matrix_home,
                                                   contact_matrix_school = contact_matrix_school,
                                                   contact_matrix_other = contact_matrix_other),
                                                 ages= as.numeric(ageGroupsV))
  
  # paramVac_edit[3,3] = as.numeric(input$immunityDuration) * .25
  # paramVac_edit[3,5] = as.numeric(input$immunityDuration)
  
  # print(porcentajeCasosGraves)
  # print(porcentajeCasosCriticos)
  tVacunasCero <<- 303
  ifrProy <<- ifr_edit[1,]
  if (input$country == "Argentina") {
    ifrProy <<- ifrProy * 2.4
  } else if (input$country == "Peru") {
    ifrProy <<- ifrProy * 3.55
  } else if (input$country == "Colombia") {
    ifrProy <<- ifrProy * 1.8
  } else if (input$country == "Chile") {
    ifrProy <<- ifrProy * 1
  } else if (input$country == "Mexico") {
    ifrProy <<- ifrProy * 1.8
  } else if (input$country == "Brazil") {
    ifrProy <<- ifrProy * 1
  }
  
  
}
  
actualizaProy <- function (input,output,session, altScenario=NA
                           ) {
  shinyjs::hide("downloadEE")
  shinyjs::hide("EESummaryTable")
  shinyjs::hide("EESummaryTable2")
  shinyjs::hide("EESummaryTable3")
  if (is.na(altScenario)) {
    defaultScenario(iso_country) 
  } else {
    altScenario(iso_country,stringency()) 
  }
  
  
  #altScenario(iso_country)
  
  # ifrProy <<- ifrProy * ifrCalibrador
  # trans_prob_param <<- trans_prob_param * transmission_probabilityCalibrador
  # porcentajeCasosGraves <<- porcentajeCasosGraves_base * porcentajeCasosGravesCalibrador
  # porcentajeCasosCriticos <<- porcentajeCasosCriticos_base * porcentajeCasosCriticosCalibrador
  # 
  
  proy <<- seir_ages(dias=diasDeProyeccion,
                    duracionE = periodoPreinfPromedio,
                    duracionIi = duracionMediaInf,
                    porc_gr = porcentajeCasosGraves,
                    porc_cr = porcentajeCasosCriticos,
                    duracionIg = diasHospCasosGraves,
                    duracionIc = diasHospCasosCriticos,
                    ifr = ifr,#ifrProy
                    contact_matrix = contact_matrix_scenario,
                    relaxationThreshold = input$relaxationThreshold,
                    contact_matrix_relaxed = contact_matrix_relaxed,
                    transmission_probability = transmission_probability,
                    N = N,
                    defunciones_reales=def_p,
                    modif_beta=efficacy$modif_beta,
                    modif_porc_gr=efficacy$modif_porc_gr,
                    modif_porc_cr=efficacy$modif_porc_cr,
                    modif_ifr=efficacy$modif_ifr,
                    planVacunacionFinal=planVacunacionFinalParam,
                    selectedPriority=selectedPriority,
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
                    relaxFactor=input$relaxationFactor,
                    country=input$country
  )
  
}


actualizaPlot <- function(input,output,session) {


  data_graf <<- bind_rows(
    tibble(Compart = "S", do.call(rbind, lapply(proy$`S: Susceptible`,colSums)) %>% as_tibble()),
    tibble(Compart = "V", do.call(rbind, lapply(proy$`V: Vaccinated`,colSums)) %>% as_tibble()),
    tibble(Compart = "vA", do.call(rbind, lapply(proy$`vA: Daily vaccinations`,colSums)) %>% as_tibble()),
    tibble(Compart = "E", do.call(rbind, lapply(proy$`E: Exposed`,colSums)) %>% as_tibble()),
    tibble(Compart = "e", do.call(rbind, lapply(proy$`e: Daily exposed`,colSums)) %>% as_tibble()),
    tibble(Compart = "I", do.call(rbind, lapply(proy$`I: Infectious`,colSums)) %>% as_tibble()),
    tibble(Compart = "Ii", do.call(rbind, lapply(proy$`Ii: Infectious (mild)`,colSums)) %>% as_tibble()),
    tibble(Compart = "Ic", do.call(rbind, lapply(proy$`Ic: Infectious (severe)`,colSums)) %>% as_tibble()),
    tibble(Compart = "Ig", do.call(rbind, lapply(proy$`Ig: Infectious (moderate)`,colSums)) %>% as_tibble()),
    tibble(Compart = "i", do.call(rbind, lapply(proy$`i: Daily infectious`,colSums)) %>% as_tibble()),
    tibble(Compart = "D", do.call(rbind, lapply(proy$`D: Deaths`,colSums)) %>% as_tibble()),
    tibble(Compart = "d", do.call(rbind, lapply(proy$`d: Daily deaths`,colSums)) %>% as_tibble()),
    tibble(Compart = "R", do.call(rbind, lapply(proy$`R: Recovered (survivors + deaths)`,colSums)) %>% as_tibble()),
    tibble(Compart = "U", do.call(rbind, lapply(proy$`U: Survivors`,colSums)) %>% as_tibble()),
    tibble(Compart = "u", do.call(rbind, lapply(proy$`u: Daily survivors`,colSums)) %>% as_tibble()),
    tibble(Compart = "yl", do.call(rbind, lapply(proy$`yl: Years lost`,colSums)) %>% as_tibble()) %>% mutate_at(ageGroups, cumsum),
    tibble(Compart = "pV", do.call(rbind, lapply(planVacunacionFinalParam,colSums)) %>% as_tibble())) %>%
    dplyr::mutate(fecha = rep(1:length(proy$S),17)) %>%
    # TODO: Arreglar
    dplyr::rename("0-17"=2, "18-29"=3, "30-39"=4, "40-49"=5, "50-59"=6, "60-69"=7, "70-79"=8, "80+"=9)
  data_graf$total<<-data_graf$`0-17`+data_graf$`18-29`+data_graf$`30-39`+data_graf$`40-49`+data_graf$`50-59`+data_graf$`60-69`+data_graf$`70-79`+data_graf$`80+`
  
  #casos reales
  
  Compart=rep("casos_registrados",nrow(dataPorEdad$FMTD$casos))
  fecha=seq(1,nrow(dataPorEdad$FMTD$casos),by=1)
  casos=dataPorEdad$FMTD$casos[-1]
  total<<-rowSums(casos)
  casos=cbind(Compart,casos,fecha,total) 
  colnames(casos)=colnames(data_graf)
  casos <- data.frame(fecha=unique(data_graf$fecha)) %>% left_join(casos, by = "fecha")
  
  #casos[is.na(casos)] <- 0
  casos$Compart <- "casos_registrados"
  
  data_graf<<-union_all(data_graf,casos)
  
  #muertes reales
  
  Compart=rep("muertes_registradas",nrow(dataPorEdad$FMTD$def))
  fecha=seq(1,nrow(dataPorEdad$FMTD$def),by=1)
  muertes=dataPorEdad$FMTD$def[2:(length(ageGroups)+1)]
  total=rowSums(muertes)
  muertes=cbind(Compart,muertes,fecha,total) 
  colnames(muertes)=colnames(data_graf)
  muertes <- data.frame(fecha=unique(data_graf$fecha)) %>% left_join(muertes, by = "fecha")
  #muertes[is.na(muertes)] <- 0
  muertes$Compart <- "muertes_registradas"
  
  data_graf<<-union_all(data_graf,muertes)
  output$graficoUnico <- renderPlotly({
    # res_t()
    
    if (length(proy) > 0 & (input$compart_a_graficar != "" | mode_ui=="basico")) {
      
      if (mode_ui=="basico") {
        col_id=str_trim(str_replace_all(substring(input$compart_checkbox,1,3),":",""))
        compart_label <- input$compart_checkbox
      } else {
        col_id=str_trim(str_replace_all(substring(input$compart_a_graficar,1,3),":",""))
        compart_label <- input$compart_a_graficar
      }
      
      
      
      dataTemp <<- data_graf %>% dplyr::filter(Compart == col_id)
      dataTemp$fechaDia <<- fechas_master 
      dataRep_cases <<- data_graf %>% dplyr::filter(Compart == "casos_registrados")
      dataRep_cases$fechaDia <<- fechas_master
      dataRep_deaths <<- data_graf %>% dplyr::filter(Compart == "muertes_registradas")
      dataRep_deaths$fechaDia = fechas_master
      
      #colnames(dataTemp)[8] <- "70-79"
      # dataTemp$fechaDia = seq(min(dataEcdc$dateRep),min(dataEcdc$dateRep)+diasDeProyeccion-1,by=1)
      valx = dataTemp$fechaDia[tVacunasCero]
      maxy = max(dataTemp$total)
      
      if (is.null(input$edad)==F & is.na(input$diasProy)==F) {
        #data=dataTemp[1:(input$t+input$diasProy),]
        data=dataTemp
        data[data<0] = 0
        plot=plot_ly(data=data, x=~fechaDia)          
        
        if (length(input$edad)>0) {
          lapply(X=input$edad, FUN = function(edad) {
            
            plot <<- add_trace(plot, y=~eval(parse(text=paste0('`',edad,'`'))), type="scatter", mode="lines", name=edad, line = list(dash = ifelse(edad=='total','','dot')))
            if (input$check_cases==T) {
              plot <<- add_trace(p=plot, data=dataRep_cases, y=~eval(parse(text=paste0('`',edad,'`'))), type="bar", name=edad)
            }
            
            if (input$check_deaths==T) {
              plot <<- add_trace(p=plot, data=dataRep_deaths, y=~eval(parse(text=paste0('`',edad,'`'))), type="bar", name=edad)
            }
            
            
          })
          
          if (input$compart_a_graficar == "Ig: Infectious (moderate) SKIP") {
            plot <-  add_segments(plot, x= data$fechaDia[1], xend = data$fechaDia[diasDeProyeccion], y = camasGenerales*porcAsignadoCovid, yend = camasGenerales*porcAsignadoCovid, name = "General beds", line=list(color="#fc9272", dash="dot"))
            plot <-  add_segments(plot, x= valx, xend = valx, y = 0, yend = max(maxy,camasGenerales*porcAsignadoCovid*1.1) , name = paste(valx), line=list(color="#bdbdbd"))    
            plot <- plot %>% layout(xaxis = list(title = "Fecha"), 
                                    yaxis = list(title = paste("Compartimento:",compart_label)))
            # , range = c(0,60000)
          } else if (input$compart_a_graficar == "Ic: Infectious (severe)" |
                     input$compart_checkbox  == "Ic: Infectious (severe)") {
            if (is.null(input$Porc_crit)==F) {
              porcAsignadoCovid <- input$Porc_crit/100
            }
            
            plot <-  add_segments(plot, x= data$fechaDia[1], xend = data$fechaDia[diasDeProyeccion], y = capacidadUTI, yend = capacidadUTI, name = "ICU beds: (100%)", line=list(color="#fc9272", dash="dot"))
            plot <-  add_segments(plot, x= data$fechaDia[1], xend = data$fechaDia[diasDeProyeccion], y = capacidadUTI*porcAsignadoCovid, yend = capacidadUTI*porcAsignadoCovid, name = paste0("ICU beds (",round(porcAsignadoCovid*100,0),"%)"), line=list(color="#fc9272", dash="dot"))
            plot <-  add_segments(plot, x= valx, xend = valx, y = 0, yend = max(maxy,capacidadUTI*porcAsignadoCovid*1.1) , name = paste(valx), line=list(color="#bdbdbd"))    
            plot <-  plot %>% layout(xaxis = list(title = "Fecha"), 
                                     yaxis = list(title = paste("Compartimento:",compart_label)))
            
          } else {
            plot <-  add_segments(plot, x= valx, xend = valx, y = 0, yend = maxy, name = paste(valx), line=list(color="#bdbdbd"))    
            plot <- plot %>% layout(xaxis = list(title = "Fecha"), 
                                    yaxis = list(title = paste("Compartimento:",compart_label)))
          }
          
        }
        
        
        if (input$check_rt==T) {
          days_before_rt <- c(rep(0,7))
          days_after_rt <- c(rep(NA,nrow(data[is.na(data$Compart),])))
          
          r <- c(days_before_rt,
                 proy$`Rt: Effective reproduction number`$`Median(R)`,
                 days_after_rt)
          
          plot <- 
            plot %>% add_trace(y = ~r, mode = "dotted", line=list(dash="dot"), type="scatter", yaxis = "y2", name = "Rt") %>%
            layout(yaxis2 = list(overlaying = "y", side = "right"),
                   xaxis = list(title = "Fecha"),
                   yaxis = list(title = paste("Compartimento:", compart_label)))
          plot %>% add_trace(y=~rep(1,1100), mode = "dotted", line=list(dash="dash", color="#bdbdbd"), type="scatter", yaxis = "y2", name = "Rt = 1")
        } else (plot)
      }
    }
  })
  
  output$graficoVac <- renderPlotly({
    # browser()
    data_graf_vac <- 
      bind_rows(
        tibble(Compart = "S_vac", do.call(rbind, lapply(proy$`S: Susceptible`,function (x) {colSums(x[3:4,])})) %>% as_tibble()),
        tibble(Compart = "S1", do.call(rbind, lapply(proy$`S: Susceptible`,function (x) {x[1,]})) %>% as_tibble()),
        tibble(Compart = "S2", do.call(rbind, lapply(proy$`S: Susceptible`,function (x) {x[2,]})) %>% as_tibble()),
        tibble(Compart = "S3", do.call(rbind, lapply(proy$`S: Susceptible`,function (x) {x[3,]})) %>% as_tibble()),
        tibble(Compart = "S4", do.call(rbind, lapply(proy$`S: Susceptible`,function (x) {x[4,]})) %>% as_tibble()),
        tibble(Compart = "VA3", do.call(rbind, lapply(proy$`vA: Daily vaccinations`,function (x) {x[3,]})) %>% as_tibble()),
        tibble(Compart = "VA4", do.call(rbind, lapply(proy$`vA: Daily vaccinations`,function (x) {x[4,]})) %>% as_tibble()),
        tibble(Compart = "utotal", do.call(rbind, lapply(proy$`U: Survivors`,function (x) {colSums(x)})) %>% as_tibble()),
        tibble(Compart = "V", do.call(rbind, lapply(proy$`V: Vaccinated`,colSums)) %>% as_tibble()))
    data_graf_vac$total <- rowSums(data_graf_vac[,c(-1)])
    data_graf_vac = data_graf_vac[data_graf_vac$Compart==input$compartVac,]
    data_graf_vac$fecha <- fechas_master
    plot_vac <- plot_ly(data_graf_vac[data_graf_vac$Compart==input$compartVac,])
    plot_vac <- add_trace(plot_vac,
                          x = ~fecha, 
                          y = ~total,
                          type = 'scatter', 
                          mode = 'lines')
    plot_vac <- layout(plot_vac,
                       yaxis2 = list(overlaying = "y", side = "right"),
                       xaxis = list(title = "Fecha"),
                       yaxis = list(title = paste("Compartimento:", first(data_graf_vac$Compart))))
    
    plot_vac
  })
  
}


actualizaTablas <- function(input,output,session) {
  data_text_temp <<- cbind(data_graf,rep(fechas_master,length(unique(data_graf$Compart))))
  colnames(data_text_temp)[ncol(data_text_temp)] <- "fechaDia"
  data_text <<- data_text_temp %>% dplyr::group_by(Compart) %>%
    dplyr::mutate(ac=cumsum(total))
  
  fechas <- c("2021-06-30",
              "2021-12-31",
              "2022-06-30")
  fechas <<- fechas
  
  tfechas <<- c(which(fechas_master == "2021-06-30"),
               which(fechas_master == "2021-12-31"),
               which(fechas_master == "2022-06-30"))
  
  def_ac <<- data_text$total[(as.character(data_text$fechaDia) %in% fechas) &
                              data_text$Compart=="D"]  
  
  casos_ac <<- data_text$ac[(as.character(data_text$fechaDia) %in% fechas) &
                             data_text$Compart=="i"]
  
  vacunas_ac <<- data_text$ac[(as.character(data_text$fechaDia) %in% fechas) &
                               data_text$Compart=="vA"] 
  tFechas <<- c(grep(fechas[1], fechas_master),
               grep(fechas[2], fechas_master),
               grep(fechas[3], fechas_master))
  
  poblacion_vac <- c(
    
    sum(sapply(proy[["vA: Daily vaccinations"]][1:tFechas[1]],
               simplify = T,
               function (x) {
                 sum(x[3, ])
               })),
    
    sum(sapply(proy[["vA: Daily vaccinations"]][1:tFechas[2]],
               simplify = T,
               function (x) {
                 sum(x[3, ])
               })),
    
    sum(sapply(proy[["vA: Daily vaccinations"]][1:tFechas[3]],
               simplify = T,
               function (x) {
                 sum(x[3, ])
               }))
  ) / sum(N) *100
  
  poblacion_vac2 <- c(
    
    sum(sapply(proy[["vA: Daily vaccinations"]][1:tFechas[1]],
               simplify = T,
               function (x) {
                 sum(x[4, ])
               })),
    
    sum(sapply(proy[["vA: Daily vaccinations"]][1:tFechas[2]],
               simplify = T,
               function (x) {
                 sum(x[4, ])
               })),
    
    sum(sapply(proy[["vA: Daily vaccinations"]][1:tFechas[3]],
               simplify = T,
               function (x) {
                 sum(x[4, ])
               }))
  ) / sum(N) *100
  
  years_lost <<- c(sum(sapply(proy[["yl: Years lost"]][1:tFechas[1]],simplify = T,sum)),
                  sum(sapply(proy[["yl: Years lost"]][1:tFechas[2]],simplify = T,sum)),
                  sum(sapply(proy[["yl: Years lost"]][1:tFechas[3]],simplify = T,sum)))
  
  costo_ec_1 <- mean(costo_economico[costo_economico$escenario=="DEFAULT" &
                                     costo_economico$fecha<"2021-06-30","costo"])*100
  costo_ec_2 <- mean(costo_economico[costo_economico$escenario=="DEFAULT" &
                                     costo_economico$fecha<"2021-12-31","costo"])*100
  
  var=c("Defunciones acumuladas",
        "Infecciones acumuladas",
        "Vacunas aplicadas",
        "Cobertura con 1 dosis (%)",
        "Cobertura con 2 dosis (%)",
        "Años de vida perdidos",
        "Costo económico")
  C1 = c(def_ac[1],
         casos_ac[1],
         vacunas_ac[1],
         poblacion_vac[1],
         poblacion_vac2[1],
         years_lost[1],
         costo_ec_1)
  
  C2 = c(def_ac[2],
         casos_ac[2],
         vacunas_ac[2],
         poblacion_vac[2],
         poblacion_vac2[2],
         years_lost[2],
         costo_ec_2)
  
  C3 = c(def_ac[3],
         casos_ac[3],
         vacunas_ac[3],
         poblacion_vac[3],
         poblacion_vac2[3],
         years_lost[3],
         0)
  
  tabla <- cbind(var,
                 format(round(C1,0), big.mark = ','),
                 format(round(C2,0), big.mark = ','),
                 format(round(C3,0), big.mark = ','))
  tabla[7,4] <- "-"
  colnames(tabla) <- c(" ",fechas)
  tabla <<- tabla
  
  tabla_scn <<- DT::datatable(tabla,
                              caption = 'Tabla de resultados',
                              options = list(ordering=F, 
                                             searching=F, 
                                             paging=F, 
                                             info=F)) %>% formatStyle(' ', `text-align` = 'left') %>%
    formatStyle(fechas[1], `text-align` = 'right') %>%
    formatStyle(fechas[2], `text-align` = 'right') %>%
    formatStyle(fechas[3], `text-align` = 'right') 
  output$resumen_tabla <- renderDataTable({
    tabla_scn
  })
  
  # if (primeraVez) {
  #   updateSelectInput(session, "compart_a_graficar", choices = c(names(proy)[0:16],"pV: Vaccination plan"), selected="i: Daily infectious")
  #   updateNumericInput(session, inputId = "t", value = tHoy)
  #   for (c in rev(str_trim(str_replace_all(substring(names(proy)[-16],1,3),":","")))) {
  #     insertUI("#content", "afterEnd",
  #              column(6,fluidRow(column(1,p(c), align="center"),
  #                                column(11,
  #                                       DTOutput(c)
  #                                       , align="center")
  #              )),
  #              immediate = TRUE
  #     )
  #   }
  #   primeraVez <<- FALSE
  # }
  # t = input$t
  # # Actualiza tab pane de tablas de comapartimentos
  # 
  # lapply(
  #   X = names(proy),
  #   FUN = function(c){
  #     ui_id=str_trim(str_replace_all(substring(c,1,3),":",""))
  #     output[[ui_id]] <- renderDT(round(proy[[c]][[t]],0), editable = F,rownames = T, options = list(paging = FALSE, info = FALSE, searching = FALSE, fixedColumns = TRUE,autoWidth = TRUE,ordering = FALSE,dom = 'Bfrtip'))
  #   }
  # )
  
}