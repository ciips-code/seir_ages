library(tidyr)
library(shiny)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(plotly)
library(bslib)
library(zoo)
library(stats)
library(ggpubr)
library(reshape2)
library(ggplot2)
library(DT)
library(EpiEstim)
library(covoid)
library(shinyjs)
library(modelr)
library(stringr)
library(shinyWidgets)
library(waiter)

jsResetCode <<- "shinyjs.reset = function() {history.go(0)}"

options(dplyr.summarise.inform = FALSE)

comp_table <<- list()
output_list <<- c()
countries <<- c("Argentina", "Brazil", "Chile", "Colombia", "Mexico", "Peru")

flags <<- c(
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ar.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/br.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/cl.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/co.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/mx.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/pe.svg"
)

# carga RData

load("data/parameters.RData", envir = .GlobalEnv)

# lee funciones
source("functions/update.R", encoding = "UTF-8")
source("functions/seirAges_matrices.R", encoding = "UTF-8")
source("functions/vacunas.R", encoding = "UTF-8")
source("functions/params.R", encoding = "UTF-8")
source("functions/ui.R", encoding = "UTF-8")

setParameters()

server <- function (input, output, session) {
  
  observe({
    
    if (is.null(output_list)) {hide("del_scenarios")}
  })
  
  w <- Waiter$new(
    id = "graficoUnico",
    html = spin_3(), 
    color = transparent(.5)
  )
  cancel.onSessionEnded <- session$onSessionEnded(function () {
    if (exists("compare")) {
      compare <<- compare[compare$Compart=="",]
    }
  })
  
  # country customize
  observeEvent(input$country, { 
   iso_country <- if (input$country=="Argentina") {"ARG"} else
      if (input$country=="Peru") {"PER"} else
      if (input$country=="Brazil") {"BRA"} else
      if (input$country=="Colombia") {"COL"} else
      if (input$country=="Mexico") {"MEX"} else
      if (input$country=="Chile") {"CHL"}
      # print(iso_country)
      
      
    capacidadUTI <<- if (input$country=="Argentina") {11676} else
                     if (input$country=="Brazil") {37950} else
                     if (input$country=="Peru") {2804} else
                     if (input$country=="Colombia") {13054} else
                     if (input$country=="Mexico") {11634} else
                     if (input$country=="Chile") {4481}
      
    # empirical cm
    if(use_empirical_mc){
      #browser()
      contact_matrix <<- get_empirical_cm(country = input$country, ages=as.numeric(ageGroupsV), type = "general")
      contact_matrix_home <<- get_empirical_cm(country = input$country, ages=as.numeric(ageGroupsV), type = "home")
      contact_matrix_school <<- get_empirical_cm(country = input$country, ages=as.numeric(ageGroupsV), type = "school")
      contact_matrix_work <<- get_empirical_cm(country = input$country, ages=as.numeric(ageGroupsV), type = "work")
      contact_matrix_other <<- get_empirical_cm(country = input$country, ages=as.numeric(ageGroupsV), type = "other")
      colnames(contact_matrix) = rownames(contact_matrix) = ageGroups
      colnames(contact_matrix_home) = rownames(contact_matrix_home) = ageGroups
      colnames(contact_matrix_school) = rownames(contact_matrix_school) = ageGroups
      colnames(contact_matrix_work) = rownames(contact_matrix_work) = ageGroups
      colnames(contact_matrix_other) = rownames(contact_matrix_other) = ageGroups
      transmission_probability = transmission_probability
        
      # population data
      load("data/population.RData")
      N <<- population[[input$country]]
        
      # epi data
      
      load(paste0("data/data", iso_country, ".RData"), envir = .GlobalEnv)
      dataPorEdad <<- formatData(iso_country, ageGroupsV)
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
      
      
      #browser()
      fechas_master <<- seq(min(dataPorEdad$FMTD$def$fecha),
                            min(dataPorEdad$FMTD$def$fecha)+diasDeProyeccion-1,by=1)
      
      
    } 
    
  })
  
  delete<<-F
  observe({
    input$mbeta_cell_edit
    if (mbeta_primeraVez==T) {
      mbeta_edit <<- modif_beta
      mbeta_primeraVez<<-F
    } else {
      mbeta_edit[as.numeric(input$mbeta_cell_edit[1]),
                 as.numeric(input$mbeta_cell_edit[2])+1] <- as.numeric(input$mbeta_cell_edit[3])
      mbeta_edit <<- mbeta_edit
    }
  })
  output$mbeta <- renderDT({
    DT::datatable(mbeta_edit, editable = T, caption = 'Beta modifier', options = list(ordering=F, searching=F, paging=F, info=F))
  })
  
  observe({
    input$mgraves_cell_edit
    if (mgraves_primeraVez==T) {
      mgraves_edit <<- modif_porc_gr
      mgraves_primeraVez<<-F
    } else {
      mgraves_edit[as.numeric(input$mgraves_cell_edit[1]),
                   as.numeric(input$mgraves_cell_edit[2])+1] <- as.numeric(input$mgraves_cell_edit[3])
      mgraves_edit <<- mgraves_edit
    }
  })
  output$mgraves <- renderDT({
    DT::datatable(mgraves_edit, editable = T, caption = 'Severe disease modifier', options = list(ordering=F, searching=F, paging=F, info=F))
  })
  
  observe({
    input$mcriticos_cell_edit
    if (mcriticos_primeraVez==T) {
      mcriticos_edit <<- modif_porc_cr
      mcriticos_primeraVez<<-F
    } else {
      mcriticos_edit[as.numeric(input$mcriticos_cell_edit[1]),
                     as.numeric(input$mcriticos_cell_edit[2])+1] <- as.numeric(input$mcriticos_cell_edit[3])
      mcriticos_edit <<- mcriticos_edit
    }
  })
  output$mcriticos <- renderDT({
    DT::datatable(mcriticos_edit, editable = T, caption = 'Critical disease modifier', options = list(ordering=F, searching=F, paging=F, info=F))
  })
  
  observe({
    input$mifr_cell_edit
    if (mifr_primeraVez==T) {
      mifr_edit <<- modif_ifr
      mifr_primeraVez<<-F
    } else {
      mifr_edit[as.numeric(input$mifr_cell_edit[1]),
                as.numeric(input$mifr_cell_edit[2])+1] <- as.numeric(input$mifr_cell_edit[3])
      mifr_edit <<- mifr_edit
    }
  })
  output$mifr <- renderDT({
    DT::datatable(mifr_edit, editable = T, caption = 'IFR modifier', options = list(ordering=F, searching=F, paging=F, info=F))
  })
  
  observe({
    input$ifrt_cell_edit
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
  })
  
  output$ifrt <- renderDT({
    DT::datatable(ifr_edit, editable = T,
                  caption = 'Age specific IFR',
                  options = list(ordering=F, 
                                 searching=F, 
                                 paging=F, 
                                 info=F))
  })
  
  observe({
    input$transprob_cell_edit
    if (transprob_primeraVez==T) {
      transprob_edit <- transmission_probability
      transprob_edit <<- as.matrix(transprob_edit)
      transprob_primeraVez<<-F
    } else {
      transprob_edit[as.numeric(input$transprob_cell_edit[1]),
                     as.numeric(input$transprob_cell_edit[2])] <- as.numeric(input$transprob_cell_edit[3])
      transprob_edit <<- transprob_edit  
    }
  })
  
  output$transprob <- renderDT({
    DT::datatable(transprob_edit, editable = T,
                  caption = 'Effective contacts matrix',
                  options = list(ordering=F, 
                                 searching=F, 
                                 paging=F, 
                                 info=F))
  })
  
  observe({
    input$paramVac_cell_edit
    if (paramVac_primeraVez==T) {
      paramVac_edit <- paramVac
      paramVac_edit <<- as.matrix(paramVac_edit)
      paramVac_primeraVez<<-F
    } else {
      paramVac_edit[as.numeric(input$paramVac_cell_edit[1]),
                    as.numeric(input$paramVac_cell_edit[2])] <<- as.numeric(input$paramVac_cell_edit[3])
      paramVac_edit <<- paramVac_edit  
    }
  })
  
  
  observe({
    input$porc_cr_cell_edit
    
    if (porc_cr_primeraVez==T) {
      porc_cr_edit <- matrix(porcentajeCasosCriticos[1,], 1, length(ageGroups))
      colnames(porc_cr_edit) = ageGroups
      porc_cr_edit <<- porc_cr_edit
      porc_cr_primeraVez<<-F
    } else {
      porc_cr_edit[as.numeric(input$porc_cr_cell_edit[1]),
                   as.numeric(input$porc_cr_cell_edit[2])+1] <- as.numeric(input$porc_cr_cell_edit[3])
      porc_cr_edit <<- porc_cr_edit
      porcentajeCasosCriticos <<- matrix(rep(porc_cr_edit,length(immunityStates)),length(immunityStates),length(ageGroups),byrow=T,dimnames = names)
    }
  })
  
  output$porc_cr <- renderDT({
    DT::datatable(porc_cr_edit, editable = T,
                  caption = 'Critic case probability',
                  options = list(ordering=F, 
                                 searching=F, 
                                 paging=F, 
                                 info=F))
  })
  
  observe({
    input$porc_gr_cell_edit
    
    if (porc_gr_primeraVez==T) {
      porc_gr_edit <- matrix(porcentajeCasosGraves[1,], 1, length(ageGroups))
      colnames(porc_gr_edit) = ageGroups
      porc_gr_edit <<- porc_gr_edit
      porc_gr_primeraVez<<-F
    } else {
      porc_gr_edit[as.numeric(input$porc_gr_cell_edit[1]),
                   as.numeric(input$porc_gr_cell_edit[2])+1] <- as.numeric(input$porc_gr_cell_edit[3])
      porc_gr_edit <<- porc_gr_edit
      porcentajeCasosGraves <<- matrix(rep(porc_gr_edit,length(immunityStates)),length(immunityStates),length(ageGroups),byrow=T,dimnames = names)
    }
  })
  
  output$porc_gr <- renderDT({
    DT::datatable(porc_gr_edit, editable = T,
                  caption = 'Severe case probability',
                  options = list(ordering=F, 
                                 searching=F, 
                                 paging=F, 
                                 info=F))
  })
  
  output$paramVac <- renderDT({
    DT::datatable(paramVac_edit, editable = T,
                  caption = 'Vaccines parameters',
                  options = list(ordering=F, 
                                 searching=F, 
                                 paging=F, 
                                 info=F))
  })
  
  proy <- reactive({
    #browser()
    # paste activa reactive (no comentar)
    paste(input$paramVac_cell_edit)
    paste(input$ifrt_cell_edit)
    paste(input$transprob_cell_edit)
    paste(input$mbeta_cell_edit)
    paste(input$mgraves_cell_edit)
    paste(input$mcriticos_cell_edit)
    paste(input$mifr_cell_edit)
    paste(input$porc_cr_cell_edit)
    paste(input$porc_gr_cell_edit)
    paste(input$country)
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
    diasVacunacion = as.numeric(as.Date(input$vacDateGoal) - as.Date('2021-01-01'))
    selectedPriority <- getPrioritiesV2(input$vacStrat)
    selectedUptake <- getUptake(input$vacUptake)
    cantidadVacunasTotal = selectedUptake * sum(N)
    ritmoVacunacion = cantidadVacunasTotal / diasVacunacion
    
    planVacunacionFinalParam <- generaEscenarioSage(input$vacUptake, input$vacDateGoal, input$vacStrat,
                                                    planVacunacionFinal, N, tVacunasCero, as.Date(diaCeroVac))
    
    planVacunacionFinalParam <- lapply(planVacunacionFinalParam, function(dia) {colnames(dia) <- ageGroups 
    return(dia)})
    
    planVacunacionFinalParam <<- planVacunacionFinalParam 
    
    ajuste = (((input$ajusta_beta*-1) + 1)/10)+0.3
    trans_prob_param <- transprob_edit * ajuste
    
    relaxNpi = FALSE
    relaxGoal = NULL
    if (input$npiStrat == "cont") {
      shinyjs::hide("relaxationDateGoal")
      shinyjs::hide("relaxationFactor")
    } else {
      shinyjs::show("relaxationDateGoal")
      shinyjs::show("relaxationFactor")
      relaxNpi = TRUE
      relaxGoal = which(fechas_master == input$relaxationDateGoal)
    }
    
    # Aplicar el NPI Scenario seleccionado y mandarlo al SEIR
    contact_matrix_scenario <- get_npi_cm_scenario(scenario = input$npiScenario,
                                                   matrix_list = list(
                                                     contact_matrix = contact_matrix,
                                                     contact_matrix_work = contact_matrix_work,
                                                     contact_matrix_home = contact_matrix_home,
                                                     contact_matrix_school = contact_matrix_school,
                                                     contact_matrix_other = contact_matrix_other),
                                                   ages= as.numeric(ageGroupsV))
    contact_matrix_relaxed <- get_npi_cm_scenario(scenario = input$npiScenarioRelaxed,
                                                   matrix_list = list(
                                                     contact_matrix = contact_matrix,
                                                     contact_matrix_work = contact_matrix_work,
                                                     contact_matrix_home = contact_matrix_home,
                                                     contact_matrix_school = contact_matrix_school,
                                                     contact_matrix_other = contact_matrix_other),
                                                   ages= as.numeric(ageGroupsV))
    efficacy = applyVaccineEfficacy(input$vacEfficacy)
    
    # paramVac_edit[3,3] = as.numeric(input$immunityDuration) * .25
    # paramVac_edit[3,5] = as.numeric(input$immunityDuration)
    
    # print(porcentajeCasosGraves)
    # print(porcentajeCasosCriticos)
    #browser()
    tVacunasCero = 303
    ifrProy = ifr_edit[1,]
    if (input$country == "Argentina") {
      ifrProy = ifrProy * 2.4
    } else if (input$country == "Peru") {
      ifrProy = ifrProy * 3.55
    } else if (input$country == "Colombia") {
      ifrProy = ifrProy * 1.8
    } else if (input$country == "Chile") {
      ifrProy = ifrProy * 1
    } else if (input$country == "Mexico") {
      ifrProy = ifrProy * 1.8
    } else if (input$country == "Brazil") {
      ifrProy = ifrProy * 1
    }
    proy <- seir_ages(dias=diasDeProyeccion,
                      duracionE = periodoPreinfPromedio,
                      duracionIi = duracionMediaInf,
                      porc_gr = porcentajeCasosGraves,
                      porc_cr = porcentajeCasosCriticos,
                      duracionIg = diasHospCasosGraves,
                      duracionIc = diasHospCasosCriticos,
                      ifr = ifrProy,
                      contact_matrix = contact_matrix_scenario,
                      relaxationThreshold = input$relaxationThreshold,
                      contact_matrix_relaxed = contact_matrix_relaxed,
                      transmission_probability = trans_prob_param,
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
    
    return(proy)
    
  })
  observe({
    if (primeraVez) {
      updateSelectInput(session, "compart_a_graficar", choices = c(names(proy())[-16],"pV: Vaccination plan"), selected="i: Daily infectious")
      updateNumericInput(session, inputId = "t", value = tHoy)
      for (c in rev(str_trim(str_replace_all(substring(names(proy())[-16],1,3),":","")))) {
        insertUI("#content", "afterEnd",
                 column(6,fluidRow(column(1,p(c), align="center"),
                                   column(11,
                                          DTOutput(c)
                                          , align="center")
                 )),
                 immediate = TRUE
        )
      }
      primeraVez <<- FALSE
    }
    t = input$t
    # Actualiza tab pane de tablas de comapartimentos
    
    lapply(
      X = names(proy()),
      FUN = function(c){
        ui_id=str_trim(str_replace_all(substring(c,1,3),":",""))
        output[[ui_id]] <- renderDT(round(proy()[[c]][[t]],0), editable = F,rownames = T, options = list(paging = FALSE, info = FALSE, searching = FALSE, fixedColumns = TRUE,autoWidth = TRUE,ordering = FALSE,dom = 'Bfrtip'))
      }
    )
  })
  
  observeEvent(input$prev, {
    # updateTables(input$t - 1)
    updateNumericInput(session,"t", value =  input$t - 1)
  })
  
  observeEvent(input$prox, {
    # updateTables(input$t + 1)
    updateNumericInput(session,"t", value =  input$t + 1)
  })
  
  data_graf <- reactive({
    w$show()
    #browser()
    
    proy <- proy()
    data_graf <- bind_rows(
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
      tibble(Compart = "pV", do.call(rbind, lapply(planVacunacionFinalParam,colSums)) %>% as_tibble())) %>%
      dplyr::mutate(fecha = rep(1:length(proy$S),16)) %>%
      # TODO: Arreglar
      dplyr::rename("0-17"=2, "18-29"=3, "30-39"=4, "40-49"=5, "50-59"=6, "60-69"=7, "70-79"=8, "80+"=9)
    data_graf$total=data_graf$`0-17`+data_graf$`18-29`+data_graf$`30-39`+data_graf$`40-49`+data_graf$`50-59`+data_graf$`60-69`+data_graf$`70-79`+data_graf$`80+`
    
    #casos reales
    
    Compart=rep("casos_registrados",nrow(dataPorEdad$FMTD$casos))
    fecha=seq(1,nrow(dataPorEdad$FMTD$casos),by=1)
    casos=dataPorEdad$FMTD$casos[-1]
    total=rowSums(casos)
    casos=cbind(Compart,casos,fecha,total) 
    colnames(casos)=colnames(data_graf)
    casos <- data.frame(fecha=unique(data_graf$fecha)) %>% left_join(casos, by = "fecha")
    
    #casos[is.na(casos)] <- 0
    casos$Compart <- "casos_registrados"
    
    data_graf=union_all(data_graf,casos)
    
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
    
    data_graf=union_all(data_graf,muertes)
    data_graf
    
  })
  
  output$graficoUnico <- renderPlotly({
    #browser()
    res_t()
    if (length(proy()) > 0 & input$compart_a_graficar != "") {
      
      col_id=str_trim(str_replace_all(substring(input$compart_a_graficar,1,3),":",""))
      dataTemp = data_graf() %>% dplyr::filter(Compart == col_id)
      dataTemp$fechaDia = fechas_master 
      dataRep_cases = data_graf() %>% dplyr::filter(Compart == "casos_registrados")
      dataRep_cases$fechaDia = fechas_master
      dataRep_deaths = data_graf() %>% dplyr::filter(Compart == "muertes_registradas")
      dataRep_deaths$fechaDia = fechas_master
      
      #colnames(dataTemp)[8] <- "70-79"
      
      # dataTemp$fechaDia = seq(min(dataEcdc$dateRep),min(dataEcdc$dateRep)+diasDeProyeccion-1,by=1)
      valx = dataTemp$fechaDia[input$t]
      maxy = max(dataTemp$total)
      
      if (is.null(input$edad)==F & is.na(input$diasProy)==F) {
        #data=dataTemp[1:(input$t+input$diasProy),]
        data=dataTemp
        data[data<0] = 0
        # browser()
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
                                    yaxis = list(title = paste("Compartimento:",input$compart_a_graficar)))
            # , range = c(0,60000)
          } else if (input$compart_a_graficar == "Ic: Infectious (severe)") {
            #browser()
            plot <-  add_segments(plot, x= data$fechaDia[1], xend = data$fechaDia[diasDeProyeccion], y = capacidadUTI, yend = capacidadUTI, name = "ICU beds: (100%)", line=list(color="#fc9272", dash="dot"))
            plot <-  add_segments(plot, x= data$fechaDia[1], xend = data$fechaDia[diasDeProyeccion], y = capacidadUTI*porcAsignadoCovid, yend = capacidadUTI*porcAsignadoCovid, name = "ICU beds (70%)", line=list(color="#fc9272", dash="dot"))
            plot <-  add_segments(plot, x= valx, xend = valx, y = 0, yend = max(maxy,capacidadUTI*porcAsignadoCovid*1.1) , name = paste(valx), line=list(color="#bdbdbd"))    
            plot <-  plot %>% layout(xaxis = list(title = "Fecha"), 
                                     yaxis = list(title = paste("Compartimento:",input$compart_a_graficar)))
            
          } else {
            plot <-  add_segments(plot, x= valx, xend = valx, y = 0, yend = maxy, name = paste(valx), line=list(color="#bdbdbd"))    
            plot <- plot %>% layout(xaxis = list(title = "Fecha"), 
                                    yaxis = list(title = paste("Compartimento:",input$compart_a_graficar)))
          }
          
          
        }
        
        
        if (input$check_rt==T) {
          days_before_rt <- c(rep(0,7))
          days_after_rt <- c(rep(NA,nrow(data[is.na(data$Compart),])))
          
          r <- c(days_before_rt,
                 proy()$`Rt: Effective reproduction number`$`Median(R)`,
                 days_after_rt)
          
          plot <- 
          plot %>% add_trace(y = ~r, mode = "dotted", line=list(dash="dot"), type="scatter", yaxis = "y2", name = "Rt") %>%
                   layout(yaxis2 = list(overlaying = "y", side = "right"),
                   xaxis = list(title = "Fecha"),
                   yaxis = list(title = paste("Compartimento:",input$compart_a_graficar)))
          plot %>% add_trace(y=~rep(1,1100), mode = "dotted", line=list(dash="dash", color="#bdbdbd"), type="scatter", yaxis = "y2", name = "Rt = 1")
        } else (plot)
      }
    }
  })
  
  
  res_t <- reactive({
    
    
    input$TSP
    
    data_text <- cbind(data_graf(),rep(fechas_master,length(unique(data_graf()$Compart))))
    colnames(data_text)[ncol(data_text)] <- "fechaDia"
    
    data_text <- data_text %>% dplyr::group_by(Compart) %>%
      dplyr::mutate(ac=cumsum(total))
    
    fechas <- c("2021-06-30",
                "2021-12-31",
                "2022-06-30")
    fechas <<- fechas
    
    tfechas <- c(which(fechas_master == "2021-06-30"),
                 which(fechas_master == "2021-12-31"),
                 which(fechas_master == "2022-06-30"))
    
    def_ac <- data_text$total[(as.character(data_text$fechaDia) %in% fechas) &
                                data_text$Compart=="D"]  
    
    casos_ac <- data_text$ac[(as.character(data_text$fechaDia) %in% fechas) &
                               data_text$Compart=="i"]
    
    vacunas_ac <- data_text$ac[(as.character(data_text$fechaDia) %in% fechas) &
                                 data_text$Compart=="vA"] 
    #browser()
    tFechas <- c(grep(fechas[1], fechas_master),
                 grep(fechas[2], fechas_master),
                 grep(fechas[3], fechas_master))
    
    poblacion_vac <- c(
      
      sum(sapply(proy()[["vA: Daily vaccinations"]][1:tFechas[1]],
                 simplify = T,
                 function (x) {
                   sum(x[3, ])
                 })),
      
      sum(sapply(proy()[["vA: Daily vaccinations"]][1:tFechas[2]],
                 simplify = T,
                 function (x) {
                   sum(x[3, ])
                 })),
      
      sum(sapply(proy()[["vA: Daily vaccinations"]][1:tFechas[3]],
                 simplify = T,
                 function (x) {
                   sum(x[3, ])
                 }))
    ) / sum(N) *100
    
    poblacion_vac2 <- c(
      
      sum(sapply(proy()[["vA: Daily vaccinations"]][1:tFechas[1]],
                 simplify = T,
                 function (x) {
                   sum(x[4, ])
                 })),
      
      sum(sapply(proy()[["vA: Daily vaccinations"]][1:tFechas[2]],
                 simplify = T,
                 function (x) {
                   sum(x[4, ])
                 })),
      
      sum(sapply(proy()[["vA: Daily vaccinations"]][1:tFechas[3]],
                 simplify = T,
                 function (x) {
                   sum(x[4, ])
                 }))
    ) / sum(N) *100

    var=c("Cumulative deaths",
          "Cumulative infections",
          "Vaccines applied",
          "Vaccination coverage dose #1 (%)",
          "Vaccination coverage dose #2 (%)")
    C1 = c(def_ac[1],
           casos_ac[1],
           vacunas_ac[1],
           poblacion_vac[1],
           poblacion_vac2[1])
    
    C2 = c(def_ac[2],
           casos_ac[2],
           vacunas_ac[2],
           poblacion_vac[2],
           poblacion_vac2[2])
    
    C3 = c(def_ac[3],
           casos_ac[3],
           vacunas_ac[3],
           poblacion_vac[3],
           poblacion_vac2[3])
    
    tabla <- cbind(var,
                   format(round(C1,0), big.mark = ','),
                   format(round(C2,0), big.mark = ','),
                   format(round(C3,0), big.mark = ','))
    
    colnames(tabla) <- c(" ",fechas)
    tabla <<- tabla
    
    tabla_scn <<- DT::datatable(tabla,
                                caption = 'Results summary',
                                options = list(ordering=F, 
                                               searching=F, 
                                               paging=F, 
                                               info=F)) %>% formatStyle(' ', `text-align` = 'left') %>%
      formatStyle(fechas[1], `text-align` = 'right') %>%
      formatStyle(fechas[2], `text-align` = 'right') %>%
      formatStyle(fechas[3], `text-align` = 'right') 
    
    tabla_scn
  })
  

  output$resumen_tabla <- renderDataTable({
    #browser()
    res_t()})

  observe({
    if (str_trim(input$save_comp_name)=="") {
      disable("save_comp")
    } else {
      enable("save_comp")
    }
    
  })
  
  observeEvent(input$del_scenarios,{
    compare <<-compare[compare$Compart=="",]
    showNotification("Scenarios deleted", type = "warning")
    comp_table <<- list()
    output_list
    hide("tables")
    output_list <<- c() 
    hide("graficoComp")
    hide("del_scenarios")
    hide("icu_beds")
    updateSelectInput(session,"saved_series",choices = "", selected = "")
    res_t()
    delete <<- T
  }
  )
  

  setDefaultParams <- function() {
    updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    updateSelectInput(session, "vacUptake", selected = "Mid-range uptake: 50%")
    updateSelectInput(session, "vacDateGoal", selected = "2021-12-31")
    updateSelectInput(session, "vacStrat", selected = "Priority: older -> adults -> young")
    updateSelectInput(session, "vacEfficacy", selected = "B2. 100%, 80%, 50%")
    # updateSelectInput(session, "immunityDuration", selected = 180)
    updateSelectInput(session, "npiScenario", selected = "Combined with schools closed")
    updateSelectInput(session, "npiScenarioRelaxed", selected = "Combined with schools closed")
    updateSelectInput(session, "relaxationThreshold", selected = "No relaxation")
    updateRadioButtons(session, "npiStrat", selected = "cont")
    updateSelectInput(session, "relaxationDateGoal", selected = "2021-06-30")
    updateSliderInput(session, "relaxationFactor", value = 0)
    updateSliderInput(session, "ajusta_beta", value = .6)
  }
  
  observeEvent(input$q1_older,{
    setDefaultParams()
    if (input$simpleCompartSelector2q1 == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector2q1 == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector2q1 == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    updateSelectInput(session, "vacStrat", selected = "Priority: older -> adults -> young")
    updateTextInput(session, "save_comp_name", value="Older population first")
  })
  observeEvent(input$q1_adult,{
    setDefaultParams()
    if (input$simpleCompartSelector2q1 == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector2q1 == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector2q1 == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    updateSelectInput(session, "vacStrat", selected = "Priority: adults -> older -> young")
    updateTextInput(session, "save_comp_name", value="Adult population first")
  })
  observeEvent(input$q1_school,{
    setDefaultParams()
    if (input$simpleCompartSelector2q1 == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector2q1 == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector2q1 == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    updateSelectInput(session, "vacStrat", selected = "Priority: school age -> others")
    updateTextInput(session, "save_comp_name", value="School aged population first")
  })
  observeEvent(input$q2_80_low,{
    setDefaultParams()
    if (input$simpleCompartSelector2q2 == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector2q2 == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector2q2 == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    updateSelectInput(session, "vacUptake", selected = "High uptake: 80%")
    updateSelectInput(session, "npiScenario", selected = "Intensive interventions with schools open")
    updateTextInput(session, "save_comp_name", value="Uptake 80%, Intensive interventions with schools open")
  })
  observeEvent(input$q2_80_mid,{
    setDefaultParams()
    if (input$simpleCompartSelector2q2 == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector2q2 == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector2q2 == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    updateSelectInput(session, "vacUptake", selected = "High uptake: 80%")
    updateSelectInput(session, "npiScenario", selected = "Intensive interventions with schools closed")
    updateTextInput(session, "save_comp_name", value="Uptake 80%, Intensive interventions with schools closed")
  })
  observeEvent(input$q2_80_high,{
    setDefaultParams()
    if (input$simpleCompartSelector2q2 == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector2q2 == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector2q2 == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    updateSelectInput(session, "vacUptake", selected = "High uptake: 80%")
    updateSelectInput(session, "npiScenario", selected = "Combined with schools closed")
    updateTextInput(session, "save_comp_name", value="Uptake 80%, Combined with schools closed")
  })
  observeEvent(input$q2_50_low,{
    setDefaultParams()
    if (input$simpleCompartSelector2q2 == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector2q2 == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector2q2 == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    updateSelectInput(session, "vacUptake", selected = "Mid-range uptake: 50%")
    updateSelectInput(session, "npiScenario", selected = "Intensive interventions with schools open")
    updateTextInput(session, "save_comp_name", value="Uptake 50%, Intensive interventions with schools open")
  })
  observeEvent(input$q2_50_mid,{
    setDefaultParams()
    if (input$simpleCompartSelector2q2 == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector2q2 == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector2q2 == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    updateSelectInput(session, "vacUptake", selected = "Mid-range uptake: 50%")
    updateSelectInput(session, "npiScenario", selected = "Intensive interventions with schools closed")
    updateTextInput(session, "save_comp_name", value="Uptake 50%, Intensive interventions with schools closed")
  })
  observeEvent(input$q2_50_high,{
    setDefaultParams()
    if (input$simpleCompartSelector2q2 == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector2q2 == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector2q2 == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    updateSelectInput(session, "vacUptake", selected = "Mid-range uptake: 50%")
    updateSelectInput(session, "npiScenario", selected = "Combined with schools closed")
    updateTextInput(session, "save_comp_name", value="Uptake 50%, Combined with schools closed")
  })
  observeEvent(input$q2_20_low,{
    setDefaultParams()
    if (input$simpleCompartSelector2q2 == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector2q2 == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector2q2 == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    updateSelectInput(session, "vacUptake", selected = "Low uptake: 20%")
    updateSelectInput(session, "npiScenario", selected = "Intensive interventions with schools open")
    updateTextInput(session, "save_comp_name", value="Uptake 20%, Intensive interventions with schools open")
  })
  observeEvent(input$q2_20_mid,{
    setDefaultParams()
    if (input$simpleCompartSelector2q2 == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector2q2 == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector2q2 == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    updateSelectInput(session, "vacUptake", selected = "Low uptake: 20%")
    updateSelectInput(session, "npiScenario", selected = "Intensive interventions with schools closed")
    updateTextInput(session, "save_comp_name", value="Uptake 20%, Intensive interventions with schools closed")
  })
  observeEvent(input$q2_20_high,{
    setDefaultParams()
    if (input$simpleCompartSelector2q2 == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector2q2 == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector2q2 == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    updateSelectInput(session, "vacUptake", selected = "Low uptake: 20%")
    updateSelectInput(session, "npiScenario", selected = "Combined with schools closed")
    updateTextInput(session, "save_comp_name", value="Uptake 20%, Combined with schools closed")
  })
  observeEvent(input$t4q1_project,{
    setDefaultParams()
    if (input$simpleCompartSelector4q == "i") {
      updateSelectInput(session, "compart_a_graficar", selected = "i: Daily infectious")
    } else if (input$simpleCompartSelector4q == "Ic") {
      updateSelectInput(session, "compart_a_graficar", selected = "Ic: Infectious (severe)")
    } else if (input$simpleCompartSelector4q == "d") {
      updateSelectInput(session, "compart_a_graficar", selected = "d: Daily deaths")
    }
    if (input$t4q1_uptake == "High") {
      updateSelectInput(session, "vacUptake", selected = "High uptake: 80%")
    } else if (input$t4q1_uptake == "Middle") {
      updateSelectInput(session, "vacUptake", selected = "Mid-range uptake: 50%")
    } else if (input$t4q1_uptake == "Low") {
      updateSelectInput(session, "vacUptake", selected = "Low uptake: 20%")
    }
    if (input$t4q1_efficacy == "High") {
      updateSelectInput(session, "vacEfficacy", selected = "B1. 100%, 80%, 80%")
    } else if (input$t4q1_efficacy == "Middle") {
      updateSelectInput(session, "vacEfficacy", selected = "C1. 80%, 80%, 50%")
    } else if (input$t4q1_efficacy == "Low") {
      updateSelectInput(session, "vacEfficacy", selected = "C2. 80%, 50%, 50%")
    }
    if (input$t4q1_priority == "Older -> adults -> young") {
      updateSelectInput(session, "vacStrat", selected = "Priority: older -> adults -> young")
    } else if (input$t4q1_priority == "Adults -> older -> young") {
      updateSelectInput(session, "vacStrat", selected = "Priority: adults -> older -> young")
    } else if (input$t4q1_priority == "Young -> others") {
      updateSelectInput(session, "vacStrat", selected = "Priority: school age -> others")
    }
    if (input$t4q1_npis == "High") {
      updateSelectInput(session, "npiScenario", selected = "Combined with schools closed")
    } else if (input$t4q1_npis == "Middle") {
      updateSelectInput(session, "npiScenario", selected = "Intensive interventions with schools closed")
    } else if (input$t4q1_npis == "Low") {
      updateSelectInput(session, "npiScenario", selected = "Intensive interventions with schools open")
    }
    if (input$t4q1_npisRelaxThreshold == "No relaxation") {
      updateSelectInput(session, "relaxationThreshold", selected = 1.1)
    } else if (input$t4q1_npisRelaxThreshold == "80%") {
      updateSelectInput(session, "relaxationThreshold", selected = .8)
    } else if (input$t4q1_npisRelaxThreshold == "50%") {
      updateSelectInput(session, "relaxationThreshold", selected = .5)
    } else if (input$t4q1_npisRelaxThreshold == "40%") {
      updateSelectInput(session, "relaxationThreshold", selected = .4)
    }
    if (input$t4q1_npisRelaxed == "High") {
      updateSelectInput(session, "npiScenarioRelaxed", selected = "Combined with schools closed")
    } else if (input$t4q1_npisRelaxed == "Middle") {
      updateSelectInput(session, "npiScenarioRelaxed", selected = "Intensive interventions with schools closed")
    } else if (input$t4q1_npisRelaxed == "Low") {
      updateSelectInput(session, "npiScenarioRelaxed", selected = "Intensive interventions with schools open")
    }
    saveLabel = paste("Coverage",input$t4q1_uptake,", Efficacy",input$t4q1_efficacy,", NPIs",input$t4q1_npis,", Priority",input$t4q1_priority)
    if (input$t4q1_npisRelaxThreshold != "No relaxation") {
      saveLabel = paste(saveLabel,", Relaxed at",input$t4q1_npisRelaxThreshold)
    }
    updateTextInput(session, "save_comp_name",value=saveLabel)
  })
  
  observeEvent(input$save_comp, {
    
    input$TSP
    show("tables")
    if (input$save_comp_name %in% output_list) {showNotification("Duplicated scenario name", type="error")} else {
      show("del_scenarios")
      show("graficoComp")
      df <- data_graf() %>% dplyr::filter(Compart==str_trim(str_replace_all(substring(input$compart_a_graficar,1,3),":",""))) %>%
        dplyr::mutate(Compart=input$save_comp_name,
                      Compart_label=input$compart_a_graficar)
      df$fechaDia=fechas_master
      compare <<- if (exists("compare")==F) {compare<<-df} else {union_all(compare,df)}
      updateSelectInput(session, "saved_series", choices = unique(compare$Compart), selected = unique(compare$Compart))
      showNotification("Saved!", type = "message")
      
      
      #browser()
      
      comp_table[[input$save_comp_name]] <<- DT::datatable(tabla,
                                                           caption = input$save_comp_name,
                                                           options = list(ordering=F, 
                                                                          searching=F, 
                                                                          paging=F, 
                                                                          info=F)) %>% formatStyle(' ', `text-align` = 'left') %>%
                                                                                       formatStyle(fechas[1], `text-align` = 'right') %>%
                                                                                       formatStyle(fechas[2], `text-align` = 'right') %>%
                                                                                       formatStyle(fechas[3], `text-align` = 'right') 
        
        table <- comp_table[[input$save_comp_name]]
        output_list <<- unique(c(output_list,names(comp_table[input$save_comp_name])))
        
        eval(parse(text=
                     paste0("output$`",input$save_comp_name,"` <<- renderDataTable({table})")

        ))
    
        output$tables <<- renderUI({
          eval(parse(text=
                       paste0("tagList(fluidRow(",paste0("column(4,DTOutput('",output_list,"'))", collapse = ','),"))")
                     
          ))    
    
        })
    }
    
    
      
    
  })
  
  output$graficoComp <- renderPlotly({
    
    input$TSP
    if (exists("compare")) {
      if (nrow(compare)>0) {
        data_comp <-  compare %>% dplyr::filter(Compart %in% input$saved_series)
        data_comp[data_comp<0] = 0
        plot_comp <- plot_ly(name=paste0(
                                         " [",
                                         str_replace_all(trimws(substring(unique(data_comp$Compart_label),1,3), "right"),":",""), 
                                         "]" 
                                        ) 
                     ) %>% layout(showlegend = TRUE)
        lapply(unique(data_comp$Compart), function(k) {
          
          plot_comp <<- add_trace(plot_comp,
                                  data_comp[data_comp$Compart==k,],
                                  x=~data_comp$fechaDia[data_comp$Compart==k],
                                  y=~data_comp$total[data_comp$Compart==k], 
                                  
                                  type="scatter", mode="lines",
                                  name=paste0(k,
                                              " [",
                                              str_replace_all(trimws(substring(unique(data_comp$Compart_label[data_comp$Compart==k]),1,3), "right"),":",""), 
                                              "]" 
                                       )
                                  )
          
        })
        
        if (input$icu_beds==T & nrow(data_comp)>0) {
          
          plot_comp <-  plot_comp %>% layout(xaxis = list(title="Date"), yaxis = list(title="Value"))    
          plot_comp <-  add_segments(plot_comp, x= data_comp$fechaDia[1], xend = max(data_comp$fechaDia), y = capacidadUTI, yend = capacidadUTI, name = "ICU beds: (100%)", line=list(color="#fc9272", dash="dot"))
          plot_comp <-  add_segments(plot_comp, x= data_comp$fechaDia[1], xend = max(data_comp$fechaDia), y = capacidadUTI*porcAsignadoCovid, yend = capacidadUTI*porcAsignadoCovid, name = "ICU beds (70%)", line=list(color="#fc9272", dash="dot"))
        }
        plot_comp %>% layout(xaxis = list(title="Date"), yaxis = list(title="Value"))
      }
  } else {NULL}
  })
  

}

shinyApp(ui = getUI(), server = server)
