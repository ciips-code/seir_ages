library(tidyr)
library(shiny)
library(dplyr)
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
library(shinyjs)
library(modelr)

rm(list = ls())

# carga RData

load("DatosIniciales_ARG.RData", envir = .GlobalEnv)
rm(list=setdiff(ls(), c("modeloSimulado",
                        "duracionMediaInf",
                        "diasHospCasosCriticos",
                        "diasHospCasosGraves",
                        "periodoPreinfPromedio",
                        "dataEcdc",
                        "porcentajeCasosGraves",
                        "porcentajeCasosCriticos")))


# lee funciones
source("functions/seirAges_matrices.R", encoding = "UTF-8")
source("functions/vacunas.R", encoding = "UTF-8")
diasDeProyeccion = 1100
ifr = c(0.003,0.0035,0.005,0.008,0.02)
primeraVez = paramVac_primeraVez = ifr_primeraVez = transprob_primeraVez = mbeta_primeraVez = mgraves_primeraVez = mcriticos_primeraVez = mifr_primeraVez = TRUE
immunityStates <<- c("No immunity", "Recovered", "Vaccinated")
ageGroups <<- c("0-17", "18-49", "50-59", "60-69", "70+")
ageGroupsV <<- c("00","18","50","60","70")
# crea matrices de contacto y efectividad
contact_matrix = matrix(c(5,1,1,1,1,
                          2,4,4,4,4,
                          2,4,4,4,4,
                          2,4,4,4,4,
                          .5,1,1,1,5),5,byrow = T)
colnames(contact_matrix) = rownames(contact_matrix) = ageGroups
transmission_probability = matrix(c(0.003,0.003,0.003,0.003,0.003,
                                    0.048,0.048,0.048,0.048,0.048,
                                    0.048,0.048,0.048,0.048,0.048,
                                    0.048,0.048,0.048,0.048,0.048,
                                    0.034,0.034,0.034,0.034,0.034),length(ageGroups),length(ageGroups),byrow = T)
# transmission_probability = transmission_probability * 0.43
colnames(transmission_probability) = rownames(transmission_probability) = ageGroups

# datos de poblacion ejemplo Argentina
N = c(13150705,20713779,4381897,3560538,3569844)

# datos de recursos
capacidadUTI <- 11517
porcAsignadoCovid <- .7


# prepara datos reportados
load("data/dataARG.RData", envir = .GlobalEnv)
source("update.R", encoding = "UTF-8")
dataPorEdad = formatData("ARG", ageGroupsV)

diaCeroVac <- min(dataPorEdad$FMTD$vac$fecha)
tVacunasCero <-  as.numeric(as.Date(diaCeroVac)-min(dataPorEdad$FMTD$def$fecha))
vacPre = lapply(1:(as.numeric(tVacunasCero)-1), matrix, data=0,
                                                        nrow=length(immunityStates),
                                                        ncol=length(ageGroups))

vacArg = lapply(1:nrow(dataPorEdad$FMTD$vac), matrix,  data=0,
                                             nrow=length(immunityStates),
                                             ncol=length(ageGroups))

for (t in 1:length(vacArg)) {
  # TODO: Expandir a otras vacunas
  vacArg[[t]][3,]  = as.numeric(dataPorEdad$FMTD$vac[t,2:ncol(dataPorEdad$FMTD$vac)])
}

promedio = round(Reduce("+", vacArg) / length(vacArg),0)
vacPlan = lapply(1:(diasDeProyeccion-length(vacArg)-length(vacPre)), matrix, data=t(promedio),
                 nrow=length(immunityStates),
                 ncol=length(ageGroups))

Av = c(vacPre,vacArg,vacPlan)
AvArg <<- Av
vacPlanDia <- length(vacPre)+length(vacArg)

# t <- seq(1:nrow(dataEcdc))
temp <- lapply(colnames(dataPorEdad$FMTD$def)[-1], function(loopCol) {
  loessCol = paste0(loopCol,'_loess')
  dataPorEdad$FMTD$def[loessCol] <<- predict(loess(dataPorEdad$FMTD$def[loopCol][,1]~seq(1,nrow(dataPorEdad$FMTD$def), by=1),span=.2))
})
rm(temp)

# muertes
loessCols = which(colnames(dataPorEdad$FMTD$def) %in% grep("loess",colnames(dataPorEdad$FMTD$def), value = TRUE))
def_p <- dataPorEdad$FMTD$def[,loessCols]
def_p <- def_p[1:(nrow(def_p)-15),]
fechas_master = seq(min(dataPorEdad$FMTD$def$fecha),
                    min(dataPorEdad$FMTD$def$fecha)+diasDeProyeccion-1,by=1)
names = list(immunityStates,
             ageGroups)
modif_beta <- matrix(rep(c(1,1,.6),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
modif_porc_gr <- matrix(rep(c(1,.3,.1),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
modif_porc_cr <- matrix(rep(c(1,.1,.03),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
modif_ifr <- matrix(rep(c(1,.05,.01),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
duracion_inmunidad = 180

namesVac = list(immunityStates,
                c("latencia", "porcV", "tiempoV", "porcProt", "tiempoP"))

paramVac <- matrix(data=c(0,0,0,0,0,
                          0,0,0,0,0,
                          20,.2,180,.6,180), nrow=length(immunityStates), ncol=5, byrow=T, dimnames = namesVac)

ui <- fluidPage(theme = bs_theme(bootswatch = "sandstone"),
                useShinyjs(),
                fluidRow(id="inputs", 
                         column(width = 4, offset = 2,
                                numericInput("t", label = NULL, value = 1, step = 1)
                                , align="right"),
                         column(width = 1,
                                actionButton("submit", label = NULL, icon = icon("refresh"))
                                , align="center"),
                         column(width = 1,
                                actionButton("prev", label = NULL, icon = icon("chevron-left"))
                                , align="center"),
                         column(width = 1,
                                actionButton("prox", label = NULL, icon = icon("chevron-right"))
                                , align="center")),
                tabsetPanel(type = "tabs",
                            tabPanel("Graphs",
                                     br(),
                                     fluidRow(column(3,selectInput("compart_a_graficar","Compartment",choices = NULL)),
                                              column(2,selectInput("edad","Age groups",
                                                                   choices=c("All ages"="total",ageGroups),
                                                                   multiple = T,
                                                                   selected= c("total")))
                                              ),
                                     plotlyOutput("graficoUnico"),
                                     tabsetPanel(type = "tabs",
                                       tabPanel("Model",
                                         fluidRow(
                                           column(12,
                                             fluidRow(
                                               column(6,
                                                  fluidRow(
                                                    column(4,
                                                          numericInput("diasProy",
                                                          "Days to display",
                                                          min=30,
                                                          max=diasDeProyeccion,
                                                          value = diasDeProyeccion)),
                                                    column(4,numericInput("duracionInm",
                                                           "Immunity duration",
                                                           min=1,
                                                           max=360,
                                                           value = 180))
                                                  ),
                                                  fluidRow(
                                                    column(12,
                                                           DT::dataTableOutput("transprob")
                                                    )
                                                  )
                                               ),
                                              column(6,
                                                     DT::dataTableOutput("ifrt"),
                                                     )
                                             )
                                           )
                                         )
                                       ),
                                       tabPanel("Scenarios",
                                                fluidRow(column(3,
                                                                selectInput(
                                                                  "vacUptake",
                                                                  label="Vaccination uptake",
                                                                  choices = c("Current uptake",
                                                                              "High uptake: 95%",
                                                                              "High uptake: 80%",
                                                                              "Mid-range uptake: 50%",
                                                                              "Low uptake: 20%",
                                                                              "No vaccination")
                                                                ),
                                                                selectInput(
                                                                  "vacDateGoal",
                                                                  label="Vaccination date goal",
                                                                  choices = c("Mid 2021"="2021-06-30",
                                                                              "End 2021"="2021-12-31",
                                                                              "Mid 2022"="2022-05-30",
                                                                              "End 2022"="2022-12-31")
                                                                ),
                                                                selectInput(
                                                                  "vacStrat",
                                                                  label="Vaccination priorities",
                                                                  choices = c("Current priorities",
                                                                              "Priority: older -> adults -> young",
                                                                              "Priority: older + adults -> young",
                                                                              "Priority: adults -> older -> young",
                                                                              "No priorities")
                                                                  )
                                                                ),
                                                         column(3,
                                                                radioButtons("npiStrat", "NPI strategy:",
                                                                             c("Continued NPIs" = "cont",
                                                                               "Relaxation of NPIs" = "relax")),
                                                                selectInput(
                                                                  "relaxationDateGoal",
                                                                  label="Relaxation date goal",
                                                                  choices = c("Mid 2021"="2021-06-30",
                                                                              "End 2021"="2021-12-31",
                                                                              "Mid 2022"="2022-05-30",
                                                                              "End 2022"="2022-12-31")
                                                                ),
                                                                sliderInput("relaxationFactor",
                                                                            "NPI Relaxation factor",
                                                                            min=0, #.3,
                                                                            max=.5, #.5,
                                                                            value=0, #.40,
                                                                            step=.05),
                                                                sliderInput("ajusta_beta",
                                                                              "Base NPI strength modifier",
                                                                              min=-1, #.3,
                                                                              max=1, #.5,
                                                                              value=0, #.40,
                                                                              step=.1)),
                                                         column(3,p("Results:"),
                                                                uiOutput("resument_text")),
                                                         column(3,DT::dataTableOutput("paramVac"))
                                                         ),
                                                fluidRow(
                                                  column(3,DT::dataTableOutput("mbeta")),
                                                  column(3,DT::dataTableOutput("mgraves")),
                                                  column(3,DT::dataTableOutput("mcriticos")),
                                                  column(3,DT::dataTableOutput("mifr"))
                                                )
                                                )
                                       
                                       
                                       
                            )),
                            tabPanel("Compartments", fluidRow(id="content"))
                ),
                fluidRow(column(12,id="content")),
                br(),
                br(),
                br(),
                br(),
                br()
)

                

server <- function (input, output, session) {
  
  
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
                      as.numeric(input$paramVac_cell_edit[2])] <- as.numeric(input$paramVac_cell_edit[3])
        paramVac_edit <<- paramVac_edit  
    }
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
    # paste activa reactive (no comentar)
    paste(input$paramVac_cell_edit)
    paste(input$ifrt_cell_edit)
    paste(input$transprob_cell_edit)
    paste(input$mbeta_cell_edit)
    paste(input$mgraves_cell_edit)
    paste(input$mcriticos_cell_edit)
    paste(input$mifr_cell_edit)
    
    duracion_inmunidad = input$duracionInm
    
    # print(input$vacDateGoal)
    # print(input$vacUptake)
    # print(input$vacStrat)
    # N, diaCeroVac, as.Date("2022-01-01"), tVacunasCero, AvArg
    # browser()
    if (input$vacUptake == "Current uptake" | input$vacUptake == "No vaccination") {
      disable("vacDateGoal")
    } else {
      enable("vacDateGoal")
    }
    
    AvArgParam <- generaEscenarioSage(input$vacUptake, input$vacDateGoal, input$vacStrat,
                                      AvArg, N, tVacunasCero, diaCeroVac)
    
    AvArgParam <<- AvArgParam 
    
    ajuste = (((input$ajusta_beta*-1) + 1)/10)+0.3
    trans_prob_param <- transprob_edit * ajuste
    
    relaxNpi = FALSE
    relaxGoal = NULL
    if (input$npiStrat == "cont") {
      disable("relaxationDateGoal")
      disable("relaxationFactor")
    } else {
      enable("relaxationDateGoal")
      enable("relaxationFactor")
      relaxNpi = TRUE
      relaxGoal = which(fechas_master == input$relaxationDateGoal)
    }
    
    proy <- seir_ages(dias=diasDeProyeccion,
              duracionE = periodoPreinfPromedio,
              duracionIi = duracionMediaInf,
              porc_gr = porcentajeCasosGraves,
              porc_cr = porcentajeCasosCriticos,
              duracionIg = diasHospCasosGraves,
              duracionIc = diasHospCasosCriticos,
              ifr = ifr_edit[1,],
              contact_matrix = contact_matrix,
              transmission_probability = trans_prob_param,
              N = N,
              defunciones_reales=def_p,
              modif_beta=modif_beta,
              modif_porc_gr=modif_porc_gr,
              modif_porc_cr=modif_porc_cr,
              modif_ifr=modif_ifr,
              Av=AvArgParam,
              immunityStates=immunityStates,
              ageGroups=ageGroups,
              paramVac=paramVac_edit,
              duracion_inmunidad=duracion_inmunidad,
              tVacunasCero=tVacunasCero,
              relaxNpi=relaxNpi,
              relaxGoal=relaxGoal,
              relaxFactor=input$relaxationFactor
    )
    
    proy$AvArg=AvArgParam
    return(proy)
    
  })
  observe({
    if (primeraVez) {
      updateSelectInput(session, "compart_a_graficar", choices = c(names(proy()),"pV"), selected="i")
      updateNumericInput(session, inputId = "t", value = tHoy)
      primeraVez <<- FALSE
    }
    t = input$t
    # Actualiza tab pane de tablas de comapartimentos
    lapply(
      X = comp,
      FUN = function(c){
        output[[c]] <- renderDT(round(proy()[[c]][[t]],0), editable = F,rownames = T, options = list(paging = FALSE, info = FALSE, searching = FALSE, fixedColumns = TRUE,autoWidth = TRUE,ordering = FALSE,dom = 'Bfrtip'))
      }
    )
  })
  comp <- rev(c("S","V","E","e","I","i","Ig","Ic","U","u","D","d"))
  newUis <- c()
  for (c in comp) {
    insertUI("#content", "afterEnd",
             column(6,fluidRow(column(1,p(c), align="center"),
                      column(11,
                             DTOutput(c)
                             , align="center")
                      )),
             immediate = TRUE
    )
  }
  
  observeEvent(input$prev, {
    # updateTables(input$t - 1)
    updateNumericInput(session,"t", value =  input$t - 1)
  })
  
  observeEvent(input$prox, {
    # updateTables(input$t + 1)
    updateNumericInput(session,"t", value =  input$t + 1)
  })
  
  data_graf <- reactive({
    
    proy <- proy()
    data_graf <- bind_rows(
      tibble(Compart = "S", do.call(rbind, lapply(proy$S,colSums)) %>% as_tibble()),
      tibble(Compart = "V", do.call(rbind, lapply(proy$V,colSums)) %>% as_tibble()),
      tibble(Compart = "v", do.call(rbind, lapply(proy$v,colSums)) %>% as_tibble()),
      tibble(Compart = "vA", do.call(rbind, lapply(proy$vA,colSums)) %>% as_tibble()),
      tibble(Compart = "E", do.call(rbind, lapply(proy$E,colSums)) %>% as_tibble()),
      tibble(Compart = "e", do.call(rbind, lapply(proy$e,colSums)) %>% as_tibble()),
      tibble(Compart = "I", do.call(rbind, lapply(proy$I,colSums)) %>% as_tibble()),
      tibble(Compart = "Ii", do.call(rbind, lapply(proy$Ii,colSums)) %>% as_tibble()),
      tibble(Compart = "Ic", do.call(rbind, lapply(proy$Ic,colSums)) %>% as_tibble()),
      tibble(Compart = "Ig", do.call(rbind, lapply(proy$Ig,colSums)) %>% as_tibble()),
      tibble(Compart = "i", do.call(rbind, lapply(proy$i,colSums)) %>% as_tibble()),
      tibble(Compart = "D", do.call(rbind, lapply(proy$D,colSums)) %>% as_tibble()),
      tibble(Compart = "d", do.call(rbind, lapply(proy$d,colSums)) %>% as_tibble()),
      tibble(Compart = "R", do.call(rbind, lapply(proy$R,colSums)) %>% as_tibble()),
      tibble(Compart = "U", do.call(rbind, lapply(proy$U,colSums)) %>% as_tibble()),
      tibble(Compart = "u", do.call(rbind, lapply(proy$u,colSums)) %>% as_tibble()),
      tibble(Compart = "tot", do.call(rbind, lapply(proy$tot,colSums)) %>% as_tibble()),
      tibble(Compart = "pV", do.call(rbind, lapply(AvArgParam,colSums)) %>% as_tibble())) %>%
      dplyr::mutate(fecha = rep(1:length(proy$S),18)) %>%
      # TODO: Arreglar
      dplyr::rename("0-17"=2, "18-49"=3, "50-59"=4, "60-69"=5, "70+"=6)
    data_graf$total=data_graf$`0-17`+data_graf$`18-49`+data_graf$`50-59`+data_graf$`60-69`+data_graf$`70+`
    data_graf
  })
  
  output$graficoUnico <- renderPlotly({
    if (length(proy()) > 0 & input$compart_a_graficar != "") {
      dataTemp = data_graf() %>% dplyr::filter(Compart == input$compart_a_graficar)
      dataTemp$fechaDia = fechas_master
      dataTemp <- dataTemp 
      dataTemp  
      # dataTemp$fechaDia = seq(min(dataEcdc$dateRep),min(dataEcdc$dateRep)+diasDeProyeccion-1,by=1)
      valx = dataTemp$fechaDia[input$t]
      maxy = max(dataTemp$total)
      
      if (is.null(input$edad)==F & is.na(input$diasProy)==F) {
          data=dataTemp[1:(input$t+input$diasProy),]
          plot=plot_ly(data=data, x=~fechaDia)          
          
          if (length(input$edad)>0) {
            lapply(X=input$edad, FUN = function(edad) {
              plot <<- add_trace(plot, y=~eval(parse(text=paste0('`',edad,'`'))), type="scatter", mode="lines", name=edad, line = list(dash = ifelse(edad=='total','','dot')))
            })
          
            if (input$compart_a_graficar == "i") {
              plot <-  add_segments(plot, x= valx, xend = valx, y = 0, yend = maxy, name = paste(valx), line=list(color="#bdbdbd"))
              plot %>% layout(xaxis = list(title = "Fecha"),
                              yaxis = list(title = paste("Compartimento:",input$compart_a_graficar)))
              # , range = c(0,60000)
            } else if (input$compart_a_graficar == "Ic") {
              #browser()
              plot <-  add_segments(plot, x= data$fechaDia[1], xend = data$fechaDia[diasDeProyeccion], y = capacidadUTI*porcAsignadoCovid, yend = capacidadUTI*porcAsignadoCovid, name = "ICU beds", line=list(color="#fc9272", dash="dot"))
              plot <-  add_segments(plot, x= valx, xend = valx, y = 0, yend = max(maxy,capacidadUTI*porcAsignadoCovid*1.1) , name = paste(valx), line=list(color="#bdbdbd"))    
              plot %>% layout(xaxis = list(title = "Fecha"), 
                              yaxis = list(title = paste("Compartimento:",input$compart_a_graficar)))
              
            } else {
              plot <-  add_segments(plot, x= valx, xend = valx, y = 0, yend = maxy, name = paste(valx), line=list(color="#bdbdbd"))    
              plot %>% layout(xaxis = list(title = "Fecha"), 
                              yaxis = list(title = paste("Compartimento:",input$compart_a_graficar)))
            }
          }
          
          
          # plot <- plot_ly(data=data, x=~fechaDia,
          #         y=~eval(parse(text=paste0('`',input$edad,'`'))),
          #         type="scatter", mode="lines", name = paste("Valor de",input$compart_a_graficar)) %>%
          #         add_segments(x= valx, xend = valx, y = 0, yend = maxy, name = paste("t"))
          # 
          
          
          
      }
      
    }
    
  })
  output$resument_text <- renderText({
    data_text <- cbind(data_graf(),rep(fechas_master,length(unique(data_graf()$Compart))))
    colnames(data_text)[ncol(data_text)] <- "fechaDia"
    
    data_text <- data_text %>% dplyr::group_by(Compart) %>%
                               dplyr::mutate(ac=cumsum(total))
    
    fechas <- c("2021-12-31",
                "2022-06-30",
                "2022-12-31")
    
    def_ac <- data_text$total[(as.character(data_text$fechaDia) %in% fechas) &
                               data_text$Compart=="D"]  
    
    casos_ac <- data_text$ac[(as.character(data_text$fechaDia) %in% fechas) &
                              data_text$Compart=="i"]
    
    vacunas_ac <- data_text$ac[(as.character(data_text$fechaDia) %in% fechas) &
                                data_text$Compart=="vA"]
    
    poblacion_vac <- data_text$ac[(as.character(data_text$fechaDia) %in% fechas) &
                                   data_text$Compart=="vA"] / sum(N) * 100
    
    
    
    HTML("Defunciones acumuladas al ", "<b>",fechas[1],"</b>", ": ", format(round(def_ac[1],0), big.mark = ',', decimal.mark = '.'), "<br/>",
         "Defunciones acumuladas al ", fechas[2],": ", format(round(def_ac[2],0), big.mark = ',', decimal.mark = '.'), "<br/>",
         "Defunciones acumuladas al ", fechas[3],": ", format(round(def_ac[3],0), big.mark = ',', decimal.mark = '.'), "<br/>",
         "Casos acumulados al ", fechas[1],": ", format(round(casos_ac[1],0), big.mark = ',', decimal.mark = '.'), "<br/>",
         "Casos acumulados al ", fechas[2],": ", format(round(casos_ac[2],0), big.mark = ',', decimal.mark = '.'), "<br/>",
         "Casos acumulados al ", fechas[3],": ", format(round(casos_ac[3],0), big.mark = ',', decimal.mark = '.'), "<br/>",
         "Vacunas aplicadas al ", fechas[1],": ", format(round(vacunas_ac[1],0), big.mark = ',', decimal.mark = '.'), "<br/>",
         "Vacunas aplicadas al ", fechas[2],": ", format(round(vacunas_ac[2],0), big.mark = ',', decimal.mark = '.'), "<br/>",
         "Vacunas aplicadas al ", fechas[3],": ", format(round(vacunas_ac[3],0), big.mark = ',', decimal.mark = '.'), "<br/>",
         "Población vacunada al ", fechas[1],": ", format(round(poblacion_vac[1],1), big.mark = ',', decimal.mark = '.'), "% <br/>",
         "Población vacunada al ", fechas[2],": ", format(round(poblacion_vac[2],1), big.mark = ',', decimal.mark = '.'), "% <br/>",
         "Población vacunada al ", fechas[3],": ", format(round(poblacion_vac[3],1), big.mark = ',', decimal.mark = '.'), "% <br/>"
    )
         

    
  })
}

shinyApp(ui = ui, server = server)
