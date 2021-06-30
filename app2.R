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
library(covoid)
library(shinyjs)
library(modelr)
library(stringr)

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
ifr = c(0.003,0.0035,0.0035,0.0035,0.005,0.008,0.02)
primeraVez = paramVac_primeraVez = ifr_primeraVez = transprob_primeraVez = mbeta_primeraVez = mgraves_primeraVez = mcriticos_primeraVez = mifr_primeraVez = TRUE
# crea matrices de contacto y efectividad - set TRUE si queremos observada
use_empirical_mc = TRUE
immunityStates <<- c("No immunity", "Recovered", "Vaccinated")
ageGroups <<- c("0-17", "18-29", "30-39", "40-49","50-59", "60-69", "70+")
ageGroupsV <<- c("00","18","30","40","50", "60", "70")
# crea matrices de contacto y efectividad
contact_matrix = matrix(c(5,1,1,1,1,1,1,
                          2,4,4,4,4,4,4,
                          2,4,4,4,4,4,4,
                          2,4,4,4,4,4,4,
                          2,4,4,4,4,4,4,
                          2,4,4,4,4,4,4,
                         .5,1,1,1,1,1,5),7,byrow = T)
colnames(contact_matrix) = rownames(contact_matrix) = ageGroups
transmission_probability = matrix(c(0.003,0.003,0.003,0.003,0.003,0.003,0.003,
                                    0.048,0.048,0.048,0.048,0.048,0.048,0.048,
                                    0.048,0.048,0.048,0.048,0.048,0.048,0.048,
                                    0.048,0.048,0.048,0.048,0.048,0.048,0.048,
                                    0.048,0.048,0.048,0.048,0.048,0.048,0.048,
                                    0.048,0.048,0.048,0.048,0.048,0.048,0.048,
                                    0.034,0.034,0.034,0.034,0.034,0.034,0.034),length(ageGroups),length(ageGroups),byrow = T)
if(use_empirical_mc){
  contact_matrix <- get_empirical_cm(ages=c(0,20,30,40,50,60,70))
  colnames(contact_matrix) = rownames(contact_matrix) = ageGroups
  transmission_probability = transmission_probability * 2.2 # a ojo
}


# transmission_probability = transmission_probability * 0.43
colnames(transmission_probability) = rownames(transmission_probability) = ageGroups

# datos de poblacion ejemplo Argentina
N = c(13150705,
      8487490,
      6482663,
      5743626,
      4381897,
      3560538,
      3569844)

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
names <- list(immunityStates,
             ageGroups)
modif_beta =  modif_beta_param = matrix(rep(c(1,1,.6),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
modif_porc_gr =  modif_porc_gr_param = matrix(rep(c(1,.3,.1),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
modif_porc_cr =  modif_porc_cr_param = matrix(rep(c(1,.1,.03),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
modif_ifr =  modif_ifr_param = matrix(rep(c(1,.05,.01),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
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
                                             ),
                                             fluidRow(
                                               column(3,DT::dataTableOutput("mbeta")),
                                               column(3,DT::dataTableOutput("mgraves")),
                                               column(3,DT::dataTableOutput("mcriticos")),
                                               column(3,DT::dataTableOutput("mifr")),
                                               column(3,DT::dataTableOutput("paramVac"))
                                             )
                                           )
                                         )
                                       ),
                                       tabPanel("Scenarios",
                                                fluidRow(column(4,
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
                                                                ),
                                                                selectInput(
                                                                  "vacEfficacy",
                                                                  label="Vaccination efficacy (Severe, Moderate, Mild)",
                                                                  choices = c("A. 100% all",
                                                                              "B1. 100%, 80%, 80%",
                                                                              "B2. 100%, 80%, 50%",
                                                                              "C1. 80%, 80%, 50%",
                                                                              "C2. 80%, 50%, 50%"),
                                                                  selected = "B2. 100%, 80%, 50%"
                                                                ),
                                                                selectInput(
                                                                  "immunityDuration",
                                                                  label="Immunity duration",
                                                                  choices = c("6 months"=180,
                                                                              "1 year"=360,
                                                                              "Lifelong"=2000)
                                                                )
                                                            ),
                                                         column(4,

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
                                                         column(4,DTOutput("resumen_tabla"))
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
    
    # N, diaCeroVac, as.Date("2022-01-01"), tVacunasCero, AvArg
    enable("vacDateGoal")
    enable("vacStrat")
    enable("vacEfficacy")
    enable("immunityDuration")
    if (input$vacUptake == "Current uptake") {
      disable("vacDateGoal")
    } else if (input$vacUptake == "No vaccination") {
      disable("vacDateGoal")
      disable("vacStrat")
      disable("vacEfficacy")
      disable("immunityDuration")
    }

    AvArgParam <- generaEscenarioSage(input$vacUptake, input$vacDateGoal, input$vacStrat,
                                      AvArg, N, tVacunasCero, diaCeroVac)
    
    
    AvArgParam <- lapply(AvArgParam, function(dia) {colnames(dia) <- ageGroups 
    return(dia)})
    
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
    
    efficacy = applyVaccineEfficacy(input$vacEfficacy)
    
    paramVac_edit[3,3] = as.numeric(input$immunityDuration) * .25
    paramVac_edit[3,5] = as.numeric(input$immunityDuration)
    
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
              modif_beta=efficacy$modif_beta,
              modif_porc_gr=efficacy$modif_porc_gr,
              modif_porc_cr=efficacy$modif_porc_cr,
              modif_ifr=efficacy$modif_ifr,
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
    
    return(proy)
    
  })
  observe({
    if (primeraVez) {
      updateSelectInput(session, "compart_a_graficar", choices = c(names(proy()),"pV: Vaccination plan"), selected="i: Daily infectious")
      updateNumericInput(session, inputId = "t", value = tHoy)
      for (c in rev(str_trim(str_replace_all(substring(names(proy()),1,3),":","")))) {
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
      tibble(Compart = "pV", do.call(rbind, lapply(AvArgParam,colSums)) %>% as_tibble())) %>%
      dplyr::mutate(fecha = rep(1:length(proy$S),16)) %>%
      # TODO: Arreglar
      dplyr::rename("0-17"=2, "18-29"=3, "30-39"=4, "40-49"=5, "50-59"=6, "60-69"=7, "70+"=8)
    data_graf$total=data_graf$`0-17`+data_graf$`18-29`+data_graf$`30-39`+data_graf$`40-49`+data_graf$`50-59`+data_graf$`60-69`+data_graf$`70+`
    
    data_graf
  })
  
  output$graficoUnico <- renderPlotly({
    if (length(proy()) > 0 & input$compart_a_graficar != "") {
      call_id=str_trim(str_replace_all(substring(input$compart_a_graficar,1,3),":",""))
      dataTemp = data_graf() %>% dplyr::filter(Compart == call_id)
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
          
            if (input$compart_a_graficar == "Ig: Infectious (moderate) SKIP") {
              plot <-  add_segments(plot, x= data$fechaDia[1], xend = data$fechaDia[diasDeProyeccion], y = camasGenerales*porcAsignadoCovid, yend = camasGenerales*porcAsignadoCovid, name = "General beds", line=list(color="#fc9272", dash="dot"))
              plot <-  add_segments(plot, x= valx, xend = valx, y = 0, yend = max(maxy,camasGenerales*porcAsignadoCovid*1.1) , name = paste(valx), line=list(color="#bdbdbd"))    
              plot %>% layout(xaxis = list(title = "Fecha"), 
                              yaxis = list(title = paste("Compartimento:",input$compart_a_graficar)))
              # , range = c(0,60000)
            } else if (input$compart_a_graficar == "Ic: Infectious (severe)") {
              #browser()
              plot <-  add_segments(plot, x= data$fechaDia[1], xend = data$fechaDia[diasDeProyeccion], y = capacidadUTI, yend = capacidadUTI, name = "ICU beds: (100%)", line=list(color="#fc9272", dash="dot"))
              plot <-  add_segments(plot, x= data$fechaDia[1], xend = data$fechaDia[diasDeProyeccion], y = capacidadUTI*porcAsignadoCovid, yend = capacidadUTI*porcAsignadoCovid, name = "ICU beds (70%)", line=list(color="#fc9272", dash="dot"))
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
  output$resumen_tabla <- renderDataTable({
    data_text <- cbind(data_graf(),rep(fechas_master,length(unique(data_graf()$Compart))))
    colnames(data_text)[ncol(data_text)] <- "fechaDia"
    
    data_text <- data_text %>% dplyr::group_by(Compart) %>%
                               dplyr::mutate(ac=cumsum(total))
    
    fechas <- c("2021-12-31",
                "2022-06-30",
                "2022-12-31")
    
    tfechas <- c(which(fechas_master == "2021-12-31"),
                 which(fechas_master == "2022-06-30"),
                 which(fechas_master == "2022-12-31"))
    
    def_ac <- data_text$total[(as.character(data_text$fechaDia) %in% fechas) &
                               data_text$Compart=="D"]  
    
    casos_ac <- data_text$ac[(as.character(data_text$fechaDia) %in% fechas) &
                              data_text$Compart=="i"]
    
    vacunas_ac <- data_text$ac[(as.character(data_text$fechaDia) %in% fechas) &
                                data_text$Compart=="vA"]
    
    poblacion_vac <- data_text$ac[(as.character(data_text$fechaDia) %in% fechas) &
                                   data_text$Compart=="vA"] / sum(N) * 100
    
    sumt1 = proy()$`S: Susceptible`[[tfechas[1]]] + proy()$`V: Vaccinated`[[tfechas[1]]] +
      proy()$`E: Exposed`[[tfechas[1]]] + proy()$`I: Infectious`[[tfechas[1]]] + 
      proy()$`R: Recovered (survivors + deaths)`[[tfechas[1]]]
    poblacion_vac[1] = sum(sumt1[3,]) / sum(N) * 100
    
    sumt2 = proy()$`S: Susceptible`[[tfechas[2]]] + proy()$`V: Vaccinated`[[tfechas[2]]] +
      proy()$`E: Exposed`[[tfechas[2]]] + proy()$`I: Infectious`[[tfechas[2]]] + 
      proy()$`R: Recovered (survivors + deaths)`[[tfechas[2]]]
    poblacion_vac[2] = sum(sumt2[3,]) / sum(N) * 100
    
    sumt3 = proy()$`S: Susceptible`[[tfechas[3]]] + proy()$`V: Vaccinated`[[tfechas[3]]] +
      proy()$`E: Exposed`[[tfechas[3]]] + proy()$`I: Infectious`[[tfechas[3]]] + 
      proy()$`R: Recovered (survivors + deaths)`[[tfechas[3]]]
    poblacion_vac[3] = sum(sumt3[3,]) / sum(N) * 100
    
    
    var=c("Cumulative deaths",
          "Cumulative infections",
          "Vaccines applied",
          "Vaccination coverage (%)")
    C1 = c(def_ac[1],
           casos_ac[1],
           vacunas_ac[1],
           poblacion_vac[1])
    
    C2 = c(def_ac[2],
           casos_ac[2],
           vacunas_ac[2],
           poblacion_vac[2])
    
    C3 = c(def_ac[3],
           casos_ac[3],
           vacunas_ac[3],
           poblacion_vac[3])
    
    tabla <- cbind(var,
                   format(round(C1,0), big.mark = ','),
                   format(round(C2,0), big.mark = ','),
                   format(round(C3,0), big.mark = ','))
    
    colnames(tabla) <- c(" ",fechas)
    
    
    DT::datatable(tabla,
                  caption = 'Results summary',
                  options = list(ordering=F, 
                                 searching=F, 
                                 paging=F, 
                                 info=F)) %>% formatStyle(' ', `text-align` = 'left') %>%
                                              formatStyle(fechas[1], `text-align` = 'right') %>%
                                              formatStyle(fechas[2], `text-align` = 'right') %>%
                                              formatStyle(fechas[3], `text-align` = 'right') 
    
  })
}

shinyApp(ui = ui, server = server)
