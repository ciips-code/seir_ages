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
library(shinyWidgets)

rm(list = ls())

# carga RData

load("DatosIniciales_ARG.RData", envir = .GlobalEnv)
rm(list=setdiff(ls(), c("duracionMediaInf",
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

primeraVez = porc_gr_primeraVez = porc_cr_primeraVez = paramVac_primeraVez = ifr_primeraVez = transprob_primeraVez = mbeta_primeraVez = mgraves_primeraVez = mcriticos_primeraVez = mifr_primeraVez = TRUE
# crea matrices de contacto y efectividad - set TRUE si queremos observada
use_empirical_mc = T
immunityStates <<- c("No immunity", "Recovered", "1Dosis", "2Dosis")
ageGroups <<- c("0-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
ageGroupsV <<- c("00","18","30","40","50", "60", "70", "80")
names <- list(immunityStates,
              ageGroups)
# crea matrices de contacto y efectividad
contact_matrix = matrix(c(5,1,1,1,1,1,1,1,
                          2,4,4,4,4,4,4,4,
                          2,4,4,4,4,4,4,4,
                          2,4,4,4,4,4,4,4,
                          2,4,4,4,4,4,4,4,
                          2,4,4,4,4,4,4,4,
                         .5,1,1,1,1,1,5,5,
                         .5,1,1,1,1,1,5,5),8,byrow = T)
colnames(contact_matrix) = rownames(contact_matrix) = ageGroups

transmission_probability = matrix(c(0.2299, 0.2413, 0.2527, 0.266, 0.2831, 0.3097, 0.3211, 0.3211,
                                    0.47795, 0.50165, 0.52535, 0.553, 0.58855, 0.64385, 0.66755, 0.66755,
                                    0.5203, 0.5461, 0.5719, 0.602, 0.6407, 0.7009, 0.7267, 0.7267,
                                    0.484, 0.508, 0.532, 0.56, 0.596, 0.652, 0.676, 0.676,
                                    0.4961, 0.5207, 0.5453, 0.574, 0.6109, 0.6683, 0.6929, 0.6929,
                                    0.5324, 0.5588, 0.5852, 0.616, 0.6556, 0.7172, 0.7436, 0.7436,
                                    0.4477, 0.4699, 0.4921, 0.518, 0.5513, 0.6031, 0.6253, 0.6253,
                                    0.4477, 0.4699, 0.4921, 0.518, 0.5513, 0.6031, 0.6253, 0.6253),length(ageGroups),length(ageGroups),byrow = T)
if(use_empirical_mc){
  contact_matrix <- get_empirical_cm(country = "Argentina", ages=as.numeric(ageGroupsV))
  colnames(contact_matrix) = rownames(contact_matrix) = ageGroups
  transmission_probability = transmission_probability * 0.23 # a ojo
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
      2323393,
      1246451)

# datos de gravedad
# Age specific IFR
ifr = c(8.8e-05,0.000284,0.000745,0.001868,0.004608,0.011231,0.026809,0.079684) * 1.4
# porcentajeCasosGraves = 0.0328
porcentajeCasosGravesRow = c(0.003634, 0.003644, 0.005372, 0.008520, 0.025740, 0.044253, 0.099200, 0.205628) * 0.7
porcentajeCasosGraves = matrix(rep(porcentajeCasosGravesRow,length(immunityStates)),4,length(ageGroups),byrow=T,dimnames = names)
# porcentajeCasosCriticos = 0.0108
porcentajeCasosCriticosRow = c(0.000966,0.000969,0.001428,0.00348,0.01326,0.024747,0.0608,0.094372) * 0.7
porcentajeCasosCriticos = matrix(rep(porcentajeCasosCriticosRow,length(immunityStates)),4,length(ageGroups),byrow=T,dimnames = names)

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

# vacArg2 = lapply(1:nrow(dataPorEdad$FMTD$vac2), matrix,  data=0,
#                  nrow=length(immunityStates),
#                 ncol=length(ageGroups))



for (t in 1:length(vacArg)) {
  # TODO: Expandir a otras vacunas
  vacArg[[t]][3,]  = as.numeric(dataPorEdad$FMTD$vac[t,2:ncol(dataPorEdad$FMTD$vac)])
  vacArg[[t]][4,]  = as.numeric(dataPorEdad$FMTD$vac2[t,2:ncol(dataPorEdad$FMTD$vac2)])
}

# for (t in 1:length(vacArg2)) {
#   # TODO: Expandir a otras vacunas
#   vacArg2[[t]][3,]  = as.numeric(dataPorEdad$FMTD$vac2[t,2:ncol(dataPorEdad$FMTD$vac2)])
# }

# pone cero si hay na en 2da dosis
vacArg <- rapply(vacArg, f=function(x) ifelse(is.na(x),0,x), how="replace" )

# promedio de aplicacion de cada dosis 
promedio =  round(Reduce("+", vacArg) / length(vacArg),0)

vacPlan = lapply(1:(diasDeProyeccion-length(vacArg)-length(vacPre)), matrix, data=t(promedio),
                 nrow=length(immunityStates),
                 ncol=length(ageGroups))

# vacPlan2 = lapply(1:(diasDeProyeccion-length(vacArg2)-length(vacPre)), matrix, data=t(promedio2),
#                   nrow=length(immunityStates),
#                   ncol=length(ageGroups))



planVacunacionFinal <<- c(vacPre,vacArg,vacPlan)


# vacPlanDia <- length(vacPre)+length(vacArg)

# t <- seq(1:nrow(dataEcdc))
temp <- lapply(colnames(dataPorEdad$FMTD$def)[-1], function(loopCol) {
  loessCol = paste0(loopCol,'_loess')
  dataPorEdad$FMTD$def[loessCol] <<- predict(loess(dataPorEdad$FMTD$def[loopCol][,1]~seq(1,nrow(dataPorEdad$FMTD$def), by=1),span=.4))
})


rm(temp)

# muertes
loessCols = which(colnames(dataPorEdad$FMTD$def) %in% grep("loess",colnames(dataPorEdad$FMTD$def), value = TRUE))
def_p <- dataPorEdad$FMTD$def[,loessCols]
def_p <- def_p[1:(nrow(def_p)-15),]

rowSums(def_p[206,])

fechas_master = seq(min(dataPorEdad$FMTD$def$fecha),
                    min(dataPorEdad$FMTD$def$fecha)+diasDeProyeccion-1,by=1)
modif_beta =  modif_beta_param = matrix(rep(c(1,0.15,.6,.5),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = names)
modif_porc_gr =  modif_porc_gr_param = matrix(rep(c(1,.3,.1,.05),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = names)
modif_porc_cr =  modif_porc_cr_param = matrix(rep(c(1,.1,.03,.02),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = names)
modif_ifr =  modif_ifr_param = matrix(rep(c(1,.05,.01,.005),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = names)
duracion_inmunidad = 180
duracion_proteccion = 360 # TODO: Implementar, Cuanto?

namesVac = list(immunityStates,
                c("latencia", "porcV", "tiempoV", "porcProt", "tiempoP", "intervaloInterDosis", "idVacuna", "dosis"))

paramVac <<- matrix(data=c(0,0,0,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,
                          20,.2,30,.6,360,30,"SPTNK",1
                          ,20,.4,30,.5,360,30,"SPTNK",2
                          # ,20,.4,0,.5,360,30,"SINOPH",1
                          # ,20,.4,0,.5,360,30,"SINOPH",2
                          ), nrow=length(immunityStates), ncol=8, byrow=T, dimnames = namesVac)

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
                                                                   selected= c("total"))),
                                              
                                                       
                                              column(4,fluidRow(column(4,textInput("save_comp_name", "Save to scenario", placeholder = "Enter name")),
                                                                column(2,br(),actionButton("save_comp", icon("chevron-right"))),
                                                                column(2,prettyCheckbox(inputId = "check_cases",label = "Show reported cases",value = FALSE),
                                                                         prettyCheckbox(inputId = "check_deaths",label = "Show reported deaths",value = FALSE)))
                                              )
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
                                                           value = duracion_inmunidad))
                                                  ),
                                                  fluidRow(
                                                    column(12,
                                                           DT::dataTableOutput("transprob")
                                                    )
                                                  )
                                               ),
                                              column(6,
                                                     DT::dataTableOutput("ifrt"),
                                                     DT::dataTableOutput("porc_gr"),
                                                     DT::dataTableOutput("porc_cr")
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
                                                                selectInput(
                                                                  "npiScenario",
                                                                  label="Starting NPI Scenario",
                                                                  choices = c("Baseline",
                                                                              "School closures",
                                                                              "Physical distancing",
                                                                              "Shielding of older people",
                                                                              "Self-isolation",
                                                                              "Combined",
                                                                              "Intensive interventions with schools closed",
                                                                              "Intensive interventions with schools open",
                                                                              "Lockdown")
                                                                ),
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
                                                                              value=.6, #.40,
                                                                              step=.1)),
                                                         column(4,DTOutput("resumen_tabla"))
                                                         )
                                                )
                                       
                                       
                                       
                            )),
                            tabPanel("Scenarios", 
                                     selectInput("saved_series", "Saved series", choices="", multiple = T),
                                     #selectInput("age_groups_comp", "Age groups", choices=c("All ages"="total",ageGroups)),
                                     plotlyOutput("graficoComp")),
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
      porcentajeCasosCriticos <<- matrix(rep(porc_cr_edit,length(immunityStates)),3,length(ageGroups),byrow=T,dimnames = names)
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
      porcentajeCasosGraves <<- matrix(rep(porc_gr_edit,length(immunityStates)),3,length(ageGroups),byrow=T,dimnames = names)
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
    
    duracion_inmunidad = input$duracionInm

    
    # N, diaCeroVac, as.Date("2022-01-01"), tVacunasCero, planVacDosis1
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

    planVacunacionFinalParam <- generaEscenarioSage(input$vacUptake, input$vacDateGoal, input$vacStrat,
                                              planVacunacionFinal, N, tVacunasCero, diaCeroVac)
    
    planVacunacionFinalParam <- lapply(planVacunacionFinalParam, function(dia) {colnames(dia) <- ageGroups 
    return(dia)})
    
    planVacunacionFinalParam <<- planVacunacionFinalParam 
    
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
    
    # Aplicar el NPI Scenario seleccionado y mandarlo al SEIR
    
    efficacy = applyVaccineEfficacy(input$vacEfficacy)
    
    # paramVac_edit[3,3] = as.numeric(input$immunityDuration) * .25
    # paramVac_edit[3,5] = as.numeric(input$immunityDuration)
    
    print(porcentajeCasosGraves)
    print(porcentajeCasosCriticos)
    
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
              planVacunacionFinal=planVacunacionFinalParam,
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
    casos <- data.frame(fecha=unique(data_graf$fecha)) %>% left_join(casos)
    
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
    muertes <- data.frame(fecha=unique(data_graf$fecha)) %>% left_join(muertes)
    #muertes[is.na(muertes)] <- 0
    muertes$Compart <- "muertes_registradas"
    
    data_graf=union_all(data_graf,muertes)
    data_graf
    
  })
  
  output$graficoUnico <- renderPlotly({
    
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
          data=dataTemp[1:(input$t+input$diasProy),]
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
  
  observe({
    if (str_trim(input$save_comp_name)=="") {
      disable("save_comp")
    } else {
      enable("save_comp")
    }
    
  })
  
  data_comp <- reactive({
    df <- data_graf() %>% dplyr::filter(Compart==str_trim(str_replace_all(substring(input$compart_a_graficar,1,3),":",""))) %>%
                          dplyr::mutate(Compart=input$save_comp_name)
    df$fechaDia=fechas_master
    df
    
  })
  
  
  
  data_comp_graf <- eventReactive(input$save_comp,{
      
      if (input$save_comp_name!="" & exists("compare")==T) {
          compare <<- union_all(compare,data_comp())} else if (input$save_comp_name!="" & exists("compare")==F) {
          compare <<- data_comp() 
          }
    showNotification("Escenario guardado") 
    compare
    })    

  
   
  
   observeEvent(input$save_comp,{
     
     updateSelectInput(session, "saved_series", choices = unique(data_comp_graf()$Compart), selected = unique(data_comp_graf()$Compart))
     
     
   })
   
   output$graficoComp <- renderPlotly({
     data_comp <-  data_comp_graf() %>% dplyr::filter(Compart %in% input$saved_series)
     plot_comp <- plot_ly() 
     lapply(unique(data_comp$Compart), function(k) {
        plot_comp <<- add_trace(plot_comp,
                               data_comp[data_comp$Compart==k,],
                               x=~data_comp$fechaDia[data_comp$Compart==k],
                               y=~data_comp$total[data_comp$Compart==k], 
                                
                               type="scatter", mode="lines",
                               name=k)
       
     })
     plot_comp %>% layout(xaxis = list(title="Date"), yaxis = list(title="Value"))    
        
     
   }) 

}
  
shinyApp(ui = ui, server = server)

  