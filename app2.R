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
primeraVez = TRUE
paramVac_primeraVez = TRUE
ifr_primeraVez = TRUE
transprob_primeraVez = TRUE
immunityStates = c("no inmunes", "recuperados", "Vacunado")
ageGroups = c("0-17", "18-49", "50-59", "60-69", "70+")
ageGroupsV = c("00","18","50","60","70")
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

modif_beta <- matrix(rep(c(1,.70,.60),length(ageGroups)),3,length(ageGroups),byrow=F)
modif_porc_gr <- matrix(rep(c(1,.70,.60),length(ageGroups)),3,length(ageGroups),byrow=F)
modif_porc_cr <- matrix(rep(c(1,.70,.60),length(ageGroups)),3,length(ageGroups),byrow=F)
modif_ifr <- matrix(rep(c(1,.70,.60),length(ageGroups)),3,length(ageGroups),byrow=F)
duracion_inmunidad = 180

names = list(immunityStates,
             ageGroups)
namesVac = list(immunityStates,
                c("latencia", "porcV", "tiempoV", "porcProt", "tiempoP"))

paramVac <- matrix(data=c(0,0,0,0,0,
                          0,0,0,0,0,
                          20,.2,180,.3,180), nrow=length(immunityStates), ncol=5, byrow=T, dimnames = namesVac)

ui <- fluidPage(theme = bs_theme(bootswatch = "sandstone"),
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
                            tabPanel("Graficos",
                                     br(),
                                     fluidRow(column(3,selectInput("compart_a_graficar","Compartimento",choices = NULL)),
                                              column(2,selectInput("edad","Ver grupos de edad",
                                                                   choices=c("Todas las edades"="total",ageGroups),
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
                                                          "Rango de días graficados",
                                                          min=30,
                                                          max=diasDeProyeccion,
                                                          value = diasDeProyeccion)),
                                                    column(4,sliderInput("ajusta_beta",
                                                           "Ajusta beta",
                                                           min=.3,
                                                           max=.5,
                                                           value= .48,
                                                           step=.01)),
                                                    column(4,numericInput("duracionInm",
                                                           "Duración de la inmunidad",
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
                                                     DT::dataTableOutput("paramVac"),
                                                     DT::dataTableOutput("ifrt")
                                                     )
                                             )
                                           )
                                         )
                                       ),
                                       tabPanel("Question #1",
                                                fluidRow(column(6,
                                                                selectInput(
                                                                  "vacStrat",
                                                                  label="Vaccination strategy",
                                                                  choices = c("Observed strategy",
                                                                              "No vaccination",
                                                                              "No priority",
                                                                              "Metas 1",
                                                                              "Priority: older -> adults -> young",
                                                                              "Priority: older + adults -> young",
                                                                              "Priority: adults -> older -> young")
                                                                  )
                                                                )
                                                         )
                                                )
                                       
                                       
                                       
                            )),
                            tabPanel("Compartimentos", fluidRow(id="content"))
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
                  caption = 'IFR por edad',
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
                  caption = 'Matriz de contacto efectivo (probabilidades de transmisión)',
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
                  caption = 'Parámetros de las vacunas',
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
    
    duracion_inmunidad = input$duracionInm
    
    
    if (input$vacStrat == "No vaccination") {
      
      AvArgParam <- lapply(AvArg, function(dia) {
        return(dia * 0)  
      })
    } else if (input$vacStrat=="No priority") {
      AvArgParam <- lapply(AvArg, function(dia) {
        totalDia <- sum(dia)
        for (i in 1:ncol(dia)) {
          for (j in 3:length(immunityStates)) {
            dia[j,i] <- totalDia * N[i]/sum(N)    
          }
        }
        return(dia)
      })
    } else if (input$vacStrat=="Priority: older -> adults -> young") {
      prioridad <- c(10,10,25,25,30)
      AvArgParam <- lapply(AvArg, function(dia) {
        totalDia <- sum(dia)
        for (i in 1:ncol(dia)) {
          for (j in 3:length(immunityStates)) {
            dia[j,i] <- totalDia * prioridad[i]/sum(prioridad)    
          }
        }
        return(dia)
      })
    } else if (input$vacStrat=="Metas 1") {
      metas <- c(0,.50,.65,.75,.85)
      AvArgParam <- generaPlanVacunacion(metas, N, diaCeroVac, as.Date("2022-01-01"), tVacunasCero, AvArg)
    } else {
        AvArgParam <- AvArg
    }
    
    AvArgParam <- lapply(AvArgParam, function (dia) {
        
      colnames(dia) <- ageGroups
      return(dia)
    })
    
    AvArgParam <<- AvArgParam 
    
    trans_prob_param <- transprob_edit * input$ajusta_beta
    
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
              tVacunasCero=tVacunasCero
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
  
  output$graficoUnico <- renderPlotly({
    if (length(proy()) > 0 & input$compart_a_graficar != "") {
      
      proy <- proy()
      data_graf <- bind_rows(
        tibble(Compart = "S", do.call(rbind, lapply(proy$S,colSums)) %>% as_tibble()),
        tibble(Compart = "V", do.call(rbind, lapply(proy$V,colSums)) %>% as_tibble()),
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
        tibble(Compart = "pV", do.call(rbind, lapply(AvArgParam,colSums)) %>% as_tibble())) %>%
        dplyr::mutate(fecha = rep(1:length(proy$S),15)) %>%
        # TODO: Arreglar
        dplyr::rename("0-17"=2, "18-49"=3, "50-59"=4, "60-69"=5, "70+"=6)
      data_graf$total=data_graf$`0-17`+data_graf$`18-49`+data_graf$`50-59`+data_graf$`60-69`+data_graf$`70+`
      dataTemp = data_graf %>% dplyr::filter(Compart == input$compart_a_graficar)
      dataTemp$fechaDia = fechas_master
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
              plot <-  add_segments(plot, x= valx, xend = valx, y = 0, yend = 60000, name = paste(valx), line=list(color="#bdbdbd"))
              plot %>% layout(xaxis = list(title = "Fecha"),
                              yaxis = list(title = paste("Compartimento:",input$compart_a_graficar),
                                           range = c(0,60000)))
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
}

shinyApp(ui = ui, server = server)
