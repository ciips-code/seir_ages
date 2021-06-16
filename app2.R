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

rm(list = ls())

# historico vacunas
source("vacunas historico arg.R", encoding = 'UTF-8')

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
source("functions/seirAges.R", encoding = "UTF-8")
ifr = c(.003,.005,0.01)
primeraVez = TRUE
paramVac_primeraVez = TRUE
ifr_primeraVez = TRUE
transprob_primeraVez = TRUE
immunityStates = c("no inmunes", "recuperados", "Vacunado")
ageGroups = c("0 a 19", "20 a 64", "65 y mas")
ageGroupsV = c(0,20,65)
# crea matrices de contacto y efectividad
contact_matrix = matrix(c(5,1,1,
                          2,4,4,
                          .5,1,5),3,byrow = T)
colnames(contact_matrix) = rownames(contact_matrix) = ageGroups
transmission_probability = matrix(.035,length(ageGroups),length(ageGroups))
colnames(transmission_probability) = rownames(transmission_probability) = ageGroups

# datos de poblacion ejemplo Argentina
N1 <- 14554190
N2 <- 25594851
N3 <-  5227722

N = c(N1,N2,N3)

# prepara datos reportados

t <- seq(1:nrow(dataEcdc))

def0019 <- dataEcdc$nd0004+dataEcdc$nd0509+dataEcdc$nd1014+dataEcdc$nd1519
def2064 <- dataEcdc$nd2024+dataEcdc$nd2529+dataEcdc$nd3034+dataEcdc$nd3539+dataEcdc$nd4044+dataEcdc$nd4549+dataEcdc$nd5054+dataEcdc$nd5559+dataEcdc$nd6064
def6599 <- dataEcdc$nd6569+dataEcdc$nd7074+dataEcdc$nd7579+dataEcdc$nd8084+dataEcdc$nd8589+dataEcdc$nd9099

def0019_loess <- predict(loess(def0019~seq(1,length(def0019), by=1),span=.2))
def2064_loess <- predict(loess(def2064~seq(1,length(def2064), by=1),span=.2))
def6599_loess <- predict(loess(def6599~seq(1,length(def6599), by=1),span=.2))

# muertes
def_p <- cbind(def0019_loess,def2064_loess,def6599_loess)

modif_beta <- matrix(c(1,1,1,
                      .70,.70,.70,
                      .60,.60,.60),3,3,byrow=T)
modif_porc_gr <- matrix(c(1,1,1,
                         .70,.70,.70,
                         .60,.60,.60),3,3,byrow=T)
modif_porc_cr <- matrix(c(1,1,1,
                         .70,.70,.70,
                         .60,.60,.60),3,3,byrow=T)
modif_ifr <- matrix(c(1,1,1,
                     .70,.70,.70,
                     .60,.60,.60),3,3,byrow=T)
diasDeProyeccion = 900
duracion_inmunidad = 180
AvArg = creaAv(min(modeloSimulado$fecha), diasDeProyeccion)[[1]]
vacPlanDia <- creaAv(min(modeloSimulado$fecha), diasDeProyeccion)[[2]]

names = list(immunityStates,
             ageGroups)
namesVac = list(immunityStates,
                c("latencia", "porcV", "tiempoV", "porcProt", "tiempoP"))

paramVac <- matrix(data=c(0,0,0,0,0,
                          0,0,0,0,0,
                          20,.2,180,.3,180), nrow=length(immunityStates), ncol=5, byrow=T, dimnames = namesVac)
source("functions/seirAges_matrices.R", encoding = "UTF-8")

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
                                     selectInput("compart_a_graficar","Compartimento",choices = NULL),
                                     plotlyOutput("graficoUnico"),
                                     hr(),
                                     fluidRow(
                                       fluidRow(
                                         column(6,
                                            fluidRow(
                                              column(4,
                                                    numericInput("diasProy",
                                                    "Días de proyección",
                                                    min=30,
                                                    max=1000,
                                                    value = 700)),
                                              column(4,sliderInput("modifica_planVac",
                                                     "Modificar plan de vacunación (%)",
                                                     min=-100,
                                                     max=100,value= 1)),
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
                            )),
                            tabPanel("Compartimentos", fluidRow(id="content")),
                            tabPanel("Formulas",
                                     p("d:"),
                                     code("d[[t]][1,] = defunciones_reales[t,]"),
                                     br(),
                                     code("d[[t]] = Ic[[t-1]]/duracionIc * ifr * modif_ifr/porc_cr*modif_porc_cr"),
                                     br(),
                                     br(),
                                     br(),
                                     fluidRow(
                                       column(width = 4,
                                              p("d[[t-1]]"),
                                              br(),
                                              DTOutput("d[[t-1]]")
                                       ),
                                       column(width = 4,
                                              p("d[[t]]"),
                                              br(),
                                              DTOutput("d[[t]]")
                                       ),
                                       column(width = 4,
                                              p("defunciones_reales[t,]"),
                                              br(),
                                              textOutput("defunciones_reales[t,]")
                                       ),
                                     ),
                                     br(),
                                     fluidRow(
                                       column(width = 3,
                                              p("Ic[[t-1]]"),
                                              br(),
                                              DTOutput("Ic[[t-1]]")
                                       ),
                                       column(width = 1,
                                              p("duracionIc"),
                                              br(),
                                              textOutput("duracionIc")
                                       ),
                                       column(width = 1,
                                              p("ifr"),
                                              br(),
                                              textOutput("ifr")
                                       ),
                                       column(width = 3,
                                              p("modif_ifr"),
                                              br(),
                                              DTOutput("modif_ifr")
                                       ),
                                       column(width = 1,
                                              p("porc_cr"),
                                              br(),
                                              textOutput("porc_cr")
                                       ),
                                       column(width = 3,
                                              p("modif_porc_cr"),
                                              br(),
                                              DTOutput("modif_porc_cr")
                                       ),
                                     ),
                                     br(),
                                     fluidRow(align="center",
                                       column(width = 8, offset = 2,
                                              textOutput("calculo-dt12"),
                                              br(),
                                              DTOutput("calculo-dt")
                                       ),
                                     ),
                            )
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
      ifr_edit <- matrix(ifr, 1,3)
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
    DT::datatable(ifr_edit, editable = T, options = list(ordering=F, 
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
    DT::datatable(transprob_edit, editable = T, options = list(ordering=F, 
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
    DT::datatable(paramVac_edit, editable = T, options = list(ordering=F, 
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
    for (i in vacPlanDia:diasDeProyeccion) {
      AvArg[[i]] <- AvArg[[i]] * input$modifica_planVac /100
    }
    
    seir_ages(dias=diasDeProyeccion,
              duracionE = periodoPreinfPromedio,
              duracionIi = duracionMediaInf,
              porc_gr = porcentajeCasosGraves,
              porc_cr = porcentajeCasosCriticos,
              duracionIg = diasHospCasosGraves,
              duracionIc = diasHospCasosCriticos,
              ifr = ifr_edit[1,],
              contact_matrix = contact_matrix,
              transmission_probability = transprob_edit,
              N = N,
              defunciones_reales=def_p,
              modif_beta=modif_beta,
              modif_porc_gr=modif_porc_gr,
              modif_porc_cr=modif_porc_cr,
              modif_ifr=modif_ifr,
              Av=AvArg,
              immunityStates=immunityStates,
              ageGroups=ageGroups,
              paramVac=paramVac_edit,
              duracion_inmunidad=duracion_inmunidad
    )
  })
  observe({
    updateSelectInput(session, "compart_a_graficar", choices = names(proy()), selected="i")
    if (primeraVez) {
      updateNumericInput(session, inputId = "t", value = tHoy)
      primeraVez <<- FALSE
    }
    t = input$t
    if (t>1) {
      # Actualiza tab pane de formula
      output[["d[[t-1]]"]] <- renderDT(round(proy()[["d"]][[t-1]],2), editable = F,rownames = T, options = list(paging = FALSE, info = FALSE, searching = FALSE, fixedColumns = TRUE,autoWidth = TRUE,ordering = FALSE,dom = 'Bfrtip'))
      output[["d[[t]]"]] <- renderDT(round(proy()[["d"]][[t]],2), editable = F,rownames = T, options = list(paging = FALSE, info = FALSE, searching = FALSE, fixedColumns = TRUE,autoWidth = TRUE,ordering = FALSE,dom = 'Bfrtip'))
      if (isolate(t)<= nrow(proy()[["defunciones_reales"]])) {
        output[["defunciones_reales[t,]"]] <- renderText(proy()[["defunciones_reales"]][t,])
      } else {
        output[["defunciones_reales[t,]"]] <- renderText("Sin datos")
      }
      output[["Ic[[t-1]]"]] <- renderDT(round(proy()[["Ic"]][[t-1]],2), editable = F,rownames = T, options = list(paging = FALSE, info = FALSE, searching = FALSE, fixedColumns = TRUE,autoWidth = TRUE,ordering = FALSE,dom = 'Bfrtip'))
      output[["duracionIc"]] <- renderText(diasHospCasosCriticos)
      output[["ifr"]] <- renderText(ifr)
      output[["modif_ifr"]] <- renderDT(round(modif_ifr,2), editable = F,rownames = T, options = list(paging = FALSE, info = FALSE, searching = FALSE, fixedColumns = TRUE,autoWidth = TRUE,ordering = FALSE,dom = 'Bfrtip'))
      output[["porc_cr"]] <- renderText(porcentajeCasosCriticos)
      output[["modif_porc_cr"]] <- renderDT(round(modif_porc_cr,2), editable = F,rownames = T, options = list(paging = FALSE, info = FALSE, searching = FALSE, fixedColumns = TRUE,autoWidth = TRUE,ordering = FALSE,dom = 'Bfrtip'))
      ifrm = matrix(rep(ifr,length(immunityStates)),length(immunityStates),3,byrow = T)
      calculo_dt <- proy()[["Ic"]][[t-1]]/diasHospCasosCriticos * (ifrm) * modif_ifr/porcentajeCasosCriticos*modif_porc_cr
      calculo_dt12 <-  proy()[["Ic"]][[t-1]][1,2]/diasHospCasosCriticos * ifr[2] * modif_ifr[1,2]/porcentajeCasosCriticos*modif_porc_cr[1,2]
      output[["calculo-dt12"]] <- renderText(paste("dt Calculado [1,2]:", calculo_dt12))
      output[["calculo-dt"]] <- renderDT(round(calculo_dt,2), editable = F,rownames = T, options = list(paging = FALSE, info = FALSE, searching = FALSE, fixedColumns = TRUE,autoWidth = TRUE,ordering = FALSE,dom = 'Bfrtip'))
    }
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
        tibble(Compart = "Ic", do.call(rbind, lapply(proy$Ic,colSums)) %>% as_tibble()),
        tibble(Compart = "i", do.call(rbind, lapply(proy$i,colSums)) %>% as_tibble()),
        tibble(Compart = "D", do.call(rbind, lapply(proy$D,colSums)) %>% as_tibble()),
        tibble(Compart = "d", do.call(rbind, lapply(proy$d,colSums)) %>% as_tibble()),
        tibble(Compart = "R", do.call(rbind, lapply(proy$R,colSums)) %>% as_tibble())) %>%
        dplyr::mutate(fecha = rep(1:length(proy$S),10)) %>%
        dplyr::rename("0-19"=2,"20-64"=3,"65+"=4)
      data_graf$total=data_graf$`0-19`+data_graf$`20-64`+data_graf$`65+`
      dataTemp = data_graf %>% dplyr::filter(Compart == input$compart_a_graficar)
      dataTemp$fechaDia = seq(min(dataEcdc$dateRep),min(dataEcdc$dateRep)+diasDeProyeccion-1,by=1)
      valx = dataTemp$fechaDia[input$t]
      maxy = max(dataTemp$total)
      #browser()
      if (is.na(input$diasProy)==F)
        {
          plot_ly(dataTemp[1:(input$t+input$diasProy),], x=~fechaDia, 
                  y=~total,
                  type="scatter", mode="lines", name = paste("Valor de",input$compart_a_graficar)) %>%
                  add_segments(x= valx, xend = valx, y = 0, yend = maxy, name = paste("t"))
        }
    }
  })
}

shinyApp(ui = ui, server = server)
