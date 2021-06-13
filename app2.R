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

load("DatosIniciales_ARG.RData")
# rm(list=setdiff(ls(), c("modeloSimulado",
#                         "duracionMediaInf", 
#                         "diasHospCasosCriticos",
#                         "diasHospCasosGraves", 
#                         "periodoPreinfPromedio",
#                         "dataEcdc",
#                         "porcentajeCasosGraves",
#                         "porcentajeCasosCriticos",
#                         "creaAv")))


# lee funciones
source("functions/seirAges.R", encoding = "UTF-8")
ifr = c(.003,.005,0.01)


# crea matrices de contacto y efectividad
contact_matrix = matrix(c(5,1,1,
                          2,4,4,
                          .5,1,5),3,byrow = T)
colnames(contact_matrix) = rownames(contact_matrix) = c("0-19","20-65","65+")
transmission_probability = matrix(.035,3,3)
colnames(transmission_probability) = rownames(transmission_probability) = c("0-19","20-65","65+")

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

# casos nuevos diarios
inf_p <- sweep(def_p, MARGIN = 2, ifr, "/")

inf_p=inf_p[-c(1:17),]
e_p=inf_p[-c(1:round(periodoPreinfPromedio,0)),]

colnames(inf_p) <- c("inf0019","inf2064","inf6599")

# expuestos
E_p = inf_p[-round(periodoPreinfPromedio,digits=0),]

# infectados
I_p = matrix(nrow=nrow(inf_p),ncol=3)

dmI_redondeada = round(duracionMediaInf,digits=0)

# susceptibles

S_p = matrix(nrow=nrow(inf_p),ncol=3)

lastS = N

for (t in 1:nrow(E_p)) {
  lastS = lastS - E_p[t,]
  S_p[t,] = lastS
}

# recuperados

R_p = matrix(nrow=nrow(inf_p),ncol=3)
lastR = c(0,0,0)

for (t in (dmI_redondeada+1):nrow(inf_p)) {
  lastR = lastR + inf_p[t-dmI_redondeada,]
  R_p[t,]= lastR
}

# arranque para seir
zero_S=colMeans(S_p[(nrow(S_p)-1):(nrow(S_p)-15),])
zero_E=colMeans(E_p[(nrow(E_p)):((nrow(E_p)-14)),])
zero_I=colMeans(I_p[(nrow(I_p)):((nrow(I_p)-14)),])
zero_R=colMeans(R_p[(nrow(R_p)):((nrow(R_p)-14)),])
zero_d=colMeans(def_p[(nrow(def_p)):((nrow(def_p)-14)),])
zero_D=colSums(def_p)
zero_N = zero_S

source("functions/seirAges_matrices.R", encoding = "UTF-8")

proy <- seir_ages(dias=700,
                  duracionE = periodoPreinfPromedio,
                  duracionIi = duracionMediaInf,
                  porc_gr = porcentajeCasosGraves,
                  porc_cr = porcentajeCasosCriticos,
                  duracionIg = diasHospCasosGraves,
                  duracionIc = diasHospCasosCriticos,
                  ifr = c(.003,.005,0.01),
                  vacunados = c(0,0,0),
                  contact_matrix = contact_matrix,
                  transmission_probability = transmission_probability,
                  N = N,
                  zero_sus = zero_S,
                  zero_exp = zero_E,
                  zero_cases = zero_I,
                  zero_rec = zero_R,
                  zero_D = zero_D,
                  zero_d = zero_d,
                  expuestos_reales=e_p,
                  defunciones_reales=def_p
                  
)


ui <- fluidPage(theme = bs_theme(bootswatch = "sandstone"),
                # fluidRow(column(12,h1("Visualizador"), align="center")),
                # hr(),
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
                fluidRow(column(12,id="content")),
                br(),
                br(),
                br(),
                br(),
                br(), plotOutput("grafico")
                )
                

server <- function (input, output, session) {
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
  updateTable <- function (table,t) {
    output[[table]] <- renderDT(round(proy[[table]][[t]],0), editable = F,rownames = T, options = list(paging = FALSE, info = FALSE, searching = FALSE, fixedColumns = TRUE,autoWidth = TRUE,ordering = FALSE,dom = 'Bfrtip'))
  }
  updateTables <- function (t) {
    print(isolate(t))
    lapply(
      X = comp,
      FUN = function(c){
        output[[c]] <- renderDT(round(proy[[c]][[t]],0), editable = F,rownames = T, options = list(paging = FALSE, info = FALSE, searching = FALSE, fixedColumns = TRUE,autoWidth = TRUE,ordering = FALSE,dom = 'Bfrtip'))
      }
    )
  }
  
  updateTables(input$t)
  # observe event for updating the reactiveValues
  observeEvent(input$submit,
               {
                 updateTables(input$t)
               })
  
  observeEvent(input$prev, {
    updateTables(input$t - 1)
    updateNumericInput(session,"t", value =  input$t - 1)
  })
  
  observeEvent(input$prox, {
    updateTables(input$t + 1)
    updateNumericInput(session,"t", value =  input$t + 1)
  })

output$grafico <- renderPlot({
  
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

  for (c in unique(data_graf$Compart)) {
    
    plot <- ggplot(data=data_graf[data_graf$Compart==c,], aes(x=fecha, y=total, group=1)) +
      geom_line()+
      geom_point() + geom_vline(xintercept = as.numeric(input$t))
    
    eval(parse(text=paste0("plot_",c," <<- plot")))
  }
  ggarrange(plot_S,
            plot_E,
            plot_I,
            plot_Ic,
            plot_i,
            plot_R,
            plot_V,
            plot_D,
            plot_d,
            plot_e,
            labels = c("S",
                       "E",
                       "I",
                       "Ic",
                       "i",
                       "R",
                       "V",
                       "D",
                       "d",
                       "e"))
  
  
  
  
  
})
}


shinyApp(ui = ui, server = server)



