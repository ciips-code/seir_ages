library(tidyr)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)
library(DT)
library(EpiEstim)

load("DatosIniciales_ARG.RData")
source("functions/seirAges.R", encoding = "UTF-8")

# carga RData
rm(list = ls())
load("DatosIniciales_ARG.RData")

rm(list=setdiff(ls(), c("duracionMediaInf", 
                        "diasHospCasosCriticos",
                        "diasHospCasosGraves", 
                        "periodoPreinfPromedio",
                        "dataEcdc",
                        "porcentajeCasosGraves",
                        "porcentajeCasosCriticos",
                        "tasaLetalidadAjustada")))
# lee funciones
source("functions/seirAges.R", encoding = "UTF-8")
ifr = c(.003,.005,0.01)

# crea matrices de contacto y efectividad
contact_matrix <<- matrix(c(3,1,1,
                          2,2,2,
                          .3,1,3),3,byrow = T)
colnames(contact_matrix) = rownames(contact_matrix) = c("0-19","20-65","65+")
transmission_probability = matrix(.05,3,3)
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

# muertes
def_p <- cbind(def0019,def2064,def6599)

# casos nuevos diarios
inf_p <- sweep(def_p, MARGIN = 2, ifr, "/")

inf_p=inf_p[-c(1:17),]
colnames(inf_p) <- c("inf0019","inf2064","inf6599")
# expuestos
E_p = inf_p[-round(periodoPreinfPromedio,digits=0),]

# infectados
I_p = matrix(nrow=nrow(inf_p),ncol=3)

dmI_redondeada = round(duracionMediaInf,digits=0)

for (t in dmI_redondeada:nrow(inf_p)) {
  I_p[t,]=colMeans(inf_p[(t-dmI_redondeada+1):t,])*dmI_redondeada
}

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


# estima Rt de cada grupo
Rt_calc <-c(
  last(estimate_R(inf_p[,1],
             method = "parametric_si",
             config = make_config(list(mean_si = 2.6,
                                       std_si = 1.5)))[["R"]]$`Mean(R)`),
  last(estimate_R(inf_p[,2],
                  method = "parametric_si",
                  config = make_config(list(mean_si = 2.6,
                                            std_si = 1.5)))[["R"]]$`Mean(R)`),
  last(estimate_R(inf_p[,3],
                  method = "parametric_si",
                  config = make_config(list(mean_si = 2.6,
                                            std_si = 1.5)))[["R"]]$`Mean(R)`))


ui <- fluidPage(theme = bs_theme(bootswatch = "sandstone"),
                fluidRow(column(12,h1("Modelo SEIR por edades"), align="center")),
                hr(),
                fluidRow(column(4,h2("Parámetros"), align="center"),column(2),column(6,h2("Matrices"),align="center")),
                br(),
                br(),
                fluidRow(column(2,numericInput("user_dias","Días de proyección",value=300,min=30,max=365),
                                  numericInput("user_duracionE","Duración exposición",value=periodoPreinfPromedio,min=1,max=20),
                                  numericInput("user_duracionIi","Duración infección",value=duracionMediaInf,min=1,max=20),
                                  numericInput("user_porc_gr","Proporción de casos graves",value=porcentajeCasosGraves,min=1,max=100),
                                  numericInput("user_duracionIg","Duración internación casos graves",value=diasHospCasosGraves,min=1,max=50)
                                  ),
                                  
                                  
                         column(2,
                                numericInput("user_porc_cr","Proporción de casos críticos",value=porcentajeCasosCriticos,min=1,max=100),
                                numericInput("user_duracionIc","Duración internación casos criticos",value=diasHospCasosCriticos,min=1,max=50),
                                numericInput("user_ifr1","IFR grupo 1",value=tasaLetalidadAjustada,min=0,max=0.99),
                                numericInput("user_ifr2","IFR grupo 2",value=tasaLetalidadAjustada,min=0,max=0.99),
                                  numericInput("user_ifr3","IFR grupo 3",value=tasaLetalidadAjustada,min=0,max=0.99)),
                         column(2,
                                numericInput("user_vacunados1","Proporción vacunados grupo 1",value=0,min=0,max=0.99),
                                numericInput("user_vacunados2","Proporción vacunados grupo 2",value=0,min=0,max=0.99),
                                numericInput("user_vacunados3","Proporción vacunados grupo 3",value=0,min=0,max=0.99),
                                numericInput("user_poblacion1","Población grupo 1",value=N1,min=1,max=100000),
                                numericInput("user_poblacion2","Población grupo 2",value=N2,min=1,max=100000),
                                numericInput("user_poblacion3","Población grupo 3",value=N3,min=1,max=100000)),
                         column(2,
                                br(),
                                
                                h4("Contactos"),
                                DTOutput("contactos"), align="center"),
                         column(2,
                                br(),
                                
                                h4("Efectividad"),
                                DTOutput("efect"), align="center"),
                         column(2,
                                br(),
                                
                                h4("Contactos efectivos"),
                                DTOutput("beta"), align="center")
                         
                      
                        ),
                br(),
                br(),
                fluidRow(column(12,h3("Gráfico SEIR"),
                                br(),
                                plotOutput("grafico_seir"), align="center",
                                br())),
                fluidRow(column(12,  h2("Gráfico de nuevos infectados diarios"), align="center")),
                br(),
                br(),
                fluidRow(column(12,  plotlyOutput("grafico_i"), align="center")),
                br(),
                br(),
                br(),
                hr(),
                fluidRow(column(3,h3("Documentos"))), 
                fluidRow(column(3,tags$a(href="seir_ages.html", "Documento Modelo Iván", target="_blank"))),
                br(),
                br(),
                br(),
                
                br(),
                br()
                
                )
                

server <- function (input, output, session) {
  # contact_matrix = matrix(c(5,1,1,
  #                           2,4,4,
  #                           .5,1,5),3,byrow = T)
  # colnames(contact_matrix) = rownames(contact_matrix) = c("0-19","20-65","65+")
  
  # transmission_probability = matrix(.05,3,3)
  # colnames(transmission_probability) = rownames(transmission_probability) = c("0-19","20-65","65+")
   
  
  
  output$contactos <- renderDT(contact_matrix, 
                               editable = T,
                               rownames = T,
                               options = list(
                                 paging = FALSE,
                                 info = FALSE,
                                 searching = FALSE,
                                 fixedColumns = TRUE,
                                 autoWidth = TRUE,
                                 ordering = TRUE,
                                 dom = 'Bfrtip'
                               ))
  output$efect <- renderDT(transmission_probability, 
                           editable = T,
                           rownames = T,
                           options = list(
                             paging = FALSE,
                             info = FALSE,
                             searching = FALSE,
                             fixedColumns = TRUE,
                             autoWidth = TRUE,
                             ordering = TRUE,
                             dom = 'Bfrtip'
                           ))
  
 
  
  
  observeEvent(input$contactos_cell_edit,{
    
    print(input$contactos_cell_edit)
    contact_matrix[as.numeric(input$contactos_cell_edit[1]),as.numeric(input$contactos_cell_edit[2])] <- as.numeric(input$contactos_cell_edit[3])
    contact_matrix <<- contact_matrix
    
  })
  
  observeEvent(input$efect_cell_edit,{
    
    print(input$efect_cell_edit)
    transmission_probability[as.numeric(input$efect_cell_edit[1]),as.numeric(input$efect_cell_edit[2])] <- as.numeric(input$efect_cell_edit[3])
    transmission_probability <<- transmission_probability
    
  })
  
  
  beta <- reactive({
    print(input$contactos_cell_edit)
    print(input$efect_cell_edit)
    contact_matrix*transmission_probability
  })
  
  output$beta <- renderDT(beta(), 
                          editable = T,
                          rownames = T,
                          options = list(
                            paging = FALSE,
                            info = FALSE,
                            searching = FALSE,
                            fixedColumns = TRUE,
                            autoWidth = TRUE,
                            ordering = TRUE,
                            dom = 'Bfrtip'
                          ))
    
  seir <- reactive({
  print(input$contactos_cell_edit)
  print(input$efect_cell_edit)
  print(contact_matrix)
  seir_ages(
    dias = input$user_dias,
    duracionE = input$user_duracionE,
    duracionIi= input$user_duracionIi, 
    porc_gr= input$user_porc_gr, 
    duracionIg= input$user_duracionIg, 
    porc_cr=input$user_porc_cr, 
    duracionIc=input$user_duracionIc,
    ifr = c(input$user_ifr1,input$user_ifr2,input$user_ifr3),
    vacunados = c(input$user_vacunados1,input$user_vacunados2,input$user_vacunados3),
    contact_matrix = contact_matrix,
    transmission_probability = transmission_probability,
    N = c(input$user_poblacion1,
          input$user_poblacion2,
          input$user_poblacion3),
    zero_sus = zero_S,
    zero_exp = zero_E,
    zero_cases = zero_I,
    zero_rec = zero_R,
    zero_D = zero_D,
    zero_d = zero_d,
    Rt = Rt_calc
    )
  })
  

  
  output$grafico_seir <- renderPlot({

    fig <- seir() %>% 
      pivot_longer(!c(fecha,group), names_to = "Compartimento", values_to = "Casos") %>% 
      ggplot(aes(x=fecha, y=Casos, color=Compartimento)) + 
      geom_line() + theme_bw() + facet_grid(~group)
    
    fig  + theme(legend.position = "top") 
      
  })
  
  output$grafico_i <- renderPlotly({
    
    proy <- data.frame(seir()[seir()$group=="g1",8],
                       seir()[seir()$group=="g2",8],
                       seir()[seir()$group=="g3",8])
    colnames(proy) <- c("inf0019","inf2064","inf6599")
    
    full <- union_all(data.frame(inf_p),proy)
    
    smooth <- 
      cbind(
        GRUPO_00_19=predict(loess(inf0019 ~ seq(1,nrow(full),1), data=full, span=0.10)),
        GRUPO_20_64=predict(loess(inf2064 ~ seq(1,nrow(full),1), data=full, span=0.10)),
        GRUPO_65_MAS=predict(loess(inf6599 ~ seq(1,nrow(full),1), data=full, span=0.10))
      )
    
    fig <- plot_ly(data.frame(smooth), x = ~seq(1,nrow(full),1), y = ~GRUPO_00_19, name = 'Menor de 20', type = 'scatter', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~GRUPO_20_64, name = '20 a 64', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~GRUPO_65_MAS, name = '65 y más', mode = 'lines')
    fig <- fig %>% layout(xaxis = list(title = "Día #"), yaxis = list(title = "Casos nuevos"))
    fig
    
    
  })

  
    

}
shinyApp(ui = ui, server = server)



