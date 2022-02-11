getUI <- function () {
  
  fluidPage(useShinyjs(),
            theme = shinytheme("flatly"),
            tags$head(HTML('<link rel="icon", href="ISO-IECS.png", type="image/png" />')),
            useWaiter(),
            titlePanel(windowTitle = "IECS: Proyecciones COVID-19", title = ""),
            fluidRow(
              br(),
              column(3,
                     fluidRow(
                       column(12,
                              tags$a(
                                img(src="iecslogo.png", height = 100, width = 300),
                                href="https://www.iecs.org.ar",
                                target="_blank"
                              )
                       )
                     ),
                     fluidRow(
                       column(6, style="padding-top: 20px;",
                              tags$a(
                                img(src="etslogo2.png", height = 65, width = 158),
                                href="https://www.iecs.org.ar/evaluacion-de-tecnologias-sanitarias-y-economia-de-la-salud/",
                                target="_blank"
                              ),
                       ),
                       column(6,
                              tags$a(
                                img(src="CIPSlogo.png", height = 88, width = 150),
                                href="https://www.iecs.org.ar/ciips/",
                                target="_blank"
                              ),
                       )
                     )
              ),
              column(9, 
                     h1("Impacto del COVID-19 en los sistemas de salud de Latinoamérica y el Caribe"),
                     h3("Proyecciones para la toma de decisiones públicas")
              )
            ),
            br(),
            br(),
            fluidRow(id="inputs", 
                     column(width = 4),
                     column(width = 4,
                            pickerInput("country", 
                                        "País", 
                                        multiple = F,
                                        choices = countries,
                                        choicesOpt = list(content =  
                                                            mapply(countries, flags, FUN = function(country, flagUrl) {
                                                              HTML(paste(
                                                                tags$img(src=flagUrl, width=20, height=15),
                                                                country
                                                              ))
                                                            }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                                          
                                        ),
                                        selected = "Argentina")
                            , align="center"),
                     column(width = 4)
                     ),
            br(),
            br(),
            fluidRow(br(),
                     br(),
                     br(),
                     column(5, 
                            offset = 1,
                            wellPanel(style = "height:330px;",
                                      em("Ultimos datos disponibles (", 
                                         tags$a(href="https://ourworldindata.org/", "OWD", target="_blank"), 
                                         ")",
                                         
                                      ), 
                                      fluidRow(
                                        column(
                                          4,
                                          tags$b("Población:"),tags$p(textOutput("population")),
                                        ),
                                        column(
                                          4,
                                          tags$b("Casos confirm. diarios:",
                                                 tags$span(id="info_Inf_diarios")),
                                          tags$p(textOutput("dailyCases")),
                                        ),
                                        column(
                                          4,
                                          tags$b("Muertes confirmadas diarias:",
                                                 tags$span(id="info_Mue_diarios")),
                                          tags$p(textOutput("dailyDeaths")),
                                        )
                                      ),
                                      
                                      fluidRow(
                                        column(
                                          4,
                                          tags$b("% de mayores de 65 años:"),tags$p(textOutput("populationOver65")),
                                        ),
                                        column(
                                          4,
                                          tags$b("Total casos confirmados:"),tags$p(textOutput("totalCases")),
                                        ),
                                        column(
                                          4,
                                          tags$b("Total muertes confirmadas:"),tags$p(textOutput("totalDeaths")),
                                        )
                                      ),
                                      
                                      fluidRow(
                                        column(
                                          4,
                                          tags$b("Esperanza de vida al nacer:"),tags$p(textOutput("lifeExp"))
                                        ),
                                        column(
                                          4,
                                          tags$b("Tests Totales c/ mill. hab."),tags$p(textOutput("totalTestPerMillon"))
                                        ),
                                        column(
                                          4,
                                          tags$b("Tests diarios (últ. dato disp.)"),tags$p(textOutput("dailyTests"))
                                        )
                                      ))
                    ),
                    column(5,
                       fluidRow(
                         column(12,
                            withSpinner(leafletOutput("map"),type = 8, size = .5, color = "#bdbdbd")
                         )
                       ),
                       fluidRow(
                         column(12,  align="right",
                                radioGroupButtons(
                                 inputId = "select_map",
                                 size="xs",
                                 choices = c("Casos", 
                                             "Defunciones"),
                                 status = "info")
                                )
                       )
                   )
            ),
            
            br(),
            hr(),
            fluidRow(column(3,radioGroupButtons(
                              inputId = "compart_checkbox",
                              choices = c("Casos"="i: Daily infectious", 
                                          "Defunciones"="d: Daily deaths", 
                                          "Camas UCI"="Ic: Infectious (severe)"),
                              status = "success"), 
                            align="left"),
                     column(1),
                     column(3,prettyCheckbox(inputId = "check_cases",label = "Mostrar casos reportados",value = FALSE),
                              prettyCheckbox(inputId = "check_deaths",label = "Mostrar defunciones reportadas",value = FALSE),
                              prettyCheckbox(inputId = "check_rt",label = "Mostrar número de reproducción (Rt)",value = FALSE)),
                     column(1),
                     column(4,selectInput("edad","Grupos de edad",
                                          choices=c("Todos"="total",ageGroups),
                                          multiple = T,
                                          selected= c("total")), align="left"
                     )
            ),
            fluidRow(column(12,withSpinner(plotlyOutput("graficoUnico"),type = 8, size = .5, color = "#bdbdbd"),
                     )
            ),
            br(),
            fluidRow(column(10)),
            fluidRow(column(
              width = 12,
              fluidRow(
                column(
                  width = 6,
                  # sliderInput(
                  #   "uptakeSlider",
                  #   "Vaccine uptake:",
                  #   width = "100%",
                  #   min = 0,
                  #   max = 100,
                  #   step = 10,
                  #   value = 50
                  # ),
                  sliderTextInput(
                    inputId = "uptakeSlider",
                    label = "Cobertura de vacunas:",
                    width = "100%",
                    choices = c("0%",
                                "20%",
                                "50%",
                                "80%",
                                "95%"),
                    selected = "80%"
                  ),
                  tags$small("% de la población a vacunar hasta el fin de 2021")
                ),
                column(
                  width = 6,
                  sliderTextInput(
                    inputId = "effectivenessSlider",
                    label = "Efectividad de la vacuna:",
                    width = "100%",
                    choices = c("Baja"="Low",
                                "Media"="Middle",
                                "Alta"="High"),
                    selected = "Middle"
                  ),
                  tags$small(
                    "* Eficacia (muerte, caso critico, hospitalización) Alta: 100%, 80%, 80%; Media: 100%, 80%, 50%; Baja: 80%, 50%, 50%"
                  )
                )
              ),
              # fluidRow(column(2,actionButton("run_basico","",icon = icon("refresh")))),
              br(),
              p("Configurar medidadas de restricción durante 2021 y 2022:"),
              fluidRow(column(
                4, style = "padding-right: 15px;",
                div(
                  actionButton(
                    "npi1",
                    HTML("<b>Distanciamiento social, uso de mascarillas faciales</b>"),
                    style = paste0(
                      "color: #222426; background-color: ",
                      colores[1],
                      "; border-color: #2e6da4; margin: 5px; width: 100%;"
                    )
                  ),
                  br(),
                  actionButton(
                    "npi2",
                    HTML("<b>Agrega aislamiento de ancianos</b>"),
                    style = paste0(
                      "color: #222426; background-color: ",
                      colores[2],
                      "; border-color: #2e6da4; margin: 5px; width: 100%;"
                    )
                  ),
                  br(),
                  actionButton(
                    "npi3",
                    HTML("<b>Agrega aislamiento personal</b>"),
                    style = paste0(
                      "color: #222426; background-color: ",
                      colores[3],
                      "; border-color: #2e6da4; margin: 5px; width: 100%;"
                    )
                  ),
                  br(),
                  actionButton(
                    "npi4",
                    HTML("<b>Agrega cierre de escuelas</b>"),
                    style = paste0(
                      "color: #222426; background-color: ",
                      colores[4],
                      "; border-color: #2e6da4; margin: 5px; width: 100%;"
                    )
                  ),
                  br(),
                  actionButton(
                    "npi5",
                    HTML("<b>Implementa confinamiento total</b>"),
                    style = paste0(
                      "color: #222426; background-color: ",
                      colores[5],
                      "; border-color: #2e6da4; margin: 5px; width: 100%;"
                    )
                  ),
                  br(),
                  # actionButton(
                  #   "npi6",
                  #   HTML("<b>Lockdown</b>"),
                  #   style = paste0(
                  #     "color: #222426; background-color: ",
                  #     colores[6],
                  #     "; border-color: #2e6da4; margin: 5px; width: 80%;"
                  #   )
                  # ),
                ),
              ),
              column(id="npis-col",
                8,
                fluidRow(
                  tags$small(
                    "Clickee en la mediddas para incorporarlas a la proyección a partir del primer mes (Enero 2021), 
                    siga agregando medidas para cada mes, o deje definina la ultima que se prolongrá hasta el final de la proyección"
                  ),
                ),
                fluidRow(id = "npis-output",
                         div(id = "tail", tags$span("Seleccionar medidas..."), style = 'float:left;')
                 ),
              ))
              
            )
           ),
           actionButton(inputId = "go", label = "Actualizar"),
           actionButton(inputId = "reset", label = "Borrar medidas"),
           
            br(),
            br(),
            
            tabsetPanel(id="TSP",
                        type = "tabs",
                        tabPanel("Tabla de resultados",
                                 column(8,DTOutput("resumen_tabla"), offset = 2)
                                 ),
                        tabPanel("Evaluación Económica",
                                 br(),
                                 fluidRow(
                                   #column(3),
                                   #column(6,dataTableOutput("eeTable")),
                                   column(3, actionLink("EEgo", "Ver tabla de resultados principales"),
                                          align="left")
                                 ),
                                 fluidRow(column(10, tableOutput("EESummaryTable"), align="center")),
                                 br(),
                                 fluidRow(column(10, tableOutput("EESummaryTable2"), align="center")),
                                 br(),
                                 fluidRow(column(10, tableOutput("EESummaryTable3"), align="center")),
                                 fluidRow(column(7),
                                          column(3, downloadButton("downloadEE", "Descargar"), align="right")
                                 )
                                 
                        ),
                        tabPanel("Análisis de sensibilidad",
                          tabsetPanel(id="senstab",
                                      type = "tabs",
                                      tabPanel("Análisis de sensibilidad (Evaluación Económica)", 
                                               id="senstabEE",
                                               fluidRow(
                                                 column(12,align="center",
                                                        h4("Seleccionar los parámetros a incluir en el análisis de sensibilidad"),
                                                        tags$span("Peor escenario", style="color: red; font-weight: bold;"),
                                                        tags$span(" - "),
                                                        tags$span("Mejor escenario", style="color: green; font-weight: bold;"),
                                                        br(),br(),br(),br(),
                                                 )
                                               ),
                                               fluidRow(column(12,
                                                               fluidRow(column(2,
                                                                               align="right",
                                                                               fluidRow(br()),
                                                                               fluidRow(br()),
                                                                               fluidRow(
                                                                                 switchInput("check_costos", 
                                                                                             onLabel = "ON",
                                                                                             offLabel = "OFF",
                                                                                             value = TRUE)
                                                                                 )
                                                                               ),
                                                                        column(2, align="right",
                                                                               br(),br(),
                                                                               tags$span("Mejor", 
                                                                                          style="color: green; font-weight: bold;")
                                                                               ),
                                                                        column(6,
                                                                               sliderInput("costosSens", 
                                                                                           tags$span("Costo de las vacunas", style="font-weight: bold;"),
                                                                                           min = -.25, max = .25, 
                                                                                           value = c(-.25,.25), step = 0.01, dragRange = T,
                                                                                           width = "100%"
                                                                               ),
                                                                               br(),
                                                                        ),
                                                                        column(2,
                                                                               br(),br(),
                                                                               tags$span("Peor", 
                                                                                         style="color: red; font-weight: bold;"))
                                                                        ),
                                                               fluidRow(column(2,
                                                                               align="right",
                                                                               fluidRow(br()),
                                                                               fluidRow(br()),
                                                                               fluidRow(
                                                                                 switchInput("check_cobertura", 
                                                                                             onLabel = "ON",
                                                                                             offLabel = "OFF",
                                                                                             value = TRUE)
                                                                               )
                                                               ),
                                                               column(2, align="right",
                                                                      br(),br(),
                                                                      tags$span("Peor", 
                                                                                style="color: red; font-weight: bold;")
                                                               ),
                                                               column(6,
                                                                      sliderInput("coberturaSens", 
                                                                                  tags$span("Cobertura de la vacunación", style="font-weight: bold;"),
                                                                                  min = -.25, max = .25, 
                                                                                  value = c(-.25,.25), step = 0.01, dragRange = T,
                                                                                  width = "100%"
                                                                      ),
                                                                      br(),
                                                               ),
                                                               column(2,
                                                                      br(),br(),
                                                                      tags$span("Mejor", 
                                                                                style="color: green; font-weight: bold;")
                                                                      )
                                                               ),
                                                              
                                                               
                                                        fluidRow(column(2,
                                                                        align="right",
                                                                        fluidRow(br()),
                                                                        fluidRow(br()),
                                                                        fluidRow(
                                                                          switchInput("check_efectividad", 
                                                                                      onLabel = "ON",
                                                                                      offLabel = "OFF",
                                                                                      value = TRUE)
                                                                        )
                                                        ),
                                                        column(2, align="right",
                                                               br(),br(),
                                                               tags$span("Peor", 
                                                                         style="color: red; font-weight: bold;")
                                                        ),
                                                        column(6,
                                                               sliderInput("efectividadSens",
                                                                           tags$span("Efectividad de las vacunas", style="font-weight: bold;"),
                                                                           min = -.25, max = .25, 
                                                                           value = c(-.25,.25), step = 0.01, dragRange = T,
                                                                           width = "100%"
                                                               ),
                                                               br(),
                                                        ),
                                                        column(2,
                                                               br(),br(),
                                                               tags$span("Mejor", 
                                                                         style="color: green; font-weight: bold;")
                                                               )
                                                        ),
                                                        fluidRow(column(2,
                                                                        align="right",
                                                                        fluidRow(br()),
                                                                        fluidRow(br()),
                                                                        fluidRow(
                                                                          switchInput("check_costo_eventos", 
                                                                                      onLabel = "ON",
                                                                                      offLabel = "OFF",
                                                                                      value = TRUE)
                                                                        )
                                                        ),
                                                        column(2, align="right",
                                                               br(),br(),
                                                               tags$span("Mejor", 
                                                                         style="color: green; font-weight: bold;")
                                                        ),
                                                        column(6,
                                                               sliderInput("costoEventosSens", 
                                                                           tags$span("Costo de los eventos", style="font-weight: bold;"),
                                                                           min = -.25, max = .25, 
                                                                           value = c(-.25,.25), step = 0.01, dragRange = T,
                                                                           width = "100%"
                                                               ),
                                                               br(),
                                                        ),
                                                        column(2,
                                                               br(),br(),
                                                               tags$span("Peor", 
                                                                         style="color: red; font-weight: bold;")
                                                               )
                                                        ),
                                                        fluidRow(
                                                          column(12,align="center",
                                                                 actionButton("runWithSensEE", 
                                                                              label = "Ejecutar análisis de sensibilidad", 
                                                                              # icon = icon("chevron-right"), 
                                                                              class = "btn-primary", style = "margin: 5px;")
                                                          ),
                                                        ),
                                                        br(),
                                                        br(),
                                                        fluidRow(column(12, tableOutput("EETableSens")))
                                               )
                                            )
                                      ),
                                      tabPanel("Análisis de sensibilidad (Epidemiológico)", id="senstabEPI",
                                               fluidRow(
                                                 column(12,align="center",
                                                        h4("Seleccionar los parámetros a incluir en el análisis de sensibilidad"),
                                                        tags$span("Peor escenario", style="color: red; font-weight: bold;"),
                                                        tags$span(" - "),
                                                        tags$span("Mejor escenario", style="color: green; font-weight: bold;"),
                                                        br(),br(),br(),br(),
                                                 )
                                               ),
                                               fluidRow(
                                                 column(12,
                                                        fluidRow(
                                                          column(2, align="right",
                                                                 fluidRow(br()),
                                                                 fluidRow(br()),
                                                                 fluidRow(
                                                                   switchInput("check_transmissionEffectivenessSens", 
                                                                               onLabel = "ON",
                                                                               offLabel = "OFF",
                                                                               value = TRUE),
                                                                 )
                                                          ),
                                                          column(2, align="right",
                                                                 br(),br(),
                                                                 htmlOutput("transEffSens_low", 
                                                                            style="color: green; font-weight: bold;")),
                                                          column(6,
                                                                 sliderInput("transmissionEffectivenessSens", htmlOutput("base_transmissionEffectivenessSens"),
                                                                             min = -1, max = 1, 
                                                                             value = c(-1,1), step = 0.01, dragRange = T,
                                                                             width = "100%"
                                                                 ),
                                                                 br(),
                                                          ),
                                                          column(2,
                                                                 br(),br(),
                                                                 htmlOutput("transEffSens_hi", 
                                                                            style="color: red; font-weight: bold;")),
                                                        ),
                                                        fluidRow(
                                                          column(2, align="right",
                                                                 fluidRow(br()),
                                                                 fluidRow(br()),
                                                                 fluidRow(
                                                                   switchInput("check_ifrSens", 
                                                                               onLabel = "ON",
                                                                               offLabel = "OFF",
                                                                               value = TRUE),
                                                                 )
                                                          ),
                                                          column(2,align="right", br(), br(),
                                                                 htmlOutput("ifrSens_low", 
                                                                            style="color: green; font-weight: bold;")),
                                                          column(6,
                                                                 sliderInput("ifrSens", htmlOutput("base_ifrSens"),
                                                                             min = -1, max = 1, 
                                                                             value = c(-1,1), step = 0.01, dragRange = T,
                                                                             width = "100%"
                                                                 ),
                                                          ),
                                                          column(2,align="left",br(),br(),
                                                                 htmlOutput("ifrSens_hi", 
                                                                            style="color: red; font-weight: bold;")),
                                                        ),
                                                        fluidRow(
                                                          column(2, align="right",
                                                                 fluidRow(br()),
                                                                 fluidRow(br()),
                                                                 fluidRow(
                                                                   switchInput("check_complicacionesSensSevere", 
                                                                               onLabel = "ON",
                                                                               offLabel = "OFF",
                                                                               value = TRUE),
                                                                 )
                                                          ),
                                                          column(2, align="right",
                                                                 br(),br(),
                                                                 htmlOutput("complicacionesSensSevere_low", 
                                                                            style="color: green; font-weight: bold;")),
                                                          column(6,
                                                                 sliderInput("complicacionesSensSevere", 
                                                                             htmlOutput("base_complicacionesSensSevere"),
                                                                             min = -1, max = 1, 
                                                                             value = c(-1,1), step = 0.01, dragRange = T,
                                                                             width = "100%"
                                                                 ),
                                                          ),
                                                          column(2,align="left",br(),br(), 
                                                                 htmlOutput("complicacionesSensSevere_hi", 
                                                                            style="color: red; font-weight: bold;")),
                                                        ),
                                                        fluidRow(
                                                          column(2, align="right",
                                                                 fluidRow(br()),
                                                                 fluidRow(br()),
                                                                 fluidRow(
                                                                   switchInput("check_complicacionesSensCritic", 
                                                                               onLabel = "ON",
                                                                               offLabel = "OFF",
                                                                               value = TRUE),
                                                                 )
                                                          ),
                                                          column(2, align="right",
                                                                 br(),br(),
                                                                 htmlOutput("complicacionesSensCritic_low", 
                                                                            style="color: green; font-weight: bold;")),
                                                          column(6,
                                                                 sliderInput("complicacionesSensCritic", 
                                                                             htmlOutput("base_complicacionesSensCritic"),
                                                                             min = -1, max = 1, 
                                                                             value = c(-1,1), step = 0.01, dragRange = T,
                                                                             width = "100%"
                                                                 ),
                                                          ),
                                                          column(2,align="left",br(),br(), 
                                                                 htmlOutput("complicacionesSensCritic_hi", 
                                                                            style="color: red; font-weight: bold;")),
                                                        ),
                                                        fluidRow(
                                                          column(2, align="right",
                                                                 fluidRow(br()),
                                                                 fluidRow(br()),
                                                                 fluidRow(
                                                                   switchInput("check_tiempoPSens", 
                                                                               onLabel = "ON",
                                                                               offLabel = "OFF",
                                                                               value = TRUE),
                                                                 )
                                                          ),
                                                          column(2, align="right",
                                                                 br(),br(),
                                                                 htmlOutput("tiempoPSens_low", 
                                                                            style="color: red; font-weight: bold;")),
                                                          column(6,
                                                                 sliderInput("tiempoPSens", 
                                                                             htmlOutput("base_tiempoPSens"),
                                                                             min = -1, max = 1, 
                                                                             value = c(-1,1), step = 0.01, dragRange = T,
                                                                             width = "100%"
                                                                 ),
                                                          ),
                                                          column(2,align="left",br(),br(), 
                                                                 htmlOutput("tiempoPSens_hi", 
                                                                            style="color: green; font-weight: bold;")),
                                                        ),
                                                        fluidRow(
                                                          column(2, align="right",
                                                                 fluidRow(br()),
                                                                 fluidRow(br()),
                                                                 fluidRow(
                                                                   switchInput("check_wainingSens", 
                                                                               onLabel = "ON",
                                                                               offLabel = "OFF",
                                                                               value = TRUE),
                                                                 )
                                                          ),
                                                          column(2, align="right",
                                                                 br(),br(),
                                                                 htmlOutput("wainingSens_low", 
                                                                            style="color: red; font-weight: bold;")),
                                                          column(6,
                                                                 sliderInput("wainingSens", htmlOutput("base_wainingSens"),
                                                                             min = -1, max = 1, 
                                                                             value = c(-1,1), step = 0.01, dragRange = T,
                                                                             width = "100%"
                                                                 ),
                                                          ),
                                                          column(2,align="left",br(),br(), 
                                                                 htmlOutput("wainingSens_hi", 
                                                                            style="color: green; font-weight: bold;")),
                                                        ),
                                                        fluidRow(
                                                          column(12,align="center",
                                                                 actionButton("runWithSens", 
                                                                              label = "Ejecutar análisis de sensibilidad", 
                                                                              # icon = icon("chevron-right"), 
                                                                              class = "btn-primary", style = "margin: 5px;")
                                                          ),
                                                        ),
                                                        # fluidRow(
                                                        #   column(12,align="center",
                                                        #          actionButton("exportSens", 
                                                        #                       label = "Export projection with sensitivity", 
                                                        #                       # icon = icon("chevron-right"), 
                                                        #                       class = "btn-primary", style = "margin: 5px;")
                                                        #   ),
                                                        # ),
                                                        fluidRow(
                                                          column(12,align="center",
                                                                 withSpinner(plotlyOutput("plotWithSens"),type = 8, size = .5, color = "#bdbdbd")
                                                          ),
                                                        )
                                                 )
                                               )
                                      )
                          )
                        ),
                        tabPanel("Recursos",
                                 br(),
                                 br(),
                                 br(),
                                 fluidRow(
                                   column(3),
                                   column(3,uiOutput("CC_set"), align="center"),
                                   column(3,uiOutput("Vent_set"), align="center"),
                                   column(3)
                                   ),
                                 br(),
                                 br(),
                                 br(),
                                 fluidRow(
                                   column(3),
                                   column(3,uiOutput("Porc_crit"), align="center"),
                                   column(3,uiOutput("Vent_por_CC"), align="center"),
                                   column(3)
                                 )
                        ),
                        tabPanel("Configración avanzada",
                                 br(),
                                 fluidRow(column(3,selectInput("compart_a_graficar","Compartment",choices = NULL)),
                                         
                                          
                                          
                                          column(4,fluidRow(column(4,textInput("save_comp_name", "Save scenario", placeholder = "Enter name")),
                                                            column(2,br(),actionButton("save_comp", icon("chevron-right")))
                                                            )
                                          )
                                 ),
                                 
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Scenario configuration",
                                                      fluidRow(column(4,
                                                                      numericInput("t", label = "Day of projection", value = 1, step = 1),
                                                                      selectInput(
                                                                        "vacUptake",
                                                                        label="Vaccination uptake",
                                                                        choices = c("High uptake: 95%",
                                                                                    "High uptake: 80%",
                                                                                    "Mid-range uptake: 50%",
                                                                                    "Low uptake: 20%",
                                                                                    "No vaccination"),
                                                                        selected = "Mid-range uptake: 50%"
                                                                      ),
                                                                      selectInput(
                                                                        "vacDateGoal",
                                                                        label="Vaccination date goal",
                                                                        choices = c("Mid 2021"="2021-06-30",
                                                                                    "End 2021"="2021-12-31",
                                                                                    "Mid 2022"="2022-05-30",
                                                                                    "End 2022"="2022-12-31"),
                                                                        selected = "2021-12-31"
                                                                      ),
                                                                      selectInput(
                                                                        "vacStrat",
                                                                        label="Vaccination priorities",
                                                                        choices = c("Priority: older -> adults -> young",
                                                                                    "Priority: older + adults -> young",
                                                                                    "Priority: adults -> older -> young",
                                                                                    "Priority: school age -> others",
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
                                                                      )
                                                                      # ,
                                                                      # selectInput(
                                                                      #   "immunityDuration",
                                                                      #   label="Immunity duration",
                                                                      #   choices = c("6 months"=180,
                                                                      #               "1 year"=360,
                                                                      #               "Lifelong"=2000)
                                                                      # )
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
                                                                           "Combined with schools closed",
                                                                           "Combined with schools open",
                                                                           "Intensive interventions with schools closed",
                                                                           "Intensive interventions with schools open",
                                                                           "Lockdown"),
                                                               selected = "Combined with schools closed"
                                                             ),
                                                             selectInput(
                                                               "npiScenarioRelaxed",
                                                               label="Relaxed NPI Scenario",
                                                               choices = c("Baseline",
                                                                           "School closures",
                                                                           "Physical distancing",
                                                                           "Shielding of older people",
                                                                           "Self-isolation",
                                                                           "Combined with schools closed",
                                                                           "Combined with schools open",
                                                                           "Intensive interventions with schools closed",
                                                                           "Intensive interventions with schools open",
                                                                           "Lockdown"),
                                                               selected = "Combined with schools closed"
                                                             ),
                                                             selectInput(
                                                               "relaxationThreshold",
                                                               label="Relaxation Threshold",
                                                               choices = c("No relaxation"=1.1,
                                                                           "80%"=.8,
                                                                           "60%"=.6,
                                                                           "50%"=.5,
                                                                           "40%"=.4)
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
                                                                         step=.1))
                                                      )
                                             ),
                                             tabPanel("Other parameters",
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
                                             tabPanel("Topic II - Question 1",
                                                      div(
                                                        p("In which order should the following groups beprioritized for COVID-19 
                                                    vaccination as vaccine supply increases so as to keep hospitalizations, 
                                                    and intensive care unit use (where available), due to COVID-19 and other 
                                                    background causes below maximum hospital capacity in the setting(s) modeled? (50% Uptake)"),
                                                        radioButtons("simpleCompartSelector2q1", "Graph:",
                                                                     c("Daily cases (morbidity):" = "i",
                                                                       "Daily deaths (mortality)" = "d",
                                                                       "Hospitalizations in ICU" = "Ic"),
                                                                     inline=T),
                                                        actionButton("q1_older", label = "Older population first", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;"),
                                                        actionButton("q1_adult", label = "Adult population first", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;"),
                                                        actionButton("q1_school", label = "School aged population first", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;")),
                                                      
                                                      column(6,DTOutput("resumen_tabla2"))
                                             ),
                                             tabPanel("Topic II - Question 2",
                                                      div(
                                                        p("What would be the optimal vaccination strategy in combination with which 
                                                    non-pharmaceutical interventions to keep hospitalizations due to COVID-19 
                                                    and other background causes below maximum hospital capacity in the settings 
                                                    modelled?"),
                                                        radioButtons("simpleCompartSelector2q2", "Graph:",
                                                                     c("Daily cases (morbidity):" = "i",
                                                                       "Daily deaths (mortality)" = "d",
                                                                       "Hospitalizations in ICU" = "Ic"),
                                                                     inline=T),
                                                        actionButton("q2_80_low", label = "Uptk 80%, intensive NPIs, schools open", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;"),
                                                        actionButton("q2_80_mid", label = "Uptk 80%, intensive NPIs, schools closed", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;"),
                                                        actionButton("q2_80_high", label = "Uptk 80%, combined NPIs", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;"),
                                                        br(),
                                                        actionButton("q2_50_low", label = "Uptk 50%, intensive NPIs, schools open", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;"),
                                                        actionButton("q2_50_mid", label = "Uptk 50%, intensive NPIs, schools closed", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;"),
                                                        actionButton("q2_50_high", label = "Uptk 50%, combined NPIs", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;"),
                                                        br(),
                                                        actionButton("q2_20_low", label = "Uptk 20%, intensive NPIs, schools open", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;"),
                                                        actionButton("q2_20_mid", label = "Uptk 20%, intensive NPIs, schools closed", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;"),
                                                        actionButton("q2_20_high", label = "Uptk 20%, combined NPIs", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;"),
                                                        br(),
                                                        tags$small("Davies, Nicholas G., et al. \"Effects of non-pharmaceutical interventions on COVID-19 cases, deaths, and demand for hospital services in the UK: a modelling study.\" The Lancet Public Health 5.7 (2020): e375-e385."),
                                                        br(),
                                                        column(6,DTOutput("resumen_tabla3"))
                                                      )
                                             ),
                                             tabPanel("Topic IV",
                                                      div(
                                                        p("Question 1: What would be the effect on SARS-CoV-2 infections, COVID-19 hospitalizations 
                                                              (including peak demand), and COVID-19 morbidity and mortality 
                                                              of lifting non-pharmaceutical interventions (e.g., business closures, 
                                                              school closures, travel restrictions, gathering size limits, mask wearing) 
                                                              at different levels of vaccine efficacy and vaccination coverage for different 
                                                              priority groups as outlined in the SAGE Prioritization Roadmap?"),
                                                        p("Question 2: What combinations of vaccine efficacy and vaccination coverage in different priority groups as 
                                                              outlined in the SAGE Prioritization Roadmap together with which combinations of non-pharmaceutical interventions 
                                                              could keep Rt below 1?"),
                                                        p("Question 3: What is the probability and stability of local or regional elimination of SARS-CoV-2 transmission 
                                                              under different scenarios of (i) vaccine efficacy and vaccination coverage in different priority 
                                                              groups as outlined in the SAGE Prioritization Roadmap and (ii) different combinations of 
                                                              non-pharmaceutical interventions?"),
                                                        radioButtons("simpleCompartSelector4q", "Graph:",
                                                                     c("Daily cases (morbidity):" = "i",
                                                                       "Daily deaths (mortality)" = "d",
                                                                       "Hospitalizations in ICU" = "Ic"),
                                                                     inline=T),
                                                        fluidRow(
                                                          column(3,
                                                                 selectInput(
                                                                   "t4q1_uptake",
                                                                   label="Coverage",
                                                                   choices = c("High",
                                                                               "Middle",
                                                                               "Low"),
                                                                   selected = "Middle"
                                                                 )
                                                          ),
                                                          column(3,
                                                                 selectInput(
                                                                   "t4q1_efficacy",
                                                                   label="Efficacy",
                                                                   choices = c("High",
                                                                               "Middle",
                                                                               "Low"),
                                                                   selected = "Middle"
                                                                 )
                                                          ),
                                                          column(3,
                                                                 selectInput(
                                                                   "t4q1_priority",
                                                                   label="Priority",
                                                                   choices = c("Older -> adults -> young",
                                                                               "Adults -> older -> young",
                                                                               "Young -> others"),
                                                                   selected = "Older -> adults -> young"
                                                                 )
                                                          )
                                                        ),
                                                        fluidRow(
                                                          column(3,
                                                                 selectInput(
                                                                   "t4q1_npis",
                                                                   label="NPIs Strength",
                                                                   choices = c("High",
                                                                               "Middle",
                                                                               "Low"),
                                                                   selected = "Middle"
                                                                 )
                                                          ),
                                                          column(3,
                                                                 selectInput(
                                                                   "t4q1_npisRelaxThreshold",
                                                                   label="Relax threshold (Uptk % in older)",
                                                                   choices = c("No relaxation",
                                                                               "80%",
                                                                               "50%",
                                                                               "40%"),
                                                                   selected = "No relaxation"
                                                                 )
                                                          ),
                                                          column(3,
                                                                 selectInput(
                                                                   "t4q1_npisRelaxed",
                                                                   label="NPIs Strength",
                                                                   choices = c("High",
                                                                               "Middle",
                                                                               "Low"),
                                                                   selected = "Middle"
                                                                 )
                                                          ),
                                                        ),
                                                        actionButton("t4q1_project", label = "Project scenario", icon = icon("chevron-right"), class = "btn-primary", style = "margin: 5px;"),
                                                        br(),br(),
                                                        tags$small("* High coverage: 80%; Middle: 50%; Low: 20%;"),br(),
                                                        tags$small("* High efficary(death, severe, moderate): 100%, 80%, 80%; Middle: 80%, 80%, 50%; Low: 80%, 50%, 50%"),br(),
                                                        tags$small("* High NPIs: combined; Middle: intensive schools closed; Low: intensive schools open"),br(),
                                                        tags$small("Davies, Nicholas G., et al. \"Effects of non-pharmaceutical interventions on COVID-19 cases, deaths, and demand for hospital services in the UK: a modelling study.\" The Lancet Public Health 5.7 (2020): e375-e385."),
                                                        br(),
                                                        column(6,DTOutput("resumen_tabla4"))
                                                      )
                                                      
                                             ),
                                             tabPanel("Reference",
                                                      tags$small("Davies, Nicholas G., et al. \"Effects of non-pharmaceutical interventions on COVID-19 cases, deaths, and demand for hospital services in the UK: a modelling study.\" The Lancet Public Health 5.7 (2020): e375-e385."),
                                                      tags$img(src='davies2.jpg')
                                             )
                                 )
                        ),
                        tabPanel("Vacunación",
                                 fluidRow(column(3,selectInput("compartVac",
                                                               "Compartimento:",
                                                               choices = c("Susceptibles vacunados"="S_vac",
                                                                           "Susceptibles 1"="S1",
                                                                           "Susceptibles 2"="S2",
                                                                           "Susceptibles 3"="S3",
                                                                           "Susceptibles 4"="S4",
                                                                           "U total"="utotal",
                                                                           "Vacunados"="V")))),
                                 fluidRow(column(12,
                                                 plotlyOutput("graficoVac"))))
                        # tabPanel("Escenarios guardados", id="SE", 
                        #          fluidRow(column(2,selectInput("saved_series", "Saved series", choices="", multiple = T)),
                        #                   column(3,
                        #                          br(),
                        #                          prettyCheckbox("icu_beds","Show ICU beds endowment"))),
                        #          #selectInput("age_groups_comp", "Age groups", choices=c("All ages"="total",ageGroups)),
                        #          plotlyOutput("graficoComp"),
                        #          br(),
                        #          div(uiOutput("tables")),
                        #          br(),
                        #          column(2,actionButton("del_scenarios","Delete all scenarios"))),
                        # tabPanel("Compartmientos", fluidRow(id="content"))
            ),
            # fluidRow(column(12,id="content")),
            br(),
            br(),
            br(),
            br(),
            br()
  )
}
  
