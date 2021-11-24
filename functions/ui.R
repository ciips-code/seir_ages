getUI <- function () {
  
  fluidPage(theme = bs_theme(bootswatch = "cerulean"),
            useShinyjs(),
            tags$style(type = "text/css", "
              .irs-bar {width: 100%; height: 25px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}
              .irs-bar-edge {background: black; border: 1px solid black; height: 25px; border-radius: 0px; width: 20px;}
              .irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
              .irs-grid-text {font-family: 'arial'; color: white !important; bottom: 17px; z-index: 1;}
              .irs-grid-pol {display: none;}
              .irs-max {font-family: 'arial'; color: white !important; background-color: white !important;}
              .irs-min {font-family: 'arial'; color: white !important; background-color: white !important;}
              .irs-from {font-family: 'arial'; color: white !important; background-color: white !important;}
              .irs-to {font-family: 'arial'; color: white !important; background-color: white !important;}
              .irs-single {color:black; background:#6666ff;}
              .irs-slider {width: 30px; height: 30px; top: 22px;}
            "),
            useWaiter(),
            extendShinyjs(text = jsResetCode, functions = "reset"),
            fluidRow(id="inputs", 
                     column(width = 4,
                            h2("IECS - CIIPS: Vaccines Impact Modeling")
                            , align="left"),
                     column(width = 3,
                            pickerInput("country", 
                                        "Country", 
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
                            , align="left"),
                     column(width = 3,
                            numericInput("t", label = "Day of projection", value = 1, step = 1)
                            , align="right"),
                     column(width = 2,
                            br(),
                            # actionButton("submit", label = NULL, icon = icon("refresh")),
                            actionButton("prev", label = NULL, icon = icon("chevron-left")),
                            actionButton("prox", label = NULL, icon = icon("chevron-right"))
                            , align="left")
            ),
            tabsetPanel(id="TSP",
                        type = "tabs",
                        tabPanel("Graphs",
                                 br(),
                                 fluidRow(column(3,selectInput("compart_a_graficar","Compartment",choices = NULL)),
                                          column(2,selectInput("edad","Age groups",
                                                               choices=c("All ages"="total",ageGroups),
                                                               multiple = T,
                                                               selected= c("total"))),
                                          
                                          
                                          column(4,fluidRow(column(4,textInput("save_comp_name", "Save scenario", placeholder = "Enter name")),
                                                            column(2,br(),actionButton("save_comp", icon("chevron-right"))),
                                                            column(2,prettyCheckbox(inputId = "check_cases",label = "Show reported cases",value = FALSE),
                                                                   prettyCheckbox(inputId = "check_deaths",label = "Show reported deaths",value = FALSE),
                                                                   prettyCheckbox(inputId = "check_rt",label = "Effective reproduction number (Rt)",value = FALSE)))
                                          )
                                 ),
                                 plotlyOutput("graficoUnico"),
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Scenario configuration",
                                                      fluidRow(column(4,
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
                                                                         step=.1)),
                                                      column(4,DTOutput("resumen_tabla"))
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
                                             tabPanel("Sensitivity analysis", id="senstab",
                                                      fluidRow(
                                                        column(12,align="center",
                                                               h4("Select parameters for sensitivity analysis"),
                                                               tags$span("Worst case scenario", style="color: red; font-weight: bold;"),
                                                               tags$span(" - "),
                                                               tags$span("Best case scenario", style="color: green; font-weight: bold;"),
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
                                                                                     label = "Run projection with sensitivity", 
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
                                                                        plotlyOutput("plotWithSens")
                                                                 ),
                                                               )
                                                        )
                                                      )
                                             ),
                                             tabPanel("EE",
                                                      br(),
                                                      fluidRow(
                                                        column(3),
                                                        column(6,dataTableOutput("eeTable")),
                                                        column(3)
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
                        tabPanel("Saved scenarios", id="SE", 
                                 fluidRow(column(2,selectInput("saved_series", "Saved series", choices="", multiple = T)),
                                          column(3,
                                                 br(),
                                                 prettyCheckbox("icu_beds","Show ICU beds endowment"))),
                                 #selectInput("age_groups_comp", "Age groups", choices=c("All ages"="total",ageGroups)),
                                 plotlyOutput("graficoComp"),
                                 br(),
                                 div(uiOutput("tables")),
                                 br(),
                                 column(2,actionButton("del_scenarios","Delete all scenarios"))),
                        tabPanel("Compartments", fluidRow(id="content"))
            ),
            # fluidRow(column(12,id="content")),
            br(),
            br(),
            br(),
            br(),
            br()
  )
}