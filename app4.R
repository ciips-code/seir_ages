#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram

colores = rev(c("#FF2929","#FF7025","#FFB822",
            "#FFFF1E","#8FFF26","#1EFF2D"))

ui <- fluidPage(theme = bs_theme(bootswatch = "cerulean"),

    # Application title
    titlePanel("Sample Data"),
    # Sidebar with a slider input for number of bins 
    # sliderInput("bins",
    #             "Number of bins:",
    #             min = 1,
    #             max = 50,
    #             value = 30),
    # Show a plot of the generated distribution
   plotOutput("distPlot"),
   column(width = 12,
        fluidRow(
            column(width = 6,
                sliderInput("uptake",
                            "Vaccine uptake:",
                            width="100%",
                            min = 0,
                            max = 100,
                            step = 10,
                            value = 50
                ),
                tags$small("% of population with complete schemas in the end of 2021")
            ),
            column(width = 6,
                   sliderTextInput(
                       inputId = "mySliderText",
                       label = "Vaccine effectiveness:",
                       width="100%",
                       choices = c(
                           "Low", 
                           "Middle", 
                           "High"),
                       selected = "Middle"
                   ),
                   tags$small("* High efficary(death, severe, moderate): 100%, 80%, 80%; Middle: 80%, 80%, 50%; Low: 80%, 50%, 50%")
            )
        ),
        br(),
        p("Cofigure NPIs (Non Pharmaceutical Interventions) during 2021:"),
        fluidRow(
        column(3,
               div(
                   actionButton("npi1",  HTML("<b>Baseline</b>"),
                                style=paste0("color: #222426; background-color: ",colores[1],"; border-color: #2e6da4; margin: 5px; width: 80%;")),
                   br(),
                   actionButton("npi2",  HTML("<b>Self-isolation</b>"),
                                style=paste0("color: #222426; background-color: ",colores[2],"; border-color: #2e6da4; margin: 5px; width: 80%;")),
                   br(),
                   actionButton("npi3",  HTML("<b>Shielding older pop.</b>"),
                                style=paste0("color: #222426; background-color: ",colores[3],"; border-color: #2e6da4; margin: 5px; width: 80%;")),
                   br(),
                   actionButton("npi4",  HTML("<b>Physical distancing</b>"),
                                style=paste0("color: #222426; background-color: ",colores[4],"; border-color: #2e6da4; margin: 5px; width: 80%;")),
                   br(),
                   actionButton("npi5",  HTML("<b>School closures</b>"),
                                style=paste0("color: #222426; background-color: ",colores[5],"; border-color: #2e6da4; margin: 5px; width: 80%;")),
                   br(),
                   actionButton("npi6",  HTML("<b>Lockdown</b>"),
                                style=paste0("color: #222426; background-color: ",colores[6],"; border-color: #2e6da4; margin: 5px; width: 80%;")),
               ),
           ),
        column(9,
               fluidRow(
                   tags$small("Click on the NPIs levels to add the interventions for the first month (January 2021), keep adding for each following month or leave empty with no changes"),
               ),
               fluidRow(id="npis-output",
                        div(id="tail",p("Add NPIs levels..."))
               ),
           )
        ),
       
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    date1 <<- "01-01-2021"
    date2 <<- "31-12-2022"
    dates <<- seq(as.Date(date1, "%d-%m-%Y"), as.Date(date2, "%d-%m-%Y"), by = "month")
    dateIndex <<- 1
    observeEvent(input$npi1, {
        addBox(1,"Baseline")
    })
    observeEvent(input$npi2, {
        addBox(2,"Self-isolation")
    })
    observeEvent(input$npi3, {
        addBox(3,"Shielding older pop.")
    })
    observeEvent(input$npi4, {
        addBox(4,"Physical distancing")
    })
    observeEvent(input$npi5, {
        addBox(5,"School closures")
    })
    observeEvent(input$npi6, {
        addBox(6,"Lockdown")
    })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = 35)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

addBox = function(npiIndex,text) {
    removeUI(selector = "#tail")
    insertUI(
        selector = "#npis-output",
        where = "beforeEnd",
        ui = div(align="center",
                 tags$span(HTML(paste0(format(dates[dateIndex],"%m"),"/",format(dates[dateIndex],"%Y"),"<br><b>",text,"</b>"))),
                 style=paste0("color: #222426; background-color: ",colores[npiIndex],"; border: 1px solid ; border-radius: 5px; margin: 5px; padding: 5px;")
            )
    )
    insertUI(
        selector = "#npis-output",
        where = "beforeEnd",
        ui =  tags$span(icon("arrow-right"), style=("margin-top: 15px;"))
    )
    insertUI(
        selector = "#npis-output",
        where = "beforeEnd",
        ui = div(id="tail", style=("margin-top: 15px;"),
            tags$span(HTML(paste0("No changes up to ",format(as.Date(date2, "%d-%m-%Y"),"%m"),"/",format(as.Date(date2, "%d-%m-%Y"),"%Y"))), style=("margin-top: 15px;")),
            )
    )
    dateIndex <<- dateIndex + 1
}

# Run the application 
shinyApp(ui = ui, server = server)
