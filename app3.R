library(shiny)
library(shinythemes)
library(shinyWidgets)


# ESTAS FUNCIONES VENDRIAN DE UN SOURCE
front_original <- function () {fluidPage(
  titlePanel("Old Faithful Geyser Data"),
  theme = shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select_front", 
                  "Seleccionar front",
                  choices = c("Original", "Alternativo")),
      h1("FRONT ORIGINAL"),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      plotOutput("distPlot"),
      div(
        p("Prueba de UI"),
        fluidRow(
          checkboxInput("schoolClosures", "School Closures", FALSE)
        ),
        fluidRow(
          prettyCheckbox(
            inputId = "checkbox2",
            label = "Click me!",
            thick = TRUE,
            animation = "pulse",
            status = "info"
          ),
        )
      )
    )
  )
)}

front_alternativo <- function () {fluidPage(
  titlePanel("Old Faithful Geyser Data"),
  theme = shinytheme("superhero"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select_front", 
                  "Seleccionar front",
                  choices = c("Original", "Alternativo"),
                  selected="Alternativo"),
      h1("FRONT ALTERNATIVO"),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)}



ui <- uiOutput("front") 

server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  # ESTE OUTPUT TIENE DEFINE TODA LA UI Y TIENE UN IF QUE DECIDE CUAL SE MUESTRA
  output$front <- renderUI({
    ui_option <-
      if ("select_front" %in% names(reactiveValuesToList(input)) == F) {
        "default"
      } else
        if ("select_front" %in% names(reactiveValuesToList(input)) ==
            T) {
          "input"
        }
    if (ui_option == "default") {
      tagList(front_original())
    } else
      if (ui_option == "input") {
        if (input$select_front == "Original") {
          tagList(front_original())
        }
        else {
          tagList(front_alternativo())
        }
      }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)