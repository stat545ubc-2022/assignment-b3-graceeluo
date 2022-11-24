library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinythemes)

bcl <-read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(theme = shinytheme("spacelab"), #Feature 1: Added "spacelab" theme to make the app more visually appealing
  img(src = "logo.png", height = "50%", width = "50%"), #Feature 2: Added an image (BC Liquor Store logo) to the UI as it improves the overall aesthetic of the app
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      tabsetPanel( #Feature 3: Added tabs of Histogram and Data Table to improve organization of the app
      tabPanel("Histogram", plotOutput("coolplot")),
      tabPanel("Data Table", DT::dataTableOutput("results"))
      ) #Feature 4: Turned data table from a static table into an interactive table to allow searching, paging and sorting abilities
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })

  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }

    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })

  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })

  output$results <- DT::renderDataTable({ #Feature 4: Turned data table from a static table into an interactive table
    filtered()
  })
}

shinyApp(ui = ui, server = server)
