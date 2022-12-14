library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinythemes)

bcl <-read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(theme = shinytheme("spacelab"), #Feature 1: Added "spacelab" theme to make the app more visually appealing
  img(src = "logo.png", height = "50%", width = "50%"), #Feature 2: Added an image (BC Liquor Store logo) to the UI as it improves the overall aesthetic of the app
  h3("The purpose of this application is to allow users to interact, evaluate, and visualize the data from the BC Liquor data set. Data can be viewed visually through the 'Histogram' tab or in table form through the 'Data Table' tab. The left side panel can be used to adjust the price, select the product type and sort by price."),
  br(),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      checkboxGroupInput("typeInput", "Product Type", #Feature 5: Changed the "Product Type" side panel to allow users to search for multiple entries simultaneously. For example, search for multiple alcohol types at once instead of wine/beer/etc, one at a time.
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      checkboxInput("sortInput", label ="Sort by Price") #Feature 6: Added an option to sort the Data Table by price to allow users to view the data from least expensive to most expensive
    ),
    mainPanel(
      tabsetPanel( #Feature 3: Added tabs of Histogram and Data Table to improve organization of the app
      tabPanel("Histogram", plotOutput("coolplot")),
      tabPanel("Data Table", downloadButton("download", "Download Results"), DT::dataTableOutput("results")) #Feature 7: Added the "Download Results" to allow users to download the data table as a csv file for the purpose of data analysis or future use
      ) #Feature 4: Turned data table from a static table into an interactive table to allow searching, paging and sorting abilities
    )
  )
)

server <- function(input, output) {
  filtered_data <-
    reactive({
      new <- bcl %>%
        filter(Price >= input$priceInput[1] &
                             Price <= input$priceInput[2] &
                             Type == input$typeInput)
      if(input$sortInput){ #Feature 6: Added an option to sort the Data Table by price to allow users to view the data from least expensive to most expensive
        new %>%
          arrange(Price)
      } else{
        new
      }
    })

  output$coolplot <- renderPlot({
    filtered_data() %>%
    ggplot(aes(Alcohol_Content)) +
      geom_histogram()
  })

  output$download <- downloadHandler( #Feature 7: Added the "Download Results" button to allow users to download the data table as a csv file for the purpose of data analysis or future use
    filename = function() {
      "bcl-results.csv"
    },
    content = function(con) {
      write.csv(filtered_data(), con)
    }
  )

  output$results <- DT::renderDataTable({ #Feature 4: Turned data table from a static table into an interactive table to allow searching, paging and sorting abilities
    filtered_data()
  })
}

shinyApp(ui = ui, server = server)
