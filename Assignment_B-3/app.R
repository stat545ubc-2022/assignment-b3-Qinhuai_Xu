library(shiny)
library(tidyverse)
library(DT)

bcl <- read_csv("bcl-data.csv")

ui <- fluidPage(
  titlePanel("BC Liquor Store Data"), 
  h5("Welcome to my shiny app!"), 
  br(), 
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, 
                  value = c(25, 40), pre = "$"), 
      # Feature 4: Allow the user to search for multiple types simultaneously
      checkboxGroupInput("typeInput", "Type", 
                   choices = c("BEER", "REFRESHMENT", 
                               "SPIRITS", "WINE"), 
                   selected = c("BEER", "REFRESHMENT", "SPIRITS", "WINE")),
      textOutput("row_count"),
      downloadButton("download")
    ),
    mainPanel(
      # Feature 5: Use tabsetPanel to place plot and table in separate tabs
      tabsetPanel(
        tabPanel("Plot", plotOutput("alcohol_hist")),
        tabPanel("Table", DT::dataTableOutput("data_table"))
      )
    )
  ), 
  a(href="https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv", 
    "Link to the original data set")
)

server <- function(input, output) {
  
  filtered_data <- 
    reactive({
      bcl %>% filter(Price > input$priceInput[1] & 
                       Price < input$priceInput[2] & 
                       Type %in% c("", input$typeInput))
    })
  
  # Feature 2: Show the number of results found whenever the filters change
  output$row_count <- 
    renderText({
        counter <- filtered_data() %>% count()
        
        paste("We found", counter$n, "options for you")
    })


  output$alcohol_hist <- 
    renderPlot({
      filtered_data() %>% 
        ggplot(aes(Alcohol_Content)) + geom_histogram(bins=30)
    })

  # Feature 1: Use the DT package to turn the static table into an interactive table
  output$data_table <- 
    DT::renderDataTable({
      filtered_data()
    })
  
  # Feature 3: Allow the user to download your table as a .csv file
  output$download <-
    downloadHandler(
      filename = function(){
        "bcl-data.csv"
      },
      content = function(file){
        write.csv(bcl, file) 
      }
    )
}

shinyApp(ui = ui, server = server)

