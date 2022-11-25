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
      checkboxGroupInput("typeInput", "Type", 
                   choices = c("BEER", "REFRESHMENT", 
                               "SPIRITS", "WINE"), 
                   selected = c("BEER", "REFRESHMENT", "SPIRITS", "WINE")),
      textOutput("row_count"),
      downloadButton("download")
    ),
    mainPanel(
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

  output$data_table <- 
    DT::renderDataTable({
      filtered_data()
    })
  
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

