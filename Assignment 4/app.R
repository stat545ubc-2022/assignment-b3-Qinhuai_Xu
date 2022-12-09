library(shiny)
library(tidyverse)
library(DT)

bcl <- read_csv("bcl-data.csv")

ui <- fluidPage(
  # Feature 8: Add CSS to make the app look nicer.
  includeCSS("www/customed.css"),
  titlePanel("BC Liquor Store Data"), 
  h4("Welcome to my shiny app!"), 
  h4("This is my project for STAT545B assignment 4, which is a Shiny app that processes the BC Liquor data. You can try many things on this website."),
  h4("For example, you can use the tools in the sidebar to filter data; Use tools in the interactive table to sort and search data; Use the plot to see the distribution of Alcohol content. Or use the download buttom to view the original dataset..."),
  br(), 
  sidebarLayout(
    sidebarPanel(
      # Feature 6: Add an image of the BC Liquor Store to the UI
      img(src = "logo.png"),
      sliderInput("priceInput", "Price", 0, 100, 
                  value = c(25, 40), pre = "$"), 
      # Feature 4: Allow the user to search for multiple types simultaneously
      checkboxGroupInput("typeInput", "Type", 
                   choices = c("BEER", "REFRESHMENT", 
                               "SPIRITS", "WINE"), 
                   selected = c("BEER", "REFRESHMENT", "SPIRITS", "WINE")),
      # Feature 7: Add country filter to allow the users to search for multiple countries simultaneously
      selectInput("countryInput", "Country",  unique(bcl$Country), selected = c("CANADA"), multiple = T),
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
                       Type %in% c("", input$typeInput) & 
                       Country %in% c("", input$countryInput))
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
  
  # Feature 3: Allow the user to download the BC Liquor table as a .csv file
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

