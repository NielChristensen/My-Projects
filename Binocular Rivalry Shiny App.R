library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Binocular Rivalry File Upload"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput('file1', 'Choose xlsx file',
                accept = c(".xlsx")
      ),
      
      
      # Horizontal line ----
      tags$hr(),
      
      
      
      # Horizontal line ----
      tags$hr(),
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  library(dplyr)
  library(readxl)
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    
    switch <- read_xlsx(input$file1$datapath)
    switch$COLUMN[switch$COLUMN == 'LEFT'] <- 1
    switch$COLUMN[switch$COLUMN == 'RIGHT'] <- 2

    switch <- switch %>% 
      filter(switch$COLUMN != 'UP')
    
    switchLength <- length(switch$COLUMN)-1
    
    switchcount <- function(y){
      x = 0
      
      for (i in 1:switchLength){
        if(switch$COLUMN[i] == switch$COLUMN[i+1] ){
          x = x 
        }
        else{
          x = x + 1
        }
      }
      paste("Number of Switches: ", print(x))
    }
    
    
    switchcount(switch)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
