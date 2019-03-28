#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("./global.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(h1("Convert BS to AD", align = "center", style = "color:Red")),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        dateInput("Input_date",
                  "Enter Date in yyy/mm/dd format"),
      
        submitButton("Submit")
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        verbatimTextOutput("date")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$date <- renderText({
     
     #paste("Input text is:", input$Input_date)
     #Date to be converted into AD ------
     input.date <- input$Input_date
     pdate <- BS_to_AD(input.date)
     paste ("The converted date is   ", pdate %>%
       format("%d %b %Y %A"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


