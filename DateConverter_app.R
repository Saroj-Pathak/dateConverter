
library(shiny)
library(shinyWidgets)

source("./global.R")

# Define UI for application

ui <- fluidPage(
    
    # Application title
    titlePanel(h1("Convert BS to AD", align = "center", style = "color:Red")),
    
    # Sidebar with a dateInput and submitButton
    sidebarLayout(
      sidebarPanel(
        
        radioButtons("rdo", "Convert to :", c("BS to AD " = "toAD",
                                         "AD to BS " = "toBS")),
        br(),
        dateInput("date",
                  "Enter Date in yyy/mm/dd format"
                  ),
        
        submitButton("Submit"),
                     
        br(),
        br(),
        br()
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        verbatimTextOutput("date")
      )
    )
  )

  
# Define server logic
server <- function(input, output) {
   output$date <- renderText({
     #Date to be converted into AD ------
     input.date <- input$date
     input.rd <- input$rdo
     
      if (input.rd == "toAD") {
        pdate <- BS_to_AD(input.date)
        paste ( "The converted date is  ",
               pdate %>%
                 format("%d %B %Y %A")
               )
      }
     else {
       pdate <- AD_to_BS(input.date)
       paste ("The converted date is  ", 
              pdate,
              input.date %>%
                format ("%A"))
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)




