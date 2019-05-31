#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
data <- mtcars
data[,c("cyl","vs","am","gear","carb")] <- 
  lapply(data[,c("cyl","vs","am","gear","carb")], as.factor)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Sidebar Layout:
   sidebarLayout(
     
     # Sidebar: for input here
     sidebarPanel(
       
       # TITLE
       textInput(inputId = "title",
                 label = "Plot title:",
                 placeholder = "Enter the title"),
       
       # Select data for x-axis
       selectInput(inputId = "x",
                   label = "x-axis",
                   choices = c("miles/gallon"="mpg",
                               "displacement" = "disp",
                               "gros horsepower" = "hp",
                               "real axle ratio"="drat",
                               "weight"= "wt",
                               "1/4 mile time"="qsec"),
                   selected = "mpg"),
       
       # Select data for y-axis
       selectInput(inputId = "y",
                   label = "y-axis",
                   choices = c("miles/gallon"="mpg",
                               "displacement" = "disp",
                               "gros horsepower" = "hp",
                               "real axle ratio"="drat",
                               "weight"= "wt",
                               "1/4 mile time"="qsec"),
                   selected = "disp"),
       
       # Coloring
       selectInput(inputId = "color",
                   label = "Color by",
                   choices = c("number of cyl"= "cyl",
                               "engine"= "vs",
                               "transmission"="am",
                               "num of forward gears"= "gear",
                               "num of carburetors"= "carb"),
                   selected = "number of cyl"),
       
       # Alfa
       sliderInput(inputId = "alpha",
                   label = "Alpha",
                   min = 0, max=1,
                   value = 0.5),
       
       numericInput(inputId = "size",
                    label = "Dot size:",
                    value = 3,
                    min=1, max = 9),
       
       #Check box
       checkboxInput(inputId = "show_d",
                     label = "Show data?",
                     value = FALSE),
       checkboxInput(inputId = "show_s",
                     label = "Show summary?",
                     value = FALSE),
       
       # Buttons
       actionButton(inputId = "b1",
                    label = "Apply"),
       
       # Submit button
       actionButton(inputId = "b2",
                    label = "Submit")
      
       
     ),
     
     # Main panel: for ouputs here
     mainPanel(
       
       # Plot output
       plotOutput(outputId = "scatter"),
       
       
       # Fluid row
       fluidRow(
         
         #Table output
         column(width = 6,
                tableOutput(outputId = "data_selected")
         ),
         column(width = 6,
                tableOutput(outputId = "summary")
         )
       ),
       
       # Message
       textOutput(outputId = "message")
                
                
       #Table output
       #tableOutput(outputId = "data_selected")
     )
   )
)
 

server <- function(input, output, session) {
  observeEvent(input$b2,{
    output$scatter <- renderPlot({
      req(input$size)
      
      ggplot(data, aes_string(x = input$x, y = input$y, color = input$color)) + 
        geom_point(alpha = input$alpha, size = input$size) + 
        ggtitle(tools::toTitleCase(isolate(input$title)))
    })
    
    new_data <- reactive({
      data %>% select(input$x, input$y, input$color)
    })
    
    output$data_selected <- renderTable({
      if (input$show_d) {
        new_data()
      }
    })
    
    output$summary <- renderTable({
      if (input$show_s) {
        new_data() %>%
          group_by_(input$color) %>%
          summarise_all(funs(mean,sd))
      }
    })
    
    observeEvent(input$b1,{output$message <-
       renderText("Thank you for clicking")
    })
  })
}    

# Run the application 
shinyApp(ui = ui, server = server)

