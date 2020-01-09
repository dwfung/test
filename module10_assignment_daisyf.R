library(shiny)
library(dplyr)
library(ggplot2)

income <- read.csv("income.csv")

ui <- fluidPage(
  titlePanel("Module 10 Assignment"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "subset_income",
                  label = "subset_income",
                  choices = c("<=50K", ">50K"),
                  multiple = FALSE),
      selectInput(inputId = "set_yaxis",
                  label = "set_yaxis", 
                  choices = c("hours_per_week","capital_loss")),       
      # Interactive piece 3: inputId = "subset_occupation"
      checkboxGroupInput(inputId = "subset_occupation", 
                         label = "Include Occupations:", 
                         choices = unique(income$occupation), 
                         selected = unique(income$occupation))),    # Main panel
    mainPanel(plotOutput(outputId = "myfigure"))
  )
)

server <- function(input, output) {
  create_subset <- reactive(income%>%
                              filter(capital_loss > 0 &
                                       income == input$subset_income &
                                       occupation %in% input$subset_occupation))
  
  output$myfigure <- renderPlot(ggplot(create_subset()) +
                                  geom_boxplot(aes_string(x = "occupation", , y = input$set_yaxis)) +
                                  theme_bw(18) +
                                  theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}


shinyApp(ui, server)