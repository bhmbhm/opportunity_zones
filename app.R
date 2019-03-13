# Load libraries
library(shiny)
library(ggplot2)
library(plotly)

# Source tax calculation script
source('calculate_taxes.R')

# Define UI
ui <- fluidPage(
  titlePanel("Opportunity Fund Calculator"),
  sidebarLayout(
    sidebarPanel(
      
      # Initialize with filtering question for filing status
      selectInput("filing_status", "Filing status:", 
                  choices = c("","Single filer", "Married, filing jointly", "Married, filing separately", "Head of Household")),
      
      # Input options for tax status
      conditionalPanel(
        condition = "input.filing_status != ''",
        numericInput("income", "Expected taxable income:", 60000, min = 0, step = 1000),
        numericInput("investment", "Price of purchase:", 100000, min = 0, step = 1000),
        numericInput("sale", "Price of sale:", 120000, min = 0, step = 1000),
        sliderInput("time", "Years from purchase to sale:", min = 0, max = 30, value = 0, step = 1)
      )
    ),
    
    # Output tax rates for capital gains and QOF's
    mainPanel(
      plotOutput("tax_viz")
      #htmlOutput("calculate_taxes")
    )
  )
)

# Define server function
server <- function(input, output) {

  # Calculate capital gains
  capital_gains <- reactive({
    req(input$filing_status)
    cp_gains <- cg_tax(input$filing_status, input$income, input$investment, input$sale, input$time)
    return(cp_gains)
  })
  
  # Calculate tax rates on a QOF
  qof_gains <- reactive({
    req(input$filing_status)
    qof_gains <- qof_tax(input$filing_status, input$income, input$investment, input$sale, input$time)
    return(qof_gains)
  })
  
  # Create data frame for visualization
  build_df <- reactive({
    req(input$filing_status)
    viz_data <- data.frame(Investment = c("Captial Gains", "Opportunity Fund"), Taxes = c(capital_gains(), qof_gains()))
    viz_data$lab <- paste0("$", viz_data$Taxes)
    return(viz_data)
  })
  
  # Create horizontal bar plot of taxes
  output$tax_viz <- renderPlot({
    req(input$filing_status)
    ggplot(data = build_df(), aes(x = Investment, y = Taxes, fill = Investment, label = lab)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label=paste0("$",Taxes)), color = "white", hjust=1.2) +
      scale_fill_manual(values = c("#4776BA", "#F5811D")) +
      theme(legend.title = element_blank()) +
      coord_flip()
  })
  
  # Render HTML text output
  # output$calculate_taxes <- renderUI({
  #   str1 <- paste0("Your taxes on your capital gains are: $", capital_gains())
  #   str2 <- paste0("You taxes on your capital gains in an Opportunity Fund are: $", qof_gains())
  #   HTML(paste(str1, str2, sep = '<br/>'))
  # })
  
}

# Create Shiny app
shinyApp(ui, server)