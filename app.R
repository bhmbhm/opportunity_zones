# Load libraries
library(shiny)
library(ggplot2)
library(plotly)

# Source tax calculation script
source('calculate_taxes.R')

# Create self-named lists of tax brackets for inputs (this is dumb I know)
ind_rates <- names(ind_tax_rates)
names(ind_rates) <- ind_rates
joint_rates <- names(joint_tax_rates)
names(joint_rates) <- joint_rates

# Define UI
ui <- fluidPage(
  titlePanel("Opportunity Fund Calculator"),
  sidebarLayout(
    sidebarPanel(
      
      # Initialize with filtering question for filing status
      selectInput("joint_filing", "Are you filing your taxes jointly?", 
                  choices = c("","Yes", "No")),
      
      # Input options for joint filing
      conditionalPanel(
        condition = "input.joint_filing == 'Yes'",
        selectInput("joint_income_bracket", "What is your annual income?",
                    choices = c("",joint_rates)),
        conditionalPanel(
          condition = "input.joint_income_bracket != ''",
          numericInput("joint_basis", "Initial investment:", 100000, min = 0, step = 1000),
          numericInput("joint_gains", "Expected capital gains:", 50000, min = 0, step = 1000),
          sliderInput("joint_time", "Years held:", min = 0, max = 30, value = 0, step = 1)
        )
      ),
      
      # Input options for single filing
      conditionalPanel(
        condition = "input.joint_filing == 'No'",
        selectInput("ind_income_bracket", "What is your annual income?",
                    choices = c("",ind_rates)),
        conditionalPanel(
          condition = "input.ind_income_bracket != ''",
          numericInput("ind_basis", "Initial investment:", 100000, min = 0, step = 1000),
          numericInput("ind_gains", "Expected capital gains:", 50000, min = 0, step = 1000),
          sliderInput("ind_time", "Years held:", min = 0, max = 30, value = 0, step = 1)
        )
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
    req(input$joint_filing)
    if (input$joint_filing == "Yes"){
      cp_gains <- cg_tax(input$joint_gains, input$joint_income_bracket, input$joint_time, TRUE)
    } else if (input$joint_filing == "No"){
      cp_gains <- cg_tax(input$ind_gains, input$ind_income_bracket, input$ind_time, FALSE)
    }
    return(cp_gains)
  })
  
  # Calculate tax rates on a QOF
  qof_gains <- reactive({
    req(input$joint_filing)
    if (input$joint_filing == "Yes"){
      qof_gains <- qof_tax(input$joint_gains, input$joint_income_bracket, input$joint_time, input$joint_basis, TRUE)
    } else if (input$joint_filing == "No"){
      qof_gains <- qof_tax(input$ind_gains, input$ind_income_bracket, input$ind_time, input$ind_basis, FALSE)
    }
    return(qof_gains)
  })
  
  # Create data frame for visualization
  build_df <- reactive({
    req(input$joint_filing)
    viz_data <- data.frame(Investment = c("Captial Gains", "Opportunity Fund"), Taxes = c(capital_gains(), qof_gains()))
    viz_data$lab <- paste0("$", viz_data$Taxes)
    return(viz_data)
  })
  
  # Create horizontal bar plot of taxes
  output$tax_viz <- renderPlot({
    req(input$joint_filing)
    ggplot(data = build_df(), aes(x = Investment, y = Taxes, fill = Investment, label = lab)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label=paste0("$",Taxes)), color = "white", hjust=1.2) +
      scale_fill_manual(values = c("#4776BA", "#F5811D")) +
      theme(legend.title = element_blank()) +
      coord_flip()
  })
  
  # Render HTML text output
  output$calculate_taxes <- renderUI({
    str1 <- paste0("Your taxes on your capital gains are: $", capital_gains())
    str2 <- paste0("You taxes on your capital gains in an Opportunity Fund are: $", qof_gains())
    HTML(paste(str1, str2, sep = '<br/>'))
  })
}

# Create Shiny app
shinyApp(ui, server)