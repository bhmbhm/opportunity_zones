# Load libraries
library(shiny)

# Source tax calculation script
source('calculate_taxes.R')

# Create self-named lists of tax brackets for inputs
ind_rates <- names(ind_tax_rates)
names(ind_rates) <- ind_rates
joint_rates <- names(joint_tax_rates)
names(joint_rates) <- joint_rates

# Define UI
ui <- fluidPage(
  titlePanel("Opportunity Fund Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("joint_filing", "Are you filing your taxes jointly?", 
                  choices = c("","Yes", "No")),
      conditionalPanel(
        condition = "input.joint_filing == 'Yes'",
        selectInput("joint_income_bracket", "What is your annual income?",
                    choices = c("",joint_rates)),
        numericInput("joint_basis", "Initial investment:", 100000, min = 0, step = 1000),
        numericInput("joint_gains", "Expected capital gains:", 50000, min = 0, step = 1000),
        sliderInput("joint_time", "Years held:", min = 0, max = 30, value = 0, step = 1)
      ),
      
      conditionalPanel(
        condition = "input.joint_filing == 'No'",
        selectInput("ind_income_bracket", "What is your annual income?",
                    choices = c("",ind_rates)),
        numericInput("ind_basis", "Initial investment:", 100000, min = 0, step = 1000),
        numericInput("ind_gains", "Expected capital gains:", 50000, min = 0, step = 1000),
        sliderInput("ind_time", "Years held:", min = 0, max = 30, value = 0, step = 1)
      )
      
    ),
    
    mainPanel(
      htmlOutput("calculate_taxes")
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
      cp_gains <- cg_tax(input$joint_gains, input$joint_income_bracket, input$joint_time, FALSE)
    }
    return(cp_gains)
  })
  
  qof_gains <- reactive({
    req(input$joint_filing)
    if (input$joint_filing == "Yes"){
      qof_gains <- qof_tax(input$joint_gains, input$joint_income_bracket, input$joint_time, input$joint_basis, TRUE)
    } else if (input$joint_filing == "No"){
      qof_gains <- qof_tax(input$joint_gains, input$joint_income_bracket, input$joint_time, input$ind_basis, FALSE)
    }
    return(qof_gains)
  })
  
  output$calculate_taxes <- renderUI({
    str1 <- paste0("Your taxes on your capital gains are: $", capital_gains())
    str2 <- paste0("You taxes on your capital gains in an Opportunity Fund are: $", qof_gains())
    HTML(paste(str1, str2, sep = '<br/>'))
  })
  
}

# Create Shiny app
shinyApp(ui, server)