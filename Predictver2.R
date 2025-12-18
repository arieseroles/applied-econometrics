library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Predict"),
  
  # Main panel for results and some explanation.
  mainPanel(
    
    # Inputs ----
    numericInput('RF', 'Risk free rate (%)', min = 0, max = 20, value = 7.5, step = 0.1),
    numericInput('SP500_MOM', 'SP500 returns (%)', min = -20, max = 50, value = 2, step = 0.1),
    numericInput('Inflation', 'Inflation rate (%)', min = -20, max = 50, value = 2, step = 0.1),
    numericInput('PHPUSD_dmAve', 'PHPUSD monthly change rate (%)', min = -10, max = 20, value = 1, step = 0.1),
    numericInput('dRemit', 'Growth in OFW remittance (%)', min = -10, max = 20, value = 1, step = 0.1),
    numericInput('BRENT', 'Price of oil (USD)', min = 1, max = 200, value = 76.44, step = 0.01),
    numericInput('Gold', 'Price of gold (USD)', min = 1, max = 5000, value = 2385.70, step = 0.01),
    numericInput('VIX_MOM', 'Change in VIX (%)', min = -50, max = 50, value = 5, step = 0.1),
    
    # Output ----
    h4('The predicted returns of the PSEI (%) using the APM model is'),
    verbatimTextOutput("PSE_MOM"),
  )
)

PSE_MOM <- function(RF,SP500_MOM, Inflation, PHPUSD_dmAve, dRemit, BRENT, Gold, VIX_MOM) {
  return(RF - 11.25 + 0.409 * SP500_MOM - 0.058 * (Inflation^2) - 0.518 * PHPUSD_dmAve - 0.001 * (dRemit^2) + 0.764 * log(BRENT) + 0.911 * log(Gold) - 0.0004 * (VIX_MOM^2))
}

server <- function(input, output) {
  output$PSE_MOM <- renderPrint({
    PSE_MOM(input$RF,input$SP500_MOM, input$Inflation, input$PHPUSD_dmAve, input$dRemit, input$BRENT, input$Gold, input$VIX_MOM)
  })
  
}

shinyApp(ui = ui, server = server)