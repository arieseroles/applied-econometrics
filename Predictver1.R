library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Predict"),
  
  # Main panel for results and some explanation.
  mainPanel(
    
    # Inputs ----
    numericInput('SP500_MOM', 'SP500 returns (%)', min = -20, max = 50, value = 2, step = 0.1),
    numericInput('PHPUSD_dmAve', 'PHPUSD monthly change rate (%)', min = -10, max = 20, value = 1, step = 0.1),
    numericInput('VIX_MOM', 'Change in VIX (%)', min = -50, max = 50, value = 5, step = 0.1),
    
    # Output ----
    h4('The predicted returns of the PSEI (%) is'),
    verbatimTextOutput("PSE_MOM"),
  )
)

PSE_MOM <- function(SP500_MOM, PHPUSD_dmAve, VIX_MOM, VIX_ma12, SP500_Ave, Gold_MOM) {
  return(0.59 + 0.41 * SP500_MOM - 0.81 * PHPUSD_dmAve - 0.0004 * (VIX_MOM^2))
}

server <- function(input, output) {
  output$PSE_MOM <- renderPrint({
    PSE_MOM(input$SP500_MOM, input$PHPUSD_dmAve, input$VIX_MOM)
  })
  
}

shinyApp(ui = ui, server = server)