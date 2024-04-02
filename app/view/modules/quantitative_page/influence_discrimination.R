
box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/quantitative/influence_discrimination_logic,
  app/logic/functions
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Foundation of the discrimination",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    h3(class = "description", "Reccurent foundation of discrimation :"),
                    p(class = "subtitle", shiny::textOutput(ns("text"))),
                    
                    # Graph goes here
                    uiOutput(ns("plot_inf_disc"))
                )
  )
  
}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    influence_discrimination <- influence_discrimination_logic$influence_discrimination(filter)
    #ns <- NS(id)
    #output$plot_personaf <- render
    button_state <- reactiveVal(FALSE)
    
    observeEvent(input$toggleButton, {
      button_state(!button_state())
      if (button_state()) {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "PieSingle"))
      } else {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "BarChart4"))
      }
    })
    
    toggle <- reactiveValues(barplot = TRUE)
    output$plot_inf_disc <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })
    
    output$barplot <- renderPlotly({
      functions$generate_barplot(influence_discrimination,"Base of the discrimination")
    })
    
    output$piechart <- renderPlotly({
      functions$generate_piechart(influence_discrimination,"Base of the discrimination")
    })
    
    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })
    output$text <- shiny::renderText({
      influence_discrimination$Var1[which.max(influence_discrimination$percentage)]
    })
    
    
  })
}
