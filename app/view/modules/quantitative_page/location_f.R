box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative/location_logic,
  app/logic/functions
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Location",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    h3(class = "description", "Most common place :"),
                    p(class = "subtitle", shiny::textOutput(ns("text"))),
                    
                    # Graph goes here
                    uiOutput(ns("plot_loc"),width="500px", height = 500)
                )
  )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_onreal <- location_logic$data_onreal(filter)
    data_onreal1 <- location_logic$data_onreal1(filter)
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
    output$plot_loc <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })

    output$barplot <- renderPlotly({ 
      functions$generate_barplot(data_onreal,"Location")
    })

    output$piechart <- renderPlotly({
      functions$generate_piechart(data_onreal,"Location")
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })
    
    output$text <- shiny::renderText({
      names(table(data_onreal1$locationOnline))[which.max(table(data_onreal1$locationOnline))]
    })


  })
}
