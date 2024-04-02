
box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/quantitative/date_occurance_logic,
  app/logic/functions
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Temporal distance between occurance and report",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    h3(class = "description", "Modal temporal distance :"),
                    p(class = "subtitle", shiny::textOutput(ns("text"))),
                    
                    # Graph goes here
                    uiOutput(ns("plot_date"))
                )
  )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_months <- date_occurance_logic$data_temp(filter)
    data_months1 <- date_occurance_logic$data_temp1(filter)
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
    output$plot_date <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })

    output$barplot <- renderPlotly({
      functions$generate_barplot(data_months,"Time")
    })

    output$piechart <- renderPlotly({
      functions$generate_piechart(data_months,"Time")
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })
    output$text <- shiny::renderText({
      names(table(data_months1$Var1))[which.max(table(data_months1$Var1))]
      
    })


  })
}
