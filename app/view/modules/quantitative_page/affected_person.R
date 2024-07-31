
box::use(
    shiny[moduleServer, div,NS, h3, p, uiOutput,
           observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
    shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput,Dropdown.shinyInput],
    plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
    magrittr[`%>%`], shinyjs
)

box::use(
    app/view/components/ui/cards,
    app/logic/quantitative/affected_person_logic,
    app/logic/import_data,
    app/logic/functions,
    app/view/components/layouts
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  cards$card_ui("Betroffene Person",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    h3(class = "description", "Anzahl der betroffenen Personen :"),
                    p(class = "subtitle", shiny::textOutput(ns("subtitle")) ), #nrow(unique(import_data$data))
                  
                  # Graph goes here
                  uiOutput(ns("plot_personaf")))
              )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    marker <- list(color = c("#2F195F"))
    color <- c("#2F195F", "#2F195F","#2F195F", "#2F195F",
               "#2F195F",  "#2F195F", "#2F195F","#2F195F","#2F195F","#2F195F","#2F195F",
               "#2F195F")
    data_personaf <- affected_person_logic$data_personaf(filter)
    
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
    output$plot_personaf <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })

    output$barplot <- renderPlotly({
      functions$generate_barplot(data_personaf,"Betroffene Person",marker)
    })

    output$piechart <- renderPlotly({
      functions$generate_piechart(data_personaf,"Betroffene Person",color)
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })
    output$subtitle <- shiny::renderText({
      data <- affected_person_logic$person_af_sub(filter)
      nrow(unique(data))
    })


  })
}
