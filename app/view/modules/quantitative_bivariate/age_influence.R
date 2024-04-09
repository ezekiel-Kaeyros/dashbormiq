
box::use(
    shiny[moduleServer, div,NS, h3, p, uiOutput,
           observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
    shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
    plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config,style,ggplotly],
    magrittr[`%>%`],
    ggplot2[ggplot,geom_tile,geom_text,scale_fill_gradient,labs,aes,theme,element_text]
)

box::use(
    app/view/components/ui/cards,
    app/logic/quantitative_bivariate/age_inf_logic,
    app/logic/functions
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Häufigkeit der verschiedenen Arten von Diskriminierung nach Altersgruppe",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                  # Graph goes here
                  uiOutput(ns("plot_personaf"))
                )
              )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #ns <- NS(id)
    #output$plot_personaf <- render
    button_state <- reactiveVal(FALSE)
    yiord_palette <- c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")

    observeEvent(input$toggleButton, {
      button_state(!button_state())
      if (button_state()) {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "table"))
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
      functions$generate_groupedbarplot(age_inf_logic$table_age_inf,"Altersgruppe","Typ der Diskriminierung")
    })

    output$piechart <- renderPlotly({
      functions$generate_table(age_inf_logic$table_age_inf,"Altersgruppe", "Typ der Diskriminierung")
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })


  })
}
