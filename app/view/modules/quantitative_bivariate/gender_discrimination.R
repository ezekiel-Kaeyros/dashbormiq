
box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config, style, ggplotly],
  magrittr[`%>%`],
  ggplot2[ggplot,geom_tile,geom_text,scale_fill_gradient,labs,aes,theme,element_text]
)

box::use(
  app/view/components/ui/cards,
  app/logic/quantitative_bivariate/gender_disc_logic,
  app/logic/functions
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Frequency of Different Forms of queerphobia by Gender",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    # Graph goes here
                    uiOutput(ns("plot_date"))
                )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #ns <- NS(id)
    
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
    output$plot_date <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })

    output$barplot <- renderPlotly({
      functions$generate_groupedbarplot(gender_disc_logic$table_gender_disc,"Gender","Discrimination")
    })

    output$piechart <- renderPlotly({
      functions$generate_table(gender_disc_logic$table_gender_disc,"Gender", "Discrimination")
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })


  })
}
