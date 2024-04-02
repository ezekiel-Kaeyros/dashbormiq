
box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config,style,ggplotly],
  magrittr[`%>%`],
  
)

box::use(
  app/view/components/ui/cards,
  app/logic/quantitative_bivariate/age_disc_logic,
  app/logic/functions
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Frequency of Different Forms of Discrimination by Age Group",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "table")),
                div(class = "card_content",
                    # Graph goes here
                    uiOutput(ns("plot_personage"))
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
    # 
    observeEvent(input$toggleButton, {
      button_state(!button_state())
      if (button_state()) {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "BarChart4"))
      } else {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "table"))
      }
    })

    toggle <- reactiveValues(piechart = TRUE)
    output$plot_personage <- renderUI({
      if (toggle$piechart) {
        plotlyOutput(ns("piechart"))
      } else {
        plotlyOutput(ns("barplot"))
      }
    })
    # 
    output$barplot <- renderPlotly({
      functions$generate_groupedbarplot(age_disc_logic$table_age_disc,"Age group","Discrimination")
    })

    output$piechart <- renderPlotly({
      functions$generate_table(age_disc_logic$table_age_disc,"Age group", "Discrimination")
    })

    observeEvent(input$toggleButton, {
      toggle$piechart <- !toggle$piechart
    })


  })
}

