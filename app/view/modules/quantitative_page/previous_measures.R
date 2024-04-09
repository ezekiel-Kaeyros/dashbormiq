box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText,plotOutput,
        renderPlot],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`], dplyr,graphics[axis, text],RColorBrewer
)

box::use(
  app/view/components/ui/cards,
  app/logic/quantitative/previous_measures_logic,
  app/logic/functions
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Bereits ergriffene Maßnahmen",#"",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    h3(class = "description", "Die meisten bisher getroffenen Maßnahmen :"),
                    p(class = "subtitle", shiny::textOutput(ns("text"))), #a refaire
                    
                    # Graph goes here
                    uiOutput(ns("previous_measures"))#,width="500px", height = 485) #,width="500px"
                )
  )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    previous_measures_f <- previous_measures_logic$previous_measures(filter)
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
    output$previous_measures <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })

    output$barplot <- renderPlotly({
      functions$generate_barplot(previous_measures_f,"Aktion")
    })

    output$piechart <- renderPlotly({
      functions$generate_piechart(previous_measures_f,"Aktion")
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })
    output$text <- shiny::renderText({
      previous_measures_f$Var1[which.max(previous_measures_f$percentage)]
    })


  })
}
