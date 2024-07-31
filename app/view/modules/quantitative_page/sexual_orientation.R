
box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/quantitative/sexual_orientation_logic,
  app/logic/functions
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Sexuelle Orientierung",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "PieSingle")),
                div(class = "card_content",
                    h3(class = "description", "Die sexuelle Orientierung des Betreffenden :"),
                    p(class = "subtitle", shiny::textOutput(ns("text"))),
                    
                    # Graph goes here
                    uiOutput(ns("plot_sex"))
                )
  )
  
}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    marker <- list(color = c("#2F195F"))
    color <- c("#2F195F", "#2F195F","#2F195F", "#2F195F",
               "#2F195F",  "#2F195F", "#2F195F","#2F195F","#2F195F","#2F195F","#2F195F","#2F195F",
               "#2F195F")
    data_sex <- sexual_orientation_logic$data_sex(filter)
    data_sex1 <- sexual_orientation_logic$data_sex1(filter)
    
    button_state <- reactiveVal(FALSE)
    
    observeEvent(input$toggleButton, {
      button_state(!button_state())
      if (button_state()) {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "BarChart4"))
      } else {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "PieSingle"))
      }
    })
    
    toggle <- reactiveValues(piechart = TRUE)
    output$plot_sex <- renderUI({
      if (toggle$piechart) {
        plotlyOutput(ns("piechart"))
      } else {
        plotlyOutput(ns("barplot"))
      }
    })
    
    output$barplot <- renderPlotly({
      functions$generate_barplot(data_sex,"Sexuelle Orientierung",marker)
    })
    
    output$piechart <- renderPlotly({
      functions$generate_piechart(data_sex,"Sexuelle Orientierung",color)
    })
    
    observeEvent(input$toggleButton, {
      toggle$piechart <- !toggle$piechart
    })
    output$text <- shiny::renderText({
      #data_sex$Var1[which.max(data_sex$percentage)]
      names(table(unlist(data_sex1$sexualOrientation)))[which.max(table(unlist(data_sex1$sexualOrientation)))]
    })
    
  })
}
