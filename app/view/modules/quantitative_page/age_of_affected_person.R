box::use(
  shiny[moduleServer, div,NS, h3, h5,p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative/age_affected_person_logic,
  app/logic/functions
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Age categories of affected persons",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "PieSingle")),
                div(class = "card_content","Most affected age group :" ,
                    h3(class = "description",shiny::textOutput(ns("text"))), 
                    p(class = "subtitle", ),
                    # Graph goes here
                    uiOutput(ns("plot_personage"))
                )
  )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_personage <- age_affected_person_logic$data_age(filter)
    data_personage1 <- age_affected_person_logic$data_age1(filter)
    
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
    output$plot_personage <- renderUI({
      if (toggle$piechart) {
        plotlyOutput(ns("piechart"))
      } else {
        plotlyOutput(ns("barplot"))
      }
    })
    
    output$barplot <- renderPlotly({
      functions$generate_barplot(data_personage,"Age")
    })
    
    output$piechart <- renderPlotly({
      functions$generate_piechart(data_personage,"Age")
    })
    # output$barplot <- renderUI ({
    #   if (filter=="An organization/institution") {
    #     shiny::textOutput(ns("text1"))
    #   } else {
    #     plotly::plotlyOutput(ns("plot1"))
    #   }
    # })
    # output$piechart <- renderUI ({
    #   if (filter=="An organization/institution") {
    #     shiny::textOutput(ns("text1"))
    #   } else {
    #     plotly::plotlyOutput(ns("plot2"))
    #   }
    # })
    # output$text1 <- shiny::renderText({
    #     paste("No data concerning age of the organization")
    # })
    # # output$text2 <- shiny::renderText({
    # #   paste("No data concerning age of the organization")
    # # })
    # output$plot1 <- renderPlotly({
    #     functions$generate_barplot(data_personage,"Age")
    # })
    # output$plot2 <- renderPlotly({
    #   functions$generate_piechart(data_personage,"Age")
    # })
    # 
    observeEvent(input$toggleButton, {
      toggle$piechart <- !toggle$piechart
    })
    
    output$text <- shiny::renderText({
      # max_index <- which.max(data_personage$Freq)
      # data_personage[max_index, ]$Var1
      names(table(data_personage1$age))[which.max(table(data_personage1$age))]
    })
    
  })
}

