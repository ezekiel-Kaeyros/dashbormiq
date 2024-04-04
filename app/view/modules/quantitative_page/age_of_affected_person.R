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
  app/logic/functions,
  app/logic/quantitative/n_employee_logic
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui(shiny::textOutput(ns("title1")), #"Age categories of affected persons"
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "PieSingle")),
                div(class = "card_content", shiny::textOutput(ns("title2")), #"Most affected age group :"
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
    data_emp <- n_employee_logic$data_emp(filter)
    
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
    
    if (filter=="Eine Organisation/Institution")  {
      output$plot_personage <- renderUI({
        if (toggle$piechart) {
          plotlyOutput(ns("piechart1"))
        } else {
          plotlyOutput(ns("barplot1"))
        }
      })
      
      output$barplot1 <- renderPlotly({
        functions$generate_barplot(data_emp,"Number of employees")
      })
      
      output$piechart1 <- renderPlotly({
        functions$generate_piechart(data_emp,"Number of employees")
      })
    } else {
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
    }
    
    if (filter=="Eine Organisation/Institution") {
      output$title1 <- shiny::renderText({
        paste("Number of employees")
      })
    } else {
      output$title1 <- shiny::renderText({
        paste("Age of affected person")
      })
    }
    
    if (filter=="Eine Organisation/Institution") {
      output$title2 <- shiny::renderText({
        paste(" ")
      })
    } else {
      output$title2 <- shiny::renderText({
        paste("Most affected age group :")
      })
    }

    observeEvent(input$toggleButton, {
      toggle$piechart <- !toggle$piechart
    })
    
    if (filter=="Eine Organisation/Institution") {
      output$text <- shiny::renderText({
        #nrow(data_emp)
        paste("")
      })
    } else {
      output$text <- shiny::renderText({
        names(table(data_personage1$age))[which.max(table(data_personage1$age))]
      })
    }
    
    
  })
}

