
box::use(
  shiny[moduleServer, div,NS, h1,h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative/gender_logic, 
  app/logic/quantitative/organization_type_logic,
  app/logic/functions
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui(shiny::textOutput(ns("title1")),#"Gender identity",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    h3(class = "description",shiny::textOutput(ns("title2")) ), #"Different genders :"
                    p(class = "subtitle", shiny::textOutput(ns("subtitle"))),
                    
                    # Graph goes here
                    uiOutput(ns("plot_persongen"))
                )
  )
}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_gender <- gender_logic$data_gender(filter)
    data_type <- organization_type_logic$data_type(filter)
    
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
    
    if (filter=="Eine Organisation/Institution")  {
      output$plot_persongen <- renderUI({
        if (toggle$barplot) {
          plotlyOutput(ns("barplot1"))
        } else {
          plotlyOutput(ns("piechart1"))
        }
      })
      
      output$barplot1 <- renderPlotly({
        functions$generate_barplot(data_type,"Typ")
      })
      
      output$piechart1 <- renderPlotly({
        functions$generate_piechart(data_type,"Typ")
      })
    } else {
      output$plot_persongen <- renderUI({
        if (toggle$barplot) {
          plotlyOutput(ns("barplot"))
        } else {
          plotlyOutput(ns("piechart"))
        }
      })
      
      output$barplot <- renderPlotly({
        functions$generate_barplot(data_gender,"Geschlecht")
      })
      
      output$piechart <- renderPlotly({
        functions$generate_piechart(data_gender,"Geschlecht")
      })
    }
    
    if (filter=="Eine Organisation/Institution") {
      output$title1 <- shiny::renderText({
        paste("Organisationstyp")
      })
    } else {
      output$title1 <- shiny::renderText({
        paste("Geschlechtsidentität")
      })
    }
    
    if (filter=="Eine Organisation/Institution") {
      output$title2 <- shiny::renderText({
        paste("Verschiedene Arten von Organisationen :")
      })
    } else {
      output$title2 <- shiny::renderText({
        paste("Unterschiedliche Geschlechter :")
      })
    }
    
    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })
    
    if (filter!="Eine Organisation/Institution"){
      output$subtitle <- shiny::renderText({
        dim(data_gender)[1]
        #nrow(data_gender)
      })
    } else {
      output$subtitle <- shiny::renderText({
        length(unique(unlist(import_data$data$organizationType)))
      })
    }
    


  })
}
