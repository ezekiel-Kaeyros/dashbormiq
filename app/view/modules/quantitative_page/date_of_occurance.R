
box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Temporal distance between occurance and report",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    h3(class = "subtitle", import_data$data_months$temporal_class[which.max(import_data$data_months$total)]),
                    p(class = "description", "Modal temporal distance"),
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
      plotly::plot_ly(import_data$data_months, x = ~temporal_class,
                      type = "bar",
                      y = ~percentage,
                      #marker = list(color = c("#0B5345", "#148F77", "#196F3D", "#52BE80", "#7DCEA0", "#CA6F1E")),
                      # marker = list(color = c("#483D8B", "slateblue","#0077BE", "#5696CC",
                      #                         "#76B7DA",  "#A6DAFF")),
                      marker =list(color="#F9AFC5"),
                      #colors = "darkviolet",
                      #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                      text = paste(import_data$data_months$pct1, sep = ""), textposition = 'outside',
                      textfont = list(size = 10), # size is defined here
                      hovertext = paste("Time: ", import_data$data_months$temporal_class,
                                        "<br>Number of persons :", import_data$data_months$total,
                                        "<br>Percentage :",import_data$data_months$pct1), #) %>%
                      #"<br>Percentage :", data_marsta()$pct1),
                      hoverinfo = 'text') %>%
        layout(title = "",
               #legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b> </b>')),
               uniformtext=list(minsize=10, mode='show'),
               xaxis = list(title = "<b> </b>", #font = list(size = 0),
                            # change x-axix size
                            tickfont = list(size = 12),
                            # change x-title size
                            titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                            tickangle= -45, showgrid = FALSE),
               yaxis = list(title = "<b> Percentage </b>",
                            titlefont = list(size = 12),
                            # change x-axix size
                            tickfont = list(size = 12),
                            ticksuffix = "%", showgrid = FALSE)
        ) %>%
        config(displayModeBar = F,
               scrollZoom = T)
    })

    output$piechart <- renderPlotly({
      plot_ly(import_data$data_months, labels= ~temporal_class,
              values= ~total, type="pie",
              hoverinfo = 'text',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              text = ~paste("Time :", temporal_class,
                            "<br>Number of persons :", total,
                            "<br>Percentage :", pct1),
              # marker = list(colors = c("#5072A7", "#1D428A","#0000ff","#7630ff","#20B2AA"),
              #               line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
              marker = list(colors = c("#F9AFC5", "#F9AFC5","#F9AFC5","#F9AFC5","#F9AFC5"),
                            line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
        layout(title="",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        layout(showlegend = FALSE)
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })


  })
}
