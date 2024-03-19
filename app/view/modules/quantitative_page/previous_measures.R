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
  app/logic/import_data,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Previous measures taken",#"",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "PieSingle")),
                div(class = "card_content",
                    h3(class = "subtitle", "Did nothing"), #a refaire
                    p(class = "description", "Most previous measure taken"),
                    # Graph goes here
                    uiOutput(ns("previous_measures"))#,width="500px", height = 485) #,width="500px"
                )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
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
    output$previous_measures <- renderUI({
      if (toggle$piechart) {
        plotlyOutput(ns("piechart"))
      } else {
        plotlyOutput(ns("barplot"))
      }
    })

    output$barplot <- renderPlotly({
      plotly::plot_ly(import_data$previous_measures_f, x = ~Var1,
                      type = "bar",
                      y = ~percentage,
                      marker =list(color="#F8D1A4"),
                      text = paste(import_data$previous_measures_f$pct1, sep = ""), textposition = 'outside',
                      textfont = list(size = 10), # size is defined here
                      hovertext = paste("Action: ", import_data$previous_measures_f$Var1,
                                        "<br>Number of persons :", import_data$previous_measures_f$Freq,
                                        "<br>Percentage :",import_data$previous_measures_f$pct1), #) %>%
                      #"<br>Percentage :", data_marsta()$pct1),
                      hoverinfo = 'text') %>%
        layout(title = "",
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
        config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
          'sendDataToCloud',
          #'toImage',
          'autoScale2d',
          'zoomIn2d',
          "zoomOut2d",
          'toggleSpikelines',
          'resetScale2d',
          'lasso2d',
          'zoom2d',
          'pan2d',
          'select2d',#,
          'hoverClosestCartesian',#,
          'hoverCompareCartesian'),
          scrollZoom = T)

    })

    output$piechart <- renderPlotly({
      plot_ly(import_data$previous_measures_f, labels= ~Var1,
              values= ~Freq, type="pie",
              hoverinfo = 'text',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              text = ~paste("Action :", Var1,
                            "<br>Number of persons :", Freq,
                            "<br>Percentage :", pct1),
              # marker = list(colors = c("#5072A7", "#1D428A","#0000ff","#7630ff","#20B2AA"),
              #               line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
              marker = list(colors = c("#F8D1A4", "#F8D1A4","#F8D1A4","#F8D1A4","#F8D1A4"),
                            line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
        layout(title="",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        layout(showlegend = FALSE) %>%
        config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
          'sendDataToCloud',
          'hoverClosestPie',
          #'toImage',
          'autoScale2d',
          'zoomIn2d',
          "zoomOut2d",
          'toggleSpikelines',
          'resetScale2d',
          'lasso2d',
          'zoom2d',
          'pan2d',
          'select2d',#,
          'hoverClosestCartesian',#,
          'hoverCompareCartesian'),
          scrollZoom = T)
    })

    observeEvent(input$toggleButton, {
      toggle$piechart <- !toggle$piechart
    })


  })
}
