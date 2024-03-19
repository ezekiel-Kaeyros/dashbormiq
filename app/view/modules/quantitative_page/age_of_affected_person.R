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
  cards$card_ui("Age categories of affected persons",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "PieSingle")),
                div(class = "card_content",
                    h3(class = "subtitle", names(table(import_data$data$age))[which.max(table(import_data$data$age))]),
                    p(class = "description", "Modal age of affected persons"),
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
    #output$plot_personaf <- render
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
      plotly::plot_ly(import_data$data_age, x = ~Var1,
                      type = "bar",
                      y = ~percentage,
                      #marker = list(color = c("#0B5345", "#148F77", "#196F3D", "#52BE80", "#7DCEA0", "#CA6F1E")),
                      # marker = list(color = c("#483D8B", "slateblue","#0077BE", "#5696CC",
                      #                         "#76B7DA",  "#A6DAFF")),
                      marker =list(color="#F8D1A4"),
                      #colors = "darkviolet",
                      #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                      text = paste(import_data$data_age$pct1, sep = ""), textposition = 'outside',
                      textfont = list(size = 10), # size is defined here
                      hovertext = paste("Age: ", import_data$data_age$Var1,
                                        "<br>Number of persons :", import_data$data_age$Freq,
                                        "<br>Percentage :",import_data$data_age$pct1), #) %>%
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
      plot_ly(import_data$data_age, labels= ~Var1,
              values= ~Freq, type="pie",
              hoverinfo = 'text',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              text = ~paste("Age :", Var1,
                            "<br>Number of persons :", Freq,
                            "<br>Percentage :", pct1),
              # marker = list(colors = c("#5072A7", "#1D428A","#0000ff","#7630ff","#20B2AA"),
              #               line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
              marker = list(colors = c("#F8D1A4", "#F8D1A4","#F8D1A4","#F8D1A4","#F8D1A4"),
                            line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
        layout(title="",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        layout(showlegend = FALSE)
    })

    observeEvent(input$toggleButton, {
      toggle$piechart <- !toggle$piechart
    })


  })
}

