box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,HTML,observeEvent,
        downloadHandler,downloadButton,outputOptions],
  shiny.router[router_server, route, router_ui],
  keyring[key_set,key_get],
  shinymanager[create_db,set_labels],
  shiny.fluent[fluentPage], utils[write.csv],
  shinyjs, lubridate,
  reactable
)

box::use(
  app/view/quantitative_page,
  app/view/qualitative_page,
  app/view/compare_page,
  app/view/more_compare_page,
  app/view/components/layouts,
  app/view/home_page,
  app/view/overview_page,
  app/view/wordcloud_page,
  app/view/quantitative_bivariate_page,
  app/logic/export_data,
  app/logic/mongo_fetch
)


#' @export
ui <- function(id) {
  #ns <- NS(id)
  fluentPage(
    shinyjs::useShinyjs(),
    div(
      style = "visibility: hidden;",
      downloadButton("download", label = "", verify_fa=FALSE)
    ),
    router_ui(
    route("home", layouts$main_layout(home_page$ui("home"))),
    route("quantitative", layouts$main_layout(quantitative_page$ui("quantitative"))),
    route("qualitative", layouts$main_layout(qualitative_page$ui("qualitative"))),
    #route("compare", layouts$main_layout(compare_page$ui("compare"))),
    #route("more_compare", layouts$main_layout(more_compare_page$ui("more_compare"))),
    #route("overview", layouts$main_layout(overview_page$overview_ui("overview"))),
    route("quantitative_bivariate", layouts$main_layout(quantitative_bivariate_page$ui("quantitative_bivariate"))),
    route("wordcloud", layouts$main_layout(wordcloud_page$wordcloud_ui("wordcloud")))
   #route("qualitative", layouts$main_layout(qualitative_page$ui(ns("qualitative")))),
   #route("compare", layouts$main_layout(more_insights_page$ui(ns("compare")))),
  ))
}



#' @export
server <- function(id, input, output, session) {

    shiny.router::router_server("")
    home_page$server("home")
    quantitative_page$server("quantitative")
    qualitative_page$server("qualitative")
    compare_page$server("compare")
    more_compare_page$server("more_compare")
    overview_page$overview_server("overview")
    wordcloud_page$wordcloud_server("wordcloud")
    quantitative_bivariate_page$server("quantitative_bivariate")

    data <- export_data$df
    observeEvent(input$export_quantitative, {
      shinyjs::click("download")
    })

    output$download <- downloadHandler(
      filename = function() {
        paste0("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    observeEvent(input$export_bivariate, {
      shinyjs::click("download")
    })
    #outputOptions (output, "download", suspendWhenHidden=FALSE)
  
    observeEvent(input$refresh, {
      print("refresh")
      shinyjs::delay(1000, {
        rm(list = ls())
        cat("\f")
        shinyjs::refresh()
        shinyjs::runjs("history.go(0)")
        app <- paste0(getwd(), "/app.R")
        Sys.setFileTime(app, lubridate::now())
      })
    })
}

