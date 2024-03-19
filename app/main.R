box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,HTML,observeEvent,
        downloadHandler,downloadButton,outputOptions],
  shiny.router[router_server, route, router_ui],
  keyring[key_set,key_get],
  shinymanager[create_db,set_labels],
  shiny.fluent[fluentPage], utils[write.csv],
  shinyjs

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
  app/logic/import_data
)

# credentials <- data.frame(
#   user = c("shiny", "shinymanager"),
#   password = c("azerty", "12345"),
#   # password will automatically be hashed
#   admin = c(FALSE, TRUE),
#   stringsAsFactors = FALSE
# )
#
# key_set("R-shinymanager-key", "antid")
#
# create_db(
#   credentials_data = credentials,
#   sqlite_path = "app/data/database.sqlite", # will be created
#   #passphrase = key_get("R-shinymanager-key", "antid")
#    passphrase = "passphrase_wihtout_keyring"
# )

# css <- HTML(".btn-primary {
#                   color: #ebe8e8;
#                   background-color: #2B8049;
#                   border-color: #2B8049;
# }
#               .panel-auth {
#               background-color: #2B8049;
#               }
#               .panel-primary {
#                   border-color: #2B8049;
#                   width: 100%;
#                   z-index: 2;
#                   top: 0;
#                   left: 0;
#                   height: 100%;
#                   position: fixed;
#                   max-width: 750px;
#                   padding: 200px 90px;
#               }")
# set_labels(
#   language = "en",
#   "Please authenticate" = "",
#   "Language"=""
# )


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
    route("compare", layouts$main_layout(compare_page$ui("compare"))),
    route("more_compare", layouts$main_layout(more_compare_page$ui("more_compare"))),
    route("overview", layouts$main_layout(overview_page$overview_ui("overview"))),
    route("quantitative_bivariate", layouts$main_layout(quantitative_bivariate_page$ui("quantitative_bivariate"))),
    route("wordcloud", layouts$main_layout(wordcloud_page$wordcloud_ui("wordcloud")))
   #route("qualitative", layouts$main_layout(qualitative_page$ui(ns("qualitative")))),
   #route("compare", layouts$main_layout(more_insights_page$ui(ns("compare")))),
  ))
}


# ui <- shinymanager::secure_app(ui,choose_language = FALSE, enable_admin = TRUE,
#                  # changing theme for the credentials
# 
#                  theme = shinythemes::shinytheme("united"),
#                  tags_top = tags$div(
#                    tags$head(tags$style(css)),
#                    tags$h3("Login", style = "text-align: left; font-weight: bold;"), #align:lnbeft
#                    tags$h5("Welcome back! Please log in to access your account ", style = "text-align: left;"),
# 
#                  )
# )

#' @export
server <- function(id, input, output, session) {
    #ns <- session$ns

    # res_auth <- shinymanager::secure_server(
    #   check_credentials = shinymanager::check_credentials(
    #     "app/data/database.sqlite",
    #     #passphrase = key_get("R-shinymanager-key", "antid")
    #      passphrase = "passphrase_wihtout_keyring"
    #   )
    # )
    router_server("home")

    home_page$server("home")
    quantitative_page$server("quantitative")
    qualitative_page$server("qualitative")
    compare_page$server("compare")
    more_compare_page$server("more_compare")
    overview_page$overview_server("overview")
    wordcloud_page$wordcloud_server("wordcloud")
    quantitative_bivariate_page$server("quantitative_bivariate")

    data <- import_data$df
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
    #outputOptions (output, "download", suspendWhenHidden=FALSE)

}

