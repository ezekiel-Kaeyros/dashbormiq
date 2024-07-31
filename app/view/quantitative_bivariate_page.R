box::use(
  shiny.fluent[Text, fluentPage],
  shiny[div, tags, NS, moduleServer, tagList, h1],
)

box::use(
  app/view/components/layouts,
  app/view/modules/quantitative_bivariate/age_discrimination,
  app/view/modules/quantitative_bivariate/age_influence,
  app/view/modules/quantitative_bivariate/gender_discrimination,
  app/view/modules/quantitative_bivariate/map_topic,
)
box::use(
  app/logic/import_data
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  shiny::uiOutput(ns("ui"))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    current_token <- shiny::reactive({
      token <- shiny.router::get_query_param("token", session)
      if(is.null(token)){
        token <- "404"
      }else{
        token <- token 
      }
      token
    })
    
    output$ui <- shiny::renderUI({
      ############# Decode JWT
      token_json_data <- jose::jwt_decode_hmac(current_token(), secret = import_data$key)
      
      ############ Detect validity time of token
      converted_time <- as.POSIXct(token_json_data$exp, origin="1970-01-01", tz="Africa/Lagos")
      
      if(token_json_data$email %in% import_data$login_data$email & token_json_data$role == import_data$role & 
         converted_time > Sys.time()){
        div(style="background-color:#F6F6F6",
            layouts$quantitative_bivariate_layout(
              div(class = "head_section",
                  h1(class = "quantitative_page__title", ""),
                  div( style = "float: right; display: flex; gap: 0.5rem;",
                       div(#style = "float: right;  gap: 0.5rem; margin-top: 10px;",#28px
                         shiny.fluent::DefaultButton.shinyInput("refresh", "Daten aktualisieren",
                                                                iconProps = list(iconName = "Refresh"),
                                                                style = "background-color: #000; text-decoration:none; padding: 1.5em 1.5em;
                                    text-align: center; border-color: #fff; border-radius: 12px;
                                    border: 1px solid black;height:60px;
                                   color: #fff; font-weight: bold;"
                         )),
                       shiny.fluent::Link(href = paste("#!/quantitative?token=", current_token(), sep = ""),
                                          "Quantitativ",
                                          style = "background-color: #000; text-decoration:none; padding: 1.5em 1.5em;
                                          border-color: #fff; border-radius: 12px; border: 1px solid black;
                                   color: #fff; display: flex;"),
                       shiny.fluent::DefaultButton.shinyInput("export_bivariate", "Daten exportieren",
                                                              iconProps = list(iconName = "Download"))
                  ),
              ),
              age_discrimination$ui(ns("affected_person")),
                                                  age_influence$ui(ns("age_of_affected_person")),
                                                  map_topic$ui(ns("map")),
                                                  gender_discrimination$ui(ns("gender_discrimination"))))
        
          
      } else{
        shiny::h3("Error 500 - Internal Server Error")
      }
    })
        
    age_discrimination$server("affected_person")
    age_influence$server("age_of_affected_person")
    gender_discrimination$server("gender_discrimination")
    map_topic$server("map")
  })
}
