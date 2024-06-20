box::use(
  shiny.fluent[Text, fluentPage,Dropdown.shinyInput],
  shiny[div, tags, NS, moduleServer, tagList,h1,h3],
  shiny.router, lubridate,
  jose, openssl
)

box::use(
  app/view/components/layouts,
  app/view/modules/quantitative_page/affected_person,
  app/view/modules/quantitative_page/age_of_affected_person,
  app/view/modules/quantitative_page/gender_identity,
  app/view/modules/quantitative_page/date_of_occurance,
  app/view/modules/quantitative_page/map,
  app/view/modules/quantitative_page/previous_measures,
  app/view/modules/quantitative_page/location_f,
  app/view/modules/quantitative_page/sexual_orientation,
  app/view/modules/quantitative_page/influence_discrimination,
  app/logic/import_data
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  shiny::uiOutput(ns("ui"))

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ################### GET the token variable in link
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
      
      if(token_json_data$email %in% import_data$login_data$email & token_json_data$role == import_data$role & converted_time > Sys.time()){
        shiny::tagList(
          div(class = "head_section",
              #h1(class = "quantitative_page__title", ""), #Quantitative statistics
              div(style="display: flex",
                  h3("Nach Identität filtern :"),
                  div(style="width: 200px; margin-top: 15px",
                      Dropdown.shinyInput(ns("filter"),
                                          value = import_data$options_filter[[1]]$key,
                                          defaultSelectedKeys = "A",
                                          options = import_data$options_filter
                      ))
              ),
              div( style = "float: right; display: flex;  gap: 0.5rem;",
                   shiny.fluent::Link(href=paste("#!/quantitative_bivariate?token=", current_token(), sep = ""),
                                      "Weiter zu Bivariate",
                                      style = "background-color: #fff; text-decoration:none; padding: 1.5em 1.5em;
                        border-color: #000; border-radius: 12px; display: flex;
                        border: 1px solid black; color: #000; font-weight: bold;"),
                   shiny.fluent::DefaultButton.shinyInput("export_quantitative", "Daten exportieren",
                                                          iconProps = list(iconName = "Download"),
                                                          style = "float: right;")
                   
              ),
              div(style = "float: right;  gap: 0.5rem; margin-top: 28px;",
                  shiny.fluent::DefaultButton.shinyInput("refresh", "Daten aktualisieren",
                                                         iconProps = list(iconName = "Refresh"),
                                                         style = "background-color: #fff; text-decoration:none; padding: 1em 1.5em;
                            text-align: center; border-color: #000; border-radius: 12px; height:45px;
                            border: 1px solid black;
                           color: #000; font-weight: bold;"
                  ))
          ),
          shiny::uiOutput(ns("quantitive_page"))
        )
      } else{
        shiny::h3("Error 500 - Internal Server Error")
      }
    })
    
    output$quantitive_page <- shiny::renderUI({
      
      if(input$filter == "Eine Organisation/Institution" | input$filter == "Eine andere Person" ){
        layouts$quantitative_page_layout(
          affected_person$ui(ns("affected_person")), 
          age_of_affected_person$ui(ns("age_of_affected_person")),
          "",
          map$ui(ns("map")),
          location_f$ui(ns("location_f")),
          "",
          gender_identity$ui(ns("gender_identity")),
          date_of_occurance$ui(ns("date_of_occurance")),
          ""
        )} else{
          layouts$quantitative_page_layout(
            affected_person$ui(ns("affected_person")), 
            age_of_affected_person$ui(ns("age_of_affected_person")),
            previous_measures$ui(ns("previous_measures")),
            map$ui(ns("map")),
            location_f$ui(ns("location_f")),
            sexual_orientation$ui(ns("sexual_orientation")),
            gender_identity$ui(ns("gender_identity")),
            date_of_occurance$ui(ns("date_of_occurance")),
            influence_discrimination$ui(ns("influence_discrimination"))
          )
        }
    })
    
    
    shiny::observeEvent(input$filter, {
      affected_person$server("affected_person", input$filter)
      age_of_affected_person$server("age_of_affected_person", input$filter)
      gender_identity$server("gender_identity", input$filter)
      location_f$server("location_f", input$filter)
      date_of_occurance$server("date_of_occurance", input$filter)
      map$server("map")
      previous_measures$server("previous_measures", input$filter)
      sexual_orientation$server("sexual_orientation", input$filter)
      influence_discrimination$server("influence_discrimination", input$filter)
      map$server("map", input$filter)
    })
    
  })
}
