box::use(
  shiny.fluent[Text, fluentPage],
  shiny[div, tags, NS, moduleServer, tagList],
)

box::use(
  app/view/components/layouts,
  app/view/modules/quantitative_page/affected_person,
  app/view/modules/quantitative_page/age_of_affected_person,
  app/view/modules/quantitative_page/gender_identity,
  app/view/modules/quantitative_page/date_of_occurance,
  app/view/modules/quantitative_page/map,
  app/view/modules/quantitative_page/previous_measures,
  app/view/modules/quantitative_page/location_f
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  layouts$quantitative_page_layout(affected_person$ui(ns("affected_person")), age_of_affected_person$ui(ns("age_of_affected_person")),
                                   map$ui(ns("map")),location_f$ui(ns("location_f")),gender_identity$ui(ns("gender_identity")),
                                   date_of_occurance$ui(ns("date_of_occurance")), previous_measures$ui(ns("previous_measures"))) #,location$ui(ns("location"))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    affected_person$server("affected_person")
    age_of_affected_person$server("age_of_affected_person")
    gender_identity$server("gender_identity")
    date_of_occurance$server("date_of_occurance")
    map$server("map")
    previous_measures$server("previous_measures")
    location_f$server("location_f")

#     shiny::observeEvent(input$export_quantitative, {
#       print("quantitative button")
#     })
  })
}
