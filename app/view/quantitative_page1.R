box::use(
  shiny.fluent[Text, fluentPage,Dropdown.shinyInput],
  shiny[div, tags, NS, moduleServer, tagList,h1,h3],
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
  layouts$quantitative_page_layout(div(class = "head_section",
                                       #h1(class = "quantitative_page__title", ""), #Quantitative statistics
                                       div(style="display: flex",
                                           h3("Filter by identity:"),
                                           div(style="width: 200px; margin-top: 15px",
                                               Dropdown.shinyInput(ns("filter"),
                                                                   value = import_data$options_filter[[1]]$key,
                                                                   options = import_data$options_filter
                                               ))
                                       ),
                                       div( style = "float: right;  gap: 0.5rem;",
                                            shiny.fluent::Link(href="#!/quantitative_bivariate", "Go to Bivariate",
                                                               style = "background-color: #fff; text-decoration:none; padding: 1em 1.5em;
                        text-align: center; border-color: #000; border-radius: 12px;
                        border: 1px solid black;
                       color: #000; font-weight: bold;"),
                                            shiny.fluent::DefaultButton.shinyInput("export_quantitative", "Export data",
                                                                                   iconProps = list(iconName = "Download"))
                                            
                                       ),
  ),
  
  affected_person$ui(ns("affected_person")), 
  #shiny::conditionalPanel("input.filter!='An organization/institution'", age_of_affected_person$ui(ns("age_of_affected_person"))),
  age_of_affected_person$ui(ns("age_of_affected_person")),
  previous_measures$ui(ns("previous_measures")),
  map$ui(ns("map")),location_f$ui(ns("location_f")),
  sexual_orientation$ui(ns("sexual_orientation")),
  gender_identity$ui(ns("gender_identity")),
  date_of_occurance$ui(ns("date_of_occurance")), 
  influence_discrimination$ui(ns("influence_discrimination"))) #,location$ui(ns("location"))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    shiny::observeEvent(input$filter, {
      affected_person$server("affected_person", input$filter)
      age_of_affected_person$server("age_of_affected_person", input$filter)
      gender_identity$server("gender_identity", input$filter)
      location_f$server("location_f", input$filter)
      date_of_occurance$server("date_of_occurance", input$filter)
      previous_measures$server("previous_measures", input$filter)
      influence_discrimination$server("influence_discrimination", input$filter)
      sexual_orientation$server("sexual_orientation", input$filter)
      map$server("map", input$filter) 
    })
    
  })
}