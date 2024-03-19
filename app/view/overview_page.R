box::use(
  shiny.fluent[Text, fluentPage],
  shiny[div, tags, NS, moduleServer, h3,h5, observe, parseQueryString,
        observeEvent, reactive,column],
  shiny.router,upstartr[not.na]
)

box::use(
  app/view/components/layouts,
  app/logic/import_data
)

#' @export
overview_ui <- function(id) {
  ns <- NS(id)
  div(id="summary",
      fluentPage(
        column(width = 6,
               h3("Summary"),
               shiny::textOutput(ns("report")),
               tags$br(),
               h3("Person affected"),
               shiny::textOutput(ns("pers_af")),
               h3("Gender identity"),
               shiny::textOutput(ns("gender")),
               h3("Age"),
               shiny::textOutput(ns("age")),
               h3("Date"),
               shiny::textOutput(ns("date")),
               shiny::conditionalPanel(
                 condition = not.na(shiny::textOutput(ns("date"))),
                 tags$h5("Incident lasted for a period: ",
                         shiny::textOutput(ns("startdate")),
                         "-",
                         shiny::textOutput(ns("enddate")))
               ),
               h3("Place of incident"),
               shiny::textOutput(ns("location")),
               h3("What happened"),
               shiny::textOutput(ns("description")),
               h3("Iinfluential Characteristics on Discrimination"),
               shiny::textOutput(ns("inf_disc")),
               h3("Is this another form of discrimination?"),
               shiny::textOutput(ns("form_disc")),
               h3("Previous measures"),
               shiny::textOutput(ns("previous_measures"))
        ),
        column(width = 6

        )

      )
      )

}

#' @export
overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    current_id <- reactive({
      id <- shiny.router::get_query_param("id", session)
      #return(id)
      #print(id)
    })
    output$report <- shiny::renderText({
      current_id()
    })
    output$pers_af <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      report$person_affected
    })
    output$gender <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      report$gender
    })
    output$age <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      as.character(report$age_cat)[1]
    })
    output$date <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      if (not.na(report$time_incident$exactDate)) {
        format(as.Date(report$time_incident$exactDate),"%d %B %Y")
      }
    })
    output$startdate <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      if (not.na(report$time_incident$startDate)) {
        format(as.Date(report$time_incident$startDate),"%d %B %Y")
      }
    })
    output$enddate <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      if (not.na(report$time_incident$startDate)) {
        format(as.Date(report$time_incident$endDate),"%d %B %Y")
      }
    })

    output$location <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      if( report$location=="Online") {
        report$location
      } else {
        report$place_discrimination$city
      }
    })
    output$description <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      report$description
    })
    output$inf_disc <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      report$influence_of_the_discrimination[[1]]
      paste(report$influence_of_the_discrimination[[1]], collapse = ", ")
    })
    output$form_disc <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      paste(report$another_discriminations[[1]], collapse = ", ")
    })
    output$previous_measures <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      if (!is.null(report$masures_taken$choice_masures[[1]])) {
        paste(report$masures_taken$choice_masures[[1]], collapse = ", ")
      } else {
        paste("No")
      }
    })




  })
}
