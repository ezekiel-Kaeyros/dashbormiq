box::use(
  shiny[moduleServer, div,NS, h1,h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/logic/mongo_fetch
)

#' @export
ui <- function(id) {
  #ns <- NS(id)
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #ns <- session$ns
    connection_string = "mongodb+srv://miq-user:Wy1N4zclOtlnR64d@miq-db.ppexwp3.mongodb.net/miq-db"
    #autoInvalidate <- shiny::reactiveTimer(10000)
    
    data <- shiny::reactivePoll(1000, session,
                         # This function returns the time that log_file was last modified
                         checkFunc = function() {
                           
                         },
                         # This function returns the content of log_file
                         valueFunc = function() {
                           mongo_fetch$fetch_mongodb(connection_string = connection_string, collection = "reports", db="miq-db")
                         }
    )
    # shiny::observe({
    #   print("yo")
    #   #data(mongo_fetch$fetch_mongodb(connection_string = connection_string, collection = "reports", db="miq-db"))
    #   if (!is.null(data())) {
    #     saveRDS(data, paste("app/data/", "dataa.rds", sep = ""))
    #     message("Data refreshed and saved successfully!")
    #   } else {
    #     message("Error: Failed to retrieve data from MongoDB.")
    #   }
    #   #saveRDS(data(), paste("app/data/","dataa",".rds", sep = ""))
    # })
    
    
  })
}