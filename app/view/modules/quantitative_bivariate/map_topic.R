box::use(
  shiny[NS,moduleServer,div, h1, p],
  leaflet[leafletOutput,renderLeaflet,addProviderTiles,addPolygons,addLegend, leaflet],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  #leafletOutput("map_plot")
  cards$card_ui("Map of Topics by Province",
                "",
                div(class = "card_content",
                    h1(class = "subtitle", ""),
                    p(class = "description", ""),
                    # Graph goes here
                    leafletOutput(ns("map_plot"), width="600px", height=405) #450
                )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$map_plot <- renderLeaflet({
     import_data$topic_map
    })

  })
}
