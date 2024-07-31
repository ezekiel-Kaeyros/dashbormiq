box::use(
  shiny[NS,moduleServer,div, h1, p],
  leaflet[leafletOutput,renderLeaflet,addProviderTiles,addPolygons,addLegend, leaflet],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative/map_logic
  
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  #leafletOutput("map_plot")
  cards$card_ui("Karte der betroffenen Personen nach Bundesländern",
                "",
                div(class = "card_content",
                    h1(class = "subtitle", ""),
                    p(class = "description", ""),
                    # Graph goes here
                    leafletOutput(ns("map_plot"), height=470) #450
                )
  )

}

#' @export
server <- function(id,filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$map_plot <- renderLeaflet({
     map_logic$map(filter)
    })

  })
}
