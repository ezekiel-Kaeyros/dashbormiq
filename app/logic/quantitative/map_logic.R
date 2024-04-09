box::use(
  dplyr[n], magrittr[`%>%`],
  leaflet[colorFactor,addProviderTiles,addPolygons,
          addLegend, leaflet, setView,removeScaleBar,labelOptions,highlightOptions],
)

box::use(
  app/logic/import_data
)

#data1 <- import_data$data1

map <- function(filter) {
  if (filter=="Alle"){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  
  data_sel <- import_data$Allemagne %>% dplyr::select(NAME_1,Province)
  #
  data_sel <- data_sel %>% dplyr::filter(NAME_1=="Nordrhein-Westfalen")
  #data_sel <- data_sel[order(data_sel$Province), ]
  data_sel$place <- data_sel$Province
  
  data$place <- data$location
  
  data11 <- data %>% dplyr::select(identity, place) %>%
    dplyr::group_by(place) %>%
    dplyr::summarise(Value=n())
  data11 <- data11 %>% dplyr::filter(place!="")
  data2 <- as.data.frame(setdiff(data_sel$Province, data11$place))
  data2 <- data2 %>% dplyr::rename(place=`setdiff(data_sel$Province, data11$place)`)
  data2$Value <- 0
  data1 <- rbind(data11,data2)
  data1$cat <- dplyr::if_else(data1$Value==0,"No data","Discrimination")
  data1 <- data1 %>%
    dplyr::inner_join(data_sel, by="place")
  
  
  data1 <- sf::st_as_sf(data1)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g person(s) affected </sup>",
    data1$Province, data1$Value
  ) %>% lapply(htmltools::HTML)
  
  #pal=colorFactor(palette=c("#FCD18C", "#CFAACB", "#93C89A"), domain=data1$cat)
  map <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") # Vous pouvez choisir un autre style de tuile si vous le souhaitez
  
  map <- leaflet(data = data1) %>%
    addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
    addPolygons(
      #fillColor =  ~colorFactor(palette=c("#FCD18C", "#CFAACB", "#93C89A"), domain=data1$cat)(data1$cat),
      fillColor = ~colorFactor(palette=c("#0000ff","#8f00ff"),domain=data1$cat)(data1$cat),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.4,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    )%>%
    # addLegend(position = "bottomright",
    #           pal=pal,
    #           values=~cat, title="Number of persons affected"
    #
    # ) %>%
    setView(lng=7.661594, lat=51.433237, zoom=7)
  #return(map)
}

