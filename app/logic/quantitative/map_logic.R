box::use(
  dplyr[n], magrittr[`%>%`],
  leaflet[colorFactor,addProviderTiles,addPolygons,
          addLegend, leaflet, setView,removeScaleBar,labelOptions,highlightOptions],
)

box::use(
  app/logic/import_data
)

# Allemagne <- import_data$Allemagne %>%
#   dplyr::rename(Province = NAME_2)
data1 <- import_data$data1

# max <- max(data1$Value)
# interval1 <- c(0, ceiling(max(data1$Value)/3), ceiling(max(data1$Value)*2/3), max)

#data for legend
#categories1 <- c("No data", "Discrimination")
# data1$cat <- dplyr::if_else(data1$Value==0,"No data","Discrimination") #, right = FALSE

labels <- sprintf(
  "<strong>%s</strong><br/>%g persons affected </sup>",
  data1$Province, data1$Value
) %>% lapply(htmltools::HTML)

#pal=colorFactor(palette=c("#ffa500","#0000ff","#8f00ff"), domain=data1$cat)
map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") # Vous pouvez choisir un autre style de tuile si vous le souhaitez

map <- leaflet(data = data1) %>%
  addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
  addPolygons(
    #fillColor =  ~colorFactor(palette=c("#ffa500","#0000ff","#8f00ff"), domain=data1$cat)(data1$cat), #"#FCD18C", "#CFAACB", "#93C89A"
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
            
 # ) %>%
  setView(lng=7.661594, lat=51.433237, zoom=7)
