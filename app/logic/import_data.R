#   mongodb+srv://miq-user:Wy1N4zclOtlnR64d@miq-db.ppexwp3.mongodb.net/miq-db
#Sys.setlocale("LC_ALL","English")
#Sys.setenv(LANG = "en_US.UTF-8")

box::use(
  jsonlite[fromJSON],
  rjson[fromJSON],
  magrittr[`%>%`],
  dplyr[mutate, if_else, summarise, group_by, select, n, rename,inner_join, filter],
  leaflet[colorFactor,leafletOutput,renderLeaflet,addProviderTiles,addPolygons,
          addLegend, leaflet, setView,removeScaleBar,labelOptions,highlightOptions],
  tm[stopwords, Corpus, VectorSource, tm_map, content_transformer, removeWords, removePunctuation,
     removeNumbers, stemDocument, stripWhitespace, DocumentTermMatrix],
  sf[st_sfc, st_centroid, st_coordinates],
  topicmodels[LDA, posterior],
  wordcloud2[wordcloud2],
  leaflet.minicharts[addMinicharts],
  janitor[clean_names], stats, tidyr
)

box::use(
  app/logic/mongo_fetch
)

#import data
root <- getwd()
path_data <- paste(root, "/", "data1", sep="")

# library(mongolite)
# library(tidyverse)

connection_string = "mongodb+srv://miq-user:Wy1N4zclOtlnR64d@miq-db.ppexwp3.mongodb.net/miq-db"
# Fonction pour rafraîchir les données depuis MongoDB
# refresh_data <- function() {
#   mongo_conn <- mongolite::mongo(collection="reports", db="miq-db", url=connection_string)
#   mongo_data <- mongo_conn$find()
#   mongo_data <- as.data.frame(mongo_data)
#   return(mongo_data)
# }
# 
# # Rafraîchir les données toutes les 5 secondes
# refresh_interval <- 5  # en secondes
# while(TRUE) {
#   data <- refresh_data()
#   #print(mongo_data)
#   Sys.sleep(refresh_interval)
# }

data <- mongo_fetch$fetch_mongodb(connection_string = connection_string, collection = "reports", db="miq-db")


################ CALL DATA IN THE DB WITH REACTIVE CONSUMER
dataxl <- shiny::reactivePoll(1000, session = shiny::getDefaultReactiveDomain(),
                            # This function returns the time that log_file was last modified
                            checkFunc = function() {
                              
                            },
                            # This function returns the content of log_file
                            valueFunc = function() {
                              mongo_fetch$fetch_mongodb(connection_string = connection_string, collection = "reports", db="miq-db")
                            }
)
# shiny::observe({
#   print(dataxl())
# })
#import geojson file for the map
file.data1 <- paste(path_data, "/germany_states.geojson", sep="")
geojson3 <-  rjson::fromJSON(file=file.data1)

file_data_topic <- paste(path_data, "/", "incidents_description.json", sep="")
textdata <- jsonlite::fromJSON(file_data_topic)
# Générer un échantillon aléatoire des indices de textdata
sample_indices <- sample(length(textdata), length(data$description), replace = TRUE)
# Remplacer les valeurs de medar$description par les valeurs correspondantes de textdata
data$description <- textdata[sample_indices]

#convert dates 
data$valueDate <- as.Date(data$valueDate) #, format = "%d.%m.%Y"
data <- tidyr::separate(data, dateRangeState, into = c("startDate", "endDate"), sep = ",")
data$startDate <- as.Date(data$startDate)
data$endDate <- as.Date(data$endDate)
data$createdAt <- as.Date(data$createdAt)


#creation of variable location with the modalities online and real life
data$locationOnline <- gsub("Es ist online passiert.", "Online",data$locationOnline)
data$locationOnline <- gsub("freeField", "Reales Leben",data$locationOnline)

data$haveYouReportedYes <- ifelse(data$haveYouReportedYes == "character(0)", NA, data$haveYouReportedYes)

file.dsph1 <- paste(path_data, "/DEU_adm_shp/DEU_adm3.shp", sep="")
# Allemagne <- sf::st_read("Data/DEU_adm_shp/DEU_adm1.shp",
#                          quiet = TRUE)
Allemagne <- sf::st_read(file.dsph1,
                         quiet = TRUE)


Allemagne <- dplyr::rename(Allemagne,  Province = `NAME_3`)

data_sel <- Allemagne %>% dplyr::select(NAME_1,Province)
# 
data_sel <- data_sel %>% dplyr::filter(NAME_1=="Nordrhein-Westfalen")
#data_sel <- data_sel[order(data_sel$Province), ]
data_sel$place <- data_sel$Province

data$place <- data$location

#construction of the dataframe to pass in the leaflet function
data11 <- data %>% dplyr::select(identity, place) %>%
  dplyr::group_by(place) %>%
  dplyr::summarise(Value=n())
data11 <- data11 %>% dplyr::filter(place!="")
data2 <- as.data.frame(setdiff(data_sel$Province, data11$place))
data2 <- data2 %>% rename(place=`setdiff(data_sel$Province, data11$place)`)
data2$Value <- 0
data1 <- rbind(data11,data2)
data1$cat <- dplyr::if_else(data1$Value==0,"No data","Discrimination")
data1 <- data1 %>%
  dplyr::inner_join(data_sel, by="place")

#convert data to display the map
data1 <- sf::st_as_sf(data1)

############### to display select input Compare page #########
key_var <- c("identity", "gender",
             "typeOfDiscrimination", "age", "locationOnline","organizationType","numberOfEmployees",
             "formOfDisc", "formOfDiscYes","haveYouReported","haveYouReportedYes","formofQueerphobia")


options_var <- lapply(seq_along(key_var), function(i) {
  list(key = key_var[i], text = key_var[i])
})

filter_list <-c("Alle",unique(data$identity))
filter_text <- c("Alle",unique(data$identity))
options_filter <- lapply(seq_along(filter_list), function(i) {
  list(key = filter_list[i], text = filter_text[i])
})

#topic modelling
# sotu_corpus <- quanteda::corpus(data$description)
# corp = quanteda::corpus_reshape(sotu_corpus, to = "sentences")
# #corp = corpus_reshape(data_corpus_inaugural, to = "paragraphs")
# dfm = quanteda::dfm(corp, remove_punct=T, remove=stopwords("german"))
# dfm = quanteda::dfm_trim(dfm, min_docfreq = 5)
# 
# dtm = quanteda::convert(dfm, to = "topicmodels")
# set.seed(1)
# 
# m = topicmodels::LDA(dtm, method = "Gibbs", k = 5,  control = list(alpha = 0.1))
# 
# dtm = dtm[slam::row_sums(dtm) > 0, ]
# phi = as.matrix(topicmodels::posterior(m)$terms)
# theta <- as.matrix(topicmodels::posterior(m)$topics)
# vocab <- colnames(phi)
# doc.length = slam::row_sums(dtm)
# term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]
# 
# json = LDAvis::createJSON(phi = phi, theta = theta, vocab = vocab,
#                           doc.length = doc.length, term.frequency = term.freq)
# route <- paste(path_data,"/Topic_modelling", sep="")
# LDAvis::serVis(json, out.dir = route, open.browser = FALSE)