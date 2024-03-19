#   mongodb+srv://miq-user:Wy1N4zclOtlnR64d@miq-db.ppexwp3.mongodb.net/miq-db
box::use(
  jsonlite[fromJSON],
  rjson[fromJSON],
  magrittr[`%>%`],
  dplyr[mutate, if_else, summarise, group_by, select, n, rename,inner_join, filter],
  leaflet[colorFactor,leafletOutput,renderLeaflet,addProviderTiles,addPolygons,
          addLegend, leaflet, setView,removeScaleBar,labelOptions,highlightOptions],
  # leaflet[
  #   leafletOutput, renderLeaflet,
  #   leaflet, setView, addTiles
  # ],
  tm[stopwords, Corpus, VectorSource, tm_map, content_transformer, removeWords, removePunctuation,
     removeNumbers, stemDocument, stripWhitespace, DocumentTermMatrix],
  sf[st_sfc, st_centroid, st_coordinates],
  topicmodels[LDA, posterior],
  wordcloud2[wordcloud2],
  leaflet.minicharts[addMinicharts],
  janitor[clean_names], stats
)

#import data
root <- getwd()
path_data <- paste(root, "/", "data1", sep="")
file.data <- paste(path_data, "/data_f.json", sep="")
data <-  jsonlite::fromJSON(file.data)
#data2 <- janitor::clean_names(data)
#names(data) <- make.names(names(data))


# file_data_topic <- paste(path_data, "/", "incidents_description.json", sep="")
# textdata <- jsonlite::fromJSON(file_data_topic)
# # Générer un échantillon aléatoire des indices de textdata
# sample_indices <- sample(length(textdata), length(data$description), replace = TRUE)
# Remplacer les valeurs de medar$description par les valeurs correspondantes de textdata
#data$description <- textdata[sample_indices]

#generate column report_id for the data
for (i in 1:nrow(data)){
  data$report_id[i] <- paste ("Report_", i, sep = "")
}

#import geojson file for the map
file.data1 <- paste(path_data, "/germany_states.geojson", sep="")
geojson3 <-  rjson::fromJSON(file=file.data1)

#replacing modalities to short them because too long when displaying the graph
data$person_affected <- gsub("I am reporting on behalf of the affected person",
                             "On behalf of a person", data$person_affected)
data$person_affected <- gsub("An organization/institution",
                             "Organization/institution", data$person_affected)
data$age <- gsub("\\[0 - 17\\]", "Under 17", data$age)
data$age <- gsub("\\[66 - 100\\]", "Over 65", data$age)

#generation of data for the frequency of gender
data_gen <- as.data.frame(table(unlist(data$gender)))
data_gen <- data_gen %>%
  mutate(percentage = round(100*(Freq/sum(Freq)),2),
                pct1 = paste0(percentage, "%"))

# interval <- c(0, 18, 27, 40, 65, Inf)
# categories <- c("Under 18 years", "18-27 years", "28-40 years", "41-65 years", "Over 65 years")
# data$category_age <- cut(data$age, breaks = interval, labels = categories, right = FALSE)

data_age <- as.data.frame(table(data$age))
data_age <- data_age %>%
  dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                pct1 = paste0(percentage, "%"))

#convert dates 
data$time_incident$exactDate <- as.Date(data$time_incident$exactDate, format = "%d.%m.%Y")
data$time_incident$startDate <- as.Date(data$time_incident$startDate, format = "%d.%m.%Y")
data$time_incident$endDate <- as.Date(data$time_incident$endDate, format = "%d.%m.%Y")
data$report_date <- as.Date(data$report_date, format = "%d.%m.%Y")

#data for the graph of temporal distance
data_temp <- data %>%
  mutate(
    temporal_distance = if_else(is.na(time_incident$exactDate), as.numeric(difftime(report_date, time_incident$endDate, units = "days")),
                                as.numeric(difftime(report_date, time_incident$exactDate, units = "days"))),
    incident_occurence = if_else(temporal_distance > 0, "longer", "ongoing"),
    incident_occurence = if_else(is.na(time_incident$exactDate) & temporal_distance > 0, "singular", incident_occurence)
  ) %>%
  mutate(
    temporal_distance = if_else(temporal_distance < 0, 0,temporal_distance)
  )

data_months <- data_temp %>%
  mutate(
    temporal_class = cut(temporal_distance,breaks = c(-Inf,90,180,270,Inf),
                         labels = c("(0-3)months","(4-6)months","(7-9)months","(10+)months"))
  ) %>%
  select(temporal_class) %>%
  group_by(temporal_class) %>%
  summarise(total = n()) %>%
  mutate(percentage=round(100*(total/sum(total)),2),
         pct1=paste0(percentage,"%"))

#creation of variable location with the modalities online and real life
data$location <- ifelse(data$place_discrimination$incident_occurred_online, "Online", "Real life")
#data$office_name <-data$masures_taken$office_namely


data_onreal <- as.data.frame(table(data$place_discrimination$incident_occurred_online))
data_onreal <- data_onreal %>%
  dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                pct1 = paste0(percentage, "%"))

#generation of the data for the graph of frequency of person affected
data_personaf <- as.data.frame(table(data$person_affected))
data_personaf <- data_personaf %>%
  dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                pct1 = paste0(percentage, "%"))

# data_disc <- as.data.frame(table(unlist(data$another_discriminations)))
# for (i in 1:nrow(data_disc)){
#   data_disc$Freq1[i] <- round(data_disc$Freq[i]*nrow(data)/sum(data_disc$Freq),0)
# }
# data_disc$Var1 <- gsub("Other form, namely",
#                                      "Others", data_disc$Var1)
# data_disc <- data_disc %>%
#   dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
#                 pct1 = paste0(percentage, "%"))
# 
# data_inf_disc <- as.data.frame(table(unlist(data$influence_of_the_discrimination)))
# for (i in 1:nrow(data_inf_disc)){
#   data_inf_disc$Freq1[i] <- round(data_inf_disc$Freq[i]*nrow(data)/sum(data_inf_disc$Freq),0)
# }
# data_inf_disc$Var1 <- gsub("Text \\(free entry\\)",
#                        "Others", data_inf_disc$Var1)
# data_inf_disc <- data_inf_disc %>%
#   dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
#                 pct1 = paste0(percentage, "%"))

# data_choice_measures <- as.data.frame(table(unlist(data$masures_taken$choice_masures)))
# data_choice_measures <- data_choice_measures %>%
#   dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
#                 pct1 = paste0(percentage, "%"))

#file.dsph <- paste(path_data, "/DEU_adm_shp/DEU_adm1.shp", sep="")
file.dsph1 <- paste(path_data, "/DEU_adm_shp/DEU_adm2.shp", sep="")
# Allemagne <- sf::st_read("Data/DEU_adm_shp/DEU_adm1.shp",
#                          quiet = TRUE)
Allemagne <- sf::st_read(file.dsph1,
                         quiet = TRUE)

Allemagne <- Allemagne %>%
  rename(Province = NAME_2)
# 
#Allemagne$Value <-0
data_sel <- Allemagne %>% select(NAME_1,Province)
# 
data_sel <- data_sel %>% filter(NAME_1=="Nordrhein-Westfalen")
data_sel <- data_sel[order(data_sel$Province), ]
# for (i in 1:nrow(data1)) {
#   data1$Value[i] <- sample(0:50, 1)
# }

data$place <- data$place_discrimination$city
#data <- data[order(data$place),]
data1 <- data %>% select(person_affected, place) %>%
  group_by(place) %>%
  summarise(Value=n())
data1 <- stats::na.omit(data1)
data1 <- data1[order(data1$place),]
data1$geometry <- data_sel$geometry
data1$Province <- data_sel$Province
# data1 <- as.data.frame(data1)


#interval1 <- c(0, 20, 40, 60)
interval1 <- c(0, ceiling(max(data1$Value)/3), ceiling(max(data1$Value)*2/3), max(data1$value))
#breaks <- stats::quantile(data1$Value, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
#data1$cat <- cut(data1$Value, breaks = breaks, labels = c("Faible", "Moyen", "Élevé"))
data1 <- sf::st_as_sf(data1)

# categories1 <- c("0-20", "20-40", "40-60")
categories1 <- c("Faible", "Moyen", "Élevé")
data1$cat <- cut(data1$Value, breaks = interval1, labels = categories1, right = FALSE)

labels <- sprintf(
  "<strong>%s</strong><br/>%g persons affected </sup>",
  data1$Province, data1$Value
) %>% lapply(htmltools::HTML)

pal=colorFactor(palette=c("#FCD18C", "#CFAACB", "#93C89A"), domain=data1$cat)
m <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") # Vous pouvez choisir un autre style de tuile si vous le souhaitez

m <- leaflet(data = data1) %>%
  addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
  addPolygons(
    fillColor =  ~colorFactor(palette=c("#FCD18C", "#CFAACB", "#93C89A"), domain=data1$cat)(data1$cat),
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
  # addPolygons(
  #   #fillColor = colorFactor(palette = "viridis", domain = data1$Value)(data1$Value),
  #   fillColor=colorFactor(palette=c("#FCD18C", "#CFAACB", "#93C89A"), domain=data1$cat)(data1$cat),
  #   fillOpacity = 0.7,
  #   color = "white",
  #   weight = 1,
  #   label = ~paste(data1$Province, "Number of persons: ", data1$Value
  #                 )
  )%>%
  addLegend(position = "bottomright",
            pal=pal,
            values=~cat, title="Number of persons affected"

  ) %>%
  setView(lng=7.661594, lat=51.433237, zoom=7)



# data$another_discriminations <- sub("c\\(", "", data$another_discriminations)
# data$another_discriminations <- sub("\\)", "", data$another_discriminations)
# #data$another_discriminations <- sub('\\"', "", data$another_discriminations)
# data$another_discriminations <- gsub('"', "", data$another_discriminations)
# result <- data %>%
#   mutate(another_discriminations = str_split(another_discriminations, ", ")) %>%
#   unnest(another_discriminations) %>%
#   group_by(age_cat, another_discriminations) %>%
#   summarize(count = n())
# result$another_discriminations <- gsub("\\b(Other form|namely)\\b", "Others", result$another_discriminations, ignore.case = TRUE)
# result <- subset(result, another_discriminations != "character(0")

# fig <- result
# fig <- fig %>% plot_ly(x = ~age_cat, y = ~count, color = ~another_discriminations)
# fig



################## Data to display age_discrimination ######################

# Convertir les listes en vecteurs pour chaque groupe d'âge
discriminations_list <- unlist(data$another_form_discriminations)

# Créer un data frame avec les informations des groupes d'âge et des discriminations
data_age_disc <- data.frame(category_age = rep(data$age, sapply(data$another_form_discriminations, length)),
                   discrimination = discriminations_list)

# Calculer la fréquence des discriminations pour chaque groupe d'âge
table_age_disc <- with(data_age_disc, table(category_age, discrimination))

# Convertir la table de fréquence en un format de données adapté à Plotly
table_age_disc <- as.data.frame(table_age_disc)

# Remplacement de "other form, namely" dans la colonne another_discriminations par "other form"
table_age_disc$discrimination <- gsub("Other, specify", "Other form", table_age_disc$discrimination)


############## Data to display age_influence ###############
# Initialisation d'une liste vide pour stocker les influences
influence_list <- unlist(data$influence_of_the_discrimination)

# Créer un data frame avec les informations des groupes d'âge et les types d'influence
data_age_inf <- data.frame(category_age = character(),
                   influence = character(),
                   stringsAsFactors = FALSE)

# Parcourir chaque ligne de medar
for (i in seq_along(data$age)) {
  # Répéter age_cat pour chaque élément de medar$influence_of_the_influence
  category_age_repeat <- rep(data$age[i], length(data$influence_of_the_discrimination[[i]]))
  # Ajouter les valeurs à data
  data_age_inf <- rbind(data_age_inf, data.frame(category_age = category_age_repeat, influence = data$influence_of_the_discrimination[[i]]))
}
# Réinitialiser les indices
rownames(data_age_inf) <- NULL
# Calculer la fréquence des influences pour chaque groupe d'âge
table_age_inf <- with(data_age_inf, table(category_age, influence))

# Convertir la table de fréquence en un format de données adapté à Plotly
table_age_inf <- as.data.frame(table_age_inf)

# Remplacement de la modalité "Text (free entry)" par "Other"
table_age_inf$influence <- gsub("Text \\(free entry\\)", "Other", table_age_inf$influence)


########### Data to display gender discrimination ############
discriminations_list <- unlist(data$another_form_discriminations)

# Créer un data frame avec les informations des groupes d'âge et des discriminations
data_gender_disc <- data.frame(gender = rep(data$gender, sapply(data$another_form_discriminations, length)),
                   discrimination = discriminations_list)

# Calculer la fréquence des discriminations pour chaque groupe d'âge
table_gender_disc <- with(data_gender_disc, table(gender, discrimination))

#Convertir la table de fréquence en un format de données adapté à Plotly
table_gender_disc <- as.data.frame(table_gender_disc)

# Remplacer "Other form, namely" par "Other form" dans la colonne "discrimination"
table_gender_disc$discrimination <- gsub("Other form, namely", "Other form", table_gender_disc$discrimination)


################# Data to display Wordcloud #############


generate_wordcloud <- function(characteristics, min_freq = 10, language = "german") {
  # Charger les mots vides pour la langue spécifiée
  stopwords <- tm::stopwords(language)

  # Créer un objet Corpus
  corpus <- Corpus(VectorSource(characteristics))

  # Chaîne de prétraitement
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords)
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  corpus <- tm_map(corpus, removeNumbers)

  # Obtenir le texte de chaque document dans le corpus
  characteristics_text <- sapply(corpus, as.character)

  # Concaténer toutes les chaînes de caractères en une seule
  all_characteristics_text <- paste(characteristics_text, collapse = " ")

  # Séparation du texte en mots
  words <- unlist(strsplit(all_characteristics_text, "\\W+"))

  # Création d'une table de fréquence des mots
  word_freq <- table(words)

  # Convertir la table en data frame
  word_freq_df <- as.data.frame(word_freq)
  names(word_freq_df) <- c("Word", "Frequency")

  # Filtrer les mots avec une fréquence minimale
  word_freq_df <- subset(word_freq_df, Frequency >= min_freq)

  # Affichage du nuage de mots interactif avec wordcloud2
  wordcloud2(word_freq_df)
}

# Utilisation de la fonction avec les données
# characteristics <- data$description
# generate_wordcloud(characteristics)

textdata <- data$description
################## Data to generate map topic ##############
german_stopwords <- stopwords("german")
# create corpus object
corpus <- Corpus(VectorSource(textdata))
# preprocessing chain
processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, german_stopwords)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "german")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

# compute document term matrix with terms >= minimumFrequency
minimumFrequency <- 3
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency,Inf))))

# set.seed(61)
# K <- 5
# topicModel <- LDA(DTM, K, method="Gibbs", control = list(iter = 500, verbose = 25))
# lda_fit <- topicModel

file_save_lda_model <- paste(path_data, "/", "lda_model.rds", sep="")
# Sauvegarde du modèle au format rds
# saveRDS(lda_fit, file = file_save_lda_model)
# Charger le modèle LDA à partir du fichier RDS
lda_fit <- readRDS(file_save_lda_model)
#
# prediction <- c()
# for (i in 1: length(data$description)){
#   text_to_categorize <- data$description[i]
#   corpus_to_categorize <- Corpus(VectorSource(text_to_categorize))
#   text_to_categorize <- tm_map(corpus_to_categorize, content_transformer(tolower))
#   text_to_categorize <- tm_map(text_to_categorize, removeWords, german_stopwords)
#   text_to_categorize <- tm_map(text_to_categorize, removePunctuation, preserve_intra_word_dashes = TRUE)
#   text_to_categorize <- tm_map(text_to_categorize, removeNumbers)
#   text_to_categorize <- tm_map(text_to_categorize, stemDocument, language = "german")
#   text_to_categorize <- tm_map(text_to_categorize, stripWhitespace)
#
# #   # Créer une Document-Term Matrix (DTM) pour le texte
#   dtm_to_categorize <- DocumentTermMatrix(corpus_to_categorize)
#
#   test.topics <- posterior(lda_fit,dtm_to_categorize)
#   index_max_probability <- which.max(test.topics$topics)
#   prediction <- c(prediction,index_max_probability)
# }

file_save_prediction <- paste(path_data, "/", "prediction.rds", sep="")
#saveRDS(prediction, file_save_prediction)
prediction <- readRDS(file_save_prediction)
data$prediction <- prediction
data$prediction[data$prediction==1] <- "topic 1"
data$prediction[data$prediction==2] <- "topic 2"
data$prediction[data$prediction==3] <- "topic 3"
data$prediction[data$prediction==4] <- "topic 4"
data$prediction[data$prediction==5] <- "topic 5"

# ajout d'une colonne province sur medar avec des valeurs aleatoire des regions du Nordrhein-Westfalen
# Générer un échantillon aléatoire des indices de data1$Province
sample_Province_indices <- sample(length(data1$Province), length(data$description), replace = TRUE)
data$Province <- data1$Province[sample_Province_indices]

# Création d'un dataframe des fréquences des sujets par province et jointure avec data1
province_topics <- as.data.frame.matrix(table(data$Province, data$prediction))

# Réinitialiser les noms de lignes
province_topics$Province <- rownames(province_topics)
rownames(province_topics) <- NULL

# innner_join entre province_topics et data1
province_topics_data1 <-  data1 %>%
  dplyr::inner_join(province_topics, by = "Province")

latitude <- c()
longitude <- c()
for (i in 1:nrow(province_topics_data1)) {
  # Récupérer les coordonnées du centre du polygone
  geometry_sf <- st_sfc(province_topics_data1[i,]$geometry)
  centroid <- st_centroid(geometry_sf)
  latitude <- c(latitude, st_coordinates(centroid)[, "Y"])
  longitude <- c(longitude, st_coordinates(centroid)[, "X"])
}
province_topics$latitude <- latitude
province_topics$longitude <- longitude

# Afficharge du map
labels_topic <- sprintf(
  "<strong>%s</strong>
  <br/>topic 1: %g
  <br/>topic 2: %g
  <br/>topic 3: %g
  <br/>topic 4: %g
  <br/>topic 5: %g",
  province_topics_data1$Province,
  province_topics_data1$`topic 1`,
  province_topics_data1$`topic 2`,
  province_topics_data1$`topic 3`,
  province_topics_data1$`topic 4`,
  province_topics_data1$`topic 5`
) %>% lapply(htmltools::HTML)

topic_map <- leaflet(width = "100%", height = "400px") %>%
  addProviderTiles("CartoDB.Positron")%>%
  addPolygons(
    data = province_topics_data1,
    weight = 2,
    opacity = 1,
    color = "#666",
    fillOpacity = 0.1 ,
    label = labels_topic,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  )%>%
  addMinicharts(
    lng = province_topics$longitude,
    lat = province_topics$latitude,
    type = "bar",
    chartdata = province_topics[1:5],
    width = 25, height = 25,
  ) %>%
  setView(lng=7.661594, lat=51.433237, zoom=7.4)



############# Data to display cases online or in real life ################
data_onreal <- as.data.frame(table(data$location))
data_onreal <- data_onreal %>%
  dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                pct1 = paste0(percentage, "%"))

############### Data to display previous mesures ################
data$previous_measures <- data$masures_taken
previous_measures <-as.data.frame(table(unlist(data$previous_measures)))
num <- sum(is.na(data$previous_measures))
new_line <- data.frame(Var1="Did nothing", Freq=num)
previous_measures_f <- rbind(previous_measures,new_line)
previous_measures_f <- previous_measures_f %>%
  dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                pct1 = paste0(percentage, "%"))
previous_measures_f$Var1 <- gsub("I have reported the case to the police",
                                 "Reported to police", previous_measures_f$Var1)
previous_measures_f$Var1 <- gsub("I have visited a counseling center",
                                 "Counceling center visited", previous_measures_f$Var1)
previous_measures_f$Var1 <- gsub("other, specify",
                                 "Others", previous_measures_f$Var1)

############### to display select input Compare page #########
#key_var <- c(names(data))
data$sexual_orientation <- data$your_sexual_orientation
data$previous_measures_taken <- data$masures_taken
key_var <- c("person_affected", "gender",
             "influence_of_the_discrimination", "another_discriminations", "age", "location",
             "sexual_orientation", "previous_measures_taken")
text_var <- gsub("_", " ", key_var)
options_var <- lapply(seq_along(key_var), function(i) {
  list(key = key_var[i], text = text_var[i])
})

df <- data
df$influence_of_the_discrimination <- gsub("c\\(", "", df$influence_of_the_discrimination)
df$influence_of_the_discrimination <- gsub("\\)", "", df$influence_of_the_discrimination)

df$another_form_discriminations <- gsub("c\\(", "", df$another_form_discriminations)
df$another_form_discriminations <- gsub("\\)", "", df$another_form_discriminations)

df$your_sexual_orientation <- gsub("c\\(", "", df$your_sexual_orientation)
df$your_sexual_orientation <- gsub("\\)", "", df$your_sexual_orientation)

df <- df %>% dplyr::select(person_affected, gender, age, time_incident,report_date,
                    place_discrimination,description, masures_taken,influence_of_the_discrimination) #,influence_of_the_discrimination,another_discriminations


#influence of the discrimination and sexual orientation

#sexual orientation and 

