box::use(
  tm[stopwords, Corpus, VectorSource, tm_map, content_transformer, removeWords, removePunctuation,
     removeNumbers, stemDocument, stripWhitespace, DocumentTermMatrix],
  wordcloud2[wordcloud2]
)

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
