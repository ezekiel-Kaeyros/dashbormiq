box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config,style,ggplotly],
  magrittr[`%>%`],
  ggplot2[ggplot,geom_tile,geom_text,scale_fill_gradient,labs,aes,theme,element_text],
  tm[stopwords, Corpus, VectorSource, tm_map, content_transformer, removeWords, removePunctuation,
     removeNumbers, stemDocument, stripWhitespace, DocumentTermMatrix],
  wordcloud2[wordcloud2],
)

#' @export
#' generate_barplot <- function(data, text) {
#'   plotly::plot_ly(data, x = ~Var1,
#'                   type = "bar",
#'                   y = ~percentage,
#'                   #marker = list(color = c("#0B5345", "#148F77", "#196F3D", "#52BE80", "#7DCEA0", "#CA6F1E")),
#'                   marker = list(color = c("#ff0000", "#ffa500","#ffff00", "#00ff00",
#'                                           "#0000ff",  "#4b0082", "#8f00ff","#FE2E9A","#81BEF7","#B43104","#FA5858","#D0FA58")),
#'                   #marker =list(color="#85C2FF"),
#'                   #colors = "darkviolet",
#'                   #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
#'                   text = paste(data$pct1, sep = ""), textposition = 'outside',
#'                   textfont = list(size = 10), # size is defined here
#'                   hovertext = paste(paste(text,":",data$Var1),
#'                     #"Person affected: ", data$Var1,
#'                                     "<br>Number of persons :", data$Freq,
#'                                     "<br>Percentage :",data$pct1), #) %>%
#'                   #"<br>Percentage :", data_marsta()$pct1),
#'                   hoverinfo = 'text') %>%
#'     layout(title = "",#margin = list(l=25, r=50, b=50, t=50, pad=4),
#'            #legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b> </b>')),
#'            uniformtext=list(minsize=10, mode='show'),
#'            xaxis = list(title = "<b> </b>", #font = list(size = 0),
#'                         # change x-axix size
#'                         tickfont = list(size = 11),
#'                         # change x-title size
#'                         titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
#'                         tickangle= -45, showgrid = FALSE),
#'            yaxis = list(title = "<b> Percentage </b>",
#'                         titlefont = list(size = 12),
#'                         # change x-axix size
#'                         tickfont = list(size = 12),
#'                         ticksuffix = "%", showgrid = FALSE)
#'     ) %>%
#'     config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
#'       'sendDataToCloud',
#'       #'toImage',
#'       'autoScale2d',
#'       'zoomIn2d',
#'       "zoomOut2d",
#'       'toggleSpikelines',
#'       'resetScale2d',
#'       'lasso2d',
#'       'zoom2d',
#'       'pan2d',
#'       'select2d',#,
#'       'hoverClosestCartesian',#,
#'       'hoverCompareCartesian'),
#'       scrollZoom = FALSE)
#' }
generate_barplot <- function(data, text, marker) {
  plotly::plot_ly(data, x = ~Var1,
                  type = "bar",
                  y = ~percentage,
                  marker = marker,
                  text = paste(data$pct1, sep = ""), textposition = 'outside',
                  textfont = list(size = 10), # size is defined here
                  hovertext = paste(paste(text,":",data$Var1),
                                    #"Person affected: ", data$Var1,
                                    "<br>Number of persons :", data$Freq,
                                    "<br>Percentage :",data$pct1), #) %>%
                  hoverinfo = 'text') %>%
    layout(title = "",
           uniformtext=list(minsize=10, mode='show'),
           xaxis = list(title = "<b> </b>", 
                        tickfont = list(size = 8),
                        titlefont = list(size = 16), 
                        tickangle= -45, showgrid = FALSE),
           yaxis = list(title = "<b> Percentage </b>",
                        titlefont = list(size = 12),
                        # change x-axix size
                        tickfont = list(size = 12),
                        ticksuffix = "%", showgrid = FALSE)
    ) %>%
    config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
      'sendDataToCloud',
      #'toImage',
      'autoScale2d',
      'zoomIn2d',
      "zoomOut2d",
      'toggleSpikelines',
      'resetScale2d',
      'lasso2d',
      'zoom2d',
      'pan2d',
      'select2d',#,
      'hoverClosestCartesian',#,
      'hoverCompareCartesian'),
      scrollZoom = FALSE)
}

#' @export
#' generate_piechart <- function(data, text) {
#'   plotly::plot_ly(data, labels= ~Var1,
#'           values= ~Freq, type="pie",
#'           hoverinfo = 'text',
#'           textinfo = 'label+percent',
#'           insidetextfont = list(color = '#000',size = 8),
#'           texttemplate = '<b>%{label}</br></br>%{percent}</b>',
#'           text = ~paste(paste(text,":",Var1),
#'                         "<br>Number of persons :", Freq,
#'                         "<br>Percentage :", pct1),
#'           marker = list(color = c("#0000ff", "#0000ff","#0000ff", "#0000ff",
#'                                   "#0000ff",  "#0000ff", "#0000ff","#0000ff","#0000ff","#0000ff","#0000ff","#0000ff"),
#'                         line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
#'     # marker = list(colors = c("#85C2FF", "#85C2FF","#85C2FF","#85C2FF","#85C2FF"),
#'     #               line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
#'     layout(title="",
#'            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#'            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
#'     layout(showlegend = FALSE) %>%
#'     config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
#'       'sendDataToCloud',
#'       #'toImage',
#'       'autoScale2d',
#'       'zoomIn2d',
#'       "zoomOut2d",
#'       'toggleSpikelines',
#'       'resetScale2d',
#'       'lasso2d',
#'       'zoom2d',
#'       'pan2d',
#'       'select2d',#,
#'       'hoverClosestCartesian',#,
#'       'hoverCompareCartesian'),
#'       scrollZoom = FALSE)
#' }
generate_piechart <- function(data, text, color) {
  plotly::plot_ly(data, labels= ~Var1,
                  values= ~Freq, type="pie",
                  hoverinfo = 'text',
                  textinfo = 'label+percent',
                  texttemplate = '<b>%{label}</br></br>%{percent}</b>',
                  insidetextfont = list(color = '#000',size = 8),
                  text = ~paste(paste(text,":",Var1),
                                "<br>Number of persons :", Freq,
                                "<br>Percentage :", pct1),
                  marker = list(colors = color,
                                line = list(color = '#FFFFFF', width = 1),
                                showlegend = FALSE)) %>%
    layout(title="",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
    layout(showlegend = FALSE) %>%
    config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
      'sendDataToCloud',
      #'toImage',
      'autoScale2d',
      'zoomIn2d',
      "zoomOut2d",
      'toggleSpikelines',
      'resetScale2d',
      'lasso2d',
      'zoom2d',
      'pan2d',
      'select2d',#,
      'hoverClosestCartesian',#,
      'hoverCompareCartesian'),
      scrollZoom = FALSE)
}

#' @export
generate_groupedbarplot <- function(data, text1,text2){
  yiord_palette <- c("#CAF0F8", "#ADE8F4", "#90E0EF", "#48CAE4", "#00B4D8", "#0096C7", "#0077B6", "#023E8A", "#03045E")
  plotly::plot_ly(data, x = ~Var1, y = ~Freq, color = ~Var2, type = "bar", colors = yiord_palette,
                  text = ~paste(paste(text1,":"), Var1, "<br>Frequency: ", Freq, "<br>",paste(text2,":"), Var2)) %>%
    layout(#title = "Frequency of Different Forms of Discrimination by Age Group",
      xaxis = list(title = text1),
      yaxis = list(title = "frequency"),
      barmode = "group")%>%
    style(hoverinfo = "text") %>%
    config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
      'sendDataToCloud',
      #'toImage',
      'autoScale2d',
      'zoomIn2d',
      "zoomOut2d",
      'toggleSpikelines',
      'resetScale2d',
      'lasso2d',
      'zoom2d',
      'pan2d',
      'select2d',#,
      'hoverClosestCartesian',#,
      'hoverCompareCartesian'),
      scrollZoom = FALSE)
} 

#' @export
generate_table <- function(data, text1, text2) {
  gg<-ggplot(data, aes(Var1, Var2)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = round(Freq, 1), text = paste(paste(text1,":"), Var1, "\n",paste(text2,":"), Var2, "\nCount:", Freq))) +
    scale_fill_gradient(low = "#CAF0F8", high = "#03045E") +
    labs(#title = "Frequency of Different Forms of Discrimination by Age Group",
      x = text1,#Age Group
      y = text2,#Discrimination
      fill = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggplotly(gg, tooltip = "text") %>%
    config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
      'sendDataToCloud',
      #'toImage',
      'autoScale2d',
      'zoomIn2d',
      "zoomOut2d",
      'toggleSpikelines',
      'resetScale2d',
      'lasso2d',
      'zoom2d',
      'pan2d',
      'select2d',#,
      'hoverClosestCartesian',#,
      'hoverCompareCartesian'),
      scrollZoom = FALSE)
}

#' @export
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
  corpus <- tm_map(corpus, removeWords, c("bemerkungen","dass"))

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
