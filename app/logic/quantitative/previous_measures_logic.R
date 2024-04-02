box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

previous_measures <- function(filter){
  if (filter=="Alle"){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  previous_measures <-as.data.frame(table(unlist(data$haveYouReportedYes)))
  num <- sum(is.na(data$haveYouReportedYes)) 
  #num <- sum(data$previous_measures!="chr//(0//)")
  new_line <- data.frame(Var1="Nichts getan", Freq=num)
  previous_measures_f <- rbind(previous_measures,new_line)
  previous_measures_f <- previous_measures_f %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                  pct1 = paste0(percentage, "%"))
  previous_measures_f$Var1 <- gsub("Ich habe den Fall bei der Polizei angezeigt",
                                   "Der Polizei gemeldet", previous_measures_f$Var1)
  previous_measures_f$Var1 <- gsub("Ich habe den Fall bei einer anderen Meldestelle angezeigt, und zwar: ",
                                   "An eine andere Meldestelle gemeldet", previous_measures_f$Var1)
  previous_measures_f$Var1 <- gsub("Anderes, und zwar:",
                                   "Anderes", previous_measures_f$Var1)
  return(previous_measures_f)
}


