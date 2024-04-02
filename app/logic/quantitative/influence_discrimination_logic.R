box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

influence_discrimination <- function(filter){
  if(filter=="Alle"){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  influence_discrimination <- as.data.frame(table(unlist(data$typeOfDiscrimination)))
  influence_discrimination <- influence_discrimination %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                  pct1 = paste0(percentage, "%"))
  influence_discrimination$Var1 <- gsub("Anderes, und zwar",
                                        "Anderes", influence_discrimination$Var1)
  return(influence_discrimination)
}

