box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

data_type <- function(filter){
  if (filter=="Eine Organisation/Institution"){
    data <- import_data$data
  } 
  else {
    data <- subset(import_data$data, identity==filter)
  }
  data_type <- as.data.frame(table(unlist(data$organizationType)))
  data_type <- data_type %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                  pct1 = paste0(percentage, "%"))
  return(data_type)
}