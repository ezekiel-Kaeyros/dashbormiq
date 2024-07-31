box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

data_emp <- function(filter){
  if (filter!="Eine Organisation/Institution"){
    data <- import_data$data
  } 
  else {
    data <- subset(import_data$data, identity==filter)
    #data <- data %>% filter(identity==filter)
  }
  data_emp <- as.data.frame(table(unlist(data$numberOfEmployees)))
  data_emp <- data_emp %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                  pct1 = paste0(percentage, "%"))
  return(data_emp)
}

data_emp1 <- function(filter){
  if (filter!="Eine Organisation/Institution"){
    data <- import_data$data
  } 
  else {
    data <- subset(import_data$data, identity==filter)
  }
  return(data)
}
