box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

data_age <- function(filter){
  if(filter=="Alle"){
    data_person_age <- import_data$data
  }
  else {
    data_person_age <- subset(import_data$data, identity==filter, select = c(age))
  }
  
  data_age <- as.data.frame(table(data_person_age$age))
  data_age <- data_age %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                  pct1 = paste0(percentage, "%"))
  
  return(data_age)
}

data_age1 <- function(filter){
  if (filter=="Alle"){
    data <- import_data$data
    return(data)
  } else {
    data <- subset(import_data$data, identity==filter)
    return(data)
  }
}