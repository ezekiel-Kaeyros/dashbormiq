box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

data_sex <- function(filter){
  if (filter=="Alle"){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  data_sex <- as.data.frame(table(unlist(data$sexualOrientation)))
  data_sex <- data_sex %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                  pct1 = paste0(percentage, "%"))
  data_sex <- subset(data_sex,Var1!="FALSE")
  
  return(data_sex)
}

data_sex1 <- function(filter){
  if (filter=='Alle'){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  return(data)
}

