box::use(
  app/logic/import_data
)

data <-import_data$data
all <- expand.grid(Var1 = unlist(data$gender), Var2 = unlist(data$formOfQueerphobia))
table_gender_disc <- as.data.frame(table(all))
