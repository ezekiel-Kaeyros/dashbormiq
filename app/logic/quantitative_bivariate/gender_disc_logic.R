box::use(
  app/logic/import_data
)

data <-import_data$data
all <- expand.grid(Var1 = unlist(data$gender), Var2 = unlist(data$formOfQueerphobia))
table_gender_disc <- as.data.frame(table(all))
# discriminations_list <- unlist(data$formOfQueerphobia)
# 
# # Créer un data frame avec les informations des groupes d'âge et des discriminations
# data_gender_disc <- data.frame(Var1 = rep(data$gender, sapply(data$formOfQueerphobia, length)),
#                                Var2 = discriminations_list)
# 
# # Calculer la fréquence des discriminations pour chaque groupe d'âge
# table_gender_disc <- with(data_gender_disc, table(Var1, Var2))
# 
# #Convertir la table de fréquence en un format de données adapté à Plotly
# table_gender_disc <- as.data.frame(table_gender_disc)
# 
# # Remplacer "Other form, namely" par "Other form" dans la colonne "discrimination"
# #table_gender_disc$Var2 <- gsub("Other form, namely", "Other form", table_gender_disc$Var2)