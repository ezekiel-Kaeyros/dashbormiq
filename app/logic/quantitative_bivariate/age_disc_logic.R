box::use(
  app/logic/import_data
)

data <- import_data$data

discriminations_list <- unlist(data$formOfQueerphobia)

# Créer un data frame avec les informations des groupes d'âge et des discriminations
data_age_disc <- data.frame(Var1 = rep(data$age, sapply(data$formOfQueerphobia, length)),
                            Var2 = discriminations_list)

# Calculer la fréquence des discriminations pour chaque groupe d'âge
table_age_disc <- with(data_age_disc, table(Var1, Var2))

# Convertir la table de fréquence en un format de données adapté à Plotly
table_age_disc <- as.data.frame(table_age_disc)

# Remplacement de "other form, namely" dans la colonne another_discriminations par "other form"
#table_age_disc$Var2 <- gsub("Other, specify", "Other form", table_age_disc$Var2)