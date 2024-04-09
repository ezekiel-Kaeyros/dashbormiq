box::use(
  app/logic/import_data
)

data <- import_data$data

# Créer un data frame avec les informations des groupes d'âge et les types d'Var2
data_age_inf <- data.frame(Var1 = character(),
                           Var2 = character(),
                           stringsAsFactors = FALSE)

# Parcourir chaque ligne de medar
for (i in seq_along(data$age)) {
  # Répéter age_cat pour chaque élément de medar$influence_of_the_influence
  category_age_repeat <- rep(data$age[i], length(data$typeOfDiscrimination[[i]]))
  # Ajouter les valeurs à data
  data_age_inf <- rbind(data_age_inf, data.frame(Var1 = category_age_repeat, Var2 = data$typeOfDiscrimination[[i]]))
}
# Réinitialiser les indices
rownames(data_age_inf) <- NULL
# Calculer la fréquence des influences pour chaque groupe d'âge
table_age_inf <- with(data_age_inf, table(Var1, Var2))

# Convertir la table de fréquence en un format de données adapté à Plotly
table_age_inf <- as.data.frame(table_age_inf)

# Remplacement de la modalité "Text (free entry)" par "Other"
#table_age_inf$Var2 <- gsub("Text \\(free entry\\)", "Other", table_age_inf$Var2)