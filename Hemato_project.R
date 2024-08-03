data = read.table("Donnees_memoire_SenGenome.csv", h=T, sep=";", dec=",", row.names=1)
View(data)
summary(data)
# data$Sexe <- as.factor(data$Sexe)
data_frame <- as.data.frame(data)
summary(data_frame)


convertion <- function(data_frame) {
  column_names <- names(data_frame)
  column_factors <- c("Sexe", "Adresse", "Region", "Departement", "Ethnie", "Sous_Ethnie",
                      "DT2", "Maladie_cardiovasculaire", "Lupus_erythemateux_systemique", "Dermatite_atopique",
                      "Polyarthrite_rhumatoide", "Beth_Vincent", "Simonin", "Rhesus", "Resultat")
  for (column in column_names) {
    if (column %in% column_factors) {
      data_frame[[column]] <- as.factor(data_frame[[column]])
    }
    else {
      data_frame[[column]] <- as.numeric(data_frame[[column]])
    }
  }
  return(data_frame)
}
data_frame = convertion(data_frame=data_frame)
summary(data_frame$Ethnie)
levels(data_frame$Ethnie)

# Data of ethnie and blood group
# Install and load dplyr package
# names(data_frame)
# install.packages("dplyr")
library(dplyr)
ethnie_blood_group_df <- select(data_frame, Region, Ethnie, Sous_Ethnie, Beth_Vincent)
ethnie_blood_group_df

check_na_value <- function(data_frame)
if (any(is.na(ethnie_blood_group_df))) {
  print("Presence of Na value")
  } else {
  print("No Na values")
}

summary(ethnie_blood_group_df$Beth_Vincent)
t = table(ethnie_blood_group_df$Ethnie, ethnie_blood_group_df$Beth_Vincent)
t
# Frequence phenotypique, allelique et genotype selon les ethnies 
groupes_ethniques <- ethnie_blood_group_df %>% 
  group_by(Ethnie)
resultats <- groupes_ethniques %>%
  summarise(
    freq_A = round(sum(Beth_Vincent == "A") / n(), 3),  # Calculer la fréquence phénotypique A
    freq_B = round(sum(Beth_Vincent == "B") / n(), 3),  # Calculer la fréquence phénotypique B
    freq_O = round(sum(Beth_Vincent == "O") / n(), 3),  # Calculer la fréquence phénotypique O
    freq_AB = round(sum(Beth_Vincent == "AB") / n(), 3), # calculer la fréquebce phenotypique AB
    p = round(1 - sqrt(freq_B + freq_O), 3),   # Calculer la fréquence allélique p
    q = round(1 - sqrt(freq_A + freq_O), 3),   # Calculer la fréquence allélique q
    r = round(1 - p - q, 3),                    # Calculer la fréquence allélique r
    AA = round(p**2, 3),
    AO = round(2 * p * r, 3),
    BB = round(q**2, 3),
    BO = round(2 * q * r, 3),
    AB = round(2 * p * q, 3),
    OO = round(r**2, 3)
    )

ethnie_rhesus_df <- select(data_frame, Region, Ethnie, Sous_Ethnie, Rhesus)

ethnie_rhesus_df <- ethnie_rhesus_df %>%
  mutate(Rhesus = as.character(Rhesus)) %>%
  mutate(Rhesus = replace(Rhesus, is.na(Rhesus), "Unknown")) %>%
  mutate(Rhesus = as.factor(Rhesus))

summary(ethnie_rhesus_df$Rhesus)


groupes_ethniques_2 <- ethnie_rhesus_df %>% 
  group_by(Ethnie)

resultats_2 <- groupes_ethniques_2 %>%
  summarise(
    frequence_D = round(sum(Rhesus == "Positif") / n(), 2),  # Calculer la fréquence phénotypique D
    frequence_d = round(sum(Rhesus == "Negatif") / n(), 2),  # Calculer la fréquence phénotypique d
    d = round(sqrt(frequence_d), 2),
    D = round((1 - d), 2)
  )

# --------------------------------------------------------#
# Analyse avec les NA dropped
cleaned_data <- na.omit(ethnie_rhesus_df)
cleaned_data

groupes_ethniques_3 <- cleaned_data %>% 
  group_by(Ethnie)

resultats_3 <- groupes_ethniques_2 %>%
  summarise(
    frequence_D = round(sum(Rhesus == "Positif") / n(), 2),  # Calculer la fréquence phénotypique D
    frequence_d = round(sum(Rhesus == "Negatif") / n(), 2),  # Calculer la fréquence phénotypique d
    d = round(sqrt(frequence_d), 2),
    D = round((1 - d), 2)
  )
# --------------------------------------------------------#