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
summary(data_frame)
levels(data_frame$Ethnie)


# Data of ethnie and blood group
# Install and load dplyr package
# names(data_frame)
#install.packages("dplyr")
library(dplyr)
ethnie_blood_group_df <- select(data_frame, Region, Ethnie, Sous_Ethnie, Beth_Vincent, Resultat)
ethnie_blood_group_df

#----------------------------------------------------------------
#summary of data
# Pourcentage AB0
summary(data_frame)

blood_group_per_region <- table(data_frame$Region, data_frame$Beth_Vincent)
blood_group_per_region
round(prop.table(blood_group,1), 2)





#----------------------------------------------------------------
check_na_value <- function(data_frame)
if (any(is.na(ethnie_blood_group_df))) {
  print("Presence of Na value")
  } else {
  print("No Na values")
}

summary(ethnie_blood_group_df$Beth_Vincent)
t = table(ethnie_blood_group_df$Ethnie, ethnie_blood_group_df$Beth_Vincent)
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

write.csv(resultats, "frequences_selon_ethnies.csv", row.names = FALSE)

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
write.csv(resultats_2, file="frequence_rhesus_by_ethnie.csv", row.names = FALSE)

ethnie_blood_rhesus_df <- select(data_frame, Region, Ethnie, Sous_Ethnie, Beth_Vincent, Resultat)
ethnie_blood_rhesus_df <- na.omit(ethnie_blood_rhesus_df)
summary(ethnie_blood_rhesus_df$Resultat)

groupes_ethniques_3 <- ethnie_blood_rhesus_df %>% 
  group_by(Ethnie)

resultats_3 <- groupes_ethniques_3 %>%
  summarise(
    freq_A_neg = round(sum(Resultat == "A-") / n(), 2),  # Calculer la fréquence phénotypique D
    freq_A_pos = round(sum(Resultat == "A+") / n(), 2),
    freq_AB_pos = round(sum(Resultat == "AB+") / n(), 2),
    freq_AB_neg = round(sum(Resultat == "AB-") / n(), 2),
    freq_B_neg = round(sum(Resultat == "B-") / n(), 2),
    freq_B_pos = round(sum(Resultat == "B+") / n(), 2),
    freq_O_neg = round(sum(Resultat == "0-") / n(), 2),
    freq_O_pos = round(sum(Resultat == "O+") / n(), 2),
  )
write.csv(resultats_3, file="AB0_Rhesus_freq_by_ethnies.csv", row.names=FALSE)

# --------------------------------------------------------#
# Analyse avec les NA dropped
ethnie_rhesus_df_2 <- select(data_frame, Region, Ethnie, Sous_Ethnie, Beth_Vincent, Rhesus, Resultat)
cleaned_data <- na.omit(ethnie_rhesus_df_2)
summary(cleaned_data)

groupes_ethniques_3 <- cleaned_data %>% 
  group_by(Ethnie)

resultats_3 <- groupes_ethniques_3 %>%
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
    OO = round(r**2, 3),
    frequence_D = round(sum(Rhesus == "Positif") / n(), 2),  # Calculer la fréquence phénotypique D
    frequence_d = round(sum(Rhesus == "Negatif") / n(), 2),  # Calculer la fréquence phénotypique d
    d = round(sqrt(frequence_d), 2),
    D = round((1 - d), 2),
    freq_A_neg = round(sum(Resultat == "A-") / n(), 2),  # Calculer la fréquence phénotypique D
    freq_A_pos = round(sum(Resultat == "A+") / n(), 2),
    freq_AB_pos = round(sum(Resultat == "AB+") / n(), 2),
    freq_AB_neg = round(sum(Resultat == "AB-") / n(), 2),
    freq_B_neg = round(sum(Resultat == "B-") / n(), 2),
    freq_B_pos = round(sum(Resultat == "B+") / n(), 2),
    freq_O_neg = round(sum(Resultat == "0-") / n(), 2),
    freq_O_pos = round(sum(Resultat == "O+") / n(), 2),
  )
write.csv(resultats_3, file="all_freq_whithout_NA.csv", row.names=FALSE)
#--------------------------------------------------------------# 
# Good code for making correlation between two categorical variable
# Point Biserial correlation
#x: num and y: category
# convert y to binary
# pb = data_frame
# pb = pb %>% mutate(
#   y = y %>% recode("Male" = 0, "Female" = 1))
# pbcorr <- cor(x, y)


names(data_frame)

corr_group_desease_df <- select(data_frame, 
                              Ethnie, 
                              DT2, 
                              Maladie_cardiovasculaire,
                              Lupus_erythemateux_systemique, 
                              Dermatite_atopique, 
                              Polyarthrite_rhumatoide,
                              Beth_Vincent,
                              Rhesus,
                              Resultat)
corr_group_desease_df <- na.omit(corr_group_desease_df)
table <-table(corr_group_desease_df$Resultat, corr_group_desease_df$Rhesus)
chisq.test(table)
prop.test(table)
install.packages("psych")
libary(psych)
# phi(table) # for just 2 col and 2 rows
install.packages("lsr")
libary(lsr)
cramersV(table)
# Let's apply to for all columns(next)
#----------------------------------------------------------------------------#

#summary(corr_group_desease_df$Resultat)

to_test <- names(corr_group_desease_df)
count <- 0
for (var in to_test) {
  count <- count + 1
  if (var != "Resultat") {
    tb <- table(corr_group_desease_df$Resultat, corr_group_desease_df[[var]])
    chi2_test <- chisq.test(tb)
    
    if (count == 1) {
      library(vcd)
    }
    print(paste(var, "test: "))
    print(chi2_test)
    print("------------------------")
    #print("Correlation result: ")
    #print(assocstats(tb)$cramersV)
    
  }
}

# (tb = xtabs(corr_group_desease_df$DT2 + corr_group_desease_df$Beth_Vincent))
library(dplyr)
corr_group_varq <- select(data_frame,
                          Cholesterol_LDL,
                          Hemoglobine_HGB,
                          Globules_Blancs_WBC,
                          Plaquettes_PLT,
                          Beth_Vincent,
                          Rhesus,
                          Resultat)


corr_group_varq <- na.omit(corr_group_varq)


# ANOVA


resultat_anova <- aov(corr_group_varq$Hemoglobine_HGB ~ corr_group_varq$Resultat, data = corr_group_varq)
summary(resultat_anova)


hist(corr_group_varq$Hemoglobine_HGB)
qqnorm(corr_group_varq$Hemoglobine_HGB)
qqline(corr_group_varq$Hemoglobine_HGB, col = "red")




cholesterol_ldl <- corr_group_varq$Hemoglobine_HGB
shapiro_test <- shapiro.test(cholesterol_ldl)
print(shapiro_test)
# Create a data frame for ggplot2
data <- data.frame(cholesterol_ldl)

# Create the histogram with a normal distribution curve
library(ggplot2)
ggplot(data, aes(x = cholesterol_ldl)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "gray", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(cholesterol_ldl, na.rm = TRUE), sd = sd(cholesterol_ldl, na.rm = TRUE)), color = "blue", size = 1) +
  labs(title = "Histogram with Normal Distribution Curve", x = "Cholesterol LDL", y = "Density") +
  theme_minimal()



# kruskal test
resultat_kruskal <- kruskal.test(corr_group_varq$Plaquettes_PLT ~ corr_group_varq$Resultat, data = corr_group_varq)
resultat_kruskal

# # Régression Linéaire
#modele <- lm(corr_group_varq$Hemoglobine_HGB ~ corr_group_varq$Resultat, data = corr_group_varq)
#summary(modele)




boxplot(corr_group_varq$Plaquettes_PLT ~ corr_group_varq$Resultat, data = corr_group_varq, main = "Boxplot des scores par groupe", xlab = "Groupe",ylab = "Score")

#install.packages("psych")
#library(psych)
#point_biserial_corr <- biserial(corr_group_varq$Cholesterol_LDL, corr_group_varq$Beth_Vincent)

# --------------------------------------------------------#

# Cartographie
libary(ggplot2)
libary(tidyverse)



map_group <- select(data_frame, Region, Beth_Vincent)
summary(map_group$Region)
table = table(map_group$Region, map_group$Beth_Vincent)
t = prop.table(table,2) * 100
t

data_2 = read.table("freq_ABO.csv", h=T, sep=";", dec=".")
sum(data_2$A)
map_group <- map_group %>% rename(subregion = Region)
map_group <- map_group %>%
  mutate(region = "Senegal")



groupes <- map_group %>% 
  group_by(region)
mapdata <- map_data("world") ##ggplot2
View(mapdata)
mapdata <- left_join(mapdata, groupes, by="region")


#---------------------------------------------------------
# Calculer les fréquences de ABO par groupe ethnique
frequences_ABO <- data_frame %>%
  group_by(Ethnie, Beth_Vincent) %>%
  summarise(count = n()) %>%
  mutate(frequency = round(count / sum(count), 4)) %>%
  ungroup()



# Créer une table de contingence
table_contingence <- xtabs(count ~ Ethnie + Beth_Vincent, data = frequences_ABO)
table_contingence
# Effectuer le test du Chi-carré
test_chi2 <- chisq.test(table_contingence)

# Afficher les résultats du test
print(test_chi2)


# Fonction pour effectuer le test exact de Fisher pour une paire de groupes ethniques
fisher_test_pair <- function(ethnie1, ethnie2, data) {
  # Filtrer les données pour les deux ethnies
  data_fisher <- data %>%
    filter(Ethnie %in% c(ethnie1, ethnie2))
  
  # Créer une table de contingence
  table_fisher <- xtabs(count ~ Ethnie + Beth_Vincent, data = data_fisher)
  
  # Effectuer le test exact de Fisher
  test_fisher <- fisher.test(table_fisher)
  p_value_rounded <- round(test_fisher$p.value, 3)
  
  #return(data.frame(Ethnie1 = ethnie1, Ethnie2 = ethnie2, P_Value = test_fisher$p.value))
  return(data.frame(Ethnie1 = ethnie1, Ethnie2 = ethnie2, P_Value = p_value_rounded))
}
# Obtenir toutes les combinaisons de paires de groupes ethniques
ethnies <- unique(frequences_ABO$Ethnie)
combinations <- combn(ethnies, 2, simplify = FALSE)

# Effectuer le test exact de Fisher pour chaque paire de groupes ethniques
test_results <- do.call(rbind, lapply(combinations, function(pair) fisher_test_pair(pair[1], pair[2], frequences_ABO)))

# Appliquer la correction de Bonferroni
test_results$P_Value_Corrected <- p.adjust(test_results$P_Value, method = "bonferroni")

# Afficher les résultats
print(test_results)
write.csv(test_results, file="test_ABO.csv")
# Calculer les fréquences de ABO/rhesus par groupe ethnique
frequences_resultat <- cleaned_data %>%
  group_by(Ethnie, Resultat) %>%
  summarise(count = n()) %>%
  mutate(frequency = round(count / sum(count), 4)) %>%
  ungroup()



# Créer une table de contingence
table_contingence <- xtabs(count ~ Ethnie + Resultat, data = frequences_resultat)
table_contingence
# Effectuer le test du Chi-carré
test_chi2 <- chisq.test(table_contingence)

# Afficher les résultats du test
print(test_chi2)


# Fonction pour effectuer le test exact de Fisher pour une paire de groupes ethniques
fisher_test_pair <- function(ethnie1, ethnie2, data) {
  # Filtrer les données pour les deux ethnies
  data_fisher <- data %>%
    filter(Ethnie %in% c(ethnie1, ethnie2))
  
  # Créer une table de contingence
  table_fisher <- xtabs(count ~ Ethnie + Resultat, data = data_fisher)
  
  # Effectuer le test exact de Fisher
  test_fisher <- fisher.test(table_fisher)
  p_value_rounded <- round(test_fisher$p.value, 3)
  
  #return(data.frame(Ethnie1 = ethnie1, Ethnie2 = ethnie2, P_Value = test_fisher$p.value))
  return(data.frame(Ethnie1 = ethnie1, Ethnie2 = ethnie2, P_Value = p_value_rounded))
}
# Obtenir toutes les combinaisons de paires de groupes ethniques
ethnies <- unique(frequences_resultat$Ethnie)
combinations <- combn(ethnies, 2, simplify = FALSE)

# Effectuer le test exact de Fisher pour chaque paire de groupes ethniques
test_results <- do.call(rbind, lapply(combinations, function(pair) fisher_test_pair(pair[1], pair[2], frequences_resultat)))

# Appliquer la correction de Bonferroni
test_results$P_Value_Corrected <- p.adjust(test_results$P_Value, method = "bonferroni")

# Afficher les résultats
print(test_results)

write.csv(test_results, file="test_fisher_ABO_rhesus.csv")

# Calculer les fréquences de rhesus par groupe ethnique
frequences_rhesus <- cleaned_data %>%
  group_by(Ethnie, Rhesus) %>%
  summarise(count = n()) %>%
  mutate(frequency = round(count / sum(count), 4)) %>%
  ungroup()



# Créer une table de contingence
table_contingence <- xtabs(count ~ Ethnie + Rhesus, data = frequences_rhesus)
table_contingence
# Effectuer le test du Chi-carré
test_chi2 <- chisq.test(table_contingence)

# Afficher les résultats du test
print(test_chi2)


# Fonction pour effectuer le test exact de Fisher pour une paire de groupes ethniques
fisher_test_pair <- function(ethnie1, ethnie2, data) {
  # Filtrer les données pour les deux ethnies
  data_fisher <- data %>%
    filter(Ethnie %in% c(ethnie1, ethnie2))
  
  # Créer une table de contingence
  table_fisher <- xtabs(count ~ Ethnie + Rhesus, data = data_fisher)
  
  # Effectuer le test exact de Fisher
  test_fisher <- fisher.test(table_fisher)
  p_value_rounded <- round(test_fisher$p.value, 3)
  
  #return(data.frame(Ethnie1 = ethnie1, Ethnie2 = ethnie2, P_Value = test_fisher$p.value))
  return(data.frame(Ethnie1 = ethnie1, Ethnie2 = ethnie2, P_Value = p_value_rounded))
}
# Obtenir toutes les combinaisons de paires de groupes ethniques
ethnies <- unique(frequences_rhesust$Ethnie)
combinations <- combn(ethnies, 2, simplify = FALSE)

# Effectuer le test exact de Fisher pour chaque paire de groupes ethniques
test_results <- do.call(rbind, lapply(combinations, function(pair) fisher_test_pair(pair[1], pair[2], frequences_resultat)))

# Appliquer la correction de Bonferroni
test_results$P_Value_Corrected <- p.adjust(test_results$P_Value, method = "bonferroni")

# Afficher les résultats
print(test_results)

write.csv(test_results, file="test_fisher_rhesus.csv")


#---------------------------------------------------------



 