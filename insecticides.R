data = read.table(file = "EdeaBueadata.csv", h=T, sep=";", dec=",", na.strings = c(""))
summary(data)
names(data)

# conversion
convertion <- function(data_frame) {
  column_names <- names(data_frame)
  for (column in column_names) {
    data_frame[[column]] <- as.factor(data_frame[[column]])
  }
  return(data_frame)
}
data_frame = convertion(data_frame=data)
# Remove the first X in all columns
colnames(data_frame) <- gsub("^X", "", colnames(data_frame))
colnames(data_frame)
summary(data_frame)

# 1.Phenotypically response of insecticides

# Create 2 groupes based on the ID
Buea <- data_frame[grepl("^B", data_frame$ID), ]
Edea <- data_frame[grepl("^E", data_frame$ID), ]

data_frame$Group <- ifelse(grepl("^B", data_frame$ID), "Buea", "Edea")
data_frame$Group <- as.factor(data_frame$Group)
# create a dataframe for insecticides 1x
library("dplyr")
insectides_1x <- select(data_frame, ID, Group,"1x_permethrin_resistance.phenotypic.")
summary(insectides_1x)
insectides_1x <- na.omit(insectides_1x)
# Note: For the 1x_permethrin_resistance their is no Edea group
#proportion <- table(insectides_1x$ID, insectides_1x$"1x_permethrin_resistance.phenotypic.")
#proportion
proportion <- summary(insectides_1x$"1x_permethrin_resistance.phenotypic.")
proportion

eff_total <- sum(summary(insectides_1x$"1x_permethrin_resistance.phenotypic."))
freq <- (proportion * 100)/eff_total
freq


# -------------------------
# create a dataframe for insecticides 2x
#libary("dplyr")
insectides_5x <- select(data_frame, ID, Group,"5X_permethrin")
summary(insectides_5x)
insectides_5x <- na.omit(insectides_5x)
proportion <- summary(insectides_5x$"5X_permethrin")
proportion

eff_total <- sum(summary(insectides_5x$"5X_permethrin"))
freq <- (proportion * 100)/eff_total
freq

#----------------------------------------------------
freq_function <- function(data_frame) {
  columns <- colnames(data_frame)
  
  for (col in columns) {
    if (!col %in% columns[1:5]) {
      insecticides <- data_frame[, c("ID", "Group", col), drop = FALSE]
      insecticides <- na.omit(insecticides)
      
      # Calculate the proportions
      proportion <- summary(insecticides[[col]])
      eff_total <- sum(proportion)
      freq <- (proportion * 100) / eff_total
      
      # Print the results
      cat(col, " result:\n")
      print(summary(insecticides$Group))
      print(proportion)
      print(eff_total)
      print(freq)
    }
  }
}

freq_function(data_frame)


#---------------------------------
# Test statistic between dose and phenotype
# Fisher test
data <- matrix(c(52, 48,   # Buea, Pirimiphosmethyl 1X
                 86, 14,   # Buea, Pirimiphosmethyl 5X
                50, 50,
                 99, 1),  # Buea, Pirimiphosmethyl 10X
               nrow = 4, byrow = TRUE)
rownames(data) <- c("Deltamethtin 1X", "Deltamethtin 1X+PBO", "Deltamethtin 5X+PBO", "Deltamethtin 10X")
colnames(data) <- c("Sensitive (S)", "Resistant (R)")

# Print the table
data

# Perform Fisher's Exact Test
fisher_test <- fisher.test(data)
fisher_test


# Perform Chi-Square Test
chisq_test <- chisq.test(data)
chisq_test

#------------------------------------
# Test statistic between country and phenotype
# Test statistic
# Fisher test
data <- matrix(c(17, 87,   # Buea, Pirimiphosmethyl 5X
                 6, 94) # Edea, Pirimiphosmethyl 5X
               , nrow = 2, byrow = TRUE)
rownames(data) <- c("Permethrin 1X/Buea", "Permethrin 1X/Edea")
colnames(data) <- c("Sensitive (S)", "Resistant (R)")

# Print the table
data

# Perform Fisher's Exact Test
fisher_test <- fisher.test(data)
fisher_test


# Perform Chi-Square Test
chisq_test <- chisq.test(data)
chisq_test
# ---------------------------------------------------

# Create the dataset that combine all insecticide dose by  locality
data <- data.frame(
  Localities = rep(c("Buea", "Edea"), each=3),
  Insecticide = rep(c("Permethrin 1X", "Permethrin 5X", "Permethrin 10X"), each=2),
  Dose = rep(c("Buea", "Edea"), 6),
  Mortality = c(17.17, 6, 37.37, 89, 100, 100)
)
data
p_value <- data.frame(
  Insecticide = c("Permethrin 1X",  "Permethrin 5X", "Permethrin 10X"),
  p_value = c(0.025, 0.07, 1)
  #p_value = rep("p-value < 2.2e-16", 2)
)


data <- data.frame(
  Localities = c("Buea", "Edea"),
  Insecticide = rep(c("Permethrin 1X + PBO"), 2),
  Dose = c("Buea", "Edea"),
  Mortality = c(54, 31)
)
data
p_value <- data.frame(
  Insecticide = c("Permethrin 1X + PBO"),
  p_value = c("0.0016")
  #p_value = rep("p-value < 2.2e-16", 2)
)



# merging p-value into data
data <- merge(data, p_value, by="Insecticide")
data
# Load ggplot2 library
#library(ggplot2)
# Convert Dose to a factor with a specified level order
#data$Dose <- factor(data$Dose, levels = c("Buea", "Edea"))
#data$Insecticide <- factor(data$Insecticide, levels=c("Permethrin 1X", "Permethrin 5X", "Permethrin 10X"))
# Create the bar plot
library(ggplot2)
ggplot(data, aes(x = Insecticide, y = Mortality, fill = Dose)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  #geom_errorbar(aes(ymin = Mortality - 5, ymax = Mortality + 5), 
                #position = position_dodge(width = 0.7), width = 0.2) +
  geom_text(aes(label = paste(Mortality)), 
            position = position_dodge(width = 0.7), vjust = -1) +
  theme_minimal() +
  geom_text(aes(x = Insecticide, y = max(Mortality) + 5, label = paste("p-value= ", p_value)), 
            vjust = 3.6, hjust=-0.3, size = 4.5) +
  labs(y = "Mortality %", fill = "Locality") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12, face = "bold"),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ) +
  ggtitle("Mortality by Insecticide between Buea and Edea")



# cereate data_frame to get percentage of mortality of an insecticide in on locality
data <- data.frame(
  Localities = rep("Buea", 7),
  #Insecticide = rep(c("Pirimiphosmethyl", "Bendiocarb"), each = 2),
  Insecticide = c(
    rep(c("Alphacypermethrin"), 3),
    rep(c("Permethrin"), 4)),
  #Dose = rep(c("1x", "5x", "10x), 2),
  Dose = c("1X","1x + PBO", "5X", "1X","1x + PBO", "5X","10x"),
  Mortality = c(26, 0, 45, 17.17,54, 37.37, 100)
  #n = c(99, 99, 100, 100, 100, 200)
)
data

p_value <- data.frame(
  Insecticide = c("Alphacypermethrin", "Permethrin"),
  p_value = c("p-value < 2.2e-16")
)
# merging p-value into data
data <- merge(data, p_value, by="Insecticide")
data
# Load ggplot2 library
#library(ggplot2)
# Convert Dose to a factor with a specified level order
data$Dose <- factor(data$Dose, levels = c("1X","1x + PBO", "5X", "10x"))

# Create the bar plot
library(ggplot2)
ggplot(data, aes(x = Insecticide, y = Mortality, fill = Dose)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  #geom_errorbar(aes(ymin = Mortality - 5, ymax = Mortality + 5), 
  #position = position_dodge(width = 0.7), width = 0.2) +
  geom_text(aes(label = paste(Mortality)), 
            position = position_dodge(width = 0.7), vjust = -1) +
  theme_minimal() +
  geom_text(aes(x = Insecticide, y = max(Mortality) + 5, label = paste(p_value)), 
            vjust = 6, hjust=0.7 , size = 4.5) +
  labs(y = "Mortality %", fill = "Dose") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 12, face = "bold"),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ) +
  ggtitle("Mortality by Insecticide and Dose for Buea")
#---------------------------------------------------
#circular diagram
library(ggplot2)


data <- data.frame(
  #Species = c("Anopheles coluzzii", "Anopheles gambiae s.s"),
  location = c("Buea", "Edea"),
  #genotype = c("RS", "SS"),
  value = c(51, 70)
)
data
# Calculate percentages
data$percentage <- round(data$value / sum(data$value) * 100, 1)

# Create a pie chart with percentage labels
ggplot(data, aes(x = "", y = value, fill = location)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() + # Removes background, grid, and axes
  labs(fill = "location") +
  geom_text(aes(label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.5), size = 4) +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  ggtitle("Proportion of Anopheles coluzzii")


#------------------------------------------------
# mutation pair species
# Create the dataset that combine all insecticide dose by  locality
# Create the data frame
data <- data.frame(
  Species = c("Buae", "Edea"),
  Mutation = rep("RS", 2),
  Frequency = c(80, 83.93)
)

# Create the p-value data frame
p_value <- data.frame(
  Mutation = "RS",
  p_value = 0.62
)

# Merge p-value into data
data <- merge(data, p_value, by = "Mutation")
data
# Load ggplot2 library
library(ggplot2)

# Create the bar plot
ggplot(data, aes(x = Mutation, y = Frequency, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5) +
  geom_text(aes(label = paste(Frequency)), 
            position = position_dodge(width = 0.7), vjust = -1) +
  theme_minimal() +
  labs(y = "Frequency %", fill = "Location") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12, face = "bold"),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ) +
  ggtitle("Proportion of ACE_1 RS variant") +
  annotate("text", x = 1.5, y = max(data$Frequency) + 5, label = paste("p-value =", unique(data$p_value)), size = 4.5, vjust = 1, hjust=0.7)

#------------------------------------------------
# Create the dataset
data <- data.frame(
  Localities = rep("Buea", 6),
  Insecticide = rep(c("Pirimiphosmethyl", "Bendiocarb"), each = 3),
  #Dose = rep(c("1x", "5x", "10x"), 2),
  Dose = c("1X", "5X", "10X", "1X", "5X", "1X+PBO"),
  Mortality = c(17.17, 37.37, 100, 26, 45, 0),
  n = c(99, 99, 100, 100, 100, 200)
)
data


#--------------------------------------------------------------------
# 3. Proportion of Anopheles gambiae S.I characterized moleculary
anopheles_data <- select(data_frame, ID, Specie, Molecular_speciation, Group)
anopheles_data <- na.omit(anopheles_data)
summary(anopheles_data)
table(anopheles_data$Group, anopheles_data$Molecular_speciation)

# 4. Proportion of Kdr allele
kdr_data <- select(data_frame, ID, Specie, Molecular_speciation, KDR, Group)
kdr_data <- na.omit(kdr_data)
kdr_data
summary(kdr_data)
result <- table(kdr_data$Group, kdr_data$KDR)
print(result)
new_data <- matrix(c(63, 44,
                    17, 11), nrow=2, byrow = TRUE)
rownames(new_data) <- c("R", "S")
colnames(new_data) <- c("Buea", "Edea")
print(new_data)
round(prop.table(new_data,1) * 100, 2)
fisher.test(new_data)


grouped_data <- kdr_data %>%
  group_by(Group, Molecular_speciation, KDR) %>%
  summarise(count = n())
grouped_data

#Proportion of ACE_1
# by location
ACE_data <- select(data_frame, ID, Specie, Molecular_speciation, "ACE.1", Group)
ACE_data <- na.omit(ACE_data)
ACE_data
summary(ACE_data)
result <- table(ACE_data$Group, ACE_data$"ACE.1")
print(result)
# I have decided to combine RS and SR in just one since it the same
new_data <- matrix(c(40, 10,
                     47, 9), nrow=2, byrow = TRUE)
rownames(new_data) <- c("Buea", "Edea")
colnames(new_data) <- c("RS", "SS")
print(new_data)
round(prop.table(new_data,1) * 100, 2)
fisher.test(new_data)


grouped_data <- ACE_data %>%
  group_by(Group, Molecular_speciation, ACE.1) %>%
  summarise(count = n())
grouped_data

# by species

result <- table(ACE_data$Molecular_speciation, ACE_data$"ACE.1")
print(result)
# I have decided to combine RS and SR in just one since it the same
new_data <- matrix(c(51, 12,
                     36, 7), nrow=2, byrow = TRUE)
rownames(new_data) <- c("Anopheles coluzzii", "Anopheles gambiae s.s")
colnames(new_data) <- c("RS", "SS")
print(new_data)
round(prop.table(new_data,1) * 100, 2)
fisher.test(new_data)


result <- table(kdr_data$Molecular_speciation, kdr_data$KDR)
print(result)
# I have decided to combine RS and SR in just one since it the same
new_data <- matrix(c(76, 10,
                     31, 18), nrow=2, byrow = TRUE)
rownames(new_data) <- c("Anopheles coluzzii", "Anopheles gambiae s.s")
colnames(new_data) <- c("R", "S")
print(new_data)
round(prop.table(new_data,1) * 100, 2)
fisher.test(new_data)

#------------------------------------------------
#4. Correlation between mutation and phenotype
corr_data <- select(data_frame, ID, KDR, "1x_permethrin_resistance.phenotypic.")
corr_data <- na.omit(corr_data)
contingent_table <- table(corr_data$KDR, corr_data$"1x_permethrin_resistance.phenotypic.")
contingent_table
contingent_table <-contengent_table[-3,]
fisher.test(contingent_table)
chisq.test(contingent_table)
#libary(lsr)
cramersV(contingent_table)


corr_data <- select(data_frame, ID, "ACE.1", "1x_permethrin_resistance.phenotypic.")
corr_data <- na.omit(corr_data)
contingent_table <- table(corr_data$"ACE.1", corr_data$"1x_permethrin_resistance.phenotypic.")
contingent_table
#contingent_table <-contengent_table[-3,]
fisher.test(contingent_table)
chisq.test(contingent_table)
#libary(lsr)
cramersV(contingent_table)
#----------------------------------------------------
#prevalence of Kdr, ACE_1 mutation in Anopheles population



# old code
# Barplot(i should use pourcentage to get all good plot)
# Sample data
data <- data.frame(
  Localities = c(rep("Buea", 3), rep("Edea", 3), 
                 rep("Buea", 3), rep("Edeaa", 3)),
  Insecticide = rep(c("Permethrin", "Alphacypermethrin"), each = 6),
  Dose = rep(c("1x", "5x", "1x+PBO"), 4),
  Mortality = c(17, 37, 100, 6, 86, 100, 
                26, 45, 0, 20, 37, 69),
  n = c(99, 99, 201, 100, 100, 100,
        100, 100, 201, 100, 100, 108)
)
data
library(ggplot2)
ggplot(data, aes(x = Localities, y = Mortality, fill = Dose)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  facet_wrap(~ Insecticide) +
  geom_errorbar(aes(ymin = Mortality - 5, ymax = Mortality + 5), 
                position = position_dodge(width = 0.7), width = 0.2) +
  geom_text(aes(label = paste("n=", n)), 
            position = position_dodge(width = 0.7), vjust = -1) +
  theme_minimal() +
  labs(y = "Mortality %", fill = "Dose") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )
