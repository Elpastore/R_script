data <- read.table("evolution_poids.csv", h=T, sep=";", dec=',')
summary(data)

#convert data to the right format
data$Numeros_poussins <- as.factor(data$Numeros_poussins)
data$Traitements <- as.factor(data$Traitements)
data$Poids <- as.numeric(data$Poids)
data$Dates <- as.Date(data$Dates,format = "%d/%m/%Y")


#load library
library(ggplot2)
library(dplyr)


# Filtering data
T1 <- data %>% filter(Traitements == "T1")
T2 <- data %>% filter(Traitements == "T2")
T3 <- data %>% filter(Traitements == "T3")

# combine Traitements
combined_data <- bind_rows(T1, T2, T3)


combined_grouped <- combined_data %>%
  group_by(Dates, Traitements) %>%
  summarise(Poids_mean = mean(Poids, na.rm = TRUE))


# Plot evolution
ggplot(combined_grouped, aes(x = Dates, y = Poids_mean, color = Traitements)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Evolution du poids en fonction du traitement",
       x = "Dates", y = "Moyenne Poids") +
  theme_minimal() +                  
  scale_color_manual(values = c("blue", "red", "green")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16)) 


# Get  the final poids
final_poids <- combined_grouped %>% filter(Dates == "2024-06-22")

ggplot(final_poids, aes(x = Traitements, y = Poids_mean, fill = Traitements)) +
  geom_col() +                        
  labs(title = "Poids final en fonction du traitement",
       x = "Traitements", y = "Moyenne Poids") +
  theme_minimal() +                  
  scale_fill_manual(values = c("blue", "red", "green")) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16))
