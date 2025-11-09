# Installation des packages 
# install.packages(c("infectiousR", "survival", "survminer", "gganimate", "gifski", "dplyr", "ggplot2", "broom"))

# Packages et librairies
library(infectiousR)
library(survival)
library(survminer)
library(gganimate)
library(gifski)
library(dplyr)
library(ggplot2)
library(broom)
library(lubridate)

# Chargement des données
data(smallpox_nigeria_df)

# Verification de la structure des données
smallpox_nigeria_df
smallpox_nigeria_df <- smallpox_nigeria_df %>%
  mutate(day_of_year = yday(date_of_onset))  
  
# Données pour suivi de la vaccination
vacc_obj <- survfit(Surv(day_of_year, vaccinated == "y") ~ gender, data = smallpox_nigeria_df)

# Préparation des données pour l'animation
vacc_data <- vacc_obj %>%
  broom::tidy() %>%
  mutate( gender = case_when(strata == "gender=f" ~ "Femme",
                             strata == "gender=m" ~ "Homme",
                             TRUE ~ as.character(strata)))

# Création du graphique animé : 
animated_plot <- vacc_data %>%
  ggplot(aes(x = time, y = estimate, color = gender)) +  geom_step(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = gender), 
              alpha = 0.15, color = NA) +
  scale_color_manual(values = c("#E7B800", "#2E9FDF")) +
  scale_fill_manual(values = c("#E7B800", "#2E9FDF")) +
  theme_minimal() +  theme(plot.title = element_text(size = 16, hjust = 0.5),
                           legend.position = "bottom") +
  
  labs(title = "Évolution de la vaccination en focntion du genre - Année 1967",
       x = "Jour", y = "Probabilité de vaccination", color = "Sexe") +
  
  xlim(90, max(vacc_data$time, na.rm = TRUE)) +ylim(0, 1) +
  transition_reveal(time) + ease_aes('linear')

# Visualisation : 
animated_plot

#ANIMATION ET SAUVEGARDE 

# Animation du graphique
anim <- animate(animated_plot,
                fps = 10,
                duration = 8,
                width = 800,
                height = 600,
                renderer = gifski_renderer("vaccination_animation.gif"))

# Affichage de l'animation
anim







