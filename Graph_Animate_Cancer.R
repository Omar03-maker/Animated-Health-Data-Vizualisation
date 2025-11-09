# PACKAGES ET LIBRAIRIES A INSTALLER : 
library(survival)
library(survminer)
library(gifski)
library(dplyr)
library(ggplot2)
library(gganimate)

# CHARGEMENT DES DONNEES : 
data(cancer)
head(cancer)
cancer_obj <- survfit(Surv(time, status == "1") ~ sex, data = cancer)

# PREPARATION DES DONNEES POUR ANIMATION 
cancer_data <- cancer_obj %>%
  broom::tidy() %>%
  mutate( sex = case_when(strata == "sex=1" ~ "Femme",
                          strata == "sex=2" ~ "Homme",
                          TRUE ~ as.character(strata)))

# CREATION GRAPH ANIME: 
# Base du graph 
animated_plot_cancer <- cancer_data %>%
  ggplot(aes(x = time, y = estimate, color = sex)) +  geom_step(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = sex), 
              alpha = 0.15, color = NA) +
# Couleur :    
  scale_color_manual(values = c("#E7B800", "#2E9FDF")) +
  scale_fill_manual(values = c("#E7B800", "#2E9FDF")) +
# Apparence du graph :    
  theme_minimal() +  theme(plot.title = element_text(hjust = 0.5),
                           legend.position = "bottom") +
# Titre et labels :   
  labs(title = "Évolution du cancer en fonction du genre",
       x = "Jour", y = "Probabilité d'apparation de cancer", color = "Sexe") +
# Limites et animation   
  xlim(90, max(cancer_data$time, na.rm = TRUE)) +ylim(0, 1) +
  transition_reveal(time) + ease_aes('linear')

# VISUALISATION : 
animated_plot_cancer

