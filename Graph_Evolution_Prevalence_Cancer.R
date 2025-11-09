# PACKAGES NÉCESSAIRES
library(ggplot2)
library(gganimate)
library(dplyr)

# DONNÉES SIMPLIFIÉES - TOP 3 par année
data <- data.frame(annee = rep(2010:2025, each = 3),
                   pays = rep(c("USA", "Pays-Bas", "Belgique"),16),
                   taux = c(
    # 2010-2013: quelques variations
    372.2, 361.9, 342.3,  # 2010
    375.8, 364.7, 340.1,  # 2011
    379.4, 367.5, 358.2,  # 2012
    383.0, 370.8, 370.3,  # 2013
    # 2014-2025: progression constante
    386.6, 374.1, 373.9,  # 2014
    390.2, 377.4, 376.8,  # 2015
    393.8, 380.7, 379.4,  # 2016
    397.4, 384.0, 382.6,  # 2017
    401.0, 387.3, 385.8,  # 2018
    404.6, 390.6, 389.0,  # 2019
    408.2, 393.9, 392.2,  # 2020
    411.8, 397.2, 395.4,  # 2021
    415.4, 400.5, 398.6,  # 2022
    419.0, 403.8, 401.8,  # 2023
    422.6, 407.1, 405.0,  # 2024
    426.2, 410.4, 408.2   # 2025
    ))

# Ajouter les rangs
data <- data %>%
  group_by(annee) %>%
  arrange(desc(taux)) %>%
  mutate(rang = row_number()) %>%
  ungroup()

# COULEURS
colors <- c("USA" = "#4285f4", "Pays-Bas" = "#ea4335", "Belgique" = "#fbbc04")

# GRAPHIQUE ANIMÉ
plot <- data %>%
  ggplot(aes(x = reorder(pays, -taux), y = taux, fill = pays)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = round(taux, 1)), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "TOP 3 - Taux de Prévalence du Cancer",
       subtitle = "Année: {closest_state}",
       y = "Taux (pour 100 000 habitants)") +
  ylim(0, max(data$taux) * 1.1) +
  transition_states(annee) +
  ease_aes("cubic-in-out")

# AFFICHAGE DANS LA FENÊTRE GRAPHIQUE
plot


