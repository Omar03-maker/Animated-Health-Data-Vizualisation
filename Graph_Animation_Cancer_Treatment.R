# Chargement des librairies
library(survival)
library(survminer)
library(gganimate)
library(gifski)
library(dplyr)
library(ggplot2)

# Création d'un jeu de données d'exemple 
# Étude de survie : Traitement du cancer du poumon
set.seed(123)  # Pour la reproductibilité

n <- 200
cancer_data <- data.frame(patient_id = 1:n,
  # Temps de survie en jours (0 à 1000 jours)
  time = round(rexp(n, rate = 0.003) + runif(n, 10, 50)),
  # Statut : 1 = censuré (vivant), 2 = décédé
  status = sample(c(1, 2), n, replace = TRUE, prob = c(0.3, 0.7)),
  # Traitement : A = Traitement standard, B = Nouveau traitement
  treatment = sample(c("Traitement A", "Traitement B"), n, replace = TRUE),
  # Âge des patients
  age = round(rnorm(n, mean = 65, sd = 12)),
  # Sexe
  sex = sample(c("Homme", "Femme"), n, replace = TRUE))

# Ajustement des temps pour avoir une distribution réaliste
cancer_data$time <- pmin(cancer_data$time, 1000)  # Maximum 1000 jours
cancer_data$time <- pmax(cancer_data$time, 1)     # Minimum 1 jour

# Vérification des données
head(cancer_data)
summary(cancer_data)

# Création de l'objet de survie
surv_obj <- survfit(Surv(time, status == 2) ~ treatment, data = cancer_data)

# Animation avec ggsurvplot et gganimate
# Création du graphique de base
base_plot <- ggsurvplot(surv_obj,
                        data = cancer_data,
                        conf.int = TRUE,
                        pval = TRUE,
                        risk.table = FALSE, # Désactivé pour l'animation
                        surv.median.line = "hv",
                        palette = c("#E7B800", "#2E9FDF"),
                        ggtheme = theme_minimal(),
                        title = "Survie selon le traitement - Cancer du poumon",
                        xlab = "Temps (jours)",
                        ylab = "Probabilité de survie",
                        legend.title = "Traitement",
                        legend.labs = c("Traitement A", "Traitement B"))
base_plot

# Préparation des données pour l'animation
surv_data <- surv_obj %>%
  broom::tidy() %>%
  mutate(treatment = case_when(strata == "treatment=Traitement A" ~ "Traitement A",
                               strata == "treatment=Traitement B" ~ "Traitement B",
                               TRUE ~ as.character(strata)))

# Création du graphique animé
animated_plot <- surv_data %>%
  ggplot(aes(x = time, y = estimate, color = treatment)) +
  geom_step(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = treatment), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values = c("#E7B800", "#2E9FDF")) +
  scale_fill_manual(values = c("#E7B800", "#2E9FDF")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  labs(title = "Courbe de survie cumulative - Jour: {closest_state}",
       subtitle = "Étude comparative de traitements contre le cancer du poumon",
       x = "Temps (jours)",
       y = "Probabilité de survie",
       color = "Traitement",
       fill = "Traitement",
       caption = "Animation: Évolution de la survie dans le temps") +
  xlim(0, max(surv_data$time)) +
  ylim(0, 1) +
  transition_reveal(time) +
  ease_aes('linear')
animated_plot

# Animation du graphique
anim <- animate(animated_plot ,
                fps = 15,
                duration = 10,
                width = 1000,
                height = 700,
                renderer = gifski_renderer("courbe_survie_animee.gif"))

# Affichage de l'animation
anim






