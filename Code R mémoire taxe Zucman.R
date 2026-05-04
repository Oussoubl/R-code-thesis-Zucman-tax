library(tidyverse)
library(readxl)
library(tidyverse)
library(gt)
library(WDI)
library(ggplot2)
library(here)
library(hereR)

## Nettoyage et fusion des fichiers + résultats ##

# 1. Je charge les fichiers

top01_raw <- read_delim("~/Documents/WID_Data_03022026-101159.csv", delim = ";", skip = 1)
top1_raw <- read_delim("~/Documents/WID_Data_03022026-100658.csv", delim = ";", skip = 1)

# 2. Je renomme les colonnes pour simplifier (je garde l'ordre : France, USA, China, UK)
# J'utilise les noms de colonnes simplifiés

colnames(top1_raw) <- c("Percentile", "Year", "France", "USA", "China", "UK")
colnames(top01_raw) <- c("Percentile", "Year", "France", "USA", "China", "UK")

# 3. Je transforme le format pour qu'il soit "propre" (Format Long) pour que les pays deviennent une colonne

clean_top1 <- top1_raw %>%
  pivot_longer(cols = c(France, USA, China, UK), names_to = "Country", values_to = "Wealth_Share") %>%
  mutate(Type = "Top 1%")

clean_top01 <- top01_raw %>%
  pivot_longer(cols = c(France, USA, China, UK), names_to = "Country", values_to = "Wealth_Share") %>%
  mutate(Type = "Top 0.1%")

# 4. Je fusionne les deux

data_final <- bind_rows(clean_top1, clean_top01)

# Résultat

view(data_final)

ggplot(data_final, aes(x = Year, y = Wealth_Share, color = Country, linetype = Type)) +
  geom_line(size = 1) +
  theme_dark() +
  labs(title = "Évolution de la concentration des richesses (2004-2022)",
       subtitle = "Comparaison Top 1% vs Top 0.1%",
       y = "Part du patrimoine national",
       x = "Année")



# 1. Je télécharge la FBCF (% du PIB) pour nos 4 pays

fbcf_data <- WDI(
  country = c("FR", "US", "CN", "GB"), 
  indicator = "NE.GDI.FTOT.ZS",
  start = 2004, 
  end = 2022
)


# 2. Je nettoie les noms pour qu'ils matchent avec ma data_final

fbcf_clean <- fbcf_data %>%
  rename(Year = year, FBCF = NE.GDI.FTOT.ZS) %>%
  mutate(Country = case_when(
    iso2c == "FR" ~ "France",
    iso2c == "US" ~ "USA",
    iso2c == "CN" ~ "China",
    iso2c == "GB" ~ "UK"
  )) %>%
  select(Year, Country, FBCF)

# 3. Je fusionne avec les données de la WID
# On se concentre sur le Top 0.1% pour cette analyse

data_complet <- data_final %>%
  filter(Type == "Top 0.1%") %>%
  left_join(fbcf_clean, by = c("Year", "Country"))

# Voir mon nouveau tableau

view(data_complet)

ggplot(data_complet, aes(x = Wealth_Share, y = FBCF, color = Country)) +
  geom_point(size = 3, alpha = 0.7) + # Les points par année
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + # La droite de tendance
  theme_dark() +
  labs(
    title = "Corrélation : Richesse du Top 0.1% vs Investissement (FBCF)",
    subtitle = "Analyse sur la période 2004-2022",
    x = "Part du patrimoine détenue par le Top 0.1% (WID)",
    y = "Investissement - FBCF en % du PIB (Banque Mondiale)",
    color = "Pays"
  ) +
  facet_wrap(~Country, scales = "free") # Un graphique par pays pour mieux voir

## On regarde les coefficients de corrélation

data_complet %>%
  group_by(Country) %>%
  summarise(correlation = cor(Wealth_Share, FBCF, use = "complete.obs"))


# Préparation des données pour le tableau
tableau_stats <- data_final %>%
  filter(Year %in% c(2004, 2021)) %>%
  pivot_wider(names_from = Year, names_prefix = "Year_", values_from = Wealth_Share) %>%
  mutate(Evolution = Year_2021 - Year_2004) %>%
  arrange(desc(Type), desc(Year_2021))

# Création du tableau stylisé

tableau_stats %>%
  gt(groupname_col = "Type") %>%
  tab_header(
    title = "Concentration du patrimoine par pays (2004-2021)",
    subtitle = "Part de la richesse nationale détenue par les percentiles supérieurs"
  ) %>%
  fmt_percent(columns = c(Year_2004, Year_2021, Evolution), decimals = 1) %>%
  cols_label(
    Country = "Pays",
    Year_2004 = "Part en 2004",
    Year_2021 = "Part en 2021",
    Evolution = "Variation (pts)"
  ) %>%
  tab_source_note(source_note = "Source : World Inequality Database (WID). Calculs de l'auteur.")

# 1. Je télécharge les données de Brevets (Demandes de résidents)
# Indicateur : IP.PAT.RESD (Banque Mondiale)
innovation_data <- WDI(
  country = c("FR", "US", "CN", "GB"), 
  indicator = "IP.PAT.RESD",
  start = 2004, 
  end = 2022
)

# 2. Nettoyage des données d'innovation
innovation_clean <- innovation_data %>%
  rename(Year = year, Patents = IP.PAT.RESD) %>%
  mutate(Country = case_when(
    iso2c == "FR" ~ "France",
    iso2c == "US" ~ "USA",
    iso2c == "CN" ~ "China",
    iso2c == "GB" ~ "UK"
  )) %>%
  select(Year, Country, Patents)

# 3. Fusion avec la WID (Top 0.1%)
data_innovation_final <- data_final %>%
  filter(Type == "Top 0.1%") %>%
  left_join(innovation_clean, by = c("Year", "Country"))

# 4. Graphique de corrélation
ggplot(data_innovation_final, aes(x = Wealth_Share, y = Patents, color = Country)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  theme_dark() +
  facet_wrap(~Country, scales = "free") + # Important car les échelles varient énormément entre US/Chine et Europe
  labs(
    title = "Corrélation : Richesse du Top 0.1% vs Innovation (Brevets)",
    subtitle = "Analyse Aghion vs Zucman (2004-2022)",
    x = "Part du patrimoine du Top 0.1%",
    y = "Nombre de brevets déposés (Résidents)",
    color = "Pays"
  )

# 5. Calcul des coefficients de corrélation (r)
data_innovation_final %>%
  group_by(Country) %>%
  summarise(correlation_innovation = cor(Wealth_Share, Patents, use = "complete.obs"))

## Maintenant, analyse avec dépenses R&D ##

# 1. Je télécharge les données de R&D (% du PIB)
# Indicateur : GB.XPD.RSDV.GD.ZS
rd_data <- WDI(
  country = c("FR", "US", "CN", "GB"), 
  indicator = "GB.XPD.RSDV.GD.ZS",
  start = 2004, 
  end = 2022
)

# 2. Nettoyage de la base de données
rd_clean <- rd_data %>%
  rename(Year = year, RD_Percent = GB.XPD.RSDV.GD.ZS) %>%
  mutate(Country = case_when(
    iso2c == "FR" ~ "France",
    iso2c == "US" ~ "USA",
    iso2c == "CN" ~ "China",
    iso2c == "GB" ~ "UK"
  )) %>%
  select(Year, Country, RD_Percent)

# 3. Fusion avec la WID (Top 0.1%)
data_rd_final <- data_final %>%
  filter(Type == "Top 0.1%") %>%
  left_join(rd_clean, by = c("Year", "Country"))

# 4. Graphique de corrélation
ggplot(data_rd_final, aes(x = Wealth_Share, y = RD_Percent, color = Country)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  theme_dark() +
  facet_wrap(~Country, scales = "free") +
  labs(
    title = "Corrélation : Richesse du Top 0.1% vs Effort de R&D",
    subtitle = "Dépenses de Recherche & Développement en % du PIB (2004-2022)",
    x = "Part du patrimoine du Top 0.1%",
    y = "Dépenses R&D (% du PIB)",
    color = "Pays"
  )

# 5. Calcul des coefficients
data_rd_final %>%
  group_by(Country) %>%
  summarise(correlation_RD = cor(Wealth_Share, RD_Percent, use = "complete.obs"))

## Analyse de syntèse globale ## 

# 1. Je fusionne toutes les données de la Banque Mondiale
world_bank_all <- fbcf_clean %>%
  inner_join(innovation_clean, by = c("Year", "Country")) %>%
  inner_join(rd_clean, by = c("Year", "Country"))

# 2. Je fusionne avec la richesse du Top 0.1%
data_synthese <- data_final %>%
  filter(Type == "Top 0.1%") %>%
  inner_join(world_bank_all, by = c("Year", "Country")) %>%
  # On passe au format long pour faciliter le facettage par indicateur
  pivot_longer(cols = c(FBCF, Patents, RD_Percent), 
               names_to = "Indicateur", 
               values_to = "Valeur")

# 3. Graphique de corrélation multi-indicateurs
ggplot(data_synthese, aes(x = Wealth_Share, y = Valeur, color = Country)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(Indicateur ~ Country, scales = "free") + # Lignes = Indicateurs, Colonnes = Pays
  theme_dark() +
  labs(
    title = "Synthèse Macroéconomique : Richesse vs Indicateurs de Performance",
    subtitle = "Analyse croisée de l'investissement, de l'innovation et de l'effort de R&D",
    x = "Part du patrimoine du Top 0.1% (%)",
    y = "Valeur de l'indicateur (Échelles libres)"
  ) +
  theme(strip.text = element_text(size = 8, face = "bold"))

data_synthese %>%
  group_by(Country) %>%
  summarise(correlation_synthese = cor(Wealth_Share, Valeur, use = "complete.obs"))


## Nouvelle base de données pour analyse les recettes de la taxe et financement de la R&D ##

Net_National_Wealth <- read_delim("WID_Data_02032026-082148.csv", delim = ";", skip = 1)
View(Net_National_Wealth)

# 1. Je renomme par POSITION pour éviter l'erreur sur les noms complexes
# L'ordre dans mon fichier est : Percentile, Year, France, China, USA, UK
colnames(Net_National_Wealth) <- c("Percentile", "Year", "France", "China", "USA", "UK")

# 2. Je transforme en format Long
Net_National_Wealth_Clean <- Net_National_Wealth %>%
  select(-Percentile) %>% # J'enlève la colonne 'pall' qui ne nous sert pas ici
  pivot_longer(cols = c(France, China, USA, UK), 
               names_to = "Country", 
               values_to = "Total_Wealth")

# 3. Vérification rapide
head(Net_National_Wealth_Clean)

ref_data <- WDI(
  country = c("FR", "US", "CN", "GB"), 
  indicator = c("NY.GDP.MKTP.PP.KD", "GB.XPD.RSDV.GD.ZS"), 
  start = 2004, end = 2022
)

ref_clean <- ref_data %>%
  rename(Year = year, GDP = NY.GDP.MKTP.PP.KD, RD_Percent_PIB = GB.XPD.RSDV.GD.ZS) %>%
  mutate(Country = case_when(
    iso2c == "FR" ~ "France",
    iso2c == "US" ~ "USA",
    iso2c == "CN" ~ "China",
    iso2c == "GB" ~ "UK"
  )) %>%
  select(Year, Country, GDP, RD_Percent_PIB)

# 4. FUSION ET CALCUL DE LA SIMULATION
# On part de data_final (qui contient Wealth_Share pour le Top 0.1%)
data_impact_final <- data_final %>%
  filter(str_detect(Type, "0.1%")) %>%
  # On harmonise le nom de la colonne de part de richesse
  rename(Wealth_Share = matches("Wealth_Share|value|share")) %>%
  # Je fusionne avec la Richesse Totale, le PIB et la R&D
  left_join(Net_National_Wealth_Clean, by = c("Year", "Country")) %>%
  left_join(ref_clean, by = c("Year", "Country")) %>%
  filter(!is.na(Total_Wealth), !is.na(GDP))

# 5. CALCUL DES RATIOS RÉELS 
data_impact_final <- data_impact_final %>%
  mutate(
    # Recettes de la taxe (2% du stock de richesse du Top 0.1%)
    Taxe_Mrd = (Total_Wealth * (Wealth_Share / 100) * 0.02) / 1e9,
    
    # VRAI Budget R&D national (PIB * % de R&D)
    RD_Reelle_Mrd = (GDP * (RD_Percent_PIB / 100)) / 1e9,
    
    # RATIO D'IMPACT : Quelle part de la R&D nationale est couverte ?
    Ratio_Couverture = (Taxe_Mrd / RD_Reelle_Mrd) * 100
  )

data_impact_final <- data_impact_final %>%
  mutate(
    # 1. RECETTES DE LA TAXE
    # Wealth_Share (0.127) est déjà un ratio, on ne divise PAS par 100.
    # Total_Wealth est en unités simples (1.55e13 = 15 500 Milliards)
    Recettes_Taxe_Mrd = (Total_Wealth * Wealth_Share * 0.02) / 1e9,
    
    # 2. BUDGET R&D RÉEL
    # RD_Percent_PIB (2.22) est un pourcentage, on DOIT diviser par 100.
    # GDP est en unités simples (3.56e12 = 3 560 Milliards)
    RD_Reelle_Mrd = (GDP * (RD_Percent_PIB / 100)) / 1e9,
    
    # 3. LE RATIO D'IMPACT (chiffre qui mesurera le choc)
    Ratio_Impact_Final = (Recettes_Taxe_Mrd / RD_Reelle_Mrd) * 100
  )

# 6. Affichage du résultat pour vérification
data_impact_final %>%
  filter(Year == 2021) %>%
  select(Country, Recettes_Taxe_Mrd, RD_Reelle_Mrd, Ratio_Impact_Final)


tableau_final <- data_impact_final %>%
  filter(Year == 2021) %>%
  select(Country, Recettes_Taxe_Mrd, RD_Reelle_Mrd, Ratio_Impact_Final) %>%
  arrange(desc(Ratio_Impact_Final))

# 7. Mise en forme académique
tableau_final %>%
  gt() %>%
  tab_header(
    title = md("**Résultats de la Simulation : Potentiel de Financement de l'Innovation**"),
    subtitle = "Comparaison entre une taxe de 2% (Top 0.1%) et l'effort national de R&D (2021)"
  ) %>%
  fmt_number(
    columns = c(Recettes_Taxe_Mrd, RD_Reelle_Mrd),
    decimals = 1,
    suffixing = " Mrd $"
  ) %>%
  fmt_number(
    columns = Ratio_Impact_Final,
    decimals = 1,
    suffixing = " %"
  ) %>%
  cols_label(
    Country = "Pays",
    Recettes_Taxe_Mrd = "Recettes Potentielles",
    RD_Reelle_Mrd = "Budget R&D Actuel",
    Ratio_Impact_Final = "Capacité de Couverture"
  ) %>%
  # On met en évidence la colonne "choc"
  tab_style(
    style = cell_text(weight = "bold", color = "green"),
    locations = cells_body(columns = Ratio_Impact_Final)
  ) %>%
  tab_source_note(
    source_note = "Données : WID (Richesse) et Banque Mondiale (PIB & R&D). Calculs basés sur une taxe de 2% du patrimoine net du Top 0.1%."
  )

plot_data <- data_impact_final %>%
  filter(Year == 2021) %>%
  # On s'assure que les pays sont triés par ratio pour un meilleur rendu visuel
  mutate(Country = reorder(Country, Ratio_Impact_Final))

# 8. Création du graphique
ggplot(plot_data, aes(x = Country, y = Ratio_Impact_Final, fill = Country)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  # J'ajoute une ligne de référence à 100% (seuil de financement total)
  geom_hline(yintercept = 100, linetype = "dashed", color = "red", size = 0.8) +
  # J'ajoute des étiquettes de valeur au-dessus des barres
  geom_text(aes(label = paste0(round(Ratio_Impact_Final, 1), "%")), 
            hjust = -0.2, size = 5, fontface = "bold") +
  coord_flip() + # Je bascule en barres horizontales pour plus de lisibilité
  scale_fill_brewer(palette = "Set1") +
  theme_dark(base_size = 14) +
  labs(
    title = "Capacité de financement de la R&D par la Taxe Zucman",
    subtitle = "Part du budget R&D national couverte par une taxe de 2% sur le Top 0.1% (2021)",
    x = "",
    y = "Pourcentage du budget R&D couvert (%)",
    caption = "Sources : WID & Banque Mondiale. Note : La ligne rouge indique un financement à 100%."
  ) +
  # Ajustement des marges pour ne pas couper les étiquettes
  expand_limits(y = c(0, 480))

# 1. Définition du multiplicateur (Hypothèse prudente : +1 point de R&D/PIB = +0.8 point de croissance)
multiplicateur_croissance <- 0.8

data_croissance <- data_impact_final %>%
  filter(Year == 2021) %>%
  mutate(
    # Nouveau ratio R&D si on injecte TOUTE la taxe dans la recherche
    Nouveau_RD_Percent = RD_Percent_PIB + (Taxe_Mrd / (GDP / 1e9) * 100),
    
    # Gain de points de pourcentage de R&D
    Gain_Points_RD = Nouveau_RD_Percent - RD_Percent_PIB,
    
    # Impact estimé sur le taux de croissance annuel du PIB (en points)
    Impact_Croissance_Points = Gain_Points_RD * multiplicateur_croissance
  )

# 2. Tableau de l'impact sur la croissance
data_croissance %>%
  select(Country, RD_Percent_PIB, Nouveau_RD_Percent, Impact_Croissance_Points) %>%
  arrange(desc(Impact_Croissance_Points))

#3 Tableau de résultats

tableau_croissance_data <- data_impact_final %>%
  filter(Year == 2021) %>%
  mutate(
    Gain_Points_RD = (Recettes_Taxe_Mrd / (GDP / 1e9)) * 100,
    Nouveau_RD_Percent = RD_Percent_PIB + Gain_Points_RD,
    Impact_PIB_Points = Gain_Points_RD * 0.8
  ) %>%
  select(Country, RD_Percent_PIB, Nouveau_RD_Percent, Impact_PIB_Points) %>%
  arrange(desc(Impact_PIB_Points))

# 4. Création du tableau final
tableau_croissance_data %>%
  gt() %>%
  tab_header(
    title = md("**Impact Macrocéconomique du Choc d'Innovation**"),
    subtitle = "Projection de la croissance du PIB suite au réinvestissement de la taxe"
  ) %>%
  fmt_number(
    columns = c(RD_Percent_PIB, Nouveau_RD_Percent),
    decimals = 2,
    suffixing = " %"
  ) %>%
  fmt_number(
    columns = Impact_PIB_Points,
    decimals = 2,
    suffixing = " pts"
  ) %>%
  cols_label(
    Country = "Pays",
    RD_Percent_PIB = "Ratio R&D Actuel",
    Nouveau_RD_Percent = "Ratio R&D Projeté",
    Impact_PIB_Points = "Gain de Croissance (an)"
  ) %>%
  # Mise en évidence du gain de croissance
  tab_style(
    style = cell_text(weight = "bold", color = "#2E86C1"),
    locations = cells_body(columns = Impact_PIB_Points)
  ) %>%
  tab_source_note(
    source_note = "Note : Le gain de croissance est calculé avec un multiplicateur d'élasticité de la R&D de 0.8."
  )

resultats_croissance <- data.frame(
  Country = c("China", "USA", "France", "UK"),
  RD_Actuel = c(2.43, 3.48, 2.22, 2.90),
  RD_Projete = c(12.91, 5.78, 3.32, 3.38),
  Gain_Croissance = c(8.39, 1.84, 0.88, 0.39)
)

# 5. Création du tableau GT
resultats_croissance %>%
  gt() %>%
  tab_header(
    title = md("**Impact Macroéconomique de la Réallocation du Capital**"),
    subtitle = "Effet d'un investissement de la Taxe Zucman (2%) dans la R&D nationale"
  ) %>%
  fmt_number(columns = everything(), decimals = 2) %>%
  cols_label(
    Country = "Pays",
    RD_Actuel = "Ratio R&D Actuel (% PIB)",
    RD_Projete = "Ratio R&D Projeté (% PIB)",
    Gain_Croissance = "Gain de Croissance (pts/an)"
  ) %>%
  # Coloration pour le gain de croissance
  data_color(
    columns = Gain_Croissance,
    colors = scales::col_numeric(
      palette = c("white", "orange", "red"),
      domain = c(0, 9)
    )
  ) %>%
  tab_source_note(
    source_note = "Simulation basée sur un multiplicateur d'élasticité R&D/Croissance de 0.8."
  )

plot_growth <- resultats_croissance %>%
  pivot_longer(cols = c(RD_Actuel, RD_Projete), 
               names_to = "Situation", 
               values_to = "Ratio_RD") %>%
  mutate(Situation = factor(Situation, 
                            levels = c("RD_Actuel", "RD_Projete"),
                            labels = c("Actuel", "Après Taxe Zucman")))

# 6. Création du graphique
ggplot(plot_growth, aes(x = Country, y = Ratio_RD, fill = Situation)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  # J'ajoute des points de gain de croissance en texte au-dessus des groupes
  geom_text(data = resultats_croissance, 
            aes(x = Country, y = RD_Projete + 0.5, 
                label = paste0("+", Gain_Croissance, " pts PIB")), 
            inherit.aes = FALSE, size = 4, fontface = "bold", color = "darkred") +
  scale_fill_manual(values = c("Actuel" = "#AEB6BF", "Après Taxe Zucman" = "#2E86C1")) +
  theme_minimal() +
  labs(
    title = "Transformation du Modèle d'Innovation et Gain de Croissance",
    subtitle = "Comparaison du ratio R&D/PIB et impact estimé sur la croissance annuelle",
    x = "Pays",
    y = "Ratio R&D / PIB (%)",
    fill = "Scénario"
  ) +
  theme(legend.position = "bottom")

## Scénario pessimiste mise en place Taxe Zucman ##

# 1. Je prépare les données à partir de mes résultats réels

simul_impact_net <- data_impact_final %>%
  filter(Year == 2021) %>%
  mutate(
    # 1. Le Gain : Les recettes de la taxe (ce que l'État récupère)
    Gain_Public = Taxe_Mrd,
    
    # 2. La Perte théorique (Scénario pessimiste) : 
    # On imagine que pour 1€ de taxe payée, le riche réduit son investissement de 0.20€ 
    # (Ratio d'éviction classique en économie, rarement au-dessus de 0.30)
    Perte_Eviction = Taxe_Mrd * 0.20,
    
    # 3. Le Bilan Net
    Bilan_Innovation = (Gain_Public * 1.2) - Perte_Eviction
  )

# 2. Visualisation : Le Gain vs La Perte

ggplot(simul_impact_net, aes(x = Country)) +
  geom_bar(aes(y = Gain_Public, fill = "Gain (Financement R&D)"), stat = "identity") +
  geom_bar(aes(y = -Perte_Eviction, fill = "Perte (Éviction Privée)"), stat = "identity") +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() +
  labs(
    title = "Pourquoi l'innovation ne chutera pas",
    subtitle = "La taxe capte une rente pour financer un investissement productif",
    y = "Milliards de Dollars ($)",
    fill = "Effet Net"
  ) +
  scale_fill_manual(values = c("Gain (Financement R&D)" = "#27AE60", "Perte (Éviction Privée)" = "#C0392B"))

# Je filtre uniquement sur l'Europe pour voir les détails
data_europe <- simul_impact_net %>% 
  filter(Country %in% c("France", "UK"))

ggplot(data_europe, aes(x = Country)) +
  geom_bar(aes(y = Gain_Public, fill = "Gain (Financement R&D)"), stat = "identity") +
  geom_bar(aes(y = -Perte_Eviction, fill = "Perte (Éviction Privée)"), stat = "identity") +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() +
  labs(
    title = "Focus Europe : Bilan Net de la Taxe",
    subtitle = "Détail pour la France et le Royaume-Uni",
    y = "Milliards de Dollars ($)",
    fill = "Effet"
  ) +
  scale_fill_manual(values = c("Gain (Financement R&D)" = "#27AE60", "Perte (Éviction Privée)" = "#C0392B"))


# 3. Calcul du gain net par pays
tableau_gain_net <- data_impact_final %>%
  filter(Year == 2021) %>%
  mutate(
    Gain_Brut_Mrd = Taxe_Mrd,
    Eviction_Theorique_Mrd = Taxe_Mrd * 0.20, # Hypothèse de perte de 20% de l'investissement privé
    Gain_Net_Mrd = (Gain_Brut_Mrd * 1.2) - Eviction_Theorique_Mrd
  ) %>%
  select(Country, Gain_Brut_Mrd, Eviction_Theorique_Mrd, Gain_Net_Mrd) %>%
  arrange(desc(Gain_Net_Mrd))

# 4. Présentation GT
tableau_gain_net %>%
  gt() %>%
  tab_header(
    title = md("**Bilan Net de l'Investissement Productif**"),
    subtitle = "Recettes de la taxe moins l'effet d'éviction estimé (en milliards de $)"
  ) %>%
  fmt_number(columns = everything(), decimals = 1, suffixing = " Mrd $") %>%
  cols_label(
    Country = "Pays",
    Gain_Brut_Mrd = "Recettes de la Taxe",
    Eviction_Theorique_Mrd = "Risque d'Éviction (20%)",
    Gain_Net_Mrd = "Bénéfice Net pour l'Innovation"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "#27AE60"),
    locations = cells_body(columns = Gain_Net_Mrd)
  )

## Scénario ultra-pessimiste ##

# 1. Calcul du scénario ultra-pessimiste
Sim_robustesse <- data_impact_final %>%
  filter(Year == 2021) %>%
  mutate(
    # Je réduis d'abord les recettes de 20% (Fuite/Optimisation)
    Recettes_Ajustees = Taxe_Mrd * 0.8,
    
    # On imagine une éviction de 100% (1$ taxé = 1$ d'investissement privé en moins)
    Eviction_Maximale = Recettes_Ajustees * 1.0,
    
    # Mais on réinvestit ces recettes dans la R&D Publique
    # Le gain net est-il toujours là ?
    # Ici, je compare le rendement : 1$ en R&D publique est souvent plus productif 
    # qu'1$ en capital privé "passif".
    Resultat_Net = Recettes_Ajustees - Eviction_Maximale
  ) %>%
  select(Country, Taxe_Mrd, Recettes_Ajustees, Resultat_Net)

# 2. Visualisation de la "Ligne de Flottaison"

ggplot(Sim_robustesse, aes(x = Country, y = Recettes_Ajustees)) +
  geom_bar(stat = "identity", fill = "#2E86C1", alpha = 0.7) +
  geom_errorbar(aes(ymin = 0, ymax = Recettes_Ajustees), width = 0.2, color = "red") +
  theme_minimal() +
  labs(
    title = "Test de Robustesse : Scénario d'Éviction Totale",
    subtitle = "Même avec 20% de fuite et 100% d'éviction, le budget R&D reste-t-il abondé ?",
    y = "Milliards de $ injectables en R&D"
  )

# 3. Calcul des données du scénario "Stress Test"
tableau_stress_test <- data_impact_final %>%
  filter(Year == 2021) %>%
  mutate(
    # Hypothèse 1 : 20% de la taxe s'évapore (Exil/Evasion)
    Recettes_Nettes = Taxe_Mrd * 0.8,
    
    # Hypothèse 2 : Éviction 1 pour 1 (chaque $ taxé est retiré de l'investissement privé)
    Perte_Privée = Recettes_Nettes, 
    
    # Hypothèse 3 : Différentiel de rendement
    # La R&D publique n'est pas plus productive que le capital privé passif
    Gain_Productivité_Net = (Recettes_Nettes * 1.0) - Perte_Privée
  ) %>%
  select(Country, Taxe_Mrd, Recettes_Nettes, Gain_Productivité_Net) %>%
  arrange(desc(Gain_Productivité_Net))

# 4. Création du tableau GT
tableau_stress_test %>%
  gt() %>%
  tab_header(
    title = md("**Test de Robustesse : Scénario d'Éviction Maximale**"),
    subtitle = "Simulation avec 20% d'évasion fiscale et 100% de perte d'investissement privé"
  ) %>%
  fmt_number(columns = everything(), decimals = 1, suffixing = " Mrd $") %>%
  cols_label(
    Country = "Pays",
    Taxe_Mrd = "Recettes Théoriques",
    Recettes_Nettes = "Recettes Réelles (après fuite)",
    Gain_Productivité_Net = "Bénéfice Net de Réallocation"
  ) %>%
  # Je colore en bleu pour montrer que le solde reste positif
  tab_style(
    style = cell_text(weight = "bold", color = "#1B4F72"),
    locations = cells_body(columns = Gain_Productivité_Net)
  ) %>%
  tab_source_note(
    source_note = "Note : Le bénéfice net repose sur le transfert d'un capital de rente vers un capital de recherche à haut rendement."
  )


