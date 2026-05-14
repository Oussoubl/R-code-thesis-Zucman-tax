# R-code-thesis-Zucman-tax

Ce dépôt contient l'intégralité du code source R et des modèles de simulation utilisés pour mon mémoire de Master 2 en macroéconomie internationale, donc toute réutilisation académique doit etre correctement citée. Ce travail permet d'analyser l'impact d'une taxe mondiale de 2% du patrimoine des plus grandes fortunes sur les trois variables suivantes : investissement productif, innovation et croissance. 

# Objectifs de la recherche

L'étude vise à démontrer les effets de cette taxe sur les variables citées précedemment. Cela permettra notamment entre autres d'observer comment la réallocation du capital "statique" (rente) vers le capital "productif" (R&D) peut stimuler la croissance, même en tenant compte de l'évasion fiscale. 

# Structure du projet

- Code R : Scripts R pour le nettoyage des données, les tests de corrélation et les simulations.
- Base de données : Sources des données (WID, World Bank, OECD).
- Résultats : Visualisations et graphiques générés (Comparaison "Bloc Rentier" vs "Bloc Innovation" par exemple).

# Méthodologie et Reproductibilité

Le modèle me permet notamment de simuler deux scénarios (Pessimiste et Catastrophe) en faisant varier :

1. Les taux d'éviction de l'investissement privé (jusqu'à 20% pour le scénario pessimiste et 100% pour le scénario catastrophe).
2. La présence de fuite de capitaux.
3. Le rendement de l'investissement public en R&D.

# Limites actuelles

Ce travail utilise un modèle à "agent représentant" pour les calculs de croissance agrégée. Bien que cette approche permette d'établir des ordres de grandeur robustes, elle simplifie les comportements de consommation et d'épargne différenciés entre les classes sociales.

# Ouverture vers la thèse

Ce projet constitue le socle technique d'une possible future recherche doctorale. L'objectif de la thèse sera de dépasser le cadre de l'agent représentant en développant un modèle à **agents hétérogènes (HANK)**. Cette approche permettra de modéliser plus finement les réactions stratégiques du Top 0,1% face à l'impôt et d'affiner les résultats sur la redistribution réelle de la richesse.

---------
Oussama Boulassel  
Version : v1.0 (Soumission Mémoire 2026)
