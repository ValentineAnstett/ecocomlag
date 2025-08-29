#Import dataset 
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Sol/Data/Processed")
Sol = read.csv("Sol.csv", header = TRUE, sep = ",", dec=".")

####INTER-SITES####

####INTRA-SITES####

##GGPLOTS##
#Type de sol 
Sol_sanslag = Sol[,-1]
Data_sanslag = Sol_sanslag [, -c(2:4)]
data_long_sanslag = Data_sanslag %>%
  pivot_longer(cols = -Site, names_to = "Sol", values_to = "Pourcentage")
ggplot(data_long_sanslag, aes(x = Site, y = Pourcentage, fill = Sol)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Type de sol par site",
    x = "Site",
    y = "Pourcentage (%)",
    fill = "Sol"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Faire pivoter les noms des lagunes


##GGPLOTS##
#POURCENTAGES 
Sol_pourcentage = Sol[, -c(5,6,7)]

data_long_pourcentage = Sol_pourcentage %>%
  pivot_longer(cols = -c(Annee, Site, ID_LAG), names_to = "Compo_sol", values_to = "Pourcentage")
##Annes par annees 
#Sur le meme graph
ggplot(data_long_pourcentage, aes(x = ID_LAG, y = Pourcentage, fill = Compo_sol)) +
  geom_bar(stat = "identity", position = "stack") + 
  facet_grid(Site ~ Annee) +  
  labs(title = "Composition du sol",
       x = "LAGUNE", 
       y = "Pourcentage dans le sol (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Graph par annee 
sites = unique(data_long_pourcentage$Site)
annees = unique(data_long_pourcentage$Annee)
especes = colnames(data_long_pourcentage)[4:ncol(data_long_pourcentage)]

for (site in sites) {
  for (annee in annees) {
    data_site_annee = data_long_pourcentage %>%
      filter(Site == site & Annee == annee)
    p = ggplot(data_site_annee, aes(x = ID_LAG, y = Pourcentage, fill = Compo_sol)) +
      geom_bar(stat = "identity", position = "stack") + 
      labs(title = paste("Composition du sol pour le site", site, "et l'année", annee),
           x = "LAGUNE", 
           y = "Pourcentage (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  }
}
#Graphs par site 
sites = unique(data_long_pourcentage$Site)
especes = colnames(data_long_pourcentage)[4:ncol(data_long)]

for (site in sites) {
  data_site = data_long_pourcentage %>%
    filter(Site == site)
  p = ggplot(data_site, aes(x = ID_LAG, y = Pourcentage, fill = Compo_sol)) +
    geom_bar(stat = "identity", position = "stack") +  
    facet_wrap(~ Annee) +  
    labs(title = paste("Composition du sol pour le site", site),
         x = "LAGUNE", 
         y = "Pourcentage (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}


###C/N
Sol_CARBAZ = Sol[, c(1,2,3,7)]

data_long_CARBAZ = Sol_CARBAZ %>%
  pivot_longer(cols = -c(Annee, Site, ID_LAG), names_to = "CarboneAzote", values_to = "Valeur")
#Graphs par site 
sites = unique(data_long_CARBAZ$Site)
especes = colnames(data_long_CARBAZ)[4:ncol(data_long_CARBAZ)]

for (site in sites) {
  data_site = data_long_CARBAZ %>%
    filter(Site == site)
  p = ggplot(data_site, aes(x = ID_LAG, y = Valeur, fill = CarboneAzote)) +
    geom_bar(stat = "identity", position = "stack") +  
    facet_wrap(~ Annee) +  
    labs(title = paste("Rapport C/N pour le site", site),
         x = "LAGUNE", 
         y = "C/N") +
    scale_fill_manual(values = (CarboneAzote = "#a6761d")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}


#P2O5 -- engrais phosphatés 

Sol_P2O5 = Sol[, c(1,2,3,5)]

data_long_P2O5 = Sol_P2O5 %>%
  pivot_longer(cols = -c(Annee, Site, ID_LAG), names_to = "Engrais_phosphates", values_to = "Pourcentage")
#Graphs par site 
sites = unique(data_long_P2O5$Site)
especes = colnames(data_long_P2O5)[4:ncol(data_long_P2O5)]

for (site in sites) {
  data_site = data_long_P2O5 %>%
    filter(Site == site)
  p = ggplot(data_site, aes(x = ID_LAG, y = Pourcentage, fill = Engrais_phosphates)) +
    geom_bar(stat = "identity", position = "stack") +  
    facet_wrap(~ Annee) +  
    labs(title = paste("Pourcentage de P2O5 dans le sol pour le site", site),
         x = "LAGUNE", 
         y = "P2O5 (%)") +
    scale_fill_manual(values = (Engrais_phosphates = "#1b9e77")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

#Azote Tot 
Sol_N = Sol[, c(1,2,3,6)]

data_long_N = Sol_N %>%
  pivot_longer(cols = -c(Annee, Site, ID_LAG), names_to = "AzoteTotal", values_to = "mg_kg")
#Graphs par site 
sites = unique(data_long_N$Site)
especes = colnames(data_long_N)[4:ncol(data_long_N)]

for (site in sites) {
  data_site = data_long_N %>%
    filter(Site == site)
  p = ggplot(data_site, aes(x = ID_LAG, y = mg_kg, fill = AzoteTotal)) +
    geom_bar(stat = "identity", position = "stack") +  
    facet_wrap(~ Annee) +  
    labs(title = paste("Quantité d'azote dans le sol sur le site", site),
         x = "LAGUNE", 
         y = "N (mg/kg)") +
    scale_fill_manual(values = (Engrais_phosphates = "#7570b3")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}
