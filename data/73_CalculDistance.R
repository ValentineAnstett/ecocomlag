library(sf)
library(dplyr)
###Paramètres 
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale")
pts_lagunes = st_read("Pts_lagunes.shp")
trait_cote  = st_read("Trait_cote.shp")
altitude = st_read("Altitude.shp")

###Calcul distance eau libre 
dist_mat = st_distance(pts_lagunes, trait_cote)
pts_lagunes$dist_trait_cote_m <- apply(dist_mat, 1, min)

### Calcul distance relief (=10m) 
altitude_10 = altitude %>% 
  filter(ALTITUDE == 10)
dist_matrix = st_distance(pts_lagunes, altitude_10)
pts_lagunes$dist_alt10 = apply(dist_matrix, 1, min)


####Dataframe propre

#Retirer les colonnes inutiles et renommer celles que l'on garde 
pts_lagunes = pts_lagunes %>%
  dplyr::select(COD_LAG, dist_trait_cote_m, dist_alt10)
pts_lagunes = st_drop_geometry(pts_lagunes)

#Renommer CODE_LAG en ID_LAG et mettre des 0 
pts_lagunes = pts_lagunes %>%
  rename(ID_LAG = COD_LAG)

pts_lagunes$ID_LAG = sub("_(\\d)$", "_0\\1", pts_lagunes$ID_LAG)

#Renommer PCA_CHA et FOS_CAB 
pts_lagunes = pts_lagunes %>%
  mutate(ID_LAG = dplyr::recode(ID_LAG,
                              "PCA_CHA_01" = "PCA_CAP_06",
                              "PCA_CHA_03" = "PCA_CAP_08",
                              "PCA_CHA_05" = "PCA_CAP_10"))

pts_lagunes$ID_LAG = gsub("FOS_CAB", "FOS_REL", pts_lagunes$ID_LAG)

#Créer colonne Site  
pts_lagunes = pts_lagunes %>%
  mutate(Site = sub("_[^_]+$", "", ID_LAG))

#Supprimer les sites et les lagunes non intégrés 
sites_a_exclure = c("BPA_PIS", "BAG_GRA", "CAN_NAZ", "HYE_VIE", "THA_SET", "SAL_DEV")
pts_lagunes = pts_lagunes %>%
  filter(!Site %in% sites_a_exclure)

lag_a_exclure =  c("PCA_CHA_02", "PCA_CHA_04", "ORB_ORP_11")
pts_lagunes = pts_lagunes %>%
  filter(!ID_LAG %in% lag_a_exclure)

#Mettre des lettres pour les sites 
pts_lagunes = pts_lagunes %>%
  mutate(Site = dplyr::recode(Site,
                              "LAP_SAL" = "A",
                              "SIG_GSA" = "B",
                              "ORB_ORP" = "C",
                              "ORB_MAI" = "D",
                              "BAG_PET" = "E",
                              "PAL_FRO" = "F",
                              "PAL_VIL" = "G",
                              "EOR_MOT" = "H",
                              "PCA_CAP" = "I",
                              "GCA_RNC" = "J",
                              "FOS_REL" = "K"))


#Adapter dans la colonne ID_LAG les nouveaux codes sites 
pts_lagunes = pts_lagunes %>%
  mutate(ID_LAG = paste0(Site, "_", sub(".*_", "", ID_LAG)))


#Enregistrer 
write.csv(pts_lagunes, file = "/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_Topo/Topographie.csv", row.names = FALSE)
