#Il faut mettre les modèles de random forest dans des listes selon 
#les arguments de la fonction.
#Par example, pour l'argument "dirRfGtyf_couvert" il faut avoir une
#liste avec le random forest 6O et le random forest 5O

#Ce script sert à créer ces listes
outputDir <- file.path("T:\\Donnees\\Courant\\Projets\\Chantier_M7M", 
                       "Script_Plusieurs_SDOMs\\M7M\\Inputs", 
                       "Listes de random forests")
rfDir6O <- file.path("T:\\Donnees\\Courant\\Projets\\Chantier_M7M", 
                     "Script_Plusieurs_SDOMs\\M7M", 
                     "Random forests\\6Ouest")
rfDir5O <- file.path("T:\\Donnees\\Courant\\Projets\\Chantier_M7M", 
                     "Script_Plusieurs_SDOMs\\M7M\\Random forests", 
                     "5Ouest")


#1. Argument "dirRfGtyf_couvert"
#1.1 Charger les modèles
#1.1.1 6 Ouest
rf_GTYF_Couv_6O <- 
  readRDS(file.path(rfDir6O, "RF - GTYF\\6O_RF_GrandTyf_Couvert.rds"))

#1.1.2 5 Ouest
rf_GTYF_Couv_5O <- 
  readRDS(file.path(rfDir5O, "RF - GTYF\\5O_RF_GrandTyf_Couvert.rds"))

#1.1.3 PROCHAINE SOUS-DOMAINE


#1.2 Créer la liste
#On va laisser les noms des sous-domaine "6O", "5O", ... parce que 
#ça va aider beaucoup après quand on veut sélectionner les modèles de random 
dirRfGtyf_couvert <- list('6O' = rf_GTYF_Couv_6O,
                          '5O' = rf_GTYF_Couv_5O)

#1.3 Enregistrer
saveRDS(dirRfGtyf_couvert,
        file = file.path(outputDir, 
                         "list_RF_GrandTYF_couvert.rds"))



#2. Argument "dirRfGtyf_pasCouvert"
#2.1 Charger les modèles
#2.1.1 6 Ouest
rf_GTYF_pasCouv_6O <- 
  readRDS(file.path(rfDir6O, "RF - GTYF\\6O_RF_GrandTyf_pasCouvert.rds"))

#2.1.2 5 Ouest
rf_GTYF_pasCouv_5O <- 
  readRDS(file.path(rfDir5O, "RF - GTYF\\5O_RF_GrandTyf_pasCouvert.rds"))

#2.1.3 PROCHAINE SOUS-DOMAINE


#2.2 Créer la liste
dirRfGtyf_pasCouvert <- list('6O' = rf_GTYF_pasCouv_6O,
                             '5O' = rf_GTYF_pasCouv_5O)

#2.3 Enregistrer
saveRDS(dirRfGtyf_pasCouvert,
        file = file.path(outputDir, "list_RF_GrandTYF_pasCouvert.rds"))



#3. Argument "dirRfClassVol_dens"
#3.1 Charger les modèles
#3.1.1 6 Ouest
rf_classVol_dens_6O <- 
  readRDS(file.path(rfDir6O, 
                    "RF - Courbe V\\6O_RF_ClasseVol_Dens_Age_30_50_70.rds"))

#3.1.2 5 Ouest
rf_classVol_dens_5O <- 
  readRDS(file.path(rfDir5O, 
                    "RF - Courbe V\\5O_RF_ClasseVol_Dens_Age_30_50_70.rds"))

#3.1.3 PROCHAINE SOUS-DOMAINE


#3.2 Créer la liste
dirRfClassVol_dens <- list('6O' = rf_classVol_dens_6O,
                           '5O' = rf_classVol_dens_5O)

#3.3 Enregistrer
saveRDS(dirRfClassVol_dens,
        file = file.path(outputDir, "list_RF_ClasseVol_Dens.rds"))



#4. Argument "dirRfClassVol_pasDens"
#4.1 Charger les modèles
#4.1.1 6 Ouest
rf_classVol_pasDens_6O <- 
  readRDS(file.path(rfDir6O, 
                    "RF - Courbe V\\6O_RF_ClasseVol_sansDens_Age_30_50_70.rds"))

#4.1.2 5 Ouest
rf_classVol_pasDens_5O <- 
  readRDS(file.path(rfDir5O, 
                    "RF - Courbe V\\5O_RF_ClasseVol_sansDens_Age_30_50_70.rds"))

#4.1.3 PROCHAINE SOUS-DOMAINE


#4.2 Créer la liste
dirRfClassVol_pasDens <- list('6O' = rf_classVol_pasDens_6O,
                              '5O' = rf_classVol_pasDens_5O)

#4.3 Enregistrer
saveRDS(dirRfClassVol_pasDens,
        file = file.path(outputDir, "list_RF_ClasseVol_pasDens.rds"))



