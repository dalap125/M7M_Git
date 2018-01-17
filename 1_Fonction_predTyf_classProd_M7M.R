#Ce code sert é appliquer un modéle de random forests pour prédire le type de forét des 
#peuplements qui ont moins de 3 métres ET ceux qui ont 4 é 6M et des TYFs Rx, Fx, 
#RxFx et FxRx. La fonction va aussi prédire la classe de volume (courbe v12 et v34) 
#de tous les peuplements qui ont moins de 7M

#Ce script peut étre roulé directement ou peut étre appelé par un autre script
#avec la fonction "source(édirectoire de ce script.Ré)"


############################################################################################################
############################################################################################################
#Voici les sections de cette fonction:
#   0. Créer un objet oé on va stocker nous extrants
#   1. Charger des packages
#   2. Charger les modéles de random forest
#   3. Joindre les 2 jeux de données (peeOri et variables climatiques)
#   4. Vérifier que toutes les variables dont on a besoin sont lé
#   5. Transformer le code des plantations en CTs (au cas oé il y a des échéques de 
#   plantations dans les peuplements pour lesquels on veut prédire le GTYF et la hauteur;
#   on a besoin de le changer parce que les modéles de random forest ne conaissent pas
#   les codes de plantation)
#   6. Adapter les variables selon les besoins des modéles de random forest (ex. vérifier que
#   que tous les niveaux factorielles sont compatibles))
#   7. Vérifier qu'on n'a pas des données manquantes (ex. toutes les peuplements ont toutes les
#   variables climatiques)
#   8. Prédire le grand TYF et la classe de volume
#   9. Stocker le jeu de données avec les prédictions de GTYF et de COURBE_V dans l'objet
#   d'extrant
#   10. Calculer le tableau resumé avec les superficies occupées par chaque groupe et
#   l'ajouter é l'objet d'extrant
#   11. Terminer la fonction en sélectionnant l'objet d'extrant
############################################################################################################
############################################################################################################


#Voici les arguments de la fonction:
#1. On peut définir la fonction "pred_Tyf_Vol_Prod_M7M()" qu'on a écrit dans un autre 
#script avec la fonction "source( édirectoire du script.Ré)". Cette fonction prédit les 
#grands TYFs et les classes de volume des peuplements qui ont moins de 7M. Cette fonction
#a besoin des arguments suivants: 
# mainDir <- file.path("T:\\Donnees\\Courant\\Projets\\Chantier_M7M", 
#                      "Script_Plusieurs_SDOMs\\M7M")
#                      
#     1.1 dirRfGtyf_couvert : directoire de la liste des modéles de random forests 
#     calibrés pour prédire  les grands TYFs de CHAQUE sous-domaine QUAND LA 
#     VARIABLE TYPE_COUV EST DISPONIBLE 
#     Ceci doit étre un fichier .rds qui a seulement une liste des modéle de random forests 
#     Ex. dirRfGtyf_couvert = file.path(mainDir, "Inputs", "Listes de random forests", "list_RF_GrandTYF_couvert.rds")

#     1.2 dirRfGtyf_pasCouvert : directoire de la liste des modéles de random forests 
#     calibrés pour prédire  les grands TYFs de CHAQUE sous-domaine QUAND LA 
#     VARIABLE TYPE_COUV N'EST PAS DISPONIBLE 
#     Ceci doit étre un fichier .rds qui a seulement une liste des modéle de random forests 
#     Ex. dirRfGtyf_pasCouvert = file.path(mainDir, "Inputs", "Listes de random forests","list_RF_GrandTYF_pasCouvert.rds")


#     2.1 dirRfClassVol_dens : directoire de la liste des modéles de random forests 
#     calibrés pour CHAQUE SOUS_DOMAINE pour prédire les classes de volume 
#     QUAND ON CONNAéT la densité. 
#     Ceci doit étre un fichier .rds qui a seulement une liste des modéle de random forests 
#     Ex. dirRfClassVol_dens = file.path(mainDir, "Inputs", "Listes de random forests","list_RF_ClasseVol_Dens.rds")

#     2.2 dirRfClassVol_pasDens : directoire de la liste des modéles de random forests 
#     calibrés pour CHAQUE SOUS_DOMAINE pour prédire les classes de volume 
#     QUAND ON NE CONNAéT PAS la densité. 
#     Ceci doit étre un fichier .rds qui a seulement une liste des modéle de random forests 
#     Ex. dirRfClassVol_pasDens = file.path(mainDir, "Inputs", "Listes de random forests","list_RF_ClasseVol_pasDens.rds")


#     3. peeOri : dataframe des données pour lesquels on veut prédire les types de forét 
#     et la classende volume. Ceci doit avoir une structure pareille aux fichiers 
#     'pee_ori.dbf'  
#     Ex. peeOri = read_csv(file.path(mainDir, "Inputs", "Update2_CSCPF_08551.csv"))
#     
#     4. donneesClim : dataframe des données avec les variables climatiques. Ceci vient 
#     d'un jeu de données qui a plusieurs variables climatiques BioSIM pour chaque 
#     peuplement dans la 6O (identifiés par la variable éGEOCODEé). 
#     Ex. donneesClim = read_csv(file.path(mainDir, "Inputs", "Climate_08551.csv"))
#     
#     5. tyfToGtyf : table de pilotage pour convertir les TYFs des peuplements de 4 é 6m 
#     en GTYFs. Le fichier spécifié doit étre un dbf
#     Ex. tyfToGtyf = read.dbf(file.path(mainDir, "Inputs\\table_pilotage_resume_tyf_gtyf.dbf"))
#
#     6. typeEcoToFamStat : table de pilotage pour déterminer la famille de station 
#     selon le sous-domaine (SDOM_BIO) et le type eco (TYPE_ECO). 
#     Le fichier spécifié doit étre un csv
#     Ex. typeEcoToFamStat = read.csv2(file.path(mainDir, "Inputs\\TypeEco_To_Fam_Stat_par_SDOM.csv"))
#
#     7. vegPot_SDOM: table de pilotage de conversion des végetations potentielles
#     "originales" dans des valeurs acceptées par le modéle de random forest du 
#     sous-domaine correspondante. Comme la végétation change de sous-domaine en 
#     sous-domaine, les valeurs de veg pot acceptées par chaque modéle change aussi.
#     Pour cette raison on a du créer cette table avec des valeurs bidons différentes
#     pour chaque sous-domaine (par example, comme FE n'existe pas vraiment dans la
#     6O, ces valeurs sont convertis dans la valeur bidon "RS". Sans éa le modéle de
#     random forest ne peut pas marcher par sous domaine et ces cas sont tellement
#     marginaux qu'ils ne doivent pas influencer les prédictions d'une faéon
#     significative; le méme a été faite pour le tableau de conversion du TYPE_ECO
#     en famille de station FAM_STAT)
#     Ex. vegPot_SDOM = read_csv2(file.path(mainDir, "Inputs", "vegPot_par_SDOM.csv"))
#     
#     8. P7M : vecteur logique (TRUE/FALSE) pour decidir si on veut prédire le
#     GTYF et la courbeV pour les peuplements qui ont 7 métres et plus. 
#     La valeur de défaut est FALSE, ce que veut dire que les variables ne sont 
#     pas prédites pour les peuplements qui on 7m et plus
#     

#L'extrant de cette fonction est une liste avec 2 dataframes:
#   1. GTYF_Prod : un dataframe avec 3 colonnes: 
#         - ID_BFEC: l'id du polygone
#         - GTYF_M7M: le GTYF prédit
#         - COURBE_V: la courbe V prédite (v12 ou v34)
#
#       Le GTYF prédit est une union: 
#           - du GTYF original des peuplements de 7M et plus (i.e. 
#           non-prédit par random forest)
#           - du GTYF original des peuplements de 4 é 6M qui avaient déjé 
#           un TYF bon (i.e. un TYF qui n'était pas des RxFx, FxRx, ...)
#           - du GTYF prédit par random forest pour les autres peuplements 
#           qui ont 4 é 6m et pour tous les peuplements qui ont 3m et moins. 
#         
#   
#   
#   2. donneesManquantes : le dataframe des polygones qui ont des données manquantes 
#   (au ca oé on en a)

require(tidyr)
require(dplyr)

# 
# 
# dirRfGtyf_couvert <- file.path(mainDir, "Inputs", "Listes de random forests",
#                                        "list_RF_GrandTYF_couvert.rds")
# dirRfGtyf_pasCouvert <- file.path(mainDir, "Inputs", "Listes de random forests",
#                                        "list_RF_GrandTYF_pasCouvert.rds")
# 
# dirRfClassVol_dens <- file.path(mainDir, "Inputs", "Listes de random forests",
#                                         "list_RF_ClasseVol_Dens.rds")
# 
# dirRfClassVol_pasDens <- file.path(mainDir, "Inputs", "Listes de random forests",
#                                            "list_RF_ClasseVol_pasDens.rds")
# 
# peeOri <- read_csv(file.path(mainDir, "Inputs", "Update2_CSCPF_08551.csv"))
# # peeOri <- 
# #   peeOri %>% 
# #   select(one_of(c("ID_BFEC", "GEOCODE_OR", "SDOM_BIO", "ORIGINE", 
# #                   "PERTURB", "TYPE_COUV", "CL_PENT", 
# #                   "CL_DRAI", "TYPE_ECO", "SUP_BRU", "TYF", 
# #                   "HAUTEUR", "DENSITE")))
# 
# 
# donneesClim <- read_csv(file.path(mainDir, "Inputs", "Climate_08551.csv"))
# 
# 
# tyfToGtyf <- read.dbf(file.path(mainDir, "Inputs", 
#                                 "table_pilotage_resume_tyf_gtyf.dbf"))
# 
# typeEcoToFamStat <- read_csv2(file.path(mainDir, "Inputs",
#                                         "TypeEco_To_Fam_Stat_par_SDOM.csv"))
# 
# vegPot_SDOM <- read_csv2(file.path(mainDir, "Inputs", "vegPot_par_SDOM.csv"))
# 

pred_Tyf_Vol_Prod_M7M <- 
  function(dirRfGtyf_couvert,
           dirRfGtyf_pasCouvert,
           dirRfClassVol_dens,
           dirRfClassVol_pasDens,
           peeOri,
           donneesClim,
           tyfToGtyf,
           typeEcoToFamStat,
           vegPot_SDOM) {  #,
           #P7M = FALSE) {  
    
    
    #0. Créer la liste oé on va stocker tous les résultats
    listeExtrant <- 
      list(GTYF_Prod = NULL,
           donneesManquantes = NULL)
    
    
    
    #1. Charger les packages nécessaires
    require(randomForest)  #Pour faire les random forests (foréts aléatoires)
    require(tidyr)         #Traitement des données
    require(dplyr)         #Traitement des données
    
    
    
    #2. Charger les modéles de random forest 
    #2.1 Modéle de random Forest pour les grands TYFs AVEC le type de couvert
    #2.1.1 Charger le modéle de random forest
    rfGTyf_couv <- readRDS(dirRfGtyf_couvert)
    
    #2.1.2 Vérifier que le modéle existe
    if(!exists("rfGTyf_couv")){  #Alors, s'il n'existe pas ("!" spécifie un négation en R)
      stop(paste("Le modéle de random forest nécessaire ne se trouve pas dans", 
                 dirRfGtyf_couvert, "(le répertoir spécifié par l'objet 'dirRfGtyf_couvert')."))
    }
    
    
    #2.2 Modéle de random Forest pour les grands TYFs SANS le type de couvert
    #2.2.1 Charger le modéle de random forest
    rfGTyf_pasCouv <- readRDS(dirRfGtyf_pasCouvert)
    
    #2.2.2 Vérifier que le modéle existe
    if(!exists("rfGTyf_pasCouv")){  #Alors, s'il n'existe pas ("!" spécifie un négation en R)
      stop(paste("Le modéle de random forest nécessaire ne se trouve pas dans", 
                 rfGTyf_pasCouv, "(le répertoir spécifié par l'objet 'dirRfGtyf_pasCouvert')."))
    }
    
    
    #2.3 Modéle de random Forest pour les classes de volume quand on connaét
    #la densité
    #2.3.1 Charger le modéle de random forest
    rfClassVol_dens <- readRDS(dirRfClassVol_dens)
    
    
    #2.3.2 Vérifier que le modéle existe
    if(!exists("rfClassVol_dens")){  #Alors, s'il n'existe pas ("!" spécifie un négation en R)
      stop(paste("Le modéle de random forest nécessaire ne se trouve pas dans", 
                 rfClassVol_dens, 
                 "(le répertoir spécifié par l'objet 'dirRfClassVol_dens')."))
    }
    
    
    #2.4 Modéle de random Forest pour les classes de volume quand on connaét
    #la densité
    #2.4.1 Charger le modéle de random forest
    rfClassVol_pasDens <- readRDS(dirRfClassVol_pasDens)
    
    
    #2.4.2 Vérifier que le modéle existe
    if(!exists("rfClassVol_pasDens")){  #Alors, s'il n'existe pas ("!" spécifie un négation en R)
      stop(paste("Le modéle de random forest nécessaire ne se trouve pas dans", 
                 rfClassVol_pasDens, 
                 "(le répertoir spécifié par l'objet 'dirRfClassVol_pasDens')."))
    }
    
    
    ###############################################################
    ###############################################################
    #If statement à ajouter quand on va ajouter la partie de prédiction
    #des TYFs et des classes de volume pour les 7MP
    # #3. Joindre les jeux de données (peeOri et variables climatiques)
    # #3.1 Maintenant, si on veut (P7M == TRUE), on peut prédire le courbeV
    # #et le GTYF des peuplements qui ont plus de 7M s'ils sont coupés
    # #On a besoin de nous assurer que la hauteur est une variable numérique
    # 
    # if(P7M == FALSE){

      donneesProc <-
        peeOri %>%
        mutate(HAUTEUR = as.numeric(as.character(HAUTEUR)),
               ID_BFEC = as.character(ID_BFEC)) %>%
        filter(HAUTEUR < 7 | HAUTEUR %in% c(NA, NULL, ""))
    #   
    # } else {
    #   browser()
    #   donneesProc <-
    #     peeOri %>%
    #     mutate(HAUTEUR = as.numeric(as.character(HAUTEUR)),
    #            ID_BFEC = as.character(ID_BFEC),
    #            ORIGINE = ifelse(HAUTEUR >= 7, "CT",
    #                             as.character(ORIGINE))) 
    #   
    # }
    ###############################################################
    ###############################################################
    
    
    #3.2 Ajouter les variables climatiques au jeu de données principal
    #3.2.1 Trouver toutes les variables utilisées par tous lesmodéles de 
    #random forests. La colonne "importance" donne-nous le noms de toutes 
    #les variables utilisées. 
    #Avec "sapply" on peut appliquer une fonction a chaque élément
    #d'une liste. Unlist() regroupe tous les éléments de la liste
    #dans un seul object (i.e. un vecteur dans ce cas) et unique()
    #donne nous les valeurs uniques de ce vecteur
    varsRf1 <- sapply(rfGTyf_couv, function(x)rownames(x$importance))
    varsRf2 <- sapply(rfGTyf_pasCouv, function(x)rownames(x$importance))
    varsRf3 <- sapply(rfClassVol_dens, function(x)rownames(x$importance))
    varsRf4 <- sapply(rfClassVol_pasDens, function(x)rownames(x$importance))
    
    
    varsRf <- unique(c(unlist(varsRf1), unlist(varsRf2),
                       unlist(varsRf3), unlist(varsRf4)))
    
    #3.3.2 La variable vegPot a été créé par nous lors de l'entraénement du modéle. Alors, 
    #pour l'instant il faut la remplacer par la variable initiale (TYPE_ECO)
    if("vegPot" %in% varsRf){
      varsRf[varsRf %in% "vegPot"] <- "TYPE_ECO"
    }
    
    #3.3.3 Trouver les variables climatiques utilisées 
    varsClimUtils <- names(donneesClim)[names(donneesClim) %in% varsRf]
    
    
    #3.3.4 Sélectionner ces variables dans le jeu de données des variables climatiques
    #(en oubliant pas le GEOCODE)
    donneesClim <- donneesClim %>% select(GEOCODE, one_of(varsClimUtils))
    
    #3.3.5 Joindre avec le jeu de données principal
    donneesProc <- 
      left_join(donneesProc %>% mutate(GEOCODE_OR = as.character(GEOCODE_OR)),   #On transforme les GEOCODE_ORS en 
                donneesClim %>% mutate(GEOCODE = as.character(GEOCODE)), #caractéres pour éviter des 
                by = c("GEOCODE_OR" = "GEOCODE"))                 #avertissements inutiles
    
    
    
    #4. Vérifier que toutes les variables utilisées dans le modéle de random forests
    #existent dans les fichiers fournis. 
    #4.1 Comme on va filtrer les données selon leur HAUTEUR, il faut que cette variable soie
    #présente aussi. On va avoir aussi besoin du TYF (pour les peuplements pour lesquels on
    #connaét déjé) et du GEOCODE. On va avoir besoin de "SDOM_BIO" pour faire les groupes
    #évolutifs é la fin
    varsTester <- 
      c(varsRf, "HAUTEUR", "TYPE_ECO", "TYF", "GEOCODE_OR", "SDOM_BIO", "ID_BFEC")
    
    varsTester <-    #FAM_STAT sera crée aprés
      varsTester[!varsTester %in% c("FAM_STAT")] 
    
    #4.2 Alors, maintenant on vérifie que ces 5 variables sont dans l'objet pee_ori fournit
    #"all()" sort "TRUE" si toutes les valeurs donnés sont TRUE, mais FALSE si au moins une des 
    #valeurs fournies est FALSE. Avec la négation (!all(...)), si toutes les valeurs sont TRUE
    #la réponse est FALSE (alors, le if(...) est ignoré), et si une valeur est FALSE, la réponse
    #est TRUE (et la fonction if(...) est fait rouler)
    if(!all(varsTester %in% names(donneesProc))){
      
      #4.2.1 Si on a une variable manquante, il faut l'identifier...
      varsManquantes <- varsTester[!varsTester %in% names(donneesProc)]
      
      #4.2.2 Et arréter la fonction
      stop(paste("La ou les variables", paste(varsManquantes, collapse = ", "), 
                 "n'existent pas dans le fichier fournit dans 'peeOri','donneesClim' et/ou",  
                 "Faites attention que cette fonction est sensible aux", 
                 "minuscules et aux majuscules. Par exemple, 'ORIGINE' est différent de 'origine'.", 
                 "S'il vous plaét changez le nom de la colonne et/ou ajoutez les variables",  
                 "manquantes avant de recommencer. Faites aussi attention que la variable",  
                 "'UPAYS_REG' devrait faire partie du jeu de données fournit dans", 
                 "'donneesClim'. La variable 'PERT_BIN' est créée avec la variable 'PERTURB'. "))
    }
    
    
    #4.3 Vérifier que la table de pilotage que convert les TYFs en GTYFs a tous les TYFs 
    #présents dans le jeu de données
    #4.3.1 Définir les objets avec les tyfs présents dans le jeu de données et les tyfs qui sont
    #dans la table de pilotage. Attention, comme on ne veut pas convertir les valeurs Rx, Fx, RxFx 
    #et FxRx, ils ne sont pas dans la table de pilotage: alors, pour nous assurer que éa ne cause
    #pas des problémes, il faut qu'on l'ajoute é l'objet des TYFs de la table
    tyfsPres <- unique(na.omit(donneesProc$TYF))
    tyfsTable <- c(as.character(tyfToGtyf$TYF), "Fx", "Rx", "RxFx", "FxRx", "Rz", 
                   "RZ", "Fz", "FZ")
    
    #4.3.2 Alors, si au moins une valeur n'est pas dans la table 
    if(!all(tyfsPres %in% tyfsTable)){
      
      #4.3.2.1 Identifier ceux qui manquent
      tyfsManq <- as.character(tyfsPres[!tyfsPres %in% tyfsTable])
      
      #4.3.2.2 Arréter la fonction
      stop(paste("Les codes de TYF", paste(tyfsManq, collapse = ", "), "ne sont pas dans la table", 
                 "de pilotage fournie dans l'objet 'tyfToGtyf'. S'il vous plaét, ajoutez-les é la", 
                 "table de pilotage. Si ce sont des codes d'essences pour lesquels on ne veut pas", 
                 "prédire un grand TYF (ex. des essences de plantation), vous pouvez mettre le code", 
                 "dans la colonne TYF, et 'NA' dans la colonne GTYF."))
    } 
    
    
    
    #5. Enlever toutes les plantations du jeu de données
    ##################################################################
    ##################################################################
    #FINALEMENT ON VA GARDER LES PLANTATIONS POUR LES CAS Oé LES 
    #PLANTATIONS NE SONT PAS BIEN RéUSSIES
    donneesProc <- 
      donneesProc %>% 
      mutate(ORIGINE = ifelse(ORIGINE %in% c("ENS", "P", "PRR", 
                                             "REA", "RIA"), 
                              yes = "CT", no = as.character(ORIGINE)),
             ORIGINE = factor(ORIGINE))
    
    ##################################################################
    ##################################################################
    # #5.1 Enregistrer le nombre de peuplements qui sont des plantations. 
    # #nrow donne nous le nombre de lignes
    # nombrePlantations <- 
    #   nrow(donneesProc %>% filter(ORIGINE %in% c("ENS", "P", "PRR", "REA", "RIA")))
    # 
    # #5.2 Faire un avertissement pour dire combien de peuplements étaient dans des plantations
    # if(!nombrePlantations %in% 0){
    #   warning(paste(nombrePlantations, "peuplements ont été enlevés lors de l'analyse",
    #                 "parce qu'ils étaient des plantations."))
    # }
    # 
    # #5.3 Et enlever les plantations
    # donneesProc <- 
    #   donneesProc %>% 
    #   filter(!ORIGINE %in% c("ENS", "ETR", "P", "PRR", "REA", "RIA", "RPS"))
    
    
    
    #6. Sélectionner et adapter les variables dont on a besoin pour entraéner le 
    #modéle de random forest
    #6.0 Définir les vecteurs avec les niveaux de facteur acceptés par le 
    #modéle de random forest pour chaque variable factorielle. Ils peuvent 
    #étre accédés avec 
    #rfGTyf_couv$oues6$forest$xlevels$ 'nom de la variable factorielle'.
    #De toutes les variables, il y a quelques unes qui peuvent changer
    #entre sous-domaines (e.g. la végétation potentielle). Ceux 
    #vont étre plus complexes é gérer
    #6.0.1 Classe de drainage
    niveauxDrainage <- rfGTyf_couv$`6O`$forest$xlevels$CL_DRAI 
    
    #6.0.2 Perturbation d'origine
    niveauxOrigine <- rfGTyf_couv$`6O`$forest$xlevels$ORIGINE 
    
    #6.0.3 Type de couvert
    niveauxCouv <- rfGTyf_couv$`6O`$forest$xlevels$TYPE_COUV 
    
    #6.0.4 Famille de station
    #Comme les niveaux de FAM_STAT et vegPot changent selon les sous-domaine,
    #le processus pour ces 2 variables (qui va étre décrit tantét) 
    #est plus compléxe 
    niveauxFamStat <- list('6O' = rfGTyf_couv$`6O`$forest$xlevels$FAM_STAT,
                           '5O' = rfGTyf_couv$`5O`$forest$xlevels$FAM_STAT)
    
    #6.0.5 Végétation potentielle
    niveauxVegPot <- list('6O' = rfGTyf_couv$`6O`$forest$xlevels$vegPot,
                          '5O' = rfGTyf_couv$`5O`$forest$xlevels$vegPot)
    
    
    #6.1 CLASSE DE DRAINAGE: Adapter la classe de drainage en utilisant juste 
    #le premier caractére
    donneesProc <- 
      donneesProc %>% 
      mutate(CL_DRAI = ifelse(substr(CL_DRAI, 1,1) %in% c("0", "1"), "1",  
                              ifelse(substr(CL_DRAI, 1,1) %in% "2", "2",
                                     ifelse(substr(CL_DRAI, 1,1) %in% "3", "3",
                                            ifelse(substr(CL_DRAI, 1,1) %in% "4", "4",
                                                   ifelse(substr(CL_DRAI, 1,1) %in% "5", "5",
                                                          ifelse(substr(CL_DRAI, 1,1) %in% "6", "6",
                                                                 CL_DRAI)))))),
             CL_DRAI = factor(CL_DRAI, levels = niveauxDrainage),
             
             
             #6.2 CLASSE DE PENTE: Adapter la classe de pente en la convertissant dans une variable
             #numérique avec la valeur médiane de la classe
             CL_PENT = ifelse(CL_PENT %in% "A", 1.5,
                              ifelse(CL_PENT %in% "B", 6,
                                     ifelse(CL_PENT %in% "C", 12,
                                            ifelse(CL_PENT %in% "D",23,
                                                   ifelse(CL_PENT %in% c("E", "F", "S"), 35,
                                                          CL_PENT))))),
             CL_PENT = as.numeric(CL_PENT),
             
             
             #6.3 CLASSE D'ORIGINE: adapter le champ d'origine en NATUREL (NAT), coupe avec
             #protection de la régénération (CPR) ou coupe totale (CT)
             ORIGINE = case_when(
               .$ORIGINE %in% c("BR", "CHT", "DT", 
                                "ES", "FR", "VER") ~            "NAT",
               .$ORIGINE %in% c("CDV", "CPH", "CPR", "CPT") ~   "CPR",
               TRUE ~      "CT"), #Toute les autres codes sont des coupes puis
                                  #on considere que tous les NAs sont des CTs
             ORIGINE = factor(ORIGINE, levels = niveauxOrigine)) %>% 
      
      
      #6.4 DENSITE: si la densité est NA, on lui donne une valeur de 0
      mutate(DENSITE = as.numeric(as.character(DENSITE)),
             
             
             #6.5 Type de couvert: Il faut vérifier que les niveaux de cette 
             #variable sont les niveaux que le modéle de random forest utilise
             TYPE_COUV = factor(TYPE_COUV, levels = niveauxCouv))
    
    
    #6.6 FAMILLE DE STATION FORESTIéRE 
    ##Cette variable est plus compliqué parce que les groupes qu'on fait peuvent 
    #varier selon le sous-domaine (i.e. selon les données disponibles). Par exemple,
    #"RES" est un groupe trés importante dans la 6Ouest mais pas dans la 3Ouest.
    #Alors, on a fait un tableau de correspondance que doit étre mis é jour pour
    #chaque sous-domaine. Les groupes de FAM_STAT acceptées doivent étre les groupes
    #acceptés par les modéles de random forest du sous-domaine correspondant.
    #6.6.1 D'abord il faut sélectionner les variables qu'on veut de la table de 
    #correspondance
    typeEcoToFamStat <- 
      typeEcoToFamStat %>% 
      mutate(SDOM_BIO = as.character(SDOM_BIO),
             TYPE_ECO = as.character(TYPE_ECO)) %>% 
      distinct()  #Pour enlever les doublons
    
    #6.6.2 Aprés il faut faire un join
    #On convertit SDOM_BIO et TYPE_ECO en variables de caractéres
    #pour éviter des messages d'erreur inutiles
    donneesProc <- 
      donneesProc %>% 
      mutate(SDOM_BIO = as.character(SDOM_BIO),
             TYPE_ECO = as.character(TYPE_ECO)) %>% 
      left_join(typeEcoToFamStat, 
                by = c("SDOM_BIO", "TYPE_ECO"))
    
    
    #6.7 VéGéTATION POTENTIELLE : 
    #Méme chose que la FAM_STAT.
    #Cette variable est plus compliqué parce que les groupes qu'on fait peuvent 
    #varier selon le sous-domaine (i.e. selon les données disponibles). Par exemple,
    #"RS" est un groupe trés importante dans la 6Ouest mais pas dans la 3Ouest.
    #Alors, on a fait un tableau de correspondance que doit étre mis é jour pour
    #chaque sous-domaine. Les groupes de vegPot acceptées doivent étre les groupes
    #acceptés par les modéles de random forest du sous-domaine correspondant. Les
    #regroupements suivent les catégories du modéle SUCCéS (quand possible)
    #6.7.1 Créer la variable de la végetation potentielle "réelle"
    donneesProc <- 
      donneesProc %>% 
      mutate(vegPot = substr(TYPE_ECO, 1, 3)) %>% 
      
      
      #6.7.2 Ajouter la valeur de veg pot convertit (i.e. le niveaux de vegetation
      #potentielle acceptées par les modéles de random forest changent selon le
      #sous-domaine)
      left_join(vegPot_SDOM, by = c("SDOM_BIO", "vegPot" = "vegPot_Ori")) %>% 
      
      #6.7.3 Enlever la valeur originale de vegPot et la remplacer par la
      #vegPot_Conv de la table de pilotage
      select(-vegPot) %>% 
      rename(vegPot = vegPot_Conv)
    
    
    #6.7.4 Maintenant qu'on n'a plus besoin du TYPE_ECO, il faut 
    #le remplacer par "vegPot" dans l'objet des variables é utiliser 
    #dans random forest
    varsRf <- ifelse(varsRf %in% "TYPE_ECO", "vegPot", varsRf)
    
    
    # #6.9 UNITé DE PAYSAGE : 
    #################################################################
    #################################################################
    # ON NE VA PLUS UTILISER L'UNITé DE PAYSAGE PARCE QUE C'EST ASSEZ
    # COMPLéXE é METTRE AVEC PLUSIEURS SOUS-DOMAINES
    #################################################################
    ################################################################# 
    # au moins 2 unités de paysage trés petites ne faisaient
    # #pas partie de notre jeu de données d'entraénement. Alors, il faut les 
    # #mettre quelque part pour éviter que le modéle plante.   
    # 
    #            #6.8.1 On met la "124" dans la "123",
    # mutate(UPAYS_REG = ifelse(UPAYS_REG %in% "124", "123",
    #            #6.8.2 La "125" dans la "126"
    #                    ifelse(UPAYS_REG %in% "125", "126",
    #            #6.8.3 Et n'importe quelle autre dans la "131", qui est l'unité la plus grosse.
    #                           ifelse(!UPAYS_REG %in% niveauxPaysReg,
    #                                  "131",
    #            #6.8.4 Sinon, on laisse la valeur originale
    #                                  as.character(UPAYS_REG)))),
    #        UPAYS_REG = factor(UPAYS_REG, levels = niveauxPaysReg)) %>%  
    
    
    
    #7. Vérifier qu'on n'a pas des données manquantes
    #7.0 Il faut enlever le TYPE_COUV de la sélection de variables é tester
    #pour des valeurs manquantes parce que le modéle fonctionne quand 
    #TYPE_COUV == NA. Il faut faire la méme chose avec la densité,
    #car on a des modéles qui marchent quand es variables ne 
    #sont pas disponibles
    varsRfPasManq <- 
      c(varsRf[!varsRf %in% c("TYPE_COUV")], "SDOM_BIO")
    
    #7.1 Obtenir le vecteur TRUE/FALSE des observations incomplétes
    #complete.cases() donne nous toutes les observations qui n'ont pas de valeurs manquantes.
    #Alors, avec "!" on peut avoir toutes les observations qui ONT des valeurs manquantes
    obsManquantes <- !complete.cases(donneesProc[ ,varsRfPasManq])
    
    
    #7.2 Alors, si on a des observations avec des valeurs manquantes, il faut avertir l'utilisateur
    #7.2.1 Si on a au moins une valeur manquante
    if(any(obsManquantes)){
      
      #7.2.2 On isole les observations qui ont des observations manquantes
      donneesManquantes <- 
        donneesProc %>% filter(obsManquantes)
      
      #7.2.3 On stock ce jeu de données dans notre liste d'extrant
      listeExtrant$donneesManquantes <- donneesManquantes
      
      #7.2.4 On fait un avertissement mais on laisse le reste de l'analyse rouler
      warning(paste("Quelques observations (", nrow(donneesManquantes), "avaient des données", 
                    "manquantes et vont avoir un tyfFinal de NA (une valeur manquante).", 
                    "Ces observations ont été enregistrées dans l'object d'extrant."))
      
    }
    
    
    
    #8. Prédire le grand TYF avec le modéle de random forests et obtenir le tyf final.
    #On va laisser le TYF original des peuplements de 7m+ et des 4-6m bien identifiés
    #par le photo-interpréte, et on va utiliser le grand tyf prédit pour les peuplements 
    #de 0 é 3 m et les 4-6m mal identifiés (ex. les RxFx) 
    #8.0 Créer une fonction qui vas nous permettre de prédire le GTYF en tenant le
    #sous-domaine en compte. En fait, la plus grosse difficulté sont la vegPot et 
    #la famille de station parce que les niveaux de cette variable varient avec
    #le sous-domaine considéré
    pred_GTYF_par_SDOM <- 
      function(inputParSDOM, 
               levelsVegPot = niveauxVegPot, 
               levelsFamStat = niveauxFamStat,
               rfGTyf_couv, rfGTyf_pasCouv,
               rfClassVol_dens, rfClassVol_pasDens){
        
        #8.0.1 Sélectionner les niveaux de FAM_STAT et de vegPot selon le sous-domaine
        #Comme le nom de chaque élément des listes qui ont les niveaux des variables
        #est égal aux niveaux du sous-domaine (6O, 5O, ...), on peut sélectionner
        #l'élément de la liste en utilisant la valeur unique du sous-domaine. Comme
        #cette fonction doit étre appliquée a un jeu de données regroupé selon le 
        #sous-domaine, la variable SDOM_BIO va toujours avoir une seule valeur unique 
        levelsFamStat <- levelsFamStat[[unique(inputParSDOM$SDOM_BIO)]]
        levelsVegPot <- levelsVegPot[[unique(inputParSDOM$SDOM_BIO)]]
        
        
        #8.0.2 Définir les niveaux des variables FAM_STAT et vegPot selon les valeurs
        #du sous-domaine  
        inputParSDOM <- 
          inputParSDOM %>%                  #On guarde les valeurs originaux de 
          mutate(FAM_STAT_ORI = FAM_STAT,   #FAM_STAT et vegPot dans une autre variable
                 vegPot_ORI = vegPot,
                 FAM_STAT = factor(FAM_STAT, levels = levelsFamStat),
                 vegPot = factor(vegPot, levels = levelsVegPot))
        
        
        #8.0.3 Sélectionner les random forests é utiliser (i.e. ceux du
        #sous-domaine correspondante)
        rfGTyf_couv_sdom <- rfGTyf_couv[[unique(inputParSDOM$SDOM_BIO)]]
        rfGTyf_pasCouv_sdom <- rfGTyf_pasCouv[[unique(inputParSDOM$SDOM_BIO)]]
        rfClassVol_dens_sdom <- rfClassVol_dens[[unique(inputParSDOM$SDOM_BIO)]]
        rfClassVol_pasDens_sdom <- rfClassVol_pasDens[[unique(inputParSDOM$SDOM_BIO)]]
        
        
        #8.0.4 Prédire le GTYF
        inputParSDOM <-
          inputParSDOM %>%

          #8.0.4.1 Si le TYPE_COUV n'est pas disponible on utilise un modéle
          mutate(predGTYF = ifelse(TYPE_COUV %in% c("", NA, NULL),
                                   as.character(predict(rfGTyf_pasCouv_sdom, .)),

                                   #8.0.4.2 S'il ne l'est pas, on utilise un autre modéle
                                   as.character(predict(rfGTyf_couv_sdom, .))))
        
    
        #8.0.5 Prédire la classe de volume
        inputParSDOM <- 
          inputParSDOM %>% 
          
          #8.0.5.1 Si on n'a pas de densité, on applique le random forest
          #qui n'a pas cette variable
          mutate(COURBE_V = ifelse(DENSITE %in% c("", NA, NULL),
                                   as.character(predict(rfClassVol_pasDens_sdom, .)),
                                   
                                   #8.0.5.2 Si on a la densité, on applique le random forest qui a la DENSITE
                                   as.character(predict(rfClassVol_dens_sdom, .))))
        
        
        #8.0.6 On récupére les valeurs originaux de FAM_STAT et vegPot
        inputParSDOM <- 
          inputParSDOM %>% 
          mutate(FAM_STAT = FAM_STAT_ORI,
                 vegPot = vegPot_ORI) %>% 
          select(-FAM_STAT_ORI, -vegPot_ORI)
        
        
        #8.0.7 Retourner ce dataframe avec toutes les prédictions
        return(inputParSDOM)
        
        
      }
    
  
    #8.1 Prédire le GTYF et la classe de volume en utilisant cette fonction
    #8.1.1 Regrouper les données selon le SDOM
    donneesProc <- 
      donneesProc %>% 
      
      #On va enlever les données qui n'ont pas un sous-domaine
      #éa ne devrait pas arriver, mais c'est une bonne idée de le mettre quand méme
      #######################################################
    filter(!is.na(SDOM_BIO)) %>% 
      #######################################################
    group_by(SDOM_BIO) %>% 
      
      #8.1.2 Utiliser la fonction qu'on vient d'écrire pour prédire les 3 variables
      do(pred_GTYF_par_SDOM(inputParSDOM = .,
                            levelsFamStat = niveauxFamStat,
                            levelsVegPot = niveauxVegPot, 
                            rfGTyf_couv = rfGTyf_couv, 
                            rfGTyf_pasCouv = rfGTyf_pasCouv, 
                            rfClassVol_dens = rfClassVol_dens,
                            rfClassVol_pasDens = rfClassVol_pasDens)) %>% 
      ungroup()
    
   
    #8.2 Traiter des prédictions des GTYFS
    #8.2.1 Ajouter la table de pilotage pour convertir les TYFs des peuplements 
    #de 4 é 6 en GTYFs (pour les peuplements dont on connait le TYF)
    donneesProc <- 
      left_join(donneesProc %>% mutate(TYF = as.character(TYF)),   #On convertisse en caractéres pour éviter
                tyfToGtyf %>% mutate(TYF = as.character(TYF)),     #des avertissements inutiles
                by = "TYF")
    
 
    #8.2.2 On garde le TYF original pour les peuplements de 7M+
    donneesProc <- 
      donneesProc %>% 
      mutate(GTYF_M7M = 
               ifelse(HAUTEUR <= 3 | HAUTEUR %in% c(NA, "NA", ""), as.character(predGTYF),
                      ifelse(HAUTEUR >= 4 & HAUTEUR <= 6 & is.na(GTYF), as.character(predGTYF),
                             ifelse(HAUTEUR >= 4 & HAUTEUR <= 6 & !is.na(GTYF), as.character(GTYF),
                                    ifelse(HAUTEUR >= 7, as.character(TYF),
                                           as.character(predGTYF))))),
             GTYF_M7M = factor(GTYF_M7M))
    
    
    #8.3 Traiter des prédictions des classes de volume
    donneesProc <- 
      donneesProc %>% 
      
      #8.3.1 Donner une valeur de NA aux peuplements qui ont une hauteur >= 7
      mutate(COURBE_V = ifelse(HAUTEUR >= 7, NA, as.character(COURBE_V)),
             
             #8.3.2 Transformer la variable dans un facteur
             COURBE_V = factor(COURBE_V, 
                               levels = levels(rfClassVol_dens$`6O`$predicted)))
    
    
    
    #9. Attribuer l'extrant principal (data frame avec l'ID_BFEC, le grand TYF prédit et
    #la classe de volume prédite) é la liste d'extrant oé on stocke nous 
    #résultats
    listeExtrant$GTYF_Prod <- 
      donneesProc %>% 
      select(ID_BFEC, GTYF_M7M, COURBE_V)
    
    
    
    #10 Calculer le tableau resumé avec les superficies occupées par chaque groupe
    tabResu <- 
      donneesProc %>% 
      
      #10.1 Regrouper les données selon nous variables
      group_by(SDOM_BIO, GTYF_M7M, GR_STAT) %>% 
      
      #10.2 Calculer la somme de leur superficies
      summarise(SUP_BRU = sum(SUP_BRU)) %>% 
      
      #10.3 Calculer leur pourcentage
      ungroup() %>% 
      mutate(SUP_POURC = SUP_BRU/sum(SUP_BRU)*100,
             SUP_POURC = round(SUP_POURC, 2)) %>% 
      
      #10.4 Mettre les lignes dans une bonne ordre
      arrange(SDOM_BIO, GTYF_M7M, GR_STAT)
    
    #10.5 Ajouter ce tableau aux extrants de la fonction
    listeExtrant$TAB_RESU <- tabResu
    
    
    #11. Faire "return" de la liste avec les dataframes d'extrant 
    return(listeExtrant)
    
  } 


