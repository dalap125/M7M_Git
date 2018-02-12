#Positionnement des polygones de M7M. 
#Ce script va positionner les peuplements qui ont moins de 7 mètres
#après qu'on aille déterminé leur GTFY, classe de volume (Script 1) et qu'on
#les aille attribué une courbe et une classe de décalage décalage (Script 2)

#Les intrants de la fonction principale (posit_M7M):
#   cloneCourbes - dataframe du catalogue des clones des courbes M7M décalées 
#   selon les classes de décalage
#   e.g. testCloneCourbes <- read_csv("T:\\Donnees\\Courant\\Projets\\Chantier_M7M\\David\\SIFORT M7M\\Prod - Fonction pred gtyf et productivité\\Inputs\\Courbes_6ouest_decal.csv")
#   
#   cscpf: dataframe du jeu de données principal
#   e.g. testCscpf <- read.dbf("T:\\Donnees\\Courant\\Projets\\Chantier_M7M\\David\\SIFORT M7M\\Prod - Fonction pred gtyf et productivité\\Outputs\\outputScript3.dbf")
#
#   metaOri: dataframe du fichier metaOri qui a les années de production 
#   originales
#   e.g. testMetaOri <- read.dbf("T:\\Donnees\\Courant\\Projets\\Chantier_M7M\\David\\SIFORT M7M\\Prod - Fonction pred gtyf et productivité\\Inputs\\metaOri.dbf")
#   
#   clAgeToNum: table de correspondance entre les classe d'âge et leur valeur 
#   numériques correspondantes
#   e.g. clAgeToNum <- 
#          read.csv2(file.path("T:\\Donnees\\Courant\\Projets\\Chantier_M7M\\David",
#                              "SIFORT M7M\\Prod - Fonction pred gtyf et productivité", 
#                              "Inputs", "classe_age_to_numerique.csv"),
#          stringsAsFactors = FALSE)
#
#   anneeDebut: année dans laquelle le calcul va commencer (le défaut est 2018)
#   e.g. anneeDebut <- 2018
#   nCluster_Improd: nombre maximale de clusters pour les groupes improductifs
#   e.g. nCluster_Improd = 2
# 
#   nCluster_Plant: nombre maximale de clusters pour les plantations
#   e.g. nCluster_Plant = 3
# 
#   nCluster_EPC: nombre maximale de clusters pour les EPCs
#   e.g. nCluster_EPC = 3
# 
#   nCluster_M7M: nombre maximale de clusters pour les autres peuplements M7M
#   e.g. nCluster_M7M = 4
#   
#   supMinClust_Improd: superficie minimale de chaque cluster improductif 
#   (en pourcentage de la superficie totale du groupe). 
#   E.g. supMinClust_Improd = 10 défine une superficie minimale d'au moins 10% de 
#   la superficied du groupe correspondant
#   e.g. supMinClust_Improd = 10
#   
#   supMinClust_Plant: superficie minimale de chaque cluster de plantation 
#   (en pourcentage de la superficie totale du groupe). 
#   e.g. supMinClust_Plant = 10
#   
#   supMinClust_EPC: superficie minimale de chaque cluster des EPC 
#   (en pourcentage de la superficie totale du groupe). 
#   e.g. supMinClust_EPC = 10
#   
#   supMinClust_M7M: superficie minimale de chaque cluster des autres M7M 
#   (en pourcentage de la superficie totale du groupe). 
#   e.g. supMinClust_M7M = 10
# 
#   traiter_7MP_Plant: indicateur logique (TRUE/FALSE) pour indiquer si on
#   veux traiter les plantations qui on 7MP
#   e.g. traiter_7MP_Plant = FALSE  ---> les plantations 7MP ne sont pas positionnées
# 
#   traiter_7MP_EPC: indicateur logique (TRUE/FALSE) pour indiquer si on
#   veux traiter les EPCs qui on 7MP
#   e.g. traiter_7MP_EPC = FALSE  ---> les EPCs 7MP ne sont pas positionnées


############################################################################
############################################################################
#On commence par définir une fonction (i.e. l'algorithme de reclassification)
#des clusters trop petits) qu'on va appeler dans la fonction principale
#(i.e. la fonction "posit_M7M")

#Cette fonction a été écrite pour être appliquée par la fonction "do()" 
#a un jeux de données qui a été regroupé par courbe (group_by(courbe)) et 
#qui a les colonnes "courbe", "AGE" (point d'attachement initiel
#avant le regroupement), "sumSupAttach" (la somme de la superficie du
#point d'attachement de la courbe correspondante) et supPour (le pourcentage
#de la superficie du point d'attachement de la courbe correspondante).
#
#Inputs:
#   - df: le jeu de données décrit ci-dessus
#   - nCluster: le maximum nombre de clusters
#   - supMinClust: la superficie minimale d'un cluster (en pourcentage
#   du groupe, pas en pourcentage de toute l'aire d'étude) 
#   
# Outputs: 
#   - Le même jeu de données défini par df oú "AGE" représente les points 
#   d'attachement initiaux et oú "clustFin" défine les clusters finaux (aprés 
#   le regroupement des clusters trop petits)  



algo_regroup_clust <- function(df, nCluster, supMinClust){
  
  #0. Traitement avant-boucle 
  #0.1 On crée la colonne qui va stocker les clusters finaux
  df <- 
    df %>% 
    mutate(clustFin = ageInit) %>% 
    select(courbe, traitement, clustFin, sumSupAttach, supPourc, ageInit)
  
  #0.2 Sélectionner la valeur de nCluster et de supMinClust selon le traitement
  nCluster <- nCluster[names(nCluster) %in% unique(df$traitement)]
  supMinClust <- supMinClust[names(supMinClust) %in% unique(df$traitement)]
  
  #0.3 Si on a des courbes qui n'ont pas un traitement valide, on arrête la 
  #fonction et les donne une valeur clustFin de NA
  
  if(!unique(df$traitement) %in% names(nCluster)){
    
    df <- 
      df %>% 
      transmute(courbe, ageInit, 
                clustFin = NA)
    return(df)
  }
  
  #0. Commencer la boucle "while". Cette boucle continue à rouler jusqu'à
  #que les conditions définies (dans ce cas: "nrow(df) > nCluster | 
  #min(df$supPourc) < supMinClust"; "|" veux dire "ou")
  #Pendant qu'on a plus que 5 clusters (nCluster) par groupe 
  #(i.e. plus que 5 valeurs uniques dans la colonne des clusters finaux)
  #ET pendant que le cluster le plus petit est plus petit que la valeur
  #minimale spécifiée par "supMinClust" (défaut = 10%)
  while(nrow(df) > nCluster | min(df$supPourc) < supMinClust){
    
    
    #1. "Break" de securité. Au cas où on a juste 1 ligne dans le groupe,
    #on finit la boucle
    if(nrow(df) == 1){break}  
    
    #2. On sélectionne le cluster le plus petit en ordenant tous les groupes
    #selon leur superficie (arrange) et en sélectionnant le premier (slice(1)) 
    petit <- 
      df %>% 
      arrange(supPourc) %>% 
      slice(1) %>% 
      select(clustFin) %>% #pour sélectionner la colonne du cluster
      unlist() %>% unname()     #comme un vecteur
    
    
    #3. Trouver le cluster le plus proche
    #3.1 Calculer la différence absolue (pas de +/-) entre ce cluster
    #et tous les autres
    altCluster <- 
      df %>% 
      mutate(diffCluster = abs(clustFin - petit)) %>% 
      
      #3.2 Ordonner le jeu de données selon la différence absolue
      arrange(diffCluster, desc(supPourc)) %>% 
      
      #3.3 Sélectionner le 2ième point (car le premier va être toujours
      #le cluster initielle parce que la différence va être 0)
      slice(2) %>% 
      
      #3.4 Et on sélectionne ce cluster d'attach
      select(clustFin) %>% unlist() %>% unname
    
    
    
    #4. Remplacer le cluster petit pour le nouveau cluster alternative plus gros
    df$clustFin <- ifelse(df$clustFin %in% petit,
                          altCluster,
                          df$clustFin)
    
    
    #5. Recalculer la superficie de chaque groupe
    df <- 
      df %>% group_by(courbe, traitement, clustFin) %>% 
      summarise(sumSupAttach = sum(sumSupAttach),
                supPourc = sum(supPourc)) %>% 
      ungroup()
    
    
    #6. Créer un dataframe où on va faire le lien entre le cluster original
    #et le cluster final où on va rattacher chaque polygone
    #6.1 Si l'objet n'existe pas, on le crée
    if(!exists("lienCluster")){
      
      lienCluster <- data.frame(ageInit = petit, #le cluster trop petit
                                
                                #le cluster alternative qui est plus gros
                                clustFin = altCluster) 
      
      #6.2 Si l'objet existe déjà (i.e. si c'est pas la première itération
      #de la boucle), on attache les nouveaux données au df qu'on a créé
      #lors de la première itération
    } else {
      
      #6.2.1 Attacher les données
      lienCluster <- 
        rbind(lienCluster,
              data.frame(ageInit = petit,
                         clustFin = altCluster))
      
      #6.2.2 Si un des clusters qui avait été considéré comme un 
      #cluster plus gros (i.e. un objet "altCluster") qui après est
      #considéré comme un cluster trop petit qu'il faut regrouper (i.e.
      #un objet "petit"), on a besoin de changer l'information dans la
      #table de lien entre les clusters d'attachement originaux et les gros
      #cluster d'attachement finaux
      if(petit %in% lienCluster$clustFin){
        
        lienCluster$clustFin <- ifelse(lienCluster$clustFin %in% petit,
                                       altCluster,
                                       lienCluster$clustFin)
      }
      
    }
  }  #Fin de la boucle "while"
  
  
  #7. Maintentant, pour faire le lien entre les clusters originaux et
  #les clusters finaux, on fait le lien entre le jeu de données principal
  #et la table "lienCluster" qui a cet information
  #On fait ça juste si on a regroupé des clusters (i.e. si le tableau
  #qui a cet information existe)
  if(exists("lienCluster")){
    
    #7.1 D'abord, on a besoin d'ajouter les valeurs "clustFin" aux
    #"ageInit": comme les gros clusters n'ont pas été remplacés, ils
    #ne sont pas présents dans la colonne ageInit. Par contre, comme
    #on veux utiliser cette colonne pour faire un join, on a besoin d'avoir
    #toutes les valeurs là dedans (pour pas avoir des valeurs manquantes)
    grosClusts <- 
      df %>% 
      distinct(clustFin) %>% 
      mutate(ageInit = clustFin) 
    
    
    #7.2 On ajoute les lignes pour les gros clusters
    lienCluster <- bind_rows(lienCluster, grosClusts)
    
    #7.3 Et on joindre les 2 jeux de données ensemble
    df <- 
      left_join(df, lienCluster, by = "clustFin") %>% 
      arrange(courbe, clustFin, ageInit)
    
    #7.4 Il faut supprimer cet objet pour eviter des bugs 
    rm(lienCluster)  
    
  }
  
  
  #8. Retourner les colonnes qu'on veut de l'objet principal
  df <- df %>% select(courbe, ageInit, clustFin)
  
  return(df)
  
}


############################################################################
############################################################################
#Voici la fonction principale!

posit_M7M <- function(cloneCourbes, 
                      cscpf, 
                      metaOri,
                      typeEcoToFamStat,
                      tabResumRegroupe,
                      gtyfToTyf,
                      clAgeToNum,
                      anneeDebut = substr(Sys.Date(), 1, 4),  #CHECK in HORIZON: automatically extract current year,
                      nCluster_Improd = 2,
                      nCluster_Plant = 4,
                      nCluster_EPC = 3,
                      nCluster_M7M = 7,
                      supMinClust_Improd = 10,
                      supMinClust_Plant = 20,
                      supMinClust_EPC = 20,
                      supMinClust_M7M = 20,
                      traiter_7MP_Plant = FALSE,
                      traiter_7MP_EPC = FALSE){ 
  
  #0. Charger les packages nécessaires
  require(dplyr)         #Traitement des données
  
  
  
  #################################################################
  #################################################################
  #################################################################
  #1. Vérifier que toutes les variables dont on a besoin sont la
  #1.1 Objet "cscpf" (jeu de données principal)
  #1.1.1 Identifier les variables nécessaires
  varsCscpf <- c("ID_BFEC", "GEOCODE_OR", "AN_ORIGINE", "CL_AGE", 
                 "IND_MAJ", "SUP_BRU", "SDOM_BIO", "TYPE_ECO", 
                 "GTYF_M7M", "COURBE_V", "DEC_CLASS", "Improd",
                 "TYPE_COURB", "ESS_RET", "PL",
                 "ORIGINE", "PERTURB", "HAUT_CONF") #hauteur confimée (M4M, 4MP-M7M et 7MP)
  
  #1.1.2 S'il y a au moins une variable manquante
  if(!all(varsCscpf %in% names(cscpf))){
    
    #On l'identifie
    varsManq <- varsCscpf[!varsCscpf %in% names(cscpf)]
    
    #Et on arrête la fonction
    stop("La ou les variables ", paste(varsManq, collapse = ", "),
         " ne sont pas présentes dans le jeu de données défini par 'cscpf'.",
         "Faites attention que cette fonction est sensible aux minuscules ",
         "et aux majuscules.")
  }
  
  
  #1.2 Objet "metaOri" (jeu de données principal)
  #1.2.1 Identifier les variables nécessaires
  varsMetaOri <- c("GEOCODE", "AN_PRO_ORI")
  
  #1.2.2 S'il y a au moins une variable manquante
  if(!all(varsMetaOri %in% names(metaOri))){
    
    #On l'identifie
    varsManq <- varsMetaOri[!varsMetaOri %in% names(metaOri)]
    
    #Et on arrête la fonction
    stop("La ou les variables ", paste(varsManq, collapse = ", "),
         " ne sont pas présentes dans le jeu de données défini par 'metaOri'.",
         "Faites attention que cette fonction est sensible aux minuscules ",
         "et aux majuscules.")
  }
  
  
  #1.3 Objet "clAgeToNum" (table de correspondance entre les classe d'âge
  #et leur valeur numériques correspondantes)
  #1.3.1 Identifier les variables nécessaires
  varsClAge <- c("CL_AGE", "REG_AGE")
  
  #1.3.2 S'il y a au moins une variable manquante
  if(!all(varsClAge %in% names(clAgeToNum))){
    
    #On l'identifie
    varsManq <- varsClAge[!varsClAge %in% names(clAgeToNum)]
    
    #Et on arrête la fonction
    stop("La ou les variables ", paste(varsManq, collapse = ", "),
         " ne sont pas présentes dans le jeu de données défini par 'clAgeToNum'.",
         "Faites attention que cette fonction est sensible aux minuscules ",
         "et aux majuscules.")
  }
  
  
  #1.4 Objet "cloneCourbes" (table de correspondance entre les classe d'âge
  #et leur valeur numériques correspondantes)
  #1.4.1 Identifier les variables nécessaires
  varsCourbes <- c("SDOM_BIO", "GR_STATION", "TYF", "CLASSE", 
                   "AGE", "VOL_HA", "DEC_CLASS", "NOM_FAMC",
                   "TYPE_COURB", "ESS_RET", "PL")
  
  #1.4.2 S'il y a au moins une variable manquante
  if(!all(varsCourbes %in% names(cloneCourbes))){
    
    #On l'identifie
    varsManq <- varsCourbes[!varsCourbes %in% names(cloneCourbes)]
    
    #Et on arrête la fonction
    stop("La ou les variables ", paste(varsManq, collapse = ", "),
         " ne sont pas présentes dans le jeu de données défini par 'cloneCourbes'.",
         "Faites attention que cette fonction est sensible aux minuscules ",
         "et aux majuscules.")
  }
  
  
  
  #1.5 Objet "tabResumRegroupe" (Tableau resumen avec le lien entre les 
  #GTYFs et GR_STATs et les GTYFs et GR_STATs regroupés (ce qui nous permet
  #de regrouper les groupes qui sont trop petits))
  #1.5.1 Identifier les variables nécessaires
  varsTabRegroupe <- c("SDOM_BIO", "GTYF_M7M", "GR_STAT", 
                       "GTYF_M7M_R", "GR_STAT_R")
  
  #1.5.2 S'il y a au moins une variable manquante
  if(!all(varsTabRegroupe %in% names(tabResumRegroupe))){
    
    #On l'identifie
    varsManq <- varsTabRegroupe[!varsTabRegroupe %in% 
                                  names(tabResumRegroupe)]
    
    #Et on arrête la fonction
    stop("La ou les variables ", paste(varsManq, collapse = ", "),
         " ne sont pas présentes dans le jeu de données défini par 'tabResumRegroupe'.",
         "Faites attention que cette fonction est sensible aux minuscules ",
         "et aux majuscules.")
  }
  
  
  #1.6 Objet "typeEcoToFamStat" (Tableau de correspondance entre le TYPE_ECO,
  #la Famille de station et le Groupe de station forestière)
  #1.6.1 Identifier les variables nécessaires
  varsTypeEco_GrStat <- c("SDOM_BIO", "TYPE_ECO", "FAM_STAT", "GR_STAT")
  
  #1.6.2 S'il y a au moins une variable manquante
  if(!all(varsTypeEco_GrStat %in% names(typeEcoToFamStat))){
    
    #On l'identifie
    varsManq <- varsTypeEco_GrStat[!varsTypeEco_GrStat %in% names(typeEcoToFamStat)]
    
    #Et on arrête la fonction
    stop("La ou les variables ", paste(varsManq, collapse = ", "),
         " ne sont pas présentes dans le jeu de données défini par 'typeEcoToFamStat'.",
         "Faites attention que cette fonction est sensible aux minuscules ",
         "et aux majuscules.")
  }
  
  
  #1.7 Objet "gtyfToTyf" (Tableau de correspondance entre lles GTYFs
  #et les TYFs)
  #1.7.1 Identifier les variables nécessaires
  varsGtyfToTyf <- c("SDOM_BIO", "GR_STAT_R", "GTYF_M7M_R", "TYF_M7M_R")
  
  #1.7.2 S'il y a au moins une variable manquante
  if(!all(varsGtyfToTyf %in% names(gtyfToTyf))){
    
    #On l'identifie
    varsManq <- varsGtyfToTyf[!varsGtyfToTyf %in% names(gtyfToTyf)]
    
    #Et on arrête la fonction
    stop("La ou les variables ", paste(varsManq, collapse = ", "),
         " ne sont pas présentes dans le jeu de données défini par 'gtyfToTyf'.",
         "Faites attention que cette fonction est sensible aux minuscules ",
         "et aux majuscules.")
  }
  
  
  
  #2. Traiter le jeu de données des polygones
  #2.1 Déterminer l'âge des peuplements dans l'année initielle (défaut = 2018)
  #Si on a l'année d'origine, on peut faire 2018 - AN_ORIGINE, 
  #sinon , il faut faire la classe d'âge  + (2018 - année de production des données)
  #2.1.1 Joindre les jeux de données
  procDon <- 
    left_join(cscpf %>% mutate(GEOCODE_OR = as.character(GEOCODE_OR)), 
              metaOri %>% transmute(GEOCODE = as.character(GEOCODE),
                                    AN_PRO_ORI = as.numeric(as.character(AN_PRO_ORI))), 
              by = c("GEOCODE_OR" = "GEOCODE"))
  
  #2.1.2 Remplir le champs AN_PRO_ORI quand il est vide
  procDon <- 
    procDon %>% 
    
    #Si l'année de production est NA et le jeu de données a été
    #mis à jour (IND_MAJ == "O"), on lui donne une valeur de 2013
    mutate(AN_PRO_ORI = 
             case_when(.$IND_MAJ %in% "O" & is.na(.$AN_PRO_ORI) ~ 2013,
                       
                       #Si l'année de production est NA et le jeu de données n'a pas 
                       #été mis à jour (IND_MAJ is NA), on lui donne une valeur de 2008
                       is.na(.$IND_MAJ) & is.na(.$AN_PRO_ORI) ~ 2008,
                       TRUE ~ .$AN_PRO_ORI))
  
  
  #2.2 Il faut ajouter la valeur numérique correspondante à chaque classe d'âge
  #2.2.1 On joindre les 2 jeux de données. On convert toutes les colonnes 
  #qu'on va utiliser dans des caractères pour éviter des messages 
  #d'avertissement inutiles
  anneeDebut <- as.numeric(anneeDebut)
  
  procDon <- 
    left_join(procDon %>% 
                mutate(CL_AGE = as.character(CL_AGE)),
              
              clAgeToNum %>% 
                transmute(CL_AGE = as.character(CL_AGE),
                          REG_AGE = as.numeric(as.character(REG_AGE))),
              by = "CL_AGE") %>% 
    
    
    #2.2.2 Si on a des valeurs NA, on les remplace par 0
    mutate(REG_AGE = ifelse(is.na(REG_AGE), 0, REG_AGE),
           AN_ORIGINE = as.numeric(as.character(AN_ORIGINE)),
           
           
           #2.3 Calculer l'âge des peuplements dans l'année initielle (défaut = 2018)
           ageInit = ifelse(is.na(AN_ORIGINE),
                            
                            #2.3.1 Si on n'a pas l'année d'origine, il faut calculer l'âge initielle 
                            #du peuplement comme la classe d'hauteur + (2018 - année de production 
                            #des données)                
                            REG_AGE + (anneeDebut - AN_PRO_ORI),
                            
                            #2.3.2 Si on a l'année d'origine, on peut faire 2018 - AN_ORIGINE, 
                            anneeDebut - AN_ORIGINE))
  
  
  #2.4 Ajouter les GTYFs et les GR_STAT regroupés (pour enlever des groupes
  #qui sont trop petits)
  #2.4.1 Ajouter les groupes de station
  procDon <- 
    left_join(procDon %>% mutate(TYPE_ECO = as.character(TYPE_ECO)), 
              typeEcoToFamStat %>% mutate_all(as.character),
              by = c("SDOM_BIO", "TYPE_ECO"))
  
  #2.4.2 Tester que toutes les combinaisons de GR_STAT et TYF existent
  #dans notre tableau de conversion
  grStat_tyf_manq <- 
    procDon %>% filter(!GTYF_M7M %in% c(NA, "NA", "na", "Na"))
  
  if(any(!paste(grStat_tyf_manq$GR_STAT, grStat_tyf_manq$GTYF_M7M) %in%
         paste(tabResumRegroupe$GR_STAT, tabResumRegroupe$GTYF_M7M))){
    
    grStat_tyf_manq <- 
      grStat_tyf_manq %>% 
      filter(!paste(GR_STAT, GTYF_M7M) %in%
               paste(tabResumRegroupe$GR_STAT, tabResumRegroupe$GTYF_M7M)) %>% 
      transmute(grStat_tyf_manq = paste(GR_STAT, GTYF_M7M)) %>% 
      distinct(grStat_tyf_manq) %>% unlist %>% unname
    
    warning("Les combinaisons de GR_STAT et TYF ", 
            paste(grStat_tyf_manq, collapse = ", "),
            " ne sont pas presentes dans le tableau de regroupement ",
            "des GR_STAT et TYF (argument 'tabResumRegroupe').")
    
  }
  
  #2.4.3 Ajouter les groupes de station regroupés
  procDon <- 
    left_join(procDon %>% 
                mutate(SDOM_BIO = as.character(SDOM_BIO),
                       GTYF_M7M = as.character(GTYF_M7M),
                       GR_STAT = as.character(GR_STAT)),
              tabResumRegroupe %>% mutate_all(as.character),
              by = c("SDOM_BIO", "GTYF_M7M", "GR_STAT")) %>%
    
    #Il faut enlever les peuplements qui n'ont pas de GTYF ni de GR_STAT
    filter(!is.na(GTYF_M7M_R), !is.na(GR_STAT_R))
  
  
  #2.5. Convertir les GTYFs en TYFs (pour qu'on puisse trouver une courbe équivalente
  #plus tard). Cette nouvelle variable va être la variable "TYF_M7M_R"
  procDon <- left_join(procDon, 
                       gtyfToTyf %>% mutate_all(as.character),
                       by = c("SDOM_BIO", "GR_STAT_R", "GTYF_M7M_R"))
    
    
  #2.6 Ajouter la classe de décalage au champs "courbe"
  #Il faut le faire d'une façon individuelle pour chaque groupe de donnees
  #(naturelle, plantation et EPC)
  #2.6.1 Plantations
  procDon_pl <-
    procDon %>% 
    filter(grepl("PL", TYPE_COURB) & !ESS_RET %in% c(NA, "NA", "na", "Na") &
             grepl("IQS", PL)) %>% 
    mutate(courbe = paste(TYPE_COURB, GR_STAT_R, ESS_RET, PL, sep = "_"))
    
  #2.6.2 EPC
  procDon_epc <- 
    procDon %>% filter(TYPE_COURB %in% "EPC" & 
                       !TYF %in% c(NA, "NA", "na", "Na")) %>% 
    mutate(courbe = paste(TYPE_COURB, SDOM_BIO, GR_STAT_R, TYF, sep = "_"))
  
  #2.6.3 Naturel
  procDon_nat <- 
    procDon %>% 
    filter(!ID_BFEC %in% c(procDon_pl$ID_BFEC, procDon_epc$ID_BFEC)) %>% 
    mutate(TYPE_COURB = "A") %>% 
    mutate(courbe = paste(SDOM_BIO, GR_STAT_R, TYF_M7M_R, 
                          "NA", COURBE_V, DEC_CLASS, 
                          sep = "_"))
  
  #2.6.4 Rejoindre les 3 jeux de donnees (probablem c'est pas la meilleure 
  #façon de faire ça, mais tant pis)
  procDon <- bind_rows(procDon_nat, procDon_pl, procDon_epc)
  
  
  #2.7 Créer la colonne des traitements (Improd, PL, EPC ou M7M).
  #Pour chaque groupe on va donner des seuils de nombre de clusters
  #et des superficies minimales différentes
  procDon <- 
    procDon %>% 
    mutate(traitement =
             
             #2.7.1 Les Improds avec M7M
             case_when(.$Improd %in% "SNAT" & 
                         .$HAUT_CONF %in% c("4MP-M7M", "M4M") ~ "SNAT",
                       
                       #2.7.2 Les plantations (avec moins ou plus de 7M)
                       .$TYPE_COURB %in% c("PL16", "PL20") ~ "PL",
                       
                       #2.7.3 Les EPCs (avec moins ou plus de 7M)
                       .$TYPE_COURB %in% "EPC" ~ "EPC",
                       
                       #2.7.4 Les autres peuplements M7M
                       .$HAUT_CONF %in% c("4MP-M7M", "M4M") ~ "M7M",
                       TRUE ~ "7MP"))
  
  
  #2.8 Déterminer les polygones qu'on veux traiter selon les paramètres
  #d'entré. Est-ce qu'on veux traiter les peuplements 7MP qui sont des 
  #plantations et des EPCs? Les variables traiter_7MP_Plant et 
  #traiter_7MP_EPC sont des variables logiques (TRUE/FALSE)
  procDon <- 
    procDon %>% 
    mutate(polysTraiter = 
             case_when(
               
               #2.8.1 Si c'est PL, 7MP et on veux traiter des plantations 7MP
               .$traitement %in% "PL" & .$HAUT_CONF %in% "7MP" &
                 traiter_7MP_Plant ~ TRUE,
               
               #2.8.2 Si c'est EPC, 7MP et on veux traiter des EPC 7MP               
               .$traitement %in% "EPC" & .$HAUT_CONF %in% "7MP" &
                 traiter_7MP_EPC ~ TRUE, 
               
               #2.8.3 Si c'est PL, 7MP et on NE VEUX PAS traiter des plantations 7MP
               .$traitement %in% "PL" & .$HAUT_CONF %in% "7MP" &
                 !traiter_7MP_Plant ~ FALSE,
               
               #2.8.4 Si c'est EPC, 7MP et on NE VEUX PAS traiter des EPC 7MP
               .$traitement %in% "EPC" & .$HAUT_CONF %in% "7MP" &
                 !traiter_7MP_EPC ~ FALSE,
               
               #2.8.5 Si ce sont d'autres traitements 7MP, on ne veut pas les traiter 
               .$traitement %in% "7MP" ~ FALSE, 
               
               #2.8.6 Si ce sont des traitements M7M, on veux les traiter
               TRUE ~ TRUE))
  
  
  
  #3. Processer les données du catalogue de courbes
  #3.0 Calculer le champs GE pour chaque grpi+e
  cloneCourbes <- 
    cloneCourbes %>% 
    mutate(GE = 
             case_when(.$TYPE_COURB %in% "A" ~ 
                            paste(.$SDOM_BIO, .$GR_STATION, .$TYF, "NA", 
                                  .$CLASSE, .$DEC_CLASS, sep = "_"),
                       grepl("PL", .$TYPE_COURB) ~
                         paste(.$TYPE_COURB, .$GR_STATION, .$ESS_RET, .$PL, 
                               sep = "_"),
                       .$TYPE_COURB %in% "EPC" ~
                         paste(.$TYPE_COURB, .$SDOM_BIO, .$GR_STATION, .$TYF, 
                               sep = "_"))
    )
  
  
  #3.1 Vérifier que toutes les courbes dont on a besoin sont dans notre 
  #jeu de données et faire un avertissement s'ils nous manquent des courbes
  courbsManq <- 
    unique(procDon$courbe) %in% cloneCourbes$GE
  
  if(!all(courbsManq)){
    
    courbsManq <- unique(procDon$courbe)[!courbsManq]
    
    warning("Les courbes suivantes ne sont pas disponibles dans le ",
            "catalogue de courbes: ", paste(courbsManq, collapse = ", ") )
    
  }
  
  
  #3.2 Sélectionner seulement les courbes qui sont dans notre jeu de données 
  cloneCourbes <- 
    cloneCourbes %>% filter(GE %in% unique(procDon$courbe))
  
  
  #3.3 Calculer le volume total par age (somme de tous
  #les valeurs regroupés par ge et age; on avait le volume
  #de plusieurs essences)
  regCloneCourbes <- 
    cloneCourbes %>% 
    group_by(GE, AGE) %>% 
    summarise(NOM_FAMC = unique(NOM_FAMC)[1],
              vol_tot = sum(VOL_HA)) %>% 
    
    #3.3 Sélectionner seulement le côté gauche (i.e. croissance) des 
    #courbes en sélectionnant toutes les observations (de chaque GE)
    #entre la première et l'age qui a le volume le plus gros
    group_by(GE) %>% 
    slice(1:which(vol_tot == max(vol_tot))[1]) %>% 
    ungroup()
  
  
  
  #4. Calculer la superficie des groupes
  #4.1 Faire l'objet qui regroupe les seuils de nombre de cluster et
  #de superficie minimale par traitement
  nCluster <- c("SNAT" = nCluster_Improd, "PL" = nCluster_Plant,
                "EPC" = nCluster_EPC, "M7M" = nCluster_M7M)
  supMinClust <- c("SNAT" = supMinClust_Improd, "PL" = supMinClust_Plant,
                   "EPC" = supMinClust_EPC, "M7M" = supMinClust_M7M)
  
  
  #4.2 Calculer la superficie occupée par chaque point d'attachement 
  #(somme des combinaisons courbe + point d'attachement [ageInit])
  regroupClusters <- 
    procDon %>% 
    group_by(courbe, traitement, ageInit) %>% 
    summarize(sumSupAttach = sum(SUP_BRU)) %>% 
    
    #4.2 Calculer le pourcentage de la superficie de chaque point d'attachement 
    #dans le groupe (donc, somme de la pourcentage dans chaque groupe
    #sera 100%)
    group_by(courbe, traitement) %>% 
    mutate(supPourc = round(sumSupAttach / sum(sumSupAttach) * 100, 2)) %>% 
    
    
    #4.3 Appliquer la fonction qu'on avait écrit A CHAQUE GROUPE. Cette
    #fonction va trouver les point d'attachement les plus proches des 
    #point d'attachement qui sont trop petits. Par défaut, on a un maximum de 
    #5 point d'attachement par courbe et chaque point d'attachement doit occuper
    #au moins 10% de la superficie du groupe.
    #L'output va être un dataframe que fait le lien entre le point 
    #d'attachement original (oriCluster; i.e. cluster initiel avant le 
    #regroupement) et le point d'attachement final proposé (clustFin) pour
    #chaque groupe. On a laissé aussi le pourcentage de superficie que
  #chaque point d'attachement final occupe, mais cette colonne a des
  #doublons (il fallait pour qu'on puisse avoir le lien entre les
  #points d'attachement initiaux et les points d'attachement finaux)
  do(algo_regroup_clust(df = ., 
                        nCluster = nCluster, 
                        supMinClust = supMinClust))
  
  
  #4.4 Joindre le jeu de de données avec le cluster final au jeu de données
  #principal
  procDon <- left_join(procDon, regroupClusters,
                       by = c("courbe", "ageInit", "traitement"))
  
  

  #5. Trouver le point d'attachement le plus proche de chaque point 
  #d'attachement finale (ceux qu'on vien de calculer)
  #5.1 Sélectionner les clusters finaux de chaque "courbe" (en enlevant
  #tous les doublons)
  pointsAttach <-
    procDon %>% 
    distinct(courbe, clustFin) %>%
    
    #5.2 Faire le join du jeu de données avec le catalogue de courbes. Après
    #le join on va avoir un gros jeux de données, et pour chaque point 
    #d'attachement final (clustFin) on va avoir plusieurs lignes avec tous 
    #les points d'attachement possibles pour chaque courbe
    mutate(courbe = as.character(courbe),
           clustFin = as.numeric(as.character(clustFin))) %>% 
    left_join(regCloneCourbes %>% 
                transmute(courbe = as.character(GE), 
                          NOM_FAMC = as.character(NOM_FAMC),
                          AGE = as.numeric(as.character(AGE))),
              by = "courbe") 
  
  #5.3 Maintenant on calcule la différence entre l'âge de chaque polygone
  #et tous les points d'attachement. On va calculer la valeur absolute (i.e.
  #en ignorant si la différence est positive ou negative)
  pointsAttach <- 
    pointsAttach %>% 
    mutate(diffAge = abs(clustFin - AGE)) %>% 
    
    #5.4 On groupe les données par courbe et point d'attachement final (clustFin)
    #pour les ordonner selon la différence d'âge (dans chaque groupe) et choisir 
    #le prémier élement (i.e. le point d'attachement de la courbe pour lequel l
    #a différence est la plus petite)
    group_by(courbe, clustFin) %>% 
    arrange(diffAge) %>% 
    slice(1) %>%  #comme c'est regroupé, slice sélectionne la première 
    #observation de chaque groupe 
    ungroup() %>% 
    
    #5.5 On n'a plus besoin de la différence d'âge
    select(courbe, clustFin, AGE, NOM_FAMC)
  
  
  
  #6. Mettre les points 4 et 5 ensemble: on va maintenant joindre les 2 
  #jeux de données de façon à définir le point d'attachement de la courbe
  #(pointsAttach$AGE) pour chaque polygone (procDon$clustFin)
  #6.1 Joindre les 2 jeux de données
  output <- 
    left_join(procDon %>% select(ID_BFEC, courbe, clustFin, 
                                 HAUT_CONF, traitement, polysTraiter),
              pointsAttach, by = c("courbe", "clustFin")) %>% 
    
    #6.2 Vérifier qu'on fait pas des positionnements pour les polygones 
    #qu'on ne veut pas (ça ne devrait pas arriver, mais en tout cas...)
    mutate(AGE = ifelse(polysTraiter %in% FALSE, NA,
                        AGE)) %>% 
    
    #6.3 Sélectionner les 3 colonnes qu'on veut
    transmute(ID_BFEC = as.character(ID_BFEC), 
              GE_M7M = courbe,
              NOM_FAMC = NOM_FAMC,
              HAUT_CONF = HAUT_CONF,
              traitement = traitement,
              pointAttach = AGE) %>% 
    as.data.frame()
  
  
  #6.3 Finir la fonction
  return(output)
  
}

