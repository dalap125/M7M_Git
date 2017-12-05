#Script2. Regroupement des polygones dans des groupes évolutifs

#mainDir <- file.path("T:\\Donnees\\Courant\\Projets\\Chantier_M7M", 
#                     "Script_Plusieurs_SDOMs\\M7M")

#Inputs:
#   1. cscpf: jeu de données oé chaque ligne réprésente un polygone 
# (comme dans un fichier PEE_ORI) avec les colonnes: 
#         - GR_STATION: groupe de stations forestiéres
#         - HAUTEUR: numérique; ceci va étre convertit en "M7M" ou "P7M"
#         - SDOM_BIO
#         - ORIGINE: les classes d'origine. Elles seront simplifiées
#         en CT, CPR et NAT.
#         - GTYF_M7M: crée par le script 1 de random forest que prédit
#          les GTYFs des peuplements M7M
#         - COURBE_V: classe de volume prédite par le méme script ci-dessus
#         - SUP_BRU: la superficie de chaque polygone
#         
#   peeOriTyf <- read_csv(file.path(mainDir, "Test outputs", "outputScript_1.csv"))
#      
#         
#   2. typeEcoToFamStat : table de pilotage pour déterminer la famille de station 
#     selon le sous-domaine (SDOM_BIO) et le type eco (TYPE_ECO). 
#     Le fichier spécifié doit étre un csv
#     Ex. typeEcoToFamStat = read.csv2(file.path(mainDir, "Inputs\\TypeEco_To_Fam_Stat_par_SDOM.csv"))

#     
#   3. tabResumRegroupe : tableau de regroupement des GR_STATs et des GTYFs. 
#   Ce tableau de correspondance sert é regrouper des combinaisons de 
#   GR_STATs et GTYFs marginales. Par example, pour la 6O ce tableau convert 
#   plus de 60 groupes en 11. Ce tableau doit avoir les valeurs pours tous les 
#   sous-domaines.
#   Ex. testTabResumRegroupe <- read.csv2(file.path(mainDir, "Inputs", 
#                                         "Tab_resumen_GTYF_par_SDOM.csv"))

#   
#   4. classDecal: classe de décalage estimée pour chaque groupe évolutif
#   (Ex. classDecal <- read.csv(file.path(mainDir, "Inputs", 
#                                 "Classes de decalage par SDom.csv")))
#
#     
#   5. gtyfToTyf: tableau de conversion des GTYFs en TYFs pour nous aider é trouver
#   une courbe correspondante pour chaque GTYF. Ce tableau doit avoir des valeurs
#   de TYFs correspondantes é chaque combinaison SDOM_BIO, GR_STAT et GTYF
#   (Ex. gtyfToTyf <- read.csv2(file.path(mainDir, "Inputs",
#                                   "Pilotage_courbes_GRSTAT_GTYF_to_TYF.csv")))
#         

#   6. cloneCourbes: catalogue des courbes décalées sur lesquels on va positionner
#   les polygones M7M
#   (Ex. cloneCourbes <- read_csv(file.path(mainDir, "Inputs", 
#                                     "Courbes_6ouest_decal.csv")))
#                                     
#   7. clAgeToNum: tableau de conversion des classes d'ége dans des valeurs
#   numériques d'ége (e.g. CL_AGE 50 == 50)
#   (Ex. testClAgeToNum <- read.csv2(file.path(mainDir, "Inputs", 
#                       "classe_age_to_numerique.csv"), stringsAsFactors = FALSE))

#  8. propSupGrosGroupes: superficie maximale cumulative occupée par les groupes 
#   qu'il faut diviser en 2 (courbes v12 et 34). Par example un valeur de 50
#   veux dire que les strates dont la somme de la superficie occupe au moins 50% 
#   du térritoire seront divisées en 2 (courbe v12 et v34)
#   (Ex. propSupGrosGroupes = 50)


#Voici la structure de cette fonction:
#   1. Vérifier que toutes les variables et objets dont on a besoin sont disponibles
#   
#   2. Ajouter les GTYFs et les GR_STAT regroupés en utilisant le tableau de 
#   conversién faite par Franéois avec l'extrant de la prémiére fonction. Ceci sert
#   é regrouper des groupes trop petits
#   
#   3. Calculer la somme cummulative des superficies de tous les groupes (du plus gros
#   au plus petit)
#   
#   4. Déterminer quels groupes il faut diviser en 2 (courbes v12 et 34) en utilisant
#   le seuil de superficie cumulative défini (défaut = 50%)
#   
#   5. Convertir les GTYFs en TYFs avec le tableau de conversion "gtyfToTyf"
#   
#   6. Déterminer quelle courbeV utiliser (quand on a qu'une courbe dans la strate)
#   
#   7. Ajouter la classe de décalage correspondante é chaque courbe
#   
#   8. Créer la colonne de la courbe finale correspondante
#   
#   9. Sélectionner les colonnes qu'on veut sortir


GE_M7M_ClassVol_Decal <- 
  function(cscpf, 
           typeEcoToFamStat,
           tabResumRegroupe,
           classDecal,
           gtyfToTyf,
           cloneCourbes,
           clAgeToNum,
           propSupGrosGroupes = 50){
    
    
    #0. Vérifier que les packages dont on a besoin sont chargés
    require(dplyr)
    
    
    #1. Vérifier que toutes les variables dont on a besoin sont la
    #1.1 Objet "cscpf" (jeu de données principal)
    #1.1.1 Identifier les variables nécessaires
    varsCscpf <- c("ID_BFEC", "GEOCODE_OR", "SDOM_BIO", "TYPE_ECO", 
                   "GTYF_M7M", "COURBE_V", 
                   "SUP_BRU", "HAUTEUR", "CL_AGE", "AN_ORIGINE")
    
    #1.1.2 S'il y a au moins une variable manquante
    if(!all(varsCscpf %in% names(cscpf))){
      
      #On l'identifie
      varsManq <- varsCscpf[!varsCscpf %in% names(cscpf)]
      
      #Et on arréte la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas présentes dans le jeu de données défini par 'cscpf'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    #1.2 Objet "typeEcoToFamStat" (Tableau de correspondance entre le TYPE_ECO,
    #la Famille de station et le Groupe de station forestiére)
    #1.2.1 Identifier les variables nécessaires
    varsTypeEco_GrStat <- c("TYPE_ECO", "FAM_STAT", "GR_STAT")
    
    #1.2.2 S'il y a au moins une variable manquante
    if(!all(varsTypeEco_GrStat %in% names(typeEcoToFamStat))){
      
      #On l'identifie
      varsManq <- varsTypeEco_GrStat[!varsTypeEco_GrStat %in% names(typeEcoToFamStat)]
      
      #Et on arréte la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas présentes dans le jeu de données défini par 'typeEcoToFamStat'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    #1.3 Objet "tabResumRegroupe" (Tableau resumen avec le lien entre les 
    #GTYFs et GR_STATs et les GTYFs et GR_STATs regroupés (ce qui nous permet
    #de regrouper les groupes qui sont trop petits))
    #1.3.1 Identifier les variables nécessaires
    varsTabRegroupe <- c("SDOM_BIO", "GTYF_M7M", "GR_STAT", 
                         "GTYF_M7M_R", "GR_STAT_R")
    
    #1.3.2 S'il y a au moins une variable manquante
    if(!all(varsTabRegroupe %in% names(tabResumRegroupe))){
      
      #On l'identifie
      varsManq <- varsTabRegroupe[!varsTabRegroupe %in% 
                                    names(tabResumRegroupe)]
      
      #Et on arréte la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas présentes dans le jeu de données défini par 'tabResumRegroupe'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    #1.4 Objet "classDecal" (Tableau des quantiles de classe de décalage)
    #1.4.1 Identifier les variables nécessaires
    varsClassDecal <- c("SDOM_BIO", "GR_STAT_R", "TYF_M7M_R", "courbeV", "DEC_CLASS")
    
    #1.4.2 S'il y a au moins une variable manquante
    if(!all(varsClassDecal %in% names(classDecal))){
      
      #On l'identifie
      varsManq <- varsClassDecal[!varsClassDecal %in% 
                                   names(classDecal)]
      
      #Et on arréte la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas présentes dans le jeu de données défini par 'classDecal'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    #1.5 Objet "gtyfToTyf" (Tableau de conversion des GTYFs en TYFs)
    #1.5.1 Identifier les variables nécessaires
    varsGtyfToTyf <- c("SDOM_BIO", "GR_STAT_R", "GTYF_M7M_R", "TYF_M7M_R")
    
    #1.5.2 S'il y a au moins une variable manquante
    if(!all(varsGtyfToTyf %in% names(gtyfToTyf))){
      
      #On l'identifie
      varsManq <- varsGtyfToTyf[!varsGtyfToTyf %in% names(gtyfToTyf)]
      
      #Et on arréte la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas présentes dans le jeu de données défini par 'gtyfToTyf'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    #1.6 Objet "cloneCourbes" (Tableau de conversion des GTYFs en TYFs)
    #1.6.1 Identifier les variables nécessaires
    varsCloneCourbes <- c("SDOM_BIO", "GR_STATION", "TYF", "CLASSE", 
                          "AGE", "HAUT_MOY", "EXTRAPOL", "DEC_CLASS")
    
    #1.6.2 S'il y a au moins une variable manquante
    if(!all(varsCloneCourbes %in% names(cloneCourbes))){
      
      #On l'identifie
      varsManq <- varsCloneCourbes[!varsCloneCourbes %in% names(cloneCourbes)]
      
      #Et on arréte la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas présentes dans le jeu de données défini par 'cloneCourbes'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    #1.7 Objet "clAgeToNum" (table de correspondance entre les classe d'ége
    #et leur valeur numériques correspondantes)
    #1.7.1 Identifier les variables nécessaires
    varsClAge <- c("CL_AGE", "REG_AGE")
    
    #1.7.2 S'il y a au moins une variable manquante
    if(!all(varsClAge %in% names(clAgeToNum))){
      
      #On l'identifie
      varsManq <- varsClAge[!varsClAge %in% names(clAgeToNum)]
      
      #Et on arréte la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas présentes dans le jeu de données défini par 'clAgeToNum'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    

    #2. Ajouter les GTYFs et les GR_STAT regroupés
    #2.1 Ajouter le groupe de station
    cscpf <- 
      left_join(cscpf %>% mutate(SDOM_BIO = as.character(SDOM_BIO),
                                 TYPE_ECO = as.character(TYPE_ECO)), 
                typeEcoToFamStat %>% mutate(SDOM_BIO = as.character(SDOM_BIO),
                                            TYPE_ECO = as.character(TYPE_ECO)),
                by = c("SDOM_BIO", "TYPE_ECO"))
    

    #2.2 Tester que toutes les combinaisons de GR_STAT et TYF existent
    #dans notre tableau de conversion
    grStat_tyf_manq <- 
      cscpf %>% filter(!GTYF_M7M %in% c(NA, "NA", "na", "Na"))
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
    
    
    #2.3 Ajouter les GTYFs et les GR_STAT regroupés (pour enlever des groupes
    #qui sont trop petits)
    #Il faut enlever les peuplements qui n'ont pas de GTYF ni de GR_STAT
    cscpf <- 
      left_join(cscpf %>% 
                  mutate(SDOM_BIO = as.character(SDOM_BIO),
                         GTYF_M7M = as.character(GTYF_M7M),
                         GR_STAT = as.character(GR_STAT)),
                tabResumRegroupe %>% 
                  transmute(SDOM_BIO = as.character(SDOM_BIO),
                            GTYF_M7M = as.character(GTYF_M7M),
                            GR_STAT = as.character(GR_STAT),
                            GTYF_M7M_R = as.character(GTYF_M7M_R),
                            GR_STAT_R = as.character(GR_STAT_R)),
                by = c("SDOM_BIO", "GTYF_M7M", "GR_STAT")) %>%
      
      filter(!is.na(GTYF_M7M_R), !is.na(GR_STAT_R))
    
    
    
    #3. Calculer la somme cummulative des superficies des groupes regroupés
    #3.1 Regrouper les données par GTYF et GR_STAT regroupés.
    #Selon Franéois on n'a pas besoin d'inclure le sous-domaine dans les
    #groupes. éa va étre fait aprés quand on décide la courbe é utiliser
    sumSup <- 
      cscpf %>% 
      group_by(SDOM_BIO, GTYF_M7M_R, GR_STAT_R) %>% 
      
      #3.2 Calculer la superficie totale de chaque groupe
      summarise(supTot = sum(SUP_BRU)) %>%
      
      #3.3 Mettre les en order decroissante de superficie
      ungroup %>% 
      arrange(desc(supTot)) %>% 
      
      #3.4 Calculer la somme cummulative et la pourcentage
      mutate(cumSup = cumsum(supTot),
             percCumSup = (cumSup/sum(supTot))*100)
    
    
    
    #4. Déterminer quels groupes il faut diviser en 2 (courbes v12 et 34).
    #On va définir un seuil de superficie cumulative (défaut = 50%). éa veut
    #dire que les strates dont la somme de la superficie occupe 50% du 
    #térritoire seront divisées en 2 (courbe v12 et v34)
    #4.1 Déterminer les strates qu'on va diviser en 2
    sumSup <- 
      sumSup %>%  
      mutate(nombreGE = ifelse(percCumSup > propSupGrosGroupes, 1, 2))
    
    
    #4.2 Ajouter le nombre de GEs par groupe au jeu de données principal
    #On sélectionne les colonnes qu'on veut de sumSup avant de faire le join
    #pour ajouter seulement la colonne qu'on veut (nombreGE)
    cscpf <- 
      left_join(cscpf, 
                sumSup %>% select(SDOM_BIO, GTYF_M7M_R, GR_STAT_R, nombreGE),
                by = c("SDOM_BIO", "GTYF_M7M_R", "GR_STAT_R"))
    
    
    
    #5. Convertir les GTYFs en TYFs (pour qu'on puisse trouver une courbe équivalente
    #plus tard). Cette nouvelle variable va s'appeler "TYF_M7M_R"
    cscpf <- left_join(x = cscpf, 
                       y = gtyfToTyf %>% 
                         mutate_all(as.character),
                       by = c("SDOM_BIO", "GR_STAT_R", "GTYF_M7M_R"))   
    
    
    
    #6. Déterminer quel courbe V utiliser: la v12/v34 si on divisde le groupe
    #en 2 ou la NA
    cscpf <- 
      cscpf %>% 
      mutate(COURBE_V_R = ifelse(nombreGE %in% 2, 
                                 yes = as.character(COURBE_V),
                                 no = NA))
    

    
    #7. Ajouter la classe de décalage correspondante é chaque courbe
    #7.1 Sélectionner les variables dont on a besoin et les transformer
    #dans des caractéres pour éviter des avertissements inutiles
    classDecal <- 
      classDecal %>% 
      select(SDOM_BIO, GR_STAT_R, TYF_M7M_R, courbeV, DEC_CLASS) %>% 
      mutate_all(as.character)
    
    #7.2 Faire le join
    cscpf <- left_join(cscpf, classDecal,
                       by = c("SDOM_BIO", "GR_STAT_R", "TYF_M7M_R", 
                              "COURBE_V_R" = "courbeV"))
    
    
    
    #8. Créer la colonne de la courbe finale correspondante
    #8.1 Créer la colonne
    #Il faut faire un ifelse selon le type de courbe pour pas ajouter
    #2 NA é la fin quand COURBE_V_R est NA
    cscpf <- 
      cscpf %>% 
      mutate(courbe = ifelse(COURBE_V_R %in% NA,
                             paste(SDOM_BIO, GR_STAT_R, TYF_M7M_R, 
                                   "NA", sep = "_"),
                             paste(SDOM_BIO, GR_STAT_R, TYF_M7M_R, 
                                      "NA", COURBE_V_R, sep = "_")))
    
    
    #8.2 Générer le champs concatene de la courbe pour le catalogue de courbes
    if(!"courbe" %in% names(cloneCourbes)){
      
      cloneCourbes <- 
        cloneCourbes %>% 
        mutate(courbe = ifelse(CLASSE %in% c(NA, "NA", "na", "Na"),
                               paste(SDOM_BIO, GR_STATION, TYF, 
                                     "NA", sep = "_"),
                               paste(SDOM_BIO, GR_STATION, TYF, 
                                     "NA", CLASSE, sep = "_")))
    }
    
    
    
    #9. Ajouter le NOM_FAMC du catalogue des clones des courbes
    #9.1 Sélectionner les colonnes qu'on veut
    nomFam <-
      cloneCourbes %>%
      select(courbe, DEC_CLASS, NOM_FAMC) %>%
      distinct()
    
    
    #9.2 Donner un avertissement si on a des courbes dans le jeu de donnees
    #qui n'existent pas dans le catalogue
    if(any(!cscpf$courbe %in% nomFam$courbe)){
      
      courbesManq <- 
        cscpf %>% 
        filter(!courbe %in% nomFam$courbe) %>% 
        distinct(courbe) %>% unlist %>% unname %>% as.character()
      
      warning("Les courbes ", paste(courbesManq, collapse = ", "),
              " n'existent pas dans le catalogue de courbes.")
      
      
    }
    

    #9.3 Faire le join
    cscpf <- left_join(cscpf, nomFam, by = c("courbe", "DEC_CLASS"))
    
    
    
    
    #9. Sélectionner les colonnes qu'on veut sortir
    output <- 
      cscpf %>% distinct(ID_BFEC, courbe, NOM_FAMC, DEC_CLASS) 
    
    
    return(as.data.frame(output))
    
    
  }##### Fin de la fonction
