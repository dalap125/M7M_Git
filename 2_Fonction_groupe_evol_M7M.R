#Script2. Regroupement des polygones dans des groupes �volutifs

#mainDir <- file.path("T:\\Donnees\\Courant\\Projets\\Chantier_M7M", 
#                     "Script_Plusieurs_SDOMs\\M7M")

#Inputs:
#   1. cscpf: jeu de donn�es o� chaque ligne r�pr�sente un polygone 
# (comme dans un fichier PEE_ORI) avec les colonnes: 
#         - GR_STATION: groupe de stations foresti�res
#         - HAUTEUR: num�rique; ceci va �tre convertit en "M7M" ou "P7M"
#         - SDOM_BIO
#         - ORIGINE: les classes d'origine. Elles seront simplifi�es
#         en CT, CPR et NAT.
#         - GTYF_M7M: cr�e par le script 1 de random forest que pr�dit
#          les GTYFs des peuplements M7M
#         - COURBE_V: classe de volume pr�dite par le m�me script ci-dessus
#         - SUP_BRU: la superficie de chaque polygone
#         
#   peeOriTyf <- read_csv(file.path(mainDir, "Test outputs", "outputScript_1.csv"))
#      
#         
#   2. typeEcoToFamStat : table de pilotage pour d�terminer la famille de station 
#     selon le sous-domaine (SDOM_BIO) et le type eco (TYPE_ECO). 
#     Le fichier sp�cifi� doit �tre un csv
#     Ex. typeEcoToFamStat = read.csv2(file.path(mainDir, "Inputs\\TypeEco_To_Fam_Stat_par_SDOM.csv"))

#     
#   3. tabResumRegroupe : tableau de regroupement des GR_STATs et des GTYFs. 
#   Ce tableau de correspondance sert � regrouper des combinaisons de 
#   GR_STATs et GTYFs marginales. Par example, pour la 6O ce tableau convert 
#   plus de 60 groupes en 11. Ce tableau doit avoir les valeurs pours tous les 
#   sous-domaines.
#   Ex. testTabResumRegroupe <- read.csv2(file.path(mainDir, "Inputs", 
#                                         "Tab_resumen_GTYF_par_SDOM.csv"))

#   
#   4. classDecal: classe de d�calage estim�e pour chaque groupe �volutif
#   (Ex. classDecal <- read.csv(file.path(mainDir, "Inputs", 
#                                 "Classes de decalage par SDom.csv")))
#
#     
#   5. gtyfToTyf: tableau de conversion des GTYFs en TYFs pour nous aider � trouver
#   une courbe correspondante pour chaque GTYF. Ce tableau doit avoir des valeurs
#   de TYFs correspondantes � chaque combinaison SDOM_BIO, GR_STAT et GTYF
#   (Ex. gtyfToTyf <- read.csv2(file.path(mainDir, "Inputs",
#                                   "Pilotage_courbes_GRSTAT_GTYF_to_TYF.csv")))
#         

#   6. cloneCourbes: catalogue des courbes d�cal�es sur lesquels on va positionner
#   les polygones M7M
#   (Ex. cloneCourbes <- read_csv(file.path(mainDir, "Inputs", 
#                                     "Courbes_6ouest_decal.csv")))
#                                     
#   7. clAgeToNum: tableau de conversion des classes d'�ge dans des valeurs
#   num�riques d'�ge (e.g. CL_AGE 50 == 50)
#   (Ex. testClAgeToNum <- read.csv2(file.path(mainDir, "Inputs", 
#                       "classe_age_to_numerique.csv"), stringsAsFactors = FALSE))

#  8. propSupGrosGroupes: superficie maximale cumulative occup�e par les groupes 
#   qu'il faut diviser en 2 (courbes v12 et 34). Par example un valeur de 50
#   veux dire que les strates dont la somme de la superficie occupe au moins 50% 
#   du t�rritoire seront divis�es en 2 (courbe v12 et v34)
#   (Ex. propSupGrosGroupes = 50)


#Voici la structure de cette fonction:
#   1. V�rifier que toutes les variables et objets dont on a besoin sont disponibles
#   
#   2. Ajouter les GTYFs et les GR_STAT regroup�s en utilisant le tableau de 
#   conversi�n faite par Fran�ois avec l'extrant de la pr�mi�re fonction. Ceci sert
#   � regrouper des groupes trop petits
#   
#   3. Calculer la somme cummulative des superficies de tous les groupes (du plus gros
#   au plus petit)
#   
#   4. D�terminer quels groupes il faut diviser en 2 (courbes v12 et 34) en utilisant
#   le seuil de superficie cumulative d�fini (d�faut = 50%)
#   
#   5. Convertir les GTYFs en TYFs avec le tableau de conversion "gtyfToTyf"
#   
#   6. D�terminer quelle courbeV utiliser (quand on a qu'une courbe dans la strate)
#   
#   7. Ajouter la classe de d�calage correspondante � chaque courbe
#   
#   8. Cr�er la colonne de la courbe finale correspondante
#   
#   9. S�lectionner les colonnes qu'on veut sortir


GE_M7M_ClassVol_Decal <- 
  function(cscpf, 
           typeEcoToFamStat,
           tabResumRegroupe,
           classDecal,
           gtyfToTyf,
           cloneCourbes,
           clAgeToNum,
           propSupGrosGroupes = 50){
    
    
    #0. V�rifier que les packages dont on a besoin sont charg�s
    require(dplyr)
    
    
    #1. V�rifier que toutes les variables dont on a besoin sont la
    #1.1 Objet "cscpf" (jeu de donn�es principal)
    #1.1.1 Identifier les variables n�cessaires
    varsCscpf <- c("ID_BFEC", "GEOCODE_OR", "SDOM_BIO", "TYPE_ECO", 
                   "GTYF_M7M", "COURBE_V", 
                   "SUP_BRU", "HAUTEUR", "CL_AGE", "AN_ORIGINE")
    
    #1.1.2 S'il y a au moins une variable manquante
    if(!all(varsCscpf %in% names(cscpf))){
      
      #On l'identifie
      varsManq <- varsCscpf[!varsCscpf %in% names(cscpf)]
      
      #Et on arr�te la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas pr�sentes dans le jeu de donn�es d�fini par 'cscpf'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    #1.2 Objet "typeEcoToFamStat" (Tableau de correspondance entre le TYPE_ECO,
    #la Famille de station et le Groupe de station foresti�re)
    #1.2.1 Identifier les variables n�cessaires
    varsTypeEco_GrStat <- c("TYPE_ECO", "FAM_STAT", "GR_STAT")
    
    #1.2.2 S'il y a au moins une variable manquante
    if(!all(varsTypeEco_GrStat %in% names(typeEcoToFamStat))){
      
      #On l'identifie
      varsManq <- varsTypeEco_GrStat[!varsTypeEco_GrStat %in% names(typeEcoToFamStat)]
      
      #Et on arr�te la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas pr�sentes dans le jeu de donn�es d�fini par 'typeEcoToFamStat'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    #1.3 Objet "tabResumRegroupe" (Tableau resumen avec le lien entre les 
    #GTYFs et GR_STATs et les GTYFs et GR_STATs regroup�s (ce qui nous permet
    #de regrouper les groupes qui sont trop petits))
    #1.3.1 Identifier les variables n�cessaires
    varsTabRegroupe <- c("SDOM_BIO", "GTYF_M7M", "GR_STAT", 
                         "GTYF_M7M_R", "GR_STAT_R")
    
    #1.3.2 S'il y a au moins une variable manquante
    if(!all(varsTabRegroupe %in% names(tabResumRegroupe))){
      
      #On l'identifie
      varsManq <- varsTabRegroupe[!varsTabRegroupe %in% 
                                    names(tabResumRegroupe)]
      
      #Et on arr�te la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas pr�sentes dans le jeu de donn�es d�fini par 'tabResumRegroupe'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    #1.4 Objet "classDecal" (Tableau des quantiles de classe de d�calage)
    #1.4.1 Identifier les variables n�cessaires
    varsClassDecal <- c("SDOM_BIO", "GR_STAT_R", "TYF_M7M_R", "courbeV", "DEC_CLASS")
    
    #1.4.2 S'il y a au moins une variable manquante
    if(!all(varsClassDecal %in% names(classDecal))){
      
      #On l'identifie
      varsManq <- varsClassDecal[!varsClassDecal %in% 
                                   names(classDecal)]
      
      #Et on arr�te la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas pr�sentes dans le jeu de donn�es d�fini par 'classDecal'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    #1.5 Objet "gtyfToTyf" (Tableau de conversion des GTYFs en TYFs)
    #1.5.1 Identifier les variables n�cessaires
    varsGtyfToTyf <- c("SDOM_BIO", "GR_STAT_R", "GTYF_M7M_R", "TYF_M7M_R")
    
    #1.5.2 S'il y a au moins une variable manquante
    if(!all(varsGtyfToTyf %in% names(gtyfToTyf))){
      
      #On l'identifie
      varsManq <- varsGtyfToTyf[!varsGtyfToTyf %in% names(gtyfToTyf)]
      
      #Et on arr�te la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas pr�sentes dans le jeu de donn�es d�fini par 'gtyfToTyf'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    #1.6 Objet "cloneCourbes" (Tableau de conversion des GTYFs en TYFs)
    #1.6.1 Identifier les variables n�cessaires
    varsCloneCourbes <- c("SDOM_BIO", "GR_STATION", "TYF", "CLASSE", 
                          "AGE", "HAUT_MOY", "EXTRAPOL", "DEC_CLASS")
    
    #1.6.2 S'il y a au moins une variable manquante
    if(!all(varsCloneCourbes %in% names(cloneCourbes))){
      
      #On l'identifie
      varsManq <- varsCloneCourbes[!varsCloneCourbes %in% names(cloneCourbes)]
      
      #Et on arr�te la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas pr�sentes dans le jeu de donn�es d�fini par 'cloneCourbes'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    #1.7 Objet "clAgeToNum" (table de correspondance entre les classe d'�ge
    #et leur valeur num�riques correspondantes)
    #1.7.1 Identifier les variables n�cessaires
    varsClAge <- c("CL_AGE", "REG_AGE")
    
    #1.7.2 S'il y a au moins une variable manquante
    if(!all(varsClAge %in% names(clAgeToNum))){
      
      #On l'identifie
      varsManq <- varsClAge[!varsClAge %in% names(clAgeToNum)]
      
      #Et on arr�te la fonction
      stop("La ou les variables ", paste(varsManq, collapse = ", "),
           " ne sont pas pr�sentes dans le jeu de donn�es d�fini par 'clAgeToNum'.",
           "Faites attention que cette fonction est sensible aux minuscules ",
           "et aux majuscules.")
    }
    
    
    
    #2. Ajouter les GTYFs et les GR_STAT regroup�s
    #2.1 Ajouter le groupe de station
    cscpf <- 
      left_join(cscpf %>% mutate(SDOM_BIO = as.character(SDOM_BIO),
                                 TYPE_ECO = as.character(TYPE_ECO)), 
                typeEcoToFamStat %>% mutate(SDOM_BIO = as.character(SDOM_BIO),
                                            TYPE_ECO = as.character(TYPE_ECO)),
                by = c("SDOM_BIO", "TYPE_ECO"))
    
    #2.2 Ajouter les GTYFs et les GR_STAT regroup�s (pour enlever des groupes
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
    
    
    
    #3. Calculer la somme cummulative des superficies des groupes regroup�s
    #3.1 Regrouper les donn�es par GTYF et GR_STAT regroup�s.
    #Selon Fran�ois on n'a pas besoin d'inclure le sous-domaine dans les
    #groupes. �a va �tre fait apr�s quand on d�cide la courbe � utiliser
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
    
    
    
    #4. D�terminer quels groupes il faut diviser en 2 (courbes v12 et 34).
    #On va d�finir un seuil de superficie cumulative (d�faut = 50%). �a veut
    #dire que les strates dont la somme de la superficie occupe 50% du 
    #t�rritoire seront divis�es en 2 (courbe v12 et v34)
    #4.1 D�terminer les strates qu'on va diviser en 2
    sumSup <- 
      sumSup %>%  
      mutate(nombreGE = ifelse(percCumSup > propSupGrosGroupes, 1, 2))
    
    
    #4.2 Ajouter le nombre de GEs par groupe au jeu de donn�es principal
    #On s�lectionne les colonnes qu'on veut de sumSup avant de faire le join
    #pour ajouter seulement la colonne qu'on veut (nombreGE)
    cscpf <- 
      left_join(cscpf, 
                sumSup %>% select(SDOM_BIO, GTYF_M7M_R, GR_STAT_R, nombreGE),
                by = c("SDOM_BIO", "GTYF_M7M_R", "GR_STAT_R"))
    
    
    
    #5. Convertir les GTYFs en TYFs (pour qu'on puisse trouver une courbe �quivalente
    #plus tard). Cette nouvelle variable va s'appeler "TYF_M7M_R"
    cscpf <- left_join(x = cscpf, 
                       y = gtyfToTyf %>% 
                         mutate_all(as.character),
                       by = c("SDOM_BIO", "GR_STAT_R", "GTYF_M7M_R"))   
    
    
    
    #6. D�terminer quel courbe V utiliser (quand on a qu'une courbe dans la strate)
    #6.1 D�terminer la superficie occup�e par chaque classe de courbeV
    supCourbeV <- 
      cscpf %>% 
      group_by(SDOM_BIO, GR_STAT_R, TYF_M7M_R, COURBE_V) %>% 
      summarise(sumSup = sum(SUP_BRU, na.rm = TRUE))
    
    #6.2 S�lectionner le groupe courbeV le plus gros de chaque strate
    #en s�lectionnant la premi�re ligne de chaque groupe apr�s l'avoir
    #ordonn� selon l'ordre decroissante de leur superficie totale
    supCourbeV <- 
      supCourbeV %>% 
      group_by(SDOM_BIO, GR_STAT_R, TYF_M7M_R) %>% 
      arrange(desc(sumSup)) %>% 
      slice(1) %>% 
      ungroup() %>% 
      
      #6.3 Renomer la colonne pour �viter des probl�mes et
      #s�lectionner seulement les courbes dont on a besoin
      transmute(SDOM_BIO, GR_STAT_R, TYF_M7M_R,
                COURBE_V_R = as.character(COURBE_V))
    
    
    #6.4 Ajouter la COURBE_V_R (regroup�e au jeu de donn�es principal)
    cscpf <- 
      left_join(cscpf, supCourbeV, 
                by = c("SDOM_BIO", "GR_STAT_R", "TYF_M7M_R"))
    
    
    #6.5 Alors, si on divise le strate en 2, on utilise la COURBE_V originale,
    #sinon on utlise la COURBE_V_R
    cscpf <- 
      cscpf %>% 
      mutate(COURBE_V_R = ifelse(nombreGE %in% 2, 
                                 yes = as.character(COURBE_V),
                                 no = as.character(COURBE_V_R)))
    
    
    
    #7. Ajouter la classe de d�calage correspondante � chaque courbe
    #7.1 S�lectionner les variables dont on a besoin et les transformer
    #dans des caract�res pour �viter des avertissements inutiles
    classDecal <- 
      classDecal %>% 
      select(SDOM_BIO, GR_STAT_R, TYF_M7M_R, courbeV, DEC_CLASS) %>% 
      mutate_all(as.character)
    
    #7.2 Faire le join
    cscpf <- left_join(cscpf, classDecal,
                       by = c("SDOM_BIO", "GR_STAT_R", "TYF_M7M_R", 
                              "COURBE_V_R" = "courbeV"))
    
    
    
    #8. Cr�er la colonne de la courbe finale correspondante
    #8.1 Cr�er la colonne
    cscpf <- 
      cscpf %>% mutate(courbe = paste(SDOM_BIO, GR_STAT_R, TYF_M7M_R, 
                                      "NA", COURBE_V_R, sep = "_"))
    
    
    #9. Ajouter le NOM_FAMC du catalogue des clones des courbes
    #9.1 S�lectionner les colonnes qu'on veut
    nomFam <-
      cloneCourbes %>%
      select(courbe, DEC_CLASS, NOM_FAMC) %>%
      distinct()
    
    #9.2 Faire le join
    cscpf <- left_join(cscpf, nomFam, by = c("courbe", "DEC_CLASS"))
    
    
    
    
    #9. S�lectionner les colonnes qu'on veut sortir
    output <- 
      cscpf %>% distinct(ID_BFEC, courbe, NOM_FAMC, DEC_CLASS) 
    
    
    return(as.data.frame(output))
    
    
  }##### Fin de la fonction