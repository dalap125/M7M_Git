#Ce code sert � appliquer un mod�le de random forests pour pr�dire le type de for�t des 
#peuplements qui ont moins de 3 m�tres ET ceux qui ont 4 � 6M et des TYFs Rx, Fx, 
#RxFx et FxRx. La fonction va aussi pr�dire la classe de volume (courbe v12 et v34) 
#de tous les peuplements qui ont moins de 7M

#Ce script peut �tre roul� directement ou peut �tre appel� par un autre script
#avec la fonction "source(�directoire de ce script.R�)"


############################################################################################################
############################################################################################################
#Voici les sections de cette fonction:
#   0. Cr�er un objet o� on va stocker nous extrants
#   1. Charger des packages
#   2. Charger les mod�les de random forest
#   3. Joindre les 2 jeux de donn�es (peeOri et variables climatiques)
#   4. V�rifier que toutes les variables dont on a besoin sont l�
#   5. Transformer le code des plantations en CTs (au cas o� il y a des �ch�ques de 
#   plantations dans les peuplements pour lesquels on veut pr�dire le GTYF et la hauteur;
#   on a besoin de le changer parce que les mod�les de random forest ne conaissent pas
#   les codes de plantation)
#   6. Adapter les variables selon les besoins des mod�les de random forest (ex. v�rifier que
#   que tous les niveaux factorielles sont compatibles))
#   7. V�rifier qu'on n'a pas des donn�es manquantes (ex. toutes les peuplements ont toutes les
#   variables climatiques)
#   8. Pr�dire le grand TYF et la classe de volume
#   9. Stocker le jeu de donn�es avec les pr�dictions de GTYF et de COURBE_V dans l'objet
#   d'extrant
#   10. Calculer le tableau resum� avec les superficies occup�es par chaque groupe et
#   l'ajouter � l'objet d'extrant
#   11. Terminer la fonction en s�lectionnant l'objet d'extrant
############################################################################################################
############################################################################################################


#Voici les arguments de la fonction:
#1. On peut d�finir la fonction "pred_Tyf_Vol_Prod_M7M()" qu'on a �crit dans un autre 
#script avec la fonction "source( �directoire du script.R�)". Cette fonction pr�dit les 
#grands TYFs et les classes de volume des peuplements qui ont moins de 7M. Cette fonction
#a besoin des arguments suivants: 
# mainDir <- file.path("T:\\Donnees\\Courant\\Projets\\Chantier_M7M", 
#                      "Script_Plusieurs_SDOMs\\M7M")
#                      
#     1.1 dirRfGtyf_couvert : directoire de la liste des mod�les de random forests 
#     calibr�s pour pr�dire  les grands TYFs de CHAQUE sous-domaine QUAND LA 
#     VARIABLE TYPE_COUV EST DISPONIBLE 
#     Ceci doit �tre un fichier .rds qui a seulement une liste des mod�le de random forests 
#     Ex. dirRfGtyf_couvert = file.path(mainDir, "Inputs", "Listes de random forests", "list_RF_GrandTYF_couvert.rds")

#     1.2 dirRfGtyf_pasCouvert : directoire de la liste des mod�les de random forests 
#     calibr�s pour pr�dire  les grands TYFs de CHAQUE sous-domaine QUAND LA 
#     VARIABLE TYPE_COUV N'EST PAS DISPONIBLE 
#     Ceci doit �tre un fichier .rds qui a seulement une liste des mod�le de random forests 
#     Ex. dirRfGtyf_pasCouvert = file.path(mainDir, "Inputs", "Listes de random forests","list_RF_GrandTYF_pasCouvert.rds")


#     2.1 dirRfClassVol_dens : directoire de la liste des mod�les de random forests 
#     calibr�s pour CHAQUE SOUS_DOMAINE pour pr�dire les classes de volume 
#     QUAND ON CONNA�T la densit�. 
#     Ceci doit �tre un fichier .rds qui a seulement une liste des mod�le de random forests 
#     Ex. dirRfClassVol_dens = file.path(mainDir, "Inputs", "Listes de random forests","list_RF_ClasseVol_Dens.rds")

#     2.2 dirRfClassVol_pasDens : directoire de la liste des mod�les de random forests 
#     calibr�s pour CHAQUE SOUS_DOMAINE pour pr�dire les classes de volume 
#     QUAND ON NE CONNA�T PAS la densit�. 
#     Ceci doit �tre un fichier .rds qui a seulement une liste des mod�le de random forests 
#     Ex. dirRfClassVol_pasDens = file.path(mainDir, "Inputs", "Listes de random forests","list_RF_ClasseVol_pasDens.rds")


#     3. peeOri : dataframe des donn�es pour lesquels on veut pr�dire les types de for�t 
#     et la classende volume. Ceci doit avoir une structure pareille aux fichiers 
#     'pee_ori.dbf'  
#     Ex. peeOri = read_csv(file.path(mainDir, "Inputs", "Update2_CSCPF_08551.csv"))
#     
#     4. donneesClim : dataframe des donn�es avec les variables climatiques. Ceci vient 
#     d'un jeu de donn�es qui a plusieurs variables climatiques BioSIM pour chaque 
#     peuplement dans la 6O (identifi�s par la variable �GEOCODE�). 
#     Ex. donneesClim = read_csv(file.path(mainDir, "Inputs", "Climate_08551.csv"))
#     
#     5. tyfToGtyf : table de pilotage pour convertir les TYFs des peuplements de 4 � 6m 
#     en GTYFs. Le fichier sp�cifi� doit �tre un dbf
#     Ex. tyfToGtyf = read.dbf(file.path(mainDir, "Inputs\\table_pilotage_resume_tyf_gtyf.dbf"))
#
#     6. typeEcoToFamStat : table de pilotage pour d�terminer la famille de station 
#     selon le sous-domaine (SDOM_BIO) et le type eco (TYPE_ECO). 
#     Le fichier sp�cifi� doit �tre un csv
#     Ex. typeEcoToFamStat = read.csv2(file.path(mainDir, "Inputs\\TypeEco_To_Fam_Stat_par_SDOM.csv"))
#
#     7. vegPot_SDOM: table de pilotage de conversion des v�getations potentielles
#     "originales" dans des valeurs accept�es par le mod�le de random forest du 
#     sous-domaine correspondante. Comme la v�g�tation change de sous-domaine en 
#     sous-domaine, les valeurs de veg pot accept�es par chaque mod�le change aussi.
#     Pour cette raison on a du cr�er cette table avec des valeurs bidons diff�rentes
#     pour chaque sous-domaine (par example, comme FE n'existe pas vraiment dans la
#     6O, ces valeurs sont convertis dans la valeur bidon "RS". Sans �a le mod�le de
#     random forest ne peut pas marcher par sous domaine et ces cas sont tellement
#     marginaux qu'ils ne doivent pas influencer les pr�dictions d'une fa�on
#     significative; le m�me a �t� faite pour le tableau de conversion du TYPE_ECO
#     en famille de station FAM_STAT)
#     Ex. vegPot_SDOM = read_csv2(file.path(mainDir, "Inputs", "vegPot_par_SDOM.csv"))
#     
#     8. P7M : vecteur logique (TRUE/FALSE) pour decidir si on veut pr�dire le
#     GTYF et la courbeV pour les peuplements qui ont 7 m�tres et plus. 
#     La valeur de d�faut est FALSE, ce que veut dire que les variables ne sont 
#     pas pr�dites pour les peuplements qui on 7m et plus
#     

#L'extrant de cette fonction est une liste avec 2 dataframes:
#   1. GTYF_Prod : un dataframe avec 3 colonnes: 
#         - ID_BFEC: l'id du polygone
#         - GTYF_M7M: le GTYF pr�dit
#         - COURBE_V: la courbe V pr�dite (v12 ou v34)
#
#       Le GTYF pr�dit est une union: 
#           - du GTYF original des peuplements de 7M et plus (i.e. 
#           non-pr�dit par random forest)
#           - du GTYF original des peuplements de 4 � 6M qui avaient d�j� 
#           un TYF bon (i.e. un TYF qui n'�tait pas des RxFx, FxRx, ...)
#           - du GTYF pr�dit par random forest pour les autres peuplements 
#           qui ont 4 � 6m et pour tous les peuplements qui ont 3m et moins. 
#         
#   
#   
#   2. donneesManquantes : le dataframe des polygones qui ont des donn�es manquantes 
#   (au ca o� on en a)

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
    
    
    #0. Cr�er la liste o� on va stocker tous les r�sultats
    listeExtrant <- 
      list(GTYF_Prod = NULL,
           donneesManquantes = NULL)
    
    
    
    #1. Charger les packages n�cessaires
    require(randomForest)  #Pour faire les random forests (for�ts al�atoires)
    require(tidyr)         #Traitement des donn�es
    require(dplyr)         #Traitement des donn�es
    
    
    
    #2. Charger les mod�les de random forest 
    #2.1 Mod�le de random Forest pour les grands TYFs AVEC le type de couvert
    #2.1.1 Charger le mod�le de random forest
    rfGTyf_couv <- readRDS(dirRfGtyf_couvert)
    
    #2.1.2 V�rifier que le mod�le existe
    if(!exists("rfGTyf_couv")){  #Alors, s'il n'existe pas ("!" sp�cifie un n�gation en R)
      stop(paste("Le mod�le de random forest n�cessaire ne se trouve pas dans", 
                 dirRfGtyf_couvert, "(le r�pertoir sp�cifi� par l'objet 'dirRfGtyf_couvert')."))
    }
    
    
    #2.2 Mod�le de random Forest pour les grands TYFs SANS le type de couvert
    #2.2.1 Charger le mod�le de random forest
    rfGTyf_pasCouv <- readRDS(dirRfGtyf_pasCouvert)
    
    #2.2.2 V�rifier que le mod�le existe
    if(!exists("rfGTyf_pasCouv")){  #Alors, s'il n'existe pas ("!" sp�cifie un n�gation en R)
      stop(paste("Le mod�le de random forest n�cessaire ne se trouve pas dans", 
                 rfGTyf_pasCouv, "(le r�pertoir sp�cifi� par l'objet 'dirRfGtyf_pasCouvert')."))
    }
    
    
    #2.3 Mod�le de random Forest pour les classes de volume quand on conna�t
    #la densit�
    #2.3.1 Charger le mod�le de random forest
    rfClassVol_dens <- readRDS(dirRfClassVol_dens)
    
    
    #2.3.2 V�rifier que le mod�le existe
    if(!exists("rfClassVol_dens")){  #Alors, s'il n'existe pas ("!" sp�cifie un n�gation en R)
      stop(paste("Le mod�le de random forest n�cessaire ne se trouve pas dans", 
                 rfClassVol_dens, 
                 "(le r�pertoir sp�cifi� par l'objet 'dirRfClassVol_dens')."))
    }
    
    
    #2.4 Mod�le de random Forest pour les classes de volume quand on conna�t
    #la densit�
    #2.4.1 Charger le mod�le de random forest
    rfClassVol_pasDens <- readRDS(dirRfClassVol_pasDens)
    
    
    #2.4.2 V�rifier que le mod�le existe
    if(!exists("rfClassVol_pasDens")){  #Alors, s'il n'existe pas ("!" sp�cifie un n�gation en R)
      stop(paste("Le mod�le de random forest n�cessaire ne se trouve pas dans", 
                 rfClassVol_pasDens, 
                 "(le r�pertoir sp�cifi� par l'objet 'dirRfClassVol_pasDens')."))
    }
    
    
    ###############################################################
    ###############################################################
    #If statement � ajouter quand on va ajouter la partie de pr�diction
    #des TYFs et des classes de volume pour les 7MP
    # #3. Joindre les jeux de donn�es (peeOri et variables climatiques)
    # #3.1 Maintenant, si on veut (P7M == TRUE), on peut pr�dire le courbeV
    # #et le GTYF des peuplements qui ont plus de 7M s'ils sont coup�s
    # #On a besoin de nous assurer que la hauteur est une variable num�rique
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
    
    
    #3.2 Ajouter les variables climatiques au jeu de donn�es principal
    #3.2.1 Trouver toutes les variables utilis�es par tous lesmod�les de 
    #random forests. La colonne "importance" donne-nous le noms de toutes 
    #les variables utilis�es. 
    #Avec "sapply" on peut appliquer une fonction a chaque �l�ment
    #d'une liste. Unlist() regroupe tous les �l�ments de la liste
    #dans un seul object (i.e. un vecteur dans ce cas) et unique()
    #donne nous les valeurs uniques de ce vecteur
    varsRf1 <- sapply(rfGTyf_couv, function(x)rownames(x$importance))
    varsRf2 <- sapply(rfGTyf_pasCouv, function(x)rownames(x$importance))
    varsRf3 <- sapply(rfClassVol_dens, function(x)rownames(x$importance))
    varsRf4 <- sapply(rfClassVol_pasDens, function(x)rownames(x$importance))
    
    
    varsRf <- unique(c(unlist(varsRf1), unlist(varsRf2),
                       unlist(varsRf3), unlist(varsRf4)))
    
    #3.3.2 La variable vegPot a �t� cr�� par nous lors de l'entra�nement du mod�le. Alors, 
    #pour l'instant il faut la remplacer par la variable initiale (TYPE_ECO)
    if("vegPot" %in% varsRf){
      varsRf[varsRf %in% "vegPot"] <- "TYPE_ECO"
    }
    
    #3.3.3 Trouver les variables climatiques utilis�es 
    varsClimUtils <- names(donneesClim)[names(donneesClim) %in% varsRf]
    
    
    #3.3.4 S�lectionner ces variables dans le jeu de donn�es des variables climatiques
    #(en oubliant pas le GEOCODE)
    donneesClim <- donneesClim %>% select(GEOCODE, one_of(varsClimUtils))
    
    #3.3.5 Joindre avec le jeu de donn�es principal
    donneesProc <- 
      left_join(donneesProc %>% mutate(GEOCODE_OR = as.character(GEOCODE_OR)),   #On transforme les GEOCODE_ORS en 
                donneesClim %>% mutate(GEOCODE = as.character(GEOCODE)), #caract�res pour �viter des 
                by = c("GEOCODE_OR" = "GEOCODE"))                 #avertissements inutiles
    
    
    
    #4. V�rifier que toutes les variables utilis�es dans le mod�le de random forests
    #existent dans les fichiers fournis. 
    #4.1 Comme on va filtrer les donn�es selon leur HAUTEUR, il faut que cette variable soie
    #pr�sente aussi. On va avoir aussi besoin du TYF (pour les peuplements pour lesquels on
    #conna�t d�j�) et du GEOCODE. On va avoir besoin de "SDOM_BIO" pour faire les groupes
    #�volutifs � la fin
    varsTester <- 
      c(varsRf, "HAUTEUR", "TYPE_ECO", "TYF", "GEOCODE_OR", "SDOM_BIO", "ID_BFEC")
    
    varsTester <-    #FAM_STAT sera cr�e apr�s
      varsTester[!varsTester %in% c("FAM_STAT")] 
    
    #4.2 Alors, maintenant on v�rifie que ces 5 variables sont dans l'objet pee_ori fournit
    #"all()" sort "TRUE" si toutes les valeurs donn�s sont TRUE, mais FALSE si au moins une des 
    #valeurs fournies est FALSE. Avec la n�gation (!all(...)), si toutes les valeurs sont TRUE
    #la r�ponse est FALSE (alors, le if(...) est ignor�), et si une valeur est FALSE, la r�ponse
    #est TRUE (et la fonction if(...) est fait rouler)
    if(!all(varsTester %in% names(donneesProc))){
      
      #4.2.1 Si on a une variable manquante, il faut l'identifier...
      varsManquantes <- varsTester[!varsTester %in% names(donneesProc)]
      
      #4.2.2 Et arr�ter la fonction
      stop(paste("La ou les variables", paste(varsManquantes, collapse = ", "), 
                 "n'existent pas dans le fichier fournit dans 'peeOri','donneesClim' et/ou",  
                 "Faites attention que cette fonction est sensible aux", 
                 "minuscules et aux majuscules. Par exemple, 'ORIGINE' est diff�rent de 'origine'.", 
                 "S'il vous pla�t changez le nom de la colonne et/ou ajoutez les variables",  
                 "manquantes avant de recommencer. Faites aussi attention que la variable",  
                 "'UPAYS_REG' devrait faire partie du jeu de donn�es fournit dans", 
                 "'donneesClim'. La variable 'PERT_BIN' est cr��e avec la variable 'PERTURB'. "))
    }
    
    
    #4.3 V�rifier que la table de pilotage que convert les TYFs en GTYFs a tous les TYFs 
    #pr�sents dans le jeu de donn�es
    #4.3.1 D�finir les objets avec les tyfs pr�sents dans le jeu de donn�es et les tyfs qui sont
    #dans la table de pilotage. Attention, comme on ne veut pas convertir les valeurs Rx, Fx, RxFx 
    #et FxRx, ils ne sont pas dans la table de pilotage: alors, pour nous assurer que �a ne cause
    #pas des probl�mes, il faut qu'on l'ajoute � l'objet des TYFs de la table
    tyfsPres <- unique(na.omit(donneesProc$TYF))
    tyfsTable <- c(as.character(tyfToGtyf$TYF), "Fx", "Rx", "RxFx", "FxRx", "Rz", 
                   "RZ", "Fz", "FZ")
    
    #4.3.2 Alors, si au moins une valeur n'est pas dans la table 
    if(!all(tyfsPres %in% tyfsTable)){
      
      #4.3.2.1 Identifier ceux qui manquent
      tyfsManq <- as.character(tyfsPres[!tyfsPres %in% tyfsTable])
      
      #4.3.2.2 Arr�ter la fonction
      stop(paste("Les codes de TYF", paste(tyfsManq, collapse = ", "), "ne sont pas dans la table", 
                 "de pilotage fournie dans l'objet 'tyfToGtyf'. S'il vous pla�t, ajoutez-les � la", 
                 "table de pilotage. Si ce sont des codes d'essences pour lesquels on ne veut pas", 
                 "pr�dire un grand TYF (ex. des essences de plantation), vous pouvez mettre le code", 
                 "dans la colonne TYF, et 'NA' dans la colonne GTYF."))
    } 
    
    
    
    #5. Enlever toutes les plantations du jeu de donn�es
    ##################################################################
    ##################################################################
    #FINALEMENT ON VA GARDER LES PLANTATIONS POUR LES CAS O� LES 
    #PLANTATIONS NE SONT PAS BIEN R�USSIES
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
    # #5.2 Faire un avertissement pour dire combien de peuplements �taient dans des plantations
    # if(!nombrePlantations %in% 0){
    #   warning(paste(nombrePlantations, "peuplements ont �t� enlev�s lors de l'analyse",
    #                 "parce qu'ils �taient des plantations."))
    # }
    # 
    # #5.3 Et enlever les plantations
    # donneesProc <- 
    #   donneesProc %>% 
    #   filter(!ORIGINE %in% c("ENS", "ETR", "P", "PRR", "REA", "RIA", "RPS"))
    
    
    
    #6. S�lectionner et adapter les variables dont on a besoin pour entra�ner le 
    #mod�le de random forest
    #6.0 D�finir les vecteurs avec les niveaux de facteur accept�s par le 
    #mod�le de random forest pour chaque variable factorielle. Ils peuvent 
    #�tre acc�d�s avec 
    #rfGTyf_couv$oues6$forest$xlevels$ 'nom de la variable factorielle'.
    #De toutes les variables, il y a quelques unes qui peuvent changer
    #entre sous-domaines (e.g. la v�g�tation potentielle). Ceux 
    #vont �tre plus complexes � g�rer
    #6.0.1 Classe de drainage
    niveauxDrainage <- rfGTyf_couv$`6O`$forest$xlevels$CL_DRAI 
    
    #6.0.2 Perturbation d'origine
    niveauxOrigine <- rfGTyf_couv$`6O`$forest$xlevels$ORIGINE 
    
    #6.0.3 Type de couvert
    niveauxCouv <- rfGTyf_couv$`6O`$forest$xlevels$TYPE_COUV 
    
    #6.0.4 Famille de station
    #Comme les niveaux de FAM_STAT et vegPot changent selon les sous-domaine,
    #le processus pour ces 2 variables (qui va �tre d�crit tant�t) 
    #est plus compl�xe 
    niveauxFamStat <- list('6O' = rfGTyf_couv$`6O`$forest$xlevels$FAM_STAT,
                           '5O' = rfGTyf_couv$`5O`$forest$xlevels$FAM_STAT)
    
    #6.0.5 V�g�tation potentielle
    niveauxVegPot <- list('6O' = rfGTyf_couv$`6O`$forest$xlevels$vegPot,
                          '5O' = rfGTyf_couv$`5O`$forest$xlevels$vegPot)
    

    
    #6.1 Si on a des valeurs manquantes, on va chercher la moyenne ou la
    #valeur dominante
    donneesProc <- 
      donneesProc %>% 
      mutate(CL_DRAI = ifelse(is.na(CL_DRAI), 
                              names(sort(table(CL_DRAI),decreasing=TRUE)[1]),
                              CL_DRAI),
             CL_PENT = ifelse(is.na(CL_PENT), 
                              names(sort(table(CL_PENT),decreasing=TRUE)[1]),
                              CL_PENT),
             TYPE_ECO = ifelse(is.na(TYPE_ECO), 
                              names(sort(table(TYPE_ECO),decreasing=TRUE)[1]),
                              TYPE_ECO),
             PRECI_TOT = ifelse(is.na(PRECI_TOT), mean(PRECI_TOT, na.rm = TRUE), PRECI_TOT),
             PRECI_UTI = ifelse(is.na(PRECI_UTI), mean(PRECI_UTI, na.rm = TRUE), PRECI_UTI),
             PRECI_NEIG = ifelse(is.na(PRECI_NEIG), mean(PRECI_NEIG, na.rm = TRUE), PRECI_NEIG),
             TMOY_SCR = ifelse(is.na(TMOY_SCR), mean(TMOY_SCR, na.rm = TRUE), TMOY_SCR),
             PJOUR_GEL = ifelse(is.na(PJOUR_GEL), mean(PJOUR_GEL, na.rm = TRUE), PJOUR_GEL),
             ARID_TOT = ifelse(is.na(ARID_TOT), mean(ARID_TOT, na.rm = TRUE), ARID_TOT),
             RADIA_TOT = ifelse(is.na(RADIA_TOT), mean(RADIA_TOT, na.rm = TRUE), RADIA_TOT),
             RADIA_SCR = ifelse(is.na(RADIA_SCR), mean(RADIA_SCR, na.rm = TRUE), RADIA_SCR),
             TMOY_AN = ifelse(is.na(TMOY_AN), mean(TMOY_AN, na.rm = TRUE), TMOY_AN),
             DPV_UTI = ifelse(is.na(DPV_UTI), mean(DPV_UTI, na.rm = TRUE), DPV_UTI),
             PP_NEIGE = ifelse(is.na(PP_NEIGE), mean(PP_NEIGE, na.rm = TRUE), PP_NEIGE),
             TMAX_AN = ifelse(is.na(TMAX_AN), mean(TMAX_AN, na.rm = TRUE), TMAX_AN),
             TMOY_JUIL = ifelse(is.na(TMOY_JUIL), mean(TMOY_JUIL, na.rm = TRUE), TMOY_JUIL))
             
             
    
    #6.2 CLASSE DE DRAINAGE: Adapter la classe de drainage en utilisant juste 
    #le premier caract�re
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
             
             
             #6.3 CLASSE DE PENTE: Adapter la classe de pente en la convertissant dans une variable
             #num�rique avec la valeur m�diane de la classe
             CL_PENT = ifelse(CL_PENT %in% "A", 1.5,
                              ifelse(CL_PENT %in% "B", 6,
                                     ifelse(CL_PENT %in% "C", 12,
                                            ifelse(CL_PENT %in% "D",23,
                                                   ifelse(CL_PENT %in% c("E", "F", "S"), 35,
                                                          CL_PENT))))),
             CL_PENT = as.numeric(CL_PENT),
             
             
             #6.4 CLASSE D'ORIGINE: adapter le champ d'origine en NATUREL (NAT), coupe avec
             #protection de la r�g�n�ration (CPR) ou coupe totale (CT)
             ORIGINE = case_when(
               .$ORIGINE %in% c("BR", "CHT", "DT", 
                                "ES", "FR", "VER") ~            "NAT",
               .$ORIGINE %in% c("CDV", "CPH", "CPR", "CPT") ~   "CPR",
               TRUE ~      "CT"), #Toute les autres codes sont des coupes puis
                                  #on considere que tous les NAs sont des CTs
                                  #
             #Si on a des NAs, on cherche la valeur dominante
             ORIGINE = ifelse(is.na(ORIGINE), 
                              names(sort(table(ORIGINE),decreasing=TRUE)[1]),
                              ORIGINE),
             ORIGINE = factor(ORIGINE, levels = niveauxOrigine)) %>% 
      
      
      #6.5 DENSITE: si la densit� est NA, on lui donne une valeur de 0
      mutate(DENSITE = as.numeric(as.character(DENSITE)),
             
             
             #6.6 Type de couvert: Il faut v�rifier que les niveaux de cette 
             #variable sont les niveaux que le mod�le de random forest utilise
             TYPE_COUV = factor(TYPE_COUV, levels = niveauxCouv))
    
    
    #6.7 FAMILLE DE STATION FORESTI�RE 
    ##Cette variable est plus compliqu� parce que les groupes qu'on fait peuvent 
    #varier selon le sous-domaine (i.e. selon les donn�es disponibles). Par exemple,
    #"RES" est un groupe tr�s importante dans la 6Ouest mais pas dans la 3Ouest.
    #Alors, on a fait un tableau de correspondance que doit �tre mis � jour pour
    #chaque sous-domaine. Les groupes de FAM_STAT accept�es doivent �tre les groupes
    #accept�s par les mod�les de random forest du sous-domaine correspondant.
    #6.7.1 D'abord il faut s�lectionner les variables qu'on veut de la table de 
    #correspondance
    typeEcoToFamStat <- 
      typeEcoToFamStat %>% 
      mutate(SDOM_BIO = as.character(SDOM_BIO),
             TYPE_ECO = as.character(TYPE_ECO)) %>% 
      distinct()  #Pour enlever les doublons
    
    #6.7.2 Apr�s il faut faire un join
    #On convertit SDOM_BIO et TYPE_ECO en variables de caract�res
    #pour �viter des messages d'erreur inutiles
    donneesProc <- 
      donneesProc %>% 
      mutate(SDOM_BIO = as.character(SDOM_BIO),
             TYPE_ECO = as.character(TYPE_ECO)) %>% 
      left_join(typeEcoToFamStat, 
                by = c("SDOM_BIO", "TYPE_ECO"))
    
    
    #6.8 V�G�TATION POTENTIELLE : 
    #M�me chose que la FAM_STAT.
    #Cette variable est plus compliqu� parce que les groupes qu'on fait peuvent 
    #varier selon le sous-domaine (i.e. selon les donn�es disponibles). Par exemple,
    #"RS" est un groupe tr�s importante dans la 6Ouest mais pas dans la 3Ouest.
    #Alors, on a fait un tableau de correspondance que doit �tre mis � jour pour
    #chaque sous-domaine. Les groupes de vegPot accept�es doivent �tre les groupes
    #accept�s par les mod�les de random forest du sous-domaine correspondant. Les
    #regroupements suivent les cat�gories du mod�le SUCC�S (quand possible)
    #6.8.1 Cr�er la variable de la v�getation potentielle "r�elle"
    donneesProc <- 
      donneesProc %>% 
      mutate(vegPot = substr(TYPE_ECO, 1, 3)) %>% 
      
      
      #6.8.2 Ajouter la valeur de veg pot convertit (i.e. le niveaux de vegetation
      #potentielle accept�es par les mod�les de random forest changent selon le
      #sous-domaine)
      left_join(vegPot_SDOM, by = c("SDOM_BIO", "vegPot" = "vegPot_Ori")) %>% 
      
      #6.8.3 Enlever la valeur originale de vegPot et la remplacer par la
      #vegPot_Conv de la table de pilotage
      select(-vegPot) %>% 
      rename(vegPot = vegPot_Conv)
    
    
    #6.8.4 Maintenant qu'on n'a plus besoin du TYPE_ECO, il faut 
    #le remplacer par "vegPot" dans l'objet des variables � utiliser 
    #dans random forest
    varsRf <- ifelse(varsRf %in% "TYPE_ECO", "vegPot", varsRf)
    
    
    # #6.9 UNIT� DE PAYSAGE : 
    #################################################################
    #################################################################
    # ON NE VA PLUS UTILISER L'UNIT� DE PAYSAGE PARCE QUE C'EST ASSEZ
    # COMPL�XE � METTRE AVEC PLUSIEURS SOUS-DOMAINES
    #################################################################
    ################################################################# 
    # au moins 2 unit�s de paysage tr�s petites ne faisaient
    # #pas partie de notre jeu de donn�es d'entra�nement. Alors, il faut les 
    # #mettre quelque part pour �viter que le mod�le plante.   
    # 
    #            #6.8.1 On met la "124" dans la "123",
    # mutate(UPAYS_REG = ifelse(UPAYS_REG %in% "124", "123",
    #            #6.8.2 La "125" dans la "126"
    #                    ifelse(UPAYS_REG %in% "125", "126",
    #            #6.8.3 Et n'importe quelle autre dans la "131", qui est l'unit� la plus grosse.
    #                           ifelse(!UPAYS_REG %in% niveauxPaysReg,
    #                                  "131",
    #            #6.8.4 Sinon, on laisse la valeur originale
    #                                  as.character(UPAYS_REG)))),
    #        UPAYS_REG = factor(UPAYS_REG, levels = niveauxPaysReg)) %>%  
    
    
    
    #7. V�rifier qu'on n'a pas des donn�es manquantes
    #7.0 Il faut enlever le TYPE_COUV de la s�lection de variables � tester
    #pour des valeurs manquantes parce que le mod�le fonctionne quand 
    #TYPE_COUV == NA. Il faut faire la m�me chose avec la densit�,
    #car on a des mod�les qui marchent quand es variables ne 
    #sont pas disponibles
    varsRfPasManq <- 
      c(varsRf[!varsRf %in% c("TYPE_COUV")], "SDOM_BIO")
    
    #7.1 Obtenir le vecteur TRUE/FALSE des observations incompl�tes
    #complete.cases() donne nous toutes les observations qui n'ont pas de valeurs manquantes.
    #Alors, avec "!" on peut avoir toutes les observations qui ONT des valeurs manquantes
    obsManquantes <- !complete.cases(donneesProc[ ,varsRfPasManq])
    
    
    #7.2 Alors, si on a des observations avec des valeurs manquantes, il faut avertir l'utilisateur
    #7.2.1 Si on a au moins une valeur manquante
    if(any(obsManquantes)){
      
      #7.2.2 On isole les observations qui ont des observations manquantes
      donneesManquantes <- 
        donneesProc %>% filter(obsManquantes)
      
      #7.2.3 On stock ce jeu de donn�es dans notre liste d'extrant
      listeExtrant$donneesManquantes <- donneesManquantes
      
      #7.2.4 On fait un avertissement mais on laisse le reste de l'analyse rouler
      warning(paste("Quelques observations (", nrow(donneesManquantes), "avaient des donn�es", 
                    "manquantes et vont avoir un tyfFinal de NA (une valeur manquante).", 
                    "Ces observations ont �t� enregistr�es dans l'object d'extrant."))
      
    }
    
    
    
    #8. Pr�dire le grand TYF avec le mod�le de random forests et obtenir le tyf final.
    #On va laisser le TYF original des peuplements de 7m+ et des 4-6m bien identifi�s
    #par le photo-interpr�te, et on va utiliser le grand tyf pr�dit pour les peuplements 
    #de 0 � 3 m et les 4-6m mal identifi�s (ex. les RxFx) 
    #8.0 Cr�er une fonction qui vas nous permettre de pr�dire le GTYF en tenant le
    #sous-domaine en compte. En fait, la plus grosse difficult� sont la vegPot et 
    #la famille de station parce que les niveaux de cette variable varient avec
    #le sous-domaine consid�r�
    pred_GTYF_par_SDOM <- 
      function(inputParSDOM, 
               levelsVegPot = niveauxVegPot, 
               levelsFamStat = niveauxFamStat,
               rfGTyf_couv, rfGTyf_pasCouv,
               rfClassVol_dens, rfClassVol_pasDens){
        
        #8.0.1 S�lectionner les niveaux de FAM_STAT et de vegPot selon le sous-domaine
        #Comme le nom de chaque �l�ment des listes qui ont les niveaux des variables
        #est �gal aux niveaux du sous-domaine (6O, 5O, ...), on peut s�lectionner
        #l'�l�ment de la liste en utilisant la valeur unique du sous-domaine. Comme
        #cette fonction doit �tre appliqu�e a un jeu de donn�es regroup� selon le 
        #sous-domaine, la variable SDOM_BIO va toujours avoir une seule valeur unique 
        levelsFamStat <- levelsFamStat[[unique(inputParSDOM$SDOM_BIO)]]
        levelsVegPot <- levelsVegPot[[unique(inputParSDOM$SDOM_BIO)]]
        
        
        #8.0.2 D�finir les niveaux des variables FAM_STAT et vegPot selon les valeurs
        #du sous-domaine  
        inputParSDOM <- 
          inputParSDOM %>%                  #On guarde les valeurs originaux de 
          mutate(FAM_STAT_ORI = FAM_STAT,   #FAM_STAT et vegPot dans une autre variable
                 vegPot_ORI = vegPot,
                 FAM_STAT = factor(FAM_STAT, levels = levelsFamStat),
                 vegPot = factor(vegPot, levels = levelsVegPot))
        
        
        #8.0.3 S�lectionner les random forests � utiliser (i.e. ceux du
        #sous-domaine correspondante)
        rfGTyf_couv_sdom <- rfGTyf_couv[[unique(inputParSDOM$SDOM_BIO)]]
        rfGTyf_pasCouv_sdom <- rfGTyf_pasCouv[[unique(inputParSDOM$SDOM_BIO)]]
        rfClassVol_dens_sdom <- rfClassVol_dens[[unique(inputParSDOM$SDOM_BIO)]]
        rfClassVol_pasDens_sdom <- rfClassVol_pasDens[[unique(inputParSDOM$SDOM_BIO)]]
        
        
        #8.0.4 Pr�dire le GTYF
        inputParSDOM <-
          inputParSDOM %>%

          #8.0.4.1 Si le TYPE_COUV n'est pas disponible on utilise un mod�le
          mutate(predGTYF = ifelse(TYPE_COUV %in% c("", NA, NULL),
                                   as.character(predict(rfGTyf_pasCouv_sdom, .)),

                                   #8.0.4.2 S'il ne l'est pas, on utilise un autre mod�le
                                   as.character(predict(rfGTyf_couv_sdom, .))))
        
    
        #8.0.5 Pr�dire la classe de volume
        inputParSDOM <- 
          inputParSDOM %>% 
          
          #8.0.5.1 Si on n'a pas de densit�, on applique le random forest
          #qui n'a pas cette variable
          mutate(COURBE_V = ifelse(DENSITE %in% c("", NA, NULL),
                                   as.character(predict(rfClassVol_pasDens_sdom, .)),
                                   
                                   #8.0.5.2 Si on a la densit�, on applique le random forest qui a la DENSITE
                                   as.character(predict(rfClassVol_dens_sdom, .))))
        
        
        #8.0.6 On r�cup�re les valeurs originaux de FAM_STAT et vegPot
        inputParSDOM <- 
          inputParSDOM %>% 
          mutate(FAM_STAT = FAM_STAT_ORI,
                 vegPot = vegPot_ORI) %>% 
          select(-FAM_STAT_ORI, -vegPot_ORI)
        
        
        #8.0.7 Retourner ce dataframe avec toutes les pr�dictions
        return(inputParSDOM)
        
        
      }
    
  
    #8.1 Pr�dire le GTYF et la classe de volume en utilisant cette fonction
    #8.1.1 Regrouper les donn�es selon le SDOM
    donneesProc <- 
      donneesProc %>% 
      
      #On va enlever les donn�es qui n'ont pas un sous-domaine
      #�a ne devrait pas arriver, mais c'est une bonne id�e de le mettre quand m�me
      #######################################################
    filter(!is.na(SDOM_BIO)) %>% 
      #######################################################
    group_by(SDOM_BIO) %>% 
      
      #8.1.2 Utiliser la fonction qu'on vient d'�crire pour pr�dire les 3 variables
      do(pred_GTYF_par_SDOM(inputParSDOM = .,
                            levelsFamStat = niveauxFamStat,
                            levelsVegPot = niveauxVegPot, 
                            rfGTyf_couv = rfGTyf_couv, 
                            rfGTyf_pasCouv = rfGTyf_pasCouv, 
                            rfClassVol_dens = rfClassVol_dens,
                            rfClassVol_pasDens = rfClassVol_pasDens)) %>% 
      ungroup()
    
   
    #8.2 Traiter des pr�dictions des GTYFS
    #8.2.1 Ajouter la table de pilotage pour convertir les TYFs des peuplements 
    #de 4 � 6 en GTYFs (pour les peuplements dont on connait le TYF)
    donneesProc <- 
      left_join(donneesProc %>% mutate(TYF = as.character(TYF)),   #On convertisse en caract�res pour �viter
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
    
    
    #8.3 Traiter des pr�dictions des classes de volume
    donneesProc <- 
      donneesProc %>% 
      
      #8.3.1 Donner une valeur de NA aux peuplements qui ont une hauteur >= 7
      mutate(COURBE_V = ifelse(HAUTEUR >= 7, NA, as.character(COURBE_V)),
             
             #8.3.2 Transformer la variable dans un facteur
             COURBE_V = factor(COURBE_V, 
                               levels = levels(rfClassVol_dens$`6O`$predicted)))
    
    
    
    #9. Attribuer l'extrant principal (data frame avec l'ID_BFEC, le grand TYF pr�dit et
    #la classe de volume pr�dite) � la liste d'extrant o� on stocke nous 
    #r�sultats
    listeExtrant$GTYF_Prod <- 
      donneesProc %>% 
      select(ID_BFEC, GTYF_M7M, COURBE_V)
    
    
    
    #10 Calculer le tableau resum� avec les superficies occup�es par chaque groupe
    tabResu <- 
      donneesProc %>% 
      
      #10.1 Regrouper les donn�es selon nous variables
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


