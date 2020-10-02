###################################################################################
## Auteur: YODA Adaman
## Technicien Supérieur de la Météorologie é l'ANAM (Agence Nationale de la Météorologie) du BURKINA FASO
## Administrateur de la base de données Météorologique
## Wathsup : +226 55 12 61 74
## Service:  +226 68 77 79 93
## E-mails: adamouyod@gmail.com/yodaadman1@gmail.com/y.adaman@yandex.com


## VOS CONTRIBUTIONS ET CONSEILS SONT VIVEMENTS RECOMMANDES POUR AMELIORER CE PROJETS


###################################################################################
### Ce script contient ces fonctions 
### debut_saison() : permet de determiner le debut de la saison pour une station et pour une annee donnees
### fin_saison() :  permet de determiner la fin de la saison
### dry_spell() :  permet de determiner les sequence sechesses de facon generale sur une partie de l'annee
### dry_spell_2(): Version amelioré de dry_spell() avec des options suplementaires
### maxDrySpell(): Cette fonction permet de renvoyer la sequence sechesse dans certaines situations malgre les données manquantes
### stress_spell(): cette fonction permet de retrouver les sequences de jours de stress hydrique pour un vecteur de pluie donnée
### seq_debut () : permet de determiner les sequences de debut de saison
### seq_fin()    : permer de determiner les sequence sechesse de fin de saison
### begin_to_end() : permet de dterminer la date exacte a partir de laquelles les sequence sechesse de fin sont recherchés
### dateoflasteful(): cette fonction permet de determiner la derniere date avant la date la plus tardive a laquelle on a boserver la capacité max au sol
### kel_jours(): cette fonction permet de determiner quel jours exacte parmis les trois jours de ebut de saison les 20.0 mm  sont atteintes
### Wrangler(): cette fonction met de traiter le data frame affin d 'eviter toute erreure possible
### NAPercent(): cette fonction permet de renvoyer le pourcentage de donnees manquantes
### gregorie_date(): cette fonction permet de convertir une date julienne en date gregorienne
### gregoire(): Cette fonction permet de convertir un vecteur de date du julien au gregorien
### Annee_bisex(): cette fonction permet de retourner un boulean T ou F montrant si une annee est bissextile ou pas
### Dateoflastefill(): Cette fonction permet de retourner la date en julien de la dernier poche pleine avant la date la plus precoce de fin de saison precisee
### calParameters(): cette fonction permet de prendre en entree un dataframe, un vecteur, et une chaine de caractere
####################################################################################
### on test si centaines varibles ne sont pas saisai par l'utilisateur on les defini


library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(xlsx)
library(magrittr)

# NA values handle --------------------------------------------------------



if(!("NaMaxPercent" %in% ls())) NaMaxPercent <- 50

if(!("NaIfToReplace" %in% ls())) NaIfToReplace <- 0.0


NAPercent <- function(x) {                                  ### cette fonction permet de renvoyer le pourcentage de valeurs manquantes dans un vecteur de donnée

  percent <- (sum(is.na(x), na.rm = TRUE)/length(x)) * 100

  return (percent)
}



# dry_spell function ------------------------------------------------------



dry_spell <- function(x){          #### Une fonction magique pour determiner les sequences sehesses
  
  x <- as.numeric(x)
  if (length(x) == 0) return(NA)   ######## Si le vecteur est vide, on retourne un na
  
  if (NAPercent(x) > NaMaxPercent) return (NA)
  
  x[is.na(x)] <- NaIfToReplace             #### On remplace les valeurs manquantes par des valeurs a remplacer si 
  
  #### Si le paramettre a une longueur nulle on return rien
  
  ##### declaration d'un compteurs ci-dessous
  
   count <- 0                   #### Lui son role c'est de compter oui compter les sequences sechesses
   
   ss <- 0                      ### on cree un vecteur qui va combiner les differentes sequences sechesse en un seul vecteur pour
                                ### ensuite extraire le maximun
    for (i in 1:length(x)){  
      
                                 ### boucle de parcours
      
      if (x[i] < seuil_pluie){   ### tester ci la valeur de la pluie vaut ou pas le seuil 
        
        count <- count + 1
     
        } else {
        
          ss <- c(ss,count)      ###  Encompassement dans le vecteur
        
          count <- 0             ### Puis on remet le compteur à zero
      }
   }
   
    ss <- c(ss,count)
    
  #  
   
   
   #################################################################################*
   #### La formule ci dessous est une version plus coourte et econnomique
   # 
     
    # ss <- which(x>= seuil_pluie)
    # 
    # if (x[length(x)] >= seuil_pluie)  ss_sechesse <- c(ss[1], ss[2:length(ss)] - ss[1:(length(ss) - 1)], length(x) - ss[length(ss)]) - 1
    # 
    # else {
    #   ss_sechesse <- c(ss[1], ss[2:length(ss)] - ss[1:(length(ss) - 1)], length(x) - ss[length(ss)] + 1) - 1
    # }
    # 
    # ss_sechesse <- ss_sechesse[ ss_chesse != 0]
    
    if(is_empty(ss)) return(NA)
  
  return (list( maximale = max(ss, na.rm = T),seq_sechesse = ss[ss !=0]))             ### On retourne la valeur maximale de sequence sechesse

}



# dry_spell_2  improved dry spell function --------------------------------



dry_spell_2 <- function(x ,setNA = NaIfToReplace,start = 1, stop = length(x),  IncludeNA = TRUE, DropNA = FALSE) {      #### Une fonction magique pour determiner les sequences sehesses
  
  x <- as.numeric(x)
  x <- x[start:stop]
  
  if (length(x) == 0) return(NA)   ######## Si le vecteur est vide, on retourne un na
  
  if (NAPercent(x) > NaMaxPercent) return (NA)
  
  if(IncludeNA) DropNA = FALSE    ### si on demmande d'inclure les NAS, On ne peut pas laisser tomber les NAS
  if(DropNA) IncludeNA = FALSE    ### Si on demmande d'inclure les NAS, on ne peut pas exclure les NAS
  
  if(DropNA){
    
    x <-  x[!is.na(x)]
    
    
  } else {
    if(!IncludeNA){                   #### 
      x[is.na(x)] <- setNA            #### On remplace les valeurs manquantes par des valeurs a remplacer si 
    }
  }
  #### Si le paramettre a une longueur nulle on return rien
  
  ##### declaration d'un compteurs ci-dessous
  
  count <- 0                   #### Lui son role c'est de compter oui compter les sequences sechesses
  
  ss <- 0                      ### on cree un vecteur qui va combiner les differentes sequences sechesse en un seul vecteur pour
  ### ensuite extraire le maximun
  
  is_na <- FALSE
  
  for (i in 1:length(x)){  
    
    ### boucle de parcours
    if (is.na(x[i])){
      
      ss <- c(ss,count)
      count <- 0 
      is_na = TRUE
      
    } else if (x[i] < seuil_pluie){   ### tester ci la valeur de la pluie vaut ou pas le seuil 
      
      count <- count + 1
      
    } else {
      
      if (is_na){
        ss <- c(ss,NA) 
        is_na = FALSE 
      } 
      ss <- c(ss,count)      ###  Encompassement dans le vecteur
      
      count <- 0             ### Puis on remet le compteur  zero
    }
  }
  
  ss <- c(ss,count)
  
  #  
  
  
  #################################################################################*
  #### La formule ci dessous est une version plus coourte et econnomique
  # 
  
  # ss <- which(x>= seuil_pluie)
  # 
  # if (x[length(x)] >= seuil_pluie)  ss_sechesse <- c(ss[1], ss[2:length(ss)] - ss[1:(length(ss) - 1)], length(x) - ss[length(ss)]) - 1
  # 
  # else {
  #   ss_sechesse <- c(ss[1], ss[2:length(ss)] - ss[1:(length(ss) - 1)], length(x) - ss[length(ss)] + 1) - 1
  # }
  # 
  # ss_sechesse <- ss_sechesse[ ss_chesse != 0]
  
  if(is_empty(ss)) return(NA)
  
  return (list( maxi = max(ss, na.rm = T),dry_spls= ss[ss !=0]))             ### On retourne la valeur maximale de sequence sechesse
  
}



# maxDrySpell -------------------------------------------------------------

maxDrySpell <- function(x,threshold = seuil_pluie, from = 1, to = length(x)) {
  
  vect <- x
  dry_1 <- dry_spell_2(x = vect,setNA = 0.0,IncludeNA = F,DropNA = F, start = from, stop = to)[[1]]
  dry_2 <- dry_spell_2(x = vect,setNA = (threshold + 1.0),IncludeNA = F,DropNA = F, start = from, stop = to)[[1]]
  
  if(any(is.na(c(dry_1,dry_2)))) return(NA)
  
  if (dry_2 == dry_1) return(dry_1) else return(NA)
}



# stress_spell -------------------------------------------------------------


stress_spell = function(x, from = 1, to = length(x), setNA = 0.0, replaceNA = F, dropNA = F,dailyPET = 5.0, soilFull = 70.0) {

  
  # x est le vecteur de pluie
  # from: ou commence les calculs
  # to : ou fini les calculs
  # setNA: valeurs de remplacement des valeurs manquantes si ya lieu
  # replaceNA: indique si les valeurs manquantes seront remplacer ou non
  # dropNA: indique si on laisse tomber les valeurs manquantes ou pas
  # soilFull :  capacité au champs du soil
  # dailyPET: ETP journaliéres 

  x = x[from:to]  # on coupe le vecteur entre les deux extremites
  
  if(NAPercent(x) > NaMaxPercent) return(NA) # si trop de données manquante on ne calcul pas

  if(dropNA) {              # si on indique de suprimer les NA on suprime et on annul le remplacement
  
    x = x[!is.na(x)]
    replaceNA = F           # annulation du remplacement
  
  } else if (replaceNA) x[is.na(x)] = setNA   # sinon si il s'agit d'un replacemnt on l'effectue



  last_was_na = F            # un flag pour gerer les données manquantes
  bilan = 0.0                # variable du bilan hydrique
  stress_count = 0           # compte les jours de stress
  stress_vect = c()          # stoke les sequences de stress
  step = 0                   

  for (i in x) {
	
	  
    step = step + 1
    
    if(is.na(i) & step == 1){            # gere les cas ou ya NA au debut
      #stress_vect = c(stress_vect,NA)
      last_was_na = T
      next
      
    } else if(!is.na(i) & step == 1) {  # gestion de la premiere
      
      last_was_na = F     
    }
    
	  if(is.na(i)) bilan = 0.0  # si on est é une données manquante on annule le bilan hydrique         
	
	  if(is.na(i) & !last_was_na) {  # si la valeur est manquante et le dernier n'est pas une valeur mqnante 
		  stress_vect = c(stress_vect,stress_count) # on ajoute la valeur au vecteur de strees
		  stress_count = 0
		  last_was_na = T
	
	  } 

	  if(!is.na(i) & last_was_na) {     # si la valeur n'est pas manquante et la derniere est maqnante on joute na (on vien dsortir d'une sequence de NA)
		  stress_vect = c(stress_vect,NA)
		  last_was_na = F                 # on leve le flag
	
	  }
	
	  if(!is.na(i)) {                  # si la valeur n'est pas manquante alors on fait le calcul necessaire
		  bilan = sum(c(bilan, i, -dailyPET), na.rm = T) # le bilan est la somme de la pluie + bilan - etp
		
		  if(bilan <= 0.0){              # si le bilan est negatif on on compte comme stress et on maintien le bilan é 0.0
			  stress_count = stress_count + 1 
			  bilan = 0.0
		
		  } else {  
		  
		    stress_vect = c(stress_vect,stress_count)
		    stress_count = 0
		  
		  }
		
		  if (bilan >= soilFull) bilan = soilFull
		
		    last_was_na = F
	
	  }
  }

  if(is.na(i)) stress_vect = c(stress_vect, NA) else stress_vect = c(stress_vect, stress_count)

  stress_vect = stress_vect[stress_vect != 0]

  if(is.null(stress_vect)) return(NA)

  return (stress_vect)

}


# kel_jours function pour trouver un jour particulier ---------------------


##### Le role de cette fonction est de trouver les debut exacte de la saison avec le critere de cumul en trois jours
##### si les valeurs des trois jours sont 0.0,19.1,2.3 alors le debut exacte de la saison est le jours trois
##### puisque la valeur du cumul de 20.0mm est atteinte au troisiemme jour



kel_jours <- function(x, sum_on_3 = sumOn3day) {                    ### Cette founction permet de determiner quel jour exacte des trois
                                              ### Jours debut de saison les 20.0 mm  de pluie sont attientes
  longueur <- length(x)                       ### pour ce faire on recuperre la longueur
  
  for (i in 1:longueur) {
    
    if(sum(x[1:i],na.rm = T) >= sum_on_3 )   ### On supprime les valeurs manquantes avant de faire la somme
    
      return(i-1)
  }
}




# debut_saison: fonction pour trouver la date de debut de saison -----------



debut_saison <- function(x, early = debut, limitDate = length(x)){   ### Cette fonction permet de dterminer la date de debut de la saison
  
  
  if (NAPercent(x) > NaMaxPercent) return (NA)
  #all(is.na(x)) <- return(NA)
  
  debut_debut <- NA                  ### On considere le debut comme etant ignoré
  
  for (i in early:limitDate) {       ### Boucle allant du  debut precoce jsqu'a la fin de l'annee
    
    if( i>= fin) {                   ### si l'on atteint la fin de la saison (1 er septembre) sans trouver le debut de la saison, on abandonne
      return (NA)
    }
    
    sum_3days  = sum(x[i:(i + en_njoursConsecutifs - 1)],na.rm = TRUE)   ### On fait la somme de pluie tombée tous les trois jour en partant d'aujourd'8 jusqu'apres demain
    sum_withNA = sum(x[i:(i + en_njoursConsecutifs - 1)])                ### lorsqu'il ya des valeurs manquantes dans les trois jours, cette somme est NA

    if (sum_3days >= sumOn3day) {    ### Si cette somme est superieur au seuil sur la trois jour,
      
      mon_vect <- x[i:(i+en_njoursConsecutifs -1)]  ### on recupere les donnees de ces trois jours
      
      flag <- kel_jours(mon_vect)                   ### on cherche kel jour exact de ces trois jours les 20.0 mm  son atteints
      
      
      ss_chesse <- dry_spell(x[(i + flag + 1):(i+ start_over_day - 1)])[[1]]      ###### on recupere la sequence sechesses et on 
      
      if (is.na(ss_chesse)) return (NA)         ##### si la sequence sechesse ne p etre calculer on retourne NA
      
      if (ss_chesse <= start_dry_noexced) {     ### on commence la recherche des sequences sechesse sur les trentes jours ki suivent
        
      
        debut_debut <- i + flag                ### Si le debut de la saison depasse fin on retourne NA
      
      if (debut_debut >= fin) return(NA)       
        
        return (debut_debut)
      
        } else if (is.na(sum_withNA)) return(NA) else next
      
    } else if (is.na(sum_withNA)) return(NA) else next
    
  }
  
   if (is.na(debut_debut)) return(NA)
  
}





# fin_saison: fonction pour determiner la fin de la saison ----------------



fin_saison <- function(my_data, fin_precoce = fin, limiteFin = length(my_data),replaceNA = NaIfToReplace) {
  
  x <- my_data
  
  if (NAPercent(x) > NaMaxPercent) return (NA)
  
  bilan <- capacity
  
  count <- 0
  
  later_date <- dateoflastefull(rainVect = x,later = fin_precoce)  #### on cherche la dernieres date de pleine poche
  
  if(is.na(later_date)) return(NA)  ### si la date de poche pleine ne peut etre calculer on revoit NA
  
  
  x <- x[later_date:limiteFin]      ### Je coupe la partie allant de la dernier poche pleine jusqu'a la fin de l'annee: la 
  
  
  
  x[is.na(x)]= replaceNA        ### Je remplace toute les valeur manquante par 0.0mm  c'est ce que fait instat
  
  for (i in x) {                    ### Une boucle pour compter le nombre de jours
    
    count <- count + 1  ## On increment le nombre de jour de 1 a chaque iteration
    
    bilan <- sum(c(bilan ,i ,- daily_etp),na.rm = T) #### on effectue le billan hydrique journalier et on 
    
    if (bilan > capacity) {                         ### On compare la quantité apres bilan a la capacité max du sol
      
      bilan <- capacity                             ### si le bilan est sup a la capacité max, on ramene le bilan a la capacité max
    
      } else if (bilan <= 0.0){                     ### sinon si le bilan est <= é 0 on sort de la boucle
      
        break
    }
  }
  
  return(count + later_date)
}





# begin_to_end: cette fonction permet de trouver la derniére pluie --------



begin_to_end <- function(x, until = (debut_saison(x) + beginToFlower)){
  
  ######################################
  ### Le parametre count permetra de counter le nombre de jours avant debutpls50 ou on a eu la pluie
  ###count <- 0
  ######################################
  #### On recupere la date jusqu'a laquelle la recherche de la ss_debut doit s'arreter
  
  if (is.na(until)) return (NA)
  
  
  return( max(which(x[1:until]> seuil_pluie))) 

  }




# seq_debut: cette fonction permet de retrouver la sequence seches --------



seq_debut <- function(x, debut = debut_saison(x)){
  
  
  if (NAPercent(x) > NaMaxPercent) return (NA)
  
  if (is.na(debut)) return(NA)
  
  
  
  if(is.na(x[debut]) | x[debut] < seuil_pluie) debut <-  begin_to_end(x,until = debut)
  
  #x[is.na(x)] <- NaIfToReplace
  
  debutpls50 <- debut + beginToFlower
  
  return( dry_spell(x[debut:debutpls50])[[1]])
}





# seq_fin: cette fonction de termine la sequence sechesse de fin ----------



seq_fin <- function(x, start = debut_saison(x)) {
  
  if (NAPercent(x) > NaMaxPercent) return (NA)
  
  #if ((debut_saison(x) +beginToFlower) >= fin) return(NA)
  
  if (is.na(start)) return(NA) 
  
  x[is.na(x)] <- NaIfToReplace
  
  debutpls50 <- start + beginToFlower
  
  if (debutpls50 >= fin_saison(x)) return(NA)
  
  return( dry_spell(x[begin_to_end(x):fin_saison(x)])[[1]])
}



# gregory_date: cette fonction permet de convertir une date julien --------



gregory_date <- function(my_date) {  #### Le but de la founction ci dessous est de determiner la date gregorienne correspondante
  
  
  nbjrs_mois <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  
  names(nbjrs_mois) <- c("Jan", "Feb" ,"Mar", "Apr", "May" ,"Jun", "Jul" ,"Aug" ,"Sep", "Oct", "Nov" ,"Dec")
  
  mois <- 0
  
  jour <- 1
  
  cumul <- 0
  
  if (is.na(my_date)) {
    
    return(NA)
    
  }
  
  for( i in nbjrs_mois) {
    
    
      mois <- mois + 1
    
    cumul <- cumul + i
    
    if (cumul == my_date){
      
      jour <- nbjrs_mois[mois]
      
      return (paste0(jour,"-",names(nbjrs_mois)[mois]))
      
    } else if (cumul > my_date ) {
      
        jour <- my_date - sum(nbjrs_mois[1:(mois-1)])
        
        return (paste0(jour,"-",names(nbjrs_mois)[mois]))
    }
  }
  
  
}




# gregoire: cette fonction renvoie un vect de date julien sous for --------



gregoire <- function(mes_dates) {
  
  resultat <- c()
  
  for (i in mes_dates) {
    
    if (is.na(i)) resultat <- c(resultat,NA)
    
    else resultat <- c(resultat, gregory_date(i))
  }
  
  return(resultat)
}




# Annee_bisex: fonction qui test si l'année est bisextile -----------------



Annee_bisex <- function(this_year) {   ####### Cette fonction permet de tester si l'annee est bixectille ou pas
                                       ####### Une annee bixectille est une annee qui est divisible par 4 mais pas par 100
  
  return(((this_year %% 4 == 0) && !(this_year %% 100 ==0)) || (this_year %% 400 == 0))  #### On effectue le test de bisextilité ici
  
}




# dateoflastefull:cette fonction permet de retenir la date de la d --------



dateoflastefull <- function(rainVect, later = fin, start = debut){ ### Cette fonction prend en entree le vecteur de pluie correspondant aux
                                              
                                                            ### aux valeurs d'une annees d'une station données
  
  if (length(rainVect)== 0) {
    return(NA)
  }  
  

  
  if (NAPercent(rainVect[start:later]) > NaMaxPercent){  ##### s'il ya assez de valeur manquantes dans le vecteur on retour la date de fin precosse comme debut de recherche de fin de saison
    
    return (NA)
  }
  
  rainVect[is.na(rainVect)] <- 0.0   
  
  
  

  bilan <- 0.0
  ful_day <- fin
  compteur <- 0
  ful_flag <- 0
  
  for(i in rainVect[1:later]){
    
    
    compteur <- compteur + 1
    bilan <- bilan + (i - daily_etp)
    
    if (bilan < 0.0) {        
      
      bilan <- 0.0               #### Le bilan hydrique ne doit pas etre negatif 
    }
    
    if (bilan >= capacity ){     #### on compare le retenu d'eau dans le sol avec la capacité max du sol
                                 #### si le contenu est superieure ou egal au max on revient au max
      bilan <- capacity
      ful_day <- compteur        ####  puis on retient la date é l'aquelle l'evenement s'est produit comme derniere date
      ful_flag <- ful_flag + 1
          
    }
  }
  
  if (ful_day <= start) ful_day <- later
  if (ful_flag == 0) ful_day <- debut_saison(rainVect)
  if (is.na(ful_day)) ful_day <- start
  
  
  return (ful_day)
}





# Wrangler: cette fonction permet un pretraitement du data frame ----------




Wrangler <- function(Dframe){
  
  
  if (!is.data.frame(Dframe)) return(Dframe)
  
  names(Dframe)[names(Dframe) %in% c("Dates","Date","dates","date","DATE","DATES")][1] = "Date"
  names(Dframe)[names(Dframe) %in% c("ANNEES","Annees","Années","Anneés","Annee","Anné","annees","années","anneés","annee","anné","YEARS","YEAR","Years","Year","year")][1] = "annee"
  names(Dframe)[names(Dframe) %in% c("MOIS","Mois","mois","MONTHS","MONTH","Months","Month","month","months")][1] = "mois"
  names(Dframe)[names(Dframe) %in% c("DAYS","DAY","Days","Day","day","days","JOURS","JOUR","Jours","Jour","jours","jour")][1] = "jour"
  
  
  if (sum(c("annee","mois","jour") %in% names(Dframe), na.rm = TRUE) == 3) {
  if (!"Date" %in% names(Dframe)) Dframe %<>% mutate( Date = str_c(jour, mois, annee, sep = "/"))
  }
  
  
  if ("Date" %in% names(Dframe)) {
    
  Dframe$Date <- lubridate::dmy(Dframe$Date)
  if (!("annee" %in% names(Dframe))) Dframe %<>% mutate(annee = lubridate::year(Date))
  if (!("mois" %in% names(Dframe))) Dframe %<>% mutate(mois = lubridate::month(Date))
  if (!("jour" %in% names(Dframe))) Dframe %<>% mutate(jour = lubridate::day(Date))
    
  }
  
  if (sum(c("annee","mois","jour") %in% names(Dframe), na.rm = TRUE) != 3 & !("Date" %in% names(Dframe))) return(NULL) 
  
  
  if("Date" %in% names(Dframe)) {
     if(class(Dframe$Date) != "Date") Dframe$Date <- lubridate::dmy(Dframe$Date)

  }
  
  
  #Dframe %<>% select(annee, everything()) %>% select(-Date) 
  
  all_years <- unique(Dframe$annee)
  
  if(!any(is.numeric(Dframe$annee))) {
    Dframe$annee %<>%  as.numeric()
  }
  
  filtre <- Dframe %>% group_by(annee) %>% summarise(Nb_jours = n())

  # S'il ya une seule annee dans les donnee, ily a necessité de creer un dataframe 
  #comportant une colonne des annees et une colonne Nb_jours
  
  if(length(all_years) == 1) filtre = data.frame(annee = all_years, filtre)
  
  filtre %<>% mutate(bissex = lubridate::leap_year(annee))  #### On ajoute une colonne montrant si l'annee est bissextile ou non
  
  filtre <- filtre %>% filter((bissex & Nb_jours < 366) | (!bissex & Nb_jours < 365))   #### On retient les annees incomplet: bissex inferieur 366 et simple inf 365
  
  annee_incomplet <- filtre$annee    ###### On recupere les annees incompletes
  
  if (!is_empty(annee_incomplet)) Dframe %<>% filter( !(annee %in% annee_incomplet) )  #### On trie pour ne retenir que les annee comportant des annees complet
  
  Dframe %<>% select(-c(jour,mois,Date)) %>% select(annee, everything())
  
  result <- list( yeartodelete = annee_incomplet, dataframe = Dframe) ##### liste de sortie
  
  
  
  return(result)
}



##################################################################################################################################

####   LA FONCTION CI DESSOUS EST CELLE KI PERMET DE DETERMINER LES DIFFERENT PARAMETRES

###################################################################################################################################"
########### La fonction ci-dessous est faite pour les calculs


calcParameters <- function(rain_data,constantes, variete) {


# Affectations des variables é leurs valeurs consecutifs

debut <<- as.integer(constantes[1])
sumOn3day <<- constantes[2]
en_njoursConsecutifs <<- as.integer(constantes[3])
start_dry_noexced <<- as.integer(constantes[4])
start_over_day <<- as.integer(constantes[5])
beginToFlower <<- as.integer(constantes[6])
capacity <<- constantes[7]
fin <<- constantes[8]
seuil_pluie <<- constantes[9]
daily_etp <<- constantes[10]



	
nom_station <- names(rain_data)[2:ncol(rain_data)] ##### On recupere d'abord les nom des stations rien que les nous de station  

################################################################
### On recupere toute les annees de la base de donnees

years <- unique(rain_data$annee)   ############################# On recupere toute les annees disponibles dans les donnees

################################################################

stations <- c()                  ### Un joli vecteur pour contenir les noms des stations
annee <- c()                     ### En voici un autre pour contenir les annee
date_debut <- c()                ###  ...
ss_debut <- c()                  ###  ...
date_floraison <- c()            ###  ...
date_fin <- c()                  ###  ...
ss_fin <- c()                    ### Et enfin les date de fin dans un beau vecteur



for( each in years){


################################################################
### On recupere les donnees pour une seule annees et on les mets dans un dataframe


  this.year_data <- rain_data %>% filter(annee  == each) #### cette ligne permet de filtrer les donnee et de
                                                         #### Retenir uniquement les donnees d'une seule annees


######## ensuite, on test si l'annee est n'est pas bisextile et si le nombre de ligne deja en place (nbr de jour) ne vaut pas 366
###### puis on ajoute une ligne de donnees manquantes
  

  if(!(Annee_bisex(each)) && (nrow(this.year_data) < 366)) 
    
  {
  
  this.year_data <-  add_row(.data = this.year_data, .after = 59)  #### On ajoute une ligne de donnee manquante
  
  this.year_data$annee[60] = each ##### On remplace le numero de 'annee

}

  if(nrow(this.year_data) < 366) next  ##### Si les jours disponibles ne sont pas complet on passe a l'annee suivante
  
  
  
  for (element in nom_station) {   #### On initialise une boucle pour parcourir les element de chaque station
  
  x <- this.year_data[[element]]        ##### On recupere les donnees de pluies d'une station donnees pour une annees entiere
  
  stations <- c(stations,element)  ##### On ajoute dans le vecteur station le nom de la station
  
  annee <- c(annee,each)          ##### On ajoute l'annee corespondante
  
  date_debut <- c(date_debut,debut_saison(x))  ###### On ajoute au vecteur des dates de debut de saison le debut de la saison
  
  date_fin <- c(date_fin,fin_saison(x))        ###### On ajoute a la date de fin de saison, la fin de la saison correspondante
  
  ss_debut <- c(ss_debut,seq_debut(x))         ###### Puis ensuite les sequences sechesse de debut de saison
  
  ss_fin <- c(ss_fin,seq_fin(x))               ###### Puis enfin les sequences sechesses de fin de saison
  
}

}


###############################################################################################"""
### AUTRES PERSPECTIVE DE CLACULE DES PARAMETRES
# for( each in years){
#   
#   
#   ################################################################
#   ### On recupere les donnees pour une seule annees et on les mets dans un dataframe
#   
#   
#   this.year_data <- rain_data %>% filter(annee  == each) #### cette ligne permet de filtrer les donnee et de
#   #### Retenir uniquement les donnees d'une seule annees
#   
#   
#   ######## ensuite, on test si l'annee est n'est pas bisextile et si le nombre de ligne deja en place (nbr de jour) ne vaut pas 366
#   ###### puis on ajoute une ligne de donnees manquantes
#   
#   
#   if(!(Annee_bisex(each)) && (nrow(this.year_data) < 366)) 
#     
#   {
#     
#     this.year_data <-  add_row(.data = this.year_data, .after = 59)  #### On ajoute une ligne de donnee manquante
#     
#     this.year_data$annee[60] =each ##### On remplace le numero de 'annee
#     
#   }
#   
#   if(nrow(rain_data) < 366) next  ##### Si les jours disponibles ne sont pas complet on passe a l'annee suivante
#   
#   
#   date_debut <- apply(this.year_data,2,debut_saison) ###### On ajoute au vecteur des dates de debut de saison le debut de la saison
#   
#   date_fin <- apply(this.year_data,2,fin_saison) ###### On ajoute a la date de fin de saison, la fin de la saison correspondante
#   
#   ss_debut <- apply(this.year_data,2,seq_debut)         ###### Puis ensuite les sequences sechesse de debut de saison
#   
#   ss_fin <- apply(this.year_data,2,seq_fin)
# 
#   
# }
# 

###########################################################################################

parametres_saison <- tibble(stations,annee,date_debut,date_fin,ss_debut,ss_fin)  #### On cree a la sortie de la boucle un data.frame ou l'on conserve le tous


parametres_saison %<>% mutate (greg_debut = gregoire(date_debut),greg_fin = gregoire(date_fin),longueur_saison = date_fin - date_debut)  ##### On joute de nouvelle colonnes indiquant les dates de fin et de debut en dates gregorien

######################################################################################
##### CONVERSION DES CARACTERES EN VRAI DATE
# LA LIGNE CI DESSOUS PERMET DE CONVERTIR LES CARATERES EN DATE

 parametres_saison$annee <- as.integer(parametres_saison$annee)
 
 Convert_date <- paste0(parametres_saison$greg_debut,'-',parametres_saison$annee)
 lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
 Convert_date <- as.Date(Convert_date,"%d-%b-%Y")
 
############################################################################################"
#### On separe les differents parametres calculées pour les ecrires table par table
 
tab_debut <- parametres_saison %>% select(annee,stations,date_debut) %>% spread(stations, date_debut)

write.xlsx(as.data.frame(tab_debut), file = paste0("Resultat/debut_saison_",variete,".xls"), sheetName = "Debut saison",row.names = F)

tab_fin <- parametres_saison %>% select(annee,stations,date_fin) %>% spread(stations, date_fin)

write.xlsx(as.data.frame(tab_fin), file = paste0("Resultat/fin_saison_",variete,".xls"), sheetName = "Fin saison",row.names = F)

tab_ssdebut <- parametres_saison %>% select(annee,stations,ss_debut) %>% spread(stations, ss_debut)

write.xlsx(as.data.frame(tab_ssdebut), file = paste0("Resultat/ss_debut_",variete,".xls"), sheetName = "Sequences sechesse debut",row.names = F)

tab_ssfin <- parametres_saison %>% select(annee,stations,ss_fin) %>% spread(stations, ss_fin)

write.xlsx(as.data.frame(tab_ssfin), file = paste0("Resultat/ss_fin_",variete,".xls"), sheetName = "Sequences sechesse fin",row.names = F)

tab_longueur_saison <- parametres_saison %>% select(annee,stations,longueur_saison) %>% spread(stations, longueur_saison)

write.xlsx(as.data.frame(tab_longueur_saison), file = paste0("Resultat/longueur_saison_",variete,".xlsx"), sheetName = "Longueur saison",row.names = F)


write.xlsx(as.data.frame(parametres_saison), file = paste0("Resultat/OUTPUT",variete,".xls"), row.names = FALSE)  ##### ecriture du fchier de sortie du data.frame



}

