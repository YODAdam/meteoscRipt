
###################################################################################
### Ce script contient ces fuctions 
### debut_saison() : permet de determiner le debut de la saison pour une station et pour une annee donnees
### fin_saison() :  permet de determiner la fin de la saison
### dry_spell() :  permet de determiner les sequence sechesses de facon generale sur une partie de l'annee
### seq_debut () : permet de determiner les sequences de debut de saison
### seq_fin()    : permer de determiner les sequence sechesse de fin de saison
### begin_to_end() : permet de dterminer la date exacte a partir de laquelles les sequence sechesse de fin sont recherchÃ©s
### dateoflasteful(): cette fonction permet de determiner la derniere date avant la date la plus tardive a laquelle on a boserver la capacité max au sol
### kel_jours(): cette fonction permet de determiner quel jours exacte parmis les trois jours de ebut de saison les 20.0 mm  sont atteintes
####################################################################################

dry_spell <- function(x){      #### Une fonction magique pour determiner les sequences sehesses
  
  x[is.na(x)] <- 0.0             #### On remplace les valeurs manquantes par des valeurs nulles
  
  if (length(x) == 0) return(NA)   #### Si le paramettre a une longueur nulle on return rien
  
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
      
        count <- 0             ### Puis on remet le compteur Ã  zero
    }
  }
  
  ss <- c(ss,count)            ###
  
  return (max(ss))             ### On retourne la valeur maximale de sequence sechesse

  }


##data = read_xls("C:/Users/inspiron17/Desktop/presass2019/pluvio_2018_appel.xls",na = c("","8888","9988"))

##############################################################################
##### Le role de cette fonction est de trouver les debut exacte de la saison avec le critere de cumul en trois jours
##### si les valeurs des trois jours sont 0.0,19.1,2.3 alors le debut exacte de la saison est le jours trois
##### puisque la valeur du cumul de 20.0mm est atteinte au troisiemme jour

kel_jours <- function(x) {                    ### Cette founction permet de determiner quel jour exacte des trois
                                              ### Jours debut de saison les 20.0 mm  de pluie sont attientes
  longueur <- length(x)                       ### pour ce faire on recuperre la longueur
  
  for (i in 1:longueur) {
    
    if(sum(x[1:i],na.rm = T) >= sumOn3day )   ### On supprime les valeurs manquantes avant de faire la somme
    
      return(i-1)
  }
}





debut_saison <- function(my_data){   ### Cette fonction permet de dterminer la date de debut de la saison
  
  x <- my_data                       
  
  debut_debut <- NA                  ### On considere le debut comme etant ignorÃ©
  
  for (i in debut:length(x)) {       ### Boucle allant du grand debut jsqu'a la fin de l'annee
    
    if( i>= fin) {                   ### si l'on atteint la fin de la saison (1 er septembre) sans trouver le debut de la saison, on abandonne
      return (NA)
    }
    
    sum_3days = sum(x[i:(i+en_njoursConsecutifs -1)],na.rm = TRUE)   ### On fait la somme de pluie tombÃ©e tous les trois jour en partant d'aujourd'8 jusqu'apres demain
    
    if (sum_3days >= sumOn3day) {    ### Si cette somme est superieur au seuil sur la trois jour,
      
      mon_vect <- x[i:(i+en_njoursConsecutifs -1)]  ### on recupere les donnees de ces trois jours
      
      flag <- kel_jours(mon_vect)                   ### on cherche kel jour exact de ces trois jours les 20.0 mm  son atteints
      
      
      if (dry_spell(x[(i+flag+1):(i+start_over_day-1)]) <= start_dry_noexced) { ### on commence la recherche des sequences sechesse sur les trentes jours ki suivent
        
      
        debut_debut <- i + flag                ### Si le debut de la saison depasse fin on retourne NA
      
      if (debut_debut >= fin) return(NA)       
        
        return (debut_debut)
      
        } else next
      
    } else next
    
  }
  
   if (is.na(debut_debut)) return(debut_debut)
  
}



fin_saison <- function(my_data) {
  
  x <- my_data
  
  bilan <- capacity
  
  count <- 0
  
  later <- dateoflastefull(x,fin)
  
  x <- x[later:length(x)] #### Je coupe la partie allant du premier septembre jusqu'a la fin de l'annee: la 
  
  ##########################  recherche de la fin de la saison va se faire Ã  ce niveau
  
  x[is.na(x)]= 0     ###### Je remplace toute les valeur manquante par 0.0mm  c'est ce que fait instat
  
  for (i in x) {   ######## Une boucle pour compter le nombre de jours
    
    count <- count + 1  ## On increment le nombre de jour de 1 a chaque iteration
    
    bilan <- sum(c(bilan ,i ,- daily_etp),na.rm = T) ####â˜º on effectue le billan hydrique journalier et on 
    
    if (bilan > capacity) {                         ### On compare la quantitÃ© apres bilan a la capacitÃ© max du sol
      
      bilan <- capacity                             ### si le bilan est sup a la capacitÃ© max, on ramene le bilan a la capacitÃ© max
    
      } else if (bilan <= 0.0){                     ### sinon si le bilan est <= Ã  0 on sort de la boucle
      
        break
    }
  }
  
  return(count + later)
}



begin_to_end <- function(x){
  
  ######################################
  ### Le parametre count permetra de counter le nombre de jours avant debutpls50 ou on a eu la pluie
  ###count <- 0
  ######################################
  #### On recupere la date jusqu'a laquelle la recherche de la ss_debut doit s'arreter
  
  if (is.na(debut_saison(x))) return (NA)
  
  debutpls50 <- debut_saison(x) + 50
  
  return( max(which(x[1:debutpls50]>0)))

  }



seq_debut <- function(my_data){
  
  
  x <- my_data
  
  
  if (is.na(debut_saison(x))) return(NA) 
  
  x[is.na(x)] <- 0.0
  
  debutpls50 <- debut_saison(x) + 50
  
  return( dry_spell(x[debut_saison(x):debutpls50]))
}



seq_fin <- function(my_data){
  
  x <- my_data
  
  #if ((debut_saison(x) +50) >= fin) return(NA)
  
  if (is.na(debut_saison(x))) return(NA) 
  
  x[is.na(x)] <- 0.0
  
  debutpls50 <- debut_saison(x) + 50
  
  if (debutpls50 >= fin_saison(x)) return(NA)
  
  return( dry_spell(x[begin_to_end(x):fin_saison(x)]))
}


######################################################################"
#### Le but de la founction ci dessous est de determiner la date gregorienne correspondante

gregory_date <- function(my_date) {
  
  
  nbjrs_mois <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  
  names(nbjrs_mois) <- c("Jan", "Feb" ,"Mar", "Apr", "May" ,"Jun", "Jul" ,"Aug" ,"Sep", "Oct", "Nov" ,"Dec")
  
  #greg_date <- c()
  #resultat <- ""
  
  #for (donnee in my_date) {
  
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


gregoire <- function(mes_dates) {
  
  resultat <- c()
  
  for (i in mes_dates) {
    
    if (is.na(i)) resultat <- c(resultat,NA)
    
    else resultat <- c(resultat, gregory_date(i))
  }
  
  return(resultat)
}



###########################################
### DETERMINER SI L'ANNEE EST BISEXTILE OU PAS POUR RESOUDRE LE CAS DU 29 FEVRIEER
###########################################


Annee_bisex <- function(this_year) {   ####### Cette fonction permet de tester si l'annee est bixectille ou pas
                                       ####### Une annee bixectille est une annee qui est divisible par 4 mais pas par 100
  
  return((this_year %% 4 == 0) && (!(this_year %% 100 ==0) || (this_year %% 400 == 0)))  #### On effectue le test de bisextilitÃ© ici
  
}


dateoflastefull <- function(rainVect, later){ ### Cette fonction prend en entree le vecteur de pluie correspondant aux
                                              ### aux valeurs d'une annees d'une station données
  
  if (length(rainVect)== 0) {
    return(NA)
  }  
    
  rainVect[rainVect<= seuil_pluie] <- 0.0     ### On concidere comme pluie tous valeur de pluie au dela de 0.85 mm
                                              ### toute les autres valeurs en dessous sont remplacées par 0.0
  vect_1 <- rainVect[1:debut]
  
  vect_1[is.na(vect_1)] <- 0.0
  
  rainVect[1:debut] <- vect_1
  
  ##rainVect[is.na(rainVect[1:debut])] <- 0.0   ### On remplace toutes les valeurs manquantes de 1 à debut par 0.0
  
  if (any(is.na(rainVect[debut:later]))){
    
    return (NA)
  }
  ##cumul <-  0.0
  bilan <- 0.0
  ful_day <- fin
  compteur <- 0
  
  for(i in rainVect[1:later]){
    
    ##cumul = cumul + i
    compteur <- compteur + 1
    bilan <- bilan + (i - daily_etp)
    
    if (bilan < 0.0) {        
      
      bilan <- 0.0                 ### Le bilan hydrique ne doit pas etre negatif 
    }
    
    if (bilan > capacity ){
      
      bilan <- capacity
      ful_day <- compteur
      
    }
  }
  
  
  return (ful_day)
}

