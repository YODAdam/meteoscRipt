

##############
### Veuillez specifier le dossier de traille ci_dessous
### N.B Le dossier de travail doit etre un docier contenant les donnes de vos stations dans un fichiers excel 97-2003

### working_dir <- "C:/Users/inspiron17/OneDrive/PROJETS R/PROJET_SAISON_PARAMETERS"


setwd(working_dir)   ##### Ici on defini notre repertoir de travail par defaut


###############################################################
### sheet_names <- excel_sheets(path = final_path)

rain_data <- read_xls(path = rain_file) ####Lecture du fichier excel 


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
  
  this.year_data$annee[60] =each ##### On remplace le numero de 'annee

}

  if(nrow(rain_data) < 366) next  ##### Si les jours disponibles ne sont pas complet on passe a l'annee suivante
  
  
  
  for (element in nom_station) {   #### On initialise une boucle pour parcourir les element de chaque station
  
  x <- rain_data[[element]]        ##### On recupere les donnees de pluies d'une station donnees pour une annees entiere
  
  stations <- c(stations,element)  ##### On ajoute dans le vecteur station le nom de la station
  
  annee <- c(annee,each)          ##### On ajoute l'annee corespondante
  
  date_debut <- c(date_debut,debut_saison(x))  ###### On ajoute au vecteur des dates de debut de saison le debut de la saison
  
  date_fin <- c(date_fin,fin_saison(x))        ###### On ajoute a la date de fin de saison, la fin de la saison correspondante
  
  ss_debut <- c(ss_debut,seq_debut(x))         ###### Puis ensuite les sequences sechesse de debut de saison
  
  ss_fin <- c(ss_fin,seq_fin(x))               ###### Puis enfin les sequences sechesses de fin de saison
  
}

}

parametres_saison <- tibble(stations,annee,date_debut,date_fin,ss_debut,ss_fin)  #### On cree a la sortie de la boucle un data.frame ou l'on conserve le tous


parametres_saison %<>% mutate (greg_debut = gregoire(date_debut),greg_fin = gregoire(date_fin),longueur_saison = date_fin - date_debut)  ##### On joute de nouvelle colonnes indiquant les dates de fin et de debut en dates gregorien

# LA LIGNE CI DESSOUS PERMET DE CONVERTIR LES CARATERES EN DATE

# parametres_saison$annee <- as.integer(parametres_saison$annee)
# 
# Convert_date <- paste0(parametres_saison$greg_debut,'-',parametres_saison$annee)
# lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
# Convert_date <- as.Date(Convert_date,"%d-%b-%Y")

write.xlsx(as.data.frame(parametres_saison), file = "OUTPUT.xlsx", row.names = FALSE)  ##### ecriture du fchier de sortie du data.frame

