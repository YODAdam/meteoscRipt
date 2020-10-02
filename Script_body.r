

###############################################################
### sheet_names <- excel_sheets(path = final_path)

if(file.exists(rain_file)){ rain_data_all <- read_excel(path = rain_file,sheet = 1,na = c("","NA","TR","Tr","tr","tR","-999.9","999")) ####Lecture du fichier excel 
} else {
  q()           ####### lorque le fichier principal n'existe pas on arrete le programme
}


################################################################################
##### ci dessous on fait le traitement du data frame

rain_data <- Wrangler(rain_data_all)

if(is.list(rain_data)) rain_data <- rain_data[[2]]  ##### si le wrangler renvoie une liste on recuppere le data frame quie est dans la liste


### on defini ci-dessous les constantes à utiliser pour le traitements
const <- c(debut,
           sumOn3day,
           en_njoursConsecutifs,
           start_dry_noexced,
           start_over_day,
           beginToFlower,
           capacity,
           fin,
           seuil_pluie,
           daily_etp
           )



###############################################################################
## 
if(file.exists(Especies_file)) {
  
  especes <- read.xlsx(Especies_file,1)
  
 for( i in 2:ncol(especes)) {
    
   calcParameters(rain_data = rain_data,constantes = especes[[i]],variete = names(especes)[i])
 }
  
} else {
  
  calcParameters(rain_data = rain_data, constantes = const, variete = "Default")
}