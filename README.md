# Les Functions

Le fichier __Fonction.r__ contient la liste des fonctions parmetant de calculer les caractériques agro-climatiques de la saison des cultures.

__debut_saison() :__ permet de determiner le debut de la saison pour une station et pour une annee donnees
Cette fonction prend 03 arguments:
- __x__ un vecteur de données. eventuellement un vecteur contenant les données de pluie pour une année et pour une station.
- __early__ correspond à la date precoce de debut de saison eventuellement le jour ou commence la recherche. Cette valeur est en date julienne (le nombre de jours ecoulés depuis le debut de l'année.

__fin_saison() :__  permet de determiner la fin de la saison

__dry_spell() :__ permet de determiner les sequence sechesses de facon generale sur une partie de l'annee

__dry_spell_2():__ Version amelioré de dry_spell() avec des options suplementaires

__maxDrySpell():__ Cette fonction permet de renvoyer la sequence sechesse dans certaines situations malgre les données manquantes

__stress_spell():__ cette fonction permet de retrouver les sequences de jours de stress hydrique pour un vecteur de pluie donnée

__seq_debut () :__ permet de determiner les sequences de debut de saison

__seq_fin()    :__ permer de determiner les sequence sechesse de fin de saison

__begin_to_end() :__ permet de dterminer la date exacte a partir de laquelles les sequence sechesse de fin sont recherchés

__dateoflasteful():__ cette fonction permet de determiner la derniere date avant la date la plus tardive a laquelle on a boserver la capacité max au sol

__kel_jours():__ cette fonction permet de determiner quel jours exacte parmis les trois jours de ebut de saison les 20.0 mm  sont atteintes

__Wrangler():__ cette fonction met de traiter le data frame affin d 'eviter toute erreure possible

__NAPercent():__ cette fonction permet de renvoyer le pourcentage de donnees manquantes

__gregorie_date():__ cette fonction permet de convertir une date julienne en date gregorienne

__gregoire():__ Cette fonction permet de convertir un vecteur de date du julien au gregorien

__Annee_bisex():__ cette fonction permet de retourner un boulean T ou F montrant si une annee est bissextile ou pas

__Dateoflastefill():__ Cette fonction permet de retourner la date en julien de la dernier poche pleine avant la date la plus precoce de fin de saison precisee

__calParameters():__ cette fonction permet de prendre en entree un dataframe, un vecteur, et une chaine de caractere.


On test si centaines varibles ne sont pas saisai par l'utilisateur on les defini


