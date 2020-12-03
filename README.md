# Les Functions

Le fichier __Fonction.r__ contient la liste des fonctions parmetant de calculer les caractériques agro-climatiques de la saison des cultures.

__debut_saison() :__ permet de determiner le debut de la saison pour une station et pour une annee donnees
Cette fonction prend 03 arguments:
- __x__ un vecteur de données. eventuellement un vecteur contenant les données de pluie pour une année et pour une station.
- __early__ correspond à la date precoce de debut de saison eventuellement le jour ou commence la recherche. Cette valeur est en date julienne (le nombre de jours ecoulés depuis le debut de l'année.
- __limiteDate__ cette valeur correspond à la date la plus tardive de debut de la saison. si aucun debut de saison n'est detecté avant cette date la fonction retourne __NA__.

__fin_saison() :__  permet de determiner la fin de la saison.
Cette fonction accepte 04 arguments:
- __my_data__ le vecteur de données
- __fin_precoce__ la date de fin de saison la plus précoce pouvant être donnée en se referant à la climatologie.
- __limiteFin__ date julienne correspondant à la date de fin la plus tardive possible. la recherche s'arrête après cette date. Par defaut elle correspond à la longueur de l'argument __my_data__.
- __replaceNA__ valeur de remplacement des valeurs manquantes.

__dry_spell() :__ permet de determiner les sequence sechesses de facon generale sur une partie de l'annee
Cette fonction prend en entrée un vecteur de valeur et renvoie la sequence sechesse.
___Remarque__: cette fonction ne renvoie pas uniquement la sequence seches maximale mais toute les sequences seches dans une liste de deux elment dont le premier est la maximale ce qui donne la possibilité de deduire d'autres statistiques autres que la maximale._

__dry_spell_2():__ Version amelioré de dry_spell() avec des options suplementaires.
Cette fonctions prend les arguments suivants:
- __x__ le vecteur de valeurs.
- __setNA__ valeur fornie en remplacement des valeurs manquantes. par defaut elle est egale à cette défini globalement __NaIfToReplace__.
- __start__ debut de recherche de la sequence seches. valeur par defaut est egale à 1.
- __stop__ égale à la longueur du vecteur par defaut.
- __IncludeNA__ indique si les valeurs manquantes sont inclusent dans la sortie.
- __DropNA__ indique si les valeurs manquantes sont suprimés dans le vecteur initial.


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


