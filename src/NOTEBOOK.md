**Carnet de notes**
===================

*Général*
---------

L'arborescence des fichiers reflète les librairies que l'on pourrait 
*moralement* utiliser hors du projet. On essaie de fournir des *feuilles de 
calcul* pour tester les librairies qui introduisent de nouveaux types.  

*Détails de l'arborescence des fichiers*
----------------------------------------

* legacy:
les reliques du logiciel ALCOOL.

* regions: traitement des régions isothétiques et cylindriques.

* languages: langages d'entrée de l'analyseur (actuellement le langage _PV_, 
une forme dégénérée de celui introduit par Dijkstra, bientôt un autre langage 
dont la syntaxe est inspirée de celle du langage _C_). 

*À faire*
----------------------------------------

* DashDot à bornes entières pour pouvoir effectuer des tests.

* Une version «simplifiée» des fonctions *future extension* et *past extension* 
dans DashDot.

* L'invariant à maintenir pour DashDot.t est la croissance stricte des valeurs 
attachées aux bornes de la liste. Pour «renverser» le temps, il suffit de 
changer la fonction compare en son opposée. L'idée est alors de calculer 
past_extension avec future_extension (et ainsi de s'épargner quelques centaines 
de lignes de code). Pour ce faire on retourne les arguments, on renverse 
l'ordre sur les bornes en prenant soin du cas où il y a des composantes non 
bornées, on appelle la version récursive de la fonction future_extension, on 
renverse le résultat et on rétablit l'ordre initial.

* Peut-on écrire future_extension à l'aide de binary_boolean_operator, 
future_closure, et past_closure ?

* Ajouter un parser au système de test de manière à pouvoir écrire des valeurs 
de type DashDot(Integer).t à partir de chaînes de caractères. 

* Adapter Circle.past_extension et HalfLine.past_extension de manière à ce 
qu'il utilise la version récursive de future_extension. Puis nettoyer le code.

* On peut sans doute simplifier future_extension en décomposant une valeur de 
type DashDot(Integer).t 

* L'exception Undefined est utile dans le cas d'opérateur qui ne sont définis 
que pour HalfLine ou Circle, comme par exemple lub pour des valeurs de type 
DashDot(Integr).t qui ont une composante connexe finie.
