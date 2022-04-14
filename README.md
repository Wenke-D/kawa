# Kawa
Implémentation de l'OOP mécanisme en manipulant la représentation intermédiaire.

projet universitaire pour le cours Compilation

M1 QDCS, Université Paris-Saclay

## Structure de projet

Dans le répertoire `src`, vous trouverez le code source et les fichiers auxiliaires.

Le fichier `rapport.md` est le rapport de ce projet.


## Tests

Dans le répertoire `src/test`, il y a des codes de kawa pour tester le compilateur, l'objectif de chacun fhichier de test est écrit en commentaire à la première ligne.


## makefile

Il y a un fichier **makefile** se situe au `src` qui contient des procédures utiles.

- `make` produira le compilateur **kawac** et l'intereprèteur **kawai** dans `src`

- `make clean` supprimera 
    1. le répertoire **_build** produit par ocamlbuild durant la compilation.
    2. tous les fichier avec extension **.pmp** dans le répertoire test
    3. les programes binaires **kawac.native**, **kawai.native**, **kawac** et **kawai**


## Codes Ocaml Supplémentaires

A part des codes fournits au début, le fichier `aux.ml` contients des functions auxiliaires qui est utilisées dans le fichier `kawa2pimp.ml`

le fichier `aux_type.ml` contient des definition de type utilisé dans les fichiers `kawa2pimp.ml` et `aux.ml`.

Donc non cycle de dépendance
