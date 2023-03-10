# IFT-2003-1

## À propos du projet
Le but de ce code est d'explorer different algorithmes et heuritiques afin de gagner une partie du jeu de gomoku.

### Heuristiques
Les 3 algorithmes implémentés dans ce travail explorent un arbre de décision en évaluant une partie des états possible lors d'un jeu.

Le code source de l'algorithme peut être trouvé au lien suivant: https://www.emse.fr/~picard/cours/ai/minimax/minimax.pl

#### minimax
Algorithme évaluant la totalité de l'arbre décisionnel afin de choisir la meilleure solution possible. 

#### alpha-beta
Un algorithme de recherche dont le but est de réduire le nombre de noeuds explorés par l'algorithme minimax.

Le code source d3e l'algorithme peut être trouvé au lien suivant à la page 366:  https://silp.iiita.ac.in/wp-content/uploads/PROLOG.pdf

#### alpha-beta borné
Algorithme alphabeta auquel des contraintes de temps et de profondeur ont été ajoutées.

### Pour lancer le projet
Ouvrez une fenêtre swi-prolog et entrez la commande suivante :
```ijprolog
working_directory(CWD, 'path/to/IFT-2003-1').
```
notez que `path/to/IFT-2003-1` est le path jusqu'au dossier dans lequel le readme se trouve.

#### Pour jouer une partie contre un agent ou faire jouer 2 agents l'un contre l'autre selon différentes configurations
```ijprolog
consult('gomoku.pl').
```
Puis suivez les instructions apparaissant à l'écran

#### Pour lancer les divers tests unitaires
vous pouvez les lancer à l'aide des commandes suivantes :

Pour le damier :
```ijprolog
consult('src/tests/board.pl').
run_tests.
```

Pour l'algorithme alpha-beta :
```ijprolog
consult('src/tests/board.pl').
run_tests().
```

Pour l'algorithme alpha-beta borné (ou encore avec contraintes) :
```ijprolog
consult('src/tests/board.pl').
run_tests.
```

POur les tests associés à l'évaluation statique du score :
```ijprolog
consult('src/tests/board.pl').
run_tests.
```

### Pour générer des statistiques sur les scores
Il est possible de faire jouer deux agents l'un contre l'autre. lorsque vous ex/cutez cette commande un fichier évaluation.cache est produit. ce fichier contient le score statique, le score heuristique et l'identifiant des joueurs à chaque tours. Vous pouvez vous servir de ces donnés afin d'évaluer la performance de diverse configuration selon divers damiers. 

