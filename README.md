# IFT-2003-1
<pre>
        ██████   ██████  ███    ███  ██████  ██   ██ ██    ██
       ██       ██    ██ ████  ████ ██    ██ ██  ██  ██    ██
       ██   ███ ██    ██ ██ ████ ██ ██    ██ █████   ██    ██
       ██    ██ ██    ██ ██  ██  ██ ██    ██ ██  ██  ██    ██
        ██████   ██████  ██      ██  ██████  ██   ██  ██████ 

Bienvenu dans Gomoku!  
Gomoku est un jeu de stratégie pour deux joueurs.  
Le but du jeu est de placer cinq pions consécutifs en ligne  
horizontalement, verticalement ou en diagonale, sur le grillage  
Chaque joueur place à tour de rôle un pion sur le plateau.  
Le joueur ayant les pions noirs débute la partie.  
Le premier joueur à atteindre 5 pions consécutifs gagne la partie.  

╔══════════════════════════════════════════════════════════════════╗  
║ Voici les options disponibles:                                   ║  
║                                                                  ║  
║ 1 - Jouer à Gomoku sur un plateau 11×11.                         ║  
║ 2 - Jouer à Gomoku sur un plateau 15×15.                         ║  
║ 3 - Jouer à Gomoku sur un plateau 19×19.                         ║  
║ 4 - Jouer à une version complètement paramétrable de Gomoku.     ║  
║ 5 - Jouer à Tic-Tac-Toe (Bonus).                                 ║  
║ Q - Quitter.                                                     ║  
║                                                                  ║  
╚══════════════════════════════════════════════════════════════════╝  
</pre>

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
consult('src/tests/heuristic_evaluation_tests.pl').
run_tests.
```

Pour l'algorithme alpha-beta :
```ijprolog
consult('src/tests/alphabeta_tests.pl').
run_tests().
```

Pour l'algorithme alpha-beta borné (ou encore avec contraintes) :
```ijprolog
consult('src/tests/bounded_alphabeta_tests.pl').
run_tests.
```

POur les tests associés à l'évaluation statique du score :
```ijprolog
consult('src/tests/static_evaluation_tests.pl').
run_tests.
```

### Analyse
Dans le devoir il est demand/ de faire le jeu en prolog, mais l<analyse peut être faite dans le langage de notre choix ;)

C'est pourquoi l'analyse est faite dans un jupyter notebook

Pour installer l'environnement:
```
conda env create -f analysis/ia_course_hw_1_env.yml
```

Puis allez dans le dossier d'analyse dans votre terminal et lancer jupyterlab!
la commande est `jupyter-lab`