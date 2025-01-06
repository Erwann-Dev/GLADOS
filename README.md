# Projet GLADOS - README
<div align="center">
    [![All Contributors](https://img.shields.io/badge/all_contributors-5-green.svg?style=flat-circle)](#Contributors)
    [![Language Use](https://img.shields.io/badge/Language_Use-Haskell-purple.svg?style=flat-circle)](#LanguageUse)
</div>


## 📝 Description
Ce projet est une implémentation d'un langage de programmation simple inspiré de Lisp. Il inclut :

- Une représentation des expressions à l'aide d'un arbre syntaxique abstrait (AST).
- Un parseur capable d'interpréter des chaînes de caractères en expressions.
- Un moteur d'évaluation pour exécuter les expressions.
- Des outils utilitaires pour gérer les entrées/sorties et afficher de l'aide.

Ce projet est conçu pour une exploration éducative des concepts d'analyse syntaxique, d'évaluation d'expressions et de gestion d'environnements dynamiques.


## Fonctionnalités principales

### 1. **Représentation des expressions (AST)**
Le fichier `AST.hs` contient la définition de l'arbre syntaxique abstrait (AST) pour le langage. Il supporte :
- Littéraux numériques et booléens
- Opérations arithmétiques et logiques
- Structures conditionnelles
- Variables et fonctions lambda
- Listes et séquences d'expressions

### 2. **Évaluation des expressions**
Le fichier `ASTEval.hs` implémente un moteur d'évaluation permettant d'exécuter les expressions définies dans l'AST. Il gère :
- Les calculs arithmétiques
- Les comparaisons
- Les affectations et définitions de variables
- Les fonctions et leur application

### 3. **Analyse syntaxique (Parsing)**
Le fichier `ASTParser.hs` fournit des outils pour convertir des chaînes de caractères en expressions AST. Il inclut :
- Un parseur pour les constantes, variables et opérations
- Le support des blocs et des fonctions
- Une syntaxe Lisp-like simplifiée

### 4. **Outils utilitaires**
Le fichier `Utils.hs` offre des fonctions utilitaires, notamment :
- Lecture de fichiers avec gestion des erreurs
- Affichage d'une aide à l'utilisation

### 5. **Configuration et Compilation**
Le fichier `Setup.hs` configure le projet pour une compilation simple à l'aide de `Cabal`.


## Installation et utilisation

### Prérequis
- **Haskell** : Assurez-vous que GHC et Cabal sont installés sur votre machine.

### Compilation
Pour compiler le projet, exécutez la commande suivante dans le répertoire racine :
```bash
cabal build
```

### Exécution
Pour exécuter le programme :
```bash
cabal run
```

Vous pouvez également fournir un fichier contenant des expressions à évaluer :
```bash
./glados [file]
```

### Aide
Pour afficher l'aide :
```bash
./glados --help
```


## Structure du projet

- **AST.hs** : Définition de l'arbre syntaxique abstrait.
- **ASTEval.hs** : Évaluation des expressions définies dans l'AST.
- **ASTParser.hs** : Analyse syntaxique pour générer l'AST à partir d'une chaîne de caractères.
- **Parser.hs** : Bibliothèque de combinateurs de parseurs.
- **Utils.hs** : Fonctions utilitaires pour la gestion des fichiers et l'affichage de l'aide.
- **Setup.hs** : Configuration du projet pour Cabal.


## 🤝 Contributors 

Thanks goes to these wonderful people

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tbody>
    <tr>
        <td align="center" valign="top" width="14.28%"><a href="http://ivesvh.com"><img src="https://avatars.githubusercontent.com/u/57303456?v=4" width="100px;" alt="Erwann-Dev"/><br /><sub><b>Erwann-Dev</b></sub></a><br /></td>      <td align="center" valign="top" width="14.28%"><a href="http://ivesvh.com"><img src="https://avatars.githubusercontent.com/u/91835054?v=4" width="100px;" alt="rayan-mazouz"/><br /><sub><b>rayan-mazouz-Dev</b></sub></a><br /></td>
        <td align="center" valign="top" width="14.28%"><a href="http://ivesvh.com"><img src="https://avatars.githubusercontent.com/u/114578871?v=4" width="100px;" alt="Erwann-Dev"/><br /><sub><b>BaderOukhai</b></sub></a><br /></td>
        <td align="center" valign="top" width="14.28%"><a href="http://ivesvh.com"><img src="https://avatars.githubusercontent.com/u/51082555?v=4" width="100px;" alt="MCK-D"/><br /><sub><b>MCK-D</b></sub></a><br /></td>
        <td align="center" valign="top" width="14.28%"><a href="http://ivesvh.com"><img src="https://avatars.githubusercontent.com/u/114921918?v=4?s=100" width="100px;" alt="aurelielefr"/><br /><sub><b>Aurélie Lefranc</b></sub></a><br></td>
    </tr>
  </tbody>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->


## Licence
Ce projet est sous licence [Epitech License].

