
<!-- index.md is generated from index.Rmd. Please edit that file -->

# Librairie R gstest

<!-- badges: start -->
<!-- [![CRAN Status](https://www.r-pkg.org/badges/version/gstest)](https://cran.r-project.org/package=gstest) -->

[![Lifecycle:
stable](man/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/ferlmic/gstest/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/ferlmic/gstest/actions/workflows/R-CMD-check.yaml?query=branch%3Amain)
[![Codecov test
coverage](https://codecov.io/gh/ferlmic/gstest/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ferlmic/gstest?branch=main)

<!-- badges: end -->
<!-- Display a link to the English `index.md' file (only when rendering an HTML document)
     &#10;     => the Pandoc "fenced_div" below (::: {.pkgdown-devel} <...> :::) is used to avoid 
        having the link generated in the pkgdown website home page
     => the link would only show in the "development" version of the pkgdown website
        (`development: mode: devel` in `_pkdown.yml` or `development: mode: auto` with a 4-level 
        version number in the DESCRIPTION file), which we do not use for gstest (we set 
        `development: mode: release` in `_pkdown.yml`, resulting in a single "release" website 
        regardless of the version number -->

<div class="pkgdown-devel">

[English (file `../index.md`)](../index.md)

</div>

## Description

Version R du système généralisé de Statistique Canada (StatCan)
**G-Séries** initialement développé en SAS<sup>®</sup>. Ce site web est
consacré à la version R de G-Séries (librairie gstest). Écrivez-nous à
<g-series@statcan.gc.ca> pour obtenir des informations sur les versions
SAS<sup>®</sup>.

> ***Note - intranet de StatCan***  
> Les employés de StatCan peuvent également visiter la page Confluence
> de G-Séries dans l’intranet de l’agence (cherchez « G-Series \|
> G-Séries » dans Confluence) ainsi que le projet de développement
> GitLab de G-Séries également hébergé dans l’intranet de l’agence
> (cherchez « G-Series in R - G-Séries en R » dans GitLab). Ce dernier
> inclut une version des informations et instructions contenues dans
> cette page qui sont spécifiques à l’infrastructure TI de StatCan (ex.,
> Artifactory et GitLab) ; voir `index_StatCan.md` dans le répertoire
> racine du projet GitLab.

G-Séries 3.0 (librairie gstest 3.0.0) est la première version du
logiciel offerte en libre accès (logiciel libre). Elle inclut le
recodage en R de toutes les fonctionalités SAS<sup>®</sup> de G‑Series
2.0, soient PROC BENCHMARKING, PROC TSRAKING et la macro
***GSeriesTSBalancing***, ainsi qu’une fonction pour l’étalonnage de
*séries de stocks* par l’entremise d’interpolations par spline cubique
où les noeuds de la spline correspondent aux ratios ou différences entre
les valeurs des étalons et de la série indicatrice. Il comprend les
*fonctions principales* suivantes:

- `benchmarking()`
- `stock_benchmarking()`
- `tsraking()`, `tsraking_driver()`
- `tsbalancing()` <br>

D’autres *fonctions utilitaires* sont également incluses dans la
librairie. Visitez la page [Référence](./reference/index.html) (barre
supérieure) pour obtenir la liste complète des fonctions disponibles.

## Installation

``` r
# Version publiée sur le CRAN
# (bientôt disponible...)
#install.packages("gstest")

# Version de développement sur GitHub
install.packages("remotes")
remotes::install_github("ferlmic/gstest")

# Version spécifique sur GitHub
remotes::install_github("ferlmic/gstest@<release-tag>")
```

où *\<release-tag\>* réfère aux valeurs énumérées sous les
[*Tags*](https://github.com/ferlmic/gstest/tags) ($\geq$ *v3.0.0*) du
projet GitHub.

#### *Alternative*

La librairie peut également être installée à partir des fichiers sources
téléchargés. Cette approche nécessite l’installation préalable des
librairies dont dépend gstest (à cause de `repos = NULL` dans l’appel
`install.packages()`).

1.  Accéder au [dépôt](https://github.com/ferlmic/gstest) (*repository*)
    GitHub pertinent (branche *main* ou *tag* $\geq$ *v3.0.0*)
2.  Télécharger les fichiers du dépôt (*Code* \> *Download ZIP*).
3.  Décompresser les fichiers téléchargés.
4.  Installer la librairie gstest (et ses librairies dépendantes) :

``` r
install.packages(c("ggplot2", "ggtext", "gridExtra", "lifecycle", "osqp", "rlang", "xmpdf"))
install.packages("<nom & chemin d'accès des fichiers du dépôt téléchargés et décompressés>",
                 repos = NULL, type = "source")
```

#### *Vignettes*

L’installation de gstest à partir du CRAN (`install.packages("gstest")`)
construit et installe automatiquement les vignettes de la librairie. Par
contre, ce n’est pas le cas par défaut lors d’une installation à partir
de GitHub (avec `remotes::install_github()`) ou à partir des fichiers
sources téléchargés (avec
`install.packages(..., repos = NULL, type = "source")`). Bien que les
vignettes ne soient pas nécessaires pour qu’une librairie soit
fonctionnelle, elles contiennent une documentation complémentaire utile.
Les vignettes de la librairie gstest sont disponibles dans le menu
déroulant ***Articles*** (barre supérieure) de ce site web et dans le
dossier `pdf/` du [dépôt](https://github.com/ferlmic/gstest) GitHub.
L’installation des vignettes avec la librairie les rend également
accessibles à partir de R (par exemple, avec `browseVignettes("gstest")`
ou `vignette("<nom-de-la-vignette>")`).

La construction des vignettes de la librairie gstest nécessite le
logiciel (gratuit) [Pandoc](https://pandoc.org/), qui est inclus dans
[RStudio](https://posit.co/downloads/), et une distribution LaTeX (par
exemple, [TinyTex](https://github.com/rstudio/tinytex-releases)). Vous
devez donc éviter d’essayer de construire les vignettes de la librairie
gstest avec l’interface graphique de base de R (à moins que vous n’ayez
une installation autonome de Pandoc) ou sans une distribution LaTeX
fonctionnelle. La construction des vignettes nécessite également les
librairies R knitr et rmarkdown.

Lors de l’installation **à partir de GitHub**, utilisez l’argument
`build_vignettes = TRUE` :

``` r
install.packages(c("knitr", "remotes", "rmarkdown"))
remotes::install_github("ferlmic/gstest", build_vignettes = TRUE)
```

Lors de l’installation **à partir des fichiers sources téléchargés**,
créez d’abord la version *groupée (« bundled »)* de la librairie avec
`devtools::build()` :

``` r
install.packages(c("devtools", "ggplot2", "ggtext", "gridExtra", "osqp", "xmpdf"))
bndl_pkg_path <- devtools::build("<nom & chemin d'accès des fichiers du dépôt téléchargés et décompressés>")
install.packages(bndl_pkg_path, repos = NULL, type = "source")
```

Remarque : les librairies knitr, lifecycle, rlang et rmarkdown sont
automatiquement installées avec devtools.

## Documentation

La documentation bilingue (anglaise-française) de la librairie gstest
disponible sur ce site est également accessible à partir de R (en
anglais uniquement). `help("gstest")` dans R affiche des informations
générales sur la librairie, y compris un lien vers (l’URL de) ce site
web. En cliquant sur le lien **Index** au bas de la page d’aide de la
librairie gstest dans RStudio (ou avec `help(package = "gstest")`), on
obtient la liste de tous les éléments de documentation disponibles pour
la librairie, dont :

- les pages d’aide des sujets/fonctions (page
  [Référence](./reference/index.html) de la barre supérieure) ;
- les vignettes, lorsqu’elles sont installées (menu déroulant
  ***Articles*** de la barre supérieure) ;
- les nouvelles de la librairie (page [Changements](./news/index.html)
  sous le menu déroulant ***Nouveautés*** de la barre supérieure).

Les pages d’aide individuelles de chaque fonction qui se trouvent dans
la page [Référence](./reference/index.html) (barre supérieure) peuvent
être accédées directement dans R avec `help("<nom-de-la-fonction>")`.
Elles contiennent des informations complètes sur chacune des fonctions,
y compris des exemples utiles, et seront votre principale source
d’information. Les vignettes du menu déroulant ***Articles*** (barre
supérieure) sont une source d’information complémentaire qui,
lorsqu’elles sont installées avec la librairie, sont également
accessibles à partir de R avec `browseVignettes("gstest")` ou
`vignette("<nom-de-la-vignette>")`. Elles comprennent :

- un *Livre de recette sur l’étalonnage*
  (`vignette("benchmarking-cookbook")`) parcourant les étapes d’un
  projet d’étalonnage typique ;
- *Un script d’étalonnage pour débutant*
  (`vignette("benchmarking-demo-script")`) illustrant l’utilisation des
  fonctions d’étalonnage dans un contexte pratique ;
- une page *« Data frame » pour la séquence de paramètres d’OSQP*
  (`vignette("osqp-settings-sequence-dataframe")`) décrivant la
  *séquence de résolution* implémentée dans `tsbalancing()` et
  expliquant comment elle peut être personnalisée.

Enfin, la page [Prise en main](./articles/gstest.html) (barre
supérieure) fournit des informations générales sur G-Séries et est
disponible sous la forme d’une vignette dans R (`vignette("gstest")`).

#### *Copie locale du site web de la librairie*

Le dossier `docs/` du [dépôt](https://github.com/ferlmic/gstest) GitHub
(branche *main* ou *tag* $\geq$ *v3.0.0*) contient les fichiers du site
web de la librairie et peut donc être téléchargé afin d’obtenir une
copie locale de ce site web. Ceci peut être utile pour une consultation
hors ligne ou pour accéder à la documentation d’une version spécifique
de la librairie (ex., la version de développement ou une version
antérieure). Après avoir téléchargé (*Code* \> *Download ZIP*) et
décompressé les fichiers du dépôt, ouvrez le fichier
`docs/fr/index.html` dans un navigateur web pour accéder à la copie
locale de la page d’accueil (page actuelle) du site web.
Alternativement, l’outil GitHub [*Download GitHub
directory*](https://download-directory.github.io/) peut être utilisé
pour télécharger uniquement le contenu du dossier `docs/` au lieu de
téléchager tous les fichiers du dépôt :

1.  Ouvrir le dossier `docs/` du
    [dépôt](https://github.com/ferlmic/gstest) GitHub pertinent (branche
    *main* ou *tag* $\geq$ *v3.0.0*).
2.  Copier l’adresse URL du dossier (bar d’adresse) dans le champ texte
    de l’outil [*Download GitHub
    directory*](https://download-directory.github.io/) et appuyer sur
    <kbd>Retour</kbd>.
3.  Décompresser le répertoire téléchargé.
4.  Ouvrir le fichier `fr/index.html` dans un navigateur web.

Remarque : la boîte *Rechercher* (barre supérieure) ne fonctionne pas
dans les copies locales du site web de la librairie.

#### *Format PDF*

La documentation en format PDF de G-Séries, également utile en mode hors
ligne ou pour une version spécifique de G-Séries, est disponible dans le
dossier `pdf/` du [dépôt](https://github.com/ferlmic/gstest) GitHub
(branche *main* ou *tag* $\geq$ *v3.0.0* pour les versions R et *tag*
$\leq$ *v2.0* pour les versions SAS<sup>®</sup>). Là encore, l’outil
GitHub [*Download GitHub
directory*](https://download-directory.github.io/) peut être utilisé
pour télécharger uniquement le contenu du dossier `pdf/` au lieu de
téléchager tous les fichiers du dépôt. La décompression du répertoire
téléchargé dévoilera alors les fichiers PDF individuels.
