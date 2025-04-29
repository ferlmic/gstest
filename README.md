
<!-- README.md is generated from README.Rmd. Please edit that file -->

# G-Series in R (package gstest)

<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/gstest)](https://cran.r-project.org/package=gstest)
[![R-CMD-check](https://github.com/ferlmic/gstest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ferlmic/gstest/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ferlmic/gstest/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ferlmic/gstest?branch=main)

<!-- badges: end -->

([Français](#g-s%C3%A9ries-en-r-librairie-gstest))

## Description

R version of Statistics Canada’s (StatCan) generalized system
**G-Series** initially developed in SAS<sup>®</sup>. This project is
devoted to G-Series in R (package gstest). Email us at
<g-series@statcan.gc.ca> for information about the SAS<sup>®</sup>
versions.

> ***Note - StatCan intranet***  
> StatCan employees can also visit the G-Series Confluence page on the
> agency’s intranet (search for “G-Series \| G-Séries” in Confluence) as
> well as the G-Series GitLab development project also hosted on the
> agency’s intranet (search for “G-Series in R - G-Séries en R” in
> GitLab). The latter includes information and instructions specific to
> the StatCan IT infrastructure (e.g., Artifactory and GitLab); see file
> `index_StatCan.md` in the GitLab project root folder.

G-Series 3.0 (package gstest 3.0.0) is the initial open-source version
of the software. It includes the rewriting in R of all SAS<sup>®</sup>
G‑Series 2.0 functionalities, that is PROC BENCHMARKING, PROC TSRAKING
and macro ***GSeriesTSBalancing*** along with a function for
benchmarking *stocks* using a spline interpolation approach where the
spline knots correspond to the benchmark-to-indicator ratios or
differences.

<https://ferlmic.github.io/gstest/en/> (GitHub Pages website) is your
*go-to* place for everything there is to know about this package:

- installation instructions (home page);
- complete function documentation with examples (**Reference** page);
- package vignettes containing complementary documentation (**Articles**
  menu);
- G-Series (R and SAS<sup>®</sup>) version *changelog* (**News** \>
  **Changelog** page);
- how to download local copies of the package HTML and PDF documentation
  (home page);
- etc.

## Project Structure

With the exception of the following directories, the files in this
project follow the common structure of a R package with a pkgdown
website:

- `docs/` (\*): bilingual pkgdown website files (usually built in a
  GitHub Actions workflow or a GitLab CI/CD pipeline rather than being
  saved with the project files).
- `fr/`: files required to generate the French version of the complete
  HTML and PDF package documentation (function reference, vignettes,
  etc.). The contents of the `fr/` directory resemble those of the root
  project directory.
- `misc/`: various files related to the package maintenance.
- `pdf/` (\*): bilingual package documentation and vignettes in PDF
  format.

(\*) The purpose of directories `docs/` and `pdf/` is to provide access
to the actual documentation, in HTML and PDF formats, of all versions
ever released of the R package as opposed to only the latest
(development) version for the package website (GitHub Pages) or the
installed version through R and RStudio.

## Contact - Support

User support for G-Series is provided by the Time Series Research and
Analysis Centre (TSRAC) in the Economic Statistics Methods Division
(ESMD) and the Digital Processing Solutions Division (DPSD). Email us at
<g-series@statcan.gc.ca> for information or help using G-Series. GitHub
account holders can also request information, ask questions or report
problems through the G-Series GitHub project
[Issues](https://github.com/ferlmic/gstest/issues) page. StatCan
employees can do the same through the **Issues** page of the G-Series
GitLab development project hosted on the StatCan intranet (search for
“G-Series in R - G-Séries en R” in GitLab).

<br>

------------------------------------------------------------------------

<br>

# G-Séries en R (librairie gstest)

<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/gstest)](https://cran.r-project.org/package=gstest)
[![R-CMD-check](https://github.com/ferlmic/gstest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ferlmic/gstest/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ferlmic/gstest/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ferlmic/gstest?branch=main)

<!-- badges: end -->

([English](#g-series-in-r-package-gstest))

## Description

Version R du système généralisé de Statistique Canada (StatCan)
**G-Séries** initialement développé en SAS<sup>®</sup>. Ce projet est
consacré à la version R de G-Séries (librairie gstest). Écrivez-nous à
<g-series@statcan.gc.ca> pour obtenir des informations sur les versions
SAS<sup>®</sup>.

> ***Note - intranet de StatCan***  
> Les employés de StatCan peuvent également visiter la page Confluence
> de G-Séries dans l’intranet de l’agence (cherchez « G-Series \|
> G-Séries » dans Confluence) ainsi que le projet de développement
> GitLab de G-Séries également hébergé dans l’intranet de l’agence
> (cherchez « G-Series in R - G-Séries en R » dans GitLab). Ce dernier
> inclut des informations et instructions qui sont spécifiques à
> l’infrastructure TI de StatCan (ex., Artifactory et GitLab) ; voir le
> fichier `index_StatCan.md` dans le répertoire racine du projet GitLab.

G-Séries 3.0 (librairie gstest 3.0.0) est la première version du
logiciel offerte en libre accès (logiciel libre). Elle inclut le
recodage en R de toutes les fonctionalités SAS<sup>®</sup> de G‑Series
2.0, soit PROC BENCHMARKING, PROC TSRAKING et la macro
***GSeriesTSBalancing***, ainsi qu’une fonction pour l’étalonnage de
*séries de stocks* par l’entremise d’interpolations par spline cubique
où les noeuds de la spline correspondent aux ratios ou différences entre
les valeurs des étalons et de la série indicatrice.

<https://ferlmic.github.io/gstest/fr/> (site web *GitHub Pages*), est
l’endroit de prédilection pour tout ce qu’il y a à savoir sur la
librairie gstest :

- instructions d’installation (page d’accueil) ;
- documentation complète des fonctions avec des exemples (page
  **Référence**) ;
- vignettes de la librairie contenant de la documentation complémentaire
  (menu **Articles**) ;
- historique des versions R et SAS<sup>®</sup> de G-Séries (page
  **Nouveautés** \> **Changements**) ;
- comment télécharger une copie locale de la documentation HTML et PDF
  de la librairie R (page d’accueil) ;
- etc.

## Structure du projet

A l’exception des répertoires suivants, les fichiers de ce projet
suivent la structure commune (habituelle) d’une librairie R avec un site
web pkgdown :

- `docs/` (\*) : fichiers du site web pkgdown (habituellement construits
  dans un *GitHub Actions workflow* ou un *GitLab CI/CD pipeline* plutôt
  que d’être sauvegardés avec les fichiers du projet).
- `fr/` : fichiers nécessaires pour générer la version française de la
  documentation complète de la librairie gstest en format HTML et PDF
  (référence des fonctions, vignettes, etc.). Le contenu du répertoire
  `fr/` ressemble à celui du répertoire racine du projet.
- `misc/` : divers fichiers liés à la maintenance de la librairie.
- `pdf/` (\*) : documentation et vignettes de la librairie gstest en
  format PDF.

(\*) La raison d’être des répertoires `docs/` et `pdf/` est de fournir
un accès à la documentation originale, en format HTML et PDF, de toutes
les versions jamais publiées de la librairie R par opposition à
uniquement la plus récente (de développement) pour le site web de la
librairie (*GitHub Pages*) ou la version installée via R et RStudio.

## Contact - Assistance

L’assistance aux utilisateurs de G-Séries assurée par le Centre de
recherche et d’analyse en séries chronologiques (CRASC) de la Division
des méthodes de la statistique économique (DMSE) et par la Division des
solutions de traitement numérique (DSTN). Écrivez-nous à
<g-series@statcan.gc.ca> pour obtenir des informations ou de l’aide sur
l’utilisation de G-Séries. Les détenteurs d’un compte GitHub peuvent
également demander des informations, de poser des questions ou de
signaler des problèmes via la page
[*Issues*](https://github.com/ferlmic/gstest/issues) du projet GitHub de
G-Séries. Les employés de StatCan peuvent faire de même via la page
**Tickets** du projet de développement GitLab de G-Séries hébergé dans
l’intranet de StatCan (recherchez « G-Series in R - G-Séries en R » dans
GitLab).
