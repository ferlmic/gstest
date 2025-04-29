
<!-- misc/README_dev.md is generated from misc/README.Rmd. Please edit that file -->

# G-Series in R (package gstest) - *Extra Info*

<!-- badges: start -->
<!-- badges: end -->

This version of the README file (`~/misc/README.md`) only lists
differences with (extra information compared to) the *official* project
README file (`~/README.md`), NEWS file (`~/NEWS.md`) and the complete R
version roadmap file (`~/G-Series_in_R_roadmap.md`).

## Description

*\[see `~/README.md`\]*

## Project Structure

*\[see `~/README.md`\]*

*\[extra info\]*

#### GitLab flows

- ***main* branch**: default branch with files corresponding to the
  latest (development) version of the package.
- ***dev* branch**: branch where development happens, with issues and
  merge requests (temporary *feature branches* merged into the *dev*
  branch when the associated issue and merge request is closed).

The *dev* branch is merged into the *main* branch either at specific
milestones of a development cycle when a new feature has been fully
tested and is ready to be deployed to *advanced users* (version number
x.y.z.90–), or at the end of a development cycle when a new version is
ready to be submitted to CRAN (version number x.y.z).

The package website is not hosted as a GitLab Pages website but rather
as a GitHub Pages website (see **Package website** below). Note that a
GitLab Pages *copy* of the package website could be generated if
wanted/necessary for the StatCan intranet (e.g., The Zone). However,
note that…

- The “Search box” in the top bar of the GitLab Pages *copy* would
  search and link (redirect) to the general public intenet GitHub Pages
  website (not the StatCan intranet GitLab Pages *copy*).
- The same would happen with the “Source:” links at the top of the
  reference pages and vignettes: they would link to the general public
  internet GitHub repo (not the StatCan intranet GitLab repo).

These (unexpected) behaviours are misleading/confusing enough IMO to
justify NOT generating a GitLab Pages “copy” in the StatCan intranet and
rely only on the (general public internet) GitHub Pages website instead
(!)

#### GitLab and GitHub mirrors

(*to be completed*)

The gstest package is officially released to the general public on
GitHub, under the StatCan group/organization
(<https://github.com/ferlmic/gstest>). The updating process from GitLab
to GitHub is semi-automated and goes through a GitLab mirror projet.

Notes:

- Only the `main` branch is *mirrored*. It’s not clear why the `dev`
  branch cannot also be mirrored, but this has the advantage to be
  coherent with the Banff mirroring process.
- The main usage/benefit of the GitLab mirror project is to
  cleanup/prepare the `main` branch repo for GitHub. E.g., remove
  irrelevant files for GitHub such as the GitLab CI/CD pipeline
  (`.gitlab-ci.yml`) or MD files with information/instructions targeted
  for the StatCan intranet or that include links to StatCan intranet
  (GitLab projects, Confluence pages, etc.).

Updating steps:

1.  Update the GitLab mirror repo (pull the `main` branch from the
    GitLab *development* project)
2.  ??? Run the repo cleanup step ???
3.  Update the GitHub mirror repo (pull the `main` branch from the
    GitLab *mirror* project)

When a new version is accepted (available) on CRAN/Artifactory:

- Create a new release in the GitLab (development) and GitHub projects
- Update the package website (manually run GitHub Action workflow
  `workflow-name`).

#### Package website

The gstest package website is hosted as a GitHub Pages website
(<https://ferlmic.github.io/gstest/en/>) and always corresponds to the
CRAN/Artifactory version (latest release). One can the access/download
the documentation (HTML or PDF) for the development version from the
GitLab `dev` and `main` branch repos (`dev` compared to `main` is
*deeper* in the development cycle) or from the GitHub `main` branch repo
(GitHub and GitLab `main` branches are identical).

Installing the development version, or a specific version (release tag),
can be done from GitLab/GitHub. See `index.md` (package website home
page) in the GitLab/GitHub project root folder for general public
information and instructions (CRAN and GitHub) or `index_StatCan.md` in
the GitLab project root folder for the equivalent information specific
to the StatCan IT infrastructure (Artifactory and GitLab).

## Contact - Support

*\[see `~/README.md`\]*

## G-Series Changelog (`NEWS.md`)

*\[see `~/News.md`\]*

## Complete R Version Roadmap/Changelog (`G-Series_in_R_roadmap.md`)

*\[extra info\]*

#### V1.4.3

Other (minor) modifications include:

- New function `gs.validate_arg_logi()` to validate logical function
  arguments (`TRUE`\|`FALSE`).
- Prefixes `bk.` and `gs.` have been added to functions and objects
  defined in scripts `aaa_utils-bench.R` and `utils-common.R`,
  respectively, in order to facilitate their identification in *regular
  scripts*. Environment `gs.e` defined in script `aaa_utils-bench.R` was
  consequently renamed to `bk.e`.

#### V1.4.0

Other (minor) modifications include:

- New vignette `osqp-settings-sequence-dataframe.Rmd`.
- Indicator series values of 0 are no longer allowed when `lambda < 0`
  in `benchmarking()` (invalid resolution of $|s_t^\dagger|^\lambda$
  otherwise).
- The bias line and the bias-corrected series are no longer plotted with
  `plot_benchAdj()` and `plot_graphTable()` when `rho = 1`: bias
  correction is irrelevant with *Denton* benchmarking and plotting them
  can actually lead to *scaling issues* with the default bias (0 or 1)
  and large series and benchmarks values (e.g., in thousands or
  millions).
- `NA` was changed to `NULL` for a few function arguments corresponding
  to objects that are not a vector of length 1.
- `NULL` is now allowed instead of `NA` for function arguments, in part
  to ensure backward compatibility given the change mentioned in the
  preceding bullet point, but also because some users may instinctively
  specify `NULL` over `NA`.
- Renamed scripts `common_utils.R` to `utils-common.R` and
  `aaa_utils_bench.R` to `aaa_utils-bench.R`.
- Renamed environment `the` to `gs.e` (a more relevant/appropriate name
  ) in script `aaa_utils-bench.R` (and in the scripts for
  `benchmarking()` and `stock_benchmarking()`).
- New section *Comparing `tsraking()` and `tsbalancing()`* in the
  documentation of `tsraking()` (shared with `tsbalancing()`).
- New section *Processing groups* in the documentation of
  `tsrakin_driverg()` (shared with `tsbalancing()`).

#### V1.03 (1.3.0)

More details on the other changes:

- Correction of a bug in the construction of matrix `J` in function
  `proc_benchmarking` when 2 distinct benchmarks have the same starting
  or ending period.
- Improvements to the setup script (R_GSeries_setup.R):
  - usage of function `utils::getSrcDirectory()` instead of “cryptic”
    code `sys.frame(1)$ofile` to retrieve the installation directory
    (parent of subdirectory “src”)
  - error message if the script is executed “interactively” (i.e. not
    using the `source()` function)
  - function `R_GSeries_setup`, compiled during the script execution, is
    removed from the R environment (this function is not meant to be
    executed outside of the “source” execution of the script)
  - only files with extension .R in dir “src” are “sourced”
  - the R Core team’s suggested “default packages” are installed and
    loaded if necessary
    (<https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Default-packages>)
- New subdirectory “common” in directory “src” contains utility
  functions shared by the R G-Series prototype functions. They are
  sourced “locally” (argument `local = TRUE`) inside the functions so
  that they do not persist in the R environment after prototype function
  executions.
- Miscellaneous performance improvements, e.g.:
  - common function `gInv_sqMat` (Moore-Penrose generalized inverse):
    - usage of function `ifelse` instead if `sapply` with `if/else`
    - change of the default tolerance to identify nonzero values (more
      in line with other software such as MATLAB), which produces
      results similar to SAS/IML (GINV func.) for “tough to inverse”
      matrices
  - direct floating point comparisons with a tolerance instead of using
    function `isTRUE` with `all.equal`
- Correction of bugs related to the usage of logical operators `&&` and
  `||` instead of `&` and `|`
- The existence of the input data frames is no longer validated using
  function `exists`, which allows for more flexibility in their
  specification when calling the functions. E.g. `my_df[row, cols]` or
  using function `plyr::adply` with `proc_tsraking` for period-by-period
  processing. If the specified data frame does not exist, a generic (but
  clear enough) error message is generated (e.g. Error in \[…\] : object
  ‘…’ not found) as opposed to a more personalized (clearer) error
  message.
- The benchmarking functions (`proc_benchmarking()` and
  `stock_benchmarking()`) now generate a “real” error message (with
  function `stop()`) in the event of “soft” error messages (with
  functions `try(stop())`). This allows for proper handling of errors
  when calling the functions with `tryCatch` or `withCallingHandlers`
  using the `error` condition. In addition, this now requires the output
  object to be returned with via `on.exit()` function. The same approach
  is used in (new) function `tsraking_driver`.
- Input objects (“data.frame” and “ts” objects) are internally coerced
  to the “pure” object type, which ensures proper processing the event
  that the functions are used with objects (current or future) that are
  instances of the original class (for which `is.data.frame()` or
  ‘is.ts()’ return value `TRUE`) but may behave slightly differently
  (not fully backward compatible), e.g., “tibble” data frames.

#### V1.02 (1.2.0)

Other minor modifications include:

- better usage of function `all.equal`
- correction of typos in comments
