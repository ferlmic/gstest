
<!-- Release_Checklist.md is generated from Release_Checklist.Rmd. Please edit that file -->

With a new release, whether it’s a major or a minor release, comes a new
version number. Make sure to update the following files:

\[Repository files\]

- [ ] ~/DESCRIPTION
  - [ ] field `Version:`
  - [ ] field `Created:`
- [ ] ~/index.Rmd (**Description**)
- [ ] ~/README.Rmd (**Description**)
- [ ] ~/NEWS.md (**G-Series Changelog**)
- [ ] ~/G-Series_in_R_roadmap.Rmd (**Complete R Version
  Roadmap/Changelog**)
- [ ] ~/misc/README.Rmd (**Changelog**)
- [ ] ~/\_pkgdown.yml
  - [ ] `repo: url: source:` (?)
  - [ ] `news: releases:`
- [ ] ~/misc/pkg_housekeeping.R (command `R CMD Rd2pdf` output PDF file
  (?))

\[GitLab release info (Deployments \> Releases \> ***New release***)\]

- [ ] Tag name
- [ ] Release title
- [ ] Release assets (**v*x.y.z* website/PDF files**)
- [ ] Release notes:
  - [ ] **Changes in version *x.y.z***
  - [ ] **Using gstest**
  - [ ] **Other sections (?)**
