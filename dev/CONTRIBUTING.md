# Development and documentation

Work in a source checkout. Install package dependencies, development
tools (devtools, pkgdown, testthat, roxygen2, knitr and rmarkdown) and Pandoc.
The tested live backend is ClickHouseHTTP 1.1.2.

## Check changes

```r
devtools::load_all()
devtools::test()  # Offline fixtures; no THIP credentials required
devtools::check()
```

From the repository parent, run `R CMD build thipR` followed by
`R CMD check --no-manual <produced-archive.tar.gz>`. Do not disable vignette
building. Synthetic chunks execute; live snippets must remain `eval=FALSE`.
Backend compatibility requires separate opt-in read-only synthetic checks in
`inst/examples/`. When sourcing a smoke script from a runner environment holding
`con`, use `source(path, local = TRUE)` so that connection remains in scope.

## Website

From the package directory:

```r
pkgdown::clean_site()
pkgdown::build_site()
```

Then run `python3 tools/check-public-docs.py docs`. Review all new public content;
the checker is a regression guard, not a substitute for review. Register new
public pages in the allowlist when appropriate. Main-branch pushes build, check
and deploy only the generated site directory; pull requests do not deploy.
Platform hostnames and account paths never go in the repository: the checkers
read them from the `THIP_PRIVATE_PATTERNS` environment variable (a CI secret).
This repository is public; `python3 tools/check-repo-files.py` scans tracked
files on every push and pull request. See also `AGENTS.md`.

Keep the README short: purpose, installation, one example and guide links.
Put workflow teaching in vignettes, argument contracts in roxygen documentation,
and changes in NEWS. Keep public examples synthetic and credential-free.
Internal study findings, credentials and deployment logs must stay outside the
package. Developer files in dev/ are excluded from the site and source archive.

## Notebook guides and installation

Generate notebooks from the same guide sources with
`python3 tools/build-guide-notebooks.py <output-directory>`; Documentation.ipynb
is the navigation index. Do not store executed notebooks containing real data.
On THIP, install into R's personal library (`R_LIBS_USER`), never the platform
library. Start here is the one vignette installed with the package; the course
guides are website-only articles in `vignettes/articles/`.
