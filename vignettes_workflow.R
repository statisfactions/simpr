## Workflow for building vignettes so they show up
## locally and without needing end-users on Github
## to install

## Taken from https://community.rstudio.com/t/browsevignettes-mypackage-saying-no-vignettes-found/68656/7
tools::buildVignettes(dir = ".", tangle=TRUE)
dir.create("inst/doc")
file.copy(dir("vignettes", full.names=TRUE), "inst/doc", overwrite=TRUE)

## Check package without deleting vignettes
devtools::check(".", vignettes=FALSE)

## Build site
pkgdown::build_site()
