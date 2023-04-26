Patch release of package simpr, now 0.2.6.  This fixes R CMD check error in CRAN version simpr 0.2.4 (issues with oversensitive package tests failing on Linux systems) that resulted in the package's archival from CRAN on 2023-03-31, and responding to feedback from CRAN maintainers for 0.2.5 submission by removing "dontrun" in one example and global environment usage in one test.  CRAN maintainer also commented about adding references to description field, but there are no relevant references to add beyond the package website already linked in URL.

## R CMD check results
R CMD check run on Ubuntu, Mac OSX, and Windows (via win-builder). There were no ERRORs or WARNINGs. There is one NOTE ("New submission" and noting the package's archival from CRAN).
