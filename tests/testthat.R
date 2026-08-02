# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(HPAanalyze)

# Bioconductor's build system sets IS_BIOC_BUILD_MACHINE (the same env var
# testthat::skip_on_bioc() checks). The suite still runs in full under
# GitHub Actions and locally, where this variable is never set.
if (isTRUE(as.logical(Sys.getenv("IS_BIOC_BUILD_MACHINE", "false")))) {
  message(
    "Skipping the testthat suite: running on Bioconductor's build system ",
    "(IS_BIOC_BUILD_MACHINE is set). The suite still runs in GitHub Actions ",
    "and locally."
  )
} else {
  test_check("HPAanalyze")
}
