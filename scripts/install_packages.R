# Install required packages for FuEtal2024 replication into user library
lib <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.5")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
cat("Using library:", lib, "\n")

pkgs <- c("tidyverse", "haven", "sandwich", "lmtest",
          "broom", "ggplot2", "patchwork", "scales", "here")

missing <- pkgs[!pkgs %in% rownames(installed.packages(lib.loc = c(lib, .libPaths())))]

if (length(missing) == 0) {
  cat("All packages already installed.\n")
} else {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing,
                   lib     = lib,
                   repos   = "https://cloud.r-project.org",
                   quiet   = FALSE,
                   Ncpus   = 4L)
  cat("Done.\n")
}
