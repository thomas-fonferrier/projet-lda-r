# Sync tutorial example files into inst/extdata/
src <- file.path("inst", "shiny", "examples")
dst <- file.path("inst", "extdata")
dir.create(dst, recursive = TRUE, showWarnings = FALSE)
files <- list.files(src, full.names = TRUE)
file.copy(files, dst, overwrite = TRUE)
message("Copied ", length(files), " file(s) to ", dst)
