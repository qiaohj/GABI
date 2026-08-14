target_dir <- "Y:/GABI/GABI"

r_files <- list.files(path = target_dir, pattern = "\\.[rR]$", full.names = TRUE, recursive = TRUE)


extract_packages <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  lines <- gsub("#.*$", "", lines)
  text <- paste(lines, collapse = "\n")
  
  m_lib <- gregexpr("(?:library|require)\\s*\\(\\s*(?:[\"']?)([a-zA-Z0-9._]+)(?:[\"']?)", text, perl = TRUE)
  lib_str <- unlist(regmatches(text, m_lib))
  pkgs_lib <- gsub(".*\\(\\s*[\"']?([a-zA-Z0-9._]+)[\"']?", "\\1", lib_str, perl = TRUE)
  m_ns <- gregexpr("([a-zA-Z0-9._]+)\\s*:{2,3}", text, perl = TRUE)
  ns_str <- unlist(regmatches(text, m_ns))
  pkgs_ns <- gsub("\\s*:{2,3}", "", ns_str, perl = TRUE)
  
  pkgs <- unique(c(pkgs_lib, pkgs_ns))
  pkgs <- pkgs[pkgs != "" & !is.na(pkgs) & !pkgs %in% c("F", "T", "c", "list", "return")]
  return(pkgs)
}

all_pkgs <- unique(unlist(lapply(r_files, extract_packages)))
all_pkgs<-all_pkgs[!all_pkgs %in% c("BioGeoBEARS", "cladoRcpp", "ggtree", "jsonlite",
                                    "curl", "randomForest", "heatmaply", "rcartocolor",
                                    "ggsci", "viridis", "scales", "broom.mixed",
                                    "ranger", "rnaturalearth", "rnaturalearthdata",
                                    "ggnewscale", "ggeffects", "astrochron", "treeio")]
installed_pkgs <- all_pkgs[all_pkgs %in% rownames(installed.packages())]
missing_pkgs <- setdiff(all_pkgs, installed_pkgs)

installed_pkgs <- sort(installed_pkgs)

pkg_versions <- vapply(installed_pkgs, function(pkg) {
  ver <- as.character(packageVersion(pkg))
  sprintf("%s v%s", pkg, ver)
}, FUN.VALUE = character(1))

formatted_output <- paste(pkg_versions, collapse = ", ")

cat(formatted_output, "\n\n")
for (pkg in installed_pkgs) {
  cat("--------------------------------------------------\n")
  cat(sprintf("Package: %s (v%s)\n", pkg, as.character(packageVersion(pkg))))
  cat("--------------------------------------------------\n")
  
  cit <- suppressWarnings(citation(pkg))
  print(cit, style = "text")
  cat("\n")
}
