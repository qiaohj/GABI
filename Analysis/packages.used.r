library(data.table)
packages <- c("data.table", "terra", "sf", "icosa", "ggplot2", "ncdf4", "RSQLite",
              "DBI", "dplyr", "furrr", "reshape2", "dggridR", "ggmap", "ggrepel",
              "ggh4x", "officer", "flextable", "patchwork", "ggeffects",
              "stringr", "stringi", "ape", "phytools", "ggtree", "phangorn")

get_pkg_info <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    return(data.table(Package = pkg, Version = "Not Installed", Citation = "Not Installed"))
  }
  ver <- as.character(packageVersion(pkg))
  cit_obj <- tryCatch(citation(pkg), error = function(e) NULL)
  if (!is.null(cit_obj)) {
    cit_txt <- paste(format(cit_obj), collapse = " ")
    cit_txt <- gsub("\\s+", " ", cit_txt)
  } else {
    cit_txt <- "No citation available"
  }
  
  return(data.table(Package = pkg, Version = ver, Citation = cit_txt))
}

pkg_summary_table <- rbindlist(lapply(packages, get_pkg_info))

print(pkg_summary_table)
fwrite(pkg_summary_table, "../Data/Tables/r_packages_citation_summary.csv", bom = TRUE)
