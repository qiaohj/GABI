library(flextable)
library(officer)
library(ggsci)

x_label<-"Years before present (kyr)"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

map_crs<-st_crs("+proj=robin +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
map_theme<-theme(
  axis.line = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5, color="#dab27f"),
  #plot.background = element_rect(fill="#fde7c0"),
  panel.background = element_rect(fill="#fde7c0"),
  #legend.background = element_rect(fill = "#fde7c0", color = NA),
  #legend.title = element_blank(),
  legend.position="bottom",
  legend.key.width = unit(1, 'cm'),
  plot.title = element_text(hjust = 0.5)
)
guide_colorbar_top<-guides(fill = guide_colorbar(
  title.position = "top",
  title.hjust = 0.5))
guide_legend_top<-guides(fill = guide_legend(
  title.position = "top",
  title.hjust = 0.5,
  nrow = 1,
  byrow = TRUE
))


color_high<-"#CC79A7"
color_low<-"#44AA99"
color_mid2<-"#88CCEE"
color_mid<-"#F7F7F7"
color_na<-color_high
color_sa<-color_low
color_1<-"#332288"
color_2<-"#DDCC77"
color_n2s<-color_1
color_s2n<-color_2
color_native<-color_high
color_immigrant<-color_low


to.doc<-function(summary_dt, title, output_file, digits = 3, in_place = F){
  summary_dt<-format_sigfigs_dt(summary_dt, digits=digits, in_place=in_place)
    
  ft_booktabs <- flextable(summary_dt) %>%
    theme_booktabs() %>%
    autofit() %>%
    set_caption(caption = title)
  
  doc <- read_docx()
  
  
  doc <- doc %>%
    body_add_flextable(value = ft_booktabs)
  
  print(doc, target = output_file)
  
  cat(paste("Saved the document to", output_file, "\n"))
}

format_sigfigs_dt <- function(dt, digits = 3, in_place = TRUE) {
  
  if (!is.data.table(dt)) stop("Must input a data.table")
  
  if (!in_place) {
    dt <- copy(dt)
  }
  
  num_cols <- names(dt)[vapply(dt, is.numeric, FUN.VALUE = logical(1))]
  
  if (length(num_cols) > 0) {
    #dt[, (num_cols) := lapply(.SD, signif, digits = digits), .SDcols = num_cols]
    dt[, (num_cols) := lapply(.SD, function(x) sprintf(paste0("%.", digits, "f"), x)), .SDcols = num_cols]
    
  }
  
  return(dt)
}

if (F){
  df$NB_factor<-factor(df$NB, 
                       levels = c("BROAD", "BIG", "MODERATE", "NARROW"), 
                       labels = c("BROAD", "MODERATE", "NARROW", "TINY"))
}