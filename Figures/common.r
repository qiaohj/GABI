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
color_low<-"#2166AC"
color_high<-"#B2182B"
color_mid<-"#F7F7F7"

color_1<-"#A8DE1C"
color_2<-"#FFC300"

sa.bridge2<-c(9580, 9662,9744,9663,9745,9664)
