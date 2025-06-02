library(data.table)
library(ape)
library(ggplot2)
library(sf)
library(BioGeoBEARS)
sf_use_s2(FALSE)
setwd("~/GIT/GABI/Script")
tree<-read.tree("../Data/phylo.tree/MSW05SupertreeNeeFix_1.tre")
disp<-read_sf("../Shape/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
disp$sci_name<-gsub(" ", "_", disp$sci_name)
ecoregion<-read_sf("../Shape/Ecoregions2017/Ecoregions2017.shp")
#load a continent shape map 
continent<-read_sf("../Shape/continents/continent.shp")
st_crs(continent)<-st_crs(disp)
i=1
realms<-unique(ecoregion$REALM)
realms<-realms[realms!="N/A"]
disp_realm <- data.table(species = tree$tip.label)
disp_realm[, (realms) := 0]

ECO_NAME<-unique(ecoregion$ECO_NAME)
ECO_NAME<-ECO_NAME[ECO_NAME!="N/A"]
disp_ECO_NAME <- data.table(species = tree$tip.label)
disp_ECO_NAME[, (ECO_NAME) := 0]


i=1
for (i in c(1:length(tree$tip.label))){
  print(paste(i, length(tree$tip.label)))
  sp<-tree$tip.label[i]
  disp_item<-disp[which(disp$sci_name==sp),]
  if (nrow(disp_item)==0){
    next()
  }
  overlap<-st_intersects(disp_item, ecoregion)
  ecoregion_item<-ecoregion[unique(unlist(overlap)),]
  disp_realm[sp==species, (unique(ecoregion_item$REALM)):=1]
  disp_ECO_NAME[sp==species, (unique(ecoregion_item$ECO_NAME)):=1]
  
}

run_object <- define_BioGeoBEARS_run()
tree<-read.tree("../Data/test/tree.txt")
is.rooted(tree)
rooted_tree <- root(tree, outgroup = "species1", resolve.root = TRUE)
plot(rooted_tree)
is.rooted(rooted_tree)
any(rooted_tree$edge.length <= 0)
rooted_tree <- impose_min_brlen(rooted_tree, min_brlen = 1e-6)
write.tree(rooted_tree, file = "../Data/test/rooted_tree.newick")

run_object$trfn<-"../Data/test/rooted_tree.newick"
run_object$geogfn<-"../Data/test/disp.txt"
run_object <- readfiles_BioGeoBEARS_run(run_object)
run_object$max_range_size <- 3
run_object$include_null_range <- TRUE
run_object$num_cores_to_use<-4
run_object$BioGeoBEARS_model_object@params_table["d","type"] <- "free"
run_object$BioGeoBEARS_model_object@params_table["e","type"] <- "free"
run_object$BioGeoBEARS_model_object@params_table["j","type"] <- "fixed"
run_object$BioGeoBEARS_model_object@params_table["j","init"] <- 0.0
run_object$BioGeoBEARS_model_object@params_table["j","est"] <- 0.0
results <- bears_optim_run(run_object)
print(results$outputs)

plot_BioGeoBEARS_results(results, analysis_titletxt="DEC Model", addl_params=list("j"), plotwhat="text")
# Get the example files directory
extdata_dir = np(system.file("extdata", package="BioGeoBEARS"))
# tmp hard code:
# extdata_dir = "/Dropbox/_njm/__packages/BioGeoBEARS_setup/inst/extdata/"

# Set the filenames (Hawaiian Psychotria from Ree & Smith 2008)
trfn = np(paste(extdata_dir, "/Psychotria_5.2.newick", sep=""))
tr = read.tree(file=trfn)

geogfn = np(paste(extdata_dir, "/Psychotria_geog.data", sep=""))

# Look at the tree and ranges, for kicks
getranges_from_LagrangePHYLIP(lgdata_fn=geogfn)
tr
run_object$trfn<-trfn
run_object$geogfn<-geogfn

## Not run: 
# Run the ML search
bears_output = bears_optim_run(run_object)
bears_output

