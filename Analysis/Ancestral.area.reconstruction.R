library(data.table)
library(ape)
library(ggplot2)
library(sf)
library(BioGeoBEARS)
library(phytools)
library(cladoRcpp)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/WD22T_11/GABI/GABI")
if (F){
  tree<-read.tree("../Data/phylo.tree/MSW05SupertreeNeeFix_1.tre")
  disp<-read_sf("../Shape/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
  disp$sci_name<-gsub(" ", "_", disp$sci_name)
  ecoregion<-read_sf("../Shape/Ecoregions2017/Ecoregions2017.shp")
  isea<-read_sf("../Shape/isea3h8/N_S_America.shp")
  #load a continent shape map 
  #continent<-read_sf("../Shape/continents/continent.shp")
  #st_crs(continent)<-st_crs(disp)
  i=1
  realms<-unique(ecoregion$REALM)
  realms<-realms[realms!="N/A"]
  disp_realm <- data.table(species = tree$tip.label)
  disp_realm[, (realms) := 0]
  
  ECO_NAME<-unique(ecoregion$ECO_NAME)
  ECO_NAME<-ECO_NAME[ECO_NAME!="N/A"]
  disp_ECO_NAME <- data.table(species = tree$tip.label)
  disp_ECO_NAME[, (ECO_NAME) := 0]
  
  isea.name<-unique(isea$seqnum)
  isea.name<-paste("i", isea.name, sep=".")
  disp_isea <- data.table(species = tree$tip.label)
  disp_isea[, (isea.name) := 0]
  
  
  i=1
  for (i in c(51:length(tree$tip.label))){
    print(paste(i, length(tree$tip.label)))
    sp<-tree$tip.label[i]
    disp_item<-disp[which(disp$sci_name==sp),]
    if (nrow(disp_item)==0){
      next()
    }
    overlap<-st_intersects(disp_item, ecoregion)
    overlap<-unique(unlist(overlap))
    if (length(overlap)==0){
      next()
    }
    ecoregion_item<-ecoregion[overlap,]
    disp_realm[sp==species, (unique(ecoregion_item$REALM)):=1]
    disp_ECO_NAME[sp==species, (unique(ecoregion_item$ECO_NAME)):=1]
    
    overlap<-st_intersects(disp_item, isea)
    overlap<-unique(unlist(overlap))
    if (length(overlap)==0){
      next()
    }
    isea_item<-isea[overlap,]
    isea_item.name<-unique(isea_item$seqnum)
    isea_item.name<-paste("i", isea_item.name, sep=".")
    
    disp_isea[sp==species, (isea_item.name):=1]
    
    
  }
  saveRDS(disp_isea, "../Data/phylo.tree/disp_isea.rda")
  saveRDS(disp_ECO_NAME, "../Data/phylo.tree/disp_ECO_NAME.rda")
  saveRDS(disp_realm, "../Data/phylo.tree/disp_realm.rda")
  
  var_names <- setdiff(names(disp_isea), "species")
  
  disp_isea_dt <- melt(disp_isea,
                       id.vars = "species",
                       measure.vars = var_names,
                       variable.name = "seqnum",
                       value.name = "value")
  disp_isea_dt<-disp_isea_dt[value==1]
  disp_isea_dt$seqnum<-as.numeric(gsub("i\\.", "", disp_isea_dt$seqnum))
  isea_dt<-data.table(seqnum=isea$seqnum, continent=isea$continent)
  disp_isea_dt<-merge(disp_isea_dt, isea_dt, by="seqnum")
  saveRDS(disp_isea_dt, "../Data/phylo.tree/disp_isea_dt.rda")
  
  disp_continent_dt<-unique(disp_isea_dt[, c("species", "value", "continent")])
  saveRDS(disp_continent_dt, "../Data/phylo.tree/disp_continent.rda")
  disp_isea_matrix <- dcast(disp_continent_dt,
                            species ~ continent,
                            value.var = "value",
                            fill = 0)
  continents<-c("N", "S", "B1", "B2")
  colnames(disp_isea_matrix)[2:5]<-continents
  
  dt_out <- disp_isea_matrix[, .(line = paste0(species, "\t", paste0(.SD, collapse = ""))), 
                             by = species, .SDcols = continents]
  n_species <- nrow(disp_isea_matrix)
  n_vars <- ncol(disp_isea_matrix) - 1
  header_line <- paste0(n_species, "\t", n_vars, " (", paste(continents, collapse = " "), ")")
  output_lines <- c(header_line, dt_out$line)
  writeLines(output_lines, "../Data/phylo.tree/disp.continent.txt")
  
  tree<-read.tree("../Data/phylo.tree/MSW05SupertreeNeeFix_1.tre")
  pruned_tree <- drop.tip(tree, tree$tip.label[!tree$tip.label %in% unique(disp_isea_dt$species)])
  plot(pruned_tree)
  length(pruned_tree$tip.label)
  is.rooted(pruned_tree)
  is.binary(pruned_tree)
  
  pruned_tree <- multi2di(pruned_tree)
  min(pruned_tree$edge.length)
  pruned_tree <- impose_min_brlen(pruned_tree, min_brlen = 1e-6)
  class(pruned_tree) 
  summary(pruned_tree$edge.length)
  sum(pruned_tree$edge.length <= 0) 
  getAnywhere(impose_min_brlen)
  pruned_tree$edge.length[pruned_tree$edge.length <= 0] <- 1e-6
  write.tree(pruned_tree, file = "../Data/phylo.tree/rooted_mammal_tree.NS.newick")
}
if (F){
  # Intitialize a default model (DEC model)
  run_object <- define_BioGeoBEARS_run()
  # Give BioGeoBEARS the location of the phylogeny Newick file
  run_object$trfn<-"../Data/phylo.tree/rooted_mammal_tree.NS.newick"
  tree<-read.tree(run_object$trfn)
  # Give BioGeoBEARS the location of the geography text file
  run_object$geogfn<-"../Data/phylo.tree/disp.continent.txt"
  run_object <- readfiles_BioGeoBEARS_run(run_object)
  
  # Input the maximum range size
  run_object$max_range_size <- 4
  tipranges <- getranges_from_LagrangePHYLIP(run_object$geogfn)
  run_object$max_range_size <- max(2, ncol(tipranges@df))
  
  
  run_object$min_branchlength <- 1e-06    # Min to treat tip as a direct ancestor (no speciation event)
  run_object$include_null_range <- TRUE    # set to FALSE for e.g. DEC* model, DEC*+J, etc.
  # (For DEC* and other "*" models, please cite: Massana, Kathryn A.; Beaulieu, 
  #  Jeremy M.; Matzke, Nicholas J.; O’Meara, Brian C. (2015). Non-null Effects of 
  #  the Null Range in Biogeographic Models: Exploring Parameter Estimation in the 
  #  DEC Model. bioRxiv,  http://biorxiv.org/content/early/2015/09/16/026914 )
  # Also: search script on "include_null_range" for other places to change
  
  # Speed options and multicore processing if desired
  run_object$on_NaN_error <- -1e50    # returns very low lnL if parameters produce NaN error (underflow check)
  run_object$speedup <- TRUE          # shorcuts to speed ML search; use FALSE if worried (e.g. >3 params)
  run_object$use_optimx <- TRUE    # if FALSE, use optim() instead of optimx();
  # if "GenSA", use Generalized Simulated Annealing, which seems better on high-dimensional
  # problems (5+ parameters), but seems to sometimes fail to optimize on simple problems
  run_object$num_cores_to_use<-30
  
  # Sparse matrix exponentiation is an option for huge numbers of ranges/states (600+)
  # I have experimented with sparse matrix exponentiation in EXPOKIT/rexpokit,
  # but the results are imprecise and so I haven't explored it further.
  # In a Bayesian analysis, it might work OK, but the ML point estimates are
  # not identical.
  # Also, I have not implemented all functions to work with force_sparse=TRUE.
  # Volunteers are welcome to work on it!!
  run_object$force_sparse <- FALSE    # force_sparse=TRUE causes pathology & isn't much faster at this scale
  
  
  run_object$BioGeoBEARS_model_object@params_table["d","type"] <- "free"
  run_object$BioGeoBEARS_model_object@params_table["e","type"] <- "free"
  run_object$BioGeoBEARS_model_object@params_table["j","type"] <- "fixed"
  run_object$BioGeoBEARS_model_object@params_table["j","init"] <- 0.0
  run_object$BioGeoBEARS_model_object@params_table["j","est"] <- 0.0
  
  # Good default settings to get ancestral states
  run_object$return_condlikes_table <- TRUE
  run_object$calc_TTL_loglike_from_condlikes_table <- TRUE
  run_object$calc_ancprobs <- TRUE    # get ancestral states from optim run
  
  # This function loads the dispersal multiplier matrix etc. from the text files into the model object. Required for these to work!
  # (It also runs some checks on these inputs for certain errors.)
  run_object <- readfiles_BioGeoBEARS_run(run_object)
  
  # Set up DEC model
  # (nothing to do; defaults)
  
  # Look at the BioGeoBEARS_run_object; it's just a list of settings etc.
  run_object
  
  # This contains the model object
  run_object$BioGeoBEARS_model_object
  
  # This table contains the parameters of the model 
  run_object$BioGeoBEARS_model_object@params_table
  
  # Run this to check inputs. Read the error messages if you get them!
  run_object <- fix_BioGeoBEARS_params_minmax(BioGeoBEARS_run_object=run_object)
  check_BioGeoBEARS_run(run_object)
  
  results <- bears_optim_run(run_object)
  saveRDS(results, "../Data/phylo.tree/ancestral.area.model.rda")
  print(results$outputs)
  
  #######################################################
  # PDF plots
  #######################################################
  pdffn = "Psychotria_DEC_vs_DEC+J_M0_unconstrained_v1.pdf"
  pdf(pdffn, height=6, width=6)
  
  #######################################################
  # Plot ancestral states - DEC
  #######################################################
  analysis_titletxt ="BioGeoBEARS DEC on Psychotria M0_unconstrained"
  
  # Setup
  results_object = results
  scriptdir = np(system.file("extdata/a_scripts", package="BioGeoBEARS"))
  
  # States
  res2 = plot_BioGeoBEARS_results(results_object, analysis_titletxt, addl_params=list("j"), 
                                  plotwhat="text", label.offset=0.45, tipcex=0.7, 
                                  statecex=0.7, splitcex=0.6, titlecex=0.8, plotsplits=TRUE, 
                                  cornercoords_loc=scriptdir, include_null_range=TRUE, tr=tree, 
                                  tipranges=tipranges)
  
  # Pie chart
  plot_BioGeoBEARS_results(results_object, analysis_titletxt, addl_params=list("j"), 
                           plotwhat="pie", label.offset=0.45, tipcex=0.7, statecex=0.7, 
                           splitcex=0.6, titlecex=0.8, plotsplits=TRUE, cornercoords_loc=scriptdir, 
                           include_null_range=TRUE, tr=tree, tipranges=tipranges)
  
  
  
  areas <- getareas_from_tipranges_object(tipranges)
  states_list_0based <- rcpp_areas_list_to_states_list(areas = areas,
                                                       maxareas = max(2, ncol(tipranges@df)),
                                                       include_null_range = TRUE)
  continent_combination<-list()
  for (i in c(1:length(states_list_0based))){
    item<-states_list_0based[[i]]+1
    continent.item<-data.table(com=i, continents=paste(continents[item], collapse = "-"))
    continent_combination[[length(continent_combination)+1]]<-continent.item
  }
  continent_combination<-rbindlist(continent_combination)
  saveRDS(continent_combination, "../Data/phylo.tree/continent_combination.rda")
  plot_BioGeoBEARS_results(results, analysis_titletxt="DEC Model", addl_params=list("j"), plotwhat="text")
  
  
  tree <- read.tree(results$inputs$tr)
  heights <- nodeHeights(tree)
  bottom_probs <- results$ML_marginal_prob_each_state_at_branch_bottom_below_node
  top_probs <- results$ML_marginal_prob_each_state_at_branch_top_AT_node
  
  time_step <- 0.01 
  start_time <- 0
  end_time <- 150
  
  times <- seq(from = start_time, to = end_time, by = time_step)
  
  all_results <- list()
  i=1
  for (i in seq_len(nrow(tree$edge))) {
    node_from <- tree$edge[i, 1]
    node_to <- tree$edge[i, 2]
    time_bottom <- heights[i, 1]
    time_top <- heights[i, 2]
    
    prob_bottom <- bottom_probs[i,]
    prob_top <- top_probs[i,]
    
    times_in_branch <- times[times >= time_bottom & times <= time_top]
    t<-times_in_branch[1]
    if (length(times_in_branch)==0){
      next()
    }
    for (t in times_in_branch) {
      w <- (t - time_bottom) / (time_top - time_bottom)
      p <- (1 - w) * prob_bottom + w * prob_top
      item<-data.table(
        node = node_to,
        time = t,
        state = seq_along(p),
        prob = p
      )
      item<-item[prob==max(prob)]
      all_results[[length(all_results) + 1]] <- item
    }
  }
  
  df <- rbindlist(all_results)
  
  saveRDS(df, "../Data/phylo.tree/ancestral.area.rda")
}
ancestral.area<-readRDS("../Data/phylo.tree/ancestral.area.rda")
disp_continent<-readRDS("../Data/phylo.tree/disp_continent.rda")
continent_combination<-readRDS("../Data/phylo.tree/continent_combination.rda")
tree<-read.tree("../Data/phylo.tree/rooted_mammal_tree.NS.newick")
tree$tip.label[1351]
disp_continent[species==tree$tip.label[1351]]
ancestral.area[node==1351]
setorderv(ancestral.area, "state", -1)
continent_combination[com==3]
