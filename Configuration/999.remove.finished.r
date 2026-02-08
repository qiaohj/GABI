library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
simulations<-data.table(dbReadTable(conn, "simulations"))
simulations$is_run<-ifelse(file.exists(sprintf("/media/huijieqiao/Butterfly/GABI/Results/%s",
                                               simulations$label)),
                           0, 1)
if (F){
  #seed_100<-readRDS("../Data/Tables/sp_full_continents.rda")
  length(unique(seed_100$seed_id))
  simulations$is_run<-0
  simulations[global_id %in%unique(seed_100$seed_id) & nb=="TINY", is_run:=1]
}
#simulations[nb!="NARROW", is_run:=0]
dbWriteTable(conn, "simulations", simulations, overwrite=T)
dbDisconnect(conn)

table(simulations$is_run)
setorderv(simulations, c("nb", "da", "is_run"))
simulations[, .(N=.N), by=list(nb, is_run)]
all.df<-simulations
all.list<-list()
for (i in c(1:nrow(all.df))){
  f<-sprintf(sprintf("/media/huijieqiao/Butterfly/GABI/Results/%s",
                     simulations[i]$label))
  n<-length(list.files(f))
  
  if (n<=2){
    print(i)
    xx<-data.table(file.info(f))
    xx$f<-f
    all.list[[length(all.list)+1]]<-xx
  }
}
all.df<-rbindlist(all.list)
dim(all.df)

if (T){
  log_folder <- "/media/huijieqiao/WD22T_50/ES.R/running.log"
  log_files <- list.files(
    path = log_folder,
    pattern = "\\.log$",
    full.names = TRUE,
    recursive = FALSE
  )
  
  #file_path<-"/media/huijieqiao/WD22T_50/ES.R/running.log/output_6.log"
  process_log_file <- function(file_path) {
    print(paste("Handling ", file_path))
    raw_data <- readBin(file_path, "raw", n = file.info(file_path)$size)
    raw_data_fixed <- raw_data
    raw_data_fixed[raw_data_fixed == as.raw(0x00)] <- as.raw(0x20)
    cleaned_string <- rawToChar(raw_data_fixed)
    log_data_final <- readLines(textConnection(cleaned_string), warn = FALSE)
    log_data_final <- log_data_final[nchar(trimws(log_data_final)) > 0]
    
    for (ii in c(1:length(log_data_final))){
      log_data_final[ii]<-trimws(log_data_final[ii])
    }
    
    
    log_data <- log_data_final
    pattern <- "^.*Index: .* \\| Year: .* \\| Cells: .* \\| Species: .* \\| Seed ID: .* \\| NB: .* \\| DA: .* \\| Running: .*$"
    target_lines <- log_data[grepl(pattern, log_data)]
    
    if (length(target_lines) == 0) {
      cat(paste("File ", basename(file_path), " no matched lines\n"))
      return(NULL)
    }
    
    dt <- data.table(log_line = target_lines)
    
    r<-dt[, {
      info_segment <- sub("^.*\\| (Year: .*)$", "\\1", log_line)
      
      extracted_cols <- tstrsplit(
        info_segment,
        split = " \\| ",
        fixed = FALSE,
        perl = TRUE
      )
      
      Year    <- sub("^Year: (.*)$", "\\1", extracted_cols[[1]])
      Cells   <- sub("^Cells: (.*)$", "\\1", extracted_cols[[2]])
      Species <- sub("^Species: (.*)$", "\\1", extracted_cols[[3]])
      seed_id <- sub("^Seed ID: (.*)$", "\\1", extracted_cols[[4]])
      NB      <- sub("^NB: (.*)$", "\\1", extracted_cols[[5]])
      DA      <- sub("^DA: (.*)$", "\\1", extracted_cols[[6]])
      Runtime <- sub("^Running: (.*)$", "\\1", extracted_cols[[7]])
      
      .(
        seed_id = as.integer(seed_id),
        NB = NB,
        DA = DA,
        Year = as.integer(Year),
        Cells = as.integer(Cells),
        Species = as.integer(Species),
        Runtime = Runtime
      )
    }, by = log_line][,.(`seed_id`, `NB`, `DA`, `Year`, `Cells`, `Species`, `Runtime`)]
    
    r[, file_source := basename(file_path)]
    
    return(r)
  }
  
  list_of_dts <- lapply(log_files, process_log_file)
  
  final_dt <- rbindlist(list_of_dts, fill = TRUE)
  setorderv(final_dt, c("seed_id", "Year"), c(1, -1))
  print(final_dt)
  result_DT <- final_dt[final_dt[, .I[which.min(Year)], by = seed_id]$V1]
  result_DT[Year==0]
  setorderv(result_DT, c("Year", "seed_id"), c(1, -1))
  
  View(result_DT[Year!=0])
  length(unique(final_dt$seed_id))
  dim(result_DT[Year!=0])
  #final_dt[seed_id==10143]
}

all.list<-list()
for (i in c(1:nrow(all.df))){
  f<-sprintf(all.df[i]$f)
  n<-length(list.files(f))
  
  if (n<=2){
    print(i)
    xx<-data.table(file.info(f))
    xx$f<-f
    all.list[[length(all.list)+1]]<-xx
  }
}
all.df<-rbindlist(all.list)
dim(all.df)
setorderv(all.df, "ctime")
View(all.df)

#removed_folder<-all.df[!gsub("/media/huijieqiao/Butterfly/GABI/Results/", "", all.df$f) %in% 
#         sprintf("%d.%s.%s", final_dt$seed_id, final_dt$NB, final_dt$DA)]
file.remove(removed_folder$f)
if (F){
  conn<-dbConnect(RSQLite::SQLite(), "../Configuration/conf.sqlite")
  s<-data.table(dbReadTable(conn, "simulations"))
  dbDisconnect(conn)
  
  s<-s[nb=="BROAD"]
  for (i in c(1:nrow(s))){
    print(i)
    folder<-sprintf("/media/huijieqiao/Butterfly/GABI/Results/%s", s[i]$label)
    if (!dir.exists(folder)){
      dir.create(folder)
    }
  }
}