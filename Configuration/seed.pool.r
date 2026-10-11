library(data.table)
library(sf)
library(RSQLite)
library(DBI)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/GABI/GABI")
conn<-dbConnect(RSQLite::SQLite(), "../Configuration/20261002/conf.sqlite")
sim<-data.table(dbReadTable(conn, "simulations"))
dbDisconnect(conn)

sim$nb<-tolower(sim$nb)
sim[nb=="narrow", nb:="TINY"]
sim[nb=="moderate", nb:="NARROW"]
sim[nb=="big", nb:="MODERATE"]
sim[nb=="broad", nb:="BROAD"]

