#######################################################################
## run.R                                                              #
## ------------------                                                 #
## Date              : April, 2020                                    #
## Author            : Alfa Nugraha                                   #
## Script purpose    :                                                #                 
##                                                                    #                                             
##                                                                    #
##                                                                    #
##                                                                    #
##                                                                    #
#######################################################################

# "c:\Program Files\R\R-3.6.3\bin\Rscript.exe" --vanila run.R -d data/raster_ha/Jawa_Barat/Admin_jabar.tif 
# -i data/raster_ha/Jawa_Barat/PL00_jabar.tif 
# -b 2000
# -f data/raster_ha/Jawa_Barat/PL00_jabar.tif
# -e 2003
# -u data/tabular/Tabel_acuan_tutupan_lahan.csv
# -z data/raster_ha/Jawa_Barat/Fungsikawasan_jabar.tif
# -k data/tabular/Tabel_acuan_fungsi_kawasan.csv
# -c data/tabular/cstock.csv
# -p data/raster_ha/Jawa_Barat/Peat_jabar.tif
# -x data/tabular/Faktor_emisi_perubahan_gambut.csv
# -h data/raster_ha/Jawa_Barat/Burn18_jabar.tif

# libraries with no duplicates====
library(optparse)

option_list = list(
  make_option(c("-w", "--workdir"), type="character", default=getwd(), help="set working directory", metavar="character", ), 
  make_option(c("-d", "--admin"), type="character", default=NULL, help="administrative boundary", metavar="character"), 
  make_option(c("-i", "--landcover1"), type="character", default=NULL, help="initial land-cover", metavar="character"), 
  make_option(c("-b", "--year1"), type="numeric", default=NULL, help="initial period", metavar="numeric"), 
  make_option(c("-f", "--landcover2"), type="character", default=NULL, help="final land-cover", metavar="character"), 
  make_option(c("-e", "--year2"), type="numeric", default=NULL, help="final period", metavar="numeric"), 
  make_option(c("-u", "--landcover_table"), type="character", default=NULL, help="land-cover class", metavar="character"), 
  make_option(c("-z", "--zone"), type="character", default=NULL, help="zone or planning unit", metavar="character"), 
  make_option(c("-k", "--zone_table"), type="character", default=NULL, help="zone class", metavar="character"), 
  make_option(c("-c", "--carbon_stock"), type="character", default=NULL, help="carbon stock", metavar="character"), 
  make_option(c("-p", "--peat"), type="character", default=NULL, help="peat", metavar="character"), 
  make_option(c("-x", "--peat_table"), type="character", default=NULL, help="peat class", metavar="character"), 
  make_option(c("-h", "--burn"), type="character", default=NULL, help="burn", metavar="character"), 
  make_option(c("-o", "--output"), type="character", default="change_map.tif", help="output file name [default=%default]", metavar="character") 
)

opt_parser = OptionParser(option_list=option_list, add_help_option = FALSE)
args = parse_args(opt_parser)

if(is.null(args$admin)){
  print_help(opt_parser)
  stop("Please insert an admininistrative boundary", call.=FALSE)
} else if(is.null(args$admin)){
  print_help(opt_parser)
  stop("Please insert an admininistrative boundary", call.=FALSE)
} else if(is.null(args$landcover1)){
  print_help(opt_parser)
  stop("Please insert an initial land-cover", call.=FALSE)  
} else if(is.null(args$landcover2)){
  print_help(opt_parser)
  stop("Please insert a final land-cover", call.=FALSE)  
} else if(is.null(args$year1)){
  print_help(opt_parser)
  stop("Please insert an initial year", call.=FALSE)  
} else if(is.null(args$year2)){
  print_help(opt_parser)
  stop("Please insert a final year", call.=FALSE)  
} else if(is.null(args$landcover_table)){
  print_help(opt_parser)
  stop("Please insert a land-cover lookup table", call.=FALSE)  
} else if(is.null(args$zone)){
  print_help(opt_parser)
  stop("Please insert a zone or planning unit", call.=FALSE)  
} else if(is.null(args$zone_table)){
  print_help(opt_parser)
  stop("Please insert a zone lookup table", call.=FALSE)  
} else if(is.null(args$carbon_stock)){
  print_help(opt_parser)
  stop("Please insert a carbon stock lookup table", call.=FALSE)  
} 


library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(rgeos)
library(grid)
library(spatial.tools)
library(rgdal)
library(plyr)
library(tiff)
library(reshape2)
library(foreign)
library(splitstackshape)
library(magick)
library(dplyr)
library(gridExtra)
library(pracma)
library(zoo)
# library(SDMTools)
# library(vegan)
# library(rtf)
# library(RSQLite)
# library(RPostgreSQL)
# library(DBI)
# library(rpostgis)

raster_category <- function(data, lookup_class, name, type="lc"){
  if(type=="pu"){
    names(lookup_class) <- c("Value", "CLASS")
  } else {
    names(lookup_class) <- c("Value", "CLASS", "Trajectory")
  }
  vat_name <- gsub(pattern = ".tif", replacement = ".tif.vat.dbf", data)
  dbf_table <- data.frame(read.dbf(vat_name), stringsAsFactors = FALSE)
  # merge with the selected lookup table
  dbf_table <- merge(dbf_table, lookup_class, by = "Value", all.x = TRUE)
  if(type=="pu"){
    colnames(dbf_table)<-c("ID", "COUNT", "Legend")
  } else {
    colnames(dbf_table)<-c("ID", "COUNT", "Legend", "Classified")
  }
  # saving into the same directory
  dbf_name <- gsub(".tif$", ".csv", data)
  write.csv(dbf_table, dbf_name, row.names = FALSE)
  
  eval(parse(text=(paste(name, "Table<<-dbf_table",  sep=""))))
  eval(parse(text=(paste(name, '<<-raster("', data, '")', sep=""))))
  eval(parse(text=(paste(name, "<<-spatial_sync_raster(", name, ',', 'ref, method = "ngb")', sep=""))))
  eval(parse(text=(paste(name, "<<-", name, "*1",  sep=""))))
}

replace_backslash <- function(string) {
  return(str_replace_all(string=string, pattern="/", repl='\\\\'))
}

ref<-raster(args$admin)
inCarbonStock<-read.table(args$carbon_stock, header = TRUE, sep = ",")
inPeatEmissionFactor<-read.table(args$peat_table, header = TRUE, sep = ",")
inZoneClass<-read.table(args$zone_table, header = TRUE, sep = ",")
inLandCoverClass<-read.table(args$landcover_table, header = TRUE, sep = ",")
initialYear<-args$year1
finalYear<-args$year2
raster_category(data=args$landcover1, lookup_class = inLandCoverClass, name = "inLandCover1")
raster_category(data=args$landcover2, lookup_class = inLandCoverClass, name = "inLandCover2")
raster_category(data=args$zone, lookup_class = inZoneClass, name = "inZone", type = "pu")
inPeat<-raster(args$peat)
inBurn<-raster(args$burn)


# initialYear<-year1
# finalYear<-year2
# initialYear<-2000
# finalYear<-2003

# inLandCover1<-raster(landcover1)
# inLandCover2<-raster(landcover2)
# inZone<-raster(zone)
# inPeat<-raster(peat)
# inBurn<-raster(peat)
# inCarbonStock<-read.table(carbon_stock, header = TRUE, sep = ",")
# inPeatEmissionFactor<-read.table(peat_table, header = TRUE, sep = ",")
# inZoneClass<-read.table(zone_table, header = TRUE, sep = ",")
# inLandCoverClass<-read.table(landcover_table, header = TRUE, sep = ",")
# inCarbonStock<-read.table(paste0(inTableDirectory, "/cstock.csv"), header = TRUE, sep = ",")
# inPeatEmissionFactor<-read.table(paste0(inTableDirectory, "/Faktor_emisi_perubahan_gambut.csv"), header = TRUE, sep = ",")
# inZoneClass<-read.table(paste0(inTableDirectory, "/Tabel_acuan_fungsi_kawasan.csv"), header = TRUE, sep = ",")
# inLandCoverClass<-read.table(paste0(inTableDirectory, "/Tabel_acuan_tutupan_lahan.csv"), header = TRUE, sep = ",")

# for(n in 1:length(years)){
  # period = years[n]
# raster_category(data=paste0(inRasterDirectory, "/", project, "/PL00_jabar.tif"), lookup_class=inLandCoverClass, name="inLandCover1") 
# raster_category(data=paste0(inRasterDirectory, "/", project, "/PL03_jabar.tif"), lookup_class=inLandCoverClass, name="inLandCover2") 
# raster_category(data=paste0(inRasterDirectory, "/", project, "/Fungsikawasan_jabar.tif"), lookup_class=inZoneClass, name="inZone", type="pu") 
# } 

# inPeat<-raster(paste0(inRasterDirectory, "/", project, "/Peat_jabar.tif"))
# inBurn<-raster(paste0(inRasterDirectory, "/", project, "/Burn18_jabar.tif"))


# inWorkingDirectory="results"
inRasterDirectory="data/raster_ha"
inTableDirectory="data/tabular"
inDescription= "spatial analysis through aksara"
inProvince= "Jawa Barat"
inCountry= "Indonesia"
# admin_attribute= readOGR(dsn = "D:/data/spatial/vector", layer = "admin_lumens_final") # ADedit
# field_attribute= "IDADM" # ADedit
# spat_res= 100
inLocation= "Jawa Barat"
project<-str_replace_all(string=inLocation, pattern=" ", repl="_")


time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

setwd(args$workdir)
project<-str_replace_all(string=project, pattern=" ", repl="_")

# create an initial coverage reference for LUMENS project
# ref<-raster(paste0(inRasterDirectory, "/", project, "/Admin_jabar.tif"))
# ref<-ref*1
names(ref)<-"Administrative maps"
Ref.name<-names(ref)
Ref.type<-class(ref)
Ref.coord<-as.character(crs(ref))
Ref.res<-res(ref)
Ref.xmin<-xmin(ref)
Ref.xmax<-xmax(ref)
Ref.ymin<-ymin(ref)
Ref.ymax<-ymax(ref)
cov.desc1<-c("Reference name","Reference class", "Reference CRS", "Reference Resolution", "Xmin", "Xmax", "Ymin", "Ymax")
cov.desc2<-as.data.frame(rbind(Ref.name, Ref.type, Ref.coord, Ref.res, Ref.xmin, Ref.xmax, Ref.ymin, Ref.ymax))
cov.desc2<-cov.desc2[1]
cov_desc<-cbind(cov.desc1,cov.desc2)
colnames(cov_desc)[1]<-"Coverage"
colnames(cov_desc)[2]<-"Description"

years <- c(initialYear, finalYear)

# set driver connection
# driver <- dbDriver('PostgreSQL')
# DB <- dbConnect(
#   driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
#   user=as.character(pgconf$user), password=as.character(pgconf$pass)
# )
# add reference map to database
# xxBUI = bit unsigned integer
# xxBSI = bit signed integer
# xxBF  = bit float
# pgWriteRast(DB, c("public", "ref_map"), raster=ref)

# srid<-tryCatch({pgSRID(DB, crs(ref), create.srid = TRUE)}, error=function(e){ })
# # ADDRASTERTOPG function
# addRasterToPG<-function(project, raster.path, raster.name, raster.srid) {
#   createNewPGTbl = pathEnv
#   createNewPGTbl[6] = paste('raster2pgsql -s ', raster.srid, ' -I -C -t auto ', str_replace_all(string=raster.path, pattern="/", repl='\\\\'), ' public.', raster.name, ' | psql -d ', project, sep="")
#   
#   newBatchFile <- file(pgEnvBatch)
#   writeLines(createNewPGTbl, newBatchFile)
#   close(newBatchFile)
#   # execute batch file
#   pgEnvBatchFile<-str_replace_all(string=pgEnvBatch, pattern="/", repl='\\\\')
#   system(pgEnvBatchFile)
# }
# addRasterToPG(project, ref, 'ref_map', srid)

# unlink shapefile and raster
# file.rename("base.tif", "reference.tif")


# RESAVE function
# resave <- function(..., list = character(), file) {
#   previous  <- load(file)
#   var.names <- c(list, as.character(substitute(list(...)))[-1L])
#   for (var in var.names) assign(var, get(var, envir = parent.frame()))
#   save(list = unique(c(previous, var.names)), file = file)
# }


# getting an information of windows architecture through the path of LUMENS installation 
# gdaltranslate<-paste0("\"",LUMENS_path, "\\bin\\gdal_translate.exe\"")
# prepare some functions and store it to LUMENS project file, so it can be used later
# GET_FROM_RDB function
# get_from_rdb <- function(symbol, filebase, envir =parent.frame()){
#   lazyLoad(filebase = filebase, envir = envir, filter = function(x) x == symbol)
# }
#
# GETRASTERFROMPG function
# getRasterFromPG<-function(pg_conf, pg_database, pg_table, pg_rasterfile) { 
#   if(!file.exists(pg_rasterfile)){
#     postgres_connection<-paste('PG:\"host=', as.character(pg_conf$host), ' port=', as.character(pg_conf$port), ' dbname=\'', pg_database, '\' user=\'', as.character(pg_conf$user), '\' password=\'', as.character(pg_conf$pass), '\' schema=\'public\'', sep="")
#     gdaltranslate_cmd<-paste(gdaltranslate, postgres_connection, sep=" ")
#     
#     postgres_table<-paste('table=\'', pg_table, '\' mode=2\"', sep="")
#     # postgres_output<-paste('-a_nodata', pg_nodata, pg_rasterfile, sep=" ")
#     gdaltranslate_cmd_pu<-paste(gdaltranslate_cmd, postgres_table, pg_rasterfile, sep=" ") 
#     system(gdaltranslate_cmd_pu)
#   }
#   loadRaster<-raster(pg_rasterfile)
#   loadRaster<-reclassify(loadRaster, cbind(255, NA))
#   
#   # define projection
#   crs(loadRaster)<-as.character(cov_desc[3,2])
#   
#   # sync spatial data
#   if(pg_table!='ref_map'){
#     compareData<-tryCatch({ compareRaster(ref, loadRaster) }, error=function(e){})
#     if(is.null(compareData)){ loadRaster<-spatial_sync_raster(loadRaster, ref, method="ngb") }  
#   }
#   return(loadRaster)
# }


# writeRastFile function
# writeRastFile <- function(raster_in, raster_ou_path = character(), cat = FALSE, colorpal, lookup){
#   # the function is to replace 'writeRaster' usage in the LUMENS scripts so that not only the raster file will be written as '.tif' but also the '.qml' file will be automatically created
#   writeRaster(raster_in, raster_ou_path, format = "GTiff", overwrite = TRUE)
#   # assessing the values in 'raster_in'
#   if(cat){
#     u_values <- unique(values(raster_in))
#     u_values <- u_values[!is.na(u_values)]
#   } else{
#     u_values <- numeric()
#     u_values[1] <- min(values(raster_in), na.rm = TRUE)
#     u_values[2] <- 0
#     u_values[3] <- max(values(raster_in), na.rm = TRUE)
#     u_values[2] <- u_values[1] + (u_values[3] - u_values[1])/2# the average of the min and max
#   }
#   u_values <- u_values[order(u_values)]
#   
#   # writing the .csv containing the paths of the output rasters
#   if(file.exists(paste0(LUMENS_path_user,"/ou_raster.csv"))){
#     ou_raster_paths <- read.csv(paste0(LUMENS_path_user,"/ou_raster.csv"), stringsAsFactors = FALSE)
#     ou_raster_paths <- rbind(ou_raster_paths, raster_ou_path)
#   } else {
#     ou_raster_paths <- data.frame(raster_ou_path = raster_ou_path, stringsAsFactors = FALSE)
#   }
#   write.csv(ou_raster_paths, paste0(LUMENS_path_user,"/ou_raster.csv"), row.names = FALSE)
# }


# 4. Running PREQUES====

analysis.option<- 1 # should have entered 0 for all analysis

T1<-initialYear
T2<-finalYear

# result_dir<-working_directory

# planning unit
zone<-inZone
lookup_z<-inZoneTable
# landuse first time period
landuse1<-inLandCover1
# landuse second time period
landuse2<-inLandCover2
# landcover lookup table
lut.lc<-inLandCoverClass
lut.lc<-lut.lc[,1:2]
lookup_l<-lut.lc
lookup_lc<-lut.lc
colnames(lookup_l)<-c("ID", "CLASS")
colnames(lookup_lc)<-c("ID", "CLASS")

nLandCoverId<-nrow(lookup_lc)
nPlanningUnitId<-nrow(lookup_z)

#=Projection handling
if (grepl("+units=m", as.character(ref@crs))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(ref)[1]*res(ref)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else if (grepl("+proj=longlat", as.character(ref@crs))){
  print("Raster maps have projection in degree unit")
  Spat_res<-res(ref)[1]*res(ref)[2]*(111319.9^2)/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else{
  statuscode<-0
  statusmessage<-"Raster map projection is unknown"
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
  quit()
}

#=Create land use change data dummy
dummy1<-data.frame(nPU=lookup_z$ID, divider=nLandCoverId*nLandCoverId)
dummy1<-expandRows(dummy1, 'divider')

dummy2<-data.frame(nT1=lookup_lc$ID, divider=nLandCoverId)
dummy2<-expandRows(dummy2, 'divider')
dummy2<-data.frame(nT1=rep(dummy2$nT1, nPlanningUnitId))

dummy3<-data.frame(nT2=rep(rep(lookup_lc$ID, nLandCoverId), nPlanningUnitId))

landUseChangeMapDummy<-cbind(dummy1, dummy2, dummy3)
colnames(landUseChangeMapDummy)<-c('ZONE', 'ID_LC1', 'ID_LC2')

#=Create cross-tabulation
R<-(zone*1) + (landuse1*100^1) + (landuse2*100^2)
lu.db<-as.data.frame(freq(R))
lu.db<-na.omit(lu.db)
n<-3
k<-0
lu.db$value_temp<-lu.db$value
while(k < n) {
  eval(parse(text=(paste("lu.db$Var", n-k, "<-lu.db$value_temp %% 100", sep=""))))  
  lu.db$value_temp<-floor(lu.db$value_temp/100)
  k=k+1
}
lu.db$value_temp<-NULL
#lu.db$value<-NULL
colnames(lu.db)=c('ID_CHG', 'COUNT', 'ZONE', 'ID_LC1', 'ID_LC2')
lu.db<-merge(landUseChangeMapDummy, lu.db, by=c('ZONE', 'ID_LC1', 'ID_LC2'), all=TRUE)
lu.db$ID_CHG<-lu.db$ZONE*1 + lu.db$ID_LC1*100^1 + lu.db$ID_LC2*100^2
lu.db<-replace(lu.db, is.na(lu.db), 0)

# chg_map<-tolower(paste('chgmap_', pu_name, T1, T2, sep=''))
# eval(parse(text=(paste("writeRaster(R, filename='", chg_map, ".tif', format='GTiff', overwrite=TRUE)", sep=""))))
writeRaster(R, filename=args$output, format='GTiff', overwrite=TRUE)


#=Create individual table for each landuse map
# set area and classified land use/cover for first landcover and second
freqLanduse_1<-inLandCover1Table
area_lc1<-subset(freqLanduse_1, select=c('ID','COUNT', 'Classified'))
colnames(area_lc1) = c("ID", "COUNT_LC1", "Classified1") 
area_lc1<-merge(area_lc1,lookup_l,by="ID")
colnames(area_lc1)[4] = "CLASS_LC1"

freqLanduse_2<-inLandCover2Table 
area_lc2<-subset(freqLanduse_2, select=c('ID','COUNT', 'Classified'))
colnames(area_lc2) = c("ID", "COUNT_LC2", "Classified2")
area_lc2<-merge(area_lc2,lookup_l,by="ID")
colnames(area_lc2)[4] = "CLASS_LC2"

# combine all data in data_merge
colnames(lookup_l)[1]="ID_LC1"
colnames(lookup_l)[2]="LC_t1"
data_merge <- merge(lu.db,lookup_l,by="ID_LC1")
colnames(lookup_l)[1]="ID_LC2"
colnames(lookup_l)[2]="LC_t2"
data_merge <- as.data.frame(merge(data_merge,lookup_l,by="ID_LC2"))
colnames(lookup_z)[1]="ZONE"
colnames(lookup_z)[2]="COUNT_ZONE"
colnames(lookup_z)[3]="Z_NAME"
data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))
data_merge$COUNT<-data_merge$COUNT*Spat_res
colnames(lookup_l)<-c("ID", "CLASS")
colnames(lookup_z)<-c("ID", "COUNT_ZONE", "ZONE")
area_zone<-lookup_z
data_merge_sel <- data_merge[ which(data_merge$COUNT > 0),]
data_merge_sel$LU_CHG <- do.call(paste, c(data_merge_sel[c("LC_t1", "LC_t2")], sep = " to "))
# eval(parse(text=(paste("write.dbf(data_merge,'lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep=""))))


#=Create overall summary of land use change
colnames(lut.lc)[1]<-"ID"
colnames(lut.lc)[2]<-"LC"

freq1<-subset(freqLanduse_1, select=c('ID','COUNT'))
# freqLanduse_1<-dbReadTable(DB, c("public", data_luc1$LUT_NAME)) 
lu.summary<-merge(lut.lc, freq1, by="ID", all.x=TRUE)
lu.summary<-replace(lu.summary, is.na(lu.summary), 0)
colnames(lu.summary)[3]<-paste(T1, "_ha", sep="")
# get freqLanduse_[i] and merge to lu.summary
for (q in 2:2) { # need to check the counter
  eval(parse(text=(paste("freqLanduse_", q, "<-inLandCover", q, "Table", sep=""))))
  eval(parse(text=(paste("freq", q, "<-subset(freqLanduse_", q, ", select=c('ID','COUNT'))", sep=""))))
  eval(parse(text=(paste("lu.summary<-merge(lu.summary, freq", q, ", by='ID', all.x=TRUE)", sep=""))))
  lu.summary<-replace(lu.summary, is.na(lu.summary), 0)
  eval(parse(text=(paste("colnames(lu.summary)[", q+2, "]<-paste(T", q, ", '_ha', sep='')", sep=""))))
}
# calculate basic statistic from summary
for (z in 3:ncol(lu.summary)){
  lu.summary[,z]<-as.numeric(lu.summary[,z])*Spat_res
}
Ov_chg<-lu.summary
colnames(Ov_chg)[2]="Land_use_type"
Ov_chg$LU_CODE<-as.factor(toupper(abbreviate(Ov_chg$Land_use_type, minlength=4, strict=FALSE, method="both")))
# make sure the attribute fields have renamed
c.name<-NULL
for(m in 2:2){ # need to check the counter
  if (m!=2) {
    eval(parse(text=(paste("Period<-T", m,"-T", m-1, sep=""))))
    sub<-eval(parse(text=(paste("T", m, sep=""))))
    sub<-paste(sub,"_ha", sep="")
    eval(parse(text=(paste("colnames(Ov_chg)[", m+2, ']="', sub, '"', sep=""))))
    c.name<-c(c.name,sub)
  } else {
    eval(parse(text=(paste("Period<-T", m,"-T", m-1, sep=""))))
  }
}
# calculate in rate per year and unit of hectare  
Ov_chg.ha<-Ov_chg[1:2]
Ov_chg.rate<-Ov_chg.ha
count.period<-2-1
p.name<-NULL
p.name2<-NULL
for(o in 1:count.period){
  name<-paste((eval(parse(text=(paste("T",o,sep=""))))), "-", eval(parse(text=(paste("T",o+1,sep="")))),"_ha", sep="")
  name2<-paste((eval(parse(text=(paste("T",o,sep=""))))), "-", eval(parse(text=(paste("T",o+1,sep="")))),"_%/yrs", sep="")
  eval(parse(text=(paste("p.chg", "<-Ov_chg[", o, "+2+1]-Ov_chg[", o, "+2]", sep=""))))
  p.rate<-round(((p.chg/(Ov_chg[3]*Period))*100), 2)
  colnames(p.chg)[1]<-name
  colnames(p.rate)[1]<-name2
  Ov_chg.ha<-cbind(Ov_chg.ha,p.chg)
  Ov_chg.rate<-cbind(Ov_chg.rate,p.rate)
  p.name<-c(p.name,name)
  p.name2<-c(p.name2,name2)
}
Ov_chg.rate<-replace(Ov_chg.rate, is.na(Ov_chg.rate), 0)

# produce table and graph for overall change
# Ov_chg.melt <- melt(data = Ov_chg, id.vars=c('Land_use_type','LU_CODE'))
# Ov_chg.melt<-Ov_chg.melt[which(Ov_chg.melt$variable!="value"),]
# colnames(Ov_chg.melt)<-c("Land_use_type","LU_CODE", "Year", "Area")
# # overall changes plot 1
# ov.change.plot.2<-ggplot(Ov_chg.melt,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
#   theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") + xlab("Land use type") +
#   theme( legend.title = element_text(size=10),legend.text = element_text(size = 10), axis.text.x = element_text(size = 10),
#          axis.title.x=element_blank())
# # produce table and graph for overall change in rate
# # NOTE: Ov_chg.merge<-cbind(Ov_chg.ha,(Ov_chg.rate[3:4]),(Ov_chg[6])) SOURCE OF ERROR STILL IN HARDCODE
# eval(parse(text=(paste("Ov_chg.merge<-cbind(Ov_chg.ha,(Ov_chg.rate[3:",2+count.period,"]),(Ov_chg[", 2+idx_landuse+1, "]))", sep=""))))
# # overall changes plot 2
# Ov_chg.melt2 <- melt(data = Ov_chg.merge, id.vars=c('Land_use_type','LU_CODE'), measure.vars=p.name)
# colnames(Ov_chg.melt2)<-c("Land_use_type","LU_CODE", "Year", "Area")
# ov.change.plot.3<-ggplot(Ov_chg.melt2,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
#   theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") + xlab("Land use type") +
#   theme( legend.title = element_text(size=8),legend.text = element_text(size = 8), axis.text.x = element_text(size = 10),
#          axis.title.x=element_blank())+coord_flip()
# # overall changes plot 3
# Ov_chg.melt3 <- melt(data = Ov_chg.merge, id.vars=c('Land_use_type','LU_CODE'), measure.vars=p.name2)
# colnames(Ov_chg.melt3)<-c("Land_use_type","LU_CODE", "Year", "Area")
# ov.change.plot.4<-ggplot(Ov_chg.melt3,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
#   theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") + xlab("Land use type") +
#   theme( legend.title = element_text(size=8),legend.text = element_text(size = 8), axis.text.x = element_text(size = 10),
#          axis.title.x=element_blank())+coord_flip()+ylim (c(-100, 100))
# 


# ===TODO=================================================

# retrieve the cstock and peat_em also other tables====
# c_luName <- "cstock"
# peat_luName <- "em_peat"
# pu_name <- "pu_ver1"
# if (pu_name=="ref") {
#   lookup_z <- dbReadTable(DB, c("public", "lut_ref"))
#   names(lookup_z) <- c("ID_Z", "Z_NAME")
# } else {
#   lookup_z<-dbReadTable(DB, c("public", list_of_data_pu[list_of_data_pu$RST_NAME==pu_name, "LUT_NAME"]))
#   names(lookup_z) <- c("ID_Z", "COUNT", "Z_NAME")
#   lookup_z <- lookup_z[ , c(1,3)]
# }
# # standardized column names
# 
# # extract peat planning unit IDs
# peat_keyword <- "eat"
# peat_puID <- lookup_z[grep(peat_keyword, lookup_z[,2]) , 1]
# 
# cstock <- inCarbonStock
# em_peat <- inPeatEmissionFactor
# 
# # lut_ref has been available in the proj.file
# 
# # list ids for forest dynamics====
# forest_ids <- cstock %>% filter(grepl(" Hutan", Tipe_tutupan_lahan) | grepl("mangrove", Tipe_tutupan_lahan)) %>% dplyr::select(ID) %>% pull()
# primaryForest_ids <- cstock %>% filter(grepl("primer", Tipe_tutupan_lahan)) %>% dplyr::select(ID) %>% pull()
# secForest_ids <- forest_ids %>% setdiff(primaryForest_ids)
# hiDensFor_ids <- cstock %>% filter(grepl("primer", Tipe_tutupan_lahan)) %>% dplyr::select(ID) %>% pull()
# loDensFor_ids <- cstock %>% filter(grepl("sekunder", Tipe_tutupan_lahan)) %>% dplyr::select(ID) %>% pull()
# # ... rest dynamics \ends----
# 
# 
# # setting the combined raster layer
# ref <- ref*10^6
# 
# yir <- unique(list_of_data_luc$PERIOD)
# yir <- sort(yir)
# 
# # LOOPING FRAMEWORK FOR EACH TIMESTEP====
# for(ts in 1: (length(yir)-1)){
#   # retrieve the changemap
#   ti1 <- yir[ts]
#   ti2 <- yir[ts+1]
#   d_ti <- ti2-ti1 # delta year
#   chMap_name <- paste0("chgmap_", pu_name, ti1, ti2)
#   chMap_name <- list_of_data_f[list_of_data_f$RST_NAME == chMap_name, "RST_DATA"]
#   ch_map <- getRasterFromPG(pgconf, project, chMap_name, paste0(chMap_name, ".tif"))#getRasterFromPG(pgconf, project, data_pu$RST_DATA, paste(data_pu$RST_DATA, '.tif', sep=''))
#   # combine with admin data
#   ch_map <- ref+ch_map
#   # retrieve freq
#   fr_chMap <- data.frame(freq(ch_map, useNA = "no"), stringsAsFactors= FALSE)
#   # disaggregate using substr # note that the result will be presented as character. Do convert into numeric
#   fr_chMap$IDADM <- floor(fr_chMap$value / 10^6)
#   fr_chMap$ID_Z <- fr_chMap$value%%10^2
#   fr_chMap$ID_T1 <- floor(fr_chMap$value%%10^4/10^2)
#   fr_chMap$ID_T2 <- floor(fr_chMap$value%%10^6/10^4)
#   fr_chMap$DegDef <- 0
#   
#   # assess for any indication of degradation (1) or deforestation (2)
#   # 1. Degradation====
#   fr_chMap <- fr_chMap %>% mutate(DegDef = case_when(ID_T1 %in% primaryForest_ids & ID_T2 %in% secForest_ids ~ 1,
#                                                      ID_T1 %in% hiDensFor_ids & ID_T2 %in% loDensFor_ids ~ 1,
#                                                      TRUE ~ DegDef
#   ))
#   # 1. ...\ends----
#   # 2. Deforestation====
#   fr_chMap <- fr_chMap %>% mutate(DegDef = case_when(ID_T1 %in% forest_ids & !ID_T2 %in% forest_ids ~ 2,
#                                                      TRUE ~ DegDef
#   ))
#   # 2. .... \ends----
#   # Replacing 0 at DegDef column with NA
#   fr_chMap[fr_chMap$DegDef == 0, "DegDef"] <- NA
#   # Define new 'hectare' column
#   fr_chMap <- fr_chMap %>% mutate(hectares = count* res(ref)[1]^2/10000)
#   # carbon merge t1-t2
#   for(w in 1:2){
#     if(w ==1) orNames <- names(cstock)
#     names(cstock)[1] <-paste0("ID_T", w)
#     names(cstock)[2] <-paste0("LC_", w)
#     names(cstock)[3] <- paste0("c_", w)
#     fr_chMap <- merge(fr_chMap, cstock, by= paste0("ID_T", w), all.x = TRUE)
#     if(w ==2) names(cstock) <- orNames
#   }
#   
#   # calculate emission
#   fr_chMap$Em_co2Eq <- (fr_chMap$c_1 - fr_chMap$c_2)*fr_chMap$count*res(ref)[1]^2/10000 * 3.67
#   fr_chMap$Seq <- 0
#   fr_chMap[fr_chMap$Em_co2Eq < 0, "Seq"] <- -1* fr_chMap[fr_chMap$Em_co2Eq < 0, "Em_co2Eq"]
#   fr_chMap[fr_chMap$Seq > 0, "Em_co2Eq"] <- 0 # correcting negative emission
#   # peat_em merge t1-t2
#   rec_table <- fr_chMap[, c("value", "Em_co2Eq", "Seq", "count", "DegDef")]
#   rec_table$Em_px <- rec_table$Em_co2Eq/rec_table$count
#   rec_table$Seq_px <- rec_table$Seq/rec_table$count
#   # generate emission and sequestration map
#   em_map <- reclassify(ch_map, rec_table[, c("value", "Em_px")])
#   writeRaster(em_map, paste0("QUES-C/Em_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
#   seq_map <- reclassify(ch_map, rec_table[, c("value", "Seq_px")])
#   writeRaster(seq_map, paste0("QUES-C/Seq_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
#   # Extra degdef_map
#   degDef_map <- reclassify(ch_map, rec_table[, c("value", "DegDef")])
#   writeRaster(degDef_map, paste0("QUES-C/DegDef_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
#   # peat_puID contains numbers which stand for id of pu containing peat
#   for(w in 1:2){
#     em_peatMod <- em_peat[, c(1, 3)]
#     em_peatMod[, 2] <- em_peatMod[, 2]*d_ti/2
#     names(em_peatMod)[1] <-paste0("ID_T", w)
#     names(em_peatMod)[2] <- paste0("EmPeat_", w)
#     fr_chMap <- merge(fr_chMap, em_peatMod, by= paste0("ID_T", w), all.x = TRUE)
#   }
#   # correction for peat_em which falls in non-peat planning unit (*0)
#   fr_chMap[which(!fr_chMap$ID_Z %in% peat_puID), c("EmPeat_1", "EmPeat_2")] <- fr_chMap[which(!fr_chMap$ID_Z %in% peat_puID), c("EmPeat_1", "EmPeat_2")]*0
#   # calculate total peat_em
#   fr_chMap[, c("EmPeat_1", "EmPeat_2")] <- fr_chMap[, c("EmPeat_1", "EmPeat_2")]*fr_chMap[, "count"]* res(ref)[1]^2/10000 # conversion factor to hectare
#   fr_chMap$EmPeatTot <- fr_chMap$EmPeat_1 + fr_chMap$EmPeat_2
#   # legend merge using lookup tables: pu, admin
#   fr_chMap <- merge(fr_chMap, lut_ref, by = "IDADM", all.x = TRUE)
#   fr_chMap <- merge(fr_chMap, lookup_z, by = "ID_Z", all.x =TRUE)
#   # add period annotation
#   fr_chMap$PERIOD <- paste0(ti1, "-", ti2)
#   # store at master table, in .csv, rbind with previous runs
#   if(ts == 1) carb_compile <- fr_chMap else carb_compile <- data.frame(rbind(carb_compile, fr_chMap), stringsAsFactors = FALSE)
#   # Summarize calculation result following the template: Period; Gross Em; Seq; Nett abg em; peat em; Total em. Store as .csv
#   if(ts == 1) {
#     summary_table <- data.frame(PERIOD = paste0(ti1, "-", ti2), G_Em = sum(fr_chMap$Em_co2Eq), Seq = sum(fr_chMap$Seq), P_Em = sum(fr_chMap$EmPeatTot), stringsAsFactors = FALSE)
#   } else {
#     summary_add <- data.frame(PERIOD = paste0(ti1, "-", ti2), G_Em = sum(fr_chMap$Em_co2Eq), Seq = sum(fr_chMap$Seq), P_Em = sum(fr_chMap$EmPeatTot), stringsAsFactors = FALSE)
#     summary_table <- data.frame(rbind(summary_table, summary_add), stringsAsFactors = FALSE)
#   }
#   # degDef_summTab ====
#   if(ts == 1) {
#     degDef_summTab <- data.frame(PERIOD = paste0(ti1, "-", ti2), Degradation = fr_chMap %>% filter(DegDef == 1) %>% dplyr::select(hectares) %>% pull() %>% sum(), Deforestation = fr_chMap %>% filter(DegDef == 2) %>% dplyr::select(hectares) %>% pull() %>% sum(), stringsAsFactors = FALSE)
#   } else {
#     degDef_summAdd <- data.frame(PERIOD = paste0(ti1, "-", ti2), Degradation = fr_chMap %>% filter(DegDef == 1) %>% dplyr::select(hectares) %>% pull() %>% sum(), Deforestation = fr_chMap %>% filter(DegDef == 2) %>% dplyr::select(hectares) %>% pull() %>% sum(), stringsAsFactors = FALSE)
#     degDef_summTab <- data.frame(rbind(degDef_summTab, degDef_summAdd), stringsAsFactors = FALSE)
#   }
#   # degDef_summTab \ends----
#   if(ts == (length(yir)-1)){
#     summary_table$NettAll <- summary_table$G_Em - summary_table$Seq + summary_table$P_Em
#     write.csv(summary_table, "QUES-C/summary_emission.csv", row.names = FALSE)
#     write.csv(carb_compile, "QUES-C/emission_LCdynamics.csv", row.names = FALSE)
#     write.csv(degDef_summTab, "QUES-C/summary_degDef.csv", row.names = FALSE)
#   }
# }
# 
