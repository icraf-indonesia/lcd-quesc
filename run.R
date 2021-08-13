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

# "c:\Program Files\R\R-3.6.3\bin\Rscript.exe" --vanila run.R -d data/raster/Indonesia/adm_peat.tif
# -i data/raster/Indonesia/PL_2006.tif
# -b 2006
# -f data/raster/Indonesia/PL_2009.tif
# -e 2009
# -u data/tabular/Tabel_acuan_tutupan_lahan.csv
# -z data/raster/Indonesia/Fungsi_kaw.tif
# -k data/tabular/Tabel_acuan_fungsi_kawasan.csv
# -c data/tabular/cstock.csv
# -p data/raster/Indonesia/Peat.tif
# -x data/tabular/Faktor_emisi_perubahan_gambut.csv
# -h data/raster/Indonesia/Burn18.tif

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
  make_option(c("-o", "--output"), type="character", default="summary.csv", help="output file name [default=%default]", metavar="character") 
)

opt_parser = OptionParser(option_list=option_list, add_help_option = FALSE)
args = parse_args(opt_parser)

peat_condition <- TRUE
if(is.null(args$admin)){
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
} else if(is.null(args$peat)){
  peat_condition <- FALSE
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

kode_admin <- data.frame(
  IDADM=c(1,28,29,31,27,30,24,23,7,14,26,6,11,33,8,25,4,13,2,12,10,5,22,32,16,17,9,18,15,34,21,20,19,3),
  Kode=c(11,12,13,14,15,16,17,18,19,21,31,32,33,34,35,36,51,52,53,61,62,63,64,65,71,72,73,74,75,76,81,82,91,94),
  Prov=c("ACEH","SUMATERA UTARA","SUMATERA BARAT","RIAU","JAMBI","SUMATERA SELATAN","BENGKULU","LAMPUNG","KEPULAUAN BANGKA BELITUNG","KEPULAUAN RIAU","DKI JAKARTA","JAWA BARAT","JAWA TENGAH","DAERAH ISTIMEWA YOGYAKARTA","JAWA TIMUR","BANTEN","BALI","NUSA TENGGARA BARAT","NUSA TENGGARA TIMUR","KALIMANTAN BARAT","KALIMANTAN TENGAH","KALIMANTAN SELATAN","KALIMANTAN TIMUR","KALIMANTAN UTARA","SULAWESI UTARA","SULAWESI TENGAH","SULAWESI SELATAN","SULAWESI TENGGARA","GORONTALO","SULAWESI BARAT","MALUKU","MALUKU UTARA","PAPUA BARAT","PAPUA"),
  Region=c("Sumatera","Sumatera","Sumatera","Sumatera","Sumatera","Sumatera","Sumatera","Sumatera","Sumatera","Sumatera","Jawa","Jawa","Jawa","Jawa","Jawa","Jawa","Bali_nusa","Bali_nusa","Bali_nusa","Kalimantan","Kalimantan","Kalimantan","Kalimantan","Kalimantan","Sulawesi","Sulawesi","Sulawesi","Sulawesi","Sulawesi","Sulawesi","Maluku","Maluku","Papua","Papua")
)

ref<-raster(args$admin)
admin_vat <- gsub(pattern = ".tif", replacement = ".tif.vat.dbf", args$admin)
admin_dbf <- data.frame(read.dbf(admin_vat), stringsAsFactors = FALSE)
# create an initial coverage reference for LUMENS project
names(ref)<-"Administrative maps"
# Ref.name<-names(ref)
# Ref.type<-class(ref)
# Ref.coord<-as.character(crs(ref))
# Ref.res<-res(ref)
# Ref.xmin<-xmin(ref)
# Ref.xmax<-xmax(ref)
# Ref.ymin<-ymin(ref)
# Ref.ymax<-ymax(ref)

inCarbonStock<-read.table(args$carbon_stock, header = TRUE, sep = ",")
inZoneClass<-read.table(args$zone_table, header = TRUE, sep = ",")
inLandCoverClass<-read.table(args$landcover_table, header = TRUE, sep = ",")
initialYear<-args$year1
finalYear<-args$year2
raster_category(data=args$landcover1, lookup_class = inLandCoverClass, name = "inLandCover1")
raster_category(data=args$landcover2, lookup_class = inLandCoverClass, name = "inLandCover2")
raster_category(data=args$zone, lookup_class = inZoneClass, name = "inZone", type = "pu")
inBurn<-raster(args$burn)
if(peat_condition){
  inPeatEmissionFactor<-read.table(args$peat_table, header = TRUE, sep = ",")
  inPeat<-raster(args$peat)
  inPeat<-spatial_sync_raster(inPeat, ref, method = 'ngb')
  inPeat<-inPeat*100
  
  # update admin code
  refPeat <- ref + inPeat
  refPeatDf <- as.data.frame(freq(refPeat))
  refPeatDf <- na.omit(refPeatDf)
  refPeatDf$IDADMP <- 1:nrow(refPeatDf)
  
  refPeatDf$IDADM <- refPeatDf$value %% 100
  refPeatDf <- merge(refPeatDf, kode_admin, by = "IDADM", all.x = TRUE)
  refPeatDf <- within(refPeatDf, {Peat<-ifelse(value>34, "Gambut", "Non Gambut")})
  
  kode_admin <- refPeatDf
  kode_admin$IDADM <- kode_admin$IDADMP
  kode_admin$value <- kode_admin$count <- kode_admin$IDADMP<- NULL
  
  # reclassify
  rcl_m_id1 <- as.matrix(refPeatDf$value)
  rcl_m_id2 <- as.matrix(refPeatDf$IDADMP)
  rcl_m<-cbind(rcl_m_id1, rcl_m_id2)
  # rcl_m<-rbind(rcl_m, c(0, NA))
  refPeatRec <- reclassify(refPeat, rcl_m)
  # refPeatRecDf <- as.data.frame(freq(refPeatRec))
  ref <- refPeatRec
}

# planning unit
# zone<-inZone
lookup_z<-inZoneTable
# landuse first time period
# landuse1<-inLandCover1
# landuse second time period
# landuse2<-inLandCover2
# landcover lookup table
lookup_lc<-inLandCoverClass
lookup_lc<-lookup_lc[,1:2]
colnames(lookup_lc)<-c("ID", "CLASS")
lookup_z<-inZoneTable[,c(1,3)]
colnames(lookup_z)<-c("ID_Z", "Zone")

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
dummy1<-data.frame(nPU=inZoneTable$ID, divider=nLandCoverId*nLandCoverId)
dummy1<-expandRows(dummy1, 'divider')

dummy2<-data.frame(nT1=lookup_lc$ID, divider=nLandCoverId)
dummy2<-expandRows(dummy2, 'divider')
dummy2<-data.frame(nT1=rep(dummy2$nT1, nPlanningUnitId))

dummy3<-data.frame(nT2=rep(rep(lookup_lc$ID, nLandCoverId), nPlanningUnitId))

landUseChangeMapDummy<-cbind(dummy1, dummy2, dummy3)
colnames(landUseChangeMapDummy)<-c('ZONE', 'ID_LC1', 'ID_LC2')

#=Create cross-tabulation
R<-(inZone*1) + (inLandCover1*100^1) + (inLandCover2*100^2)
# lu.db<-as.data.frame(freq(R))
# lu.db<-na.omit(lu.db)
# n<-3
# k<-0
# lu.db$value_temp<-lu.db$value
# while(k < n) {
#   eval(parse(text=(paste("lu.db$Var", n-k, "<-lu.db$value_temp %% 100", sep=""))))  
#   lu.db$value_temp<-floor(lu.db$value_temp/100)
#   k=k+1
# }
# lu.db$value_temp<-NULL
# #lu.db$value<-NULL
# colnames(lu.db)=c('ID_CHG', 'COUNT', 'ZONE', 'ID_LC1', 'ID_LC2')
# lu.db<-merge(landUseChangeMapDummy, lu.db, by=c('ZONE', 'ID_LC1', 'ID_LC2'), all=TRUE)
# lu.db$ID_CHG<-lu.db$ZONE*1 + lu.db$ID_LC1*100^1 + lu.db$ID_LC2*100^2
# lu.db<-replace(lu.db, is.na(lu.db), 0)

# writeRaster(R, filename=args$output, format='GTiff', overwrite=TRUE)

forest_ids <- inCarbonStock %>% filter(grepl("primer", as.character(inCarbonStock[,2])) | grepl("sekunder", as.character(inCarbonStock[,2]))) %>% dplyr::select(ID) %>% pull() 
# nonforest_ids <- inCarbonStock$ID %>% setdiff(forest_ids)
primaryForest_ids <- inCarbonStock %>% filter(grepl("primer", as.character(inCarbonStock[,2]))) %>% dplyr::select(ID) %>% pull()
secForest_ids <- forest_ids %>% setdiff(primaryForest_ids)

ref <- ref*10^6

# retrieve the changemap
deltaYear <- finalYear-initialYear # delta year
# combine with admin data
chg_map <- ref+R
# retrieve freq
freq_chgmap <- data.frame(freq(chg_map, useNA = "no"), stringsAsFactors= FALSE)
# disaggregate using substr # note that the result will be presented as character. Do convert into numeric
freq_chgmap$IDADM <- floor(freq_chgmap$value / 10^6)
freq_chgmap$ID_Z <- freq_chgmap$value %% 10^2
freq_chgmap$ID_T1 <- floor(freq_chgmap$value %% 10^4 / 10^2)
freq_chgmap$ID_T2 <- floor(freq_chgmap$value %% 10^6 / 10^4)
freq_chgmap$DegDef <- 0

# assess for any indication of degradation (1) or deforestation (2)
# 1. Degradation====
freq_chgmap <- freq_chgmap %>% mutate(DegDef = case_when(ID_T1 %in% primaryForest_ids & ID_T2 %in% secForest_ids ~ 1, TRUE ~ DegDef))
# 2. Deforestation====
freq_chgmap <- freq_chgmap %>% mutate(DegDef = case_when(ID_T1 %in% forest_ids & !ID_T2 %in% forest_ids ~ 2, TRUE ~ DegDef))

# Replacing 0 at DegDef column with NA
freq_chgmap[freq_chgmap$DegDef == 0, "DegDef"] <- NA
# Define new 'hectare' column
freq_chgmap <- freq_chgmap %>% mutate(hectares = count*res(ref)[1]*res(ref)[2]*(111319.9^2)/10000)

# get prov ID
freq_chgmap <- merge(freq_chgmap, kode_admin, by = "IDADM", all.x = TRUE)

# carbon merge t1-t2
inCStock<-melt(data = inCarbonStock, id.vars=c(names(inCarbonStock[1]), names(inCarbonStock[2])), measure.vars=c('Jawa', 'Bali_nusa', 'Kalimantan', 'Sulawesi', 'Maluku', 'Papua', 'Sumatera'))
for(w in 1:2){
  if(w==1) orNames <- names(inCStock)
  names(inCStock)[1] <-paste0("ID_T", w)
  names(inCStock)[2] <-paste0("LC_", w)
  names(inCStock)[3] <-paste0("Region")
  names(inCStock)[4] <- paste0("c_", w)
  freq_chgmap <- merge(freq_chgmap, inCStock, by=c(paste0("ID_T", w), "Region"), all.x = TRUE)
  if(w==2) names(inCStock) <- orNames
}

# calculate emission
freq_chgmap$Em_co2Eq <- (freq_chgmap$c_1 - freq_chgmap$c_2)*freq_chgmap$count*res(ref)[1]*res(ref)[2]*(111319.9^2)/10000 * 3.67
freq_chgmap$Seq <- 0
freq_chgmap <- freq_chgmap %>% mutate(Em_co2Eq = case_when(is.na(Em_co2Eq) ~ 0, TRUE ~ Em_co2Eq))
freq_chgmap[freq_chgmap$Em_co2Eq < 0, "Seq"] <- -1* freq_chgmap[freq_chgmap$Em_co2Eq < 0, "Em_co2Eq"]
freq_chgmap[freq_chgmap$Seq > 0, "Em_co2Eq"] <- 0 # correcting negative emission

freq_chgmap <- merge(freq_chgmap, lookup_z, by = "ID_Z", all.x =TRUE)

# calculate peat and total emission
if(peat_condition){
  for(w in 1:2){
    em_peat <- inPeatEmissionFactor[, c(1, 3)]
    em_peat[, 2] <- em_peat[, 2]*deltaYear/2
    names(em_peat)[1] <-paste0("ID_T", w)
    names(em_peat)[2] <- paste0("EmPeat_", w)
    freq_chgmap <- merge(freq_chgmap, em_peat, by= paste0("ID_T", w), all.x = TRUE)
  }
  peat_idadm <- subset(kode_admin, Peat=="Gambut", select=c(IDADM, Prov, Peat))
  colnames(peat_idadm) <- c("IDADM", "Admin", "Peat")
  # correction for peat_em which falls in non-peat planning unit (*0)
  freq_chgmap[which(!freq_chgmap$IDADM %in% peat_idadm$IDADM), c("EmPeat_1", "EmPeat_2")] <- freq_chgmap[which(!freq_chgmap$IDADM %in% peat_idadm$IDADM), c("EmPeat_1", "EmPeat_2")]*0
  # calculate total peat_em
  freq_chgmap[, c("EmPeat_1", "EmPeat_2")] <- freq_chgmap[, c("EmPeat_1", "EmPeat_2")]*freq_chgmap[, "count"]*res(ref)[1]*res(ref)[2]*(111319.9^2)/10000 # conversion factor to hectare
  freq_chgmap$EmPeatTot <- freq_chgmap$EmPeat_1 + freq_chgmap$EmPeat_2
  freq_chgmap$EmTOT <- freq_chgmap$Em_co2Eq + freq_chgmap$EmPeatTot
} else {
  freq_chgmap$EmTOT <- freq_chgmap$Em_co2Eq
}
# add period annotation
# Summary Table====
freq_chgmap$PERIOD <- paste0(initialYear, "-", finalYear)
filter_table <- freq_chgmap[, c("Kode", "Prov", "PERIOD", "EmTOT", "Seq", "count", "hectares", "DegDef")]
filter_table$Deforestasi<-NA
filter_table$Degradasi<-NA
filter_table <- within(filter_table, {Degradasi<-ifelse(DegDef==1, hectares, Degradasi)})
filter_table <- within(filter_table, {Deforestasi<-ifelse(DegDef==2, hectares, Deforestasi)})
prov_def<-aggregate(Deforestasi~Kode+Prov,data=filter_table,FUN=sum)
prov_deg<-aggregate(Degradasi~Kode+Prov,data=filter_table,FUN=sum)
prov_em<-aggregate(EmTOT~Kode+Prov,data=filter_table,FUN=sum)
prov_seq<-aggregate(Seq~Kode+Prov,data=filter_table,FUN=sum)
summary_table <- merge(prov_em, prov_seq, by=c("Kode", "Prov"), all.x=T)
summary_table <- merge(summary_table, prov_def, by=c("Kode", "Prov"), all.x=T)
summary_table <- merge(summary_table, prov_deg, by=c("Kode", "Prov"), all.x=T)
summary_table <- replace(summary_table, is.na(summary_table), 0)
summary_table$tahun_periode_awal <- initialYear
summary_table$tahun_periode_akhir <- finalYear
colnames(summary_table) <- c("id_prov", "prov", "emisi", "sekuestrasi", "deforestasi", "degradasi", "tahun_periode_awal", "tahun_periode_akhir")
write.table(summary_table, args$output, quote=FALSE, row.names=FALSE, sep=",")  

#=Create individual table for each landuse map
# set area and classified land use/cover for first landcover and second
# freqLanduse_1<-inLandCover1Table
# area_lc1<-subset(freqLanduse_1, select=c('ID','COUNT', 'Classified'))
# colnames(area_lc1) = c("ID", "COUNT_LC1", "Classified1") 
# area_lc1<-merge(area_lc1,lookup_l,by="ID")
# colnames(area_lc1)[4] = "CLASS_LC1"
# 
# freqLanduse_2<-inLandCover2Table 
# area_lc2<-subset(freqLanduse_2, select=c('ID','COUNT', 'Classified'))
# colnames(area_lc2) = c("ID", "COUNT_LC2", "Classified2")
# area_lc2<-merge(area_lc2,lookup_l,by="ID")
# colnames(area_lc2)[4] = "CLASS_LC2"

# combine all data in data_merge
# colnames(lookup_l)[1]="ID_LC1"
# colnames(lookup_l)[2]="LC_t1"
# data_merge <- merge(lu.db,lookup_l,by="ID_LC1")
# colnames(lookup_l)[1]="ID_LC2"
# colnames(lookup_l)[2]="LC_t2"
# data_merge <- as.data.frame(merge(data_merge,lookup_l,by="ID_LC2"))
# colnames(lookup_z)[1]="ZONE"
# colnames(lookup_z)[2]="COUNT_ZONE"
# colnames(lookup_z)[3]="Z_NAME"
# data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))
# data_merge$COUNT<-data_merge$COUNT*Spat_res
# colnames(lookup_l)<-c("ID", "CLASS")
# colnames(lookup_z)<-c("ID", "COUNT_ZONE", "ZONE")
# area_zone<-lookup_z
# data_merge_sel <- data_merge[ which(data_merge$COUNT > 0),]
# data_merge_sel$LU_CHG <- do.call(paste, c(data_merge_sel[c("LC_t1", "LC_t2")], sep = " to "))
# eval(parse(text=(paste("write.dbf(data_merge,'lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep=""))))


