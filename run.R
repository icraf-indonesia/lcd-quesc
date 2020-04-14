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

# libraries with no duplicates====
library(rtf)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(rgeos)
library(grid)
library(jsonlite)
library(RPostgreSQL)
library(DBI)
library(rpostgis)
library(spatial.tools)
library(rgdal)
library(plyr)
library(tiff)
library(reshape2)
library(foreign)
library(splitstackshape)
library(magick)
library(vegan)
library(RSQLite)
library(SDMTools)
library(dplyr)
library(gridExtra)
library(pracma)
library(zoo)



# 0. INPUTS====
working_directory= "D:/"
raster_directory= paste0(working_directory, "data/raster")
table_directory =  paste0(working_directory, "data/table")
description= "bauH"
province= "Jambi"
country= "Indonesia"
admin_attribute= readOGR(dsn = "D:/data/spatial/vector", layer = "admin_lumens_final") # ADedit
field_attribute= "IDADM" # ADedit
spat_res= 100
dissolve_table = paste0(table_directory, "/kab.csv")
# 0.5 EXTRA INPUT VARIABLES====
# scenarios <- c("bauP", "ggp") #ADedit
scenarios <- c("bauH", "GG") # ADCHECK
# block <- c("low", "high", "wet")

# LOOPING PER SCENARIOS====
# for(sc in 1: length(scenarios)){ #ADopen
sc = 1 # ADCHECK
# LOOPING PER BLOCK====
# for(bck in 1: length(block)){ # ADopen cancelled
# 1. create database====
location= "Jambi"
project= paste0(scenarios[sc], "_", location)

#=Set time start
time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")


# check desktop architecture
win_arch=Sys.getenv("R_ARCH")
user_doc = Sys.getenv("USERPROFILE")
LUMENS_path = paste0(Sys.getenv("ProgramFiles"), "\\LUMENS")
if (file.exists(LUMENS_path)){
  processing_path = paste0(LUMENS_path, "\\apps\\qgis\\python\\plugins\\processing\\r\\scripts")
} else{
  LUMENS_path = paste0(Sys.getenv("ProgramFiles(x86)"), "\\LUMENS")
  processing_path = paste0(LUMENS_path, "\\apps\\qgis\\python\\plugins\\processing\\r\\scripts")
}
postgre_path = paste0(Sys.getenv("ProgramFiles"), "\\PostgreSQL\\9.6")
if(!file.exists(postgre_path)){
  postgre_path = paste0(Sys.getenv("ProgramFiles(x86)"), "\\PostgreSQL\\9.6")
  if(!file.exists(postgre_path)){
    statuscode<-0
    statusmessage<-"Please install PostgreSQL database.."
    statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
    quit()
  }
}

# set PostgreSQL driver and profile
user_appdata<-Sys.getenv("APPDATA")
pgconf_file<-paste0(user_appdata, "\\postgresql\\pgpass.conf")
if(file.exists(pgconf_file)){
  pgconf_line<-readLines(pgconf_file)
  pgconf_len<-length(pgconf_line)
  pgconf_line<-pgconf_line[pgconf_len]
  pgconf_list<-unlist(str_split(pgconf_line, ':'))
  pgconf<-data.frame(rbind(pgconf_list))
  colnames(pgconf)<-c("host", "port", "auth", "user", "pass")
} else {
  # please install PostgreSQL 
  statuscode<-0
  statusmessage<-"Please check PostgreSQL configuration.."
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
  quit()
}

# check status of PostgreSQL server
pg_isready<-paste0("pg_isready -p ", pgconf$port)
pg_response<-system(pg_isready)
if(pg_response==2){
  # please check your connection
  statuscode<-0
  statusmessage<-"Please check PostgreSQL connection.."
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
  quit()
}

#=Create structure folder for LUMENS project 
setwd(working_directory)
project<-str_replace_all(string=project, pattern=" ", repl="_")
project_path <- paste(working_directory, "/", project, sep="")
PUR_path <- paste(project_path, "/PUR", sep="")
QUES_path <- paste(project_path, "/QUES", sep="")
PreQUES_path <- paste(QUES_path, "/PreQUES", sep="")
QUESC_path <- paste(QUES_path, "/QUES-C", sep="")
QUESB_path <- paste(QUES_path, "/QUES-B", sep="")
QUESH_path <- paste(QUES_path, "/QUES-H", sep="")
TA_path <- paste(project_path, "/TA", sep="")
SCIENDO_path  <- paste(project_path, "/SCIENDO", sep="")
#help_path  <- paste(LUMENS_path, "/help", sep="")
dir.create(project_path, mode="0777")
dir.create(PUR_path, mode="0777")
dir.create(QUES_path, mode="0777")
dir.create(PreQUES_path, mode="0777")
dir.create(QUESC_path, mode="0777")
dir.create(QUESB_path, mode="0777")
dir.create(QUESH_path, mode="0777")
dir.create(TA_path, mode="0777")
dir.create(SCIENDO_path, mode="0777")
#dir.create(help_path, mode="0777")

#This variables only to find out the identity of user who create database for the first time 
user_temp_folder<-Sys.getenv("TEMP")
if(user_temp_folder=="") {
  user_temp_folder<-Sys.getenv("TMP")
}
LUMENS_path_user <- paste(user_temp_folder,"/LUMENS", sep="") 
dir.create(LUMENS_path_user, mode="0777")

# clear temp first
setwd(LUMENS_path_user)
unlink(list.files(pattern="*"))

#=Set reference data
# save as temporary data 
setwd(project_path)
writeOGR(admin_attribute, dsn=project_path, "reference", overwrite_layer=TRUE, driver="ESRI Shapefile")
# rasterizing the polygon data of reference (e.g administrative, such as district or province boundary map) using gdal_rasterize
shp_dir<-paste(project_path,"/", "reference.shp", sep="")
file_out<-paste(project_path, "/", "reference.tif", sep="")
res<-spat_res
gdalraster<-paste0("\"", LUMENS_path, "\\bin\\gdal_rasterize.exe\"")
osgeo_comm<-paste(gdalraster, shp_dir, file_out,"-a IDADM -tr", res, res, "-a_nodata 255 -ot Byte", sep=" ")
system(osgeo_comm)

# create an initial coverage reference for LUMENS project
ref<-raster(file_out)
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
# load reference attribute from csv dissolve table 
lut_ref<-read.table(dissolve_table, header=TRUE, sep=",")
colnames(lut_ref)[2]="ADMIN_UNIT"

# set batch parameter 
pgEnvBatch <- paste(LUMENS_path_user, "/pg_env.bat", sep="")
pathEnv = ""
pathEnv[1] = paste0("@SET PATH=", postgre_path, "\\bin;%PATH%")
pathEnv[2] = paste0("@SET PGDATA=", postgre_path, "\\data")
pathEnv[3] = paste0("@SET PGUSER=", pgconf$user)
pathEnv[4] = paste0("@SET PGPORT=", pgconf$port)
pathEnv[5] = paste0("@SET PGLOCALEDIR=", postgre_path, "\\share\\locale\n")

createNewPGTbl = pathEnv
# project as a new pg_db name
createNewPGTbl[6] = paste("createdb ", project, sep="")
createNewPGTbl[7] = paste('psql -d ', project, ' -c "CREATE EXTENSION postgis;"', sep="")
createNewPGTbl[8] = paste('psql -d ', project, ' -c "CREATE EXTENSION postgis_topology;"\n', sep="")

newBatchFile <- file(pgEnvBatch)
writeLines(createNewPGTbl, newBatchFile)
close(newBatchFile)
# execute batch file
pgEnvBatchFile<-str_replace_all(string=pgEnvBatch, pattern="/", repl='\\\\')
system(pgEnvBatchFile)

# set driver connection
driver <- dbDriver('PostgreSQL')
DB <- dbConnect(
  driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)
# add reference map to database
# xxBUI = bit unsigned integer
# xxBSI = bit signed integer
# xxBF  = bit float
# pgWriteRast(DB, c("public", "ref_map"), raster=ref)

srid<-tryCatch({pgSRID(DB, crs(ref), create.srid = TRUE)}, error=function(e){ })
# ADDRASTERTOPG function
addRasterToPG<-function(project, raster.path, raster.name, raster.srid) {
  createNewPGTbl = pathEnv
  createNewPGTbl[6] = paste('raster2pgsql -s ', raster.srid, ' -I -C -t auto ', str_replace_all(string=raster.path, pattern="/", repl='\\\\'), ' public.', raster.name, ' | psql -d ', project, sep="")
  
  newBatchFile <- file(pgEnvBatch)
  writeLines(createNewPGTbl, newBatchFile)
  close(newBatchFile)
  # execute batch file
  pgEnvBatchFile<-str_replace_all(string=pgEnvBatch, pattern="/", repl='\\\\')
  system(pgEnvBatchFile)
}
addRasterToPG(project, 'reference.tif', 'ref_map', srid)

# unlink shapefile and raster
file.rename("reference.tif", "base.tif")
unlink(list.files(pattern = "reference"))
file.rename("base.tif", "reference.tif")

# write project properties into table
proj_descr <- as.data.frame(rbind(project, description, working_directory, location, province, country))
test<-c(rownames(proj_descr))
proj_descr<-cbind(test, proj_descr)
colnames(proj_descr)[1]<-"Type"
colnames(proj_descr)[2]<-"Description"
proj_descr<-as.data.frame(proj_descr)
proj.file<-paste(project_path, "/",project,".lpj", sep="")

#=Set all values, functions, and initial indices to zero, for each index serves as a counter
# e.g landuse.index serve as a counter of landuse numbers
#setwd(DATA_path)
db_name<-paste(project, ".lpj", sep="")
idx_landuse=0
idx_pu=1
idx_rec_pu=0
idx_factor=0
idx_lut=0
idx_lut_carbon=0
idx_lut_landuse=0
idx_lut_pu=0
idx_period=0
idx_PUR=0
idx_PreQUES=0
idx_QUESC=0
idx_QUESB=0
idx_QUESH=0
idx_SCIENDO_led=0
idx_SCIENDO_lucm=0
idx_TA_opcost=0
idx_TA_regeco=0
# getting an information of windows architecture through the path of LUMENS installation 
gdaltranslate<-paste0("\"",LUMENS_path, "\\bin\\gdal_translate.exe\"")
# prepare some functions and store it to LUMENS project file, so it can be used later
# RESAVE function
resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}
# GET_FROM_RDB function
# get_from_rdb <- function(symbol, filebase, envir =parent.frame()){
#   lazyLoad(filebase = filebase, envir = envir, filter = function(x) x == symbol)
# }
# GETRASTERFROMPG function
getRasterFromPG<-function(pg_conf, pg_database, pg_table, pg_rasterfile) { 
  if(!file.exists(pg_rasterfile)){
    postgres_connection<-paste('PG:\"host=', as.character(pg_conf$host), ' port=', as.character(pg_conf$port), ' dbname=\'', pg_database, '\' user=\'', as.character(pg_conf$user), '\' password=\'', as.character(pg_conf$pass), '\' schema=\'public\'', sep="")
    gdaltranslate_cmd<-paste(gdaltranslate, postgres_connection, sep=" ")
    
    postgres_table<-paste('table=\'', pg_table, '\' mode=2\"', sep="")
    # postgres_output<-paste('-a_nodata', pg_nodata, pg_rasterfile, sep=" ")
    gdaltranslate_cmd_pu<-paste(gdaltranslate_cmd, postgres_table, pg_rasterfile, sep=" ") 
    system(gdaltranslate_cmd_pu)
  }
  loadRaster<-raster(pg_rasterfile)
  loadRaster<-reclassify(loadRaster, cbind(255, NA))
  
  # define projection
  crs(loadRaster)<-as.character(cov_desc[3,2])
  
  # sync spatial data
  if(pg_table!='ref_map'){
    compareData<-tryCatch({ compareRaster(ref, loadRaster) }, error=function(e){})
    if(is.null(compareData)){ loadRaster<-spatial_sync_raster(loadRaster, ref, method="ngb") }  
  }
  return(loadRaster)
}
# writeRastFile function
writeRastFile <- function(raster_in, raster_ou_path = character(), cat = FALSE, colorpal, lookup){
  # the function is to replace 'writeRaster' usage in the LUMENS scripts so that not only the raster file will be written as '.tif' but also the '.qml' file will be automatically created
  writeRaster(raster_in, raster_ou_path, format = "GTiff", overwrite = TRUE)
  # assessing the values in 'raster_in'
  if(cat){
    u_values <- unique(values(raster_in))
    u_values <- u_values[!is.na(u_values)]
  } else{
    u_values <- numeric()
    u_values[1] <- min(values(raster_in), na.rm = TRUE)
    u_values[2] <- 0
    u_values[3] <- max(values(raster_in), na.rm = TRUE)
    u_values[2] <- u_values[1] + (u_values[3] - u_values[1])/2# the average of the min and max
  }
  u_values <- u_values[order(u_values)]
  # the only difference between the continuous and discrete style is in the number of <item> under <colorrampshader>
  # writing the qml file
  if(grepl(".tif$", raster_ou_path)){
    qml_file_conn <- file(gsub(pattern = ".tif", replacement = ".qml", x = raster_ou_path))
  } else qml_file_conn <- file(paste0(raster_ou_path, ".qml"))
  qml_texts <- character()
  # standardized lines
  qml_texts[1] <- paste0('<!DOCTYPE qgis PUBLIC \'http://mrcc.com/qgis.dtd\' \'SYSTEM\'>')
  qml_texts[2] <- paste0('<qgis version="2.0.0-Taoge" minimumScale="0" maximumScale="1e+08" hasScaleBasedVisibilityFlag="0">')
  qml_texts[3] <- paste0('  <pipe>')
  qml_texts[4] <- paste0('    <rasterrenderer opacity="1" alphaBand="-1" classificationMax="', max(u_values), '" classificationMinMaxOrigin="MinMaxFullExtentEstimated" band="1" classificationMin="', min(u_values), '" type="singlebandpseudocolor">')
  qml_texts[5] <- paste0('      <rasterTransparency/>')
  qml_texts[6] <- paste0('      <rastershader>')
  qml_texts[7] <- paste0('        <colorrampshader colorRampTye="INTERPOLATED" clip="0">')
  # generating the right number of colors according to the colorpal (using 'colorRampPalette')
  my_col <- colorRampPalette(colorpal)
  my_col <- my_col(length(u_values))# number of classes as the basis of color interpolation
  # define labels for each values
  if(cat){
    names(lookup) <- c("values", "labels")
    lbls <- data.frame(values = u_values, orr = seq(length(u_values)), stringsAsFactors = FALSE)
    lookup$values <- as.numeric(lookup$values)
    lbls <- merge(lbls, lookup, by = "values", all.x = TRUE)
    lbls <- lbls[order(lbls$orr),"labels"]
  } else {
    lbls <- round(u_values,digits = 2)
  }
  # looping to generate the qml lines
  for(ql in 8: (7+length(u_values))){
    qml_texts[ql] <- paste0('          <item alpha="255" value="', u_values[ql-7], '" label="', lbls[ql-7], '" color="', my_col[ql-7], '"/>')
  }
  qml_texts <- c(qml_texts, paste0('        </colorrampshader>'))
  qml_texts <- c(qml_texts, paste0('      </rastershader>'))
  qml_texts <- c(qml_texts, paste0('    </rasterrenderer>'))
  qml_texts <- c(qml_texts, paste0('    <brightnesscontrast brightness="0" contrast="0"/>'))
  qml_texts <- c(qml_texts, paste0('    <huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>'))
  qml_texts <- c(qml_texts, paste0('    <rasterresampler maxOversampling="2"/>'))
  qml_texts <- c(qml_texts, paste0('  </pipe>'))
  qml_texts <- c(qml_texts, paste0('  <blendMode>0</blendMode>'))
  qml_texts <- c(qml_texts, paste0('</qgis>'))
  # writing down the qml lines into the path specified at qml_file_conn
  writeLines(qml_texts, qml_file_conn)
  close(qml_file_conn)
  # writing the .csv containing the paths of the output rasters
  if(file.exists(paste0(LUMENS_path_user,"/ou_raster.csv"))){
    ou_raster_paths <- read.csv(paste0(LUMENS_path_user,"/ou_raster.csv"), stringsAsFactors = FALSE)
    ou_raster_paths <- rbind(ou_raster_paths, raster_ou_path)
  } else {
    ou_raster_paths <- data.frame(raster_ou_path = raster_ou_path, stringsAsFactors = FALSE)
  }
  write.csv(ou_raster_paths, paste0(LUMENS_path_user,"/ou_raster.csv"), row.names = FALSE)
}

#=Save all params into .RData objects
save(LUMENS_path_user,
     LUMENS_path,
     pgEnvBatch,
     pathEnv,
     idx_landuse,
     proj_descr,
     ref,
     srid,
     lut_ref,
     location,
     province,
     country,
     cov_desc,
     idx_pu,
     idx_rec_pu,
     idx_factor,
     idx_lut,
     idx_lut_carbon,
     idx_lut_landuse,
     idx_lut_pu,
     idx_period,
     idx_PUR,
     idx_PreQUES,
     idx_QUESC,
     idx_QUESB,
     idx_QUESH,
     idx_SCIENDO_led,
     idx_SCIENDO_lucm,
     idx_TA_opcost,
     idx_TA_regeco,
     win_arch,
     processing_path,
     gdalraster,
     gdaltranslate,
     addRasterToPG,
     getRasterFromPG,
     postgre_path,
     pgconf,
     user_doc,
     resave,
     writeRastFile,
     file=proj.file)
# write the properties of reference data to PostgreSQL
eval(parse(text=(paste("list_of_data_pu<-data.frame(RST_DATA='ref', RST_NAME=names(ref), LUT_NAME='lut_ref', row.names=NULL)", sep=""))))
csv_file<-paste(LUMENS_path_user,"/csv_planning_unit.csv", sep="")
write.table(list_of_data_pu, csv_file, quote=FALSE, row.names=FALSE, sep=",")

dbWriteTable(DB, "list_of_data_pu", list_of_data_pu, append=TRUE, row.names=FALSE)
dbWriteTable(DB, "lut_ref", lut_ref, row.names=FALSE)
dbDisconnect(DB)

#=Create LUMENS Project Report (.doc)
# arrange numerous colors with RColorBrewer
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- rev(brewer.pal(11, "RdYlGn"))
myColors8 <- "#000000"
myColors9 <- brewer.pal(12, "Set3")
if (0 %in% lut_ref$IDADM){
  myColors  <-c(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
} else {
  myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
}
# create an Rplot for reference map
myColors.lu <- myColors[1:(length(unique(lut_ref$IDADM))+1)]
ColScale.lu<-scale_fill_manual(name=field_attribute, breaks=c(0, lut_ref$IDADM), labels=c("NoData", as.character(lut_ref$ADMIN_UNIT)), values=myColors.lu)
plot.admin<-gplot(ref, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu + theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=10),
         legend.text = element_text(size=10),
         legend.key.height = unit(0.35, "cm"),
         legend.key.width = unit(0.35, "cm"))
# write report   
title1<-"{\\colortbl;\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red146\\green208\\blue80;\\red0\\green176\\blue240;\\red140\\green175\\blue71;\\red0\\green112\\blue192;\\red79\\green98\\blue40;} \\pard\\qr\\b\\fs70\\cf2 L\\cf3U\\cf4M\\cf5E\\cf6N\\cf7S \\cf1REPORT \\par\\b0\\fs20\\ql\\cf1"
title2<-paste("\\pard\\qr\\b\\fs40\\cf1 Create LUMENS Project ", "for ", location, ", ", province, ", ", country, "\\par\\b0\\fs20\\ql\\cf1", sep="")
sub_title<-"\\cf2\\b\\fs32 Ringkasan Deskripsi Projek\\cf1\\b0\\fs20"
chapter1<-"\\cf2\\b\\fs28 Deskripsi Projek \\cf1\\b0\\fs20"
chapter2<-"\\cf2\\b\\fs28 Cakupan Geografis Projek \\cf1\\b0\\fs20"
chapter3<-"\\cf2\\b\\fs28 Data-data Acuan Dalam Projek \\cf1\\b0\\fs20"
time_start<-paste("Proses LUMENS dimulai : ", time_start, sep="")
time_end<-paste("Proses LUMENS selesai : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("-------------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
rtffile <- RTF(paste0(project, "_", gsub(" ", "_", location), ".doc"), font.size=9)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, title1)
addParagraph(rtffile, title2)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, time_start)
#addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
width<-as.vector(c(1.34,3.1))
addTable(rtffile,proj_descr,font.size=8,col.widths=width)
addPageBreak(rtffile)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
#addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, paste("Selamat datang di LUMENS!!. Anda telah berhasil menyusun konfigurasi data-data awal yang akan digunakan dalam perencanaan penggunaan lahan yang mempertimbangkan berbagai fungsi lingkungan. LUMENS project file terdiri dari dua file utama dengan akhiran .lpj dan lpd. Project file yang telah anda buat bernama ", project, ".lpj."))
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Deskripsi projek menyimpan informasi umum yang anda masukkan mengenai projek ini")
addNewLine(rtffile)
width<-as.vector(c(1.34,3.1))
addTable(rtffile,proj_descr,font.size=8,col.widths=width)
addNewLine(rtffile)
addParagraph(rtffile, chapter2)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Cakupan geografis projek menyimpan informasi mengenai cakupan area yang akan digunakan di dalam project, batas-batas koordinat, sistem projeksi serta resolusi spasial yang akan digunakan dalam projek")
addNewLine(rtffile)
addTable(rtffile,cov_desc,font.size=8,col.widths=width)
addNewLine(rtffile)
addPageBreak(rtffile)
addParagraph(rtffile, chapter3)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Berikut ini adalah beberapa data yang akan dijadikan data acuan dalam projek ini")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\cf4\\b \\fs20 Peta batas administrasi\\b \\fs20\\cf1", sep=" "))
addPlot(rtffile,plot.fun=print, width=6,height=4.5,res=150,  plot.admin)
addNewLine(rtffile)
done(rtffile)
# show result via shell command 

# detect winword
# rtf viewer

#command<-paste("start ", "winword ", project_path, "/LUMENS_Create-Project_report.doc", sep="" )
#shell(command)

#=Writing final status message (code, message)
statuscode<-1
statusmessage<-"LUMENS database has been created!"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)

# ADhere 14.19
# 2 Add tabular data: lu_lookup, contrast, c_lookup, c_peat into database====
DB <- dbConnect(
  driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)
fil_tbl <- c("lut_lc_f.csv", "contrast_GGjambi.csv", "cstock_jambi_v4.csv", "em_peat_v4.csv")
tname <- c("lu_lookup", "contrast", "cstock", "em_peat")
# LOOP====
for(td in 1:length(tname)){ # ADopen
  
  table_name <- tname[td]
  tb_bute= paste0(table_directory, "/", fil_tbl[td])
  table_name <- str_replace_all(string=table_name, pattern=" ", repl=".")
  
  idx_lut<-idx_lut+1
  eval(parse(text=(paste("in_lut", idx_lut, "<-read.table(tb_bute, header=TRUE, sep=',')", sep=""))))
  
  eval(parse(text=(paste("list_of_data_lut<-data.frame(TBL_DATA='in_lut", idx_lut,"', TBL_NAME='", table_name, "', row.names=NULL)", sep=""))))
  
  InLUT_i <- paste('in_lut', idx_lut, sep="")
  
  dbWriteTable(DB, "list_of_data_lut", list_of_data_lut, append=TRUE, row.names=FALSE)
  dbWriteTable(DB, InLUT_i, eval(parse(text=(paste(InLUT_i, sep="" )))), append=TRUE, row.names=FALSE)
  
  #write to csv # rudimentary, removed
  list_of_data_lut<-dbReadTable(DB, c("public", "list_of_data_lut"))
  csv_file<-paste(LUMENS_path_user,"/csv_lookup_table.csv", sep="")
  write.table(list_of_data_lut, csv_file, quote=FALSE, row.names=FALSE, sep=",")
  
  resave(idx_lut, file=proj.file)
} #ADopen
dbDisconnect(DB)

#=Writing final status message (code, message)
statuscode<-1
statusmessage<-"Lookup table has been added"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)

# ADCHECK
# 3 Add raster data: all raster data within designated scenario folder====
yir <- 2018
yir <- c(yir, 2024, 2030, 2036, 2045)
# } else if(sc==2){
#   # yir <- 2017
#   yir <- c(2024, 2030, 2036, 2045)
# }
# yir <- c(2014, 2018, 2022)
# define the source file of landuse lookup table
leg_lc <- paste0(table_directory, "/", fil_tbl[1]) # reuse previously defined table


gen.att_lu <- function(rs_eman=character(), lookup_tl = leg_lc){# rs_eman = file location of the raster data; lookup_tl = file location of the lookup
  lookup_tl <- read.csv(lookup_tl, stringsAsFactors = FALSE)
  names(lookup_tl) <- c("Value", "CLASS", "Trajectory")
  vat_name <- gsub(pattern = ".tif", replacement = ".tif.vat.dbf", rs_eman)
  ati_table <- data.frame(read.dbf(vat_name), stringsAsFactors = FALSE)
  # merge with the selected lookup table
  ati_table <- merge(ati_table, lookup_tl, by = "Value", all.x = TRUE)
  ati_table <- ati_table[, c("Value", "Count", "CLASS", "Trajectory")]
  # saving into the same directory
  ati_name <- gsub(".tif$", ".csv", rs_eman)
  write.csv(ati_table, ati_name, row.names = FALSE)
  if(file.exists(ati_name)) print(paste0("raster attribute for year", yir[nyir], " has been successfully written"))
  return(ati_name)
}

# LOOP year-th times ADopen====
for(nyir in 1:length(yir)){
  ##DB-PostgreSQL=group
  type = 0
  period = yir[nyir]
  data = paste0(raster_directory, "/", scenarios[sc], "/lc_", scenarios[sc], "_", period, ".tif") # filename
  description= paste0("lc_", scenarios[sc], "_", period) # raster name
  attribute_table= gen.att_lu(data) # attribute table location which has columns c("ID", "COUNT", "Legend", "Classified")
  #statusoutput=output table
  #passfilenames
  
  #=Load active project 
  # load(proj.file)
  
  #=Create raster_category function
  # to synchronize all of the data spatial input
  command="raster"
  raster_category<-function(category, name, desc) {
    eval(parse(text=(paste(name, "<<-", command,'("', data, '")', sep=""))))
    eval(parse(text=(paste(name, "<<-spatial_sync_raster(", name, ',', 'ref, method = "ngb")', sep=""))))
    eval(parse(text=(paste(name, "<<-", name, "*1",  sep=""))))
    eval(parse(text=(paste("names(", name, ")<<-desc", sep=""))))
    eval(parse(text=(paste(name, "@title<<-category", sep=""))))
  }
  
  # set driver connection
  driver <- dbDriver('PostgreSQL')
  # project <- as.character(proj_descr[1,2])
  DB <- dbConnect(
    driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
    user=as.character(pgconf$user), password=as.character(pgconf$pass)
  )
  
  #=Classify raster into three types of input
  # type 0: land_use_cover
  # type 1: planning_unit
  # type 2: factor
  if(type==0){
    category<-"land_use_cover"
    data_name<-"lulc"
    
    #write index
    idx_landuse<-idx_landuse+1
    idx_period<-idx_period+1
    eval(parse(text=(paste("period", idx_period, "<-period", sep=""))))
    period_i<-paste("period", idx_period, sep="")
    eval(parse(text=(paste(period_i, "<-period", sep="" ))))
    index1<-idx_landuse
    
    #create raster data & reclass the value of nodata 
    # tryCatch({
    #   raster_category(category=category, name=paste("in_hist_", data_name, index1, sep=""), desc=description) 
    # }, error=function(e){ 
    #   statuscode<-0
    #   statusmessage<-e    
    # })
    raster_temp<-raster(data)
    raster_temp<-reclassify(raster_temp, cbind(NA, 255)) # need to set as a dynamic variable
    raster_temp_name<-paste0(LUMENS_path_user, "/raster_temp.tif")
    writeRaster(raster_temp, filename=raster_temp_name, format="GTiff", overwrite=TRUE)
    
    #create attribute table
    attribute_table<-read.table(attribute_table, sep=",", header = TRUE)
    colnames(attribute_table)<-c("ID", "COUNT", "Legend", "Classified")
    eval(parse(text=(paste("in_hist_", data_name, "_lut", idx_landuse, "<-attribute_table",  sep=""))))
    
    #write raster detail to PostgreSQL
    eval(parse(text=(paste("list_of_data_luc<-data.frame(RST_DATA='in_hist_", data_name, idx_landuse,"', RST_NAME='", description, "', PERIOD=", period, ", LUT_NAME='in_hist_", data_name,"_lut", idx_landuse, "', row.names=NULL)", sep=""))))
    
    InHistLanduseLUT_i <- paste('in_hist_', data_name, "_lut", idx_landuse, sep="")
    InHistLanduse_i <- paste('in_hist_', data_name, idx_landuse, sep="")
    
    #append list
    dbWriteTable(DB, "list_of_data_luc", list_of_data_luc, append=TRUE, row.names=FALSE)
    dbWriteTable(DB, InHistLanduseLUT_i, eval(parse(text=(paste(InHistLanduseLUT_i, sep="" )))), append=TRUE, row.names=FALSE)
    # pgWriteRast(DB, c("public", InHistLanduse_i), raster=eval(parse(text=(paste(InHistLanduse_i, sep="" )))))
    
    #write to csv
    list_of_data_luc<-dbReadTable(DB, c("public", "list_of_data_luc"))
    csv_file<-paste(LUMENS_path_user,"/csv_", category, ".csv", sep="")
    write.table(list_of_data_luc, csv_file, quote=FALSE, row.names=FALSE, sep=",")
    
    addRasterToPG(project, raster_temp_name, InHistLanduse_i, srid)
    
    # resave index
    eval(parse(text=(paste("resave(idx_landuse, idx_period, ", period_i, ", file=proj.file)", sep=""))))
    
    statuscode<-1
    statusmessage<-"land use/cover data has been added"
  }
  dbDisconnect(DB)
  
  #=Writing final status message (code, message)
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
} # Loop for nyir ends ADopen


# to add planning unit raster ADadd====
type = 1
# period = yir[nyir]
data = paste0(raster_directory, "/plan_unit_jambi_final_f.tif") # filename
description= "pu_ver1" # raster name
attribute_table= paste0(table_directory, "/pu_lookup_f.csv") # attribute table location which has columns c("ID", "COUNT", "Legend")
#statusoutput=output table
#passfilenames

#=Load active project 
# load(proj.file)

#=Create raster_category function
# to synchronize all of the data spatial input
command="raster"
raster_category<-function(category, name, desc) {
  eval(parse(text=(paste(name, "<<-", command,'("', data, '")', sep=""))))
  eval(parse(text=(paste(name, "<<-spatial_sync_raster(", name, ',', 'ref, method = "ngb")', sep=""))))
  eval(parse(text=(paste(name, "<<-", name, "*1",  sep=""))))
  eval(parse(text=(paste("names(", name, ")<<-desc", sep=""))))
  eval(parse(text=(paste(name, "@title<<-category", sep=""))))
}

# set driver connection
# driver <- dbDriver('PostgreSQL')
# project <- as.character(proj_descr[1,2])
DB <- dbConnect(
  driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)

#=Classify raster into three types of input
# type 0: land_use_cover
# type 1: planning_unit
# type 2: factor
if(type==1){
  category<-"planning_unit"
  data_name<-"in_pu"
  
  #write index
  idx_pu<-idx_pu+1
  index1<-idx_pu
  
  # tryCatch({
  #   raster_category(category=category, name=paste(data_name, index1, sep=""), desc=description) 
  # }, error=function(e){ 
  #   statuscode<-0
  #   statusmessage<-e    
  # })
  raster_temp<-raster(data)
  raster_temp<-reclassify(raster_temp, cbind(NA, 255)) # need to set as a dynamic variable
  raster_temp_name<-paste0(LUMENS_path_user, "/raster_temp.tif")
  writeRaster(raster_temp, filename=raster_temp_name, format="GTiff", overwrite=TRUE)
  
  attribute_table<-read.csv(attribute_table, header = TRUE)
  colnames(attribute_table)<-c("ID", "COUNT", "Legend")
  eval(parse(text=(paste(data_name, "_lut", idx_pu, "<-attribute_table",  sep=""))))
  
  eval(parse(text=(paste("list_of_data_pu<-data.frame(RST_DATA='", data_name, idx_pu,"', RST_NAME='", description, "', LUT_NAME='", data_name, "_lut", idx_pu, "', row.names=NULL)", sep=""))))
  
  InPuLUT_i <- paste(data_name, "_lut", idx_pu, sep="")
  InPu_i <- paste(data_name, idx_pu, sep="")
  
  #append list
  dbWriteTable(DB, "list_of_data_pu", list_of_data_pu, append=TRUE, row.names=FALSE)
  dbWriteTable(DB, InPuLUT_i, eval(parse(text=(paste(InPuLUT_i, sep="" )))), append=TRUE, row.names=FALSE)
  # pgWriteRast(DB, c("public", InPu_i), raster=eval(parse(text=(paste(InPu_i, sep="" )))))
  
  #write to csv
  list_of_data_pu<-dbReadTable(DB, c("public", "list_of_data_pu"))
  csv_file<-paste(LUMENS_path_user,"/csv_", category, ".csv", sep="")
  write.table(list_of_data_pu, csv_file, quote=FALSE, row.names=FALSE, sep=",")
  
  addRasterToPG(project, raster_temp_name, InPu_i, srid)
  
  resave(idx_pu, file=proj.file)
  
  statuscode<-1
  statusmessage<-"planning unit has been added"
}

dbDisconnect(DB)

#=Writing final status message (code, message)
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)



# 4. Running PREQUES====

# INPUTS
analysis.option<- 1 # should have entered 0 for all analysis
planning_unit = "pu_ver1"
lukup_lc = "lu_lookup"
raster.nodata = 0
proj.file <- paste0(working_directory, "/", scenarios[sc], "_", location, "/", scenarios[sc], "_", location, ".lpj")
# definition of year couples
lenuses <- character()
lenuses <- paste0("lc_", scenarios[sc], "_", yir)


# Loop for couple====
for(lenuses_n in 1:(length(lenuses)-1)){
  
  landuse_1=lenuses[lenuses_n]
  landuse_2=lenuses[lenuses_n+1]
  
  time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
  
  #=Load active project
  # load(proj.file)
  
  # set driver connection
  # driver <- dbDriver('PostgreSQL')
  # project <- as.character(proj_descr[1,2])
  DB <- dbConnect(
    driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
    user=as.character(pgconf$user), password=as.character(pgconf$pass)
  )
  
  #=Retrieve all list of data that are going to be used
  # list_of_data_luc ==> list of data land use/cover 
  # list_of_data_pu ==> list of data planning unit
  # list_of_data_f ==> list of data factor
  # list_of_data_lut ==> list of data lookup table
  list_of_data_luc<-dbReadTable(DB, c("public", "list_of_data_luc"))
  list_of_data_pu<-dbReadTable(DB, c("public", "list_of_data_pu"))
  list_of_data_lut<-dbReadTable(DB, c("public", "list_of_data_lut"))
  # return the selected data from the list
  data_luc1<-list_of_data_luc[which(list_of_data_luc$RST_NAME==landuse_1),]
  data_luc2<-list_of_data_luc[which(list_of_data_luc$RST_NAME==landuse_2),]
  data_pu<-list_of_data_pu[which(list_of_data_pu$RST_NAME==planning_unit),]
  data_lut<-list_of_data_lut[which(list_of_data_lut$TBL_NAME==lukup_lc),]
  
  #=Set initial variables
  # time period
  T1<-data_luc1$PERIOD
  T2<-data_luc2$PERIOD
  
  #=Set working directory
  pu_name<-data_pu$RST_NAME
  idx_PreQUES<-idx_PreQUES+1
  preques_folder<-paste(idx_PreQUES, "_PreQUES_",T1,"_",T2,"_",pu_name,sep="")
  result_dir<-paste(dirname(proj.file),"/QUES/PreQUES/", preques_folder, sep="")
  dir.create(result_dir)
  
  # create temp directory
  dir.create(LUMENS_path_user, mode="0777")
  setwd(LUMENS_path_user)
  
  #=Set initial variables
  # reference map
  ref.obj<-exists('ref')
  ref.path<-paste(dirname(proj.file), '/ref.tif', sep='')
  if(!ref.obj){
    if(file.exists(ref.path)){
      ref<-raster(ref.path)
    } else {
      ref<-getRasterFromPG(pgconf, project, 'ref_map', 'ref.tif')
    }
  }
  # planning unit
  if (data_pu$RST_DATA=="ref") {
    zone<-ref
    count_ref<-as.data.frame(freq(ref))
    count_ref<-na.omit(count_ref)
    colnames(count_ref)<-c("IDADM", "COUNT")
    ref_table<-dbReadTable(DB, c("public", as.character(data_pu$LUT_NAME)))
    lookup_z<-merge(count_ref, ref_table, by="IDADM")
  } else {
    zone<-getRasterFromPG(pgconf, project, data_pu$RST_DATA, paste(data_pu$RST_DATA, '.tif', sep=''))
    lookup_z<-dbReadTable(DB, c("public", data_pu$LUT_NAME)) 
  }
  # landuse first time period
  landuse1<-getRasterFromPG(pgconf, project, data_luc1$RST_DATA, paste(data_luc1$RST_DATA, '.tif', sep=''))
  # landuse second time period
  landuse2<-getRasterFromPG(pgconf, project, data_luc2$RST_DATA, paste(data_luc2$RST_DATA, '.tif', sep=''))
  # landcover lookup table
  lut.lc<-dbReadTable(DB, c("public", data_lut$TBL_DATA)) 
  lut.lc<-lut.lc[,1:2]
  lookup_lc2<-lut.lc
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
  xtab<-tolower(paste('xtab_', pu_name, T1, T2, sep=''))
  data_xtab<-list_of_data_lut[which(list_of_data_lut$TBL_NAME==xtab),]
  if(nrow(data_xtab)==0){
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
    
    idx_lut<-idx_lut+1
    eval(parse(text=(paste("in_lut", idx_lut, " <- lu.db", sep=""))))
    
    eval(parse(text=(paste("list_of_data_lut<-data.frame(TBL_DATA='in_lut", idx_lut,"', TBL_NAME='", xtab, "', row.names=NULL)", sep=""))))
    # save to PostgreSQL
    InLUT_i <- paste('in_lut', idx_lut, sep="")
    dbWriteTable(DB, InLUT_i, eval(parse(text=(paste(InLUT_i, sep="" )))), append=TRUE, row.names=FALSE)
    dbWriteTable(DB, "list_of_data_lut", list_of_data_lut, append=TRUE, row.names=FALSE)
    
    setwd(result_dir)
    idx_factor<-idx_factor+1
    chg_map<-tolower(paste('chgmap_', pu_name, T1, T2, sep=''))
    eval(parse(text=(paste("writeRaster(R, filename='", chg_map, ".tif', format='GTiff', overwrite=TRUE)", sep=""))))
    eval(parse(text=(paste("factor", idx_factor, "<-'", chg_map, "'", sep='')))) 
    eval(parse(text=(paste("list_of_data_f<-data.frame(RST_DATA='factor", idx_factor,"', RST_NAME='", chg_map, "', row.names=NULL)", sep=""))))  
    InFactor_i <- paste("factor", idx_factor, sep="")  
    dbWriteTable(DB, "list_of_data_f", list_of_data_f, append=TRUE, row.names=FALSE)
    #write to csv
    list_of_data_f<-dbReadTable(DB, c("public", "list_of_data_f"))
    csv_file<-paste(dirname(proj.file),"/csv_factor_data.csv", sep="")
    write.table(list_of_data_f, csv_file, quote=FALSE, row.names=FALSE, sep=",")  
    addRasterToPG(project, paste0(chg_map, '.tif'), InFactor_i, srid)
    resave(idx_lut, idx_factor, file=proj.file)
  } else {
    lu.db<-dbReadTable(DB, c("public", data_xtab$TBL_DATA))
  }
  
  #=Create individual table for each landuse map
  # set area and classified land use/cover for first landcover and second
  freqLanduse_1<-dbReadTable(DB, c("public", data_luc1$LUT_NAME)) 
  area_lc1<-subset(freqLanduse_1, select=c('ID','COUNT', 'Classified'))
  colnames(area_lc1) = c("ID", "COUNT_LC1", "Classified1") 
  area_lc1<-merge(area_lc1,lookup_l,by="ID")
  colnames(area_lc1)[4] = "CLASS_LC1"
  
  freqLanduse_2<-dbReadTable(DB, c("public", data_luc2$LUT_NAME)) 
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
  #area<-min(sum(area_zone[,2]), sum(area_lc1[,2]), sum(area_lc2[,2]))
  data_merge_sel <- data_merge[ which(data_merge$COUNT > 0),]
  data_merge_sel$LU_CHG <- do.call(paste, c(data_merge_sel[c("LC_t1", "LC_t2")], sep = " to "))
  
  #=Create land use change map
  # eval(parse(text=(paste("write.dbf(data_merge,'lu.db_", pu_name ,"_", T1, "_", T2, ".dbf')", sep=""))))
  
  #=Calculate top ten largest land use changes 
  # create IDC, the difference between two ID_L, which is used to see the changes in land cover
  # then remove unchanged land cover, and finally save the top ten largest changes
  lg_chg <- data_merge_sel
  lg_chg$ID1<-as.numeric(as.character((lg_chg$ID_LC1)))
  lg_chg$ID2<-as.numeric(as.character((lg_chg$ID_LC2)))
  lg_chg$IDC<-lg_chg$ID1-lg_chg$ID2
  lg_chg<-lg_chg[ which(lg_chg$IDC!=0),]
  lg_chg <- as.data.frame(lg_chg[order(-lg_chg$COUNT),])
  lg_chg$CHG_CODE<-as.factor(toupper(abbreviate(lg_chg$LU_CHG, minlength=5, strict=FALSE, method="both")))
  lg_chg$ID1<-lg_chg$ID2<-lg_chg$IDC<-NULL
  # top ten changes
  lg_chg_top<-head(lg_chg, n=10)
  lg_chg_top$LC_t1<-lg_chg_top$LC_t2<-NULL
  # summary of landuse dominant change
  chg_only<-aggregate(COUNT~LU_CHG,data=lg_chg,FUN=sum)
  chg_only$CHG_CODE<-as.factor(toupper(abbreviate(chg_only$LU_CHG, minlength=5, strict=FALSE, method="both")))
  chg_only<-chg_only[order(-chg_only$COUNT),]
  chg_only<-chg_only[c(1,3,2)]
  # top ten dominant changes based on landuse/cover change
  chg_only_top<-head(chg_only, n=10)
  # summary of zonal dominant change
  lg_chg_zonal<-as.data.frame(NULL)
  for (l in 1:length(area_zone$ID)){
    tryCatch({
      a<-(area_zone$ID)[l]
      lg_chg_z<-lg_chg
      lg_chg_z<-as.data.frame(lg_chg_z[which(lg_chg_z$ZONE == a),])
      lg_chg_z<-aggregate(COUNT~ZONE+LU_CHG,data=lg_chg_z,FUN=sum)
      lg_chg_z$CHG_CODE<-as.factor(toupper(abbreviate(lg_chg_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
      lg_chg_z<-lg_chg_z[order(-lg_chg_z$COUNT),]
      lg_chg_z<-lg_chg_z[c(1,2,4,3)]
      #top ten dominant changes based on planning unit
      lg_chg_z_10<-head(lg_chg_z,n=10)
      lg_chg_zonal<-rbind(lg_chg_zonal,lg_chg_z_10)
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  # produce chart of largest source of changes in landuse
  colnames(chg_only_top)[3]<-"COUNT"
  Largest.chg<- ggplot(data=chg_only_top, aes(x=reorder(CHG_CODE, -COUNT),y=COUNT, fill=CHG_CODE))+geom_bar(stat='identity',position='dodge')+
    geom_text(data=chg_only_top, aes(x=CHG_CODE, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
    ggtitle(paste("10 Perubahan Tutupan Lahan Dominan di", location, T1,"-",T2 )) +
    labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)') + guides(fill=FALSE)+
    theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  #=Create overall summary of land use change
  lut.lc<-lut.lc[which(lut.lc$ID!=raster.nodata),]
  colnames(lut.lc)[1]<-"ID"
  colnames(lut.lc)[2]<-"LC"
  
  freq1<-subset(freqLanduse_1, select=c('ID','COUNT'))
  # freqLanduse_1<-dbReadTable(DB, c("public", data_luc1$LUT_NAME)) 
  lu.summary<-merge(lut.lc, freq1, by="ID")
  colnames(lu.summary)[3]<-paste(period1, "_ha", sep="")
  # get freqLanduse_[i] and merge to lu.summary
  for (q in 2:idx_landuse) {
    eval(parse(text=(paste("freqLanduse_", q, "<-dbReadTable(DB, c('public', list_of_data_luc$LUT_NAME[", q, "]))", sep=""))))
    eval(parse(text=(paste("freq", q, "<-subset(freqLanduse_", q, ", select=c('ID','COUNT'))", sep=""))))
    eval(parse(text=(paste("lu.summary<-merge(lu.summary, freq", q, ", by='ID')", sep=""))))
    eval(parse(text=(paste("colnames(lu.summary)[", q+2, "]<-paste(period", q, ", '_ha', sep='')", sep=""))))
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
  for(m in 2:idx_landuse){
    if (m!=idx_landuse) {
      eval(parse(text=(paste("Period<-period", m,"-period", m-1, sep=""))))
      sub<-eval(parse(text=(paste("period", m, sep=""))))
      sub<-paste(sub,"_ha", sep="")
      eval(parse(text=(paste("colnames(Ov_chg)[", m+2, ']="', sub, '"', sep=""))))
      c.name<-c(c.name,sub)
    } else {
      eval(parse(text=(paste("Period<-period", m,"-period", m-1, sep=""))))
    }
  }
  # calculate in rate per year and unit of hectare  
  Ov_chg.ha<-Ov_chg[1:2]
  Ov_chg.rate<-Ov_chg.ha
  count.period<-idx_landuse-1
  p.name<-NULL
  p.name2<-NULL
  for(o in 1:count.period){
    name<-paste((eval(parse(text=(paste("period",o,sep=""))))), "-", eval(parse(text=(paste("period",o+1,sep="")))),"_ha", sep="")
    name2<-paste((eval(parse(text=(paste("period",o,sep=""))))), "-", eval(parse(text=(paste("period",o+1,sep="")))),"_%/yrs", sep="")
    eval(parse(text=(paste("p.chg", "<-Ov_chg[", o, "+2+1]-Ov_chg[", o, "+2]", sep=""))))
    p.rate<-round(((p.chg/(Ov_chg[3]*Period))*100), 2)
    colnames(p.chg)[1]<-name
    colnames(p.rate)[1]<-name2
    Ov_chg.ha<-cbind(Ov_chg.ha,p.chg)
    Ov_chg.rate<-cbind(Ov_chg.rate,p.rate)
    p.name<-c(p.name,name)
    p.name2<-c(p.name2,name2)
  }
  # produce table and graph for overall change
  Ov_chg.melt <- melt(data = Ov_chg, id.vars=c('Land_use_type','LU_CODE'))
  Ov_chg.melt<-Ov_chg.melt[which(Ov_chg.melt$variable!="value"),]
  colnames(Ov_chg.melt)<-c("Land_use_type","LU_CODE", "Year", "Area")
  # overall changes plot 1
  ov.change.plot.2<-ggplot(Ov_chg.melt,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
    theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") + xlab("Land use type") +
    theme( legend.title = element_text(size=10),legend.text = element_text(size = 10), axis.text.x = element_text(size = 10),
           axis.title.x=element_blank())
  # produce table and graph for overall change in rate
  # NOTE: Ov_chg.merge<-cbind(Ov_chg.ha,(Ov_chg.rate[3:4]),(Ov_chg[6])) SOURCE OF ERROR STILL IN HARDCODE
  eval(parse(text=(paste("Ov_chg.merge<-cbind(Ov_chg.ha,(Ov_chg.rate[3:",2+count.period,"]),(Ov_chg[", 2+idx_landuse+1, "]))", sep=""))))
  # overall changes plot 2
  Ov_chg.melt2 <- melt(data = Ov_chg.merge, id.vars=c('Land_use_type','LU_CODE'), measure.vars=p.name)
  colnames(Ov_chg.melt2)<-c("Land_use_type","LU_CODE", "Year", "Area")
  ov.change.plot.3<-ggplot(Ov_chg.melt2,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
    theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") + xlab("Land use type") +
    theme( legend.title = element_text(size=8),legend.text = element_text(size = 8), axis.text.x = element_text(size = 10),
           axis.title.x=element_blank())+coord_flip()
  # overall changes plot 3
  Ov_chg.melt3 <- melt(data = Ov_chg.merge, id.vars=c('Land_use_type','LU_CODE'), measure.vars=p.name2)
  colnames(Ov_chg.melt3)<-c("Land_use_type","LU_CODE", "Year", "Area")
  ov.change.plot.4<-ggplot(Ov_chg.melt3,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
    theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") + xlab("Land use type") +
    theme( legend.title = element_text(size=8),legend.text = element_text(size = 8), axis.text.x = element_text(size = 10),
           axis.title.x=element_blank())+coord_flip()+ylim (c(-100, 100))
  # save all summary to database
  # write.dbf(Ov_chg, "Overall_change.dbf")
  # write.dbf(data_merge, "Changes_database.dbf")
  # write.dbf(Ov_chg.ha, "Overall_change_in_hectares.dbf")
  # write.dbf(Ov_chg.rate, "Overall_change_in_rates.dbf")
  
  #=Create Alpha-Beta analysis
  # function of alpha-beta
  alphabeta<-function(cross_temp_all, lookup_cover, t1, t2, area.analysis) {
    
    #select LU changes for creating blank diagonal values matrix
    cross_temp.blank_diag<-cross_temp_all
    cross_temp.blank_diag$check<-as.numeric(as.character(unlist((cross_temp.blank_diag$ID_LC2))))-as.numeric(as.character(unlist((cross_temp.blank_diag$ID_LC1))))
    #cross_temp.blank_diag<-filter(cross_temp.blank_diag, check!=0)
    cross_temp.blank_diag<-cross_temp.blank_diag[which(cross_temp.blank_diag$check!=0),]
    
    #create land use transition matrix
    cross_temp.blank_diag$ID_LC1<-as.numeric(as.character(unlist((cross_temp.blank_diag$ID_LC1))))
    colnames(lookup_cover)[1] = "ID_LC1"
    colnames(lookup_cover)[2] = "LC1"
    cross_temp.blank_diag<-merge(cross_temp.blank_diag,lookup_cover[],by='ID_LC1', all=T)
    
    cross_temp.blank_diag$ID_LC2<-as.numeric(as.character(unlist((cross_temp.blank_diag$ID_LC2))))
    colnames(lookup_cover)[1] = "ID_LC2"
    colnames(lookup_cover)[2] = "LC2"
    cross_temp.blank_diag<-merge(cross_temp.blank_diag,lookup_cover[],by='ID_LC2', all=T)
    
    cross_temp.melt <- melt(data = cross_temp.blank_diag, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))
    cross_temp.melt.cast <- dcast(data = cross_temp.melt, formula = ID_LC1 ~ ID_LC2, fun.aggregate = sum)
    cross_temp.melt.cast[1]<-NULL
    cross_temp.melt.cast<-cross_temp.melt.cast[-nrow(cross_temp.melt.cast),]; #remove last row
    cross_temp.melt.cast<-cross_temp.melt.cast[,-ncol(cross_temp.melt.cast)]; #remove last column
    
    a.table<-cross_temp.melt.cast
    tot.col<-colSums(cross_temp.melt.cast)
    
    
    for(x in 1:ncol(a.table)){
      eval(parse(text=(paste("a.table[", x, "]<-a.table[",x,"]/tot.col[",x,"]", sep=""))))
    }
    a.table[is.na(a.table)] <- 0
    a.table$"NA"<-NULL ;#remove NA column
    #a.table<-a.table[-nrow(a.table),];#remove last NA row
    a.table<-do.call(data.frame,lapply(a.table, function(x) replace(x, is.infinite(x),0)));#replace Inf
    
    a.val<-rowSums(a.table)
    
    #calculate beta
    b.table<-cross_temp.melt.cast
    tot.row<-rowSums(cross_temp.melt.cast)
    
    for(y in 1:ncol(b.table)){
      eval(parse(text=(paste("b.table[", y, ",]<-b.table[",y,",]/tot.row[",y,"]", sep=""))))
    }
    b.table[is.na(b.table)] <- 0
    b.table<-do.call(data.frame,lapply(b.table, function(x) replace(x, is.infinite(x),0)));#replace Inf
    b.table$"NA"<-NULL
    #b.table <- b.table[-nrow(b.table),];#remove last NA row
    b.val<-colSums(b.table)
    
    
    #bind alpha and beta
    eval(parse(text=( paste("id<-na.omit(as.integer(colnames(cross_temp.melt.cast)))",sep=''))))
    fromto.table<-cbind(na.omit(id),a.val,b.val)
    
    colnames(lookup_cover)[1] = "ID"
    colnames(lookup_cover)[2] = "LC"
    colnames(fromto.table)<-c("ID","a.val","b.val")
    fromto.table<-merge(lookup_cover,fromto.table, by='ID', all=T); #<- ALPHA beta TABLE
    
    #============================
    #LULC summary
    
    area.lu1<-ddply(cross_temp_all, c("ID_LC1"), summarise,LU1=sum(COUNT))
    colnames(area.lu1)[1]<-"ID"
    area.lu2<-ddply(cross_temp_all, c("ID_LC2"), summarise,LU2=sum(COUNT))
    colnames(area.lu2)[1]<-"ID"
    LULC_summary<-merge(area.lu1, area.lu2, by="ID", ALL=T)
    #LULC_summary$ID<-as.character(LULC_summary$ID)
    #LULC_summary[is.na(LULC_summary)] <- 0
    LULC_summary <- LULC_summary[-nrow(LULC_summary),]
    
    
    LULC_summary$Change<-as.numeric(LULC_summary$LU2)-as.numeric(LULC_summary$LU1)
    LULC_summary$Total_change<-abs(LULC_summary$Change)
    fromto.table_LULC<-na.omit(merge(fromto.table, LULC_summary, by='ID', all=T))
    value<-data.frame(stringsAsFactors=FALSE)
    
    #negative change or positive change
    for(d in 1:nrow(fromto.table_LULC)){
      if(fromto.table_LULC[d,7]>0){
        nval<-1
        value<-rbind(value,nval)
      } else {
        nval<-0
        value<-rbind(value,nval)
      }}
    
    value[value==0]<-"decreasing"
    value[value==1]<-"increasing"
    colnames(value)<-"Trend"
    fromto.table_LULC<-cbind(fromto.table_LULC,value)
    fromto.table_LULC<-arrange(fromto.table_LULC, -Total_change)
    
    #rate of Change
    fromto.table_LULC$rate<-(abs(fromto.table_LULC$Total_change/fromto.table_LULC$LU1)*100)
    fromto.table_LULC$rate[is.na(fromto.table_LULC$rate)] <- 0
    
    fromto.table_LULC$area<-area.analysis
    fromto.table_LULC$lc_abr<-toupper(abbreviate(fromto.table_LULC$LC))
    
    #print(fromto.table_LULC)
    return(fromto.table_LULC)
  }
  # function of alpha-beta graph
  alphabeta.plot<-function(alphabeta_table, t1, t2, area.analysis){
    #ignore transition less than 1
    #alphabeta_table[alphabeta_table$a.val<0.5&alphabeta_table$b.val<0.5, 1:10]<-NA 
    alphabeta_table<-alphabeta_table[-which(alphabeta_table$a.val < 0.5 & alphabeta_table$b.val < 0.5),]
    if(nrow(alphabeta_table)!=0){
      ab.plot<-ggplot(alphabeta_table, aes(x=a.val, y=b.val,xend=6, yend=6)) +
        geom_point(data=alphabeta_table,aes(x=a.val, y=b.val,size =Total_change, colour =Trend), alpha=.5)+
        geom_text(size=3, aes(label=lc_abr),hjust=0.5,vjust=-1, angle =0)+
        scale_size(range = c(1,50)) + labs(x = paste("Alpha", t1), y= paste("Beta", t2))+ ggtitle(paste(area.analysis, t1, '-', t2))
      return(ab.plot)
      #+theme_bw()
    }
  }
  
  if(analysis.option==2 | analysis.option==0){
    # alpha-beta at landscape level
    landscape.alphabeta<-alphabeta(data_merge, lookup_lc, T1, T2, paste("Keseluruhan", location))
    landscape.alphabeta.plot<-alphabeta.plot(landscape.alphabeta, T1, T2,paste("Keseluruhan", location))
    
    # alpha-beta at planning unit level
    alpha_beta_database<-data.frame()
    for(i in 1:nrow(lookup_z)){
      if (i==1){
        tryCatch({
          zone_id<-lookup_z$ID[i]
          #print(zone_id)
          eval(parse(text=( paste("cross_temp_zone<-na.omit(data_merge[ which(data_merge$ZONE==",zone_id,"),])", sep=''))))
          zona<-paste(lookup_z$ZONE[i])
          eval(parse(text=( paste('alphabeta_zone_', zone_id,"<-alphabeta(cross_temp_zone, lookup_l, T1,T2, zona)", sep=''))))
          eval(parse(text=( paste('alphabeta_plot_zone_', zone_id,"<-alphabeta.plot(alphabeta_zone_", zone_id,", T1, T2, zona)", sep=''))))
          #plot.name<-paste("alpha_beta_",location,"_",T1,"_",T2,"_zona_", zona,".png", sep='')
          #eval(parse(text=( paste('ggsave(alphabeta_plot_zone_', zone_id,",file= plot.name,width=20,height=20, units='cm')", sep=''))))
          alpha_beta_database<-rbind(landscape.alphabeta,alphabeta_zone_1)
        }, error=function(e){cat("No Alpha-beta analysis", "\n")})
      } else {
        tryCatch({
          zone_id<-lookup_z$ID[i]
          #print(zone_id)
          eval(parse(text=( paste("cross_temp_zone<-na.omit(data_merge[ which(data_merge$ZONE==",zone_id,"),])", sep=''))))
          zona<-paste(lookup_z$ZONE[i])
          eval(parse(text=( paste('alphabeta_zone_', zone_id,"<-alphabeta(cross_temp_zone, lookup_l, T1,T2, zona)", sep=''))))
          eval(parse(text=( paste('alphabeta_plot_zone_', zone_id,"<-alphabeta.plot(alphabeta_zone_", zone_id,", T1, T2, zona)", sep=''))))
          eval(parse(text=( paste("alpha_beta_database<-rbind(alpha_beta_database, alphabeta_zone_", zone_id,")", sep=''))))
          #eval(parse(text=( paste('alphabeta_plot_zone_', zone_id, sep=""))))
        },error=function(e){cat("Nice try pal! ~ please re-check your input data :",conditionMessage(e), "\n")})
      }
    }
    
    # write.dbf(alpha_beta_database, "Pre_QUES_IO_table.dbf")
  } else { print("Alpha-beta analysis is skipped") }
  
  #=Create Pre-QUES Change Report (.lpr)
  # create maps for report
  # arrange numerous colors with RColorBrewer
  myColors1 <- brewer.pal(9,"Set1")
  myColors2 <- brewer.pal(8,"Accent")
  myColors3 <- brewer.pal(12,"Paired")
  myColors4 <- brewer.pal(9, "Pastel1")
  myColors5 <- brewer.pal(8, "Set2")
  myColors6 <- brewer.pal(8, "Dark2")
  myColors7 <- rev(brewer.pal(11, "RdYlGn"))
  myColors8 <- "#000000"
  myColors9 <- brewer.pal(12, "Set3")
  if (0 %in% lu.db$ZONE){
    myColors  <-c(myColors8, myColors5,myColors1, myColors2, myColors7, myColors4, myColors5, myColors6)
  } else {
    myColors  <-c(myColors5,myColors1, myColors2, myColors3, myColors4, myColors7, myColors6)
  }
  # zone map
  myColors.Z <- myColors[1:length(unique(lu.db$ZONE))]
  ColScale.Z<-scale_fill_manual(name="Unit perencanaan", breaks=area_zone$ID, labels=area_zone$ZONE, values=myColors.Z)
  plot.Z<-gplot(zone, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
    coord_equal() + ColScale.Z +
    theme(plot.title = element_text(lineheight= 5, face="bold")) +
    theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
           panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
           legend.title = element_text(size=8),
           legend.text = element_text(size=6),
           legend.key.height = unit(0.25, "cm"),
           legend.key.width = unit(0.25, "cm"))
  #rm(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6, myColors9)
  
  if (0 %in% lu.db$ID_LC1){
    myColors  <-c(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
  } else {
    myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
  }
  myColors.lu <- myColors[1:length(unique(lu.db$ID_LC1))]
  # land use/cover map first period
  area_lcmap<-subset(area_lc1, select=c("ID", "COUNT_LC1"))
  colnames(lookup_l)[1]<-"ID"
  area_lcmap<-merge(area_lcmap,lookup_l,by="ID")
  colnames(area_lcmap)[3] = "CLASS_LC1"
  ColScale.lu<-scale_fill_manual(name="Jenis tutupan lahan", breaks=area_lcmap$ID, labels=area_lcmap$CLASS_LC1, values=myColors.lu)
  plot.LU1<-gplot(landuse1, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
    coord_equal() + ColScale.lu +
    theme(plot.title = element_text(lineheight= 5, face="bold")) +
    theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
           panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
           legend.title = element_text(size=8),
           legend.text = element_text(size = 6),
           legend.key.height = unit(0.25, "cm"),
           legend.key.width = unit(0.25, "cm"))
  # land use/cover map next period
  area_lcmap<-subset(area_lc2, select=c("ID", "COUNT_LC2"))
  colnames(lookup_l)[1]<-"ID"
  area_lcmap<-merge(area_lcmap,lookup_l,by="ID")
  colnames(area_lcmap)[3] = "CLASS_LC2"
  ColScale.lu<-scale_fill_manual(name="Jenis tutupan lahan", breaks=area_lcmap$ID, labels=area_lcmap$CLASS_LC2, values=myColors.lu)
  plot.LU2<-gplot(landuse2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
    coord_equal() + ColScale.lu +
    theme(plot.title = element_text(lineheight= 5, face="bold")) +
    theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
           panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
           legend.title = element_text(size=8),
           legend.text = element_text(size = 6),
           legend.key.height = unit(0.25, "cm"),
           legend.key.width = unit(0.25, "cm"))
  
  setwd(result_dir)
  # write report Pre-QUES change
  title1<-"{\\colortbl;\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red146\\green208\\blue80;\\red0\\green176\\blue240;\\red140\\green175\\blue71;\\red0\\green112\\blue192;\\red79\\green98\\blue40;} \\pard\\qr\\b\\fs70\\cf2 L\\cf3U\\cf4M\\cf5E\\cf6N\\cf7S \\cf1REPORT \\par\\b0\\fs20\\ql\\cf1"
  title2<-paste("\\pard\\qr\\b\\fs40\\cf1 PreQUES-Land Use Change Analysis ", "for ", location, ", ", province, ", ", country, "\\par\\b0\\fs20\\ql\\cf1", sep="")
  sub_title<-"\\cf2\\b\\fs32 ANALISA PERUBAHAN PENGGUNAAN LAHAN\\cf1\\b0\\fs20"
  #date<-paste("Date : ", date, sep="")
  time_start<-paste("Proses dimulai : ", time_start, sep="")
  time_end<-paste("Proses selesai : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
  line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
  area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
  I_O_period_1_rep<-paste("\\b","\\fs20", T1)
  I_O_period_2_rep<-paste("\\b","\\fs20", T2)
  chapter1<-"\\cf2\\b\\fs28 DATA YANG DIGUNAKAN \\cf1\\b0\\fs20"
  chapter2<-"\\cf2\\b\\fs28 HASIL ANALISA PADA TINGKAT BENTANG LAHAN \\cf1\\b0\\fs20"
  chapter3<-"\\cf2\\b\\fs28 HASIL ANALISA PADA TINGKAT UNIT PERENCANAAN \\cf1\\b0\\fs20"
  rtffile <- RTF("Pre-QUES_change_report.doc", font.size=11, width = 8.267, height = 11.692, omi = c(0,0,0,0))
  file.copy(paste0(LUMENS_path, "/ques_cover.png"), result_dir, recursive = FALSE)
  file.copy(paste0(LUMENS_path, "/ques_line.png"), result_dir, recursive = FALSE)
  img_location<-paste0(result_dir, "/ques_cover.png")
  line_location<-paste0(result_dir, "/ques_line.png")
  
  # loading the .png image to be edited
  cover <- image_read(img_location)
  # to display, only requires to execute the variable name, e.g.: "> cover"
  # adding text at the desired location
  text_submodule <- paste("Sub-Modul Perubahan Penggunaan Lahan\n\nAnalisis Perubahan Penggunaan Lahan\n", location, ", ", "Periode ", T1, "-", T2, sep="")
  cover_image <- image_annotate(cover, text_submodule, size = 23, gravity = "southwest", color = "white", location = "+46+220", font = "Arial")
  cover_image <- image_write(cover_image)
  # 'gravity' defines the 'baseline' anchor of annotation. "southwest" defines the text shoul be anchored on bottom left of the image
  # 'location' defines the relative location of the text to the anchor defined in 'gravity'
  # configure font type
  addPng(rtffile, cover_image, width = 8.267, height = 11.692)
  addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1,1,1,1))
  
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addNewLine(rtffile)
  addParagraph(rtffile, title1)
  addParagraph(rtffile, title2)
  addNewLine(rtffile)
  #addPng(rtffile, line_location, width = 8.267, height = 0.1)
  addParagraph(rtffile, time_start)
  addParagraph(rtffile, time_end)
  #addPng(rtffile, line_location, width = 8.267, height = 0.1)
  addNewLine(rtffile)
  width<-as.vector(c(1.34,3.1))
  addTable(rtffile,proj_descr,font.size=8,col.widths=width)
  addPageBreak(rtffile)
  addParagraph(rtffile, sub_title)
  addNewLine(rtffile)
  #addPng(rtffile, line_location, width = 8.267, height = 0.1)
  #addParagraph(rtffile, date)
  addParagraph(rtffile, time_start)
  addParagraph(rtffile, time_end)
  #addPng(rtffile, line_location, width = 8.267, height = 0.1)
  addNewLine(rtffile)
  addParagraph(rtffile, "Analisa perubahan tutupan lahan dilakukan untuk mengetahui kecenderungan perubahan tutupan lahan di suatu daerah pada satu kurun waktu. Analisa ini dilakukan dengan menggunakan data peta tutupan lahan pada dua periode waktu yang berbeda. Selain itu, dengan memasukkan data unit perencanaan kedalam proses analisa, dapat diketahui kecenderungan perubahan tutupan lahan pada masing-masing kelas unit perencanaan yang ada. Informasi yang dihasilkan melalui analisa ini dapat digunakan dalam proses perencanaan untuk berbagai hal. Diantaranya adalah: menentukan prioritas pembangunan, mengetahui faktor pemicu perubahan penggunaan lahan, merencanakan skenario pembangunan di masa yang akan datang, dan lain sebagainya.")
  addNewLine(rtffile)
  addParagraph(rtffile, chapter1)
  #addPng(rtffile, line_location, width = 8.267, height = 0.1)
  addNewLine(rtffile)
  addParagraph(rtffile, "Data yang digunakan dalam analisa ini adalah data peta penggunaan lahan dan data peta unit perencanaan daerah. Data pendukung yang digunakan adalah peta acuan tipe penggunaan lahan dan data acuan kelas unit perencanaan")
  addNewLine(rtffile)
  #addTable(rtffile,test3,font.size=8)
  #addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Peta Tutupan Lahan ", location, " tahun ", T1," \\b0 \\fs20", sep=""))
  addNewLine(rtffile, n=1)
  tryCatch({
    addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, plot.LU1 )
    addNewLine(rtffile, n=1)
  },error=function(e){cat("Nice try pal! ~ please re-check your input data :",conditionMessage(e), "\n"); addParagraph(rtffile, "no data");addNewLine(rtffile)})
  addParagraph(rtffile, paste("\\b \\fs20 Peta Tutupan Lahan ", location, " tahun ", T2," \\b0 \\fs20", sep=""))
  addNewLine(rtffile, n=1)
  tryCatch({
    addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, plot.LU2 )
    addNewLine(rtffile, n=1)
  },error=function(e){cat("Nice try pal! ~ please re-check your input data :",conditionMessage(e), "\n"); addParagraph(rtffile, "no data");addNewLine(rtffile)})
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Peta Unit Perencanaan "," \\b0 \\fs20", sep=""))                                                                                                                                                                                                                                                                                                                                                                                                                           
  addNewLine(rtffile)
  addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150, plot.Z)
  addNewLine(rtffile)
  addParagraph(rtffile, chapter2)
  #addPng(rtffile, line_location, width = 18.267, height = 0.3)
  addNewLine(rtffile)
  addParagraph(rtffile, "Pada bagian ini disajikan hasil analisa perubahan penggunaan lahan untuk keseluruhan bentang lahan yang dianalisa. Beberapa bentuk analisa yang dilakukan antara lain: perbandingan luasan tutupan lahan pada periode analisa dan tipe perubahan lahan dominan pada bentang lahan yang dianalisa")
  addNewLine(rtffile)
  addParagraph(rtffile, "Tabel Intisari Perubahan Tutupan Lahan menyajikan perbandingan luasan tipe-tipe tutupan lahan di sebuah bentnag lahan pada kurun waktu tertentu. Kolom Overall Change menunjukkan perubahan luasan dalam satuan hektar. Notasi negatif pada kolom ini menunjukkan pengurangan luasan sebaliknya notasi positif menunjukkan penambahan luasan. Kolom Rate menunjukkan laju perubahan luasan tutupan lahan dalam satuan %/tahun. Kolom ini dihitung dengan mengurangi luasan pada t2-t1 kemudian dibagi dengan perkalian luasan pada t1 dan kurun waktu analisa")
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Intisari Perubahan Tutupan Lahan di", location, "\\b \\fs20", sep=" "))
  #width<-as.vector(c(0.44,2,0.69,0.69,0.69,0.69))
  addTable(rtffile,Ov_chg,font.size=8)
  addNewLine(rtffile)
  addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=150,  ov.change.plot.2)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Luasan Perubahan Tutupan Lahan (ha) di", location, "\\b \\fs20", sep=" "))
  #width<-as.vector(c(0.44,2,0.69,0.69))
  addTable(rtffile,Ov_chg.ha,font.size=8)
  addNewLine(rtffile)
  addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=150,  ov.change.plot.3)
  
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Rerata Luasan Perubahan Tutupan Lahan (%/tahun) di", location, "\\b \\fs20", sep=" "))
  #width<-as.vector(c(0.44,2,1,1))
  addTable(rtffile,Ov_chg.rate,font.size=8)
  addNewLine(rtffile)
  addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=150,  ov.change.plot.4)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Sepuluh Perubahan Lahan Dominan di",location, "\\b0 \\fs20", sep=" "))
  addNewLine(rtffile, n=1)
  colnames(chg_only_top)[3]<-"Luas(ha)"
  addTable(rtffile, chg_only_top)
  addNewLine(rtffile, n=1)
  addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150,Largest.chg)
  addNewLine(rtffile)
  addNewLine(rtffile)
  # land use dominant changes report
  if(analysis.option==1 | analysis.option==0){
    addParagraph(rtffile, chapter3)
    #addPng(rtffile, line_location, width = 8.267, height = 0.1)
    addNewLine(rtffile)
    addParagraph(rtffile, "Pada bagian ini disajikan hasil analisa perubahan penggunaan lahan untuk masing-masing kelas unit perencanaan yang dianalisa. Beberapa bentuk analisa yang dilakukan antara lain: perbandingan luasan tutupan lahan pada periode analisa dan tipe perubahan lahan dominan pada unit perencanaan yang dianalisa")
    addNewLine(rtffile)
    for(i in 1:length(lookup_z$ID)){
      tryCatch({
        zonal.db<-lookup_z
        zonal.db$Z_CODE<-toupper(abbreviate(zonal.db$ZONE))
        zona<-paste("\\b", "\\fs20", i, "\\b0","\\fs20")
        zona_nm<-paste("\\b", "\\fs20", zonal.db$ZONE[i], "\\b0","\\fs20")
        zona_ab<-paste("\\b", "\\fs20", zonal.db$Z_CODE[i], "\\b0","\\fs20")
        addParagraph(rtffile, "\\b \\fs20 Sepuluh Tipe Perubahan Tutupan Lahan Dominan di Unit Perencanaan \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm, "\\b \\fs20 (\\b0 \\fs20", zona_ab, "\\b \\fs20)\\b0 \\fs20" )
        addNewLine(rtffile, n=1)
        lg_chg_zon<-lg_chg_zonal[which(lg_chg_zonal$ZONE == i),]
        lg_chg_zon$ZONE<-NULL
        lg_chg_plot<-lg_chg_zon
        colnames(lg_chg_zon)[3]<-"Luas (ha)"
        addTable(rtffile, lg_chg_zon)
        addNewLine(rtffile, n=1)
        # largest source of change in planning unit level
        Largest.chg.z<- ggplot(data=lg_chg_plot, aes(x=reorder(CHG_CODE, -COUNT),y=COUNT, fill=CHG_CODE))+geom_bar(stat='identity',position='dodge')+
          geom_text(data=lg_chg_plot, aes(x=CHG_CODE, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
          ggtitle(paste("10 Perubahan Dominan pada Unit Perencanaan",i, "-", zonal.db$Z_CODE[i] )) + guides(fill=FALSE) +
          labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)') + guides(fill=FALSE)+
          theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
          theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
                panel.grid.major=element_blank(), panel.grid.minor=element_blank())
        
        addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, Largest.chg.z )
        addNewLine(rtffile, n=1)
      },error=function(e){cat("Nice try pal! ~ please re-check your input data :",conditionMessage(e), "\n"); addParagraph(rtffile, "no data");addNewLine(rtffile)})
    }
  }
  # alpha-beta analysis report
  if(analysis.option==2 | analysis.option==0){
    addParagraph(rtffile, paste("\\b \\fs20 Grafik IO: dinamika perubahan lahan di\\b0 \\fs20 ", area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20", I_O_period_2_rep, sep=" "))
    addPlot(rtffile,plot.fun=print, width=6.7,height=5,res=150,  landscape.alphabeta.plot)
    addNewLine(rtffile)
    addNewLine(rtffile)
    
    for(i in 1:nrow(lookup_z)){
      checkAlphabetaPlot<-eval(parse(text=( paste("exists('alphabeta_plot_zone_", i, "')" , sep=""))))
      if(checkAlphabetaPlot){
        tryCatch({
          eval(parse(text=( paste("checkNULL<-is.null(alphabeta_plot_zone_", i, ")", sep=""))))
          if(!checkNULL){
            zona<-paste("\\b", "\\fs20", i, "\\b0","\\fs20")
            zona_nm<-paste("\\b", "\\fs20", lookup_z$ZONE[i], "\\b0","\\fs20")
            addParagraph(rtffile, "\\b \\fs20 Grafik IO: dinamika perubahan lahan di \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm )
            
            addNewLine(rtffile)
            eval(parse(text=( paste("addPlot(rtffile,plot.fun=print, width=6.7,height=5,res=150, alphabeta_plot_zone_",i,")", sep=''))))
            
            addNewLine(rtffile)
          }
        },error=function(e){cat("please re-check your input data :",conditionMessage(e), "\n"); addParagraph(rtffile, "no data");addNewLine(rtffile)})
      }
    }
  }
  addNewLine(rtffile)
  done(rtffile)
  
  #=Land use change database export
  #eval(parse(text=(paste("PreQUES_data_", data[1,2], "_", data[2,2], "<-data", sep=""   ))))
  #newPre<-paste("PreQUES_data_", data[1,2], "_", data[2,2], sep="")
  
  #command<-paste("resave(PreQUES.index,Ov_chg,Ov_chg.ha,lut.lc,Ov_chg.rate,", newPre, ",",  sep="")
  #eval(parse(text=(paste("rtffilePreQUES_", check_record, " <- rtffile", sep=""))))
  # eval(parse(text=(paste("PreQUES_database_", pu_name, "_", T1, "_", T2, " <- data_merge", sep=""))))
  #command<-paste("resave(rtffilePreQUES_", check_record, ",PreQUES_database_", pu_name, "_", data[1,2], "_", data[2,2], ",run_record,PreQUES.index,lut.lc,",  sep="")
  #command<-paste(command,"file='",proj.file,"')", sep="")
  
  # eval(parse(text=(paste("resave(PreQUES_database_", pu_name, "_", T1, "_", T2, ", idx_PreQUES, file='", proj.file, "')", sep=""))))
  eval(parse(text=(paste('rtf_PreQUES_', T1, '_', T2, '_', pu_name, '<-rtffile', sep=''))))
  eval(parse(text=(paste('resave(rtf_PreQUES_', T1, '_', T2, '_', pu_name, ', file=proj.file)', sep=''))))
  resave(idx_PreQUES, file=proj.file)
  
  #=Pre-QUES Land Use Change Trajectories
  if(analysis.option==3 | analysis.option==0){
    #create run record
    #traj_record <- data.frame(check_record, T1, T2, pu_selected, "Traj")
    #colnames(traj_record)[1] <- "rec"
    #colnames(traj_record)[2] <- "T1"
    #colnames(traj_record)[3] <- "T2"
    #colnames(traj_record)[4] <- "pu_selected"
    #colnames(traj_record)[5] <- "modul"
    #run_record <- rbind(run_record, traj_record)
    
    # substitute lookup table internal
    trj<-c(11:17,22:27,32:37,42:44,46:47,52:57,62:67,77,88)
    lookup_traj<-as.data.frame(trj)
    remove(trj)
    lookup_traj$Traj<-c("Stable natural forest","Loss to logged-over forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Loss to logged-over forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Loss to infrastructure","Other")
    lookup_traj$Def<-c("Stable forest", "Forest degradation", "Deforestation","Deforestation","Deforestation","Deforestation","Deforestation","Stable forest", "Deforestation","Deforestation","Deforestation","Deforestation","Deforestation", "Reforestation", "Other","Other","Other","Other","Other", "Other","Other","Other","Other","Other","Other","Other","Other","Other","Other","Other", "Other","Other","Other","Other","Other","Other","Other", "Others")
    name_traj<-lookup_traj
    name_traj$ID_trf<-c(5,3,7,6,1,4,2,3,7,6,1,4,2,8,7,6,1,4,2,8,7,6,4,2,8,7,6,1,4,2,8,7,6,1,4,2,2,9)
    ID_T<-c(1:9)
    leg_traj<-as.data.frame(ID_T)
    remove(ID_T)
    leg_traj$Trajectories<-c("Loss to cropland","Loss to infrastructure","Loss to logged-over forest","Loss to bare land and abandoned","Stable natural forest","Recovery to agroforest","Recovery to tree cropping","Recovery to forest","Other")
    
    lu_class<-c(1,2,3,4,5,6,7,8)
    lu_class<-as.data.frame(lu_class)
    lu_class$Classified<-c("Undisturbed forest", "Logged-over forest", "Monoculture tree-based plantation", "Mixed tree-based plantation", "Agriculture/annual crop", "Shrub, grass, and cleared land", "Settlement and built-up area", "Others")
    
    # create trajectories database
    freq1<-subset(area_lc1, select=c('ID', 'CLASS_LC1', 'Classified1'))
    freq2<-subset(area_lc2, select=c('ID', 'CLASS_LC2', 'Classified2'))
    colnames(freq1)[3]<-"Classified"
    colnames(freq2)[3]<-"Classified"
    freq1<-merge(freq1, lu_class, by="Classified", all = TRUE)
    freq2<-merge(freq2, lu_class, by="Classified", all = TRUE)
    freq1<-subset(freq1, select=c('ID', 'CLASS_LC1', 'lu_class'))
    freq2<-subset(freq2, select=c('ID', 'CLASS_LC2', 'lu_class'))
    colnames(freq1)[1]="ID_LC1"
    colnames(freq1)[2]="CLASS1"
    colnames(freq1)[3]="ID_L1"
    data_merge_tr<-as.data.frame(merge(data_merge, freq1, by="ID_LC1", all = TRUE))
    colnames(freq2)[1]="ID_LC2"
    colnames(freq2)[2]="CLASS2"
    colnames(freq2)[3]="ID_L2"
    data_merge_tr<-as.data.frame(merge(data_merge_tr, freq2, by="ID_LC2", all = TRUE))
    data_merge_tr$CLASS1<-data_merge_tr$CLASS2<-NULL
    data_merge_tr$T1<-data_merge_tr$ID_L1*10
    data_merge_tr$T2<-data_merge_tr$ID_L2
    data_merge_tr$TR<-data_merge_tr$T1+data_merge_tr$T2
    colnames(lookup_traj)[1]="TR"
    PreQUES_traj_database<-as.data.frame(merge(data_merge_tr,lookup_traj, by="TR", all = TRUE))
    PreQUES_traj_database$Traj_Code<-toupper(abbreviate(PreQUES_traj_database$Traj))
    
    #cross_temp<-data.frame(Var1=PreQUES_traj_database$ID_LC1, Var2=PreQUES_traj_database$ID_LC2, Var3=PreQUES_traj_database$ZONE, Freq=PreQUES_traj_database$COUNT)
    
    # Land cover change trajectories index
    #Calculate deforestation, degradation, and reforestation rate
    #PREQUES_filter_2<-function(idlc1, idlc2, filter1, filter2) {
    #  eval(parse(text=( paste("PreQUES_traj_database.filtered<-filter(PreQUES_traj_database,ID_L1",filter1,as.characteridlc1,',','ID_L2',filter2,idlc2,')', sep=''))))
    #  #PreQUES_traj_database.filtered<-filter(PreQUES_traj_database,ID_L1 == idlc1, ID_L2 == idlc2)
    #  PreQUES_traj_database.filtered<-filter(PreQUES_traj_database.filtered, COUNT!=0)
    #  PreQUES_traj_database.filtered <- aggregate(COUNT ~  Z_NAME, data=PreQUES_traj_database.filtered, FUN=sum)
    #  return(PreQUES_traj_database.filtered)
    #}
    
    #1. Deforestation
    #index.deforest<-PREQUES_filter_2(c(1,2),c(1,2),'==','!=')
    tryCatch({
      #index.deforest<-filter(PreQUES_traj_database, ID_L1==c(1,2), ID_L2!=c(1,2)) 
      index.deforest1<-subset(PreQUES_traj_database, ID_L1==1)
      index.deforest2<-subset(PreQUES_traj_database, ID_L1==2)
      index.deforest<-rbind(index.deforest1, index.deforest2)
      index.deforest<-subset(index.deforest, ID_L2!=1)
      index.deforest<-subset(index.deforest, ID_L2!=2)
      index.deforest<-subset(index.deforest, COUNT!=0)
      index.deforest <- aggregate(COUNT ~  Z_NAME, data=index.deforest, FUN=sum)
      colnames(index.deforest)<-c('ZONE', 'Deforestasi')
      total.deforest<-data.frame(ZONE="TOTAL",Deforestasi=sum(index.deforest[,2]))
      index.deforest<-rbind(index.deforest,total.deforest)
    },error=function(e){cat("No deforestation found", "\n")})
    
    #2. Forest degradation
    tryCatch({
      #index.forest.degrad<-filter(PreQUES_traj_database,ID_L1==1,ID_L2==c(2))
      index.forest.degrad<-subset(PreQUES_traj_database, ID_L1==1)
      index.forest.degrad<-subset(index.forest.degrad, ID_L2==2)
      index.forest.degrad<-subset(index.forest.degrad, COUNT!=0)
      index.forest.degrad <- aggregate(COUNT ~  Z_NAME, data=index.forest.degrad, FUN=sum)
      colnames(index.forest.degrad)<-c('ZONE', 'Degradasi_Hutan')
      total.forest.degrad<-data.frame(ZONE="TOTAL",Degradasi_Hutan=sum(index.forest.degrad[,2]))
      index.forest.degrad<-rbind(index.forest.degrad,total.forest.degrad)
    },error=function(e){cat("No degradation found", "\n")})
    
    #3. Reforestation
    tryCatch({
      #index.reforest<-filter(PreQUES_traj_database,ID_L1==c(3,4,5,6,7,8),ID_L2==c(1,2))
      index.reforest<-subset(PreQUES_traj_database,ID_L1!=1)
      index.reforest<-subset(index.reforest,ID_L1!=2)
      index.reforest1<-subset(index.reforest,ID_L2==1)
      index.reforest2<-subset(index.reforest,ID_L2==2)
      index.reforest<-rbind(index.reforest1, index.reforest2)
      index.reforest<-subset(index.reforest, COUNT!=0)
      index.reforest <- aggregate(COUNT ~  Z_NAME, data=index.reforest, FUN=sum)
      colnames(index.reforest)<-c('ZONE', 'Reforestasi')
      total.reforest<-data.frame(ZONE="TOTAL",Reforestasi=sum(index.reforest[,2]))
      index.reforest<-rbind(index.reforest,total.reforest)
    },error=function(e){cat("No reforestation found", "\n")})
    
    #4. Stable forest
    #index.stable.forest<-PREQUES_filter_2(1,1,'==','==')
    #colnames(index.stable.forest)<-c('ZONE', 'Tetap_Hutan')
    #total.stable.forest<-data.frame(ZONE="TOTAL",Tetap_Hutan=sum(index.stable.forest[,2]))
    #index.stable.forest<-rbind(index.stable.forest,total.stable.forest)
    
    #5. Initial forest cover
    tryCatch({
      index.init.forest<-subset(PreQUES_traj_database,ID_L1==1)
      index.init.forest<-subset(index.init.forest, COUNT!=0)
      index.init.forest <- aggregate(COUNT ~  Z_NAME, data=index.init.forest, FUN=sum)
      colnames(index.init.forest)<-c('ZONE', 'Forest_T1')
      total.init.forest<-data.frame(ZONE="TOTAL",Forest_T1=sum(index.init.forest[,2]))
      index.init.forest<-rbind(index.init.forest,total.init.forest)
    },error=function(e){cat("No initial forest cover found", "\n")})
    
    #6. Total forest cover
    tryCatch({
      #index.init.forestandlogged<-filter(PreQUES_traj_database,ID_L1==c(1,2))
      index.init.forestandlogged1<-subset(PreQUES_traj_database,ID_L1==1)
      index.init.forestandlogged2<-subset(PreQUES_traj_database,ID_L1==2)
      index.init.forestandlogged<-rbind(index.init.forestandlogged1, index.init.forestandlogged2)
      index.init.forestandlogged<-subset(index.init.forestandlogged, COUNT!=0)
      index.init.forestandlogged <- aggregate(COUNT ~  Z_NAME, data=index.init.forestandlogged, FUN=sum)
      colnames(index.init.forestandlogged)<-c('ZONE', 'Forest_T1')
      total.init.forestandlogged<-data.frame(ZONE="TOTAL",Forest_T1=sum(index.init.forestandlogged[,2]))
      index.init.forestandlogged<-rbind(index.init.forestandlogged,total.init.forestandlogged)
    },error=function(e){cat("No total forest cover found", "\n")})
    
    #7. Initial non-forest
    tryCatch({
      #index.init.nonforest<-filter(PreQUES_traj_database,ID_L1!=c(1,2))
      index.init.nonforest<-subset(PreQUES_traj_database,ID_L1!=1)
      index.init.nonforest<-subset(index.init.nonforest,ID_L1!=2)
      index.init.nonforest<-subset(index.init.nonforest, COUNT!=0)
      index.init.nonforest <- aggregate(COUNT ~  Z_NAME, data=index.init.nonforest, FUN=sum)
      colnames(index.init.nonforest)<-c('ZONE', 'Forest_T1')
      total.init.nonforest<-data.frame(ZONE="TOTAL",Forest_T1=sum(index.init.nonforest[,2]))
      index.init.nonforest<-rbind(index.init.nonforest,total.init.nonforest)
    },error=function(e){cat("No initial forest cover found", "\n")})
    
    tryCatch({
      #degradation rate ******
      degrad.rate<-merge(index.forest.degrad, index.init.forest, by='ZONE', all=T)
      degrad.rate$Degradation_Rate<-(degrad.rate[2]/degrad.rate[3]*100)[,1]
      degrad.rate<-cbind(degrad.rate[1], round(degrad.rate[4],2))
    },error=function(e){cat("No degradation found",conditionMessage(e), "\n")})
    
    tryCatch({
      #deforestation rate ******
      deforest.rate<-merge(index.deforest, index.init.forestandlogged, by='ZONE', all=T)
      deforest.rate$Deforestation_Rate<-(deforest.rate[2]/deforest.rate[3]*100)[,1]
      deforest.rate<-cbind(deforest.rate[1], round(deforest.rate[4],2))
    },error=function(e){cat("No deforestation found",conditionMessage(e), "\n")})
    
    tryCatch({
      #reforestation rate ******
      reforest.rate<-merge(index.reforest, index.init.nonforest, by='ZONE', all=T)
      reforest.rate$Reforestation_Rate<-(reforest.rate[2]/reforest.rate[3]*100)[,1]
      reforest.rate<-cbind(reforest.rate[1], round(reforest.rate[4],2))
    },error=function(e){cat("No reforestation found",conditionMessage(e), "\n")})
    
    #summary forest change in percentage
    forest.change.rate<-merge(deforest.rate, degrad.rate, by='ZONE', all=T)
    if(ncol(reforest.rate)==2){
      forest.change.rate<-merge(forest.change.rate, reforest.rate,by='ZONE',all=T)
    } else {print('no reforestation found')}
    
    
    #create trajectories map
    landuse_tr1<-landuse1
    landuse_tr2<-landuse2
    landuse_tr1<- reclassify(landuse_tr1, cbind(128,NA))
    landuse_tr2<- reclassify(landuse_tr2, cbind(128,NA))
    landuse_tr1<-ratify(landuse_tr1,count=TRUE,overwrite=TRUE)
    landuse_tr2<-ratify(landuse_tr2,count=TRUE,overwrite=TRUE)
    colnames(freq1)[1]="ID"
    levels(landuse_tr1)<-merge((levels(landuse_tr1)), freq1, by="ID", all = TRUE)
    colnames(freq2)[1]="ID"
    levels(landuse_tr2)<-merge((levels(landuse_tr2)), freq2, by="ID", all = TRUE)
    landuse_tr1<-deratify(landuse_tr1, 'ID_L1')
    landuse_tr2<-deratify(landuse_tr2, 'ID_L2')
    lu_trajectories<-overlay(landuse_tr1, landuse_tr2, fun=function(x,y){return((x*10)+y)})
    lu_trajectories<-ratify(lu_trajectories, count=TRUE, overwrite=TRUE)
    colnames(name_traj)[1]="ID"
    levels(lu_trajectories)<-merge((levels(lu_trajectories)),name_traj,by='ID', all = TRUE)
    lu_trajectories_final<-deratify(lu_trajectories,'ID_trf')
    lu_trajectories_final<-ratify(lu_trajectories_final, count=TRUE, overwrite=TRUE)
    colnames(leg_traj)[1]="ID"
    levels(lu_trajectories_final)<-merge((levels(lu_trajectories_final)),leg_traj,by='ID', all = TRUE)
    
    #calculate summary statistics by zone and overall
    PreQUES_traj_database.melt <- melt(data = PreQUES_traj_database, id.vars=c('Z_NAME','Traj', 'Traj_Code'), measure.vars=c('COUNT'))
    PreQUES_traj_database.zone <- dcast(data = PreQUES_traj_database.melt, formula = Z_NAME ~ Traj_Code, fun.aggregate = sum)
    PreQUES_traj_database.overal <- dcast(data = PreQUES_traj_database.melt, formula = Traj ~ ., fun.aggregate = sum)
    
    PreQUES_traj_forest.melt <- melt(data = PreQUES_traj_database, id.vars=c('Z_NAME','Def'), measure.vars=c('COUNT'))
    PreQUES_traj_forest.zone <- dcast(data = PreQUES_traj_forest.melt, formula = Z_NAME ~ Def, fun.aggregate = sum)
    PreQUES_traj_forest.zone$Other<-NULL
    PreQUES_traj_forest.zone$Others<-NULL
    
    PreQUES_traj_forest.overal <- dcast(data = PreQUES_traj_forest.melt, formula = Def ~ ., fun.aggregate = sum)
    
    PreQUES_traj_drive.melt <- melt(data = PreQUES_traj_database, id.vars=c('Traj','Def'), measure.vars=c('COUNT'))
    PreQUES_traj_drive.zone <- dcast(data = PreQUES_traj_drive.melt, formula = Traj ~ Def, fun.aggregate = sum)
    PreQUES_traj_drive.zone$Other<-NULL
    PreQUES_traj_drive.zone$Others<-NULL
    
    #plot trajectories map
    myColors1 <- brewer.pal(9,"Set1")
    myColors2 <- brewer.pal(8,"Accent")
    myColors3 <- brewer.pal(12,"Paired")
    myColors4 <- brewer.pal(9, "Pastel1")
    myColors5 <- brewer.pal(8, "Set2")
    myColors6 <- brewer.pal(8, "Dark2")
    myColors7 <- brewer.pal(11, "Spectral")
    myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
    
    if (0 %in% levels(lu_trajectories_final)[[1]][1]){
      myColors  <-c(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
    } else {
      myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
    }
    myColors.lu <- myColors[1:nrow(unique(levels(lu_trajectories_final)[[1]][1]))]
    
    ColScale.lu<-scale_fill_manual(name="Jenis alur perubahan", breaks=as.vector(unlist(levels(lu_trajectories_final)[[1]][1])), labels=as.vector(unlist(levels(lu_trajectories_final)[[1]][3])), values=myColors.lu)
    
    plot.LU1<-gplot(lu_trajectories_final, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
      coord_equal() + ColScale.lu +
      theme(plot.title = element_text(lineheight= 5, face="bold")) +
      theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title = element_text(size=9),
             legend.text = element_text(size = 8),
             legend.key.height = unit(0.25, "cm"),
             legend.key.width = unit(0.25, "cm"))
    
    colnames(PreQUES_traj_database.melt)<-c("Zone", "Trajectories","Abbrev", "variable", "Area"); #rename column names
    #graph needs to be checked, sometimes plot with melt data type isn't correctly displaying   
    plot_traj<-ggplot(data=PreQUES_traj_database.melt,aes(factor(Zone),Area,fill=factor(Trajectories)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
      theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
      theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)')+coord_flip()+
      theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
    
    plot_traj_group<-ggplot(data=PreQUES_traj_database.overal,aes(Traj,.,fill=Traj))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
      theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
      theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan lahan', y='Luas area (Ha)')+coord_flip()+
      theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
    
    colnames(PreQUES_traj_forest.melt)<-c("Zone", "Forest_Change","variable", "Area"); #rename column names
    #graph needs to be checked, sometimes plot with melt data type isn't correctly displaying   
    plot_def<-ggplot(data=PreQUES_traj_forest.melt,aes(factor(Zone),Area,fill=factor(Forest_Change)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
      theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
      theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
      theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
    
    colnames(PreQUES_traj_drive.melt)<-c("Trajectories", "Forest_Change","variable", "Area"); #rename column names
    #graph needs to be checked, sometimes plot with melt data type isn't correctly displaying   
    plot_drive<-ggplot(data=PreQUES_traj_drive.melt,aes(factor(Trajectories),Area,fill=factor(Forest_Change)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
      theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
      theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
      theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
    
    colnames(PreQUES_traj_database.overal)<-c("Trajectories", "Area (Ha)")
    colnames(PreQUES_traj_database.zone)[1]<-c("Trajectories")
    colnames(PreQUES_traj_forest.overal)<-c("Forest cover changes", "Area (Ha)")
    
    #trajectories data export
    PreQUES.index.traj=paste( "_",pu_name,"_",T1, "_",T2, sep="")
    eval(parse(text=(paste("PreQUES_traj_database", PreQUES.index.traj, "<-PreQUES_traj_database", sep=""   ))))
    newTraj<-paste("PreQUES_traj_database", PreQUES.index.traj, sep="")
    
    #eval(parse(text=(paste("PreQUES_traj_summary", PreQUES.index.traj, "<-PreQUES_traj_database.overal", sep=""   ))))
    #newTrajsum<-paste("PreQUES_traj_summary", PreQUES.index.traj, sep="")
    
    #eval(parse(text=(paste("PreQUES_traj_zone", PreQUES.index.traj, "<-PreQUES_traj_database.zone", sep=""   ))))
    #newTrajz<-paste("PreQUES_traj_zone", PreQUES.index.traj, sep="")
    
    #eval(parse(text=(paste("PreQUES_traj_map", PreQUES.index.traj, "<-lu_trajectories_final", sep=""   ))))
    #newTrajmap<-paste("PreQUES_traj_map", PreQUES.index.traj, sep="")
    
    #command<-paste("resave(", newTraj, ",", newTrajsum, ",",newTrajz,",",newTrajmap, ",", sep="")
    
    #write report
    rtffile <- RTF("Pre-QUES_Trajectory_report.doc", font.size=11, width = 8.267, height = 11.692, omi = c(0,0,0,0))
    file.copy(paste0(LUMENS_path, "/ques_cover.png"), result_dir, recursive = FALSE)
    file.copy(paste0(LUMENS_path, "/ques_line.png"), result_dir, recursive = FALSE)
    img_location<-paste0(result_dir, "/ques_cover.png")
    line_location<-paste0(result_dir, "/ques_line.png")
    
    # loading the .png image to be edited
    cover <- image_read(img_location)
    # to display, only requires to execute the variable name, e.g.: "> cover"
    # adding text at the desired location
    text_submodule <- paste("Sub-Modul Perubahan Penggunaan Lahan\n\nAnalisis Alur Penggunaan Lahan\n", location, ", ", "Periode ", T1, "-", T2, sep="")
    cover_image <- image_annotate(cover, text_submodule, size = 23, gravity = "southwest", color = "white", location = "+46+220", font = "Arial")
    cover_image <- image_write(cover_image)
    # 'gravity' defines the 'baseline' anchor of annotation. "southwest" defines the text shoul be anchored on bottom left of the image
    # 'location' defines the relative location of the text to the anchor defined in 'gravity'
    # configure font type
    addPng(rtffile, cover_image, width = 8.267, height = 11.692)
    addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1,1,1,1))
    
    title1<-"{\\colortbl;\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red146\\green208\\blue80;\\red0\\green176\\blue240;\\red140\\green175\\blue71;\\red0\\green112\\blue192;\\red79\\green98\\blue40;} \\pard\\qr\\b\\fs70\\cf2 L\\cf3U\\cf4M\\cf5E\\cf6N\\cf7S \\cf1REPORT \\par\\b0\\fs20\\ql\\cf1"
    title2<-paste("\\pard\\qr\\b\\fs40\\cf1 PreQUES-Land Use Trajectory Analysis ", "for ", location, ", ", province, ", ", country, "\\par\\b0\\fs20\\ql\\cf1", sep="")
    sub_title<-"\\cf2\\b\\fs32 ANALISA ALUR PENGGUNAAN LAHAN\\cf1\\b0\\fs20"
    #date<-paste("Date : ", date, sep="")
    time_end<-paste("Proses selesai : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
    line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
    area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
    I_O_period_1_rep<-paste("\\b","\\fs20", T1)
    I_O_period_2_rep<-paste("\\b","\\fs20", T2)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addNewLine(rtffile)
    addParagraph(rtffile, title1)
    addParagraph(rtffile, title2)
    addNewLine(rtffile)
    addPng(rtffile, line_location)
    addParagraph(rtffile, time_start)
    addParagraph(rtffile, time_end)
    addPng(rtffile, line_location)
    addNewLine(rtffile)
    width<-as.vector(c(1.34,3.1))
    addTable(rtffile,proj_descr,font.size=8,col.widths=width)
    addPageBreak(rtffile)
    addParagraph(rtffile, sub_title)
    addNewLine(rtffile)
    addPng(rtffile, line_location)
    
    #addParagraph(rtffile, date)
    addNewLine(rtffile)
    addParagraph(rtffile, "Analisa perubahan tutupan lahan dilakukan untuk mengetahui kecenderungan perubahan tutupan lahan di suatu daerah pada satu kurun waktu. Analisa ini dilakukan dengan menggunakan data peta tutupan lahan pada dua periode waktu yang berbeda. Selain itu, dengan memasukkan data unit perencanaan kedalam proses analisa, dapat diketahui kecenderungan perubahan tutupan lahan pada masing-masing kelas unit perencanaan yang ada. Informasi yang dihasilkan melalui analisa ini dapat digunakan dalam proses perencanaan untuk berbagai hal. Diantaranya adalah: menentukan prioritas pembangunan, mengetahui faktor pemicu perubahan penggunaan lahan, merencanakan skenario pembangunan di masa yang akan datang, dan lain sebagainya.")
    addNewLine(rtffile)
    addParagraph(rtffile, paste("\\b \\fs24 ALUR PERUBAHAN PENGGUNAAN LAHAN\\b0 \\fs24", sep=""))
    addNewLine(rtffile)
    addPng(rtffile, line_location)
    addNewLine(rtffile)
    addParagraph(rtffile, "Alur perubahan penggunaan lahan merupakan ringkasan keseluruhan tipe rangkaian perubahan penggunaan lahan yang mungkin terjadi di sebuah daerah. Kategori besar dari alur perubahan lahan dibagi menjadi dua jenis yaitu Loss of tree cover dan recovery of tree cover")
    addNewLine(rtffile)
    
    addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot.LU1)
    addParagraph(rtffile, paste("\\b \\fs20 Gambar 1. Peta Kelompok Perubahan Penutupan Lahan\\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
    addNewLine(rtffile)
    addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_traj_group)
    addParagraph(rtffile, paste("\\b \\fs20 Gambar 2. Grafik Kelompok Perubahan Penutupan Lahan\\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
    addNewLine(rtffile)
    addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_traj)
    addParagraph(rtffile, paste("\\b \\fs20 Gambar 3. Grafik Kelompok Perubahan Penutupan Lahan\\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",  I_O_period_1_rep,"-", I_O_period_2_rep,"\\b \\fs20 berdasarkan Unit Perencanaan \\b0 \\fs20", sep=" "))
    addNewLine(rtffile)
    addNewLine(rtffile)
    addParagraph(rtffile, paste("\\b \\fs20 Tabel 1. Luas Area Kelompok Perubahan Penutupan Lahan \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",  I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
    addTable(rtffile,PreQUES_traj_database.overal,font.size=8)
    addNewLine(rtffile)
    addParagraph(rtffile, paste("\\b \\fs20 Tabel 2. Luas Area Kelompok Perubahan Lahan di \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20", I_O_period_2_rep,"\\b \\fs20 Tiap Unit Perencanaan \\b0 \\fs20", sep=" "))
    addTable(rtffile,PreQUES_traj_database.zone,font.size=8)
    
    #plot: total changes per trajectory
    for(s in 2:(ncol(PreQUES_traj_database.zone))){
      print(s)
      c<-s-1
      PreQUES_traj_database.zone.melt_pertrajek<- melt(data = PreQUES_traj_database.zone, id.vars=c('Trajectories'), measure.vars=c(colnames(PreQUES_traj_database.zone)[s]))
      plot_per_trajek<-ggplot(data=PreQUES_traj_database.zone.melt_pertrajek,aes(factor(Trajectories),value,fill=factor(Trajectories)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
        theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
        theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan lahan', y='Luas area (Ha)')+coord_flip()+
        theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
      #eval(parse(text=( paste("plot.per.trajek_",s,'_',colnames(PreQUES_traj_database.zone)[s],'<-plot_per_trajek', sep=''))));#save plots
      addNewLine(rtffile)
      addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_per_trajek)
      addNewLine(rtffile)
      addParagraph(rtffile, paste("\\b \\fs20 Sub Gambar ",c,". Grafik Perubahan Lahan di Berbagai Zona Perencanaan untuk jenis ",colnames(PreQUES_traj_database.zone)[s], "\\b0 \\fs20 di ", location, "\\b \\fs20 Tahun \\b0 \\fs20",I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
      addNewLine(rtffile)
    }
    
    addNewLine(rtffile)
    addParagraph(rtffile, paste("\\b \\fs24 PERUBAHAN TUTUPAN HUTAN\\b0 \\fs24", sep=""))
    addNewLine(rtffile)
    addPng(rtffile, line_location)
    addNewLine(rtffile)
    addParagraph(rtffile, "Salah satu bentuk alur perubahan penggunaan lahan yang paling banyak mendapatkan perhatian adalah alur perubahan hutan alam menjadi tipe tutupan lahan lainnya (deforestasi) dan perubahan hutan alam primer menjadi hutan alam sekunder (degradasi). Bagian ini memperlihatkan hasil analisa LUMENS terhadap perubahan tutupan hutan di sebuah daerah")
    addNewLine(rtffile)
    addParagraph(rtffile, paste("\\b \\fs20 Gambar 4. Grafik Perubahan Tutupan Hutan di Berbagai Zona Perencanaan\\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep,"\\b \\fs20 berdasarkan Unit Perencanaan \\b0 \\fs20", sep=" "))
    addNewLine(rtffile)
    addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_def)
    addNewLine(rtffile)
    addParagraph(rtffile, paste("\\b \\fs20 Tabel 3. Luas deforestasi \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
    addTable(rtffile,PreQUES_traj_forest.overal,font.size=8)
    addNewLine(rtffile)
    addParagraph(rtffile, paste("\\b \\fs20 Tabel 4. Luas deforestasi berdasarkan zonasi \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20",I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
    addTable(rtffile,PreQUES_traj_forest.zone,font.size=8)
    addNewLine(rtffile)
    addParagraph(rtffile, paste("\\b \\fs20 Tabel 5. Laju deforestasi berdasarkan zonasi \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
    addTable(rtffile,forest.change.rate,font.size=8)
    addNewLine(rtffile)
    
    #plot: total changes per trajectory
    for(s in 2:(ncol(PreQUES_traj_forest.zone))){
      print(s)
      c<-s-1
      PreQUES_traj_database.zone.melt_pertrajek<- melt(data = PreQUES_traj_forest.zone, id.vars=c('Z_NAME'), measure.vars=c(colnames(PreQUES_traj_forest.zone)[s]))
      colnames(PreQUES_traj_database.zone.melt_pertrajek)[1]<-'ZONE'
      plot_per_trajek<-ggplot(data=PreQUES_traj_database.zone.melt_pertrajek,aes(factor(ZONE),value,fill=factor(ZONE)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
        theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
        theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
        theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
      #eval(parse(text=( paste("plot.per.trajek_",s,'_',colnames(PreQUES_traj_database.zone)[s],'<-plot_per_trajek', sep=''))));#save plots
      addNewLine(rtffile)
      addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_per_trajek)
      addNewLine(rtffile)
      addParagraph(rtffile, paste("\\b \\fs20 Sub Gambar ",c,". Grafik Perubahan Hutan di Berbagai Zona Perencanaan untuk ",colnames(PreQUES_traj_forest.zone)[s], "\b  di \\b0 \\fs20", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
      addNewLine(rtffile)
    }
    
    addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_drive)
    addNewLine(rtffile)
    addParagraph(rtffile, paste("\\b \\fs20 Gambar 5. Grafik Perubahan Tutupan Hutan dan alur perubahan yang menyebabkannya\\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep,"\\b \\fs20 berdasarkan Unit Perencanaan \\b0 \\fs20", sep=" "))
    addNewLine(rtffile)
    
    addParagraph(rtffile, paste("\\b \\fs20 Tabel 5. Luas deforestasi berdasarkan alur perubahan \\b0 \\fs20 ", location, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep,"-", I_O_period_2_rep, sep=" "))
    addTable(rtffile,PreQUES_traj_drive.zone,font.size=8)
    addNewLine(rtffile)
    done(rtffile)
    
    #eval(parse(text=(paste("rtffileTraj_", check_record, " <- rtffile", sep=""))))
    #command<-paste("resave(rtffileTraj_",check_record,",run_record,", newTraj,",", sep="")
    #command<-paste(command,"file='",proj.file,"')", sep="")
    
    #eval(parse(text=(command)))
    
    eval(parse(text=(paste('rtf_Traj_', T1, '_', T2, '_', pu_name, '<-rtffile', sep=''))))
    eval(parse(text=(paste('resave(rtf_Traj_', T1, '_', T2, '_', pu_name, ', file=proj.file)', sep=''))))
  }
  
  unlink(list.files(pattern = ".tif"))
  unlink(list.files(pattern = ".grd"))
  unlink(list.files(pattern = ".gri"))
  unlink(list.files(pattern = ".png"))
  
  
  # command2<-paste("start ", "winword ", result_dir, "/LUMENS_Pre-QUES_change_report.lpr", sep="" )
  # shell(command2)
  dbDisconnect(DB)
  #CLEAN ENVIRONMENT
  #rm(list=ls(all.names=TRUE))
  
  #=Writing final status message (code, message)
  statuscode<-1
  statusmessage<-"Pre-QUES analysis successfully completed!"
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
  
} # Loop for couple ends ADopen


# ===TODO=================================================

# 1. Setting up basic database parameters====
proj.file <- choose.files()
# select the .lpj based on which the project shall be executed
load(proj.file)

driver <- dbDriver('PostgreSQL')
project <- as.character(proj_descr[1,2])
DB <- dbConnect(
  driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port), #ADCHECK
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)

# calling up the lists of data
# Setting up the working directory
setwd(paste0(dirname(proj.file), "/QUES"))
# derive the list of available data----
list_of_data_luc<-dbReadTable(DB, c("public", "list_of_data_luc"))
list_of_data_pu<-dbReadTable(DB, c("public", "list_of_data_pu"))
list_of_data_lut<-dbReadTable(DB, c("public", "list_of_data_lut"))
list_of_data_f<-dbReadTable(DB, c("public", "list_of_data_f"))

# To retrieve ref data


# retrieve the cstock and peat_em also other tables====
c_luName <- "cstock"
peat_luName <- "em_peat"
pu_name <- "pu_ver1"
if (pu_name=="ref") {
  lookup_z <- dbReadTable(DB, c("public", "lut_ref"))
  names(lookup_z) <- c("ID_Z", "Z_NAME")
} else {
  lookup_z<-dbReadTable(DB, c("public", list_of_data_pu[list_of_data_pu$RST_NAME==pu_name, "LUT_NAME"]))
  names(lookup_z) <- c("ID_Z", "COUNT", "Z_NAME")
  lookup_z <- lookup_z[ , c(1,3)]
}
# standardized column names

# extract peat planning unit IDs
peat_keyword <- "eat"
peat_puID <- lookup_z[grep(peat_keyword, lookup_z[,2]) , 1]
cstock <- dbReadTable(DB, c("public", list_of_data_lut[list_of_data_lut$TBL_NAME==c_luName, "TBL_DATA"]))
em_peat <- dbReadTable(DB, c("public", list_of_data_lut[list_of_data_lut$TBL_NAME==peat_luName, "TBL_DATA"]))

# lut_ref has been available in the proj.file

# list ids for forest dynamics====
forest_ids <- cstock %>% filter(grepl(" forest", Landcover) | grepl("mangrove", Landcover)) %>% dplyr::select(ID) %>% pull()
primaryForest_ids <- cstock %>% filter(grepl("^Undisturbed", Landcover)) %>% dplyr::select(ID) %>% pull()
secForest_ids <- forest_ids %>% setdiff(primaryForest_ids)
hiDensFor_ids <- cstock %>% filter(grepl("high dens", Landcover)) %>% dplyr::select(ID) %>% pull()
loDensFor_ids <- cstock %>% filter(grepl("low dens", Landcover)) %>% dplyr::select(ID) %>% pull()
# ... rest dynamics \ends----


# setting the combined raster layer
ref <- ref*10^6

yir <- unique(list_of_data_luc$PERIOD)
yir <- sort(yir)

# LOOPING FRAMEWORK FOR EACH TIMESTEP====
for(ts in 1: (length(yir)-1)){
  # retrieve the changemap
  ti1 <- yir[ts]
  ti2 <- yir[ts+1]
  d_ti <- ti2-ti1 # delta year
  chMap_name <- paste0("chgmap_", pu_name, ti1, ti2)
  chMap_name <- list_of_data_f[list_of_data_f$RST_NAME == chMap_name, "RST_DATA"]
  ch_map <- getRasterFromPG(pgconf, project, chMap_name, paste0(chMap_name, ".tif"))#getRasterFromPG(pgconf, project, data_pu$RST_DATA, paste(data_pu$RST_DATA, '.tif', sep=''))
  # combine with admin data
  ch_map <- ref+ch_map
  # retrieve freq
  fr_chMap <- data.frame(freq(ch_map, useNA = "no"), stringsAsFactors= FALSE)
  # disaggregate using substr # note that the result will be presented as character. Do convert into numeric
  fr_chMap$IDADM <- floor(fr_chMap$value / 10^6)
  fr_chMap$ID_Z <- fr_chMap$value%%10^2
  fr_chMap$ID_T1 <- floor(fr_chMap$value%%10^4/10^2)
  fr_chMap$ID_T2 <- floor(fr_chMap$value%%10^6/10^4)
  fr_chMap$DegDef <- 0
  
  # assess for any indication of degradation (1) or deforestation (2)
  # 1. Degradation====
  fr_chMap <- fr_chMap %>% mutate(DegDef = case_when(ID_T1 %in% primaryForest_ids & ID_T2 %in% secForest_ids ~ 1,
                                                     ID_T1 %in% hiDensFor_ids & ID_T2 %in% loDensFor_ids ~ 1,
                                                     TRUE ~ DegDef
  ))
  # 1. ...\ends----
  # 2. Deforestation====
  fr_chMap <- fr_chMap %>% mutate(DegDef = case_when(ID_T1 %in% forest_ids & !ID_T2 %in% forest_ids ~ 2,
                                                     TRUE ~ DegDef
  ))
  # 2. .... \ends----
  # Replacing 0 at DegDef column with NA
  fr_chMap[fr_chMap$DegDef == 0, "DegDef"] <- NA
  # Define new 'hectare' column
  fr_chMap <- fr_chMap %>% mutate(hectares = count* res(ref)[1]^2/10000)
  # carbon merge t1-t2
  for(w in 1:2){
    if(w ==1) orNames <- names(cstock)
    names(cstock)[1] <-paste0("ID_T", w)
    names(cstock)[2] <-paste0("LC_", w)
    names(cstock)[3] <- paste0("c_", w)
    fr_chMap <- merge(fr_chMap, cstock, by= paste0("ID_T", w), all.x = TRUE)
    if(w ==2) names(cstock) <- orNames
  }
  
  # calculate emission
  fr_chMap$Em_co2Eq <- (fr_chMap$c_1 - fr_chMap$c_2)*fr_chMap$count*res(ref)[1]^2/10000 * 3.67
  fr_chMap$Seq <- 0
  fr_chMap[fr_chMap$Em_co2Eq < 0, "Seq"] <- -1* fr_chMap[fr_chMap$Em_co2Eq < 0, "Em_co2Eq"]
  fr_chMap[fr_chMap$Seq > 0, "Em_co2Eq"] <- 0 # correcting negative emission
  # peat_em merge t1-t2
  rec_table <- fr_chMap[, c("value", "Em_co2Eq", "Seq", "count", "DegDef")]
  rec_table$Em_px <- rec_table$Em_co2Eq/rec_table$count
  rec_table$Seq_px <- rec_table$Seq/rec_table$count
  # generate emission and sequestration map
  em_map <- reclassify(ch_map, rec_table[, c("value", "Em_px")])
  writeRaster(em_map, paste0("QUES-C/Em_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
  seq_map <- reclassify(ch_map, rec_table[, c("value", "Seq_px")])
  writeRaster(seq_map, paste0("QUES-C/Seq_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
  # Extra degdef_map
  degDef_map <- reclassify(ch_map, rec_table[, c("value", "DegDef")])
  writeRaster(degDef_map, paste0("QUES-C/DegDef_", ti1, "_", ti2, ".tif"), overwrite =TRUE)
  # peat_puID contains numbers which stand for id of pu containing peat
  for(w in 1:2){
    em_peatMod <- em_peat[, c(1, 3)]
    em_peatMod[, 2] <- em_peatMod[, 2]*d_ti/2
    names(em_peatMod)[1] <-paste0("ID_T", w)
    names(em_peatMod)[2] <- paste0("EmPeat_", w)
    fr_chMap <- merge(fr_chMap, em_peatMod, by= paste0("ID_T", w), all.x = TRUE)
  }
  # correction for peat_em which falls in non-peat planning unit (*0)
  fr_chMap[which(!fr_chMap$ID_Z %in% peat_puID), c("EmPeat_1", "EmPeat_2")] <- fr_chMap[which(!fr_chMap$ID_Z %in% peat_puID), c("EmPeat_1", "EmPeat_2")]*0
  # calculate total peat_em
  fr_chMap[, c("EmPeat_1", "EmPeat_2")] <- fr_chMap[, c("EmPeat_1", "EmPeat_2")]*fr_chMap[, "count"]* res(ref)[1]^2/10000 # conversion factor to hectare
  fr_chMap$EmPeatTot <- fr_chMap$EmPeat_1 + fr_chMap$EmPeat_2
  # legend merge using lookup tables: pu, admin
  fr_chMap <- merge(fr_chMap, lut_ref, by = "IDADM", all.x = TRUE)
  fr_chMap <- merge(fr_chMap, lookup_z, by = "ID_Z", all.x =TRUE)
  # add period annotation
  fr_chMap$PERIOD <- paste0(ti1, "-", ti2)
  # store at master table, in .csv, rbind with previous runs
  if(ts == 1) carb_compile <- fr_chMap else carb_compile <- data.frame(rbind(carb_compile, fr_chMap), stringsAsFactors = FALSE)
  # Summarize calculation result following the template: Period; Gross Em; Seq; Nett abg em; peat em; Total em. Store as .csv
  if(ts == 1) {
    summary_table <- data.frame(PERIOD = paste0(ti1, "-", ti2), G_Em = sum(fr_chMap$Em_co2Eq), Seq = sum(fr_chMap$Seq), P_Em = sum(fr_chMap$EmPeatTot), stringsAsFactors = FALSE)
  } else {
    summary_add <- data.frame(PERIOD = paste0(ti1, "-", ti2), G_Em = sum(fr_chMap$Em_co2Eq), Seq = sum(fr_chMap$Seq), P_Em = sum(fr_chMap$EmPeatTot), stringsAsFactors = FALSE)
    summary_table <- data.frame(rbind(summary_table, summary_add), stringsAsFactors = FALSE)
  }
  # degDef_summTab ====
  if(ts == 1) {
    degDef_summTab <- data.frame(PERIOD = paste0(ti1, "-", ti2), Degradation = fr_chMap %>% filter(DegDef == 1) %>% dplyr::select(hectares) %>% pull() %>% sum(), Deforestation = fr_chMap %>% filter(DegDef == 2) %>% dplyr::select(hectares) %>% pull() %>% sum(), stringsAsFactors = FALSE)
  } else {
    degDef_summAdd <- data.frame(PERIOD = paste0(ti1, "-", ti2), Degradation = fr_chMap %>% filter(DegDef == 1) %>% dplyr::select(hectares) %>% pull() %>% sum(), Deforestation = fr_chMap %>% filter(DegDef == 2) %>% dplyr::select(hectares) %>% pull() %>% sum(), stringsAsFactors = FALSE)
    degDef_summTab <- data.frame(rbind(degDef_summTab, degDef_summAdd), stringsAsFactors = FALSE)
  }
  # degDef_summTab \ends----
  if(ts == (length(yir)-1)){
    summary_table$NettAll <- summary_table$G_Em - summary_table$Seq + summary_table$P_Em
    write.csv(summary_table, "QUES-C/summary_emission.csv", row.names = FALSE)
    write.csv(carb_compile, "QUES-C/emission_LCdynamics.csv", row.names = FALSE)
    write.csv(degDef_summTab, "QUES-C/summary_degDef.csv", row.names = FALSE)
  }
}

