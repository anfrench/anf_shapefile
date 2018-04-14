#demo write a shapefile
# written by Andrew N. French , USDA/ARS Maricopa, AZ
# commments to andrew.french@ars.usda.gov

# this version written December 2017

#need to include these libraries to write shapefiles
library(rgeos) #
library(maptools)
library(shapefiles)

homedir <- "C:/Data/progs/rprogs" #"D:/AFren_stuff/progs/rprogs"

# This version writes a shape file for polygon (type code '5') and optionally incorporates attributes
# also writes centroid shape file for each polygon
# UTM coordinates in meters, right handed coordinates assumed
# starting plot position, say nw or ne or sw or se
# whether to fill by column or row first
# whether to fill serpentine pattern or by dead-head

ldir <- "C:/Data/F13B2/2017_2018Wheat/"
  #"C:/Data/F13B2/2017/"
  #"E:/Data/F13_B2/2017/"
  #"E:/Data/F115/2017/"
  #"C:/Data/HTP_MACworkshop/UAV/" 
  #"C:/Data/F115_maize/2016/GIS/"
#setwd(ldir)

#FUNCTIONS
ft2m <- function(lft) {
  #convert feet to meters
  lm <- 0.3048*lft
}

deg2rad <- function(ldeg) {
  #convert decimal degrees to radians
  lrad <- ldeg*pi/180.0
}
rad2deg <- function(lrad) {
  #convert radians to decimal degrees
  ldeg <- lrad*180.0/pi
}

#END FUNCTIONS

#=======================
#USER SPECIFICATIONS

ofil     <- "wheatF13B2_2017_2018_2rowplots"
  #"wheatF13B2_2017_2018_1rowplots"
  #"wheatF13B2_2017_2018_2rowplots"
  #"wheatF13B2_2017_2018_1rowplots"
  #"sorghumF13B2_2017_UTMCoords"
  #"maizeF115_2017_Plot1_260dummy"
  #"maizeF115_2017_Plot261_520"
  #"maizeF115_2017_Plot1_260"
  #"sorghumexpfall2016" #desired name of output shapefile
len_ofil <- nchar(ofil)
ocentroidfil <- paste(ofil,"_centroid",sep="")
#If you dont have polygons aligned with UTM grid axes you can specify the rotation angle and rotation point

oparmfil <- paste(ofil,"_parms.txt",sep="")


gridazimuthdeg <- -0.51 #-0.51 #-0.0 #-0.0#-20.0 # counter-clockwise variation in degrees of local grid 'up' azimuth from UTM grid north
#use negative value for clockwise rotation
#probably want to choose rotation center at mid-point of field

rotationcenter_x <-  409002.0 #107.3525 #409030.64 #409025.240  #409002.0 #409001.0 #409017.0 #UTM Easting
rotationcenter_y <- 3660080.0 #  12.224 #3659442.85 #3659491.006 #3660079.0 #3660085.0-0.3-(3.657/2.0) #3659444.0 #UTM Northing


#Hemisphere use "N" or "S" # if North use N, sets false Northing to 0.0
# if South use, sets false Northing to 10000000.0
# false Easting for both N and S is 500000.0
hemisphere <- "N"

#UTM Zone zones are longitude bands 6 degrees wide, zone 1 -180 to -174, zone 60 174 to 180
utmzone <- 12 #UTM Zone integer, valid range is 1-60

#Uppper left grid reference coordinates in UTM
# ===================================================================================
#N.B. THIS IS THE UTM coordinate of the ->northwest<- corner of the northwesternmost plot
# 

xref0  <- 408990.151+(0.01*14.5)-0.017
  #408989.209+(1.5*0.764)-0.204 #409036.2642 #-1.152 #0.0 #409025.243149 #F115 western plots #EASTING
  #409039.464 #F115 eastern plots
  #409025.243149 #F115 western plots #409025.240-0.41855886+0.421708  #408993.341-0.8865 #408991.378-0.94 #409012.0 #UTM Easting meters

yref0  <- 3660186.834-(0.01*6.74)
  #3660187.319-0.485 #3660188.556 #24.822#24.448 #0.0 #3659494.054#3659394.994 #F115 eastern and western plots #NORTHING
  #3659394.994 #F115 western plots#3659494.054 #-3.135541-0.048524 #3660179.136-2.13-4.135+1.437 #3660187.988 #3659502.0 #UTM Northing meters
# ===================================================================================

#plot dimensions for one plot, all plots will be the same size

pltxdim <- 0.762*2 #3.50 #ft2m(40.0/12.0) 
#1.530 #1.51-0.08 #ft2m(40.0/12.0) # horizontal dimension
#convert 40 inch to meters; or you can just enter the dimensions in meters

pltydim <- 3.4852 #3.50-(0.0148) #0.764 #ft2m(10.0) #3.685 #3.657 #ft2m(10.0) #vertical dimension

#buffer dimensions along vertical dimension
buffY_xdim <- pltxdim
buffY_ydim <- 0.5 #0.485 #0.0 #ft2m(3.0) #0.30 #ft2m(3.0)

#buffer dimensions along horizontal dimension
buffX_xdim <- 0.0 #0.458 #pltxdim #0.0
buffX_ydim <- pltydim #0.764 #pltydim #0.0


numplots_horiz <- 16 #32 #16 #32 #54 #10 #7#8#10 #Number of polygons to draw along the horizontal
numplots_vert  <- 54 #32 #26 #50 #28 #Number of polygons to draw along the vertical

#create matrix for numbering

idnumstart <- 1 #261 #starting plot number
idnumincr  <- 1 #plot number increment


#plotnumorder determines if numbering is row-major or column major
# for east to west or west to east specify ew
# for north to south or south to north specify nw
# for serpentine pattern append the selection with 's'

plotnumorder <- "ew" #"ns" #ews" #"ews" #options are ew, ns; or for serpentine ews or nss

#startplotnum determines which of the four corners of the field is the starting number
# Northwest corner: nw; Southwest corner: sw; Northeast corner:ne; Southeast corner: se

startplotnum <- "sw" #"nw" #sw" #"nw" #options are nw, ne, sw, se


# or hard-code the attribute names in ddTable below!


#option to attach attributes by reading from csv file
# ======================================================================
#N.B.- the csv file must have header record with one of them being "Id"
# There must be the same number of Id records in attribfil as there are specified in 
# the plot numbering step

# ======================================================================

#make sure the head labels dont use special symbols, no dots, dollar signs
#the file needs to be place in same working directory as destination

use_attribute_file <- "yes" #set to "yes" if you want to read attribute data file
#if you dont use attribute file then Plot_ID attribute field will be created


attribfil <- "Season5_DurumWheat_Attributes_Plots_TwoRowplotid.csv"
  #"Season5_DurumWheat_Attributes_SubPlots_OneRowplotid.csv"
  #"Season5_DurumWheat_Attributes_Plots_TwoRowplotid.csv"
  #"Season5_DurumWheat_Attributes_SubPlots_OneRowplotid.csv"
  #"TERRAREF_2017_Sorghum_season4_ShapeFilePLotNames.csv"
  #"2017_CornellFieldDesign_MaricopaAZtable_P1_260.csv"




# ===========================



#END USER SPECIFICATIONS

if(use_attribute_file=="yes") {
 #open and read attribute file
 fattribfil  <- paste(ldir,attribfil,sep="")
 attribdat   <- read.table(fattribfil,header=TRUE,sep=",",stringsAsFactors=FALSE)
 attribnames <- names(attribdat)
 dimattribdat <- dim(attribdat)

#determine if the attribute file has the required Id field, and halt if not
 locid <- which(attribnames=='Id')
 #if(length(locid==0)) {
#  print(paste("your attribute file does not have an Id field but must!"))
#  stop
# }

#determine the number of unique Id values in the attribute file
 attribiduniq <- unique(attribdat$Id)
 numattribiduniq <- length(attribdat)
 dimattrib <- dim(attribdat)
 numrecsattrib <- dimattrib[1]
#identify attributes that are not 'Id' labeled
 numattribnames   <- length(attribnames)
 nonid            <- which(attribnames!='Id')
 myattribnames    <- attribnames[nonid]
 nummyattribnames <- length(myattribnames)

} # end of attribute file reading 
# ================================

#build full path names for output shapefiles and parms file
fofil    <- paste(ldir,ofil,sep="")
focentroidfil <- paste(ldir,ocentroidfil,sep="")
foparmfil <- paste(ldir,oparmfil,sep="")

#write prj file as a string, much easier than trying to install rgdal and readOGR
fofilprj <- paste(ldir,ofil,".prj",sep="") #this will be file name for the prj file
focentroidfilprj <- paste(ldir,ocentroidfil,".prj",sep="")

#total number of plots is product of horizontal and vertical counts
numplots_total <- numplots_horiz*numplots_vert #compute total number of polygons in shapefile

#the number of plots must equal the number of unique Id values in attribute file if you 
# are going to append attributes

if(numplots_total!=numrecsattrib) {
  print(paste("the number of plots to create does not equal the number of unique Id values in attribute table."))
  stop
}


#generate the plot numbers
plotnumvec <- (0:(numplots_total-1))*idnumincr+idnumstart

#generate UTM information for creation of the 'prj' shapefile
utmzoneint <- as.integer(utmzone) #ensure value is integer not numeric

utmzonestr1 <- sprintf("%2d",utmzoneint) #convert integer to string
utmzonestr2 <- gsub(" ","",utmzonestr1) #strip out blanks

#compute central meridian given the UTM zone
centralmerid <- (6.0*(utmzone-1))-177.0
centralmeridstr <- sprintf("%6.1f",centralmerid)


plotdimxy <- c(pltxdim,pltydim) #extent of one plot in meters, x then y dimension


#compute reference coordinate for northwest corner of southwesternmost plot
xref <- xref0
yref <- yref0-((numplots_vert-1)*(pltydim+buffY_ydim))

#translate coordinates to have origin centered over rotation point
#after rotation, translate coordinates back to original coordinate system

xorig <- xref-rotationcenter_x #translate horizontal points relative to rotation point
yorig <- -(rotationcenter_y-yref)

#layout of plot with four corners

#total number of vertices
numpoints_plots <- numplots_total*5 #number of vertices per single plot is 4 but repeat first point to close the polygon
#define vertices clockwise starting at nw corner
plotxoffsets <- c(0.0,plotdimxy[1],plotdimxy[1],0.0)
plotyoffsets <- c(0.0,0.0,-plotdimxy[2],-plotdimxy[2])

#initialize vectors for x and y locations of vertices
lxvals_plots <- rep(0.0,numpoints_plots)
lyvals_plots <- rep(0.0,numpoints_plots)

#compute points upper left to lower right row-wise
#

#for(rct in numplots_vert:1) {
for(rct in 1:numplots_vert) { #row counter
  lyoff <- yorig+((rct-1)*(plotdimxy[2]+buffY_ydim))
  for(cct in 1:numplots_horiz) { #column counter
    lxoff <- xorig+((cct-1)*(plotdimxy[1]+buffX_xdim))
    
    for(vct in 1:4) { #vertex counter
      lindex <- (((numplots_vert-rct))*numplots_horiz*5)+(cct-1)*5+vct #count by 5! to account for repeated first vertex
      print(paste("lindex:",lindex))
      lxvals_plots[lindex] <- lxoff+plotxoffsets[vct]
      lyvals_plots[lindex] <- lyoff+plotyoffsets[vct]
    }
    #add fifth point to close polygon
      lindex <- lindex+1
      lxvals_plots[lindex] <- lxvals_plots[lindex-4]
      lyvals_plots[lindex] <- lyvals_plots[lindex-4]
      #print(paste("add fifth point:",lindex))
      #lindex <- lindex+1
  }
}


#rotation matrix
gridazimuthrad <- deg2rad(gridazimuthdeg)

rotmat <- matrix(0.0,nrow=2,ncol=2)
rotmat[1,1] <-  cos(-gridazimuthrad)
rotmat[2,2] <-  cos(-gridazimuthrad)
rotmat[1,2] <- -sin(-gridazimuthrad)
rotmat[2,1] <-  sin(-gridazimuthrad)
#check offsets
myxymat <- cbind(lxvals_plots,lyvals_plots)
myxyrotmat <- myxymat%*%rotmat
myxyrotmat[,1] <- myxyrotmat[,1]+xref-xorig
myxyrotmat[,2] <- myxyrotmat[,2]+yref-yorig





#generate a plot id matrix upper left corner is index row 1 column 1

#the following 200+ lines of code consider the following plot numbering options:
# -whether to number row-wise, ie east to west or west to east
# -which corner to start numbering: northwest, southwest, northeast, southeast
# -whether to number plots in the same direction, e.g. west to east for all rows;
#   or to number in serpentine pattern

if(plotnumorder=="ew") {
  tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=TRUE)
  
  if(startplotnum=="nw") {
    idmatrix <- tmatrix
    #matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=TRUE)
  }
  if(startplotnum=="sw") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=TRUE)
    idmatrix <- apply(tmatrix,2,rev)
  }
  if(startplotnum=="ne") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=TRUE)
    idmatrix <- tmatrix
    for(rct in 1:numplots_vert) {
      idmatrix[rct,] <- rev(tmatrix[rct,])
    }
  }
  if(startplotnum=="se") {
   #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=TRUE)
    tmatrix2 <- apply(tmatrix,2,rev)
    idmatrix <- tmatrix2
    for(rct in 1:numplots_vert) {
      idmatrix[rct,] <- rev(tmatrix2[rct,])
    }
  }
}

if(plotnumorder=="ns") {
  tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=FALSE)
  if(startplotnum=="nw") {
    idmatrix <- tmatrix
    #matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=FALSE)
  }
  if(startplotnum=="sw") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=FALSE)
    idmatrix <- apply(tmatrix,2,rev)
  }
  if(startplotnum=="ne") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=FALSE)
    idmatrix <- tmatrix
    for(rct in 1:numplots_vert) {
      idmatrix[rct,] <- rev(tmatrix[rct,])
    }
  }
  if(startplotnum=="se") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=FALSE)
    tmatrix2 <- apply(tmatrix,2,rev)
    idmatrix <- tmatrix2
    for(rct in 1:numplots_vert){
      idmatrix[rct,] <- rev(tmatrix2[rct,])
    }
    
  }
}
#end of normal dead-head numbering pattern

#begin option for serpentine numbering

if(plotnumorder=="ews") { #serpentine
  rowflag <- rep(0,numplots_vert)
  if(numplots_vert%%2==0) {
    #even number of rows
    rowflag <- rep(c(0,1),numplots_vert/2)
  } else {
    rowflag <- rep(0,numplots_vert)
    myseq <- seq(2,((numplots_vert/2)-1),by=2)
    rowflag[myseq] <- 1
  }
  #loc1 <- which(rowflag==1)
  #numloc1 <- length(loc1)
  
  tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=TRUE)
  
  if(startplotnum=="nw") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=TRUE)
    idmatrix <- tmatrix
    loc1 <- which(rowflag==1)
    numloc1 <- length(loc1)
    for(rct in 1:numloc1) {
      idmatrix[loc1[rct],] <- rev(tmatrix[loc1[rct],])
    }
  }
  
  if(startplotnum=="sw") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=TRUE)
    tmatrix2 <- apply(tmatrix,2,rev)
    idmatrix <- tmatrix2
    rowflag2 <- rev(rowflag)
    loc1 <- which(rowflag2==1)
    numloc1 <- length(loc1) 
    for(rct in 1:numloc1) {
      idmatrix[loc1[rct],] <- rev(tmatrix2[loc1[rct],])
    }
  }
  
  if(startplotnum=="ne") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=TRUE)
    tmatrix2 <- tmatrix
    for(rct in 1:numplots_vert) {
      tmatrix2[rct,] <- rev(tmatrix[rct,])
    }
    idmatrix <- tmatrix2
    loc1 <- which(rowflag==1)
    numloc1 <- length(loc1)
    for(rct in 1:numloc1){
      idmatrix[loc1[rct],] <- rev(tmatrix2[loc1[rct],])
    }
  }
  if(startplotnum=="se") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=TRUE)
    tmatrix2 <- apply(tmatrix,2,rev)
    tmatrix3 <- tmatrix2
    for(rct in 1:numplots_vert) {
      tmatrix3[rct,] <- rev(tmatrix2[rct,])
    }
    idmatrix <- tmatrix3
    rowflag2 <- rev(rowflag)
    loc1 <- which(rowflag2==1)
    numloc1 <- length(loc1)
    for(rct in 1:numloc1) {
      idmatrix[loc1[rct],] <- rev(tmatrix3[loc1[rct],])
    }
  }
}



if(plotnumorder=="nss") { #serpentine pattern column major
  colflag <- rep(0,numplots_horiz)
  if(numplots_horiz%%2==0) {
    #even number of columns
    colflag <- rep(c(0,1),numplots_horiz/2)
  } else {
    colflag <- rep(0,numplots_horiz)
    myseq <- seq(2,((numplots_horiz/2)-1),by=2)
    colflag[myseq] <- 1
  }
  
  
  tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=FALSE)
  if(startplotnum=="nw") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=FALSE)
    idmatrix <- tmatrix
    loc1 <- which(colflag==1)
    numloc1 <- length(loc1)
    for(cct in 1:numloc1) {
      idmatrix[,loc1[cct]] <- rev(tmatrix[,loc1[cct]])
    }
    
  }
  if(startplotnum=="sw") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=FALSE)
    tmatrix2 <- apply(tmatrix,2,rev)
    idmatrix <- tmatrix2
    loc1 <- which(colflag==1)
    numloc1 <- length(loc1)
    for(cct in 1:numloc1) {
      idmatrix[,loc1[cct]] <- rev(tmatrix2[,loc1[cct]])
    }
  }
  
  if(startplotnum=="ne") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=FALSE)
    tmatrix2 <- tmatrix
    for(rct in 1:numplots_vert) {
      tmatrix2[rct,] <- rev(tmatrix[rct,])
    }
    idmatrix <- tmatrix2
    colflag2 <- rev(colflag)
    loc1 <- which(colflag2==1)
    numloc1 <- length(loc1)
    for(cct in 1:numloc1) {
      idmatrix[,loc1[cct]] <- rev(tmatrix2[,loc1[cct]])
    }
  }
  
  if(startplotnum=="se") {
    #tmatrix <- matrix(plotnumvec,nrow=numplots_vert,ncol=numplots_horiz,byrow=FALSE)
    tmatrix2 <- apply(tmatrix,2,rev)
    tmatrix3 <- tmatrix2
    for(rct in 1:numplots_vert){
      tmatrix3[rct,] <- rev(tmatrix2[rct,])
    }
    idmatrix <- tmatrix3
    colflag2 <- ref(colflag)
    loc1 <- which(colflag2==1)
    numloc1 <- length(loc1)
    for(cct in 1:numloc1) {
      idmatrix[,loc1[cct]] <- rev(tmatrix3[,loc1[cct]])
    }
    
  }
}
# need to decompose idmatrix into a vector
 tidmatrix  <- t(idmatrix)
 idmatrix1D <- tidmatrix


dim(idmatrix1D) <- c(numplots_total)


IDvec <- rep(0,numpoints_plots)

#PlotIDvec <- 1:numplots_total
PlotIDvec <- as.vector(idmatrix1D) #be sure to re-cast idmatrix1D as a vector

#assign PlotID values to every vertex
for(pct in 1:numplots_total) {
  lindices <- (pct*5-4):(pct*5)
  #lindices <- (pct*4-3):(pct*4)
  IDvec[lindices] <- as.integer(idmatrix1D[pct])
    #pct
}
#dd is the table containing utm coordinates for every vertex
dd      <- data.frame(Id=IDvec,X=as.vector(myxyrotmat[,1]),Y=as.vector(myxyrotmat[,2]))

#ddTable is the table of attributes
if(use_attribute_file=="yes") {
  ddTable <- attribdat
} else {
  ddTable <- data.frame(Id=PlotIDvec,Name=paste("Plot_",PlotIDvec,sep="")) #generic attributes
}
#ddTable2 <- attribdat

ddShapefile <- convert.to.shapefile(dd,ddTable,"Id",5) #type 5 is polygon

#write the polygon shapefile
write.shapefile(ddShapefile,fofil,arcgis=T)

#make a table for centroids
ddIduniq <- unique(dd$Id)
numddIduniq <- length(ddIduniq)
centroidtable <- matrix(0.0,nrow=numddIduniq,ncol=2)
for(ict in 1:numddIduniq) {
  curId <- ddIduniq[ict]
  locid <- which(dd$Id==curId)
  lxvals <- dd$X[locid]
  lyvals <- dd$Y[locid]
  
  centroidtable[ict,1] <- mean(lxvals[1:4])
  centroidtable[ict,2] <- mean(lyvals[1:4])
  
}
ddcentroidTable <- data.frame(Id=ddIduniq,X=centroidtable[,1],y=centroidtable[,2])
ddcentroidShapefil <- convert.to.shapefile(ddcentroidTable,ddTable,"Id",1)

#write the centroid shapefile
write.shapefile(ddcentroidShapefil,focentroidfil,arcgis=T)

# create the prj string
#Northern hemisphere is default
if(hemisphere=="S") {
 prjstring <- paste('PROJCS["WGS_1984_UTM_Zone_',utmzonestr2,'S",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",500000.0],PARAMETER["False_Northing",10000000.0],PARAMETER["Central_Meridian",',centralmeridstr,'],PARAMETER["Scale_Factor",0.9996],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]',sep="")  
} else {
 prjstring <- paste('PROJCS["WGS_1984_UTM_Zone_',utmzonestr2,'N",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",',centralmeridstr,'],PARAMETER["Scale_Factor",0.9996],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]',sep="")
}

proj4str <- CRS("+proj=utm +zone=12 +ellps=WGS84")

#write the prj component of the polygon shapefile
focon <- file(fofilprj,'w')
write(prjstring,focon,append=FALSE)
close(focon)

#write the prj component of the centroid shapefile
focon2 <- file(focentroidfilprj,'w')
write(prjstring,focon2,append=FALSE)
close(focon2)

#create and write output parm information file
focon3 <- file(foparmfil,'w')
parmhdr <- paste("parameter:value")
write(parmhdr,focon3,append=FALSE)
workdir <- paste("working_directory",ldir,sep=":")
write(workdir,focon3,append=TRUE)
outpolyfil <- paste("output_polygon_shapefile",ofil,sep=":")
write(outpolyfil,focon3,append=TRUE)
outcentroidfil <- paste("output_point_shapefile",ocentroidfil,sep=":")
write(outcentroidfil,focon3,append=TRUE)
utmgridrotationangle <- paste("utm_grid_rotation_degrees",gridazimuthdeg,sep=":")
write(utmgridrotationangle,focon3,append=TRUE)
utmegridrotationcenter <- paste("grid_rotation_point_UTME",rotationcenter_x,sep=":")
write(utmegridrotationcenter,focon3,append=TRUE)
utmngridrotationcenter <- paste("grid_rotation_point_UTMN",rotationcenter_y,sep=":")
write(utmngridrotationcenter,focon3,append=TRUE)
hemilbl <- paste("Hemisphere",hemisphere,sep=":")
write(hemilbl,focon3,append=TRUE)
uzone <- paste("UTM_Zone",utmzone,sep=":")
write(uzone,focon3,append=TRUE)
x0 <- paste("UTME_NWcorner",xref0,sep=":")
write(x0,focon3,append=TRUE)
y0 <- paste("UTMN_NWcorner",yref0,sep=":")
write(y0,focon3,append=TRUE)
plotxdim <- paste("PlotWidthMeters",pltxdim,sep=":")
write(plotxdim,focon3,append=TRUE)
plotydim <- paste("PlotHeightMeters",pltydim,sep=":")
write(plotydim,focon3,append=TRUE)
buffX_x <- paste("EWBuffer_widthMeters",buffX_xdim,sep=":")
write(buffX_x,focon3,append=TRUE)
buffX_y <- paste("EWBuffer_heightMeters",buffX_ydim,sep=":")
write(buffX_y,focon3,append=TRUE)
buffY_x <- paste("NSBuffer_widthMeters",buffY_xdim,sep=":")
write(buffY_x,focon3,append=TRUE)
buffY_y <- paste("NSbuffer_heightMeters",buffY_ydim,sep=":")
write(buffY_y,focon3,append=TRUE)
numplotx <- paste("NumberofPlotsEW",numplots_horiz,sep=":")
write(numplotx,focon3,append=TRUE)
numploty <- paste("NumberofPlotsNS",numplots_vert,sep=":")
write(numploty,focon3,append=TRUE)
idnumstrt <- paste("IDNumberStart",idnumstart,sep=":")
write(idnumstrt,focon3,append=TRUE)
idnumi <- paste("IDNumberIncrement",idnumincr,sep=":")
write(idnumi,focon3,append=TRUE)
plotnumor <- paste("PlotNumberingOrder",plotnumorder,sep=":")
write(plotnumor,focon3,append=TRUE)
strtplotpos <- paste("StartPlotPosition",startplotnum,sep=":")
write(strtplotpos,focon3,append=TRUE)
uattrib <- paste("Using_Attribute_File",use_attribute_file,sep=":")
write(uattrib,focon3,append=TRUE)
attfil <- paste("Attribute_File_Name",attribfil,sep=":")
write(attfil,focon3,append=TRUE)
close(focon3)
#DONE write parm information file

pstr <- paste("wrote shapefiles:",fofil)
print(pstr)
cpstr <- paste("wrote centroid files:",focentroidfil)
print(cpstr)
ppstr <- paste("wrote parm file:",foparmfil)
print(ppstr)

#setwd(homedir)