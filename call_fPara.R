#Basic Settings
#-----------------------------------------------------------------------------------------------------
W.DIR = "...."
FUNC.DIR = "_functions/"

#-----------------------------------------------------------------------------------------------------
print("Import functions")
#-----------------------------------------------------------------------------------------------------
source(file.path(W.DIR,FUNC.DIR,"fPackages.R"))
source(file.path(W.DIR,FUNC.DIR,"fPara.R"))

#-----------------------------------------------------------------------------------------------------
print("Parameters")
#-----------------------------------------------------------------------------------------------------
HR <- "DESTLI0503850019_RADOLANGT10MM_buffer5000.csv"
PH <- "ST_LO_epsg31468_DOY-HS_agg_215.csv"
SC <- "SC215.csv"
PLANT = 215
OUT.DIR = "_output/"
IN.DIR = "_input/"

fPara(W.DIR,
      IN.DIR,
      OUT.DIR,
      PLANT,
      HR,
      PH,
      SC)
