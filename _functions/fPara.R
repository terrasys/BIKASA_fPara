fPara <- function(W.DIR,
                  IN.DIR,
                  OUT.DIR,
                  PLANT,
                  HR,
                  PH,
                  SC){

if(PLANT == 202){
#Import
p <- read.csv2(file.path(W.DIR,IN.DIR,PH)) 
r <- read.csv2(file.path(W.DIR,IN.DIR,HR))
c <- read.csv2(file.path(W.DIR,IN.DIR,SC))  
#Classification of phenological phases

p
r$PHASE <- NA
r$PHASE <- ifelse(r$DOY<=p[which(p$PHASE==15),]$DOY,
                      12,
                      r$PHASE)
r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==15),]$DOY) & r$DOY<=p[which(p$PHASE==18),]$DOY,
                      15,
                      r$PHASE)
r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==18),]$DOY) & r$DOY<=p[which(p$PHASE==19),]$DOY,
                      18,
                      r$PHASE)
r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==19),]$DOY) & r$DOY<=p[which(p$PHASE==21),]$DOY,
                  19,
                  r$PHASE)
r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==21),]$DOY) & r$DOY<=p[which(p$PHASE==24),]$DOY,
                      21,
                      r$PHASE)
r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==24),]$DOY) & r$DOY<=p[which(p$PHASE==10),]$DOY,
                      24,
                      r$PHASE)
r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==10),]$DOY) & r$DOY<=p[which(p$PHASE==12),]$DOY,
                      10,
                      r$PHASE)
r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==12),]$DOY),
                      12,
                      r$PHASE)
#Calculating negative DOYs in precipitaion file
p$DOY2 <- p$DOY
p[which(p$PHASE==10),]$DOY2 <- p[which(p$PHASE==10),]$DOY-365 
p[which(p$PHASE==12),]$DOY2 <- p[which(p$PHASE==12),]$DOY-365
#Merging precipitaion file and soil cover information
r <- merge(r,c,by="PHASE")
r$SC <- r$SC/100
#Calculation of vegetation period's DOYs
r$DOY2 <- r$DOY
r[which(r$PHASE==10),]$DOY2 <- r[which(r$PHASE==10),]$DOY-365 
r[which(r$DOY>=p[which(p$PHASE==12),]$DOY),]$DOY2 <- r[which(r$DOY>=p[which(p$PHASE==12),]$DOY),]$DOY-365
#Order according to new DOY values 
r <- r[order(r$DOY2),]
names(r)[names(r) == 'DOY'] <- 'DOY1'
names(r)[names(r) == 'SC'] <- paste("SC",PLANT,sep="")
#Export
write.csv2(r,paste(W.DIR,OUT.DIR,substr(HR,1,nchar(HR)-4),"_",PLANT,".csv",sep=""))
}

if(PLANT == 205){
  #Import
  p <- read.csv2(file.path(W.DIR,IN.DIR,PH)) 
  r <- read.csv2(file.path(W.DIR,IN.DIR,HR))
  c <- read.csv2(file.path(W.DIR,IN.DIR,SC))  
  #Classification of phenological phases
  r$PHASE <- NA
  r$PHASE <- ifelse(r$DOY<=p[which(p$PHASE==67),]$DOY,
                    14,
                    r$PHASE)
  r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==67),]$DOY) & r$DOY<=p[which(p$PHASE==17),]$DOY,
                    67,
                    r$PHASE)
  r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==17),]$DOY) & r$DOY<=p[which(p$PHASE==5),]$DOY,
                    17,
                    r$PHASE)
  r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==5),]$DOY) & r$DOY<=p[which(p$PHASE==22),]$DOY,
                    5,
                    r$PHASE)
  r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==22),]$DOY) & r$DOY<=p[which(p$PHASE==24),]$DOY,
                    22,
                    r$PHASE)
  r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==24),]$DOY) & r$DOY<=p[which(p$PHASE==10),]$DOY,
                    24,
                    r$PHASE)
  r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==10),]$DOY) & r$DOY<=p[which(p$PHASE==12),]$DOY,
                    10,
                    r$PHASE)
  r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==12),]$DOY) & r$DOY<=p[which(p$PHASE==14),]$DOY,
                    12,
                    r$PHASE)
  r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==14),]$DOY),
                    14,
                    r$PHASE)
  #Calculating negative DOYs in precipitaion file
  p$DOY2 <- p$DOY
  p[which(p$PHASE==10),]$DOY2 <- p[which(p$PHASE==10),]$DOY-365 
  p[which(p$PHASE==12),]$DOY2 <- p[which(p$PHASE==12),]$DOY-365
  p[which(p$PHASE==14),]$DOY2 <- p[which(p$PHASE==14),]$DOY-365
  #Merging precipitaion file and soil cover information
  r <- merge(r,c,by="PHASE")
  r$SC <- r$SC/100
  #Calculation of vegetation period's DOYs
  r$DOY2 <- r$DOY
  r[which(r$PHASE==10),]$DOY2 <- r[which(r$PHASE==10),]$DOY-365 
  r[which(r$DOY>=p[which(p$PHASE==12),]$DOY),]$DOY2 <- r[which(r$DOY>=p[which(p$PHASE==12),]$DOY),]$DOY-365
  r[which(r$DOY>=p[which(p$PHASE==14),]$DOY),]$DOY2 <- r[which(r$DOY>=p[which(p$PHASE==14),]$DOY),]$DOY-365
  #Order according to new DOY values 
  r <- r[order(r$DOY2),]
  names(r)[names(r) == 'DOY'] <- 'DOY1'
  names(r)[names(r) == 'SC'] <- paste("SC",PLANT,sep="")
  #Export
  setwd(file.path(W.DIR,OUT.DIR))
  #Export
  write.csv2(r,paste(substr(HR,1,nchar(HR)-4),"_",PLANT,".csv",sep=""))
}
  
if(PLANT == 215){
    #Import
    p <- read.csv2(file.path(W.DIR,IN.DIR,PH)) 
    r <- read.csv2(file.path(W.DIR,IN.DIR,HR))
    c <- read.csv2(file.path(W.DIR,IN.DIR,SC))  
    #Classification of phenological phases
    r$PHASE <- NA
    r$PHASE <- ifelse(r$DOY<=p[which(p$PHASE==10),]$DOY,
                      0,
                      r$PHASE)
    r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==10),]$DOY) & r$DOY<=p[which(p$PHASE==12),]$DOY,
                      10,
                      r$PHASE)
    r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==12),]$DOY) & r$DOY<=p[which(p$PHASE==67),]$DOY,
                      12,
                      r$PHASE)
    r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==67),]$DOY) & r$DOY<=p[which(p$PHASE==65),]$DOY,
                      67,
                      r$PHASE)
    r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==65),]$DOY) & r$DOY<=p[which(p$PHASE==5),]$DOY,
                      65,
                      r$PHASE)
    r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==5),]$DOY) & r$DOY<=p[which(p$PHASE==19),]$DOY,
                      5,
                      r$PHASE)
    r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==19),]$DOY) & r$DOY<=p[which(p$PHASE==20),]$DOY,
                      19,
                      r$PHASE)
    r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==20),]$DOY) & r$DOY<=p[which(p$PHASE==21),]$DOY,
                      20,
                      r$PHASE)
    r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==21),]$DOY) & r$DOY<=p[which(p$PHASE==24),]$DOY,
                      21,
                      r$PHASE)
    r$PHASE <- ifelse((r$DOY>=p[which(p$PHASE==24),]$DOY),
                      24,
                      r$PHASE)
    #Merging precipitaion file and soil cover information
    r <- merge(r,c,by="PHASE")
    r$SC <- r$SC/100
    names(r)[names(r) == 'SC'] <- paste("SC",PLANT,sep="")
    #Export
    setwd(file.path(W.DIR,OUT.DIR))
    #Export
    write.csv2(r,paste(substr(HR,1,nchar(HR)-4),"_",PLANT,".csv",sep=""))
  }  
}


