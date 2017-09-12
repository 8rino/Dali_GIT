### R code from vignette source 'Tesi_GITDalila.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: PacchettiRichiesti
###################################################
library(nlme)
library(plyr)
library(lattice)
library(xtable)
library(foreign)
library(agricolae)
library(multcomp)


###################################################
### code chunk number 2: setupDir
###################################################
info.sistema <-
Sys.info()[c(1,7)]
if(info.sistema[1]=="Windows"){
  DirMain <- paste("C:/Users/", info.sistema[2], "/Dropbox/MOLTE_R", sep="")
} else {
  if(info.sistema[2]=="simone" ){
    DirMain <-"~/Simo_GIT"
  }else{
    DirMain <-"~/Documenti/BitBucket/Simo_GIT"
  }
}
DirData <-
file.path(DirMain,"dati_grezzi")
DirElab <-
file.path(DirMain,"dati_elaborati")
DirGraf <-
file.path(DirMain, "grafici")
DirFunz <-
file.path(DirMain, "funzioni")
DirCod <-
file.path(DirMain, "codice")
DirTab <-
file.path(DirMain, "tabelle")


