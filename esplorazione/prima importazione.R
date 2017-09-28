info.sistema <-
    Sys.info()[c(1,7)]
if(info.sistema[1]=="Windows"){
    DirMain <- paste("C:/Users/", info.sistema[2], "/Dropbox/MOLTE_R", sep="")
} else {
    if(info.sistema[2]=="dalila" ){
        DirMain <-"~/Dali_GIT"
    }else{
        DirMain <-"~/Documenti/BitBucket/Dalila_GIT"
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
## Dati veri e propri
df.data <-
    read.table(file.path(DirData, "batch1ottobre.csv" ),
               sep= ";", dec=",",
               skip=2, fileEncoding = "UTF-8")
## Corrispondenze tra massa e programmazione
df.corrispondenze <-
    read.table(file.path(DirData, "corrispondenze_nomi.csv" ),
               sep= ";", dec=",", header=TRUE,
               fileEncoding = "UTF-8",
               colClasses="character")
## Ginnastica per importare dati di massa
vec.nomi.colonne <-
    readLines(file.path(DirData, "batch1ottobre.csv" ),n=2)
vec.nomi.colonne1 <-
    unlist(strsplit(vec.nomi.colonne[1], "[;]"))
vec.nomi.colonne1[-1] <-
    substr(vec.nomi.colonne1[-1],
           1,
           nchar(vec.nomi.colonne1[-1])-
           nchar("Results")-1
           )
vec.nomi.colonne2 <-
    unlist(strsplit(vec.nomi.colonne[2], "[;]"))

vec.nomi <-
    c(vec.nomi.colonne2[1:7],
      vec.nomi.colonne1[-(1:7)]
      )

names(df.data) <- vec.nomi
names(df.data)[c(1:2,4,7)] <-
    c(paste("MH", 1:2, sep="."),
      "DATA.FILE","INJ.DATE")



df.data$INJ.DATE <-
    strptime(as.character(df.data$INJ.DATE), "%d/%m/%Y %R")
                                        #df.data <- df.data[-20,-(1:2)]


require(plyr)
df.data$STRINGA <-
    as.character(mapvalues(df.data$DATA.FILE,
                           from = as.character(unique(df.data$DATA.FILE)),
                           to = df.corrispondenze$CAMPIONI.R
                           )
                 )
df.data <- df.data[-14,]
df.data$MAN <-
    factor(
        substr(df.data$STRINGA,1,2),
        levels = c("CO", "OO")
    )
df.data$FIELD <-
    factor(
        substr(df.data$STRINGA,3,4)
    )
df.data$TIL <-
    factor(
        substr(df.data$STRINGA,5,5)
    )
df.data$TIL <-
    mapvalues(df.data$TIL,
              from = LETTERS[1:3],
              to = c("Ara", "Rip","Fzo")
              )
df.data$PARC <-
    factor(
        substr(df.data$STRINGA,6,6)
    )

df.data <- df.data[,c(1:7,43:47, 8:42)]

write.table(df.data, file=file.path(DirElab, "ottobre.csv"),
               sep= ";", dec=",",
               fileEncoding = "UTF-8",
               col.names = NA)
