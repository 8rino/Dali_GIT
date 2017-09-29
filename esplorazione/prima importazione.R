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
df.MOLTE <-
    read.table(file.path(DirData, "corrispondenze_MOLTE.csv" ),
               sep= ";", dec=",", header=TRUE,
               fileEncoding = "UTF-8",
               colClasses="character")
df.composti <-
    read.table(file.path(DirData, "corrispondenze_PLFA.csv" ),
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
                           to = df.MOLTE$CAMPIONI.R
                           )
                 )
                                        #df.data <- df.data[-14,] ## Per ora si toglie
df.data$MAN <-
    factor(
        substr(df.data$STRINGA,1,2),
        levels = c("CO", "OO", "MS")
    )
df.data$FIELD <-
    factor(
        substr(df.data$STRINGA,3,4)
    )
df.data$TIL <-
    factor(
        substr(df.data$STRINGA,5,5),
        levels = c("A", "B", "C", "D")
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

df.data <-
    df.data[,c(1:7,43:47, 8:42)]

names(df.data)[13:47]<- df.composti$NOME.R

write.table(df.data, file=file.path(DirElab, "ottobre.csv"),
            sep= ";", dec=",",
            fileEncoding = "UTF-8",
            col.names = NA)

#################################
## Fine Importazione dati autunno
#################################

table(df.data$MAN, df.data$TIL)
i.PLFA <- 13:47

lis.data <- split(df.data, df.data$Type)
require(lattice)

raggruppa <-
    with(lis.data[[2]], interaction(MAN,TIL))##,EXTRACT,INJECTION))

pdf(file.path(DirGraf, "primo tentativo.pdf"))
for (i in i.PLFA){
    print(
        dotplot(TIL ~ lis.data[[2]][,i],
                data = lis.data[[2]],
                groups = lis.data$Sample$MAN,
                xlab = names(lis.data[[2]])[i],
                key = simpleKey(levels(lis.data$Sample$MAN), space = "right"),
                aspect = 0.8,
                layout=c(1,1),
                ylab=NULL, scale="free", pch=4, cex=2
                )
    )
    print(
        bwplot(raggruppa ~ lis.data[[2]][,i],
               data = lis.data[[2]],
               aspect = 0.8,
               xlab = names(lis.data[[2]])[i],
               ylab = NULL,
               scale = "free", col=1:2,
               )
    )
}
dev.off()
