info.sistema <-
    Sys.info()[c(1,7)]
if(info.sistema[1]=="Windows"){
    DirMain <-
        paste("C:/Users/",
              info.sistema[2],
              "/Documents/GitHub/Dali_GIT", sep="")
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
require(plyr)
## Corrispondenze tra massa e programmazione
df.ottobre <-
    read.table(file.path(DirData, "corrispondenzeCampioniOttobre.csv" ),
               sep= ";", dec=",", header=TRUE,
               fileEncoding = "UTF-8",
               colClasses="character")[,3:4]
df.giugno <-
    read.table(file.path(DirData, "corrispondenzeCampioniGiugno.csv" ),
               sep= ";", dec=",", header=TRUE,
               fileEncoding = "UTF-8",
               colClasses="character")[,3:4]
df.MOLTE <- rbind.data.frame(df.ottobre, df.giugno)
df.composti <-
    read.table(file.path(DirData, "corrispondenze_PLFA.csv" ),
               sep= ";", dec=",", header=TRUE,
               fileEncoding = "UTF-8",
               colClasses="character")[,3:4]

for (i in 1:2){
    nome.file.da.importare <-
        if (AREA){
          c("Ott_elaboratoAREA.csv","Giu_elaboratoAREA.csv")[i]
        }else{
           c("Ott_elaboratoCONCbis.csv","Giu_elaboratoCONCbis.csv")[i]
        }
    ## Dati veri e propri
    df.data <-
        read.table(file.path(DirElab, nome.file.da.importare ),
                   sep= ";", dec=",",skip=1)
    ## Ginnastica per importare dati di massa
    vec.nomi.colonne <-
        readLines(file.path(DirElab,
                            nome.file.da.importare ), n=1)
    vec.nomi.colonne <-
        unlist(strsplit(vec.nomi.colonne[1], "[;]"))
     ##
    names(df.data) <- vec.nomi.colonne
    if(i==1){
        df.autunno <- df.data
        df.autunno$STAGIONE <- "Aut"
    }else{
        df.data$STAGIONE <- "Est"
        df.data <- rbind.data.frame(df.autunno, df.data)
        rm(df.autunno)
    }
}

names(df.data)[c(1,2, 4,7)] <-
    c("MH1", "MH2", "Data.File", "Acq.Date.Time")

## Pericoloso modo di agire, ma necessario
## Attenzione all'INDISPENSABILE sort !!!\


ordina <-
    order(as.character(df.MOLTE$CAMPIONI.MASSHUNTER))
df.MOLTE <- df.MOLTE[ordina,]
df.data$STRINGA <-
    as.character(
        mapvalues(df.data$Data.File,
                  from =
                      sort(as.character(df.data$Data.File)),
                  to = df.MOLTE$CAMPIONI.R)
    )

df.data$MAN <-
    factor(
        substr(df.data$STRINGA,1,2),
        levels = c("CO", "OO", "MS", "ST", "BI")
    )

df.data$FIELD <-
    factor(
        substr(df.data$STRINGA,3,4)
    )
df.data$TIL <-
    factor(
        substr(df.data$STRINGA,5,5),
        levels = c("A", "B", "C", "D", "N")
    )
df.data$PARC <-
    factor(
        substr(df.data$STRINGA,6,6),
        levels = c("1", "2", "3", "0", "A")
    )
##
df.data[df.data$Type=="Cal", 47:50] <- NA
df.data$TIL <-
    mapvalues(df.data$TIL,
              from = LETTERS[1:3],
              to = c("Ara", "Rip","Fzo")
              )
df.data$STAGIONE <-
    factor(df.data$STAGIONE, levels= c("Aut", "Est"))
df.data$Acq.Date.Time <-
    strptime(as.character(df.data$Acq.Date.Time), "%d/%m/%Y %R")
## df.data <- df.data[order(df.data$INJ.DATE),]


df.data <-
    df.data[ ,c(1:7, 46:51, 8:45)]
i.PLFA <- 14:51

##cbind(names(df.data)[i.PLFA], df.composti$NOME.R)

names(df.data)[i.PLFA] <- df.composti$NOME.R.nuovo

df.data$SOMMA.AREE <-
    apply(df.data[,i.PLFA], 1,
          function(x) sum(x, na.rm=TRUE))
##table(df.data$MAN, df.data$TIL)
##


write.table(df.data, file=file.path(DirElab, "TuttiDatiNUOVI.csv"),
            sep= ";", dec=",",
            fileEncoding = "UTF-8",
            col.names = NA)

#################################
## Fine Importazione dati autunno
#################################

table(df.data$MAN, df.data$TIL)

lis.data <-
    split(df.data, df.data$Type)
require(lattice)

raggruppa <-
    with(lis.data$Sample, interaction(TIL, MAN, STAGIONE))##,EXTRACT,INJECTION))

pdf(file.path(DirGraf, "secondo tentativo.pdf"))
for (i in c(i.PLFA, 52)){ #52 per le aree
    print(
        dotplot(raggruppa ~ lis.data$Sample[,i],
                data = lis.data$Sample,
                groups = raggruppa,
                xlab = names(lis.data$Sample)[i],
                key = simpleKey(levels(lis.data$Sample$MAN), space = "right"),
                aspect = 0.8,
                layout=c(1,1),
                ylab=NULL, scale="free", pch=4, cex=2
                )
    )
    print(
        bwplot(raggruppa ~ lis.data$Sample[,i],
               data = lis.data$Sample,
               aspect = 0.8,
               xlab = names(lis.data$Sample)[i],
               ylab = NULL,
               scale = "free", col=1:2,
               )
    )
}
dev.off()

