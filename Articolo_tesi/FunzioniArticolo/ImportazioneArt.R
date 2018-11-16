## Proviene da seconda importazione tesi

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
df.masse <-
    read.table(file.path(DirData, "corrispondenze_PLFA.csv" ),
               sep= ";", dec=",", header=TRUE,
               fileEncoding = "UTF-8",
               colClasses=c(rep("character",4), "numeric"))[,4:5]
df.masse$NOME.R.nuovo <-
    paste("C", df.masse$NOME.R.nuovo, sep=".")


lis.tutto <-
    ## si crea una lista vuota dove inserire i dati in varia declinazione
    ## (moli conc area etc)
    list(CATEGORIA= data.frame(),
         PLFA=list(),
         MICROBI=list(),
         SOMME=data.frame()
         )
flagAREA <- ## serve per importare AREE e concentrazioni; turnaround
    ## da programma pregersso
    c(TRUE, FALSE)
for(j in 1:2){## j è l'indice per AREA e CONCENTRAZIONE
    AREA <- flagAREA[j]
    for (i in 1:2){ ## i = indice per AUTUNNO ESTATE
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
            df.autunno$SEASON <- "Fa"
        }else{
            df.data$SEASON <- "Su"
            df.data <-
                rbind.data.frame(df.autunno, df.data)
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
                  to = c("plw", "chp","dsh")
                  )
    df.data$MAN <-
      mapvalues(df.data$MAN,
                from = c("CO", "OO", "MS", "ST", "BI"),
                to = c("Co", "Or", "MS", "ST", "BI")
                )
    df.data$SEASON <-
        factor(df.data$SEASON, levels= c("Fa", "Su"))
    df.data$Acq.Date.Time <-
        strptime(as.character(df.data$Acq.Date.Time), "%d/%m/%Y %R")
    df.data <- ## riorganizza le colonne
        df.data[ ,c(1:7, 46:51, 8:45)]
    i.PLFA <- 14:51
    names(df.data)[i.PLFA] <- df.composti$NOME.R.nuovo
    ## turnaround per eliminare delle colonne inavvertitamente
    ## sfuggite alla prima analisi 15 gen 2018
    togli.questi <-
        ## data 15 gen 2018 unk126 è un inquinante e "a-C14:0" è una
        ## colonna di zeri
        c(15,36)
    df.data <- ## riorganizza le colonne
        df.data[ ,-togli.questi]
    rm(togli.questi)
    i.PLFA <- 14:49
    ## Qui giunti è opportuno togliere le righe/campioni doppi o sospetti
    doppioni.peggio <-
        with(df.data,
             which(Data.File %in%
                   c("CO9C1M FORSE.D",
                     "OO2B1 M.D",
                     "CO9C1MR.D",
                     "9-9A2 COLONNA TAGLIATA.D",
                     "10-2C2 COLONNA TAGLIATA.D",
                     "19-10B3 BIS COLONNA TAGLIATA.D")
                   )
             )
    df.data <- df.data[-doppioni.peggio, ]
    rm(doppioni.peggio)
    lis.data <- ## si separano i campioni dagli altri; claibraz e MIX
        split(df.data, df.data$Type)
    ## Pettinatura categorie
    lis.data$Sample <-
        lis.data$Sample[lis.data$Sample$MAN %in% c( "Co", "Or" ),]
    lis.data$Sample$MAN <- lis.data$Sample$MAN[drop=TRUE]
    lis.data$Sample$TIL <- lis.data$Sample$TIL[drop=TRUE]
    lis.data$Sample$FIELD <- lis.data$Sample$FIELD[drop=TRUE]
    names(lis.data$Sample)[i.PLFA] <-
        paste("C", names(lis.data$Sample)[i.PLFA], sep=".")
    lis.tutto$CATEGORIA <- lis.data$Sample[,1:13]
    if(AREA){
        df.data$SOMMA.AREE <-
            apply(df.data[,i.PLFA], 1,
                  function(x) sum(x, na.rm=TRUE))
        lis.tutto$PLFA$AREA <- lis.data$Sample[,-(1:13)]
    }else{
        df.data$SOMMA.CONC <-
            apply(df.data[,i.PLFA], 1,
                  function(x) sum(x, na.rm=TRUE))
        lis.tutto$PLFA$CONC.ng.ul <-  lis.data$Sample[,-(1:13)]
    }
    nomefile <-
        ifelse(AREA, "TuttiDatiNUOVI_AREA.csv", "TuttiDatiNUOVI_CONC.csv")
    write.table(df.data, file=file.path(DirElab, nomefile),
                sep= ";", dec=",",
                fileEncoding = "UTF-8",
                col.names = NA)
}
## Pulizia dataregion
rm(ordina);rm(nome.file.da.importare);rm(nomefile);rm(flagAREA)
rm(vec.nomi.colonne);rm(AREA)

##############################
## CALCOLO delle nanoMOLI
##############################
lis.tutto$PLFA$nMOLI <-
    lis.tutto$PLFA$AREA
PLFA.non.validi <- c(2,23)
for(i in 1:dim(lis.tutto$PLFA$CONC.ng.ul)[2]){# 200 sono i ul per riprendere estratto che viene da 1gr di suolo
    lis.tutto$PLFA$nMOLI[,i] <-
        200*lis.tutto$PLFA$CONC.ng.ul[,i]/df.masse$PM[-PLFA.non.validi][i]
}
####################################
## CALCOLO delle colonne delle somme
####################################

lis.tutto$SOMME <- lis.tutto$PLFA$AREA[,1:3]
for(i in 1:3){
    lis.tutto$SOMME[,i] <-
        apply(lis.tutto[[2]][[i]], 1, sum)
}
names(lis.tutto$SOMME) <-
    names(lis.tutto$PLFA)

#####################################################################
## Da qui in poi si selezionana le colonne tipiche dei MICROBI Non
## sapendo quale sia il miglior parametro si cerca la media e la
## mediana di ogni PLFA (colonna) e poi si prende sia il massimo che
## il minimo
##
## liste necessarie;
##
## lis.MediaMedianaMaxMin: un ramo per ogni tipo di microbo con dentro
## un vettore coi nomi dei PLFA (maxmeanmamedianxmin....min). Le
## micorrize hanno un solo PLFA caratteristico
##
##lis.PLFA.microbi: un ramo per ogni tipo di microbo
#####################################################################

source(file.path(DirFunz,"righe_nulleArt.R"))

lis.MediaMedianaMaxMin <-
    list(list())

lis.PLFA.microbi <- ## lista con i nomi dei microbi
    list(gramN=c(),
         gramP=c(),
         actino=c(),
         fungi=c(),
         protozoi=c(),
         micorrize=c())

for (i in 1:6){
    lis.PLFA.microbi[[i]] <-
        as.vector(df.gruppiMicrobici[,1][df.gruppiMicrobici[,i+1]==1])
}
lis.nomi.PLFA.microbi <-
    lis.PLFA.microbi

questo.parametro <-
    c("mean", "median")
massimominimo <-
    c("max", "min")
for(w in 1:length(lis.tutto$PLFA)){## w indice per AREA, CONC, MOLI
    for (i in 1:5){## i indice per i gruppi microbici
        vec.composti <-
            as.vector(df.gruppiMicrobici[,1][df.gruppiMicrobici[,i+1]==1])
        acchiappa <-
            which(names(lis.tutto$PLFA[[w]]) %in% vec.composti)
        df.butta <- t(lis.tutto$PLFA[[w]][ ,acchiappa])
        mtx.MeanMedian <-
            matrix(ncol=length(questo.parametro),
                   nrow=dim(df.butta)[1],
                   dimnames=list(row.names(df.butta),
                                 questo.parametro))
        for (k in 1:2){## k indice per mediamediana
            mtx.MeanMedian[,k] <-
                apply(df.butta, 1, get(questo.parametro[k]))
        }
        vec.risultati <- c()
        for (j in 1:length(massimominimo)){## j indice per maxmin medie
            vec.risultati[j] <-
                row.names(mtx.MeanMedian)[which(mtx.MeanMedian[,j]==
                                                max(mtx.MeanMedian[,j]))]
            names(vec.risultati)[j] <-
                paste(massimominimo[1],dimnames(mtx.MeanMedian)[[2]][j], sep=".")
        }
        for (j in 3:4){## j indice per maxmin mediane
            vec.risultati[j] <-
                row.names(mtx.MeanMedian)[which(mtx.MeanMedian[,j-2]==
                                                min(mtx.MeanMedian[,j-2]))]
            names(vec.risultati)[j] <-
                paste(massimominimo[2],dimnames(mtx.MeanMedian)[[2]][j-2], sep=".")
        }
        lis.PLFA.microbi[[i]] <-
            vec.risultati
    }
    lis.PLFA.microbi[[6]] <- rep("C.00LinC16.1.omega5", j)
    lis.MediaMedianaMaxMin[[w]] <- lis.PLFA.microbi
}
names(lis.MediaMedianaMaxMin) <-
    names(lis.tutto$PLFA)

## A questo punto abbiamo lis.MediaMedianaMaxMin con dentro AREA CONC
## e MOLI con dentro ancora microbi con dentro maxminmedmediana

## la lis.nonloso conterrà un dataframe con gli opportuni PLFA
## (maxminmediamedian)
## Ogni lis.nonloso vienne appesa alla lis.tutto$MICROBI

for(k in 1:3){## k indice per AREA CONC e MOLI
    lis.nonloso <-
        list()
    for(i in 1:4){ ## i indice per maxminmediamediana
        PLFA.selezionati <- ## pesca i nomi dalla lista
            unlist(lapply(lis.MediaMedianaMaxMin[[k]], function(x) x[i]))
        df.selezionati <- ## crea un df
            lis.tutto$PLFA[[k]][,PLFA.selezionati]
        names(df.selezionati) <-
            paste(substr(names(PLFA.selezionati),1,5),
                  names(df.selezionati))
        lis.nonloso[[i]] <- ## fa la somma delle righe
            cbind.data.frame(df.selezionati,
                             SOMMA = apply(df.selezionati,2,sum)
                             )
        names(lis.nonloso)[i] <-
            substr(names(PLFA.selezionati)[1], 7,16)
    }
    lis.tutto$MICROBI[[k]] <- ## si attacca la lis.nonloso al punto opportuno
        lis.nonloso
}
names(lis.tutto$MICROBI) <-
    names(lis.tutto$PLFA)


#################################
## Fine Importazione e pettinatura
#################################


################### AVANZI ##########################
## table(df.data$MAN, df.data$TIL)


## require(lattice)

## raggruppa <-
##     with(lis.data$Sample, interaction(TIL, MAN, STAGIONE))##,EXTRACT,INJECTION))

## pdf(file.path(DirGraf, "secondo tentativo.pdf"))
## for (i in c(i.PLFA, 50)){ #52 vecchio per le aree
##     print(
##         dotplot(raggruppa ~ lis.data$Sample[,i],
##                 data = lis.data$Sample,
##                 groups = raggruppa,
##                 xlab = names(lis.data$Sample)[i],
##                 key = simpleKey(levels(lis.data$Sample$MAN), space = "right"),
##                 aspect = 0.8,
##                 layout=c(1,1),
##                 ylab=NULL, scale="free", pch=4, cex=2
##                 )
##     )
##     print(
##         bwplot(raggruppa ~ lis.data$Sample[,i],
##                data = lis.data$Sample,
##                aspect = 0.8,
##                xlab = names(lis.data$Sample)[i],
##                ylab = NULL,
##                scale = "free", col=1:2,
##                )
##     )
## }
## dev.off()
