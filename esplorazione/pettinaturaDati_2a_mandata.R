## mtx.nomi.colonne <- matrix(ncol=1, nrow3)

## nome.file.da.importare <-
##     c("exportOttobre_batchCONC.csv",
##       "exportAvanzi_batchCONC.csv",
##       "exportGiugno_batchCONC.csv")
## for(i in 1:3){
## mtx.nomi.colonne[i, ] <-
##     readLines(file.path(DirData,nome.file.da.importare[1]),n=1)
## }

## mtx.nomi.colonne[1,]==mtx.nomi.colonne[2,]|mtx.nomi.colonne[1,]==mtx.nomi.colonne[3,]
info.sistema <-
    Sys.info()[c(1,7)]
if(info.sistema[1]=="Windows"){
    DirMain <-
        paste("C:/Users/",
              info.sistema[2],
              "/Dropbox/MOLTE_R", sep="")
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

df.ott <-
        read.table(file.path(DirData, "exportOttobre_batchAREA.csv" ),
                   sep= ",",
                   header=FALSE, skip=2)[-39,]
df.avanzott <-
        read.table(file.path(DirData, "exportAvanzi_batchAREA.csv" ),
                   sep= ",", header=FALSE, skip=2)[c(5,7),]

df.giu <-
    read.table(file.path(DirData, "exportGiugno_batchAREA.csv" ),
               sep= ",", , skip=2, header=FALSE)[-c(9,10),]

df.avanzgiu <-
        read.table(file.path(DirData, "exportAvanzi_batchAREA.csv" ),
                   sep= ",",, skip=2, header=FALSE)[c(3,4,6,8),]


df.ott <- rbind.data.frame(df.ott, df.avanzott)
df.giu <- rbind.data.frame(df.giu, df.avanzgiu)

df.ottCONC <-
        read.table(file.path(DirData, "exportOttobre_batchCONC.csv" ),
                   sep= ",",
                   header=FALSE, skip=2)
df.avanzottCONC <-
        read.table(file.path(DirData, "exportAvanzi_batchCONC.csv" ),
                   sep= ",", header=FALSE, skip=2)[c(5,7),]

df.giuCONC <-
        read.table(file.path(DirData, "exportGiugno_batchCONC.csv" ),
                   sep= ",", , skip=2, header=FALSE)

df.avanzgiuCONC <-
        read.table(file.path(DirData, "exportAvanzi_batchCONC.csv" ),
                   sep= ",",, skip=2, header=FALSE)[c(3,4,6,8),]


df.ottCONC<- rbind.data.frame(df.ottCONC, df.avanzottCONC)
df.giuCONC <- rbind.data.frame(df.giuCONC, df.avanzgiuCONC)


vec.nomi.colonne <-
        readLines(file.path(DirData,
                            "exportOttobre_batchCONC.csv" ),n=2)
    vec.nomi.colonne1 <-
        unlist(strsplit(vec.nomi.colonne[1], "[,]"))
    vec.nomi.colonne2 <-
        unlist(strsplit(vec.nomi.colonne[2], "[,]"))
    ## toglie la tringa finale "Results"
    vec.nomi.colonne1[-1] <-
        substr(vec.nomi.colonne1[-1],
               1,
               nchar(vec.nomi.colonne1[-1])-
               nchar("Results")-1
               )
    vec.nomi <-
        c(vec.nomi.colonne2[1:7],
          vec.nomi.colonne1[-(1:7)]
          )


names(df.ottCONC) <- vec.nomi
names(df.ott) <- vec.nomi
names(df.giuCONC) <- vec.nomi
names(df.giu) <- vec.nomi

write.table(df.ott, file="Ott_elaboratoAREA.csv", sep=";", col.names = NA)
write.table(df.giu, file="Giu_elaboratoAREA.csv", sep=";", col.names = NA)
write.table(df.ottCONC, file="Ott_elaboratoCONC.csv", sep=";", col.names = NA)
write.table(df.giuCONC, file="Giu_elaboratoCONC.csv", sep=";", col.names = NA)
