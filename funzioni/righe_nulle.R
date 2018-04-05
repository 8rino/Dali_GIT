# info.sistema <-
#     Sys.info()[c(1,7)]
# if(info.sistema[1]=="Windows"){
#     DirMain <-
#         paste("C:/Users/",
#               info.sistema[2],
#               "/Dropbox/MOLTE_R", sep="")
# } else {
#     if(info.sistema[2]=="dalila" ){
#         DirMain <-"~/Dali_GIT"
#     }else{
#         DirMain <-"~/Documenti/BitBucket/Dalila_GIT"
#     }
# }

# DirData <-
#     file.path(DirMain,"dati_grezzi")
# DirElab <-
#     file.path(DirMain,"dati_elaborati")
# DirGraf <-
#     file.path(DirMain, "grafici")
# DirFunz <-
#     file.path(DirMain, "funzioni")
# DirCod <-
#     file.path(DirMain, "codice")
## Righe sopra eliminate dopo tesi conseguita verificare su linux


## Gruppi_Microbici <-
##     read.table("/home/dalila/Dali_GIT/dati_grezzi/spettriBATCH1.csv",
##                sep= ";", header= TRUE)
##8 è un dataframe: df ci ricorda la classe dell'oggetto
##8 uso delle minuscMaiusc per leggere meglio
##8 osserva che ho razionalizzato anche il file spettriBATCH1.csv con una
##8 colonna note. Conviene SEMPRE avere colonne con dati omogenei.
##8 Guardati il file csv come è stato modificato

df.gruppiMicrobici <-
    read.table(file.path(DirData,"spettriBATCH1.csv"),
               sep= ";", header= TRUE,
               colClasses =
                   c("character", rep("numeric",6), "character")
               )

## Gruppi_Microbici
##8 ok vedere l'oggetto ma rischi di vedere passare centinaia di
##8 pagine se è grosso. Quindi;


head(df.gruppiMicrobici)
names(df.gruppiMicrobici)
sapply(df.gruppiMicrobici, class)
##8 in genere questi comandi sopra non si scrivono nello script ma
##8 solo nella console per controllo
unique(df.gruppiMicrobici[,2:7])
##8 il comando sopra (da dare nella console) scova le righe uguali
##quindi

questi.no <-
    which(
        is.na(rowSums(df.gruppiMicrobici[,2:7]))|
        df.gruppiMicrobici$composti== "C.00UnkC12.6" |
        df.gruppiMicrobici$composti== "C.11MetC14.0"
        )


df.gruppiMicrobici <-
    df.gruppiMicrobici[-questi.no,]

##8 va bene anche apply come hai scritto tu
composti.non.assegnabili <-
    which(rowSums(df.gruppiMicrobici[,2:7])==0)

df.gruppiMicrobici$composti[composti.non.assegnabili]

sink(file.path(DirElab, "Uscita_dati.txt"))
print("Questi composti qui sotto non sono assegnabili ad alcun gruppo")
df.gruppiMicrobici$composti[composti.non.assegnabili]
print("Sono ")
length(composti.non.assegnabili)
print("su")
dim(df.gruppiMicrobici)[1]
print("composti riconosciuti alla massa, ovvero il")
paste(
    round(
    (length(composti.non.assegnabili)/
     dim(df.gruppiMicrobici)[1])*100,
    2),
    "% del totale")
sink()

##8 OK apprezzo lo sforzo. Senza ironia. Vedi sopra uno dei mille modi
## sottogruppo <- (Gruppi_Microbici[,3:8])
## sottogruppo
## names(sottogruppo)
## sottogruppo<-na.omit(sottogruppo)
## sottogruppo
## SommaRighe <- apply(sottogruppo, 1, sum)
## SommaRighe
## RigheNulle<- which(SommaRighe==0)
## RigheNulle
## length(RigheNulle)
