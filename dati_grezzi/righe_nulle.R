Gruppi_Microbici <- read.table("/home/dalila/Dali_GIT/dati_grezzi/spettriBATCH1.csv", sep= ";", header= TRUE)
Gruppi_Microbici
sottogruppo <- (Gruppi_Microbici[,3:8])
sottogruppo
names(sottogruppo)
sottogruppo<-na.omit(sottogruppo)
sottogruppo
SommaRighe <- apply(sottogruppo, 1, sum)
SommaRighe
RigheNulle<- which(SommaRighe==0)
RigheNulle
length(RigheNulle)

