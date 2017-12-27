AREA <- FALSE
#setwd("/home/Documenti/BitBucket/Dalila_GIT/esplorazione")
setwd("/home/dalila/Dali_GIT/esplorazione")

source("seconda importazione.R")
source(file.path(DirFunz, "biplotAcomp.R"))
#source("/home/ottorino/Documenti/BitBucket/Dalila_GIT/esplorazione/righe_nulle.R")
source("/home/dalila/Dali_GIT/esplorazione/righe_nulle.R")
require(compositions)
## lis.data$Sample <-
##     lis.data$Sample[order(lis.data$Sample$INJ.DATE),]
df.elabora <-
    lis.data$Sample
    ## lis.data$Sample[-c(15:16, 20), c(1:12, 49:50, 13:48)]
## 15:16 e 20  son tre righe da togliere, colonne riorganizzate
## 20 è il mix dei campioni

df.elabora <- df.elabora[df.elabora$MAN %in% c( "CO", "OO" ),]
df.elabora$MAN <- df.elabora$MAN[drop=TRUE]
df.elabora$TIL <- df.elabora$TIL[drop=TRUE]
df.elabora$FIELD <- df.elabora$FIELD[drop=TRUE]

names(df.elabora)[14:51] <-
    paste("C", names(df.elabora)[14:51], sep=".")

df.masse <-
    read.table(file.path(DirData, "corrispondenze_PLFA.csv" ),
               sep= ";", dec=",", header=TRUE,
               fileEncoding = "UTF-8",
               colClasses=c(rep("character",4), "numeric"))[,4:5]
df.masse$NOME.R.nuovo <-
    paste("C", df.masse$NOME.R.nuovo, sep=".")
sum(names(df.elabora)[14:51]!=df.masse$NOME.R.nuovo)

## PLFA.no <- c(17:18, 36:39, 49:50)
## TIC.no <- c(16, 29, 42)
## sopra colonne o doppie o indesiderate

elimina.questi <- -c(1:13, 15,36, 52)

Y.CONC <-
    acomp( df.elabora[, elimina.questi])
Y.MOLI <-
    Y.CONC + df.masse$PM[-c(2,23)]
## lm.prova <-
##     lm(SOMMA.AREE ~ MAN*TIL*STAGIONE, data=df.data)
## anova(lm.prova)

## lm.prova1 <-
##     lm(SOMMA.AREE ~ TIL*STAGIONE, data=df.data)
## anova(lm.prova, lm.prova1)
## anova(lm.prova1)

lm.1 <-
    lm(ilr(Y.CONC) ~ df.elabora$MAN + df.elabora$STAGIONE + df.elabora$TIL)
anova(lm.1)

lm.1 <-
    lm(ilr(Y.MOLI) ~ df.elabora$MAN + df.elabora$STAGIONE + df.elabora$TIL)
anova(lm.1)

## qqnorm(ilrInv(resid(lm.1), orig=Y.CONC))

##########################
lis.elabora <- split(df.elabora, df.elabora$STAGIONE)

Y.aut <-
    acomp( lis.elabora[[1]][,elimina.questi])

Y.est <-
    acomp( lis.elabora[[2]][,elimina.questi])

lis.pcx <- list(AUT=princomp(Y.aut), EST = princomp(Y.est))


fun.biplot(x=lis.pcx[[1]],
           col=c(1,"transparent"),
           choices=c(1,2), ## assi/componenti da considerare
           scale=0,
           pc.biplot=FALSE,
           col.obs = as.numeric(lis.elabora[[1]]$FIELD),
           etich = interaction(lis.elabora[[1]]$TIL, lis.elabora[[1]]$MAN)
           )

fun.biplot(x=lis.pcx[[2]],
           col=c(1,"transparent"),
           choices=c(1,2), ## assi/componenti da considerare
           scale=0,
           pc.biplot=FALSE,
           col.obs =  as.numeric(lis.elabora[[1]]$FIELD),
           etich = interaction(lis.elabora[[2]]$TIL, lis.elabora[[2]]$MAN)
           )



pcx.PLFA <- princomp(Y.CONC)

sum(pcx.PLFA$sdev[2]^2) / mvar(Y.CONC)

##pdf(file.path(DirGraf, "PCAcomposizionale_Medie.pdf")


colori <-   # as.numeric(df.elabora$TIL)
    as.numeric(interaction(df.elabora$MAN, df.elabora$TIL))

fun.biplot(x=pcx.PLFA,
           col=c(1,"transparent"),
           choices=c(1,2), ## assi/componenti da considerare
           scale=1,
           pc.biplot=FALSE,
#           col.obs = as.numeric(df.elabora$PARC),
           col.obs = as.numeric(df.elabora$STAGIONE),
           etich = interaction(df.elabora$TIL, df.elabora$MAN)
           )
# dev.off()
###CONTROLLINO NON BUTTARE !!
## butta <-
## cbind(df.gruppiMicrobici$composti,
##       dimnames(pcx.PLFA$loadings)[[1]]
##       )

## sum(butta[,1]!=butta[,2])

par(mfrow = c(2,4))
fun.biplot(x=pcx.PLFA,
           col=c(1,"transparent"),
           choices=c(1,2), ## assi/componenti da considerare
           scale=1,
           pc.biplot=FALSE,
#           col.obs = as.numeric(df.elabora$PARC),
           col.obs = as.numeric(df.elabora$STAGIONE),
           etich = interaction(df.elabora$TIL, df.elabora$MAN)
           )
for (dalila in 2:7){
    titolo <-
        names(df.gruppiMicrobici)[dalila]
plot(0, 0,
     xlim=c(-0.6,0.6),
     ylim=c(-0.6, 0.6),
     main = titolo
     )
for (i in 1:dim(pcx.PLFA$loadings)[1]){
    posizioneX <-
        pcx.PLFA$loadings[i,1]
    posizioneY <- pcx.PLFA$loadings[i,2]
    titolo <-
        names(df.gruppiMicrobici)[dalila]
    colore <-
        ifelse(df.gruppiMicrobici[i, dalila]==1,
               "black", ## nero se vero
               "transparent")# grifgiochiaro se falso
arrows(0, 0,
       posizioneX, posizioneY,
       col=colore,
       length=0.1)
text(posizioneX, posizioneY,
     label = dimnames(pcx.PLFA$loadings)[[1]][i],
     pos= 1,
     offset=0.5, col=colore)
    }
}





biplot(x=pcx.PLFA,
       col=c(1, "transparent"),
            ## main="foglie",
            choices=c(1,2), ## assi/componenti da considerare
            scale=1,
       pc.biplot=TRUE
       )


 plot(1:10, xlim=c(-1,1), ylim=c(-1,1))
 arrows(0,0,-0.536,0.418, col=2)
 arrows(0,0,-0.583,0.188, col=2)
 arrows(0,0,-0.278,-0.873, col=2)
  arrows(0,0,-0.543,-0.167, col=2)

#################### AVANZI
somma.aree <-
apply(df.elabora[-18,-c(1:14, PLFA.no,TIC.no)], 1, function(x) sum(x,na.rm=TRUE))

lm.1 <-
    lm(somma.aree ~ MAN+TIL, data = df.elabora[-18,])
anova(lm.1)
summary(lm.1)

bwplot(interaction(df.elabora$MAN, df.elabora$TIL)[-18] ~ somma.aree)

               )
##################################
par(mfrow = c(2,2))


plot(0, 0,
     xlim=c(-0.6,0.6),
     ylim=c(-0.6, 0.6),
     main = 
)


for (i in 1:dim(pcx.PLFA$loadings)[1]){
  posizioneX <-
    pcx.PLFA$loadings[i,1]
  posizioneY <- pcx.PLFA$loadings[i,2]
  titolo <-
    names(df.gruppiMicrobici)[2:7]
  colore <-
    ifelse(pcx.PLFA$loadings[i, ]>=0,
           "black", ## nero se vero
           "transparent")# grifgiochiaro se falso
  arrows(0, 0,
         posizioneX, posizioneY,
         col=colore,
         length=0.1)
  text(posizioneX, posizioneY,
       label = dimnames(pcx.PLFA$loadings)[[1]][i],
       pos= 1,
       offset=0.5, col=colore)
}
}


plot(0, 0,
     xlim=c(-0.6,0.6),
     ylim=c(-0.6, 0.6),
     main = 
)


for (i in 1:dim(pcx.PLFA$loadings)[1]){
  posizioneX <-
    pcx.PLFA$loadings[i,1]
  posizioneY <- pcx.PLFA$loadings[i,2]
  titolo <-
    names(df.gruppiMicrobici)[2:7]
  colore <-
    ifelse(pcx.PLFA$loadings[i, ]<=0,
           "black", ## nero se vero
           "transparent")# grifgiochiaro se falso
  arrows(0, 0,
         posizioneX, posizioneY,
         col=colore,
         length=0.1)
  text(posizioneX, posizioneY,
       label = dimnames(pcx.PLFA$loadings)[[1]][i],
       pos= 1,
       offset=0.5, col=colore)
}
}


plot(0, 0,
     xlim=c(-0.6,0.6),
     ylim=c(-0.6, 0.6),
     main = 
)


for (i in 1:dim(pcx.PLFA$loadings)[1]){
  posizioneX <-
    pcx.PLFA$loadings[i,1]
  posizioneY <- pcx.PLFA$loadings[i,2]
  titolo <-
    names(df.gruppiMicrobici)[2:7]
  colore <-
    ifelse(pcx.PLFA$loadings[,i ]>=0, #non ha senso... come gli dico che voglio le y positive?
           "black", ## nero se vero
           "transparent")# grifgiochiaro se falso
  arrows(0, 0,
         posizioneX, posizioneY,
         col=colore,
         length=0.1)
  text(posizioneX, posizioneY,
       label = dimnames(pcx.PLFA$loadings)[[1]][i],
       pos= 1,
       offset=0.5, col=colore)
}
}


plot(0, 0,
     xlim=c(-0.6,0.6),
     ylim=c(-0.6, 0.6),
     main = 
)


for (i in 1:dim(pcx.PLFA$loadings)[1]){
  posizioneX <-
    pcx.PLFA$loadings[i,1]
  posizioneY <- pcx.PLFA$loadings[i,2]
  titolo <-
    names(df.gruppiMicrobici)[2:7]
  colore <-
    ifelse(pcx.PLFA$loadings[,i]<=0, #non ha senso... come gli dico che voglio le y negative?
           "black", ## nero se vero
           "transparent")# grifgiochiaro se falso
  arrows(0, 0,
         posizioneX, posizioneY,
         col=colore,
         length=0.1)
  text(posizioneX, posizioneY,
       label = dimnames(pcx.PLFA$loadings)[[1]][i],
       pos= 1,
       offset=0.5, col=colore)
}
}
