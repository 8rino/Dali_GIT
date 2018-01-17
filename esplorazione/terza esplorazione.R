## ics <- seq(-2,2, by=0.01)
## ics2 <- ics^2
## yps2 <- 2^2-ics^2

## plot(sqrt(ics2),sqrt(yps2), type ="l")
## angolo <- seq(0, 30, length=0.25*length(ics))
## plot(sqrt(ics2), sqrt(yps2)+sin(angolo), type ="l")


AREA <- FALSE ## serve per considerare le concentrazioni e non le aree
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

setwd(file.path(DirMain,"esplorazione"))

source("seconda importazione.R")
source(file.path(DirFunz, "biplotAcomp.R"))
source("righe_nulle.R")
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

names(df.elabora)[i.PLFA] <-
    paste("C", names(df.elabora)[i.PLFA], sep=".")

df.masse <-
    read.table(file.path(DirData, "corrispondenze_PLFA.csv" ),
               sep= ";", dec=",", header=TRUE,
               fileEncoding = "UTF-8",
               colClasses=c(rep("character",4), "numeric"))[,4:5]
df.masse$NOME.R.nuovo <-
    paste("C", df.masse$NOME.R.nuovo, sep=".")

## riga sotto; un controllo sui nomi
## sum(names(df.elabora)[i.PLFA]!=df.masse$NOME.R.nuovo[-c(2,23)])

## PLFA.no <- c(17:18, 36:39, 49:50)
## TIC.no <- c(16, 29, 42)
## sopra colonne o doppie o indesiderate

elimina.questi <- -c(1:13, 15,36, 52)



######## PROVE DALILA

lm.proviamo <- ## logaritmo necessario
    lm(log(SOMMA.MOLI) ~ STAGIONE+TIL ,
       data=df.elabora.nano.MOLI.gr)
anova(lm.proviamo)

summary(lm.proviamo)
with(df.elabora.nano.MOLI.gr, boxplot(log(SOMMA.CONC)~MAN*STAGIONE*TIL))

#### da qui il DISASTRO!

lis.disastro <-
    list(df.elabora$TIL,
         df.elabora$STAGIONE,
         df.elabora$MAN
         )

df.disastro <-
    aggregate(df.elabora[,i.PLFA],
              by= lis.disastro,
              function(x) mean(x, na.rm=TRUE))
names(df.disastro)[1:3] <-
    c("TIL", "STAGIONE", "MAN")
row.names(df.disastro) <-
    as.character(interaction(df.disastro[,1],df.disastro[,2],df.disastro[,3]))

togli <- c(26,34)
stars(df.disastro[,-c(1:3, togli)]/12,
      key.loc = c(0.8,1),
      key.xpd=TRUE,
      draw.segments = TRUE,
      scale = FALSE,
      full = TRUE)

#per avere le moli -> concentrazione * 10^-3 (VOLUME) tutto / pm (df.masse) e poi rimoltiplicato per 10^-6
########### NON PIU i.PLFA

df.elabora.nano.MOLI.gr <- df.elabora

for(i in i.PLFA){# 200 sono i ul pewr irprendere estratto che viene da 1gr di suolo
    df.elabora.nano.MOLI.gr[,i] <- 200*df.elabora[,i]/df.masse[-c(2,23),2][i-13]
}

df.elabora.nano.MOLI.gr$SOMMA.MOLI <-
    apply(df.elabora.nano.MOLI.gr[,c(14:25, 27:33, 35:49)], 1,
          function(x) sum(x, na.rm=TRUE))



lm.provaMoli<-
  lm(log(SOMMA.MOLI) ~ STAGIONE+TIL+MAN, data=df.elabora.MOLI)

anova(lm.provaMoli)

summary(lm.provaMoli)


df.disastro.MOLI <-
    aggregate(df.elabora.MOLI[,i.PLFA],
              by= lis.disastro,
              function(x) mean(x, na.rm=TRUE))
names(df.disastro.MOLI)[1:3] <-
    c("TIL", "STAGIONE", "MAN")
row.names(df.disastro.MOLI) <-
    as.character(interaction(df.disastro.MOLI[,1],df.disastro.MOLI[,2],df.disastro.MOLI[,3]))

stars(df.disastro.MOLI[,-(1:3)],
      key.loc = c(0.8,1),
      key.xpd=TRUE,
      draw.segments = TRUE,
      scale = TRUE,
      full = TRUE)



##### FINE PROVE DALILA

#### tentativo gruppi microbici
lis.PLFA.microbi <-
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


for (i in 1:5){
vec.composti <-
    as.vector(df.gruppiMicrobici[,1][df.gruppiMicrobici[,i+1]==1])
acchiappa <-
    which(names(df.elabora.nano.MOLI.gr) %in% vec.composti)
df.butta <- df.elabora.nano.MOLI.gr[,acchiappa]
vec.scelta <-
    apply(df.butta,2, sum)
PLFA.selezionato <-
    names(which(vec.scelta==max(vec.scelta)))
lis.PLFA.microbi[[i]] <-PLFA.selezionato
}
lis.PLFA.microbi[[6]] <- "C.00LinC16.1.omega5"


solo.questi <- #lis.PLFA.microbi$protozoi
    unique(
       unlist(lis.PLFA.microbi))
solo.questi <-
    c(1:13, which(names(df.elabora.nano.MOLI.gr) %in% solo.questi))
df.elabora.MOLI.MICROBI <-
    df.elabora.nano.MOLI.gr[,solo.questi]
Y.MOLI <-
        acomp( df.elabora.MOLI.MICROBI[, -(1:13)])
lm.1.MOLI <-
    lm(ilr(Y.MOLI) ~
           df.elabora.MOLI.MICROBI$MAN +
           df.elabora.MOLI.MICROBI$STAGIONE +
           df.elabora.MOLI.MICROBI$TIL)
anova(lm.1.MOLI)
plot(Y.MOLI,
     col=as.numeric(df.elabora.MOLI.MICROBI$STAGIONE)
,
     pch=as.numeric(df.elabora.MOLI.MICROBI$MAN)
     )







df.disastro <-
    aggregate(df.elabora.MOLI.MICROBI[,i.PLFA],
              by= lis.disastro,
              function(x) mean(x, na.rm=TRUE))
names(df.disastro)[1:3] <-
    c("TIL", "STAGIONE", "MAN")
row.names(df.disastro) <-
    as.character(interaction(df.disastro[,1],df.disastro[,2],df.disastro[,3]))


    stars(df.elabora.MOLI.MICROBI[,-(1:13)],
      key.loc = c(0.8,1),
      key.xpd=TRUE,
      draw.segments = TRUE,
      scale = TRUE,
      full = TRUE)



#### FINE tentativo gruppi microbici
Y.MOLI <-
        acomp( df.elabora.MOLI.MICROBI[, -(1:13)])
lm.1.MOLI <-
    lm(ilr(Y.MOLI) ~ df.elabora.MOLI.MICROBI$MAN + df.elabora.MOLI.MICROBI$STAGIONE + df.elabora.MOLI.MICROBI$TIL)
anova(lm.1.MOLI)

pcx <- princomp(Y.MOLI)
fun.biplot(x=pcx,
           col=c(1,"transparent"),
           choices=c(1,2), ## assi/componenti da considerare
           scale=0,
           pc.biplot=FALSE,
           col.obs = as.numeric(interaction(df.elabora.MOLI.MICROBI$TIL, df.elabora.MOLI.MICROBI$MAN)),
           etich = interaction(df.elabora.MOLI.MICROBI$TIL, df.elabora.MOLI.MICROBI$MAN)
           )


p


elimina.questi <- -c(1:13, 50)


Y.CONC <-
    acomp( df.elabora[, c(elimina.questi,-togli)])
Y.MOLI <-
        acomp( df.elabora.nano.MOLI.gr[, elimina.questi])
## lm.prova <-
##     lm(SOMMA.AREE ~ MAN*TIL*STAGIONE, data=df.data)
## anova(lm.prova)

## lm.prova1 <-
##     lm(SOMMA.AREE ~ TIL*STAGIONE, data=df.data)
## anova(lm.prova, lm.prova1)
## anova(lm.prova1)

lm.1.CONC <-
    lm(ilr(Y.CONC) ~
           df.elabora$MAN +
           df.elabora$STAGIONE +
           df.elabora$TIL)
anova(lm.1.CONC)

lm.1.MOLI <-
    lm(ilr(Y.MOLI) ~ df.elabora.MOLI$MAN + df.elabora.MOLI$STAGIONE + df.elabora.MOLI$TIL)
anova(lm.1.MOLI)

## qqnorm(ilrInv(resid(lm.1), orig=Y.CONC))

##########################
lis.elabora <- split(df.elabora, df.elabora$STAGIONE)

Y.aut <-
    acomp( lis.elabora[[1]][,elimina.questi])

Y.est <-
    acomp( lis.elabora[[2]][,elimina.questi])

lis.pcx <- list(AUT=princomp(Y.aut), EST = princomp(Y.est))
pcx.all <- princomp(acomp(df.elabora.MOLI[,-c(1:13, 50,51)]))

fun.biplot(x=pcx.all,
           col=c(1,"transparent"),
           choices=c(1,2), ## assi/componenti da considerare
           scale=0,
           pc.biplot=FALSE,
           col.obs = as.numeric(df.elabora.MOLI$FIELD),
           etich = interaction(df.elabora.MOLI$TIL, df.elabora.MOLI$MAN)
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
           #col.obs = as.numeric(df.elabora$MAN),
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
par(mfrow = c(1,1))


plot(0, 0,
     xlim=c(0,0.6),
     ylim=c(-0.2, 0.2),
     main =
         )

                                        #restringendo tra x > 0.2 e y>0.1 non ho niente di "interessante", e anche con 0.1 e 0.1
                                        # for (i in 1:dim(pcx.PLFA$loadings)[1]){
                                        #   posizioneX <-
                                        #     pcx.PLFA$loadings[i,1]
                                        #   posizioneY <- pcx.PLFA$loadings[i,2]
                                        #   titolo <-
                                        #     names(df.gruppiMicrobici)[2:7]
                                        #   colore <-
                                        #     ifelse(pcx.PLFA$loadings[i,1]>=0.2 & pcx.PLFA$loadings[i,2]>=0.1,
                                        #            #restringrndo così tanto ho solo C17:1omeg9, non è tipico
                                        #            "black", ## nero se vero
                                        #            "transparent")# grifgiochiaro se falso
                                        #   arrows(0, 0,
                                        #          posizioneX, posizioneY,
                                        #          col=colore,
                                        #          length=0.1)
                                        #   text(posizioneX, posizioneY,
                                        #        label = dimnames(pcx.PLFA$loadings)[[1]][i],
                                        #        pos= 1,
                                        #        offset=0.5, col=colore)
                                        # }
                                        # }

                                        #con 0.05 sia in x che in y ho c18:1 cis e trans identific dei funghi e gram-
                                        # for (i in 1:dim(pcx.PLFA$loadings)[1]){
                                        #   posizioneX <-
                                        #     pcx.PLFA$loadings[i,1]
                                        #   posizioneY <- pcx.PLFA$loadings[i,2]
                                        #   titolo <-
                                        #     names(df.gruppiMicrobici)[2:7]
                                        #   colore <-
                                        #     ifelse(pcx.PLFA$loadings[i,1]>=0.05 & pcx.PLFA$loadings[i,2]>=0.05,
                                        #
                                        #            "black", ## nero se vero
                                        #            "transparent")# grifgiochiaro se falso
                                        #   arrows(0, 0,
                                        #          posizioneX, posizioneY,
                                        #          col=colore,
                                        #          length=0.1)
                                        #   text(posizioneX, posizioneY,
                                        #        label = dimnames(pcx.PLFA$loadings)[[1]][i],
                                        #        pos= 1,
                                        #        offset=0.5, col=colore)
                                        # }
                                        # }


                                        #restringendo con le sole x positive e y tra -0.05 e 0.05
                                        #ho i 3 composti tipici dei gram -
                                        #il cyclico c19 e i c16:1 sia omega 5 che 7, mi mancherebbe solo il ciclico 17
                                        #se allargo un poà, 0.1 e -0.1 ho anche i due composti dei gram - comuni con
                                        #funghi e micorrize, ovvero c18:1
for (i in 1:dim(pcx.PLFA$loadings)[1]){
    posizioneX <-
        pcx.PLFA$loadings[i,1]
    posizioneY <- pcx.PLFA$loadings[i,2]
    titolo <-
        names(df.gruppiMicrobici)[2:7]
    colore <-
        ifelse(pcx.PLFA$loadings[i,1]>=0 &
               pcx.PLFA$loadings[i,2]>= -0.05 &
               pcx.PLFA$loadings[i,2]<=0.05,
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
    mtext("gram neg",3)
}


logico <- c()
plot(0, 0,
     xlim=c(-0.05,0.05),
     ylim=c(-0.2, 0.2),
     main = "protozoi solo  c20:2"
     )
                                        #su, di interessante ci sono solo i protozoi con i c20:2
for (i in 1:dim(pcx.PLFA$loadings)[1]){
    posizioneX <-
        pcx.PLFA$loadings[i,1]
    posizioneY <- pcx.PLFA$loadings[i,2]
    titolo <-
        names(df.gruppiMicrobici)[2:7]
    colore <- ifelse(pcx.PLFA$loadings[i,2]>=0 &
                     pcx.PLFA$loadings[i,1]>= -0.05 &
                     pcx.PLFA$loadings[i,1]<=0.05,
                     "black", ## nero se vero
                     "transparent")# grifgiochiaro se falso
    logico[i] <-
        ifelse(colore=="black", TRUE, FALSE)
    arrows(0, 0,
           posizioneX, posizioneY,
           col=colore,
           length=0.1)
    text(posizioneX, posizioneY,
         label = dimnames(pcx.PLFA$loadings)[[1]][i],
         pos= 1,
         offset=0.5, col=colore)
}
vec.sopra <-
    dimnames(pcx.PLFA$loadings)[[1]][logico]


plot(0, 0,
     xlim=c(-0.2,0.3),
     ylim=c(-0.4, 0),
     main =  "gram +"
     )
                                        # ho 5 su 6 componenti dei gram +
for (i in 1:dim(pcx.PLFA$loadings)[1]){
    posizioneX <-
        pcx.PLFA$loadings[i,1]
    posizioneY <- pcx.PLFA$loadings[i,2]
    titolo <-
        names(df.gruppiMicrobici)[2:7]
    colore <- ifelse(pcx.PLFA$loadings[i,2]<=-0.03 &
                     pcx.PLFA$loadings[i,1]>= -0.05 &
                     pcx.PLFA$loadings[i,1]<=0.05,
                     "black", ## nero se vero
                     "transparent")# grigiochiaro se falso
    logico[i] <- ifelse(pcx.PLFA$loadings[i,2]<=-0.03 &
                        pcx.PLFA$loadings[i,1]>= -0.05 &
                        pcx.PLFA$loadings[i,1]<=0.05,
                        TRUE, ## nero se vero
                        FALSE)# grigi
    arrows(0, 0,
           posizioneX, posizioneY,
           col=colore,
           length=0.1)
    text(posizioneX, posizioneY,
         label = dimnames(pcx.PLFA$loadings)[[1]][i],
         pos= 1,
         offset=0.5, col=colore)
}

vec.sotto <-
    dimnames(pcx.PLFA$loadings)[[1]][logico]


plot(0, 0,
     xlim=c(-0.5,0),
     ylim=c(-0.2, 0.2),
     main =
         )
                                        #così ho un fungo, l'altro è tutto a destra con i gram - e un attinomiceto
                                        # l'altro degli attinomiceti, segnato come met01c17 va in giù
for (i in 1:dim(pcx.PLFA$loadings)[1]){
    posizioneX <-
        pcx.PLFA$loadings[i,1]
    posizioneY <- pcx.PLFA$loadings[i,2]
    titolo <-
        names(df.gruppiMicrobici)[2:7]
    colore <- ifelse(pcx.PLFA$loadings[i,1]<=0 &
                     pcx.PLFA$loadings[i,2]>= -0.05 &
                     pcx.PLFA$loadings[i,2]<=0.05,
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


lm.soprasotto <-
    lm(pcx.PLFA$scores[,2] ~ df.elabora$MAN*df.elabora$TIL*df.elabora$STAGIONE)
anova(lm.soprasotto)



pdf("butta.pdf")
par(mfrow=c(2,2))
for(i in seq(1,36, 4)){
    for(k in 0:3){
        boxplot(Y.CONC[,i+k] ~ interaction(df.elabora$STAGIONE),
                main= names(Y.CONC)[i+k], ylab="Y.CONC")
    }
}
dev.off()

df.elabora$TIL <- factor(df.elabora$TIL, levels=c("Fzo","Rip", "Ara")[c(3,2,1)])

lm.tutto <-
    lm(ilr(Y.CONC) ~
           df.elabora$STAGION *
           df.elabora$TIL *
           df.elabora$MAN)

lm.no3 <- lm(ilr(Y.CONC) ~
             df.elabora$STAGIONE + df.elabora$TIL + df.elabora$MAN +
             df.elabora$TIL:df.elabora$MAN+
             df.elabora$MAN:df.elabora$STAGIONE+
             df.elabora$TIL:df.elabora$STAGIONE )

lm.pippo <- lm(ilr(Y.CONC) ~
             df.elabora$STAGIONE + df.elabora$TIL + df.elabora$MAN)

anova(lm.tutto, lm.pippo, lm.no3)


anova(lm(ilr(Y.CONC) ~
anova(lm(ilr(Y.CONC) ~ df.elabora$MAN + df.elabora$STAGIONE + df.elabora$TIL))[4,]

##
anova(lm.no3)
coefs <- ilrInv(coef(lm.1.CONC), orig=Y.CONC)
barplot(coefs, las=2, col=rainbow(4))

B <- clr(coefs[-1,])
svdlm <- svd(B)
coloredBiplot(x=svdlm$v[,1:2], y=svdlm$u[,1:2],
              scale=0
              )


xarrows=TRUE,
              ypch=4, ynames=colnames(B),
              xnames=rownames(B)
              )

sum(svdlm$d[1:2]^2/sum(svdlm$d^2))


### ESEMPI TOLOSANA DELGADO

GeoChemSed=read.csv(file.path(DirData,"GraVel.csv"),header=TRUE)
Y=acomp(GeoChemSed[,7:10])
names(GeoChemSed)

Covariables=GeoChemSed[,c("discharge","relief","grain",
"position")]
X1 = Covariables$discharge
X2 = Covariables$relief
X4 = factor(Covariables$position)
X3 = factor(Covariables$grain,c("f","m","c"), ordered=TRUE)
levels(X3) <- c("fine","medium","coarse")
contrasts(X3)<-"contr.treatment"

model = lm(ilr(Y) ~ log(X1) + X2 + X3 + X4)

coefs <- ilrInv(coef(model),orig=Y)
B = clr(coefs[-1,])
svdlm = svd(B)
opar <- par(xpd=NA,mar=c(2,2,0,0))
coloredBiplot(#svdlm,
    x=svdlm$v, y=svdlm$u,
              scale=0,
    xlabs.pc=rownames(B))
, xlim=c(-4,4), ylim=c(-1,1))
             # scale =0, xarrows=TRUE)
           # ypch=4,
           #   ynames=colnames(B),
sum(svdlm$d[1:2]^2)/sum(svdlm$d^2)



xc <- acomp( df.elabora[, elimina.questi])
dd <- dist(xc)
hc <- hclust(dd, method="ward.D2")
plot(hc)


xc <- acomp( df.elabora[, elimina.questi])
dd <- as.dist(
    variation(xc))
hc <-  hclust(dd, method="ward.D2")
      (dend1 <- as.dendrogram(hc)) # "print()" method
plot(dend1)
rect.hclust(hc , k=7, border="red")


hc <- hclust(dd, method="ward.D2")
pdf("butta2.pdf")
plot(hc)
dev.off()


require(MASS)
fattore <- interaction(df.elabora$TIL, df.elabora$MAN, df.elabora$STAGIONE)
res <- qda(x=data.frame(ilr(xc)), grouping=fattore)
pairs(res, col=(1:12)[as.integer(fattore)])

d <-  hclust(dist(mtcars))
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(hc , k=5, border="red")
