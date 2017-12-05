source("prima importazione.R")
source(file.path(DirFunz, "biplotAcomp.R"))

require(compositions)
## lis.data$Sample <-
##     lis.data$Sample[order(lis.data$Sample$INJ.DATE),]
df.elabora <-
    lis.data$Sample
    ## lis.data$Sample[-c(15:16, 20), c(1:12, 49:50, 13:48)]
## 15:16 e 20  son tre righe da togliere, colonne riorganizzate
## 20 è il mix dei campioni

PLFA.no <- c(17:18, 36:39, 49:50)
TIC.no <- c(16, 29, 42)
## sopra colonne o doppie o indesiderate
Y.aree <-
    acomp( df.elabora[,-c(1:13,52)])

fattore <-  #df.elabora$TIL
    interaction(df.elabora$MAN)
fattore <- fattore[drop= TRUE]

lm.1 <-
    lm(ilr(Y.aree) ~ fattore)
anova(lm.1)

#qqnorm(ilrInv(resid(lm.1), orig=Y.aree))

##########################

pcx.PLFA <- princomp(Y.aree)

##pdf(file.path(DirGraf, "PCAcomposizionale_Medie.pdf")


colori <-
    as.numeric(interaction(df.elabora$MAN, df.elabora$STAGIONE))

fun.biplot(x=pcx.PLFA,
           col=c(1,"transparent"),
           choices=c(1,2), ## assi/componenti da considerare
           scale=1,
           pc.biplot=FALSE,
           col.obs = colori+2#,
           #etich = df.elabora$STRINGA
           )
                                        # dev.off()


biplot(x=pcx.PLFA,
       col=c(1, "transparent"),
            ## main="foglie",
            choices=c(1,2), ## assi/componenti da considerare
            scale=1,
            pc.biplot=TRUE
            )

somma.aree <-
apply(df.elabora[-18,-c(1:14, PLFA.no,TIC.no)], 1, function(x) sum(x,na.rm=TRUE))

lm.1 <-
    lm(somma.aree ~ MAN+TIL, data = df.elabora[-18,])
anova(lm.1)
summary(lm.1)

bwplot(interaction(df.elabora$MAN, df.elabora$TIL)[-18] ~ somma.aree)

               )
