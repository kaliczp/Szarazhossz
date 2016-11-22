## Cél, egy 1 oszlopos txt-ből (napi csapadékok) számolja ki a
## leghosszabb csapadékszegény időszak hosszát adott évre (és ennek 30
## éves átlagát).  Kérdés az is, hogy egy 1 oszlopos txt mellé
## (pl. csapadékadatok) lehet-e tetetni az R-rel év és hónap
## oszlopokat.  Erről tudnánk majd beszélni?

## a csapadékszegény időszak gondolom az lehet, hogy hány napig egymás
## után van 1 mm-nél kevesebb eső - amúgy PAI-t szeretnék számolni,
## annak az egyik korrekciós faktorához kell, pontosabban ott se
## definiálták

## Napi csapadék generálás
Time <- seq(as.Date("2013-01-01"),as.Date("2016-12-31"),"days")
library(xts)
TS.xts <- xts(rep.int(0, times=length(Time)), Time)
csapi <- sample(1:length(Time), size=0.3*length(Time))
TS.xts[csapi] <- round(rf(length(csapi),20,1),1)
ttbig <- length(TS.xts[TS.xts >100])
TS.xts[TS.xts >100] <- round(rnorm(ttbig, mean=80, sd=10),1)
plot(TS.xts,typ="h")

## Éves száraz hossz számítás
szarazhossz <- function(x) {
    ## Nagyobb-e 1 mm-nél 
    szarazTS.xts <- x < 1
    szaraz.diff <- diff(szarazTS.xts)
    ## Végére -1, ha az időszak utolsó napjaiban nincs eső
    if(all(tail(szaraz.diff) == 0)) {
        szaraz.diff <- c(szaraz.diff, xts(-1,index(tail(szaraz.diff,1))+1))
        szarazTS.xts <- c(szarazTS.xts,xts(FALSE,index(tail(szarazTS.xts,1))+1))
    }
    szaraz.idx <- which(szaraz.diff == -1)
    ## Ha az elején van hosszabb száraz
    szaraz.idx <- c(1, szaraz.idx)
    max(period.apply(szarazTS.xts, szaraz.idx, sum))
}

apply.yearly(TS.xts, szarazhossz)

## Modell adatok
p.raw <- scan("PAI/test_P_HUN_2001-2099_JJA.txt")


## Dátum generálás
ttonlyoneyear <- 2001:2099
ttyears <- rep(ttonlyoneyear, each=90)
alldate <- as.Date(paste0(ttyears,
                          rep(paste0(rep(c("-06-","-07-","-08-"),each=30),
                              rep(1:30,3)),
                               length(ttonlyoneyear))
                          )
                   )

p.xts <- xts(p.raw, alldate)

hossz.xts <- apply.yearly(p.xts, szarazhossz)
write.zoo(hossz.xts, "hossz.csv", sep=";")

pdf()
plot.xts(hossz.xts, xaxs="i", xaxt="n", main="", ylab="Szárazidőszak [nap]")
axis.POSIXct(1,at=as.POSIXct(paste0(2000+1:9*10,"-08-30")),format="%Y")
lines(xts(lowess(hossz.xts)$y, index(hossz.xts)), lwd=2)
dev.off()
