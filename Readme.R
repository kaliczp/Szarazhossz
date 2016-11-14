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
    szaraz.idx <- which(szaraz.diff == -1)
    max(period.apply(szarazTS.xts, szaraz.idx, sum))
}

apply.yearly(TS.xts, szarazhossz)
