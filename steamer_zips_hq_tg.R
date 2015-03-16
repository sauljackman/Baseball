library(foreign)
library(reshape2)
library(dplyr)
library(ggplot2)

##############################################################
## Steamer Projections
##############################################################

## Hitters

s_hit <- read.csv("~/Dropbox/Baseball/Steamer Hitters.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
s_hit <- merge(s_hit, id, by.x="playerid", by.y="IDFANGRAPHS")

s_hit$sH <- s_hit$H
s_hit$sR <- s_hit$R
s_hit$sHR <- s_hit$HR
s_hit$sRBI <- s_hit$RBI
s_hit$sSB <- s_hit$SB
s_hit$sAB <- s_hit$AB
s_hit$sPA <- s_hit$PA
s_hit$sAVG <- s_hit$AVG
s_hit$Name <- s_hit$Name.x

shit <- s_hit[, c("Name", "POS", "sAVG", "sPA", "sR", "sHR", "sRBI", "sSB", "sAB", "sH")]


## Pitchers

s_pit <- read.csv("~/Dropbox/Baseball/Steamer Pitchers.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
s_pit <- merge(s_pit, id, by.x="playerid", by.y="IDFANGRAPHS")

s_pit$sWH <- s_pit$BB + s_pit$H
s_pit$sW <- s_pit$W
s_pit$sK <- s_pit$SO
s_pit$sSV <- s_pit$SV
s_pit$sERA <- s_pit$ERA
s_pit$sWHIP <- s_pit$WHIP
s_pit$sIP <- s_pit$IP
s_pit$Name <- s_pit$Name.x
s_pit$sER <- s_pit$ER

spit <- s_pit[, c("Name", "POS", "sIP", "sW", "sK", "sSV", "sERA", "sWHIP", "sWH", "sER")]


##############################################################
## Zips Projections
##############################################################

## Hitters

zips_hit <- read.csv("~/Dropbox/Baseball/ZiPS Hitters.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
zips_hit <- merge(zips_hit, id, by.x="Player", by.y="Name")

zips_hit$zipsPA <- zips_hit$AB + zips_hit$BB + zips_hit$HP
zips_hit$zipsH <- zips_hit$H
zips_hit$zipsR <- zips_hit$R
zips_hit$zipsHR <- zips_hit$HR
zips_hit$zipsRBI <- zips_hit$RBI
zips_hit$zipsSB <- zips_hit$SB
zips_hit$zipsAB <- zips_hit$AB
zips_hit$zipsAVG <- zips_hit$BA
zips_hit$Name <- zips_hit$Player

zipshit <- zips_hit[, c("Name", "zipsAVG", "zipsPA", "zipsR", "zipsHR", "zipsRBI", "zipsSB", "zipsAB", "zipsH")]


## Pitchers

zips_pit <- read.csv("~/Dropbox/Baseball/ZiPS Pitchers.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
zips_pit <- merge(zips_pit, id, by.x="Player", by.y="Name")

zips_pit$zipsWH <- zips_pit$BB + zips_pit$H
zips_pit$zipsW <- zips_pit$W
zips_pit$zipsK <- zips_pit$SO
zips_pit$zipsERA <- zips_pit$ERA
zips_pit$zipsWHIP <- (zips_pit$BB + zips_pit$H)/zips_pit$IP
zips_pit$zipsIP <- zips_pit$IP
zips_pit$zipsER <- zips_pit$ER
zips_pit$Name <- zips_pit$Player

zipspit <- zips_pit[, c("Name", "zipsIP", "zipsW", "zipsK", "zipsERA", "zipsWHIP", "zipsWH", "zipsER")]


##############################################################
## Baseball HQ Projections
##############################################################

## Hitters

hq_hit <- read.csv("~/Dropbox/Baseball/HQ Hitters.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
hq_hit <- merge(hq_hit, id, by="Name")
hq_hit$PA <- hq_hit$AB + hq_hit$BB

hq_hit$hqH <- hq_hit$H
hq_hit$hqR <- hq_hit$R
hq_hit$hqHR <- hq_hit$HR
hq_hit$hqRBI <- hq_hit$RBI
hq_hit$hqSB <- hq_hit$SB
hq_hit$hqAB <- hq_hit$AB
hq_hit$hqPA <- hq_hit$AB + hq_hit$BB
hq_hit$hqAVG <- hq_hit$AVG/1000

hqhit <- hq_hit[, c("Name", "hqAVG", "hqPA", "hqR", "hqHR", "hqRBI", "hqSB", "hqAB", "hqH")]


## Pitchers

hq_pit <- read.csv("~/Dropbox/Baseball/HQ Pitchers.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
hq_pit <- merge(hq_pit, id, by="Name")

hq_pit$hqWH <- hq_pit$BB + hq_pit$H
hq_pit$hqW <- hq_pit$W
hq_pit$hqK <- hq_pit$K
hq_pit$hqSV <- hq_pit$Sv
hq_pit$hqERA <- hq_pit$ERA
hq_pit$hqWHIP <- hq_pit$WHIP
hq_pit$hqIP <- hq_pit$IP
hq_pit$hqER <- hq_pit$ER

hqpit <- hq_pit[, c("Name", "hqIP", "hqW", "hqK", "hqSV", "hqERA", "hqWHIP", "hqWH", "hqER")]


##############################################################
## TG Projections
##############################################################

## Hitters

tg_hit <- read.csv("~/Dropbox/Baseball/TG Hitters.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
tg_hit <- merge(tg_hit, id, by="Name")

tg_hit$tgH <- tg_hit$H
tg_hit$tgR <- tg_hit$R
tg_hit$tgHR <- tg_hit$HR
tg_hit$tgRBI <- tg_hit$RBI
tg_hit$tgSB <- tg_hit$SB
tg_hit$tgAB <- tg_hit$AB
tg_hit$tgPA <- tg_hit$AB + tg_hit$BB
tg_hit$tgAVG <- tg_hit$AVG

tghit <- tg_hit[, c("Name", "tgAVG", "tgPA", "tgR", "tgHR", "tgRBI", "tgSB", "tgAB", "tgH")]


## Pitchers

tg_pit <- read.csv("~/Dropbox/Baseball/TG Pitchers.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
tg_pit <- merge(tg_pit, id, by="Name")

tg_pit$tgWH <- tg_pit$BB + tg_pit$H
tg_pit$tgW <- tg_pit$W
tg_pit$tgK <- tg_pit$K
tg_pit$tgSV <- tg_pit$SV
tg_pit$tgERA <- tg_pit$ERA
tg_pit$tgWHIP <- tg_pit$WHIP
tg_pit$tgER <- tg_pit$ER
tg_pit$tgIP <- tg_pit$IP

tgpit <- tg_pit[, c("Name", "tgIP", "tgW", "tgK", "tgSV", "tgERA", "tgWHIP", "tgWH", "tgER")]


##############################################################
## CBS/Davenport Projections
##############################################################

## Hitters

cd_hit <- read.csv("~/Dropbox/Baseball/CBS_Davenport Hitters.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
cd_hit <- merge(cd_hit, id, by.x="Player.Name", by.y="Name")

cd_hit$cdH <- cd_hit$hits
cd_hit$cdR <- cd_hit$runs
cd_hit$cdHR <- cd_hit$hrs
cd_hit$cdRBI <- cd_hit$rbi
cd_hit$cdSB <- cd_hit$sb
cd_hit$cdAB <- cd_hit$ab
cd_hit$cdPA <- cd_hit$ab + cd_hit$bb
cd_hit$cdAVG <- cd_hit$ave
cd_hit$Name <- cd_hit$Player.Name

cdhit <- cd_hit[, c("Name", "cdAVG", "cdR", "cdHR", "cdRBI", "cdSB", "cdAB", "cdH", "cdPA")]


## Pitchers

cd_pit <- read.csv("~/Dropbox/Baseball/CBS_Davenport Pitchers.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
cd_pit <- merge(cd_pit, id, by.x="Player.Name", by.y="Name")

cd_pit$cdER <- cd_pit$er
cd_pit$cdWH <- cd_pit$bbi + cd_pit$h
cd_pit$cdW <- cd_pit$w
cd_pit$cdK <- cd_pit$k
cd_pit$cdSV <- cd_pit$sv
cd_pit$cdERA <- cd_pit$era
cd_pit$cdWHIP <- cd_pit$whip
cd_pit$cdIP <- cd_pit$ip
cd_pit$Name <- cd_pit$Player.Name

cdpit <- cd_pit[, c("Name", "cdW", "cdK", "cdSV", "cdERA", "cdWHIP", "cdWH", "cdER", "cdIP")]


##############################################################
## ESPN/CBS Projections
##############################################################

## Hitters

ec_hit <- read.csv("~/Dropbox/Baseball/ESPN_CBS Hitters.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
ec_hit <- merge(ec_hit, id, by.x="Player.Name", by.y="Name")

ec_hit$ecH <- ec_hit$hits
ec_hit$ecR <- ec_hit$runs
ec_hit$ecHR <- ec_hit$hrs
ec_hit$ecRBI <- ec_hit$rbi
ec_hit$ecSB <- ec_hit$sb
ec_hit$ecAB <- ec_hit$ab
ec_hit$ecPA <- ec_hit$ab + ec_hit$bb
ec_hit$ecAVG <- ec_hit$ave
ec_hit$Name <- ec_hit$Player.Name

echit <- ec_hit[, c("Name", "ecAVG", "ecR", "ecHR", "ecRBI", "ecSB", "ecAB", "ecH", "ecPA")]


## Pitchers

ec_pit <- read.csv("~/Dropbox/Baseball/ESPN_CBS Pitchers.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
ec_pit <- merge(ec_pit, id, by.x="Player.Name", by.y="Name")

ec_pit$ecER <- ec_pit$er
ec_pit$ecWH <- ec_pit$bbi + ec_pit$h
ec_pit$ecW <- ec_pit$w
ec_pit$ecK <- ec_pit$k
ec_pit$ecSV <- ec_pit$sv
ec_pit$ecERA <- ec_pit$era
ec_pit$ecWHIP <- ec_pit$whip
ec_pit$ecIP <- ec_pit$ip
ec_pit$Name <- ec_pit$Player.Name

ecpit <- ec_pit[, c("Name", "ecW", "ecK", "ecSV", "ecERA", "ecWHIP", "ecWH", "ecER", "ecIP")]


##############################################################
## ESPN/Davenport Projections
##############################################################

## Hitters

ed_hit <- read.csv("~/Dropbox/Baseball/ESPN_Davenport Hitters.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
ed_hit <- merge(ed_hit, id, by.x="Player.Name", by.y="Name")

ed_hit$edH <- ed_hit$hits
ed_hit$edR <- ed_hit$runs
ed_hit$edHR <- ed_hit$hrs
ed_hit$edRBI <- ed_hit$rbi
ed_hit$edSB <- ed_hit$sb
ed_hit$edAB <- ed_hit$ab
ed_hit$edPA <- ed_hit$ab + ed_hit$bb
ed_hit$edAVG <- ed_hit$ave
ed_hit$Name <- ed_hit$Player.Name

edhit <- ed_hit[, c("Name", "edAVG", "edR", "edHR", "edRBI", "edSB", "edAB", "edH", "edPA")]


## Pitchers

ed_pit <- read.csv("~/Dropbox/Baseball/ESPN_Davenport Pitchers.csv")
id <- read.csv("~/Dropbox/Baseball/SFBB-Player-ID-Map.csv")
ed_pit <- merge(ed_pit, id, by.x="Player.Name", by.y="Name")

ed_pit$edER <- ed_pit$er
ed_pit$edWH <- ed_pit$bbi + ed_pit$h
ed_pit$edW <- ed_pit$w
ed_pit$edK <- ed_pit$k
ed_pit$edSV <- ed_pit$sv
ed_pit$edERA <- ed_pit$era
ed_pit$edWHIP <- ed_pit$whip
ed_pit$edIP <- ed_pit$ip
ed_pit$Name <- ed_pit$Player.Name

edpit <- ed_pit[, c("Name", "edW", "edK", "edSV", "edERA", "edWHIP", "edWH", "edER", "edIP")]


##############################################################
## Merging Projections
##############################################################


## Hitters

hit <- merge(shit, hqhit, by="Name", all=TRUE)
hit <- merge(hit, zipshit, by="Name", all=TRUE)
hit <- merge(hit, tghit, by="Name", all=TRUE)


for (i in 1:nrow(hit)) {
	hit$R[i] <- round(mean(c(hit$sR[i], hit$zipsR[i], hit$hqR[i], hit$tgR[i]), na.rm=TRUE), 0)
	hit$HR[i] <- round(mean(c(hit$sHR[i], hit$zipsHR[i], hit$hqHR[i], hit$tgHR[i]), na.rm=TRUE), 0)
	hit$RBI[i] <- round(mean(c(hit$sRBI[i], hit$zipsRBI[i], hit$hqRBI[i], hit$tgRBI[i]), na.rm=TRUE), 0)
	hit$SB[i] <- round(mean(c(hit$sSB[i], hit$zipsSB[i], hit$hqSB[i], hit$tgSB[i]), na.rm=TRUE), 0)
	hit$AVG[i] <- round(mean(c(hit$sAVG[i], hit$zipsAVG[i], hit$hqAVG[i], hit$tgAVG[i]), na.rm=TRUE), 3)
	hit$AB[i] <- round(mean(c(hit$sAB[i], hit$zipsAB[i], hit$hqAB[i], hit$tgAB[i]), na.rm=TRUE), 0)
	hit$PA[i] <- round(mean(c(hit$sPA[i], hit$zipsPA[i], hit$hqPA[i], hit$tgPA[i]), na.rm=TRUE), 0)
	hit$H[i] <- round(mean(c(hit$sH[i], hit$zipsH[i], hit$hqH[i], hit$tgH[i]), na.rm=TRUE), 0)
}

#hit[1:20, c("Name", "POS", "PA", "AVG", "R", "HR", "RBI", "SB")]


## Pitchers

pit <- merge(spit, hqpit, by="Name", all=TRUE)
pit <- merge(pit, zipspit, by="Name", all=TRUE)
pit <- merge(pit, tgpit, by="Name", all=TRUE)


for (i in 1:nrow(pit)) {
	pit$W[i] <- round(mean(c(pit$sW[i], pit$zipsW[i], pit$hqW[i], pit$tgW[i]), na.rm=TRUE), 0)
	pit$K[i] <- round(mean(c(pit$sK[i], pit$zipsK[i], pit$hqK[i], pit$tgK[i]), na.rm=TRUE), 0)
	pit$SV[i] <- round(mean(c(pit$sSV[i], pit$hqSV[i], pit$tgSV[i]), na.rm=TRUE), 0)
	pit$ERA[i] <- round(mean(c(pit$sERA[i], pit$zipsERA[i], pit$hqERA[i], pit$tgERA[i]), na.rm=TRUE), 2)
	pit$WHIP[i] <- round(mean(c(pit$sWHIP[i], pit$zipsWHIP[i], pit$hqWHIP[i], pit$tgWHIP[i]), na.rm=TRUE), 2)
	pit$IP[i] <- round(mean(c(pit$sIP[i], pit$zipsIP[i], pit$hqIP[i], pit$tgIP[i]), na.rm=TRUE), 0)
	pit$ER[i] <- round(mean(c(pit$sER[i], pit$zipsER[i], pit$hqER[i], pit$tgER[i]), na.rm=TRUE), 0)
	pit$WH[i] <- round(mean(c(pit$sWH[i], pit$zipsWH[i], pit$hqWH[i], pit$tgWH[i]), na.rm=TRUE), 0)
}


#pit[1:20, c("Name", "IP", "W", "K", "SV", "ERA", "WHIP")]


##############################################################
## Converting Into Rankings
##############################################################

## Hitters

for (i in 1:10) {
	hit$zHR <- (hit$HR-mean(hit$HR[1:130]))/sd(hit$HR[1:130])
	hit$zR <- (hit$R-mean(hit$R[1:130]))/sd(hit$R[1:130])
	hit$zRBI <- (hit$RBI-mean(hit$RBI[1:130]))/sd(hit$RBI[1:130])
	hit$zSB <- (hit$SB-mean(hit$SB[1:130]))/sd(hit$SB[1:130])
	hit$xH <- hit$H - (hit$AB*mean(hit$AVG[1:130]))
	hit$zAVG <- (hit$xH - mean(hit$xH[1:130]))/sd(hit$xH[1:130])
	hit$zTOT <- hit$zHR + hit$zR + hit$zRBI + hit$zSB + hit$zAVG
	hit <- hit[order(hit$zTOT, decreasing=TRUE),]
	hitC <- subset(hit, hit$POS=="C")
	hit1B <- subset(hit, hit$POS=="1B")
	hit2B <- subset(hit, hit$POS=="2B")
	hitSS <- subset(hit, hit$POS=="SS")
	hit3B <- subset(hit, hit$POS=="3B")
	hitOF <- subset(hit, hit$POS=="OF")
	hitDH <- subset(hit, hit$POS=="DH")
	hitC$zVAA <- hitC$zTOT - mean(hitC$zTOT[1:10])
	hit1B$zVAA <- hit1B$zTOT - mean(hit1B$zTOT[1:20])
	hit2B$zVAA <- hit2B$zTOT - mean(hit2B$zTOT[1:15])
	hitSS$zVAA <- hitSS$zTOT - mean(hitSS$zTOT[1:15])
	hit3B$zVAA <- hit3B$zTOT - mean(hit3B$zTOT[1:18])
	hitOF$zVAA <- hitOF$zTOT - mean(hitOF$zTOT[1:52])
	hitDH$zVAA <- hitDH$zTOT - mean(hit1B$zTOT[1:20])
	hitC$zVAR <- hitC$zVAA - hitC$zVAA[10]
	hit1B$zVAR <- hit1B$zVAA - hit1B$zVAA[20]
	hit2B$zVAR <- hit2B$zVAA - hit2B$zVAA[15]
	hitSS$zVAR <- hitSS$zVAA - hitSS$zVAA[15]
	hit3B$zVAR <- hit3B$zVAA - hit3B$zVAA[18]
	hitOF$zVAR <- hitOF$zVAA - hitOF$zVAA[52]
	hitDH$zVAR <- hitDH$zVAA - hit1B$zVAA[20]	
	hit <- rbind(hitC,hit1B,hit2B,hitSS,hit3B,hitOF,hitDH)
	hit <- hit[order(hit$zVAR, decreasing=TRUE),]
}

hit <- unique(hit)

#hit[1:20, c("Name", "POS", "AB", "zR", "zHR", "zRBI", "zSB", "zAVG", "zTOT", "zVAA", "zVAR")]


## All Pitchers

for (i in 1:10) {
	pit$zK <- (pit$K-mean(pit$K[1:90]))/sd(pit$K[1:90])
	pit$zW <- (pit$W-mean(pit$W[1:90]))/sd(pit$W[1:90])
	pit$xER <- -(pit$ER - (pit$IP/9)*mean(pit$ERA[1:90]))
	pit$zERA <- (pit$xER - mean(pit$xER[1:90]))/sd(pit$xER[1:90])
	pit$xWH <- -(pit$WH - pit$IP*mean(pit$WHIP[1:90]))
	pit$zWHIP <- (pit$xWH - mean(pit$xWH[1:90]))/sd(pit$xWH[1:90])
	pit$zSV <- (pit$SV-mean(pit$SV[1:90]))/(sd(pit$SV[1:90])+.0001)
	pit$zTOT <- pit$zK + pit$zW + pit$zERA + pit$zWHIP + pit$zSV
	pit <- pit[order(pit$zTOT, decreasing=TRUE),]
	pit$POS <- as.character(pit$POS)
	pit$POS[pit$IP>135] <- "SP"
	pit$POS[pit$SV>5] <- "RP"
	pitSP <- subset(pit, pit$POS=="SP")
	pitRP <- subset(pit, pit$POS=="RP")
	pitSP$zVAA <- pitSP$zTOT - mean(pitSP$zTOT[1:60])
	pitRP$zVAA <- pitRP$zTOT - mean(pitRP$zTOT[1:30])
	pitSP$zVAR <- pitSP$zVAA - pitSP$zVAA[60]
	pitRP$zVAR <- pitRP$zVAA - pitRP$zVAA[30]
	pit <- rbind(pitSP, pitRP)
	pit <- pit[order(pit$zVAR, decreasing=TRUE),]
}

pit <- unique(pit)

#pit[1:40, c("Name", "POS", "zK", "zW", "zERA", "zWHIP", "zSV", "zTOT", "zVAA", "zVAR")]



##############################################################
## All Player Rankings
##############################################################

rank <- merge(hit, pit, by=c("Name","POS","zTOT","zVAA", "zVAR"), all=TRUE)
adp <- read.csv("~/Dropbox/Baseball/ADP.csv")
rank <- merge(rank, adp, by="Name", all.x=TRUE)

rank <- rank[order(rank$zVAR, decreasing=TRUE),]
rank <- rank[1:500,]
rownames(rank) <- rank$Name

rankC <- subset(rank, rank$POS=="C")
rank1B <- subset(rank, rank$POS=="1B")
rank2B <- subset(rank, rank$POS=="2B")
rankSS <- subset(rank, rank$POS=="SS")
rank3B <- subset(rank, rank$POS=="3B")
rankOF <- subset(rank, rank$POS=="OF")
rankDH <- subset(rank, rank$POS=="DH")
rankSP <- subset(rank, rank$POS=="SP")
rankRP <- subset(rank, rank$POS=="RP")

rankC$POSRANK <- c()
for(i in 1:length(rankC$Name)){
	rankC$POSRANK[i] <- i
}

rank1B$POSRANK <- c()
for(i in 1:length(rank1B$Name)){
	rank1B$POSRANK[i] <- i
}

rank2B$POSRANK <- c()
for(i in 1:length(rank2B$Name)){
	rank2B$POSRANK[i] <- i
}

rankSS$POSRANK <- c()
for(i in 1:length(rankSS$Name)){
	rankSS$POSRANK[i] <- i
}

rank3B$POSRANK <- c()
for(i in 1:length(rank3B$Name)){
	rank3B$POSRANK[i] <- i
}

rankOF$POSRANK <- c()
for(i in 1:length(rankOF$Name)){
	rankOF$POSRANK[i] <- i
}

rankDH$POSRANK <- c()
for(i in 1:length(rankDH$Name)){
	rankDH$POSRANK[i] <- i
}

rankSP$POSRANK <- c()
for(i in 1:length(rankSP$Name)){
	rankSP$POSRANK[i] <- i
}

rankRP$POSRANK <- c()
for(i in 1:length(rankRP$Name)){
	rankRP$POSRANK[i] <- i
}

rankHIT <- rbind(rankC,rank1B,rank2B,rankSS,rank3B,rankOF,rankDH)
rankHIT <- rankHIT[order(rankHIT$zVAR, decreasing=TRUE),]
rankHIT$VALUE <- rankHIT$zVAR*(2350*.7/sum(c(rankC$zVAR[1:10], rank1B$zVAR[1:20], rank2B$zVAR[1:15], rankSS$zVAR[1:15], rank3B$zVAR[1:18], rankOF$zVAR[1:52]))) + 1

rankPIT <- rbind(rankSP,rankRP)
rankPIT <- rankPIT[order(rankPIT$zVAR, decreasing=TRUE),]
rankPIT$VALUE <- rankPIT$zVAR*(2350*.3/sum(rankPIT$zVAR[1:90])) + 1

rank <- rbind(rankHIT, rankPIT)


rank <- rank[order(rank$VALUE, decreasing=TRUE),]
rank$VALUE <- round(rank$VALUE,1)

rank$RANK <- c()
for(i in 1:length(rank$Name)){
	rank$RANK[i] <- i
}

rank[1:300, c("POS", "RANK", "ADP", "POSRANK", "PA", "AVG", "R", "HR", "RBI", "SB", "IP", "W", "K", "ERA", "WHIP", "SV", "zTOT", "VALUE")]


rank[rank$POS=="SP"|rank$POS=="RP", c("POS", "RANK", "ADP", "POSRANK", "PA", "AVG", "R", "HR", "RBI", "SB", "IP", "W", "K", "ERA", "WHIP", "SV", "zTOT", "VALUE")]

rank[rank$POS=="RP", c("POS", "RANK", "ADP", "POSRANK", "PA", "AVG", "R", "HR", "RBI", "SB", "IP", "W", "K", "ERA", "WHIP", "SV", "zTOT", "VALUE")]




## Graph of value dropoff by position
qplot(POSRANK, VALUE, data=rank[1:250,], col=POS, facets= ~ POS, alpha=.5)


## Update draft list during draft

draft <- tbl_df(draft)
draft <- select(draft, Name, RANK, ADP, POS, POSRANK, VALUE, zTOT, PA, AVG, R, HR, RBI, SB, IP, W, K, ERA, WHIP, SV)
draft <- arrange(draft, desc(VALUE))
View(draft)

ldraft <- draft
ldraft <- filter(ldraft, Name!="Mike Trout")
View(ldraft)

## Summarize statistics of team
andrew <- tbl_df(rank[c("Jonathan Lucroy", "Miguel Cabrera", "Dee Gordon", "Adrian Beltre", "Troy Tulowitzki", "Starlin Castro", "Ryan Zimmerman", "Corey Dickerson", "Nelson Cruz", "Jorge Soler", "Ben Zobrist", "Wil Myers", "Kris Bryant", "Jordan Zimmermann", "Masahiro Tanaka", "Lance Lynn", "Justin Verlander", "Jake Odorizzi", "Shelby Miller", "Craig Kimbrel", "Steve Cishek", "Zach Britton"), c("Name", "PA", "AB", "AVG", "R", "HR", "RBI", "SB", "H", "IP", "W", "K", "ERA", "WHIP", "SV", "ER", "WH", "zTOT", "VALUE")])

andrew <- mutate(andrew, H = AVG*AB, ER = ERA*IP, WH = WHIP*IP)

andrew.team <- summarize(andrew, team = "Andrew", totalAVG = sum(H, na.rm=T)/sum(AB, na.rm=T), totalR = sum(R, na.rm=T),  totalHR = sum(HR, na.rm=T), totalRBI = sum(RBI, na.rm=T), totalSB = sum(SB, na.rm=T), totalW = sum(W, na.rm=T), totalK = sum(K, na.rm=T), totalSV = sum(SV, na.rm=T), totalERA = sum(ER, na.rm=T)/sum(IP, na.rm=T), totalWHIP = sum(WH, na.rm=T)/sum(IP, na.rm=T))

max <- tbl_df(rank[c("Wilin Rosario", "Adam LaRoche", "Jason Kipnis", "Nolan Arenado", "Ian Desmond", "Erick Aybar", "Brandon Belt", "Jose Bautista", "Adam Jones", "Carlos Gonzalez", "Jay Bruce", "Mookie Betts", "Rusney Castillo", "David Price", "Jacob deGrom", "Garrett Richards", "Chris Archer", "Marcus Stroman", "Kevin Gausman", "Aroldis Chapman", "Huston Street", "Hector Rondon"), c("Name", "PA", "AB", "AVG", "R", "HR", "RBI", "SB", "H", "IP", "W", "K", "ERA", "WHIP", "SV", "ER", "WH", "zTOT", "VALUE")])

max <- mutate(max, H = AVG*AB, ER = ERA*IP, WH = WHIP*IP)

max.team <- summarize(max, team = "Max", totalAVG = sum(H, na.rm=T)/sum(AB, na.rm=T), totalR = sum(R, na.rm=T),  totalHR = sum(HR, na.rm=T), totalRBI = sum(RBI, na.rm=T), totalSB = sum(SB, na.rm=T), totalW = sum(W, na.rm=T), totalK = sum(K, na.rm=T), totalSV = sum(SV, na.rm=T), totalERA = sum(ER, na.rm=T)/sum(IP, na.rm=T), totalWHIP = sum(WH, na.rm=T)/sum(IP, na.rm=T))

ingrid <- tbl_df(rank[c("Buster Posey", "Adrian Gonzalez", "Anthony Rendon", "Matt Carpenter", "Elvis Andrus", "Kolten Wong", "Matt Adams", "Justin Upton", "Matt Kemp", "Alex Rios", "Lorenzo Cain", "Coco Crisp", "Carlos Beltran", "Clayton Kershaw", "Jeff Samardzija", "James Shields", "Ian Kennedy", "Jose Quintana", "Danny Salazar", "Trevor Rosenthal", "Joaquin Benoit", "Jake McGee"), c("Name", "PA", "AB", "AVG", "R", "HR", "RBI", "SB", "H", "IP", "W", "K", "ERA", "WHIP", "SV", "ER", "WH", "zTOT", "VALUE")])

ingrid <- mutate(ingrid, H = AVG*AB, ER = ERA*IP, WH = WHIP*IP)

ingrid.team <- summarize(ingrid, team = "Ingrid", totalAVG = sum(H, na.rm=T)/sum(AB, na.rm=T), totalR = sum(R, na.rm=T),  totalHR = sum(HR, na.rm=T), totalRBI = sum(RBI, na.rm=T), totalSB = sum(SB, na.rm=T), totalW = sum(W, na.rm=T), totalK = sum(K, na.rm=T), totalSV = sum(SV, na.rm=T), totalERA = sum(ER, na.rm=T)/sum(IP, na.rm=T), totalWHIP = sum(WH, na.rm=T)/sum(IP, na.rm=T))

ashley <- tbl_df(rank[c("Evan Gattis", "Paul Goldschmidt", "Robinson Cano", "Evan Longoria", "Hanley Ramirez", "Dustin Pedroia", "Kyle Seager", "Jacoby Ellsbury", "Hunter Pence", "Mark Trumbo", "Leonys Martin", "A.J. Pollock", "Chris Carter", "Tyson Ross", "Carlos Carrasco", "Mat Latos", "Matt Cain", "Dallas Keuchel", "R.A. Dickey", "Glen Perkins", "Joe Nathan", "Tyler Clippard"), c("Name", "PA", "AB", "AVG", "R", "HR", "RBI", "SB", "H", "IP", "W", "K", "ERA", "WHIP", "SV", "ER", "WH", "zTOT", "VALUE")])

ashley <- mutate(ashley, H = AVG*AB, ER = ERA*IP, WH = WHIP*IP)

ashley.team <- summarize(ashley, team = "Ashley", totalAVG = sum(H, na.rm=T)/sum(AB, na.rm=T), totalR = sum(R, na.rm=T),  totalHR = sum(HR, na.rm=T), totalRBI = sum(RBI, na.rm=T), totalSB = sum(SB, na.rm=T), totalW = sum(W, na.rm=T), totalK = sum(K, na.rm=T), totalSV = sum(SV, na.rm=T), totalERA = sum(ER, na.rm=T)/sum(IP, na.rm=T), totalWHIP = sum(WH, na.rm=T)/sum(IP, na.rm=T))

alison <- tbl_df(rank[c("Yadier Molina", "Edwin Encarnacion", "Brian Dozier", "Pedro Alvarez", "Jose Reyes", "Chase Utley", "Joey Votto", "Yasiel Puig", "Starling Marte", "Kole Calhoun", "Marcell Ozuna", "Brandon Moss", "Oswaldo Arcia", "Stephen Strasburg", "Yu Darvish", "Hyun-Jin Ryu", "Homer Bailey", "Collin McHugh", "Scott Kazmir", "David Robertson", "Fernando Rodney", "Jenrry Mejia"), c("Name", "PA", "AB", "AVG", "R", "HR", "RBI", "SB", "H", "IP", "W", "K", "ERA", "WHIP", "SV", "ER", "WH", "zTOT", "VALUE")])

alison <- mutate(alison, H = AVG*AB, ER = ERA*IP, WH = WHIP*IP)

alison.team <- summarize(alison, team = "Alison", totalAVG = sum(H, na.rm=T)/sum(AB, na.rm=T), totalR = sum(R, na.rm=T),  totalHR = sum(HR, na.rm=T), totalRBI = sum(RBI, na.rm=T), totalSB = sum(SB, na.rm=T), totalW = sum(W, na.rm=T), totalK = sum(K, na.rm=T), totalSV = sum(SV, na.rm=T), totalERA = sum(ER, na.rm=T)/sum(IP, na.rm=T), totalWHIP = sum(WH, na.rm=T)/sum(IP, na.rm=T))

derek <- tbl_df(rank[c("Salvador Perez", "Lucas Duda", "Rougned Odor", "Carlos Santana", "Xander Bogaerts", "Jedd Gyorko", "Eric Hosmer", "Carlos Gomez", "Ryan Braun", "Brett Gardner", "Curtis Granderson", "Danny Santana", "Justin Morneau", "Felix Hernandez", "Corey Kluber", "Jon Lester", "Matt Harvey", "Sonny Gray", "Gio Gonzalez", "Greg Holland", "Jonathan Papelbon", "Wade Davis"), c("Name", "PA", "AB", "AVG", "R", "HR", "RBI", "SB", "H", "IP", "W", "K", "ERA", "WHIP", "SV", "ER", "WH", "zTOT", "VALUE")])

derek <- mutate(derek, H = AVG*AB, ER = ERA*IP, WH = WHIP*IP)

derek.team <- summarize(derek, team = "Derek", totalAVG = sum(H, na.rm=T)/sum(AB, na.rm=T), totalR = sum(R, na.rm=T),  totalHR = sum(HR, na.rm=T), totalRBI = sum(RBI, na.rm=T), totalSB = sum(SB, na.rm=T), totalW = sum(W, na.rm=T), totalK = sum(K, na.rm=T), totalSV = sum(SV, na.rm=T), totalERA = sum(ER, na.rm=T)/sum(IP, na.rm=T), totalWHIP = sum(WH, na.rm=T)/sum(IP, na.rm=T))

alex <- tbl_df(rank[c("Brian McCann", "Jose Abreu", "Neil Walker", "Pablo Sandoval", "Alcides Escobar", "Asdrubal Cabrera", "David Wright", "Alex Gordon", "Jayson Werth", "Denard Span", "Shin-Soo Choo", "Torii Hunter", "David Ortiz", "Max Scherzer", "Madison Bumgarner", "Zack Greinke", "Adam Wainwright", "Julio Teheran", "Cliff Lee", "Kenley Jansen", "Koji Uehara", "Ken Giles"), c("Name", "PA", "AB", "AVG", "R", "HR", "RBI", "SB", "H", "IP", "W", "K", "ERA", "WHIP", "SV", "ER", "WH", "zTOT", "VALUE")])

alex <- mutate(alex, H = AVG*AB, ER = ERA*IP, WH = WHIP*IP)

alex.team <- summarize(alex, team = "Alex", totalAVG = sum(H, na.rm=T)/sum(AB, na.rm=T), totalR = sum(R, na.rm=T),  totalHR = sum(HR, na.rm=T), totalRBI = sum(RBI, na.rm=T), totalSB = sum(SB, na.rm=T), totalW = sum(W, na.rm=T), totalK = sum(K, na.rm=T), totalSV = sum(SV, na.rm=T), totalERA = sum(ER, na.rm=T)/sum(IP, na.rm=T), totalWHIP = sum(WH, na.rm=T)/sum(IP, na.rm=T))

ted <- tbl_df(rank[c("Yan Gomes", "Anthony Rizzo", "Martin Prado", "Manny Machado", "Alexei Ramirez", "Brett Lawrie", "Prince Fielder", "Giancarlo Stanton", "Michael Brantley", "Bryce Harper", "Yoenis Cespedes", "Josh Harrison", "Gregory Polanco", "Cole Hamels", "Gerrit Cole", "Jake Arrieta", "Jered Weaver", "Jose Fernandez", "Aaron Sanchez", "Cody Allen", "Sean Doolittle", "Francisco Rodriguez"), c("Name", "PA", "AB", "AVG", "R", "HR", "RBI", "SB", "H", "IP", "W", "K", "ERA", "WHIP", "SV", "ER", "WH", "zTOT", "VALUE")])

ted <- mutate(ted, H = AVG*AB, ER = ERA*IP, WH = WHIP*IP)

ted.team <- summarize(ted, team = "Ted", totalAVG = sum(H, na.rm=T)/sum(AB, na.rm=T), totalR = sum(R, na.rm=T),  totalHR = sum(HR, na.rm=T), totalRBI = sum(RBI, na.rm=T), totalSB = sum(SB, na.rm=T), totalW = sum(W, na.rm=T), totalK = sum(K, na.rm=T), totalSV = sum(SV, na.rm=T), totalERA = sum(ER, na.rm=T)/sum(IP, na.rm=T), totalWHIP = sum(WH, na.rm=T)/sum(IP, na.rm=T))

aaron <- tbl_df(rank[c("Devin Mesoraco", "Freddie Freeman", "Ian Kinsler", "Chase Headley", "Jimmy Rollins", "Howie Kendrick", "Victor Martinez", "Mike Trout", "Billy Hamilton", "Charlie Blackmon", "Jason Heyward", "Christian Yelich", "J.D. Martinez", "Chris Sale", "Alex Cobb", "Alex Wood", "Michael Wacha", "Andrew Cashner", "Francisco Liriano", "Andrew Miller", "Addison Reed", "Pat Neshek"), c("Name", "PA", "AB", "AVG", "R", "HR", "RBI", "SB", "H", "IP", "W", "K", "ERA", "WHIP", "SV", "ER", "WH", "zTOT", "VALUE")])

aaron <- mutate(aaron, H = AVG*AB, ER = ERA*IP, WH = WHIP*IP)

aaron.team <- summarize(aaron, team = "Aaron", totalAVG = sum(H, na.rm=T)/sum(AB, na.rm=T), totalR = sum(R, na.rm=T),  totalHR = sum(HR, na.rm=T), totalRBI = sum(RBI, na.rm=T), totalSB = sum(SB, na.rm=T), totalW = sum(W, na.rm=T), totalK = sum(K, na.rm=T), totalSV = sum(SV, na.rm=T), totalERA = sum(ER, na.rm=T)/sum(IP, na.rm=T), totalWHIP = sum(WH, na.rm=T)/sum(IP, na.rm=T))

saul <- tbl_df(rank[c("Russell Martin", "Albert Pujols", "Jose Altuve", "Josh Donaldson", "Jean Segura", "Daniel Murphy", "Todd Frazier", "Andrew McCutchen", "George Springer", "Matt Holliday", "Melky Cabrera", "Ben Revere", "Chris Davis", "Johnny Cueto", "Hisashi Iwakuma", "Anibal Sanchez", "Drew Smyly", "Phil Hughes", "Michael Fiers", "Mark Melancon", "Dellin Betances", "Drew Storen"), c("Name", "PA", "AB", "AVG", "R", "HR", "RBI", "SB", "H", "IP", "W", "K", "ERA", "WHIP", "SV", "ER", "WH", "zTOT", "VALUE")])

saul <- mutate(saul, H = AVG*AB, ER = ERA*IP, WH = WHIP*IP)

saul.team <- summarize(saul, team = "Saul", totalAVG = sum(H, na.rm=T)/sum(AB, na.rm=T), totalR = sum(R, na.rm=T),  totalHR = sum(HR, na.rm=T), totalRBI = sum(RBI, na.rm=T), totalSB = sum(SB, na.rm=T), totalW = sum(W, na.rm=T), totalK = sum(K, na.rm=T), totalSV = sum(SV, na.rm=T), totalERA = sum(ER, na.rm=T)/sum(IP, na.rm=T), totalWHIP = sum(WH, na.rm=T)/sum(IP, na.rm=T))

projected_standings <- rbind(saul.team, aaron.team, andrew.team, ted.team, max.team, ingrid.team, alison.team, derek.team, alex.team, ashley.team)

projected_standings <- arrange(projected_standings, totalAVG)
projected_standings <- mutate(projected_standings, AVG.points = rownames(projected_standings))
projected_standings <- arrange(projected_standings, totalR)
projected_standings <- mutate(projected_standings, R.points = rownames(projected_standings))
projected_standings <- arrange(projected_standings, totalHR)
projected_standings <- mutate(projected_standings, HR.points = rownames(projected_standings))
projected_standings <- arrange(projected_standings, totalRBI)
projected_standings <- mutate(projected_standings, RBI.points = rownames(projected_standings))
projected_standings <- arrange(projected_standings, totalSB)
projected_standings <- mutate(projected_standings, SB.points = rownames(projected_standings))
projected_standings <- arrange(projected_standings, totalW)
projected_standings <- mutate(projected_standings, W.points = rownames(projected_standings))
projected_standings <- arrange(projected_standings, totalK)
projected_standings <- mutate(projected_standings, K.points = rownames(projected_standings))
projected_standings <- arrange(projected_standings, totalSV)
projected_standings <- mutate(projected_standings, SV.points = rownames(projected_standings))
projected_standings <- arrange(projected_standings, desc(totalERA))
projected_standings <- mutate(projected_standings, ERA.points = rownames(projected_standings))
projected_standings <- arrange(projected_standings, desc(totalWHIP))
projected_standings <- mutate(projected_standings, WHIP.points = rownames(projected_standings))
team_points <- select(projected_standings, team, AVG.points, R.points, HR.points, RBI.points, SB.points, W.points, K.points, SV.points, ERA.points, WHIP.points)

team_rankings <- data.frame(team_points, row.names = team_points$team)
drops <- c("team")
team_rankings <- team_rankings[,!(names(team_rankings) %in% drops)]
for (i in 1:nrow(team_rankings)) {
	team_rankings$TOTAL.points[i] <- as.numeric(team_rankings$AVG.points[i]) + as.numeric(team_rankings$R.points[i]) + as.numeric(team_rankings$HR.points[i]) + as.numeric(team_rankings$RBI.points[i]) + as.numeric(team_rankings$SB.points[i]) + as.numeric(team_rankings$W.points[i]) + as.numeric(team_rankings$K.points[i]) + as.numeric(team_rankings$SV.points[i]) + as.numeric(team_rankings$ERA.points[i]) + as.numeric(team_rankings$WHIP.points[i])
}
team_rankings <- team_rankings[order(team_rankings$TOTAL.points, decreasing=TRUE),]
team_rankings
