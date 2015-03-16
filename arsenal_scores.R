pit <- read.csv("~/Dropbox/Baseball/Hardball Times/4Saul_tonsOfPitcherInfo_try2.csv")
pit <- subset(pit, pit$yer==2014)

pit <- subset(pit, pit$IP>40)

pit[is.na(pit)] <- 0
pit$POS <- NULL

for (i in 1:nrow(pit)) {
	if (pit$perc_FS[i] > 0.03) {
		pit$FS[i] <- 1
	}
	else {
		pit$FS[i] <- 0
	}
	if (pit$perc_CU[i] > 0.03) {
		pit$CU[i] <- 1
	}
	else {
		pit$CU[i] <- 0
	}
	if (pit$perc_SI[i] > 0.03) {
		pit$SI[i] <- 1
	}
	else {
		pit$SI[i] <- 0
	}
	if (pit$perc_FT[i] > 0.03) {
		pit$FT[i] <- 1
	}
	else {
		pit$FT[i] <- 0
	}
	if (pit$perc_FF[i] > 0.03) {
		pit$FF[i] <- 1
	}
	else {
		pit$FF[i] <- 0
	}
	if (pit$perc_CH[i] > 0.03) {
		pit$CH[i] <- 1
	}
	else {
		pit$CH[i] <- 0
	}
	if (pit$perc_SL[i] > 0.03) {
		pit$SL[i] <- 1
	}
	else {
		pit$SL[i] <- 0
	}
	if (pit$perc_FC[i] > 0.03) {
		pit$FC[i] <- 1
	}
	else {
		pit$FC[i] <- 0
	}
	if (pit$perc_KN[i] > 0.03) {
		pit$KN[i] <- 1
	}
	else {
		pit$KN[i] <- 0
	}
	if (pit$perc_FA[i] > 0.03) {
		pit$FA[i] <- 1
	}
	else {
		pit$FA[i] <- 0
	}
	if (pit$perc_KC[i] > 0.03) {
		pit$KC[i] <- 1
	}
	else {
		pit$KC[i] <- 0
	}
	if (pit$IP[i] > 100) {
		pit$POS[i] <- 1
	}
	else {
		pit$POS[i] <- 0
	}
	pit$FS_ss[i] <- pit$perc_FS[i] * pit$swgstr_FS[i]
	pit$FS_gb[i] <- pit$perc_FS[i] * pit$GB_perc_FS[i]
    pit$FS_zp[i] <- pit$perc_FS[i] * pit$ZonePercent_FS[i]
	pit$CU_ss[i] <- pit$perc_CU[i] * pit$swgstr_CU[i]
	pit$CU_gb[i] <- pit$perc_CU[i] * pit$GB_perc_CU[i]
	pit$CU_zp[i] <- pit$perc_CU[i] * pit$ZonePercent_CU[i]
	pit$SI_ss[i] <- pit$perc_SI[i] * pit$swgstr_SI[i]
	pit$SI_gb[i] <- pit$perc_SI[i] * pit$GB_perc_SI[i]
	pit$SI_zp[i] <- pit$perc_SI[i] * pit$ZonePercent_SI[i]
	pit$FT_ss[i] <- pit$perc_FT[i] * pit$swgstr_FT[i]
	pit$FT_gb[i] <- pit$perc_FT[i] * pit$GB_perc_FT[i]
	pit$FT_zp[i] <- pit$perc_FT[i] * pit$ZonePercent_FT[i]
	pit$FF_ss[i] <- pit$perc_FF[i] * pit$swgstr_FF[i]
	pit$FF_gb[i] <- pit$perc_FF[i] * pit$GB_perc_FF[i]
	pit$FF_zp[i] <- pit$perc_FF[i] * pit$ZonePercent_FF[i]
	pit$CH_ss[i] <- pit$perc_CH[i] * pit$swgstr_CH[i]
	pit$CH_gb[i] <- pit$perc_CH[i] * pit$GB_perc_CH[i]
	pit$CH_zp[i] <- pit$perc_CH[i] * pit$ZonePercent_CH[i]
	pit$SL_ss[i] <- pit$perc_SL[i] * pit$swgstr_SL[i]
	pit$SL_gb[i] <- pit$perc_SL[i] * pit$GB_perc_SL[i]
	pit$SL_zp[i] <- pit$perc_SL[i] * pit$ZonePercent_SL[i]
	pit$FC_ss[i] <- pit$perc_FC[i] * pit$swgstr_FC[i]
	pit$FC_gb[i] <- pit$perc_FC[i] * pit$GB_perc_FC[i]
	pit$FC_zp[i] <- pit$perc_FC[i] * pit$ZonePercent_FC[i]
	pit$KN_ss[i] <- pit$perc_KN[i] * pit$swgstr_KN[i]
	pit$KN_gb[i] <- pit$perc_KN[i] * pit$GB_perc_KN[i]
	pit$KN_zp[i] <- pit$perc_KN[i] * pit$ZonePercent_KN[i]
	pit$FA_ss[i] <- pit$perc_FA[i] * pit$swgstr_FA[i]
	pit$FA_gb[i] <- pit$perc_FA[i] * pit$GB_perc_FA[i]
	pit$FA_zp[i] <- pit$perc_FA[i] * pit$ZonePercent_FA[i]
	pit$KC_ss[i] <- pit$perc_KC[i] * pit$swgstr_KC[i]
	pit$KC_gb[i] <- pit$perc_KC[i] * pit$GB_perc_KC[i]
	pit$KC_zp[i] <- pit$perc_KC[i] * pit$ZonePercent_KC[i]
}

model <- lm(ERA ~ FS + FS_ss + FS_gb + FS_zp + CU + CU_ss + CU_gb + CU_zp + SI + SI_ss + SI_gb + SI_zp + FT + FT_ss + FT_gb + FT_zp + FF + FF_ss + FF_gb + FF_zp + CH + CH_ss + CH_gb + CH_zp + SL + SL_ss + SL_gb + SL_zp + FC + FC_ss + FC_gb + FC_zp + FA + FA_ss + FA_gb + FA_zp + KC + KC_ss + KC_gb + KC_zp + POS, data = pit)
reg <- summary(model)

regc <- reg$coefficients[,1]

for(i in 1:nrow(pit)){
        pit$aERA[i] <- regc[1] + regc[2]*pit$perc_FS[i] + regc[3]*pit$FS_ss[i] + regc[4]*pit$FS_gb[i] + regc[5]*pit$FS_zp[i] + regc[6]*pit$perc_CU[i] + regc[7]*pit$CU_ss[i] + regc[8]*pit$CU_gb[i] + regc[9]*pit$CU_zp[i] + regc[10]*pit$perc_SI[i] + regc[11]*pit$SI_ss[i] + regc[12]*pit$SI_gb[i] + regc[13]*pit$SI_zp[i] + regc[14]*pit$perc_FT[i] + regc[15]*pit$FT_ss[i] + regc[16]*pit$FT_gb[i] + regc[17]*pit$FT_zp[i] + regc[18]*pit$perc_FF[i] + regc[19]*pit$FF_ss[i] + regc[20]*pit$FF_gb[i] + regc[21]*pit$FF_zp[i] + regc[22]*pit$perc_CH[i] + regc[23]*pit$CH_ss[i] + regc[24]*pit$CH_gb[i] + regc[25]*pit$CH_zp[i] + regc[26]*pit$perc_SL[i] + regc[27]*pit$SL_ss[i] + regc[28]*pit$SL_gb[i] + regc[29]*pit$SL_zp[i] + regc[30]*pit$perc_FC[i] + regc[31]*pit$FC_ss[i] + regc[32]*pit$FC_gb[i] + regc[33]*pit$FC_zp[i] + regc[34]*pit$perc_FA[i] + regc[35]*pit$FA_ss[i] + regc[36]*pit$FA_gb[i] + regc[37]*pit$FA_zp[i] + regc[38]*pit$perc_KC[i] + regc[39]*pit$KC_ss[i] + regc[40]*pit$KC_gb[i] + regc[41]*pit$KC_zp[i] + regc[42]*pit$POS[i]
}

arsenal <- pit[order(pit$aERA),]
arsenal <- arsenal[, c("pitcher", "IP", "ERA", "xFIP", "SIERA", "aERA")]
head(arsenal[arsenal$IP>100,], 30)
head(arsenal[arsenal$IP<100,], 30)

