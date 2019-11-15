CRCCountryCohort <- rownames(combined_short_metadata_2[combined_short_metadata_2$country %in% droplevels(combined_short_metadata_2[CRCIndividuals,"country"]),])
AdenomaCountryCohort <- rownames(combined_short_metadata_2[combined_short_metadata_2$country %in% droplevels(combined_short_metadata_2[AdenomaIndividuals,"country"]),])
#T2DCountryCohort <- rownames(combined_short_metadata_2[combined_short_metadata_2$country %in% droplevels(combined_short_metadata_2[T2DIndividuals,"country"]),])
CirrhosisCountryCohort <- rownames(combined_short_metadata_2[combined_short_metadata_2$country %in% droplevels(combined_short_metadata_2[CirrhosisIndividuals,"country"]),])
IBDCountryCohort <- rownames(combined_short_metadata_2[combined_short_metadata_2$country %in% droplevels(combined_short_metadata_2[IBDIndividuals,"country"]),])
T2DCountryCohort <- c(intersect(c(Young,Middle),AsiaIndividuals),intersect(Elderly,c(AllEUIndividuals,AsiaIndividuals)))

print("Comparing")
WilcoxComparisonCountryCohortIBDElderly <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(IBDCountryCohort,intersect(IBDIndividuals,c(Elderly))),core_species]),t(combined_species_profile_with_age_country_final[intersect(IBDCountryCohort,intersect(SelectControls,c(Elderly))),core_species]))
WilcoxComparisonCountryCohortIBDMiddle <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(IBDCountryCohort,intersect(IBDIndividuals,c(Middle))),core_species]),t(combined_species_profile_with_age_country_final[intersect(IBDCountryCohort,intersect(SelectControls,c(Middle))),core_species]))
WilcoxComparisonCountryCohortIBDYoung <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(IBDCountryCohort,intersect(IBDIndividuals,c(Young))),core_species]),t(combined_species_profile_with_age_country_final[intersect(IBDCountryCohort,intersect(SelectControls,c(Young))),core_species]))

WilcoxComparisonCountryCohortT2DElderly <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(T2DCountryCohort,intersect(T2DIndividuals,c(Elderly))),core_species]),t(combined_species_profile_with_age_country_final[intersect(T2DCountryCohort,intersect(SelectControls,c(Elderly))),core_species]))
WilcoxComparisonCountryCohortT2DMiddle <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(T2DCountryCohort,intersect(T2DIndividuals,c(Middle))),core_species]),t(combined_species_profile_with_age_country_final[intersect(T2DCountryCohort,intersect(SelectControls,c(Middle))),core_species]))
WilcoxComparisonCountryCohortT2DYoung <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(T2DCountryCohort,intersect(T2DIndividuals,c(Young))),core_species]),t(combined_species_profile_with_age_country_final[intersect(T2DCountryCohort,intersect(SelectControls,c(Young))),core_species]))

WilcoxComparisonCountryCohortCirrhosisElderly <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CirrhosisCountryCohort,intersect(CirrhosisIndividuals,c(Elderly))),core_species]),t(combined_species_profile_with_age_country_final[intersect(CirrhosisCountryCohort,intersect(SelectControls,c(Elderly))),core_species]))
WilcoxComparisonCountryCohortCirrhosisMiddle <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CirrhosisCountryCohort,intersect(CirrhosisIndividuals,c(Middle))),core_species]),t(combined_species_profile_with_age_country_final[intersect(CirrhosisCountryCohort,intersect(SelectControls,c(Middle))),core_species]))
WilcoxComparisonCountryCohortCirrhosisYoung <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CirrhosisCountryCohort,intersect(CirrhosisIndividuals,c(Young))),core_species]),t(combined_species_profile_with_age_country_final[intersect(CirrhosisCountryCohort,intersect(SelectControls,c(Young))),core_species]))


WilcoxComparisonCountryCohortCRCElderly <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CRCCountryCohort,intersect(CRCIndividuals,c(Elderly))),core_species]),t(combined_species_profile_with_age_country_final[intersect(CRCCountryCohort,intersect(SelectControls,c(Elderly))),core_species]))
WilcoxComparisonCountryCohortCRCYoungMiddle <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CRCCountryCohort,intersect(CRCIndividuals,c(Young,Middle))),core_species]),t(combined_species_profile_with_age_country_final[intersect(CRCCountryCohort,intersect(SelectControls,c(Young,Middle))),core_species]))

WilcoxComparisonCountryCohortAdenomaElderly <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(AdenomaCountryCohort,intersect(AdenomaIndividuals,c(Elderly))),core_species]),t(combined_species_profile_with_age_country_final[intersect(AdenomaCountryCohort,intersect(SelectControls,c(Elderly))),core_species]))
WilcoxComparisonCountryCohortAdenomaYoungMiddle <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(AdenomaCountryCohort,intersect(AdenomaIndividuals,c(Young,Middle))),core_species]),t(combined_species_profile_with_age_country_final[intersect(AdenomaCountryCohort,intersect(SelectControls,c(Young,Middle))),core_species]))

print("Merging")

temp0 <- merge(WilcoxComparisonCountryCohortIBDYoung[WilcoxComparisonCountryCohortIBDYoung[,3] < 0.1,2],WilcoxComparisonCountryCohortIBDMiddle[WilcoxComparisonCountryCohortIBDMiddle[,3] < 0.1,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonCountryCohortIBDYoung[WilcoxComparisonCountryCohortIBDYoung[,3] < 0.1,2],WilcoxComparisonCountryCohortIBDMiddle[WilcoxComparisonCountryCohortIBDMiddle[,3] < 0.1,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

temp1 <- merge(temp0,WilcoxComparisonCountryCohortIBDElderly[WilcoxComparisonCountryCohortIBDElderly[,3] < 0.1,2],by="row.names",all=TRUE)[,-1]
rownames(temp1) <- merge(temp0,WilcoxComparisonCountryCohortIBDElderly[WilcoxComparisonCountryCohortIBDElderly[,3] < 0.1,2],by="row.names",all=TRUE)[,1]
temp1 <- apply(temp1,2,function(x)(ifelse(is.na(x),0,x)))

IBDMarkerDirectionsCountryCohort <- temp1

temp0 <- merge(WilcoxComparisonCountryCohortT2DYoung[WilcoxComparisonCountryCohortT2DYoung[,3] < 0.1,2],WilcoxComparisonCountryCohortT2DMiddle[WilcoxComparisonCountryCohortT2DMiddle[,3] < 0.1,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonCountryCohortT2DYoung[WilcoxComparisonCountryCohortT2DYoung[,3] < 0.1,2],WilcoxComparisonCountryCohortT2DMiddle[WilcoxComparisonCountryCohortT2DMiddle[,3] < 0.1,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

temp1 <- merge(temp0,WilcoxComparisonCountryCohortT2DElderly[WilcoxComparisonCountryCohortT2DElderly[,3] < 0.1,2],by="row.names",all=TRUE)[,-1]
rownames(temp1) <- merge(temp0,WilcoxComparisonCountryCohortT2DElderly[WilcoxComparisonCountryCohortT2DElderly[,3] < 0.1,2],by="row.names",all=TRUE)[,1]
temp1 <- apply(temp1,2,function(x)(ifelse(is.na(x),0,x)))

T2DMarkerDirectionsCountryCohort <- temp1

temp0 <- merge(WilcoxComparisonCountryCohortCirrhosisYoung[WilcoxComparisonCountryCohortCirrhosisYoung[,3] < 0.1,2],WilcoxComparisonCountryCohortCirrhosisMiddle[WilcoxComparisonCountryCohortCirrhosisMiddle[,3] < 0.1,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonCountryCohortCirrhosisYoung[WilcoxComparisonCountryCohortCirrhosisYoung[,3] < 0.1,2],WilcoxComparisonCountryCohortCirrhosisMiddle[WilcoxComparisonCountryCohortCirrhosisMiddle[,3] < 0.1,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

temp1 <- merge(temp0,WilcoxComparisonCountryCohortCirrhosisElderly[WilcoxComparisonCountryCohortCirrhosisElderly[,3] < 0.1,2],by="row.names",all=TRUE)[,-1]
rownames(temp1) <- merge(temp0,WilcoxComparisonCountryCohortCirrhosisElderly[WilcoxComparisonCountryCohortCirrhosisElderly[,3] < 0.1,2],by="row.names",all=TRUE)[,1]
temp1 <- apply(temp1,2,function(x)(ifelse(is.na(x),0,x)))

CirrhosisMarkerDirectionsCountryCohort <- temp1


temp0 <- merge(WilcoxComparisonCountryCohortCRCYoungMiddle[WilcoxComparisonCountryCohortCRCYoungMiddle[,3] < 0.1,2],WilcoxComparisonCountryCohortCRCElderly[WilcoxComparisonCountryCohortCRCElderly[,3] < 0.1,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonCountryCohortCRCYoungMiddle[WilcoxComparisonCountryCohortCRCYoungMiddle[,3] < 0.1,2],WilcoxComparisonCountryCohortCRCElderly[WilcoxComparisonCountryCohortCRCElderly[,3] < 0.1,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

CRCMarkerDirectionsCountryCohort <- temp0


temp0 <- merge(WilcoxComparisonCountryCohortAdenomaYoungMiddle[WilcoxComparisonCountryCohortAdenomaYoungMiddle[,3] < 0.1,2],WilcoxComparisonCountryCohortAdenomaElderly[WilcoxComparisonCountryCohortAdenomaElderly[,3] < 0.1,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonCountryCohortAdenomaYoungMiddle[WilcoxComparisonCountryCohortAdenomaYoungMiddle[,3] < 0.1,2],WilcoxComparisonCountryCohortAdenomaElderly[WilcoxComparisonCountryCohortAdenomaElderly[,3] < 0.1,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

AdenomaMarkerDirectionsCountryCohort <- temp0

colnames(IBDMarkerDirectionsCountryCohort) <- c("Young","Middle","Elderly")
colnames(T2DMarkerDirectionsCountryCohort) <- c("Young","Middle","Elderly")
colnames(CirrhosisMarkerDirectionsCountryCohort) <- c("Young","Middle","Elderly")
colnames(CRCMarkerDirectionsCountryCohort) <- c("Middle","Elderly")
colnames(AdenomaMarkerDirectionsCountryCohort) <- c("Middle","Elderly")

temp0 <- merge(T2DMarkerDirectionsCountryCohort,IBDMarkerDirectionsCountryCohort,by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(T2DMarkerDirectionsCountryCohort,IBDMarkerDirectionsCountryCohort,by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

temp1 <- merge(temp0,CirrhosisMarkerDirectionsCountryCohort,by="row.names",all=TRUE)[,-1]
rownames(temp1) <- merge(temp0,CirrhosisMarkerDirectionsCountryCohort,by="row.names",all=TRUE)[,1]
temp1 <- apply(temp1,2,function(x)(ifelse(is.na(x),0,x)))

temp2 <- merge(temp1,CRCMarkerDirectionsCountryCohort,by="row.names",all=TRUE)[,-1]
rownames(temp2) <- merge(temp1,CRCMarkerDirectionsCountryCohort,by="row.names",all=TRUE)[,1]
temp2 <- apply(temp2,2,function(x)(ifelse(is.na(x),0,x)))

temp3 <- merge(temp2,AdenomaMarkerDirectionsCountryCohort,by="row.names",all=TRUE)[,-1]
rownames(temp3) <- merge(temp2,AdenomaMarkerDirectionsCountryCohort,by="row.names",all=TRUE)[,1]
temp3 <- apply(temp3,2,function(x)(ifelse(is.na(x),0,x)))

AllDiseaseMarkerDirectionsCountryCohort <- temp3
colnames(AllDiseaseMarkerDirectionsCountryCohort) <- c("T2D_Young","T2D_Middle","T2D_Elderly","IBD_Young","IBD_Middle","IBD_Elderly","Cirrhosis_Young","Cirrhosis_Middle","Cirrhosis_Elderly","CRC_Middle","CRC_Elderly","Adenoma_Young","Adenoma_Elderly")



