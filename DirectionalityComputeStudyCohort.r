CRCStudyCohort <- c(VogtmannE_2016Individuals,FengQ_2015Individuals,ZellerG_2014Individuals)
AdenomaStudyCohort <- c(FengQ_2015Individuals,ZellerG_2014Individuals)
IBDStudyCohort <- c(FranzosaCA_2018Individuals,NielsenHB_2014Individuals)
CirrhosisStudyCohort <- c(QinN_2014Individuals)
T2DStudyCohort <- c(QinJ_2012Individuals,KarlssonFH_2013Individuals)

WilcoxComparisonStudyCohortIBDElderly <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(IBDStudyCohort,intersect(IBDIndividuals,c(Elderly))),core_species]),t(combined_species_profile_with_age_country_final[intersect(IBDStudyCohort,intersect(SelectControls,c(Elderly))),core_species]))
WilcoxComparisonStudyCohortIBDMiddle <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(IBDStudyCohort,intersect(IBDIndividuals,c(Middle))),core_species]),t(combined_species_profile_with_age_country_final[intersect(IBDStudyCohort,intersect(SelectControls,c(Middle))),core_species]))
WilcoxComparisonStudyCohortIBDYoung <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(IBDStudyCohort,intersect(IBDIndividuals,c(Young))),core_species]),t(combined_species_profile_with_age_country_final[intersect(IBDStudyCohort,intersect(SelectControls,c(Young))),core_species]))

WilcoxComparisonStudyCohortT2DElderly <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(T2DStudyCohort,intersect(T2DIndividuals,c(Elderly))),core_species]),t(combined_species_profile_with_age_country_final[intersect(T2DStudyCohort,intersect(SelectControls,c(Elderly))),core_species]))
WilcoxComparisonStudyCohortT2DMiddle <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(T2DStudyCohort,intersect(T2DIndividuals,c(Middle))),core_species]),t(combined_species_profile_with_age_country_final[intersect(T2DStudyCohort,intersect(SelectControls,c(Middle))),core_species]))
WilcoxComparisonStudyCohortT2DYoung <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(T2DStudyCohort,intersect(T2DIndividuals,c(Young))),core_species]),t(combined_species_profile_with_age_country_final[intersect(T2DStudyCohort,intersect(SelectControls,c(Young))),core_species]))

WilcoxComparisonStudyCohortCirrhosisElderly <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CirrhosisStudyCohort,intersect(CirrhosisIndividuals,c(Elderly))),core_species]),t(combined_species_profile_with_age_country_final[intersect(CirrhosisStudyCohort,intersect(SelectControls,c(Elderly))),core_species]))
WilcoxComparisonStudyCohortCirrhosisMiddle <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CirrhosisStudyCohort,intersect(CirrhosisIndividuals,c(Middle))),core_species]),t(combined_species_profile_with_age_country_final[intersect(CirrhosisStudyCohort,intersect(SelectControls,c(Middle))),core_species]))
WilcoxComparisonStudyCohortCirrhosisYoung <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CirrhosisStudyCohort,intersect(CirrhosisIndividuals,c(Young))),core_species]),t(combined_species_profile_with_age_country_final[intersect(CirrhosisStudyCohort,intersect(SelectControls,c(Young))),core_species]))


WilcoxComparisonStudyCohortCRCElderly <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CRCStudyCohort,intersect(CRCIndividuals,c(Elderly))),core_species]),t(combined_species_profile_with_age_country_final[intersect(CRCStudyCohort,intersect(SelectControls,c(Elderly))),core_species]))
WilcoxComparisonStudyCohortCRCYoungMiddle <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CRCStudyCohort,intersect(CRCIndividuals,c(Young,Middle))),core_species]),t(combined_species_profile_with_age_country_final[intersect(CRCStudyCohort,intersect(SelectControls,c(Young,Middle))),core_species]))

WilcoxComparisonStudyCohortAdenomaElderly <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(AdenomaStudyCohort,intersect(AdenomaIndividuals,c(Elderly))),core_species]),t(combined_species_profile_with_age_country_final[intersect(AdenomaStudyCohort,intersect(SelectControls,c(Elderly))),core_species]))
WilcoxComparisonStudyCohortAdenomaYoungMiddle <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(AdenomaStudyCohort,intersect(AdenomaIndividuals,c(Young,Middle))),core_species]),t(combined_species_profile_with_age_country_final[intersect(AdenomaStudyCohort,intersect(SelectControls,c(Young,Middle))),core_species]))


temp0 <- merge(WilcoxComparisonStudyCohortIBDYoung[WilcoxComparisonStudyCohortIBDYoung[,1] < 0.05,2],WilcoxComparisonStudyCohortIBDMiddle[WilcoxComparisonStudyCohortIBDMiddle[,1] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonStudyCohortIBDYoung[WilcoxComparisonStudyCohortIBDYoung[,1] < 0.05,2],WilcoxComparisonStudyCohortIBDMiddle[WilcoxComparisonStudyCohortIBDMiddle[,1] < 0.05,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

temp1 <- merge(temp0,WilcoxComparisonStudyCohortIBDElderly[WilcoxComparisonStudyCohortIBDElderly[,1] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp1) <- merge(temp0,WilcoxComparisonStudyCohortIBDElderly[WilcoxComparisonStudyCohortIBDElderly[,1] < 0.05,2],by="row.names",all=TRUE)[,1]
temp1 <- apply(temp1,2,function(x)(ifelse(is.na(x),0,x)))

IBDMarkerDirectionsStudyCohort <- temp1

temp0 <- merge(WilcoxComparisonStudyCohortT2DYoung[WilcoxComparisonStudyCohortT2DYoung[,1] < 0.05,2],WilcoxComparisonStudyCohortT2DMiddle[WilcoxComparisonStudyCohortT2DMiddle[,1] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonStudyCohortT2DYoung[WilcoxComparisonStudyCohortT2DYoung[,1] < 0.05,2],WilcoxComparisonStudyCohortT2DMiddle[WilcoxComparisonStudyCohortT2DMiddle[,1] < 0.05,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

temp1 <- merge(temp0,WilcoxComparisonStudyCohortT2DElderly[WilcoxComparisonStudyCohortT2DElderly[,1] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp1) <- merge(temp0,WilcoxComparisonStudyCohortT2DElderly[WilcoxComparisonStudyCohortT2DElderly[,1] < 0.05,2],by="row.names",all=TRUE)[,1]
temp1 <- apply(temp1,2,function(x)(ifelse(is.na(x),0,x)))

T2DMarkerDirectionsStudyCohort <- temp1

temp0 <- merge(WilcoxComparisonStudyCohortCirrhosisYoung[WilcoxComparisonStudyCohortCirrhosisYoung[,1] < 0.05,2],WilcoxComparisonStudyCohortCirrhosisMiddle[WilcoxComparisonStudyCohortCirrhosisMiddle[,1] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonStudyCohortCirrhosisYoung[WilcoxComparisonStudyCohortCirrhosisYoung[,1] < 0.05,2],WilcoxComparisonStudyCohortCirrhosisMiddle[WilcoxComparisonStudyCohortCirrhosisMiddle[,1] < 0.05,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

temp1 <- merge(temp0,WilcoxComparisonStudyCohortCirrhosisElderly[WilcoxComparisonStudyCohortCirrhosisElderly[,1] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp1) <- merge(temp0,WilcoxComparisonStudyCohortCirrhosisElderly[WilcoxComparisonStudyCohortCirrhosisElderly[,1] < 0.05,2],by="row.names",all=TRUE)[,1]
temp1 <- apply(temp1,2,function(x)(ifelse(is.na(x),0,x)))

CirrhosisMarkerDirectionsStudyCohort <- temp1


temp0 <- merge(WilcoxComparisonStudyCohortCRCYoungMiddle[WilcoxComparisonStudyCohortCRCYoungMiddle[,1] < 0.05,2],WilcoxComparisonStudyCohortCRCElderly[WilcoxComparisonStudyCohortCRCElderly[,1] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonStudyCohortCRCYoungMiddle[WilcoxComparisonStudyCohortCRCYoungMiddle[,1] < 0.05,2],WilcoxComparisonStudyCohortCRCElderly[WilcoxComparisonStudyCohortCRCElderly[,1] < 0.05,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

CRCMarkerDirectionsStudyCohort <- temp0


temp0 <- merge(WilcoxComparisonStudyCohortAdenomaYoungMiddle[WilcoxComparisonStudyCohortAdenomaYoungMiddle[,1] < 0.05,2],WilcoxComparisonStudyCohortAdenomaElderly[WilcoxComparisonStudyCohortAdenomaElderly[,1] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonStudyCohortAdenomaYoungMiddle[WilcoxComparisonStudyCohortAdenomaYoungMiddle[,1] < 0.05,2],WilcoxComparisonStudyCohortAdenomaElderly[WilcoxComparisonStudyCohortAdenomaElderly[,1] < 0.05,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

AdenomaMarkerDirectionsStudyCohort <- temp0

colnames(IBDMarkerDirectionsStudyCohort) <- c("Young","Middle","Elderly")
colnames(T2DMarkerDirectionsStudyCohort) <- c("Young","Middle","Elderly")
colnames(CirrhosisMarkerDirectionsStudyCohort) <- c("Young","Middle","Elderly")
colnames(CRCMarkerDirectionsStudyCohort) <- c("Middle","Elderly")
colnames(AdenomaMarkerDirectionsStudyCohort) <- c("Middle","Elderly")

temp0 <- merge(T2DMarkerDirectionsStudyCohort,IBDMarkerDirectionsStudyCohort,by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(T2DMarkerDirectionsStudyCohort,IBDMarkerDirectionsStudyCohort,by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

temp1 <- merge(temp0,CirrhosisMarkerDirectionsStudyCohort,by="row.names",all=TRUE)[,-1]
rownames(temp1) <- merge(temp0,CirrhosisMarkerDirectionsStudyCohort,by="row.names",all=TRUE)[,1]
temp1 <- apply(temp1,2,function(x)(ifelse(is.na(x),0,x)))

temp2 <- merge(temp1,CRCMarkerDirectionsStudyCohort,by="row.names",all=TRUE)[,-1]
rownames(temp2) <- merge(temp1,CRCMarkerDirectionsStudyCohort,by="row.names",all=TRUE)[,1]
temp2 <- apply(temp2,2,function(x)(ifelse(is.na(x),0,x)))

temp3 <- merge(temp2,AdenomaMarkerDirectionsStudyCohort,by="row.names",all=TRUE)[,-1]
rownames(temp3) <- merge(temp2,AdenomaMarkerDirectionsStudyCohort,by="row.names",all=TRUE)[,1]
temp3 <- apply(temp3,2,function(x)(ifelse(is.na(x),0,x)))

AllDiseaseMarkerDirectionsStudyCohort <- temp3
colnames(AllDiseaseMarkerDirectionsStudyCohort) <- c("T2D_Young","T2D_Middle","T2D_Elderly","IBD_Young","IBD_Middle","IBD_Elderly","Cirrhosis_Young","Cirrhosis_Middle","Cirrhosis_Elderly","CRC_Middle","CRC_Elderly","Adenoma_Young","Adenoma_Elderly")



