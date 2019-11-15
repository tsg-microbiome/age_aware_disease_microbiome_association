WilcoxComparisonIBDElderlyTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(IBDCohort,intersect(IBDIndividuals,c(Elderly))),IBDElderlyTop85]),t(combined_species_profile_with_age_country_final[intersect(IBDCohort,intersect(SelectControls,c(Elderly))),IBDElderlyTop85]))
WilcoxComparisonIBDMiddleTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(IBDCohort,intersect(IBDIndividuals,c(Middle))),IBDMiddleTop85]),t(combined_species_profile_with_age_country_final[intersect(IBDCohort,intersect(SelectControls,c(Middle))),IBDMiddleTop85]))
WilcoxComparisonIBDYoungTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(IBDCohort,intersect(IBDIndividuals,c(Young))),IBDYoungTop85]),t(combined_species_profile_with_age_country_final[intersect(IBDCohort,intersect(SelectControls,c(Young))),IBDYoungTop85]))

WilcoxComparisonT2DElderlyTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(T2DCohort,intersect(T2DIndividuals,c(Elderly))),T2DElderlyTop85]),t(combined_species_profile_with_age_country_final[intersect(T2DCohort,intersect(SelectControls,c(Elderly))),T2DElderlyTop85]))
WilcoxComparisonT2DMiddleTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(T2DCohort,intersect(T2DIndividuals,c(Middle))),T2DMiddleTop85]),t(combined_species_profile_with_age_country_final[intersect(T2DCohort,intersect(SelectControls,c(Middle))),T2DMiddleTop85]))
WilcoxComparisonT2DYoungTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(T2DCohort,intersect(T2DIndividuals,c(Young))),T2DYoungTop85]),t(combined_species_profile_with_age_country_final[intersect(T2DCohort,intersect(SelectControls,c(Young))),T2DYoungTop85]))

WilcoxComparisonCirrhosisElderlyTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CirrhosisCohort,intersect(CirrhosisIndividuals,c(Elderly))),CirrhosisElderlyTop85]),t(combined_species_profile_with_age_country_final[intersect(CirrhosisCohort,intersect(SelectControls,c(Elderly))),CirrhosisElderlyTop85]))
WilcoxComparisonCirrhosisMiddleTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CirrhosisCohort,intersect(CirrhosisIndividuals,c(Middle))),CirrhosisMiddleTop85]),t(combined_species_profile_with_age_country_final[intersect(CirrhosisCohort,intersect(SelectControls,c(Middle))),CirrhosisMiddleTop85]))
WilcoxComparisonCirrhosisYoungTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CirrhosisCohort,intersect(CirrhosisIndividuals,c(Young))),CirrhosisYoungTop85]),t(combined_species_profile_with_age_country_final[intersect(CirrhosisCohort,intersect(SelectControls,c(Young))),CirrhosisYoungTop85]))


WilcoxComparisonCRCElderlyTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CRCCohort,intersect(CRCIndividuals,c(Elderly))),CRCElderlyTop85]),t(combined_species_profile_with_age_country_final[intersect(CRCCohort,intersect(SelectControls,c(Elderly))),CRCElderlyTop85]))
WilcoxComparisonCRCYoungMiddleTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(CRCCohort,intersect(CRCIndividuals,c(Young,Middle))),CRCYoungMiddleTop85]),t(combined_species_profile_with_age_country_final[intersect(CRCCohort,intersect(SelectControls,c(Young,Middle))),CRCYoungMiddleTop85]))

WilcoxComparisonAdenomaElderlyTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(AdenomaCohort,intersect(AdenomaIndividuals,c(Elderly))),AdenomaElderlyTop85]),t(combined_species_profile_with_age_country_final[intersect(AdenomaCohort,intersect(SelectControls,c(Elderly))),AdenomaElderlyTop85]))
WilcoxComparisonAdenomaYoungMiddleTop85 <- wilcox_batch(t(combined_species_profile_with_age_country_final[intersect(AdenomaCohort,intersect(AdenomaIndividuals,c(Young,Middle))),AdenomaYoungMiddleTop85]),t(combined_species_profile_with_age_country_final[intersect(AdenomaCohort,intersect(SelectControls,c(Young,Middle))),AdenomaYoungMiddleTop85]))


temp0 <- merge(WilcoxComparisonIBDYoungTop85[WilcoxComparisonIBDYoungTop85[,3] < 0.05,2],WilcoxComparisonIBDMiddleTop85[WilcoxComparisonIBDMiddleTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonIBDYoungTop85[WilcoxComparisonIBDYoungTop85[,3] < 0.05,2],WilcoxComparisonIBDMiddleTop85[WilcoxComparisonIBDMiddleTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

temp1 <- merge(temp0,WilcoxComparisonIBDElderlyTop85[WilcoxComparisonIBDElderlyTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp1) <- merge(temp0,WilcoxComparisonIBDElderlyTop85[WilcoxComparisonIBDElderlyTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,1]
temp1 <- apply(temp1,2,function(x)(ifelse(is.na(x),0,x)))

IBDMarkerDirections <- temp1

temp0 <- merge(WilcoxComparisonT2DYoungTop85[WilcoxComparisonT2DYoungTop85[,3] < 0.05,2],WilcoxComparisonT2DMiddleTop85[WilcoxComparisonT2DMiddleTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonT2DYoungTop85[WilcoxComparisonT2DYoungTop85[,3] < 0.05,2],WilcoxComparisonT2DMiddleTop85[WilcoxComparisonT2DMiddleTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

temp1 <- merge(temp0,WilcoxComparisonT2DElderlyTop85[WilcoxComparisonT2DElderlyTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp1) <- merge(temp0,WilcoxComparisonT2DElderlyTop85[WilcoxComparisonT2DElderlyTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,1]
temp1 <- apply(temp1,2,function(x)(ifelse(is.na(x),0,x)))

T2DMarkerDirections <- temp1

temp0 <- merge(WilcoxComparisonCirrhosisYoungTop85[WilcoxComparisonCirrhosisYoungTop85[,3] < 0.05,2],WilcoxComparisonCirrhosisMiddleTop85[WilcoxComparisonCirrhosisMiddleTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonCirrhosisYoungTop85[WilcoxComparisonCirrhosisYoungTop85[,3] < 0.05,2],WilcoxComparisonCirrhosisMiddleTop85[WilcoxComparisonCirrhosisMiddleTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

temp1 <- merge(temp0,WilcoxComparisonCirrhosisElderlyTop85[WilcoxComparisonCirrhosisElderlyTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp1) <- merge(temp0,WilcoxComparisonCirrhosisElderlyTop85[WilcoxComparisonCirrhosisElderlyTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,1]
temp1 <- apply(temp1,2,function(x)(ifelse(is.na(x),0,x)))

CirrhosisMarkerDirections <- temp1


temp0 <- merge(WilcoxComparisonCRCYoungMiddleTop85[WilcoxComparisonCRCYoungMiddleTop85[,3] < 0.05,2],WilcoxComparisonCRCElderlyTop85[WilcoxComparisonCRCElderlyTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonCRCYoungMiddleTop85[WilcoxComparisonCRCYoungMiddleTop85[,3] < 0.05,2],WilcoxComparisonCRCElderlyTop85[WilcoxComparisonCRCElderlyTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

CRCMarkerDirections <- temp0


temp0 <- merge(WilcoxComparisonAdenomaYoungMiddleTop85[WilcoxComparisonAdenomaYoungMiddleTop85[,3] < 0.05,2],WilcoxComparisonAdenomaElderlyTop85[WilcoxComparisonAdenomaElderlyTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(WilcoxComparisonAdenomaYoungMiddleTop85[WilcoxComparisonAdenomaYoungMiddleTop85[,3] < 0.05,2],WilcoxComparisonAdenomaElderlyTop85[WilcoxComparisonAdenomaElderlyTop85[,3] < 0.05,2],by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

AdenomaMarkerDirections <- temp0

colnames(IBDMarkerDirections) <- c("Young","Middle","Elderly")
colnames(T2DMarkerDirections) <- c("Young","Middle","Elderly")
colnames(CirrhosisMarkerDirections) <- c("Young","Middle","Elderly")
colnames(CRCMarkerDirections) <- c("Middle","Elderly")
colnames(AdenomaMarkerDirections) <- c("Middle","Elderly")

temp0 <- merge(T2DMarkerDirections,IBDMarkerDirections,by="row.names",all=TRUE)[,-1]
rownames(temp0) <- merge(T2DMarkerDirections,IBDMarkerDirections,by="row.names",all=TRUE)[,1]
temp0 <- apply(temp0,2,function(x)(ifelse(is.na(x),0,x)))

temp1 <- merge(temp0,CirrhosisMarkerDirections,by="row.names",all=TRUE)[,-1]
rownames(temp1) <- merge(temp0,CirrhosisMarkerDirections,by="row.names",all=TRUE)[,1]
temp1 <- apply(temp1,2,function(x)(ifelse(is.na(x),0,x)))

temp2 <- merge(temp1,CRCMarkerDirections,by="row.names",all=TRUE)[,-1]
rownames(temp2) <- merge(temp1,CRCMarkerDirections,by="row.names",all=TRUE)[,1]
temp2 <- apply(temp2,2,function(x)(ifelse(is.na(x),0,x)))

temp3 <- merge(temp2,AdenomaMarkerDirections,by="row.names",all=TRUE)[,-1]
rownames(temp3) <- merge(temp2,AdenomaMarkerDirections,by="row.names",all=TRUE)[,1]
temp3 <- apply(temp3,2,function(x)(ifelse(is.na(x),0,x)))

AllDiseaseMarkerDirections <- temp3
colnames(AllDiseaseMarkerDirections) <- c("T2D_Young","T2D_Middle","T2D_Elderly","IBD_Young","IBD_Middle","IBD_Elderly","Cirrhosis_Young","Cirrhosis_Middle","Cirrhosis_Elderly","CRC_Middle","CRC_Elderly","Adenoma_Young","Adenoma_Elderly")



