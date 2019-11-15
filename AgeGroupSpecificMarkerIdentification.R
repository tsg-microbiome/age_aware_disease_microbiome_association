print("Select Markers")
CirrhosisSelectMarkers <- unique(c(CirrhosisYoungTop85,CirrhosisMiddleTop85,CirrhosisElderlyTop85))
IBDSelectMarkers <- unique(c(IBDYoungTop85,IBDMiddleTop85,IBDElderlyTop85))
T2DSelectMarkers <- unique(c(T2DYoungTop85,T2DMiddleTop85,T2DElderlyTop85))
CRCSelectMarkers <- unique(c(CRCYoungMiddleTop85,CRCElderlyTop85))
AdenomaSelectMarkers <- unique(c(AdenomaYoungMiddleTop85,AdenomaElderlyTop85))

print("Differential Markers for Cirrhosis")
CirrhosisAllAgeGroupsFeatureProfile <- rbind(rfCirrhosisYoung_Young$featureProfile,rfCirrhosisYoung_Middle$featureProfile,rfCirrhosisYoung_Elderly$featureProfile,rfCirrhosisMiddle_Young$featureProfile,rfCirrhosisMiddle_Middle$featureProfile,rfCirrhosisMiddle_Elderly$featureProfile,rfCirrhosisElderly_Young$featureProfile,rfCirrhosisElderly_Middle$featureProfile,rfCirrhosisElderly_Elderly$featureProfile)
CirrhosisAllAgeGroupsRawFeatureProfile <- CirrhosisAllAgeGroupsFeatureProfile
CirrhosisAllAgeGroupsFeatureProfile <- t(apply(CirrhosisAllAgeGroupsFeatureProfile,1,rank_scale))
CirrhosisAllAgeGroupsFeatureProfile1 <- t(apply(CirrhosisAllAgeGroupsFeatureProfile,1,range_scale))
CirrhosisMeanRawMarkerScores <- cbind(colMeans(CirrhosisAllAgeGroupsRawFeatureProfile[1:300,]),colMeans(CirrhosisAllAgeGroupsRawFeatureProfile[301:600,]),colMeans(CirrhosisAllAgeGroupsRawFeatureProfile[601:900,]))
CirrhosisMeanRankedMarkerScores <- cbind(colMeans(CirrhosisAllAgeGroupsFeatureProfile[1:300,]),colMeans(CirrhosisAllAgeGroupsFeatureProfile[301:600,]),colMeans(CirrhosisAllAgeGroupsFeatureProfile[601:900,]))
CirrhosisMeanRankedMarkerScores1 <- cbind(colMeans(CirrhosisAllAgeGroupsFeatureProfile1[1:300,]),colMeans(CirrhosisAllAgeGroupsFeatureProfile1[301:600,]),colMeans(CirrhosisAllAgeGroupsFeatureProfile1[601:900,]))
CirrhosisAllAgeGroupsDunns <- batch_dunns(CirrhosisAllAgeGroupsRawFeatureProfile[,CirrhosisSelectMarkers],as.factor(c(rep("Young",300),rep("Middle",300),rep("Elderly",300))))
CirrhosisAllAgeGroupsFeatureComp <- cbind(CirrhosisMeanRawMarkerScores[CirrhosisSelectMarkers,],CirrhosisAllAgeGroupsDunns$kruskal_p,CirrhosisAllAgeGroupsDunns$CorrectedP)
CirrhosisAllAgeGroupsFeatureComp <- as.data.frame(CirrhosisAllAgeGroupsFeatureComp)
CirrhosisAllAgeGroupsFeatureComp$Young_Feature <- ifelse(rownames(CirrhosisAllAgeGroupsFeatureComp) %in% CirrhosisYoungTop85,1,0)
CirrhosisAllAgeGroupsFeatureComp$Middle_Feature <- ifelse(rownames(CirrhosisAllAgeGroupsFeatureComp) %in% CirrhosisMiddleTop85,1,0)
CirrhosisAllAgeGroupsFeatureComp$Elderly_Feature <- ifelse(rownames(CirrhosisAllAgeGroupsFeatureComp) %in% CirrhosisElderlyTop85,1,0)

print("Differential Markers for T2D")
T2DAllAgeGroupsFeatureProfile <- rbind(rfT2DYoung_Young$featureProfile,rfT2DYoung_Middle$featureProfile,rfT2DYoung_Elderly$featureProfile,rfT2DMiddle_Young$featureProfile,rfT2DMiddle_Middle$featureProfile,rfT2DMiddle_Elderly$featureProfile,rfT2DElderly_Young$featureProfile,rfT2DElderly_Middle$featureProfile,rfT2DElderly_Elderly$featureProfile)
T2DAllAgeGroupsRawFeatureProfile <- T2DAllAgeGroupsFeatureProfile
T2DAllAgeGroupsFeatureProfile <- t(apply(T2DAllAgeGroupsFeatureProfile,1,rank_scale))
T2DAllAgeGroupsFeatureProfile1 <- t(apply(T2DAllAgeGroupsFeatureProfile,1,range_scale))
T2DMeanRawMarkerScores <- cbind(colMeans(T2DAllAgeGroupsRawFeatureProfile[1:300,]),colMeans(T2DAllAgeGroupsRawFeatureProfile[301:600,]),colMeans(T2DAllAgeGroupsRawFeatureProfile[601:900,]))
T2DMeanRankedMarkerScores <- cbind(colMeans(T2DAllAgeGroupsFeatureProfile[1:300,]),colMeans(T2DAllAgeGroupsFeatureProfile[301:600,]),colMeans(T2DAllAgeGroupsFeatureProfile[601:900,]))
T2DMeanRankedMarkerScores1 <- cbind(colMeans(T2DAllAgeGroupsFeatureProfile1[1:300,]),colMeans(T2DAllAgeGroupsFeatureProfile1[301:600,]),colMeans(T2DAllAgeGroupsFeatureProfile1[601:900,]))
T2DAllAgeGroupsDunns <- batch_dunns(T2DAllAgeGroupsRawFeatureProfile[,T2DSelectMarkers],as.factor(c(rep("Young",300),rep("Middle",300),rep("Elderly",300))))
T2DAllAgeGroupsFeatureComp <- cbind(T2DMeanRawMarkerScores[T2DSelectMarkers,],T2DAllAgeGroupsDunns$kruskal_p,T2DAllAgeGroupsDunns$CorrectedP)
T2DAllAgeGroupsFeatureComp <- as.data.frame(T2DAllAgeGroupsFeatureComp)
T2DAllAgeGroupsFeatureComp$Young_Feature <- ifelse(rownames(T2DAllAgeGroupsFeatureComp) %in% T2DYoungTop85,1,0)
T2DAllAgeGroupsFeatureComp$Middle_Feature <- ifelse(rownames(T2DAllAgeGroupsFeatureComp) %in% T2DMiddleTop85,1,0)
T2DAllAgeGroupsFeatureComp$Elderly_Feature <- ifelse(rownames(T2DAllAgeGroupsFeatureComp) %in% T2DElderlyTop85,1,0)

print("Differential Markers for IBD")
IBDAllAgeGroupsFeatureProfile <- rbind(rfIBDYoung_Young$featureProfile,rfIBDYoung_Middle$featureProfile,rfIBDYoung_Elderly$featureProfile,rfIBDMiddle_Young$featureProfile,rfIBDMiddle_Middle$featureProfile,rfIBDMiddle_Elderly$featureProfile,rfIBDElderly_Young$featureProfile,rfIBDElderly_Middle$featureProfile,rfIBDElderly_Elderly$featureProfile)
IBDAllAgeGroupsRawFeatureProfile <- IBDAllAgeGroupsFeatureProfile
IBDAllAgeGroupsFeatureProfile <- t(apply(IBDAllAgeGroupsFeatureProfile,1,rank_scale))
IBDAllAgeGroupsFeatureProfile1 <- t(apply(IBDAllAgeGroupsFeatureProfile,1,range_scale))
IBDMeanRawMarkerScores <- cbind(colMeans(IBDAllAgeGroupsRawFeatureProfile[1:300,]),colMeans(IBDAllAgeGroupsRawFeatureProfile[301:600,]),colMeans(IBDAllAgeGroupsRawFeatureProfile[601:900,]))
IBDMeanRankedMarkerScores <- cbind(colMeans(IBDAllAgeGroupsFeatureProfile[1:300,]),colMeans(IBDAllAgeGroupsFeatureProfile[301:600,]),colMeans(IBDAllAgeGroupsFeatureProfile[601:900,]))
IBDMeanRankedMarkerScores1 <- cbind(colMeans(IBDAllAgeGroupsFeatureProfile1[1:300,]),colMeans(IBDAllAgeGroupsFeatureProfile1[301:600,]),colMeans(IBDAllAgeGroupsFeatureProfile1[601:900,]))
IBDAllAgeGroupsDunns <- batch_dunns(IBDAllAgeGroupsRawFeatureProfile[,IBDSelectMarkers],as.factor(c(rep("Young",300),rep("Middle",300),rep("Elderly",300))))
IBDAllAgeGroupsFeatureComp <- cbind(IBDMeanRawMarkerScores[IBDSelectMarkers,],IBDAllAgeGroupsDunns$kruskal_p,IBDAllAgeGroupsDunns$CorrectedP)
IBDAllAgeGroupsFeatureComp <- as.data.frame(IBDAllAgeGroupsFeatureComp)
IBDAllAgeGroupsFeatureComp$Young_Feature <- ifelse(rownames(IBDAllAgeGroupsFeatureComp) %in% IBDYoungTop85,1,0)
IBDAllAgeGroupsFeatureComp$Middle_Feature <- ifelse(rownames(IBDAllAgeGroupsFeatureComp) %in% IBDMiddleTop85,1,0)
IBDAllAgeGroupsFeatureComp$Elderly_Feature <- ifelse(rownames(IBDAllAgeGroupsFeatureComp) %in% IBDElderlyTop85,1,0)


print("Differential Markers for CRC")
CRCAllAgeGroupsFeatureProfile <- rbind(rfCRCYoungMiddle_YoungMiddle$featureProfile,rfCRCYoungMiddle_Elderly$featureProfile,rfCRCElderly_YoungMiddle$featureProfile,rfCRCElderly_Elderly$featureProfile)
CRCAllAgeGroupsRawFeatureProfile <- CRCAllAgeGroupsFeatureProfile
CRCAllAgeGroupsFeatureProfile <- t(apply(CRCAllAgeGroupsFeatureProfile,1,rank_scale))
CRCAllAgeGroupsFeatureProfile1 <- t(apply(CRCAllAgeGroupsFeatureProfile,1,range_scale))
CRCMeanRawMarkerScores <- cbind(colMeans(CRCAllAgeGroupsRawFeatureProfile[1:200,]),colMeans(CRCAllAgeGroupsRawFeatureProfile[201:400,]))
CRCMeanRankedMarkerScores <- cbind(colMeans(CRCAllAgeGroupsFeatureProfile[1:200,]),colMeans(CRCAllAgeGroupsFeatureProfile[201:400,]))
CRCMeanRankedMarkerScores1 <- cbind(colMeans(CRCAllAgeGroupsFeatureProfile1[1:200,]),colMeans(CRCAllAgeGroupsFeatureProfile1[201:400,]))
CRCAllAgeGroupsWilcox <- wilcox_batch(t(CRCAllAgeGroupsRawFeatureProfile[1:200,CRCSelectMarkers]),t(CRCAllAgeGroupsRawFeatureProfile[201:400,CRCSelectMarkers]))
CRCAllAgeGroupsFeatureComp <- cbind(CRCMeanRawMarkerScores[intersect(rownames(CRCMeanRawMarkerScores),rownames(CRCAllAgeGroupsWilcox)),],CRCAllAgeGroupsWilcox[intersect(rownames(CRCMeanRawMarkerScores),rownames(CRCAllAgeGroupsWilcox)),3])
CRCAllAgeGroupsFeatureComp <- as.data.frame(CRCAllAgeGroupsFeatureComp)
CRCAllAgeGroupsFeatureComp$YoungMiddle_Feature <- ifelse(rownames(CRCAllAgeGroupsFeatureComp) %in% CRCYoungMiddleTop85,1,0)
CRCAllAgeGroupsFeatureComp$Elderly_Feature <- ifelse(rownames(CRCAllAgeGroupsFeatureComp) %in% CRCElderlyTop85,1,0)



print("Differential Markers for Polyps")
AdenomaAllAgeGroupsFeatureProfile <- rbind(rfAdenomaYoungMiddle_YoungMiddle$featureProfile,rfAdenomaYoungMiddle_Elderly$featureProfile,rfAdenomaElderly_YoungMiddle$featureProfile,rfAdenomaElderly_Elderly$featureProfile)
AdenomaAllAgeGroupsRawFeatureProfile <- AdenomaAllAgeGroupsFeatureProfile
AdenomaAllAgeGroupsFeatureProfile <- t(apply(AdenomaAllAgeGroupsFeatureProfile,1,rank_scale))
AdenomaAllAgeGroupsFeatureProfile1 <- t(apply(AdenomaAllAgeGroupsFeatureProfile,1,range_scale))
AdenomaMeanRawMarkerScores <- cbind(colMeans(AdenomaAllAgeGroupsRawFeatureProfile[1:200,]),colMeans(AdenomaAllAgeGroupsRawFeatureProfile[201:400,]))
AdenomaMeanRankedMarkerScores <- cbind(colMeans(AdenomaAllAgeGroupsFeatureProfile[1:200,]),colMeans(AdenomaAllAgeGroupsFeatureProfile[201:400,]))
AdenomaMeanRankedMarkerScores1 <- cbind(colMeans(AdenomaAllAgeGroupsFeatureProfile1[1:200,]),colMeans(AdenomaAllAgeGroupsFeatureProfile1[201:400,]))
AdenomaAllAgeGroupsWilcox <- wilcox_batch(t(AdenomaAllAgeGroupsRawFeatureProfile[1:200,AdenomaSelectMarkers]),t(AdenomaAllAgeGroupsRawFeatureProfile[201:400,AdenomaSelectMarkers]))
AdenomaAllAgeGroupsFeatureComp <- cbind(AdenomaMeanRawMarkerScores[intersect(rownames(AdenomaMeanRawMarkerScores),rownames(AdenomaAllAgeGroupsWilcox)),],AdenomaAllAgeGroupsWilcox[intersect(rownames(AdenomaMeanRawMarkerScores),rownames(AdenomaAllAgeGroupsWilcox)),3])
AdenomaAllAgeGroupsFeatureComp <- as.data.frame(AdenomaAllAgeGroupsFeatureComp)
AdenomaAllAgeGroupsFeatureComp$YoungMiddle_Feature <- ifelse(rownames(AdenomaAllAgeGroupsFeatureComp) %in% AdenomaYoungMiddleTop85,1,0)
AdenomaAllAgeGroupsFeatureComp$Elderly_Feature <- ifelse(rownames(AdenomaAllAgeGroupsFeatureComp) %in% AdenomaElderlyTop85,1,0)

print("Age Trends")
WilcoxBatchControls_Across_Young_Elderly <- wilcox_batch(t(AllRegionSpeciesProfileRangeScaled[intersect(SelectControls2,c(Young)),core_species]),t(AllRegionSpeciesProfileRangeScaled[intersect(SelectControls2,c(Elderly)),core_species]))
Controls_Across_Young_Elderly_Direction <- ifelse(WilcoxBatchControls_Across_Young_Elderly[,3]>0.1,0,sign(WilcoxBatchControls_Across_Young_Elderly[,2])*(-log(WilcoxBatchControls_Across_Young_Elderly[,3],10)))

WilcoxBatchControls_Across_Middle_Elderly <- wilcox_batch(t(AllRegionSpeciesProfileRangeScaled[intersect(SelectControls2,c(Middle)),core_species]),t(AllRegionSpeciesProfileRangeScaled[intersect(SelectControls2,c(Elderly)),core_species]))
Controls_Across_Middle_Elderly_Direction <- ifelse(WilcoxBatchControls_Across_Middle_Elderly[,3]>0.1,0,sign(WilcoxBatchControls_Across_Middle_Elderly[,2])*(-log(WilcoxBatchControls_Across_Middle_Elderly[,3],10)))

WilcoxBatchControls_Across_Young_Middle <- wilcox_batch(t(AllRegionSpeciesProfileRangeScaled[intersect(SelectControls2,c(Young)),core_species]),t(AllRegionSpeciesProfileRangeScaled[intersect(SelectControls2,c(Middle)),core_species]))
Controls_Across_Young_Middle_Direction <- ifelse(WilcoxBatchControls_Across_Young_Middle[,3]>0.1,0,sign(WilcoxBatchControls_Across_Young_Middle[,2])*(-log(WilcoxBatchControls_Across_Young_Middle[,3],10)))

temp0 <- merge(as.data.frame(WilcoxBatchControls_Across_Young_Elderly[,4]),as.data.frame(WilcoxBatchControls_Across_Middle_Elderly[,4]),by="row.names",header=TRUE)[,-1]
rownames(temp0) <- merge(as.data.frame(WilcoxBatchControls_Across_Young_Elderly[,4]),as.data.frame(WilcoxBatchControls_Across_Middle_Elderly[,4]),by="row.names",header=TRUE)[,1]

temp1 <- merge(temp0,as.data.frame(WilcoxBatchControls_Across_Young_Elderly[,5]),by="row.names",header=TRUE)[,-1]
rownames(temp1) <- merge(temp0,as.data.frame(WilcoxBatchControls_Across_Young_Elderly[,5]),by="row.names",header=TRUE)[,1]

MedianAbundancesAcrossAgeGroups <- temp1
colnames(MedianAbundancesAcrossAgeGroups) <- c("Young","Middle","Elderly")

ComparativePValuesAcrossAgeGroups <- cbind(Controls_Across_Young_Middle_Direction[rownames(MedianAbundancesAcrossAgeGroups)],Controls_Across_Middle_Elderly_Direction[rownames(MedianAbundancesAcrossAgeGroups)],Controls_Across_Young_Elderly_Direction[rownames(MedianAbundancesAcrossAgeGroups)])

AgeAssociations <- cbind(MedianAbundancesAcrossAgeGroups,ComparativePValuesAcrossAgeGroups)
AgeAssociations <- cbind(AgeAssociations,tag_age_classification(AgeAssociations))
colnames(AgeAssociations)[4:7] <- c("Young_Middle","Middle_Elderly","Young_Elderly","AgeTag")












