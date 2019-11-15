CirrhosisYoungTop85 <- names(which(rank_scale(colMeans(rbind(rfCirrhosisYoung_Young$featureProfile,rfCirrhosisYoung_Middle$featureProfile,rfCirrhosisYoung_Elderly$featureProfile))) > 0.85))
CirrhosisMiddleTop85 <- names(which(rank_scale(colMeans(rbind(rfCirrhosisMiddle_Young$featureProfile,rfCirrhosisMiddle_Middle$featureProfile,rfCirrhosisMiddle_Elderly$featureProfile))) > 0.85))
CirrhosisElderlyTop85 <- names(which(rank_scale(colMeans(rbind(rfCirrhosisElderly_Young$featureProfile,rfCirrhosisElderly_Middle$featureProfile,rfCirrhosisElderly_Elderly$featureProfile))) > 0.85))
CirrhosisYoungMiddleTop85 <- union(CirrhosisYoungTop85,CirrhosisMiddleTop85)

T2DYoungTop85 <- names(which(rank_scale(colMeans(rbind(rfT2DYoung_Young$featureProfile,rfT2DYoung_Middle$featureProfile,rfT2DYoung_Elderly$featureProfile))) > 0.85))
T2DMiddleTop85 <- names(which(rank_scale(colMeans(rbind(rfT2DMiddle_Young$featureProfile,rfT2DMiddle_Middle$featureProfile,rfT2DMiddle_Elderly$featureProfile))) > 0.85))
T2DElderlyTop85 <- names(which(rank_scale(colMeans(rbind(rfT2DElderly_Young$featureProfile,rfT2DElderly_Middle$featureProfile,rfT2DElderly_Elderly$featureProfile))) > 0.85))
T2DYoungMiddleTop85 <- union(T2DYoungTop85,T2DMiddleTop85)

IBDYoungTop85 <- names(which(rank_scale(colMeans(rbind(rfIBDYoung_Young$featureProfile,rfIBDYoung_Middle$featureProfile,rfIBDYoung_Elderly$featureProfile))) > 0.85))
IBDMiddleTop85 <- names(which(rank_scale(colMeans(rbind(rfIBDMiddle_Young$featureProfile,rfIBDMiddle_Middle$featureProfile,rfIBDMiddle_Elderly$featureProfile))) > 0.85))
IBDElderlyTop85 <- names(which(rank_scale(colMeans(rbind(rfIBDElderly_Young$featureProfile,rfIBDElderly_Middle$featureProfile,rfIBDElderly_Elderly$featureProfile))) > 0.85))
IBDYoungMiddleTop85 <- union(IBDYoungTop85,IBDMiddleTop85)

CRCYoungMiddleTop85 <- names(which(rank_scale(colMeans(rbind(rfCRCYoungMiddle_YoungMiddle$featureProfile,rfCRCYoungMiddle_Elderly$featureProfile))) > 0.85))
CRCElderlyTop85 <- names(which(rank_scale(colMeans(rbind(rfCRCElderly_YoungMiddle$featureProfile,rfCRCElderly_Elderly$featureProfile))) > 0.85))


AdenomaYoungMiddleTop85 <- names(which(rank_scale(colMeans(rbind(rfAdenomaYoungMiddle_YoungMiddle$featureProfile,rfAdenomaYoungMiddle_Elderly$featureProfile))) > 0.85))
AdenomaElderlyTop85 <- names(which(rank_scale(colMeans(rbind(rfAdenomaElderly_YoungMiddle$featureProfile,rfAdenomaElderly_Elderly$featureProfile))) > 0.85))








