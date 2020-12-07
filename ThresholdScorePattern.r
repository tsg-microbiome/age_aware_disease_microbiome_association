T2DFeaturesPercentileDetection <- rbind(100*quantile(colSums(apply(rbind(rfT2DYoung_Elderly$featureProfile,rfT2DYoung_Middle$featureProfile,rfT2DYoung_Young$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/300,
100*quantile(colSums(apply(rbind(rfT2DMiddle_Elderly$featureProfile,rfT2DMiddle_Middle$featureProfile,rfT2DMiddle_Young$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/300,
100*quantile(colSums(apply(rbind(rfT2DElderly_Elderly$featureProfile,rfT2DElderly_Middle$featureProfile,rfT2DElderly_Young$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/300)
rownames(T2DFeaturesPercentileDetection) <- c("Young","Middle","Elderly")

IBDFeaturesPercentileDetection <- rbind(100*quantile(colSums(apply(rbind(rfIBDYoung_Elderly$featureProfile,rfIBDYoung_Middle$featureProfile,rfIBDYoung_Young$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/300,
100*quantile(colSums(apply(rbind(rfIBDMiddle_Elderly$featureProfile,rfIBDMiddle_Middle$featureProfile,rfIBDMiddle_Young$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/300,
100*quantile(colSums(apply(rbind(rfIBDElderly_Elderly$featureProfile,rfIBDElderly_Middle$featureProfile,rfIBDElderly_Young$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/300)
rownames(IBDFeaturesPercentileDetection) <- c("Young","Middle","Elderly")

CirrhosisFeaturesPercentileDetection <- rbind(100*quantile(colSums(apply(rbind(rfCirrhosisYoung_Elderly$featureProfile,rfCirrhosisYoung_Middle$featureProfile,rfCirrhosisYoung_Young$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/300,
100*quantile(colSums(apply(rbind(rfCirrhosisMiddle_Elderly$featureProfile,rfCirrhosisMiddle_Middle$featureProfile,rfCirrhosisMiddle_Young$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/300,
100*quantile(colSums(apply(rbind(rfCirrhosisElderly_Elderly$featureProfile,rfCirrhosisElderly_Middle$featureProfile,rfCirrhosisElderly_Young$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/300)
rownames(CirrhosisFeaturesPercentileDetection) <- c("Young","Middle","Elderly")

CRCFeaturesPercentileDetection <- rbind(100*quantile(colSums(apply(rbind(rfCRCYoungMiddle_Elderly$featureProfile,rfCRCYoungMiddle_YoungMiddle$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/200,
100*quantile(colSums(apply(rbind(rfCRCElderly_Elderly$featureProfile,rfCRCElderly_YoungMiddle$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/200)
rownames(CRCFeaturesPercentileDetection) <- c("YoungMiddle","Elderly")

AdenomaFeaturesPercentileDetection <- rbind(100*quantile(colSums(apply(rbind(rfAdenomaYoungMiddle_Elderly$featureProfile,rfAdenomaYoungMiddle_YoungMiddle$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/200,
100*quantile(colSums(apply(rbind(rfAdenomaElderly_Elderly$featureProfile,rfAdenomaElderly_YoungMiddle$featureProfile),2,function(x)(ifelse(x>0,1,0)))),seq(0,1,0.05))/200)
rownames(AdenomaFeaturesPercentileDetection) <- c("YoungMiddle","Elderly")

