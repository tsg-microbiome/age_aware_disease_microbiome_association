library(lmtest)

features <- unique(c(CRCYoungMiddleTop85,CRCElderlyTop85))

AllDiseased <- intersect(CRCIndividuals,CRCCountryCohort)
AllControls <- intersect(SelectControls,CRCCountryCohort)
OverallAssociationCRCLME <- as.data.frame(matrix(NA,length(features),20))
rownames(OverallAssociationCRCLME) <- features
colnames(OverallAssociationCRCLME) <- c("RSquared_Fit1","PValue_Fit1","RSquared_Fit2","PValue_Fit2","RSquared_Fit3","PValue_Fit3","ModelAnovaP_Fit2_Fit1","ModelAnovaSumOfSq_Fit2_Fit1","ModelAnovaP_Fit2_Fit3","ModelAnovaSumOfSq_Fit2_Fit3","AIC_Fit1","AIC_Fit2","AIC_Fit3","BIC_Fit1","BIC_Fit2","BIC_Fit3","Lrt_P_Fit2_Fit1","Lrt_P_Fit2_Fit3","Adjust_Lrt_P_Fit2_Fit1","Adjust_Lrt_P_Fit2_Fit3")

Diseased <- intersect(CRCIndividuals,CRCCountryCohort)
Controls <- intersect(SelectControls,CRCCountryCohort)
for(i in 1:length(features))
{
	
	fit1_new <- lm(combined_species_profile_with_age_country_log1[c(Diseased,Controls),features[i]]~combined_species_profile_with_age_country_final1[c(Diseased,Controls),"country"]+as.factor(c(rep("1",length(Diseased)),rep("0",length(Controls)))))
	fit2_new <- lm(combined_species_profile_with_age_country_log1[c(Diseased,Controls),features[i]]~combined_species_profile_with_age_country_final1[c(Diseased,Controls),"country"]+as.factor(c(rep("1",length(Diseased)),rep("0",length(Controls))))*ifelse(combined_species_profile_with_age_country_final1[c(Diseased,Controls),"age_group"] %in% c("Young","Middle"),"YoungMiddle","Elderly"))
	fit3_new <- lm(combined_species_profile_with_age_country_log1[c(Diseased,Controls),features[i]]~combined_species_profile_with_age_country_final1[c(Diseased,Controls),"country"]+as.factor(c(rep("1",length(Diseased)),rep("0",length(Controls))))+ifelse(combined_species_profile_with_age_country_final1[c(Diseased,Controls),"age_group"] %in% c("Young","Middle"),"YoungMiddle","Elderly"))
	t1 <- summary(fit1_new)
	t2 <- summary(fit2_new)
	t4 <- summary(fit3_new)
	t3 <- anova(fit1_new,fit2_new)
	t5 <- anova(fit3_new,fit2_new)
	t6 <- lrtest(fit2_new,fit1_new)
	t7 <- lrtest(fit2_new,fit3_new)
	OverallAssociationCRCLME[i,1] <- t1$adj.r.squared
	OverallAssociationCRCLME[i,2] <- pf(t1$fstatistic[1],t1$fstatistic[2],t1$fstatistic[3],lower.tail=F)
	OverallAssociationCRCLME[i,3] <- t2$adj.r.squared
	OverallAssociationCRCLME[i,4] <- pf(t2$fstatistic[1],t2$fstatistic[2],t2$fstatistic[3],lower.tail=F)
	OverallAssociationCRCLME[i,5] <- t4$adj.r.squared
	OverallAssociationCRCLME[i,6] <- pf(t4$fstatistic[1],t4$fstatistic[2],t4$fstatistic[3],lower.tail=F)
	OverallAssociationCRCLME[i,7] <- as.numeric(t3$Pr[2])
	OverallAssociationCRCLME[i,8] <- as.numeric(t3$Sum[2])
	OverallAssociationCRCLME[i,9] <- as.numeric(t5$Pr[2])
	OverallAssociationCRCLME[i,10] <- as.numeric(t5$Sum[2])
	OverallAssociationCRCLME[i,11] <- AIC(fit1_new)
	OverallAssociationCRCLME[i,12] <- AIC(fit2_new)
	OverallAssociationCRCLME[i,13] <- AIC(fit3_new)
	OverallAssociationCRCLME[i,14] <- BIC(fit1_new)
	OverallAssociationCRCLME[i,15] <- BIC(fit2_new)
	OverallAssociationCRCLME[i,16] <- BIC(fit3_new)
	OverallAssociationCRCLME[i,17] <- t6$Pr[2]
	OverallAssociationCRCLME[i,18] <- t7$Pr[2]
	i <- i + 1
}

#OverallAssociationCRCLME[,2] <- p.adjust(OverallAssociationCRCLME[,2])
#OverallAssociationCRCLME[,4] <- p.adjust(OverallAssociationCRCLME[,4])
#OverallAssociationCRCLME[,6] <- p.adjust(OverallAssociationCRCLME[,6])
#OverallAssociationCRCLME[,7] <- p.adjust(OverallAssociationCRCLME[,7])
#OverallAssociationCRCLME[,9] <- p.adjust(OverallAssociationCRCLME[,9])
#OverallAssociationCRCLME[,19] <- p.adjust(OverallAssociationCRCLME[,17])
#OverallAssociationCRCLME[,20] <- p.adjust(OverallAssociationCRCLME[,18])

FilteredOverallAssociationCRCLME <- OverallAssociationCRCLME[CRC_select_species,]
FilteredOverallAssociationCRCLME[,1] <- ifelse(FilteredOverallAssociationCRCLME[,1] < 0,min((FilteredOverallAssociationCRCLME[,1])[FilteredOverallAssociationCRCLME[,1]>0]),FilteredOverallAssociationCRCLME[,1])
FilteredOverallAssociationCRCLME[,3] <- ifelse(FilteredOverallAssociationCRCLME[,3] < 0,min((FilteredOverallAssociationCRCLME[,3])[FilteredOverallAssociationCRCLME[,3]>0]),FilteredOverallAssociationCRCLME[,3])
FilteredOverallAssociationCRCLME[,5] <- ifelse(FilteredOverallAssociationCRCLME[,5] < 0,min((FilteredOverallAssociationCRCLME[,5])[FilteredOverallAssociationCRCLME[,5]>0]),FilteredOverallAssociationCRCLME[,5])
	
