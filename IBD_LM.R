library(lmtest)

features <- unique(c(IBDYoungTop85,IBDMiddleTop85,IBDElderlyTop85))

AllDiseased <- intersect(IBDIndividuals,IBDCountryCohort)
AllControls <- intersect(SelectControls,IBDCountryCohort)
OverallAssociationIBDLME <- as.data.frame(matrix(NA,length(features),20))
rownames(OverallAssociationIBDLME) <- features
colnames(OverallAssociationIBDLME) <- c("RSquared_Fit1","PValue_Fit1","RSquared_Fit2","PValue_Fit2","RSquared_Fit3","PValue_Fit3","ModelAnovaP_Fit2_Fit1","ModelAnovaSumOfSq_Fit2_Fit1","ModelAnovaP_Fit2_Fit3","ModelAnovaSumOfSq_Fit2_Fit3","AIC_Fit1","AIC_Fit2","AIC_Fit3","BIC_Fit1","BIC_Fit2","BIC_Fit3","Lrt_P_Fit2_Fit1","Lrt_P_Fit2_Fit3","Adjust_Lrt_P_Fit2_Fit1","Adjust_Lrt_P_Fit2_Fit3")

Diseased <- intersect(IBDIndividuals,IBDCountryCohort)
Controls <- intersect(SelectControls,IBDCountryCohort)
for(i in 1:length(features))
{
	fit3 <- lm(combined_species_profile_with_age_country_log1[c(Diseased,Controls),features[i]]~combined_species_profile_with_age_country_final1[c(Diseased,Controls),"country"])
	fit1_new <- lm(combined_species_profile_with_age_country_log1[c(Diseased,Controls),features[i]]~combined_species_profile_with_age_country_final1[c(Diseased,Controls),"country"]+as.factor(c(rep("1",length(Diseased)),rep("0",length(Controls)))))
	fit2_new <- lm(combined_species_profile_with_age_country_log1[c(Diseased,Controls),features[i]]~combined_species_profile_with_age_country_final1[c(Diseased,Controls),"country"]+as.factor(c(rep("1",length(Diseased)),rep("0",length(Controls))))*combined_species_profile_with_age_country_final1[c(Diseased,Controls),"age_group"])
	fit3_new <- lm(combined_species_profile_with_age_country_log1[c(Diseased,Controls),features[i]]~combined_species_profile_with_age_country_final1[c(Diseased,Controls),"country"]+as.factor(c(rep("1",length(Diseased)),rep("0",length(Controls))))+combined_species_profile_with_age_country_final1[c(Diseased,Controls),"age_group"])
	t1 <- summary(fit1_new)
	t2 <- summary(fit2_new)
	t4 <- summary(fit3_new)
	t3 <- anova(fit1_new,fit2_new)
	t5 <- anova(fit3_new,fit2_new)
	t6 <- lrtest(fit2_new,fit1_new)
	t7 <- lrtest(fit2_new,fit3_new)
	OverallAssociationIBDLME[i,1] <- t1$adj.r.squared
	OverallAssociationIBDLME[i,2] <- pf(t1$fstatistic[1],t1$fstatistic[2],t1$fstatistic[3],lower.tail=F)
	OverallAssociationIBDLME[i,3] <- t2$adj.r.squared
	OverallAssociationIBDLME[i,4] <- pf(t2$fstatistic[1],t2$fstatistic[2],t2$fstatistic[3],lower.tail=F)
	OverallAssociationIBDLME[i,5] <- t4$adj.r.squared
	OverallAssociationIBDLME[i,6] <- pf(t4$fstatistic[1],t4$fstatistic[2],t4$fstatistic[3],lower.tail=F)
	OverallAssociationIBDLME[i,7] <- as.numeric(t3$Pr[2])
	OverallAssociationIBDLME[i,8] <- as.numeric(t3$Sum[2])
	OverallAssociationIBDLME[i,9] <- as.numeric(t5$Pr[2])
	OverallAssociationIBDLME[i,10] <- as.numeric(t5$Sum[2])
	OverallAssociationIBDLME[i,11] <- AIC(fit1_new)
	OverallAssociationIBDLME[i,12] <- AIC(fit2_new)
	OverallAssociationIBDLME[i,13] <- AIC(fit3_new)
	OverallAssociationIBDLME[i,14] <- BIC(fit1_new)
	OverallAssociationIBDLME[i,15] <- BIC(fit2_new)
	OverallAssociationIBDLME[i,16] <- BIC(fit3_new)
	OverallAssociationIBDLME[i,17] <- t6$Pr[2]
	OverallAssociationIBDLME[i,18] <- t7$Pr[2]
	i <- i + 1
}

#OverallAssociationIBDLME[,2] <- p.adjust(OverallAssociationIBDLME[,2])
#OverallAssociationIBDLME[,4] <- p.adjust(OverallAssociationIBDLME[,4])
#OverallAssociationIBDLME[,6] <- p.adjust(OverallAssociationIBDLME[,6])
#OverallAssociationIBDLME[,7] <- p.adjust(OverallAssociationIBDLME[,7])
#OverallAssociationIBDLME[,9] <- p.adjust(OverallAssociationIBDLME[,9])
#OverallAssociationIBDLME[,19] <- p.adjust(OverallAssociationIBDLME[,17])
#OverallAssociationIBDLME[,20] <- p.adjust(OverallAssociationIBDLME[,18])

FilteredOverallAssociationIBDLME <- OverallAssociationIBDLME[IBD_select_species,]
FilteredOverallAssociationIBDLME[,1] <- ifelse(FilteredOverallAssociationIBDLME[,1] < 0,min((FilteredOverallAssociationIBDLME[,1])[FilteredOverallAssociationIBDLME[,1]>0]),FilteredOverallAssociationIBDLME[,1])
FilteredOverallAssociationIBDLME[,3] <- ifelse(FilteredOverallAssociationIBDLME[,3] < 0,min((FilteredOverallAssociationIBDLME[,3])[FilteredOverallAssociationIBDLME[,3]>0]),FilteredOverallAssociationIBDLME[,3])
FilteredOverallAssociationIBDLME[,5] <- ifelse(FilteredOverallAssociationIBDLME[,5] < 0,min((FilteredOverallAssociationIBDLME[,5])[FilteredOverallAssociationIBDLME[,5]>0]),FilteredOverallAssociationIBDLME[,5])
	
