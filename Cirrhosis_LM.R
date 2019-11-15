library(lmtest)

features <- unique(c(CirrhosisYoungTop85,CirrhosisMiddleTop85,CirrhosisElderlyTop85))

AllDiseased <- intersect(CirrhosisIndividuals,CirrhosisCountryCohort)
AllControls <- intersect(SelectControls,CirrhosisCountryCohort)
OverallAssociationCirrhosisLME <- as.data.frame(matrix(NA,length(features),20))
rownames(OverallAssociationCirrhosisLME) <- features
colnames(OverallAssociationCirrhosisLME) <- c("RSquared_Fit1","PValue_Fit1","RSquared_Fit2","PValue_Fit2","RSquared_Fit3","PValue_Fit3","ModelAnovaP_Fit2_Fit1","ModelAnovaSumOfSq_Fit2_Fit1","ModelAnovaP_Fit2_Fit3","ModelAnovaSumOfSq_Fit2_Fit3","AIC_Fit1","AIC_Fit2","AIC_Fit3","BIC_Fit1","BIC_Fit2","BIC_Fit3","Lrt_P_Fit2_Fit1","Lrt_P_Fit2_Fit3","Adjust_Lrt_P_Fit2_Fit1","Adjust_Lrt_P_Fit2_Fit3")

Diseased <- intersect(CirrhosisIndividuals,CirrhosisCountryCohort)
Controls <- intersect(SelectControls,CirrhosisCountryCohort)
for(i in 1:length(features))
{
	
	fit1_new <- lm(combined_species_profile_with_age_country_log1[c(Diseased,Controls),features[i]]~as.factor(c(rep("1",length(Diseased)),rep("0",length(Controls)))))
	fit2_new <- lm(combined_species_profile_with_age_country_log1[c(Diseased,Controls),features[i]]~as.factor(c(rep("1",length(Diseased)),rep("0",length(Controls))))*combined_species_profile_with_age_country_final1[c(Diseased,Controls),"age_group"])
	fit3_new <- lm(combined_species_profile_with_age_country_log1[c(Diseased,Controls),features[i]]~as.factor(c(rep("1",length(Diseased)),rep("0",length(Controls))))+combined_species_profile_with_age_country_final1[c(Diseased,Controls),"age_group"])
	t1 <- summary(fit1_new)
	t2 <- summary(fit2_new)
	t4 <- summary(fit3_new)
	t3 <- anova(fit1_new,fit2_new)
	t5 <- anova(fit3_new,fit2_new)
	t6 <- lrtest(fit2_new,fit1_new)
	t7 <- lrtest(fit2_new,fit3_new)
	OverallAssociationCirrhosisLME[i,1] <- t1$adj.r.squared
	OverallAssociationCirrhosisLME[i,2] <- pf(t1$fstatistic[1],t1$fstatistic[2],t1$fstatistic[3],lower.tail=F)
	OverallAssociationCirrhosisLME[i,3] <- t2$adj.r.squared
	OverallAssociationCirrhosisLME[i,4] <- pf(t2$fstatistic[1],t2$fstatistic[2],t2$fstatistic[3],lower.tail=F)
	OverallAssociationCirrhosisLME[i,5] <- t4$adj.r.squared
	OverallAssociationCirrhosisLME[i,6] <- pf(t4$fstatistic[1],t4$fstatistic[2],t4$fstatistic[3],lower.tail=F)
	OverallAssociationCirrhosisLME[i,7] <- as.numeric(t3$Pr[2])
	OverallAssociationCirrhosisLME[i,8] <- as.numeric(t3$Sum[2])
	OverallAssociationCirrhosisLME[i,9] <- as.numeric(t5$Pr[2])
	OverallAssociationCirrhosisLME[i,10] <- as.numeric(t5$Sum[2])
	OverallAssociationCirrhosisLME[i,11] <- AIC(fit1_new)
	OverallAssociationCirrhosisLME[i,12] <- AIC(fit2_new)
	OverallAssociationCirrhosisLME[i,13] <- AIC(fit3_new)
	OverallAssociationCirrhosisLME[i,14] <- BIC(fit1_new)
	OverallAssociationCirrhosisLME[i,15] <- BIC(fit2_new)
	OverallAssociationCirrhosisLME[i,16] <- BIC(fit3_new)
	OverallAssociationCirrhosisLME[i,17] <- t6$Pr[2]
	OverallAssociationCirrhosisLME[i,18] <- t7$Pr[2]
	i <- i + 1
}

#OverallAssociationCirrhosisLME[,2] <- p.adjust(OverallAssociationCirrhosisLME[,2])
#OverallAssociationCirrhosisLME[,4] <- p.adjust(OverallAssociationCirrhosisLME[,4])
#OverallAssociationCirrhosisLME[,6] <- p.adjust(OverallAssociationCirrhosisLME[,6])
#OverallAssociationCirrhosisLME[,7] <- p.adjust(OverallAssociationCirrhosisLME[,7])
#OverallAssociationCirrhosisLME[,9] <- p.adjust(OverallAssociationCirrhosisLME[,9])
#OverallAssociationCirrhosisLME[,19] <- p.adjust(OverallAssociationCirrhosisLME[,17])
#OverallAssociationCirrhosisLME[,20] <- p.adjust(OverallAssociationCirrhosisLME[,18])

FilteredOverallAssociationCirrhosisLME <- OverallAssociationCirrhosisLME[Cirrhosis_select_species,]
FilteredOverallAssociationCirrhosisLME[,1] <- ifelse(FilteredOverallAssociationCirrhosisLME[,1] < 0,min((FilteredOverallAssociationCirrhosisLME[,1])[FilteredOverallAssociationCirrhosisLME[,1]>0]),FilteredOverallAssociationCirrhosisLME[,1])
FilteredOverallAssociationCirrhosisLME[,3] <- ifelse(FilteredOverallAssociationCirrhosisLME[,3] < 0,min((FilteredOverallAssociationCirrhosisLME[,3])[FilteredOverallAssociationCirrhosisLME[,3]>0]),FilteredOverallAssociationCirrhosisLME[,3])
FilteredOverallAssociationCirrhosisLME[,5] <- ifelse(FilteredOverallAssociationCirrhosisLME[,5] < 0,min((FilteredOverallAssociationCirrhosisLME[,5])[FilteredOverallAssociationCirrhosisLME[,5]>0]),FilteredOverallAssociationCirrhosisLME[,5])
	
