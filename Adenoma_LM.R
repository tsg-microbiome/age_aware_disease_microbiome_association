library(lmtest)

features <- unique(c(AdenomaYoungMiddleTop85,AdenomaElderlyTop85))

AllDiseased <- intersect(AdenomaIndividuals,AdenomaCountryCohort)
AllControls <- intersect(SelectControls,AdenomaCountryCohort)
OverallAssociationAdenomaLME <- as.data.frame(matrix(NA,length(features),20))
rownames(OverallAssociationAdenomaLME) <- features
colnames(OverallAssociationAdenomaLME) <- c("RSquared_Fit1","PValue_Fit1","RSquared_Fit2","PValue_Fit2","RSquared_Fit3","PValue_Fit3","ModelAnovaP_Fit2_Fit1","ModelAnovaSumOfSq_Fit2_Fit1","ModelAnovaP_Fit2_Fit3","ModelAnovaSumOfSq_Fit2_Fit3","AIC_Fit1","AIC_Fit2","AIC_Fit3","BIC_Fit1","BIC_Fit2","BIC_Fit3","Lrt_P_Fit2_Fit1","Lrt_P_Fit2_Fit3","Adjust_Lrt_P_Fit2_Fit1","Adjust_Lrt_P_Fit2_Fit3")

Diseased <- intersect(AdenomaIndividuals,AdenomaCountryCohort)
Controls <- intersect(SelectControls,AdenomaCountryCohort)
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
	OverallAssociationAdenomaLME[i,1] <- t1$adj.r.squared
	OverallAssociationAdenomaLME[i,2] <- pf(t1$fstatistic[1],t1$fstatistic[2],t1$fstatistic[3],lower.tail=F)
	OverallAssociationAdenomaLME[i,3] <- t2$adj.r.squared
	OverallAssociationAdenomaLME[i,4] <- pf(t2$fstatistic[1],t2$fstatistic[2],t2$fstatistic[3],lower.tail=F)
	OverallAssociationAdenomaLME[i,5] <- t4$adj.r.squared
	OverallAssociationAdenomaLME[i,6] <- pf(t4$fstatistic[1],t4$fstatistic[2],t4$fstatistic[3],lower.tail=F)
	OverallAssociationAdenomaLME[i,7] <- as.numeric(t3$Pr[2])
	OverallAssociationAdenomaLME[i,8] <- as.numeric(t3$Sum[2])
	OverallAssociationAdenomaLME[i,9] <- as.numeric(t5$Pr[2])
	OverallAssociationAdenomaLME[i,10] <- as.numeric(t5$Sum[2])
	OverallAssociationAdenomaLME[i,11] <- AIC(fit1_new)
	OverallAssociationAdenomaLME[i,12] <- AIC(fit2_new)
	OverallAssociationAdenomaLME[i,13] <- AIC(fit3_new)
	OverallAssociationAdenomaLME[i,14] <- BIC(fit1_new)
	OverallAssociationAdenomaLME[i,15] <- BIC(fit2_new)
	OverallAssociationAdenomaLME[i,16] <- BIC(fit3_new)
	OverallAssociationAdenomaLME[i,17] <- t6$Pr[2]
	OverallAssociationAdenomaLME[i,18] <- t7$Pr[2]
	i <- i + 1
}

#OverallAssociationAdenomaLME[,2] <- p.adjust(OverallAssociationAdenomaLME[,2])
#OverallAssociationAdenomaLME[,4] <- p.adjust(OverallAssociationAdenomaLME[,4])
#OverallAssociationAdenomaLME[,6] <- p.adjust(OverallAssociationAdenomaLME[,6])
#OverallAssociationAdenomaLME[,7] <- p.adjust(OverallAssociationAdenomaLME[,7])
#OverallAssociationAdenomaLME[,9] <- p.adjust(OverallAssociationAdenomaLME[,9])
#OverallAssociationAdenomaLME[,19] <- p.adjust(OverallAssociationAdenomaLME[,17])
#OverallAssociationAdenomaLME[,20] <- p.adjust(OverallAssociationAdenomaLME[,18])

FilteredOverallAssociationAdenomaLME <- OverallAssociationAdenomaLME[Adenoma_select_species,]
FilteredOverallAssociationAdenomaLME[,1] <- ifelse(FilteredOverallAssociationAdenomaLME[,1] < 0,min((FilteredOverallAssociationAdenomaLME[,1])[FilteredOverallAssociationAdenomaLME[,1]>0]),FilteredOverallAssociationAdenomaLME[,1])
FilteredOverallAssociationAdenomaLME[,3] <- ifelse(FilteredOverallAssociationAdenomaLME[,3] < 0,min((FilteredOverallAssociationAdenomaLME[,3])[FilteredOverallAssociationAdenomaLME[,3]>0]),FilteredOverallAssociationAdenomaLME[,3])
FilteredOverallAssociationAdenomaLME[,5] <- ifelse(FilteredOverallAssociationAdenomaLME[,5] < 0,min((FilteredOverallAssociationAdenomaLME[,5])[FilteredOverallAssociationAdenomaLME[,5]>0]),FilteredOverallAssociationAdenomaLME[,5])
	
