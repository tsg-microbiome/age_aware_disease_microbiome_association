library(lmtest)

features <- unique(c(T2DYoungTop85,T2DMiddleTop85,T2DElderlyTop85))

T2DCountryNewCohort <- c(intersect(c(Young,Middle),AsiaIndividuals),intersect(Elderly,c(AllEUIndividuals,AsiaIndividuals)))

AllDiseased <- intersect(T2DIndividuals,T2DCountryNewCohort)
AllControls <- intersect(SelectControls,T2DCountryNewCohort)
OverallAssociationT2DLME <- as.data.frame(matrix(NA,length(features),20))
rownames(OverallAssociationT2DLME) <- features
colnames(OverallAssociationT2DLME) <- c("RSquared_Fit1","PValue_Fit1","RSquared_Fit2","PValue_Fit2","RSquared_Fit3","PValue_Fit3","ModelAnovaP_Fit2_Fit1","ModelAnovaSumOfSq_Fit2_Fit1","ModelAnovaP_Fit2_Fit3","ModelAnovaSumOfSq_Fit2_Fit3","AIC_Fit1","AIC_Fit2","AIC_Fit3","BIC_Fit1","BIC_Fit2","BIC_Fit3","Lrt_P_Fit2_Fit1","Lrt_P_Fit2_Fit3","Adjust_Lrt_P_Fit2_Fit1","Adjust_Lrt_P_Fit2_Fit3")

Diseased <- intersect(T2DIndividuals,T2DCountryNewCohort)
Controls <- intersect(SelectControls,T2DCountryNewCohort)
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
	OverallAssociationT2DLME[i,1] <- t1$adj.r.squared
	OverallAssociationT2DLME[i,2] <- pf(t1$fstatistic[1],t1$fstatistic[2],t1$fstatistic[3],lower.tail=F)
	OverallAssociationT2DLME[i,3] <- t2$adj.r.squared
	OverallAssociationT2DLME[i,4] <- pf(t2$fstatistic[1],t2$fstatistic[2],t2$fstatistic[3],lower.tail=F)
	OverallAssociationT2DLME[i,5] <- t4$adj.r.squared
	OverallAssociationT2DLME[i,6] <- pf(t4$fstatistic[1],t4$fstatistic[2],t4$fstatistic[3],lower.tail=F)
	OverallAssociationT2DLME[i,7] <- as.numeric(t3$Pr[2])
	OverallAssociationT2DLME[i,8] <- as.numeric(t3$Sum[2])
	OverallAssociationT2DLME[i,9] <- as.numeric(t5$Pr[2])
	OverallAssociationT2DLME[i,10] <- as.numeric(t5$Sum[2])
	OverallAssociationT2DLME[i,11] <- AIC(fit1_new)
	OverallAssociationT2DLME[i,12] <- AIC(fit2_new)
	OverallAssociationT2DLME[i,13] <- AIC(fit3_new)
	OverallAssociationT2DLME[i,14] <- BIC(fit1_new)
	OverallAssociationT2DLME[i,15] <- BIC(fit2_new)
	OverallAssociationT2DLME[i,16] <- BIC(fit3_new)
	OverallAssociationT2DLME[i,17] <- t6$Pr[2]
	OverallAssociationT2DLME[i,18] <- t7$Pr[2]
	i <- i + 1
}

#OverallAssociationT2DLME[,2] <- p.adjust(OverallAssociationT2DLME[,2])
#OverallAssociationT2DLME[,4] <- p.adjust(OverallAssociationT2DLME[,4])
#OverallAssociationT2DLME[,6] <- p.adjust(OverallAssociationT2DLME[,6])
#OverallAssociationT2DLME[,7] <- p.adjust(OverallAssociationT2DLME[,7])
#OverallAssociationT2DLME[,9] <- p.adjust(OverallAssociationT2DLME[,9])
#OverallAssociationT2DLME[,19] <- p.adjust(OverallAssociationT2DLME[,17])
#OverallAssociationT2DLME[,20] <- p.adjust(OverallAssociationT2DLME[,18])

FilteredOverallAssociationT2DLME <- OverallAssociationT2DLME[T2D_select_species,]
FilteredOverallAssociationT2DLME[,1] <- ifelse(FilteredOverallAssociationT2DLME[,1] < 0,min((FilteredOverallAssociationT2DLME[,1])[FilteredOverallAssociationT2DLME[,1]>0]),FilteredOverallAssociationT2DLME[,1])
FilteredOverallAssociationT2DLME[,3] <- ifelse(FilteredOverallAssociationT2DLME[,3] < 0,min((FilteredOverallAssociationT2DLME[,3])[FilteredOverallAssociationT2DLME[,3]>0]),FilteredOverallAssociationT2DLME[,3])
FilteredOverallAssociationT2DLME[,5] <- ifelse(FilteredOverallAssociationT2DLME[,5] < 0,min((FilteredOverallAssociationT2DLME[,5])[FilteredOverallAssociationT2DLME[,5]>0]),FilteredOverallAssociationT2DLME[,5])
	
