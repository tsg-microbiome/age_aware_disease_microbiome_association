AllDiseased <- intersect(CirrhosisIndividuals,CirrhosisCountryCohort)
AllControls <- intersect(SelectControls,CirrhosisCountryCohort)
adonisCirrhosisDiseaseIntAgeGroup <- adonis(as.dist(1-cor(t(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),core_species]),method="spearman")/2)~as.factor(c(rep("Diseased",length(AllDiseased)),rep("Controls",length(AllControls))))*as.factor(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),"age_group"]))

AllDiseased <- intersect(T2DIndividuals,T2DCountryCohort)
AllControls <- intersect(SelectControls,T2DCountryCohort)
adonisT2DDiseaseIntAgeGroup <- adonis(as.dist(1-cor(t(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),core_species]),method="spearman")/2)~as.factor(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),"country"])+as.factor(c(rep("Diseased",length(AllDiseased)),rep("Controls",length(AllControls))))*as.factor(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),"age_group"]))

AllDiseased <- intersect(IBDIndividuals,IBDCountryCohort)
AllControls <- intersect(SelectControls,IBDCountryCohort)
adonisIBDDiseaseIntAgeGroup <- adonis(as.dist(1-cor(t(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),core_species]),method="spearman")/2)~as.factor(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),"country"])+as.factor(c(rep("Diseased",length(AllDiseased)),rep("Controls",length(AllControls))))*as.factor(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),"age_group"]))

AllDiseased <- intersect(CRCIndividuals,CRCCountryCohort)
AllControls <- intersect(SelectControls,CRCCountryCohort)
adonisCRCDiseaseIntAgeGroup <- adonis(as.dist(1-cor(t(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),core_species]),method="spearman")/2)~as.factor(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),"country"])+as.factor(c(rep("Diseased",length(AllDiseased)),rep("Controls",length(AllControls))))*as.factor(ifelse(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),"age_group"] %in% c("Young","Middle"),"YoungMiddle","Elderly")))

AllDiseased <- intersect(AdenomaIndividuals,AdenomaCountryCohort)
AllControls <- intersect(SelectControls,AdenomaCountryCohort)
adonisAdenomaDiseaseIntAgeGroup <- adonis(as.dist(1-cor(t(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),core_species]),method="spearman")/2)~as.factor(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),"country"])+as.factor(c(rep("Diseased",length(AllDiseased)),rep("Controls",length(AllControls))))*as.factor(ifelse(combined_species_profile_with_age_country_final1[c(AllDiseased,AllControls),"age_group"] %in% c("Young","Middle"),"YoungMiddle","Elderly")))
