#Groups at the level of country
CRCCountryCohort <- rownames(combined_short_metadata_2[combined_short_metadata_2$country %in% droplevels(combined_short_metadata_2[CRCIndividuals,"country"]),])
AdenomaCountryCohort <- rownames(combined_short_metadata_2[combined_short_metadata_2$country %in% droplevels(combined_short_metadata_2[AdenomaIndividuals,"country"]),])
CirrhosisCountryCohort <- rownames(combined_short_metadata_2[combined_short_metadata_2$country %in% droplevels(combined_short_metadata_2[CirrhosisIndividuals,"country"]),])
IBDCountryCohort <- rownames(combined_short_metadata_2[combined_short_metadata_2$country %in% droplevels(combined_short_metadata_2[IBDIndividuals,"country"]),])
T2DCountryCohort <- c(intersect(c(Young,Middle),AsiaIndividuals),intersect(Elderly,c(AllEUIndividuals,AsiaIndividuals)))
#Groups at the level of study
CRCStudyCohort <- c(ZellerG_2014Individuals,VogtmannE_2016Individuals,FengQ_2015Individuals)
AdenomaStudyCohort <- c(ZelleG_2014Individuals,FengQ_2015Individuals)
IBDStudyCohort <- c(FranzosaEA_2018Individuals,NielsenHB_2014Individuals)
T2DStudyCohort <- c(KarlssonFH_2013Individuals,QinJ_2012Individuals)
CirrhosisStudyCohort <- c(QinN_2014Individuals)
#Groups at the level of continent
CRCCohort <- c(AllEUIndividuals,NorthAmericaIndividuals)
IBDCOhort <- c(AllEUIndividuals,NorthAmericaIndividuals)
T2DCohort <- c(AsiaIndividuals,AllEUIndividuals)
CirrhosisCohort <- c(AsiaIndividuals)
AdenomaCohort <- c(AllEUIndividuals)
