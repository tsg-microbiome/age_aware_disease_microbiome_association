#Loads Libraries
library(pROC)
library(randomForest)
library(gtools)
library(vegan)
library(ade4)
library(randomForestExplainer)
library(effsize)
library(gtools)
library(RColorBrewer)

#Codes
wilcox_batch = function(x,y)
{
	p_array <- NULL;
	type_array <- NULL;
	mean1_array <- NULL;
	mean2_array <- NULL;
	x <- x[abs(rowSums(x,na.rm=TRUE)) > 0,];
	y <- y[abs(rowSums(y,na.rm=TRUE)) > 0,];
	z <- intersect(rownames(x),rownames(y));
	for(i in 1:length(z))
	{
		p_array[i] <- wilcox.test(as.numeric(x[z[i],]),as.numeric(y[z[i],]))$p.value;
		type_array[i] <- ifelse(mean(as.numeric(x[z[i],]),na.rm=TRUE) > mean(as.numeric(y[z[i],]),na.rm=TRUE), 1, ifelse(mean(as.numeric(x[z[i],]),na.rm=TRUE) < mean(as.numeric(y[z[i],]),na.rm=TRUE),-1,0));
		mean1_array[i] <- mean(as.numeric(x[z[i],]),na.rm=TRUE);
		mean2_array[i] <- mean(as.numeric(y[z[i],]),na.rm=TRUE);
		i <- i + 1;
	}
	out <- as.data.frame(cbind(p_array,type_array,p.adjust(p_array),mean1_array,mean2_array));
	rownames(out) <- z;
	out <- apply(out,1,function(x)(ifelse(is.nan(x),1,x)));
	return(t(out));
}

iterative_rf_fixed_size = function(data,window1,window2,disease,control,iter,trainsize,testsize)
{
	set.seed(150);
	featureProfile <- as.data.frame(matrix(NA,iter,ncol(data)));
	AUCArray <- NULL;
	SensitivityArray <- NULL;
	SpecificityArray <- NULL;
	numberDiseasedSamples <- NULL;
	numberControlSamples <- NULL;
	trainDiseaseSamples <- NULL;
	testDiseaseSamples <- NULL;
	trainControlSamples <- NULL;
	testControlSamples <- NULL;
	#threshold <- ifelse(length(intersect(window,disease)) <= 20,10,20);
	for(i in 1:iter)
	{
		if(length(intersect(window1,disease)) > trainsize)
		{
			trainDisease <- sample(intersect(window1,disease),trainsize,replace=FALSE);
		}
		else
		{
			trainDisease <- sample(intersect(window1,disease),trainsize,replace=TRUE);
		}

		if(length(intersect(window1,control)) > trainsize)
		{
			trainControl <- sample(intersect(window1,control),trainsize,replace=FALSE);
		}
		else
		{
			trainControl <- sample(intersect(window1,control),trainsize,replace=TRUE);
		}
		
		tempTrain <- rbind(data[trainDisease,],data[trainControl,]);
		rownames(tempTrain) <- make.names(rownames(tempTrain),unique=TRUE);
		TrainDiseaseTags <- NULL;
		TrainDiseaseTags[1:length(trainDisease)] <- "Diseased";
		TrainControlTags <- NULL;
		TrainControlTags[1:length(trainControl)] <- "Control";
		numberDiseasedSamples[i] <- nrow(trainDisease);
		numberControlSamples[i] <- nrow(trainControl);
		TrainTags <- c(TrainDiseaseTags,TrainControlTags);
		rfTempComp <- randomForest(as.factor(TrainTags)~.,tempTrain);
		#print("model created");
		featureProfile[i,] <- sapply(colnames(tempTrain),function(x)(ifelse(x %in% rownames(rfTempComp$importance),rfTempComp$importance[x,],0)));
		if(length(setdiff(window1,window2)) == 0)
		{
			
			if(length(setdiff(intersect(window1,disease),trainDisease)) > testsize)
			{
				#print("LOOP1a");
				testDisease <- sample(setdiff(intersect(window1,disease),trainDisease),testsize,replace=FALSE);
				#print(testDisease);
			}
			else
			{	
				#print(intersect(window1,disease));
				testDisease <- sample(setdiff(intersect(window1,disease),trainDisease),testsize,replace=TRUE);
			}
			
			if(length(setdiff(intersect(window1,control),trainControl)) > testsize)
			{
				testControl <- sample(setdiff(intersect(window1,control),trainControl),testsize,replace=FALSE);
			}
			else
			{
				testControl <- sample(setdiff(intersect(window1,control),trainControl),testsize,replace=TRUE);
			}
			#print("test created");
			
		}
		else
		{
			if(length(intersect(window2,disease)) > testsize)
			{
				testDisease <- sample(intersect(window2,disease),testsize,replace=FALSE);
			}
			else
			{
				testDisease <- sample(intersect(window2,disease),testsize,replace=TRUE);
			}
			
			if(length(intersect(window2,control)) > testsize)
			{
				testControl <- sample(intersect(window2,control),testsize,replace=FALSE);
			}
			else
			{
				testControl <- sample(intersect(window2,control),testsize,replace=TRUE);
			}
			
			
		}
		tempTest <- rbind(data[testDisease,],data[testControl,]);
		TestDiseaseTags <- NULL;
		TestDiseaseTags[1:length(testDisease)] <- "Diseased";
		TestControlTags <- NULL;
		TestControlTags[1:length(testControl)] <- "Control";
		TestTags <- c(TestDiseaseTags,TestControlTags);
		rownames(tempTest) <- make.names(rownames(tempTest),unique=TRUE);
		rfTempPredict <- predict(rfTempComp,tempTest,type="vote",norm.votes=TRUE);
		print(i);
		AUCArray[i] <- auc(TestTags,rfTempPredict[,2])[1];
		print(median(AUCArray));
		SensitivityArray[i] <- length(which(predict(rfTempComp,tempTest[1:testsize,])=="Diseased"))/length(predict(rfTempComp,tempTest[1:testsize,]));
		SpecificityArray[i] <- length(which(predict(rfTempComp,tempTest[(testsize+1):nrow(tempTest),])=="Control"))/length(predict(rfTempComp,tempTest[(testsize+1):nrow(tempTest),]));
		if(i == 100)
		{
			trainDiseaseSamples <- trainDisease;
			testDiseaseSamples <- testDisease;
			trainControlSamples <- trainControl;
			testControlSamples <- testControl;
		}
		i <- i + 1;
	}
	colnames(featureProfile) <- colnames(tempTrain);
	#returnList <- list("featureProfile"=featureProfile,"AUC"=AUCArray,"Accuracy"=AccuracyArray);
	returnList <- list("AUC"=AUCArray,"Sensitivity"=SensitivityArray,"Specificity"=SpecificityArray,"numberDiseasedSamples"=numberDiseasedSamples,"numberControlSamples"=numberControlSamples,"featureProfile"=featureProfile,"trainDisease100"=trainDiseaseSamples,"trainControl100"=trainControlSamples,"testDisease100"=testDiseaseSamples,"testControl100"=testControlSamples);
	return(returnList);
		
}

rank_scale=function(x)
{
	x <- rank(x);
	y <- (rank(x)-min(rank(x)))/(max(rank(x))-min(rank(x)));
	return(y);
}

rank_scale2=function(x,range_min,range_max)
{
	x <- rank(x);
	y <- range_min + (range_max - range_min)*(rank(x)-min(rank(x)))/(max(rank(x))-min(rank(x)));
	return(y);
}

range_scale=function(x)
{
	y <- (x-min(x))/(max(x)-min(x));
	return(y);
}

range_scale2=function(x,range_min,range_max)
{
	y <- range_min + (range_max - range_min)*(x-min(x))/(max(x)-min(x));
	return(y);
}

effect_size_calculator=function(x,y)
{
	library(effsize);
	species_list <- intersect(colnames(x),colnames(y));
	effect_size = matrix(NA,length(species_list),5);
	rownames(effect_size) <- species_list;
	for(i in 1:length(species_list))
	{
		print(species_list[i]);
		gg <- effsize::cohen.d(x[,species_list[i]],y[,species_list[i]])
		#print(gg);
		gt <- wilcox.test(x[,species_list[i]],y[,species_list[i]])
		effect_size[i,1] <- as.numeric(gg$estimate[1]);
		effect_size[i,2] <- as.factor(gg$magnitude[1]);
		effect_size[i,3] <- as.numeric(gt$p.value[1]);
		effect_size[i,4] <- mean(x[,species_list[i]]);
		effect_size[i,5] <- mean(y[,species_list[i]]);
	}
	colnames(effect_size) <- c("Estimate","Level","NominalP","MedianGroup1","MedianGroup2");
	effect_size <- apply(effect_size,2,function(x)(ifelse(is.nan(x),0,x)))
	#effect_size <- effect_size[names(which(!is.nan(rowSums(effect_size)))),];
	return(effect_size);
}
	
iterative_effsize=function(data1,data2,iter,size)
{
	print(dim(data1));
	print(dim(data2));
	nspecies <- intersect(colnames(data1),colnames(data2));
	
	effsize_matrix <- as.data.frame(matrix(NA,iter,length(nspecies)));
	pvalue_matrix <- as.data.frame(matrix(NA,iter,length(nspecies)));
	if(length(nspecies) > 1)
	{
		data1 <- data1[,nspecies];
		data2 <- data2[,nspecies];
		for(i in 1:iter)
		{
			tempRow1 <- sample(rownames(data1),size,replace=FALSE);
			tempRow2 <- sample(rownames(data2),size,replace=FALSE);
			gg <- effect_size_calculator(data1[tempRow1,],data2[tempRow2,]);
			#print(nrow(gg));
			effsize_matrix[i,] <- gg[,1];
			pvalue_matrix[i,] <- -log(gg[,3],10);
		}
	}
	else
	{
		data1_rownames <- rownames(data1);
		data2_rownames <- rownames(data2);
		data1 <- as.data.frame(data1[,nspecies]);
		data2 <- as.data.frame(data2[,nspecies]);
		rownames(data1) <- data1_rownames;
		rownames(data2) <- data2_rownames;
		for(i in 1:iter)
		{
			tempRow1 <- rownames(data1)[sample(1:nrow(data1),size,replace=FALSE)];
			tempRow2 <- rownames(data2)[sample(1:nrow(data2),size,replace=FALSE)];
			gg <- effect_size_calculator(data1[tempRow1,],data2[tempRow2,]);
			#print(nrow(gg));
			effsize_matrix[i,] <- gg[,1];
			pvalue_matrix[i,] <- -log(gg[,3],10);
		}
	}
	colnames(effsize_matrix) <- nspecies;
	colnames(pvalue_matrix) <- nspecies;
	pvalue_matrix <- apply(pvalue_matrix,2,function(x)(ifelse(is.finite(x),x,0)));
	returnlist <- list("EffSize"=effsize_matrix,"PValue"=pvalue_matrix);
	return(returnlist);
}

