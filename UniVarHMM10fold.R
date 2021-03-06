library(doSNOW)

predict <- function(exam, iter, steps){
library(HMM)
print(paste(exam, "loading data"))
d <- read.csv(file=paste(c("C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\data\\",exam,".csv"),collapse=""),head=TRUE,sep=",", stringsAsFactors=FALSE)
#d <- read.csv(file="C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\data\\ALB.csv",head=TRUE,sep=",", stringsAsFactors=FALSE)

#exams <- c("CHE","T-CHO","TP","Type","Activity")

print(summary(d))
prob <- function (x) {x / sum (x)}  # Makes it a probability (it sums to 1)
# "P", "PP" and "PPP" sao fillers para que o num de simbolos seja multiplo do num de estados
if(exam == "ALB"){
	vals <- c("H","N","L","VL")
}else{
	if(exam == "WBC" || exam == "PLT"){
		vals <- c("UL","VL","L","N","H")
	}else{
		if(exam == "RBC" || exam == "HGB" || exam == "HCT" || exam == "MCV"){
			vals <- c("H","N","L")
		}else{
			if(exam == "Type"){
				vals <- c("B","C","P","PP")
			} else {
				if(exam == "CHE" || exam == "T-CHO" || exam == "TP"){
					vals <- c("H","N","L","VL","VH","P", "PP", "PPP")
				} else {
					if(exam == "Activity"){
						vals <- c("A2","A1","A3","P")
					} else {
						vals <- c("N","H","VH","UH")
					}
				}
			}
		}
	}
}

rows <- nrow(d)
fold <- floor(rows/10)

folds <- c(1,fold,fold*2,fold*3,fold*4,fold*5,fold*6,fold*7,fold*8,fold*9)
fileConn<-file(paste(c("C:\\hepat_data030704\\data\\predictionsHMM\\new\\",exam,"_Predictions.csv"),collapse=""))
#fileConn<-file("C:\\hepat_data030704\\data\\predictionsHMM\\ALB_Predictions.csv")
for(p in 1:9){
	print(p)
	if( p == 1){
	train <- d[folds[p+1]+1:nrow(d)-fold,]
	test <- d[folds[p]:folds[p+1],]
	print(paste("train ", folds[p+1]+1,"-" , nrow(d)))
	print(paste("test ", folds[p],"-" , folds[p+1]))
	}else{
		if(p == 9){
			train <- d[1:folds[p]-1,]
			test <- d[folds[p]:nrow(d),]
			print(paste("train ", 1,"-",folds[p]-1))
			print(paste("test ", folds[p],"-" , nrow(d)))
		}else {
			train1 <- d[1:folds[p]-1,]
			train2 <- d[folds[p+1]+1:nrow(d),]
			size <- nrow(d) - folds[p+1]
			test <- d[folds[p]:folds[p+1],]
			train <- rbind(train1, train2[1,])
			for(k in 2:size){
				train <- rbind(train, train2[k,])
			}
			print(paste("train ", 1,"-",folds[p]-1))
			print(paste("test ", folds[p],"-" , folds[p+1]))
			print(paste("train ", folds[p+1]+1,"-",nrow(d)))
		}
	}
	
#	hmm = initHMM(c("1","2","3","4"), vals,
#		transProbs=matrix(c(.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25),4),
#		emissionProbs=matrix(c(.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25),4))	

	print(paste(exam, "initialization"))
	hmm = initHMM(c("1","2","3","4"), vals, startProbs=(prob (runif (4))),
		transProbs=apply (matrix (runif(16), 4), 1, prob),
		emissionProbs=apply (matrix (runif(16), 4), 1, prob))	
		
	#hmm = initHMM(c("1","2"), vals, startProbs=(prob (runif (2))),
	#	transProbs=apply (matrix (runif(4), 2), 1, prob),
	#	emissionProbs=apply (matrix (runif(4), 2), 1, prob))
	
	#train hmm
	print(paste(exam, "Build training"))
	m = 1
	observations <- vector()
	for (i in 1:nrow(train)) {
		for (j in 2:ncol(train)) {
			observations[m] <- train[[i,j]]
			m = m + 1
		}
	}
	print(paste(exam, "BaumWelch", "iter ->", iter))
	vt = baumWelch(hmm, observations, maxIterations=iter, delta=1E-9, pseudoCount=0)

	#predict
	print(paste(exam, "Forward"))
	for (i in 1:nrow(test)) {
		m = 1
		observations <- vector()
		#get values of row
		for (j in seq(2, ncol(test)-1, by=1)) {
			observations[m] <- test[[i,j]]
			m = m + 1
		}
		#forward and save for every possible value
		probs <- vector()
		for(j in 1:length(vals)){
			observations[m] <- vals[j]
			f <- forward(vt$hmm, observations)
			#print(observations)
			#print(f)
			probs[j] <- f[1,steps]
			for(k in 2:4){
				if (f[k,steps] > probs[j]){
					probs[j] <- f[k,steps]
				}
			}
		}
		max <- (-200)
		for(j in 1:length(vals)){
			if (probs[j] > max){
				index <- j
				max <- probs[j]
			}
		}
		#if( index != 2){
	#		print(observations)
	#		print(paste("chosen",vals[index]))
	#	}
		obs <- vector()
		obs[1] <- test[i,1]
		obs[2] <- vals[index]
		#print(test[i,])
		#print(obs)
		if( p == 1 && i == 1){
			text <- paste(obs,collapse=",")
		}else{
			text <- c(text,paste(obs,collapse=","))
		}
	}
}
write(text,fileConn)
close(fileConn)
}

predict2 <- function(exam, iter, steps){
print(paste(exam, "loading data"))
d <- read.csv(file=paste(c("C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\data\\",exam,".csv"),collapse=""),head=TRUE,sep=",", stringsAsFactors=FALSE)
#d <- read.csv(file="C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\data\\ALB.csv",head=TRUE,sep=",", stringsAsFactors=FALSE)

#exams <- c("CHE","T-CHO","TP","Type","Activity")

print(summary(d))
prob <- function (x) {x / sum (x)}  # Makes it a probability (it sums to 1)
# "P", "PP" and "PPP" sao fillers para que o num de simbolos seja multiplo do num de estados
if(exam == "ALB"){
	vals <- c("H","N","L","VL")
}else{
	if(exam == "WBC" || exam == "PLT"){
		vals <- c("UL","VL","L","N","H")
	}else{
		if(exam == "RBC" || exam == "HGB" || exam == "HCT" || exam == "MCV"){
			vals <- c("H","N","L")
		}else{
			if(exam == "Type"){
				vals <- c("B","C")
			} else {
				if(exam == "CHE" || exam == "T-CHO" || exam == "TP"){
					vals <- c("H","N","L","VL","VH","P")
				} else {
					if(exam == "Activity"){
						vals <- c("A2","A1","A3","P")
					} else {
						vals <- c("N","H","VH","UH")
					}
				}
			}
		}
	}
}

rows <- nrow(d)
fold <- floor(rows/10)

folds <- c(1,fold,fold*2,fold*3,fold*4,fold*5,fold*6,fold*7,fold*8,fold*9)
fileConn<-file(paste(c("C:\\hepat_data030704\\data\\predictionsHMM\\new\\",exam,"_Predictions.csv"),collapse=""))
#fileConn<-file("C:\\hepat_data030704\\data\\predictionsHMM\\ALB_Predictions.csv")
for(p in 1:9){
	print(p)
	if( p == 1){
	train <- d[folds[p+1]+1:nrow(d)-fold,]
	test <- d[folds[p]:folds[p+1],]
	print(paste("train ", folds[p+1]+1,"-" , nrow(d)))
	print(paste("test ", folds[p],"-" , folds[p+1]))
	}else{
		if(p == 9){
			train <- d[1:folds[p]-1,]
			test <- d[folds[p]:nrow(d),]
			print(paste("train ", 1,"-",folds[p]-1))
			print(paste("test ", folds[p],"-" , nrow(d)))
		}else {
			train1 <- d[1:folds[p]-1,]
			train2 <- d[folds[p+1]+1:nrow(d),]
			size <- nrow(d) - folds[p+1]
			test <- d[folds[p]:folds[p+1],]
			train <- rbind(train1, train2[1,])
			for(k in 2:size){
				train <- rbind(train, train2[k,])
			}
			print(paste("train ", 1,"-",folds[p]-1))
			print(paste("test ", folds[p],"-" , folds[p+1]))
			print(paste("train ", folds[p+1]+1,"-",nrow(d)))
		}
	}

	print(paste(exam, "initialization"))
		
	hmm = initHMM(c("1","2"), vals, startProbs=(prob (runif (2))),
		transProbs=apply (matrix (runif(4), 2), 1, prob),
		emissionProbs=apply (matrix (runif(4), 2), 1, prob))
	
	#train hmm
	print(paste(exam, "Build training"))
	m = 1
	observations <- vector()
	for (i in 1:nrow(train)) {
		for (j in 2:ncol(train)) {
			observations[m] <- train[[i,j]]
			m = m + 1
		}
	}
	print(paste(exam, "BaumWelch", "iter ->", iter))
	vt = baumWelch(hmm, observations, maxIterations=iter, delta=1E-9, pseudoCount=0)

	#predict
	print(paste(exam, "Forward"))
	for (i in 1:nrow(test)) {
		m = 1
		observations <- vector()
		#get values of row
		for (j in seq(2, ncol(test)-1, by=1)) {
			observations[m] <- test[[i,j]]
			m = m + 1
		}
		#forward and save for every possible value
		probs <- vector()
		for(j in 1:length(vals)){
			observations[m] <- vals[j]
			f <- forward(vt$hmm, observations)
			#print(observations)
			#print(f)
			probs[j] <- f[1,steps]
			k <- 2
			if (f[k,steps] > probs[j]){
				probs[j] <- f[k,steps]
			}		
		}
		max <- (-200)
		for(j in 1:length(vals)){
			if (probs[j] > max){
				index <- j
				max <- probs[j]
			}
		}
		obs <- vector()
		obs[1] <- test[i,1]
		obs[2] <- vals[index]
		if( p == 1 && i == 1){
			text <- paste(obs,collapse=",")
		}else{
			text <- c(text,paste(obs,collapse=","))
		}
	}
}
write(text,fileConn)
close(fileConn)
}

predict7 <- function(exam, iter, steps){
print(paste(exam, "loading data"))
d <- read.csv(file=paste(c("C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\data\\",exam,".csv"),collapse=""),head=TRUE,sep=",", stringsAsFactors=FALSE)
#d <- read.csv(file="C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\data\\ALB.csv",head=TRUE,sep=",", stringsAsFactors=FALSE)

#exams <- c("CHE","T-CHO","TP","Type","Activity")

print(summary(d))
prob <- function (x) {x / sum (x)}  # Makes it a probability (it sums to 1)
# "P", "PP" and "PPP" sao fillers para que o num de simbolos seja multiplo do num de estados
if(exam == "ALB"){
	vals <- c("H","N","L","VL","P", "PP", "PPP")
}else{
	if(exam == "WBC" || exam == "PLT"){
		vals <- c("UL","VL","L","N","H")
	}else{
		if(exam == "RBC" || exam == "HGB" || exam == "HCT" || exam == "MCV"){
			vals <- c("H","N","L")
		}else{
			if(exam == "Type"){
				vals <- c("B","C","P","PP","PA", "PPA", "APPP")
			} else {
				if(exam == "CHE" || exam == "T-CHO" || exam == "TP"){
					vals <- c("H","N","L","VL","VH","P", "PP")
				} else {
					if(exam == "Activity"){
						vals <- c("A2","A1","A3","P","PA", "APP", "APPP")
					} else {
						vals <- c("N","H","VH","UH","P", "PP", "PPP")
					}
				}
			}
		}
	}
}

rows <- nrow(d)
fold <- floor(rows/10)

folds <- c(1,fold,fold*2,fold*3,fold*4,fold*5,fold*6,fold*7,fold*8,fold*9)
fileConn<-file(paste(c("C:\\hepat_data030704\\data\\predictionsHMM\\new\\",exam,"_Predictions.csv"),collapse=""))
#fileConn<-file("C:\\hepat_data030704\\data\\predictionsHMM\\ALB_Predictions.csv")
for(p in 1:9){
	print(p)
	if( p == 1){
	train <- d[folds[p+1]+1:nrow(d)-fold,]
	test <- d[folds[p]:folds[p+1],]
	print(paste("train ", folds[p+1]+1,"-" , nrow(d)))
	print(paste("test ", folds[p],"-" , folds[p+1]))
	}else{
		if(p == 9){
			train <- d[1:folds[p]-1,]
			test <- d[folds[p]:nrow(d),]
			print(paste("train ", 1,"-",folds[p]-1))
			print(paste("test ", folds[p],"-" , nrow(d)))
		}else {
			train1 <- d[1:folds[p]-1,]
			train2 <- d[folds[p+1]+1:nrow(d),]
			size <- nrow(d) - folds[p+1]
			test <- d[folds[p]:folds[p+1],]
			train <- rbind(train1, train2[1,])
			for(k in 2:size){
				train <- rbind(train, train2[k,])
			}
			print(paste("train ", 1,"-",folds[p]-1))
			print(paste("test ", folds[p],"-" , folds[p+1]))
			print(paste("train ", folds[p+1]+1,"-",nrow(d)))
		}
	}
	
	
	
	print(paste(exam, "initialization"))
	hmm = initHMM(c("1","2","3","4","5","6","7"), vals, startProbs=(prob (runif (7))),
		transProbs=apply (matrix (runif(49), 7), 1, prob),
		emissionProbs=apply (matrix (runif(49), 7), 1, prob))	
		
	#hmm = initHMM(c("1","2"), vals, startProbs=(prob (runif (2))),
	#	transProbs=apply (matrix (runif(4), 2), 1, prob),
	#	emissionProbs=apply (matrix (runif(4), 2), 1, prob))
	
	#train hmm
	print(paste(exam, "Build training"))
	m = 1
	observations <- vector()
	for (i in 1:nrow(train)) {
		for (j in 2:ncol(train)) {
			observations[m] <- train[[i,j]]
			m = m + 1
		}
	}
	print(paste(exam, "BaumWelch ", "iter ->", iter))
	vt = baumWelch(hmm, observations, maxIterations=iter, delta=1E-9, pseudoCount=0)

	#predict
	print(paste(exam, "Forward"))
	for (i in 1:nrow(test)) {
		m = 1
		observations <- vector()
		#get values of row
		for (j in seq(2, ncol(test)-1, by=1)) {
			observations[m] <- test[[i,j]]
			m = m + 1
		}
		#forward and save for every possible value
		probs <- vector()
		for(j in 1:length(vals)){
			observations[m] <- vals[j]
			f <- forward(vt$hmm, observations)
			#print(observations)
			#print(f)
			probs[j] <- f[1,steps]
			for(k in 2:7){
				if (f[k,steps] > probs[j]){
					probs[j] <- f[k,steps]
				}
			}
		}
		max <- (-200)
		for(j in 1:length(vals)){
			if (probs[j] > max){
				index <- j
				max <- probs[j]
			}
		}
		#if( index != 2){
	#		print(observations)
	#		print(paste("chosen",vals[index]))
	#	}
		obs <- vector()
		obs[1] <- test[i,1]
		obs[2] <- vals[index]
		#print(test[i,])
		#print(obs)
		if( p == 1 && i == 1){
			text <- paste(obs,collapse=",")
		}else{
			text <- c(text,paste(obs,collapse=","))
		}
	}
}
write(text,fileConn)
close(fileConn)
}


#
#				----------- RUN ------------------------
#


#"GPT","GOT","ZTT","TTT","D-BIL","I-BIL","ALB","T-CHO","T-BIL","TP","Type","CHE","Activity"
#"GPT","GOT","ZTT"
exams <- c("GPT","GOT","ZTT","TTT","D-BIL","I-BIL","ALB","T-CHO","T-BIL","TP","Type","CHE","Activity")

cl <- makeCluster(3, type="SOCK")
registerDoSNOW(cl)

writeLines(c(""), "C:\\hepat_data030704\\data\\predictionsHMM\\__log.txt")

foreach(i=1:length(exams) , .combine=rbind) %dopar% {
	sink("C:\\hepat_data030704\\data\\predictionsHMM\\__log.txt", append=TRUE)
	print(exams)
	#predict(exams[i], 10,12)
	#predict2(exams[i], 10,12)
	predict(exams[i], 5, 3)
	#predict7(exams[i], 10,12)
	#predict7(exams[i], 25,12)
}

stopCluster(cl)
