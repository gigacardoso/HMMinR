
library(doSNOW)

predict <- function(states, exam, iter, steps){
library(HMM)
print(paste(exam, "loading data"))
d <- read.csv(file=paste(c("C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\multidata\\",exam,".csv"),collapse=""),head=TRUE,sep=",", stringsAsFactors=FALSE)
#d <- read.csv(file="C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\data\\ALB.csv",head=TRUE,sep=",", stringsAsFactors=FALSE)

#exams <- c("GPT","GOT","ZTT","TTT","D-BIL","I-BIL","ALB","T-CHO","T-BIL","TP","Type","CHE","Activity")

#sprint(summary(d))
prob <- function (x) {x / sum (x)}  # Makes it a probability (it sums to 1)
# "P", "PP" and "PPP" sao fillers para que o num de simbolos seja multiplo do num de estados
vals <- c(
"H_ALB","N_ALB","L_ALB","VL_ALB",
"B_Type","C_Type",
"H_CHE","N_CHE","L_CHE","VL_CHE","VH_CHE",
"H_T-CHO","N_T-CHO","L_T-CHO","VL_T-CHO","VH_T-CHO",
"H_TP","N_TP","L_TP","VL_TP","VH_TP",
"A1_Activity","A2_Activity","A3_Activity",
"N_GPT","H_GPT","VH_GPT","UH_GPT",
"N_GOT","H_GOT","VH_GOT","UH_GOT",
"N_ZTT","H_ZTT","VH_ZTT","UH_ZTT",
"N_TTT","H_TTT","VH_TTT","UH_TTT",
"N_D-BIL","H_D-BIL","VH_D-BIL","UH_D-BIL",
"N_I-BIL","H_I-BIL","VH_I-BIL","UH_I-BIL",
"N_T-BIL","H_T-BIL","VH_T-BIL","UH_T-BIL",
"$")



rows <- nrow(d)
fold <- floor(rows/10)

folds <- c(1,fold,fold*2,fold*3,fold*4,fold*5,fold*6,fold*7,fold*8,fold*9)
fileConn<-file(paste(c("C:\\hepat_data030704\\data\\predictionsHMM_Multi\\",exam,"_Predictions.csv"),collapse=""))
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
	stat <- c("a","b")
	for(i in 1:(states-2)){
		stat <- c( stat , paste("s",i))
	}
	print(paste(exam, "initialization"))
	hmm = initHMM(stat, vals, startProbs=(prob (runif (4))),
		transProbs=apply (matrix (runif(states*states), states), 1, prob),
		emissionProbs=apply (matrix (runif(states*length(vals)), states), 1, prob))	

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
		observations[m] <- "$"
		m = m + 1
	}
	print(paste(exam, "BaumWelch", "iter ->", iter))
	vt = baumWelch(hmm, observations, maxIterations=iter, delta=1E-9, pseudoCount=0)

	#predict
	values <- getPossibleValues(exam)
	
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
		for(j in 1:length(values)){
			observations[m] <- values[j]
			observations[(m+1)] <- "$"
			f <- forward(vt$hmm, observations)
			#print(observations)
			#print(f)
			probs[j] <- f[1,ncol(f)]
			for(k in 2:4){
				if (f[k,ncol(f)] > probs[j]){
					probs[j] <- f[k,ncol(f)]
				}
			}
		}
		max <- (-2000000)
		for(j in 1:length(values)){
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
		obs[2] <- values[index]
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


getPossibleValues <- function(exam){
	if(exam == "ALB"){
		vals <- c("H_ALB","N_ALB","L_ALB","VL_ALB")
	}		
	if(exam == "Type"){
	vals <- c("B_Type","C_Type")
	}
	if(exam == "CHE"){
	vals <- c("H_CHE","N_CHE","L_CHE","VL_CHE","VH_CHE")
	} 
	if(exam == "T-CHO") {
		vals <- c("H_T-CHO","N_T-CHO","L_T-CHO","VL_T-CHO","VH_T-CHO")
	}
	if(exam == "TP") {
		vals <- c("H_TP","N_TP","L_TP","VL_TP","VH_TP")
	}
	if(exam == "T-BIL") {
		vals <- c("N_T-BIL","H_T-BIL","VH_T-BIL","UH_T-BIL")
	}
	if(exam == "Activity"){
		vals <- c("A1_Activity","A2_Activity","A3_Activity")
	}
	if(exam == "GPT"){
		vals <- c("N_GPT","H_GPT","VH_GPT","UH_GPT")
	}
	if(exam == "GOT"){
		vals <- c("N_GOT","H_GOT","VH_GOT","UH_GOT")
	}
	if(exam == "ZTT"){
		vals <- c("N_ZTT","H_ZTT","VH_ZTT","UH_ZTT")
	}
	if(exam == "TTT"){
		vals <- c("N_TTT","H_TTT","VH_TTT","UH_TTT")
	}
	if(exam == "D-BIL"){
		vals <- c("N_D-BIL","H_D-BIL","VH_D-BIL","UH_D-BIL")
	}
	if(exam == "I-BIL"){
		vals <- c("N_I-BIL","H_I-BIL","VH_I-BIL","UH_I-BIL")
	}
	print(vals)
	return(vals)
}



#
#				----------- RUN ------------------------
#

#"GPT","GOT","ZTT","TTT","D-BIL","I-BIL","ALB","T-CHO","T-BIL","TP","Type","CHE","Activity"
#"GPT"
exams <- c("GPT","GOT","ZTT","TTT","D-BIL","I-BIL","ALB","T-CHO","T-BIL","TP","Type","CHE","Activity")

cl <- makeCluster(4, type="SOCK")
registerDoSNOW(cl)

foreach(i=1:length(exams) , .combine=rbind) %dopar% {
	print(exams)
	#predict(#states,exam, #iter, #steps)
	predict(4,exams[i], 1, 3)
}

stopCluster(cl)
