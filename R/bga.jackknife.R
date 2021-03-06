"bga.jackknife" <-
function(data, classvec, type="coa", ...){
	ntrain<-ncol(data)  # Microarray data, samples in columns
	if (!ntrain ==length(classvec)) stop("ncol in training data not equal to length classvec")
	classvec<-checkfac(classvec)
        
	jackset<-function(ntrain=10,i=1) {
	  # Jackknife, One Leave out analysis
	  # to run use:: for (i in c(1:ntrain)) print(jackset(ntrain,i))
	  # ntrain is the ncol in the data, i is the sample to be left out
	  
	  ind<-c(1:ntrain)
	  newtrain<-ind[-(i)]
	  newtest<-ind[i]
	  return(list(train=newtrain,test=newtest))
	}               
	
	out<-NULL
	for (i in c(1:ntrain)) {
	  ind<-jackset(ntrain,i)
	  traindata<-data[,ind$train]
		testdata<- data[,ind$test, drop=FALSE]
		trainvec<- classvec[ind$train]
		testvec <- classvec[ind$test]
		bga.res <- bga.suppl(traindata, testdata, trainvec, testvec, suponly=TRUE, type=type)
		out <- rbind.data.frame(out, bga.res)
	}

	# Do some v simple stats
	n.correct<-length(which(out[,"predicted"]== out[,"true.class"]))
	p.correct<-n.correct/ntrain*100
	n.incorrect<-length(which(out[,"predicted"]!= out[,"true.class"]))
	stats=c("No.correct"=n.correct, "No.incorrect"=n.incorrect, "%correct"=round(p.correct,2))
	# return 
	return(list("results"=out, "summary"=stats))
}
