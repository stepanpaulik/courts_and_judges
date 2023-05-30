### Set Flags ###

SAVE <- TRUE
MCMCAdapt <- 1000
MCMCBurn <- 1000
MCMCSample <- 100000
MCMCThin <- 10
ModelChoice <- "D"

### Load Libraries ###

library(rjags)
library(car)

### Load Data ###

VoteData <- read.csv("FreedomReligionVoteData.csv",header=TRUE)
CaseData <- read.csv("FreedomReligionCaseData.csv",header=TRUE)
CiteData <- read.csv("FreedomReligionCiteData.csv",header=TRUE)

### Clean Data ###

VoteDataColNames <- substr(colnames(VoteData),2,4)[2:length(colnames(VoteData))]
VoteDataRowNames <- VoteData[,1]
VoteData <- VoteData[,2:length(colnames(VoteData))]
rownames(VoteData) <- VoteDataRowNames
colnames(VoteData) <- VoteDataColNames

JusticeNames <- rownames(VoteData)
JusticeVoteMatrix <- as.matrix(VoteData)
JusticeCount <- dim(JusticeVoteMatrix)[1]
CaseCount <- dim(JusticeVoteMatrix)[2]
VoteCount <- sum(is.na(JusticeVoteMatrix) == 0)

Y <- rep(NA, VoteCount)
JusticeID <- rep(NA, VoteCount)
DecisionID <- rep(NA, VoteCount)
votecount <- 0
for (i in 1:JusticeCount){
	for (j in 1:CaseCount){
		if (is.na(JusticeVoteMatrix[i,j]) == 0){
			votecount <- votecount + 1
			Y[votecount] <- JusticeVoteMatrix[i,j]
			JusticeID[votecount] <- i
			DecisionID[votecount] <- j
		}
	}
}

FlatData <- CiteData
rownames(FlatData) <- NULL
colnames(FlatData) <- c("NewCaseID","PrecendentID","Y")
FlatData[,3] <- replace(FlatData[,3],FlatData[,3] == 4,0)
FlatData[,3] <- replace(FlatData[,3],FlatData[,3] == 3,1)
FlatData[,3] <- replace(FlatData[,3],FlatData[,3] == 2,1)
FlatData[,3] <- replace(FlatData[,3],FlatData[,3] == 5,0)
FlatData[,3] <- replace(FlatData[,3],FlatData[,3] == 7,0)

UniqueCaseIDs <- sort(unique(c(FlatData[,1],FlatData[,2])))
CaseIDPairs <- paste(FlatData[,1],FlatData[,2])

IntExtIDMap <- UniqueCaseIDs
ExtIntIDMap <- rep(NA,max(UniqueCaseIDs))
for (i in 1:length(UniqueCas eIDs)){
	ExtIntIDMap[UniqueCaseIDs[i]] <- i
	}

NewCaseID <- ExtIntIDMap[FlatData[,1]]
PrecedentID <- ExtIntIDMap[FlatData[,2]]
Z <- FlatData[,3]
OpinionCount <- length(UniqueCaseIDs)
NewOpinionCount <- length(CaseData$id)
CitationCount <- length(Z)

AuthorMQScore <- rep(NA, OpinionCount)
SpaethCode <- rep(NA, OpinionCount)
CaseNumber <- rep(NA, OpinionCount)
Opinion <- rep(NA, OpinionCount)
Solo <- rep(NA, OpinionCount)
AuthorID <- rep(NA, OpinionCount)
Year <- rep(NA, OpinionCount)
AuthorName <- factor(rep(NA, OpinionCount),levels=levels(CaseData[,5]))
CaseVote <- matrix(rep(NA,JusticeCount*OpinionCount),ncol=JusticeCount)
WhichVote <- matrix(rep(NA,9*OpinionCount),ncol=9)
JusticesInCourt <- rep(NA,OpinionCount)
WhichMajority <- matrix(rep(NA,9*OpinionCount),ncol=9)
CoalitionID <- rowSums(2^WhichMajority,na.rm=TRUE)
JusticesInMajority <- rep(NA,OpinionCount)
for (i in 1: NewOpinionCount){
	TempInternalID <- ExtIntIDMap[CaseData$id[i]]
	AuthorMQScore[TempInternalID] <- CaseData[i,7]
	SpaethCode[TempInternalID] <- CaseData[i,6]
	AuthorName[TempInternalID] <- CaseData[i,5]
	CaseNumber[TempInternalID] <- CaseData[i,2]
	Year[TempInternalID] <- CaseData[i,8]
	Opinion[TempInternalID] <- CaseData[i,3]
	Solo[TempInternalID] <- CaseData$solo_dissent[i]
	AuthorID[TempInternalID] <- match(as.character(CaseData[i,5]),JusticeNames)
	tempid <- match(CaseNumber[TempInternalID],colnames(VoteData))
	CaseVote[TempInternalID,] <- VoteData[,tempid]
	JusticesInCourt[TempInternalID] <- length(which(is.na(CaseVote[TempInternalID,]) == 0))
	WhichVote[TempInternalID,1:JusticesInCourt[TempInternalID]] <- which(is.na(CaseVote[TempInternalID,]) == 0)
	JusticesInMajority[TempInternalID] <- length(which(is.na(CaseVote[TempInternalID,]) == 0 & CaseVote[TempInternalID,] == 1))
	WhichMajority[TempInternalID,1:JusticesInMajority[TempInternalID]] <- which(is.na(CaseVote[TempInternalID,]) == 0  & CaseVote[TempInternalID,] == 1)
	}
	
JusticesInCourtStartIndex <- c(1,1+cumsum(JusticesInCourt))[1: NewOpinionCount]
JusticesInCourtEndIndex <- c(cumsum(JusticesInCourt))[1: NewOpinionCount]
JusticesInCourtID <- as.vector(t(WhichVote))[is.na(as.vector(t(WhichVote))) == 0]


	
MajorityOpinionID <- which(Opinion == 1 & is.na(AuthorID) == 0)
ConcurringOpinionID <- which((Opinion == 2 | Opinion == 2.3) & is.na(AuthorID) == 0)
DissentingOpinionID <- which(Opinion == 3 & is.na(AuthorID) == 0)
PrecedentOpinionID <- which(is.na(Opinion) | is.na(AuthorID) == 1)
OpinionIndex <- c(MajorityOpinionID, ConcurringOpinionID, DissentingOpinionID, PrecedentOpinionID)
OpinionTypeCuts <- c(1,length(MajorityOpinionID),length(MajorityOpinionID)+1,length(MajorityOpinionID)+length(ConcurringOpinionID),length(MajorityOpinionID)+length(ConcurringOpinionID)+1,length(MajorityOpinionID)+length(ConcurringOpinionID)+length(DissentingOpinionID),length(MajorityOpinionID)+length(ConcurringOpinionID)+length(DissentingOpinionID)+1,OpinionCount)

#############################

if (SAVE){
rm("model.jags")
model.jags <- jags.model(file="ModelF.txt",n.adapt = MCMCAdapt, data = globalenv())
update(model.jags, MCMCBurn)
model.out <- jags.samples(model.jags,c("x.opinion","alpha","beta","lambda","kappa"),n.iter= MCMCSample,thin= MCMCThin)
save(model.out,file=paste("FRSavedMCMCSample",ModelChoice,".RData",sep=""))
} else {
load(file=paste("FRSavedMCMCSample",ModelChoice,".RData",sep=""))
}

### Determine Space Polarity ###

polarity.justice <- sign(rowMeans(model.out$x.justice)[27])
polarity.opinion <- polarity.justice

### Extract Ideal Points ###

x.justice.est <- polarity.justice*rowMeans(model.out$x.justice)
x.justice.se <- sd(t(model.out$x.justice[,,1]))
alpha.est <- rowMeans(model.out$alpha)
beta.est <- polarity.justice*rowMeans(model.out$beta)

x.justice.quantiles <- matrix(rep(NA,3*JusticeCount),ncol=3)
for (i in 1:JusticeCount){
x.justice.quantiles[i,] <- quantile(polarity.justice*t(model.out$x.justice[i,,1]),c(0.025,0.5,0.975))
}

IdealPointEstimateTable <- data.frame(JusticeNames,x.justice.est,x.justice.se,x.justice.quantiles)
colnames(IdealPointEstimateTable) <- c("JusticeName","x.justice.est","x.justice.se","x.justice.025","x.justice.5","x.justice.975")
print(IdealPointEstimateTable)

### Extract Opinion Locations ### 

x.opinion.chain <- model.out$x.opinion[1:OpinionCount,,1]
lambda.chain <- model.out$lambda[1,,1]
kappa.chain <- model.out$kappa[1,,1]

x.opinion.est <- rowMeans(x.opinion.chain)
x.opinion.se <- sd(t(x.opinion.chain))
x.opinion.weights <- 1/(x.opinion.se)^2
lambda.est <- mean(lambda.chain)
kappa.est <- mean(kappa.chain)
lambda.se <- sd(lambda.chain)
kappa.se <- sd(kappa.chain)

x.opinion.quantiles <- matrix(rep(NA,3*OpinionCount),ncol=3)
for (i in 1:OpinionCount){
x.opinion.quantiles[i,] <- quantile(t(model.out$x.opinion[i,,1]),c(0.025,0.5,0.975))
}
colnames(x.opinion.quantiles ) <- c("x.opinion.025","x.opinion.5","x.opinion.975")

CiteCount <- rep(NA,length(x.opinion.est))
CitedCount <- rep(NA,length(x.opinion.est))
for (i in 1:OpinionCount){
	CiteCount[i] <- sum(NewCaseID == i)
	CitedCount[i] <- sum(PrecedentID == i)
	}
	
AuthorIRTScore <- rep(NA, OpinionCount)
AuthorIRTerr <- rep(NA, OpinionCount)
for (i in 1:OpinionCount){
	justiceloc <- match(AuthorName[i],IdealPointEstimateTable$Justice)
	AuthorIRTScore[i] <- IdealPointEstimateTable[justiceloc,2]
	AuthorIRTerr[i] <- IdealPointEstimateTable[justiceloc,3]
	}
	
### Determine Theoretically Relevant Quantities ###

sims <- length(lambda.chain)
MedianJusticeIdealPoint.chain <- matrix(rep(NA, OpinionCount*sims),ncol=OpinionCount)
MedianMajorityIdealPoint.chain <- matrix(rep(NA, OpinionCount*sims),ncol=OpinionCount)
MeanMajorityIdealPoint.chain <- matrix(rep(NA, OpinionCount*sims),ncol=OpinionCount)
AuthorIRTScore.chain <- matrix(rep(NA, OpinionCount*sims),ncol=OpinionCount)

for (i in 1:NewOpinionCount){
	JinC <- WhichVote[i,]
	JinM <- WhichVote[i,CaseVote[i,WhichVote[i,]] == 1]
	for (sim in 1:sims){
		AuthorIRTScore.chain[sim,i] <- polarity.justice*model.out$x.justice[AuthorID[i],sim,1]
		MedianJusticeIdealPoint.chain[sim,i] <- median(polarity.justice*model.out$x.justice[JinC,sim,1],na.rm=TRUE)
		MedianMajorityIdealPoint.chain[sim,i] <- median(polarity.justice*model.out$x.justice[JinM,sim,1],na.rm=TRUE)
		MeanMajorityIdealPoint.chain[sim,i] <- mean(polarity.justice*model.out$x.justice[JinM,sim,1],na.rm=TRUE)
		}
}

MedianJusticeIdealPoint <- colMeans(MedianJusticeIdealPoint.chain)
MedianMajorityIdealPoint <- colMeans(MedianMajorityIdealPoint.chain)
MeanMajorityIdealPoint <- colMeans(MeanMajorityIdealPoint.chain)
MedianJusticeIdealPoint.se <- sd(MedianJusticeIdealPoint.chain,na.rm=TRUE)
MedianMajorityIdealPoint.se <- sd(MedianMajorityIdealPoint.chain,na.rm=TRUE)
MeanMajorityIdealPoint.se <- sd(MeanMajorityIdealPoint.chain,na.rm=TRUE)


AuthorCheck <- t(x.opinion.chain) > AuthorIRTScore.chain
MedianCheck <- t(x.opinion.chain) > MedianJusticeIdealPoint.chain
MedianMajorityCheck <- t(x.opinion.chain) > MedianMajorityIdealPoint.chain

SigAuthor <- (abs(colMeans(AuthorCheck) - 0.5) > 0.475)
SigMedian <- (abs(colMeans(MedianCheck) - 0.5) > 0.475)
SigMedianMajority <- (abs(colMeans(MedianMajorityCheck) - 0.5) > 0.475)

print(mean(SigAuthor[Opinion == 1],na.rm=TRUE))
print(table(SigAuthor[Opinion == 1]))
print(mean(SigMedian[Opinion == 1],na.rm=TRUE))
print(table(SigMedian[Opinion == 1]))
print(mean(SigMedianMajority[Opinion == 1],na.rm=TRUE))
print(table(SigMedianMajority[Opinion == 1]))

### Write Out Opinion Locations ###

if (SAVE){
OpinionTable <- data.frame(Year,IntExtIDMap,CaseNumber,x.opinion.est,x.opinion.se,x.opinion.quantiles,AuthorMQScore, AuthorIRTScore, AuthorIRTerr,MedianJusticeIdealPoint,MedianJusticeIdealPoint.se,MedianMajorityIdealPoint, MedianMajorityIdealPoint.se,SpaethCode,AuthorName,Opinion,CiteCount,CitedCount,Year)
write.csv(OpinionTable,"FRCommonSpaceOpinionScores.csv")

JusticeTable <- IdealPointEstimateTable
write.csv(JusticeTable,"FRCommonSpaceJusticeScores.csv")

EIVTable <- data.frame(x.opinion.est,x.opinion.se,AuthorIRTScore, AuthorIRTerr,MedianJusticeIdealPoint,MedianJusticeIdealPoint.se,MedianMajorityIdealPoint, MedianMajorityIdealPoint.se)[Opinion == 1,]
EIVTable <- EIVTable[rowSums(is.na(EIVTable)) < 2,]
write.csv(EIVTable,"FREIVData.csv")
}



### GLS Regression Analysis ###

summary(lm(x.opinion.est[Opinion == 1]~ AuthorIRTScore[Opinion == 1],weights=x.opinion.weights[Opinion == 1]))
summary(lm(x.opinion.est[Opinion == 1]~ MedianJusticeIdealPoint[Opinion == 1],weights=x.opinion.weights[Opinion == 1]))
summary(lm(x.opinion.est[Opinion == 1]~ MedianMajorityIdealPoint[Opinion == 1],weights=x.opinion.weights[Opinion == 1]))
summary(lm(x.opinion.est[Opinion == 1]~ MedianMajorityIdealPoint[Opinion == 1] + MedianJusticeIdealPoint[Opinion == 1] +  AuthorIRTScore[Opinion == 1],weights=x.opinion.weights[Opinion == 1]))







### EIV Analysis ###




# No Variables #

Yest <- as.numeric(EIVTable[,1])
Yerr <- as.numeric(EIVTable[,2])
N <- length(Yest)

eiv.jags <- jags.model(file="lmeiv0.jags",n.adapt = 1000)
update(eiv.jags, 5000)
eiv.out <- jags.samples(eiv.jags,c("beta","sigma"),n.iter= 5000,thin= 1)

beta.chain <- eiv.out$beta[,,1]
beta.est <- mean(beta.chain)
beta.se <- sd(beta.chain)
sigma.chain <- eiv.out$sigma[,,1]
sigma.est <- mean(sigma.chain)
sigma.se <- sd(sigma.chain)
print(data.frame(beta.est,beta.se))
print(data.frame(sigma.est,sigma.se))




# Author #

Yest <- as.numeric(EIVTable[,1])
Yerr <- as.numeric(EIVTable[,2])
Xest <- as.numeric(EIVTable[,c(3)])
Xerr <- as.numeric(EIVTable[,c(4)])
N <- length(Xest)

eiv.jags <- jags.model(file="lmeiv1.jags",n.adapt = 1000)
update(eiv.jags, 5000)
eiv.out <- jags.samples(eiv.jags,c("beta","sigma"),n.iter= 5000,thin= 1)

beta.chain <- t(eiv.out$beta[,,1])
beta.est <- colMeans(beta.chain)
beta.se <- sd(beta.chain)
sigma.chain <- eiv.out$sigma[,,1]
sigma.est <- mean(sigma.chain)
sigma.se <- sd(sigma.chain)
print(data.frame(beta.est,beta.se))
print(data.frame(sigma.est,sigma.se))

# Median #

Yest <- as.numeric(EIVTable[,1])
Yerr <- as.numeric(EIVTable[,2])
Xest <- as.numeric(EIVTable[,c(5)])
Xerr <- as.numeric(EIVTable[,c(6)])
N <- length(Xest)

eiv.jags <- jags.model(file="lmeiv1.jags",n.adapt = 1000)
update(eiv.jags, 5000)
eiv.out <- jags.samples(eiv.jags,c("beta","sigma"),n.iter= 5000,thin= 1)

beta.chain <- t(eiv.out$beta[,,1])
beta.est <- colMeans(beta.chain)
beta.se <- sd(beta.chain)
sigma.chain <- eiv.out$sigma[,,1]
sigma.est <- mean(sigma.chain)
sigma.se <- sd(sigma.chain)
print(data.frame(beta.est,beta.se))
print(data.frame(sigma.est,sigma.se))


# Coalition Median #

Yest <- as.numeric(EIVTable[,1])
Yerr <- as.numeric(EIVTable[,2])
Xest <- as.numeric(EIVTable[,c(7)])
Xerr <- as.numeric(EIVTable[,c(8)])
N <- length(Xest)

eiv.jags <- jags.model(file="lmeiv1.jags",n.adapt = 1000)
update(eiv.jags, 5000)
eiv.out <- jags.samples(eiv.jags,c("beta","sigma"),n.iter= 5000,thin= 1)

beta.chain <- t(eiv.out$beta[,,1])
beta.est <- colMeans(beta.chain)
beta.se <- sd(beta.chain)
sigma.chain <- eiv.out$sigma[,,1]
sigma.est <- mean(sigma.chain)
sigma.se <- sd(sigma.chain)
print(data.frame(beta.est,beta.se))
print(data.frame(sigma.est,sigma.se))






# Author + Median #

Yest <- as.numeric(EIVTable[,1])
Yerr <- as.numeric(EIVTable[,2])
Xest <- as.matrix(EIVTable[,c(3,5)])
Xerr <- as.matrix(EIVTable[,c(4,6)])
N <- dim(Xest)[1]

eiv.jags <- jags.model(file="lmeiv2.jags",n.adapt = 1000)
update(eiv.jags, 5000)
eiv.out <- jags.samples(eiv.jags,c("beta","sigma"),n.iter= 5000,thin= 1)

beta.chain <- t(eiv.out$beta[,,1])
beta.est <- colMeans(beta.chain)
beta.se <- sd(beta.chain)
sigma.chain <- eiv.out$sigma[,,1]
sigma.est <- mean(sigma.chain)
sigma.se <- sd(sigma.chain)
print(data.frame(beta.est,beta.se))
print(data.frame(sigma.est,sigma.se))

# Median + Coalition Median #

Yest <- as.numeric(EIVTable[,1])
Yerr <- as.numeric(EIVTable[,2])
Xest <- as.matrix(EIVTable[,c(5,7)])
Xerr <- as.matrix(EIVTable[,c(6,8)])
N <- dim(Xest)[1]

eiv.jags <- jags.model(file="lmeiv2.jags",n.adapt = 1000)
update(eiv.jags, 5000)
eiv.out <- jags.samples(eiv.jags,c("beta","sigma"),n.iter= 5000,thin= 1)

beta.chain <- t(eiv.out$beta[,,1])
beta.est <- colMeans(beta.chain)
beta.se <- sd(beta.chain)
sigma.chain <- eiv.out$sigma[,,1]
sigma.est <- mean(sigma.chain)
sigma.se <- sd(sigma.chain)
print(data.frame(beta.est,beta.se))
print(data.frame(sigma.est,sigma.se))


# Author + Coalition Median #

Yest <- as.numeric(EIVTable[,1])
Yerr <- as.numeric(EIVTable[,2])
Xest <- as.matrix(EIVTable[,c(3,7)])
Xerr <- as.matrix(EIVTable[,c(4,8)])
N <- dim(Xest)[1]

eiv.jags <- jags.model(file="lmeiv2.jags",n.adapt = 1000)
update(eiv.jags, 5000)
eiv.out <- jags.samples(eiv.jags,c("beta","sigma"),n.iter= 5000,thin= 1)

beta.chain <- t(eiv.out$beta[,,1])
beta.est <- colMeans(beta.chain)
beta.se <- sd(beta.chain)
sigma.chain <- eiv.out$sigma[,,1]
sigma.est <- mean(sigma.chain)
sigma.se <- sd(sigma.chain)
print(data.frame(beta.est,beta.se))
print(data.frame(sigma.est,sigma.se))







# All Variables #

Yest <- as.numeric(EIVTable[,1])
Yerr <- as.numeric(EIVTable[,2])
Xest <- as.matrix(EIVTable[,c(3,5,7)])
Xerr <- as.matrix(EIVTable[,c(4,6,8)])
N <- dim(Xest)[1]

eiv.jags <- jags.model(file="lmeiv3.jags",n.adapt = 1000)
update(eiv.jags, 5000)
eiv.out <- jags.samples(eiv.jags,c("beta","sigma"),n.iter= 5000,thin= 1)

beta.chain <- t(eiv.out$beta[,,1])
beta.est <- colMeans(beta.chain)
beta.se <- sd(beta.chain)
sigma.chain <- eiv.out$sigma[,,1]
sigma.est <- mean(sigma.chain)
sigma.se <- sd(sigma.chain)
print(data.frame(beta.est,beta.se))
print(data.frame(sigma.est,sigma.se))







### Plot Figure 1 ###

pdf("FRFigure1.pdf",10,5)
par(mfrow=c(1,2))
      
        plot(density(x.opinion.est[SpaethCode==1 & (Opinion == 1)],na.rm=T), # liberal decisions
            col="grey",lwd=2,lty=1,xlim=c(-3,3),ylim=c(0,1.25),
            main="Distribution of Majority Opinion by Judgment",xlab="Opinion Location",bty="n")
        #points(density(x.opinion.est[SpaethCode==1 & (Opinion == 2)],na.rm=T),  # liberal concurrence
            #col="blue",lwd=1,type="l",lty=3)

        points(density(x.opinion.est[SpaethCode==0 & Opinion == 1],na.rm=T),  # conservative majority
            col="black",lwd=2,type="l",lty=1)
         #points(density(x.opinion.est[SpaethCode==0 & (Opinion == 2)],na.rm=T),  # conservative concurrence
            #col="red",lwd=1,type="l",lty=3)
            
        points(density(x.opinion.est[Opinion == 1],na.rm=T),  # all decisions
            col="dark grey",lwd=3,type="l",lty=1)
        
        #abline(v=mean(x.opinion.est[SpaethCode==1],na.rm=T),lty=5,lwd=1,col="grey")
        #abline(v=mean(x.opinion.est[SpaethCode==0],na.rm=T),lty=5,lwd=1,col="black")

        legend('topleft',col=c("dark grey","grey","black"),lty=c(1,1,1),lwd=c(3,2,2),
      c("All Decisions","Liberal (Spaeth)","Conservative (Spaeth)"),bg="white")


opinions <- subset(OpinionTable,AuthorIRTScore!='NA')
majority <- subset(opinions,Opinion==1)

sorted.opinions<-opinions[sort(opinions$x.opinion.est,index.return=TRUE)$ix,]
sorted.majority<-majority[sort(majority$x.opinion.est,index.return=TRUE)$ix,]

    
    plot(c(-3,3),c(0,nrow(sorted.majority)),type="n",ylab="",xlab="Opinion Location",bty="n",yaxt="n",main="Estimated Majority Opinion Locations")
    color<-c("black","grey")
    col.id<-sorted.majority$SpaethCode+1
    points(sorted.majority$x.opinion.est,1:nrow(sorted.majority),cex=0.5,pch=21,col=color[col.id],bg=color[col.id])
    for(i in 1:nrow(sorted.majority)){
            lines(c(sorted.majority$x.opinion.025[i],sorted.majority$x.opinion.975[i]),c(i,i),lwd=1,lty=1,col=color[col.id[i]])
    }    

    selected <- c("Lemon v. Kurtzman",
                        "Lee v. Weisman",
                        "Agostini v. Felton",
                        "Aguilar v. Felton")
    selected.codes<-c(2005,3037,3064,2079)

    for(i in 1:length(selected)){
        the.op <- which(sorted.majority$IntExtIDMap==selected.codes[i])
		if (sign(sorted.majority$x.opinion.est[the.op]) == 1){
        arrows(sorted.majority$x.opinion.est[the.op]-sign(sorted.majority$x.opinion.est[the.op])*1.5,the.op,sorted.majority$x.opinion.025[the.op] - 0.1,the.op,length=0.15)
        text(sorted.majority$x.opinion.est[the.op]-sign(sorted.majority$x.opinion.est[the.op])*1.5,the.op,labels=selected[i],
            pos=ifelse(sign(sorted.majority$x.opinion.est[the.op])==1,2,4),cex=0.75)
        } else {
        arrows(sorted.majority$x.opinion.est[the.op]-sign(sorted.majority$x.opinion.est[the.op])*1.5,the.op,sorted.majority$x.opinion.975[the.op] + 0.1,the.op,length=0.15)
        text(sorted.majority$x.opinion.est[the.op]-sign(sorted.majority$x.opinion.est[the.op])*1.5,the.op,labels=selected[i],
            pos=ifelse(sign(sorted.majority$x.opinion.est[the.op])==1,2,4),cex=0.75)        	
        	}
    }
    
dev.off()





### Plot Figure 2 ###

pdf("FRFigure2.pdf",10,5)
par(mfrow=c(1,1))

plot(x.opinion.est[Opinion == 1]~Year[Opinion == 1],pch=18,xlab="Year",ylab="Majority Opinion Location",main="Freedom of Religion Doctrine",ylim=c(-2.5,2.5),type="n",xlim=c(1954,2008))
points(x.opinion.est[Opinion == 1 & SpaethCode == 0]~Year[Opinion == 1 & SpaethCode == 0],pch="C",col="black")
points(x.opinion.est[Opinion == 1 & SpaethCode == 1]~Year[Opinion == 1 & SpaethCode == 1],pch="L",col="grey")
plotframe <- data.frame(Year[Opinion == 1 & SpaethCode == 0],x.opinion.est[Opinion == 1 & SpaethCode == 0])
plotframe <- plotframe[is.na(plotframe[,1]) == FALSE,]
lines(lowess(plotframe),col="black",lwd=2)
plotframe <- data.frame(Year[Opinion == 1 & SpaethCode == 1],x.opinion.est[Opinion == 1 & SpaethCode == 1])
plotframe <- plotframe[is.na(plotframe[,1]) == FALSE,]
lines(lowess(plotframe),col="grey",lwd=2)
plotframe <- data.frame(Year[Opinion == 1],x.opinion.est[Opinion == 1])
plotframe <- plotframe[is.na(plotframe[,1]) == FALSE,]
lines(lowess(plotframe),col="dark grey")
lines(MedianJusticeIdealPoint[Opinion == 1]~Year[Opinion == 1],lty=2,col="dark grey")
legend("topleft",c("Conservative Judgments","Liberal Judgments","All Judgments","Median Justice"), lty=c(1,1,1,2),pch=c("C","L","",""),lwd=c(2,2,1,1),col=c("black","grey","dark grey","dark grey"),ncol=2)


dev.off()











### Plot Figure 3 ###


PlotEllipses <- sample(which(Opinion == 1),8)

library(car)
pdf(file="FRFigure3.pdf",width=12,height=4)

par(mfrow=c(1,3),cex.main=1)


plot(x.opinion.est~ MedianJusticeIdealPoint,type="n",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),xlab="Court Median Location",ylab="Majority Opinion Location",main="Median Justice Model")
overlaps <- rep(NA,OpinionCount)
for (i in 1:NewOpinionCount){
	if (is.na(Opinion[i]) == FALSE){
	if (Opinion[i] == 1 & is.na(mean(MedianJusticeIdealPoint.chain[,i])) == FALSE){
		shape <- var(cbind(MedianJusticeIdealPoint.chain[,i], x.opinion.chain[i,]))
		center <- c(mean(MedianJusticeIdealPoint.chain[,i]), mean(x.opinion.chain[i,]))
		radius <- sqrt(2 * qf(0.95, 2, length(MedianJusticeIdealPoint.chain[,i])-1))
		angles <- (0:51) * 2 * pi/51
    		unit.circle <- cbind(cos(angles), sin(angles))
   		ellpoints <- t(center + radius * t(unit.circle %*% chol(shape)))
   		overlaps[i] <- SigMedian[i] == 0
   		
  		if (is.element(i,PlotEllipses)){
			data.ellipse(MedianJusticeIdealPoint.chain[,i],x.opinion.chain[i,], levels=c(0.95),plot.points=FALSE,col="light grey",lwd=0.5,center.cex=1,center.pch=0)
			}
		}
	}
	}
	
points(x.opinion.est[Opinion == 1 & overlaps == 1]~ MedianJusticeIdealPoint[Opinion == 1& overlaps == 1],pch=1,cex=1)
points(x.opinion.est[Opinion == 1 & overlaps == 0]~ MedianJusticeIdealPoint[Opinion == 1& overlaps == 0],pch=1,cex=1)
abline(0,1)
R2 <- 1 - var(x.opinion.est[Opinion == 1]-MedianJusticeIdealPoint[Opinion == 1],na.rm=TRUE)/var(x.opinion.est[Opinion == 1],na.rm=TRUE)
#text(1.5,-2,expression(R^2 == 0.13))
#text(1.5,-2,paste(expression(R^2),"=",round(R2,2)))
#text(1.5,-2.3,paste("Coverage =", round(mean(overlaps,na.rm=TRUE),2)))
print(table(overlaps))

plot(x.opinion.est~AuthorIRTScore,type="n",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),xlab="Opinion Author Location",ylab="Majority Opinion Location",main="Author Monopoly Model")
overlaps <- rep(NA,OpinionCount)
for (i in 1:NewOpinionCount){
	if (is.na(Opinion[i]) == FALSE){
	if (Opinion[i] == 1 & is.na(mean(AuthorIRTScore.chain[,i])) == FALSE){
		shape <- var(cbind(AuthorIRTScore.chain[,i], x.opinion.chain[i,]))
		center <- c(mean(AuthorIRTScore.chain[,i]), mean(x.opinion.chain[i,]))
		radius <- sqrt(2 * qf(0.95, 2, length(AuthorIRTScore.chain[,i])-1))
		angles <- (0:51) * 2 * pi/51
    		unit.circle <- cbind(cos(angles), sin(angles))
   		ellpoints <- t(center + radius * t(unit.circle %*% chol(shape)))
   		overlaps[i] <- SigAuthor[i] == 0
   				if (is.element(i,PlotEllipses)){
			data.ellipse(AuthorIRTScore.chain[,i],x.opinion.chain[i,], levels=c(0.95),plot.points=FALSE,col="light grey",lwd=0.5,center.cex=1,center.pch=0)
			}
		}
	}	
	}
points(x.opinion.est[Opinion == 1 & overlaps == 1]~ AuthorIRTScore[Opinion == 1& overlaps == 1],pch=1,cex=1)
points(x.opinion.est[Opinion == 1 & overlaps == 0]~ AuthorIRTScore[Opinion == 1& overlaps == 0],pch=1,cex=1)

abline(0,1)
R2 <- 1 - var(x.opinion.est[Opinion == 1]-AuthorIRTScore[Opinion == 1],na.rm=TRUE)/var(x.opinion.est[Opinion == 1],na.rm=TRUE)
#text(1.5,-2,expression(R^2 == "-0.28"))
#text(1.5,-2,paste(expression(R^2),"=",round(R2,2)))
#text(1.5,-2.3,paste("Coverage =", round(mean(overlaps,na.rm=TRUE),2)))
print(table(overlaps))



plot(x.opinion.est~MedianMajorityIdealPoint,type="n",xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),xlab="Coalition Median Location",ylab="Majority Opinion Location",main="Majority Median Model")
overlaps <- rep(NA,OpinionCount)
for (i in 1:NewOpinionCount){
	if (is.na(Opinion[i]) == FALSE){
	if (Opinion[i] == 1 & is.na(mean(MedianMajorityIdealPoint.chain[,i])) == FALSE){
		shape <- var(cbind(MedianMajorityIdealPoint.chain[,i], x.opinion.chain[i,]))
		center <- c(mean(MedianMajorityIdealPoint.chain[,i]), mean(x.opinion.chain[i,]))
		radius <- sqrt(2 * qf(0.95, 2, length(MedianMajorityIdealPoint.chain[,i])-1))
		angles <- (0:51) * 2 * pi/51
    		unit.circle <- cbind(cos(angles), sin(angles))
   		ellpoints <- t(center + radius * t(unit.circle %*% chol(shape)))
   		overlaps[i] <- SigMedianMajority[i] == 0
		if (is.element(i,PlotEllipses)){
			data.ellipse(MedianMajorityIdealPoint.chain[,i],x.opinion.chain[i,], levels=c(0.95),plot.points=FALSE,col="light grey",lwd=0.5,center.cex=1,center.pch=0)
			}
		}
	}
	}
	
points(x.opinion.est[Opinion == 1 & overlaps == 1]~ MedianMajorityIdealPoint[Opinion == 1& overlaps == 1],pch=1,cex=1)
points(x.opinion.est[Opinion == 1 & overlaps == 0]~ MedianMajorityIdealPoint[Opinion == 1& overlaps == 0],pch=1,cex=1)
abline(0,1)
R2 <- 1 - var(x.opinion.est[Opinion == 1]-MedianMajorityIdealPoint[Opinion == 1],na.rm=TRUE)/var(x.opinion.est[Opinion == 1],na.rm=TRUE)
#text(1.5,-2,expression(R^2 == "0.60"))
#text(1.5,-2,paste(expression(R^2),"=",round(R2,2)))
#text(1.5,-2.3,paste("Coverage =", round(mean(overlaps,na.rm=TRUE),2)))
print(table(overlaps))
dev.off()






### Run Posterior Predictive Check ###

PredictiveData <- FlatData[is.na(FlatData[,3]) == FALSE,]
PredictiveData[,3] <- replace(PredictiveData[,3] , PredictiveData[,3] != 0 & PredictiveData[,3] != 1,NA)
TotalCites <- dim(PredictiveData)[1]
rownames(PredictiveData) <- NULL
print(mean(PredictiveData[,3]))
NewCasePosition <- rep(NA, TotalCites)
PrecedentPosition <- rep(NA, TotalCites)

#SortedOpinions <- sort(x.opinion.est,index.return=TRUE)$ix
for (cite in 1:TotalCites){
	NewCasePosition[cite] <- x.opinion.est[ExtIntIDMap[PredictiveData[cite,1]]]
	PrecedentPosition[cite] <- x.opinion.est[ExtIntIDMap[PredictiveData[cite,2]]]	
	}
trueY <- PredictiveData[,3]
pY.est <- pnorm(kappa.est + lambda.est*(NewCasePosition - PrecedentPosition)^2)
simY <- rep(NA,length(trueY))
for (cite in 1: TotalCites){
	simY[cite] <- rbinom(1,1,pY.est[cite])
	}
print(mean(simY == trueY,na.rm=TRUE))

ppsims <- 10
pY.sim <- rep(0,length(trueY))
for (sim in 1: ppsims){
for (cite in 1:TotalCites){
	NewCasePosition[cite] <- x.opinion.chain[ExtIntIDMap[PredictiveData[cite,1]],sim]
	PrecedentPosition[cite] <- x.opinion.chain[ExtIntIDMap[PredictiveData[cite,2]],sim	]
	}
pY.sim <- pY.sim + pnorm(kappa.chain[sim] + lambda.chain[sim]*(NewCasePosition - PrecedentPosition)^2)/ppsims
}
simY <- rep(NA,length(trueY))
for (cite in 1: TotalCites){
	simY[cite] <- rbinom(1,1,pY.sim[cite])
	}
print(mean(simY == trueY,na.rm=TRUE))
PositiveCites <- mean(trueY,na.rm=TRUE)
print(PositiveCites* PositiveCites + (1-PositiveCites)*(1-PositiveCites))







####### Generate LaTeX Tables ########



OpinionLaTeXTable <- OpinionTable[1:NewOpinionCount,c(1,3,6,7,8,17,18,19,20)]
OpinionLaTeXTable[,3] <- sprintf("%.2f",OpinionLaTeXTable[,3])
OpinionLaTeXTable[,4] <- sprintf("%.2f",OpinionLaTeXTable[,4])
OpinionLaTeXTable[,5] <- sprintf("%.2f",OpinionLaTeXTable[,5])
OpinionLaTeXTable[,7] <- replace(OpinionLaTeXTable[,7],OpinionLaTeXTable[,7] == 1,"M")
OpinionLaTeXTable[,7] <- replace(OpinionLaTeXTable[,7],OpinionLaTeXTable[,7] == 2,"C")
OpinionLaTeXTable[,7] <- replace(OpinionLaTeXTable[,7],OpinionLaTeXTable[,7] == 2.3,"C/D")
OpinionLaTeXTable[,7] <- replace(OpinionLaTeXTable[,7],OpinionLaTeXTable[,7] == 3,"D")
colnames(OpinionLaTeXTable) <- c("Year","Case","2.5% Quantile","Median Posterior","97.5% Quantile","Author","Type","Citations","Cited")
rownames(OpinionLaTeXTable) <- NULL

CaseCodes <- read.csv("FreedomReligionCaseNames.csv")

CaseNames <- CaseCodes$Name[1:length(OpinionLaTeXTable[,3])]
for (i in 1:length(OpinionLaTeXTable[,3])){
	CaseNames[i] <- CaseCodes$Name[which(OpinionLaTeXTable[i,2] == CaseCodes$Case)[1]]
	}
	
OpinionLaTeXTable[,2]	<- CaseNames

write.table(OpinionLaTeXTable,file="FROpinionLaTeXTable.tex",quote=FALSE,sep="&",eol="\\\\ \n",row.names=FALSE,col.names=TRUE)





OpinionCSVTable <- OpinionTable[1:NewOpinionCount,c(1,3,4,5,6,7,8,17,18,19,20)]
OpinionCSVTable[,3] <- sprintf("%.3f", OpinionCSVTable[,3])
OpinionCSVTable[,4] <- sprintf("%.3f", OpinionCSVTable[,4])
OpinionCSVTable[,5] <- sprintf("%.3f", OpinionCSVTable[,5])
OpinionCSVTable[,6] <- sprintf("%.3f", OpinionCSVTable[,6])
OpinionCSVTable[,7] <- sprintf("%.3f", OpinionCSVTable[,7])
OpinionCSVTable[,9] <- replace(OpinionCSVTable[,9], OpinionCSVTable[,9] == 1,"M")
OpinionCSVTable[,9] <- replace(OpinionCSVTable[,9], OpinionCSVTable[,9] == 2,"C")
OpinionCSVTable[,9] <- replace(OpinionCSVTable[,9], OpinionCSVTable[,9] == 2.3,"C/D")
OpinionCSVTable[,9] <- replace(OpinionCSVTable[,9], OpinionCSVTable[,9] == 3,"D")
colnames(OpinionCSVTable) <- c("Year","Case","PosteriorMean","PosteriorSD","Posterior025","Posterior500","Posterior975","Author","Type","Citations","Cited")
rownames(OpinionCSVTable) <- NULL

CaseCodes <- read.csv("FreedomReligionCaseNames.csv")

CaseNames <- CaseCodes$Name[1:length(OpinionCSVTable[,3])]
for (i in 1:length(OpinionCSVTable[,3])){
	CaseNames[i] <- CaseCodes$Name[which(OpinionCSVTable[i,2] == CaseCodes$Case)[1]]
	}
	
OpinionCSVTable[,2] <- CaseNames

write.table(OpinionCSVTable,file="FROpinionTable.csv",quote=TRUE,sep=",",row.names=FALSE,col.names=TRUE)






FirstTerm <- c(1937,1970,1956,1994,1969,1945,1962,1949,1939,1965,1939,1993,1962,1955,1941,1988,1967,1949,1981,1972,1938,1972,1986,1990,1975,1958,1991,1953,1957) #through harlan
LastTerm <- c(1971,1994,1990,NA,1986,1958,1993,1967,1975,1969,1962,NA,1965,1971,1954,NA,1991,1956,2006,1987,1957,2005,NA,NA,NA,1981,NA,1969,1962)
CasesForJustice <- rowSums(is.na(JusticeVoteMatrix) == FALSE)


JusticeLaTeXTable <- JusticeTable[,c(1,2,3,3,4,5,6)]
JusticeLaTeXTable[,2] <- FirstTerm
JusticeLaTeXTable[,3] <- LastTerm
JusticeLaTeXTable[,4] <- CasesForJustice
JusticeLaTeXTable[,5] <- round(JusticeLaTeXTable[,5],2)
JusticeLaTeXTable[,6] <- round(JusticeLaTeXTable[,6],2)
JusticeLaTeXTable[,7] <- round(JusticeLaTeXTable[,7],2)
colnames(JusticeLaTeXTable) <- c("Name","First Term","Last Term","Cases","2.5% Quantile","Median Posterior","97.5% Quantile")
JusticeLaTeXTable <- JusticeLaTeXTable[JusticeLaTeXTable[,4] != 0,]
JusticeLaTeXTable <- JusticeLaTeXTable[sort(JusticeLaTeXTable[,2], index.return=TRUE)$ix,]

##sort!

write.table(JusticeLaTeXTable,file="FRJusticeLaTeXTable.tex",quote=FALSE,sep="&",eol="\\\\ \n",row.names=FALSE,col.names=TRUE)

