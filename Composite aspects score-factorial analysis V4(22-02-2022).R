setwd("C:/Users/Dell/Desktop/Dr. BBS/Composite score aspects")

library(readxl)
Participation <- read_excel("Farmers_data_V6(22_02_2022).xlsx")


Cerealseeds<- cbind (Participation$Improveaccesstoqualityseedsofcereals,
                        Participation$Improveprotectionofindigenousseeds,
                        Participation$Availabilityofimprovedvarietiesofcerealsseeds)

Production<-cbind(Participation$Improveplantprotection,
                  Participation$Improvestorageofcereals,
                  Participation$Improveirrigationforfarming
                  )

Environment<-cbind(Participation$Reducelandslideproblemsinthefarm,
                   Participation$Reducepollutants,
                   Participation$Createhabitatsforwildlife,
                   Participation$Improvesoilfertilityforcereals)
View(Environment)

Social<-cbind(Participation$Improvenetworkingamongfarmers,
              Participation$`Increasefarmers'groupsinthecommunity`,
              Participation$Improvelocalgroupmanagement,
              Participation$Improverelationshipoffarmersamongthem)

Marketing<-cbind(Participation$Setpriceofgrains,
                 Participation$Negotiatepriceforcereals,
                 Participation$Findmarketforcereals)

# Cerealseeds

Cerealseed_score<-factanal(x=Cerealseeds, factors = 1, 
                        scores = "regression")
Cerealseed_score
Cerealseed_score_score<-as.vector(Cerealseed_score$scores)
View(Cerealseed_score_score)
summary(Cerealseed_score_score)

fal.loadings<-Cerealseed_score$loadings[,1]
fal.loadings

weighted.sd<-function(x,w){
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w<- sum(x*w)/sum(w)
  x.sd.w<-sqrt((sum.w/(sum.w^2-sum.w2))*sum(w*(x-mean.w)^2))
  return(x.sd.w)
}

re.scale<- function(f.scores,raw.data,loadings){
  fz.scores <- (f.scores + mean(f.scores))/(sd(f.scores))
  means <- apply(raw.data,1, weighted.mean, w=loadings)
  sds <- apply(raw.data, 1, weighted.sd, w=loadings)
  grand.mean<-mean(means)
  grand.sd<-mean(sds)
  final.scores<-((fz.scores*grand.sd)+grand.mean)
  return(final.scores)
}
final.scores.ssl<-re.scale(
  Cerealseed_score_score,Cerealseeds,fal.loadings)  

summary(final.scores.ssl)
View(final.scores.ssl)

get.scores.fun<-function(data){
  fact<- factanal(data,factors=1,scores='regression')
  f.scores<-fact$scores[,1]
  f.loads<-fact$loadings[,1]
  rescaled.scores<-re.scale(f.scores, data,f.loads)
  output.list<-list(rescaled.scores, f.loads)
  names(output.list)<- c("rescaled.scores", "factor.loadings")
  return(output.list)
}
get.scores.fun

scores.and.loadings.1<-get.scores.fun(Cerealseeds)
scores.and.loadings.1$factor.loadings
View(scores.and.loadings.1)

install.packages('writexl')
library(writexl)

write_xlsx(final.scores.ssl, 
           "C:/Users/Dell/Desktop/Dr. BBS\\participation.xlsx")
df<-data.frame(final.scores.ssl)
write_xlsx(df, 
           "C:/Users/Dell/Desktop/Dr. BBS/Composite score aspects\\cerealseedcompositescore.xlsx")

# Production

Production_score<-factanal(x=Production, factors = 1, 
                           scores = "regression")
Production_score
Production_score_score<-as.vector(Production_score$scores)

summary(Production_score_score)

fal.loadings<-Production_score$loadings[,1]
fal.loadings

weighted.sd<-function(x,w){
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w<- sum(x*w)/sum(w)
  x.sd.w<-sqrt((sum.w/(sum.w^2-sum.w2))*sum(w*(x-mean.w)^2))
  return(x.sd.w)
}

re.scale<- function(f.scores,raw.data,loadings){
  fz.scores <- (f.scores + mean(f.scores))/(sd(f.scores))
  means <- apply(raw.data,1, weighted.mean, w=loadings)
  sds <- apply(raw.data, 1, weighted.sd, w=loadings)
  grand.mean<-mean(means)
  grand.sd<-mean(sds)
  final.scores<-((fz.scores*grand.sd)+grand.mean)
  return(final.scores)
}
final.scores.ssl<-re.scale(
  Production_score_score,Production,fal.loadings)  

summary(final.scores.ssl)
View(final.scores.ssl)

# Environment
Environment_score<-factanal(x=Environment, factors = 1, 
                           scores = "regression")
Environment_score
Environment_score_score<-as.vector(Environment_score$scores)
View(Environment_score_score)


fal.loadings<-Environment_score$loadings[,1]
fal.loadings

weighted.sd<-function(x,w){
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w<- sum(x*w)/sum(w)
  x.sd.w<-sqrt((sum.w/(sum.w^2-sum.w2))*sum(w*(x-mean.w)^2))
  return(x.sd.w)
}

re.scale<- function(f.scores,raw.data,loadings){
  fz.scores <- (f.scores + mean(f.scores))/(sd(f.scores))
  means <- apply(raw.data,1, weighted.mean, w=loadings)
  sds <- apply(raw.data, 1, weighted.sd, w=loadings)
  grand.mean<-mean(means)
  grand.sd<-mean(sds)
  final.scores<-((fz.scores*grand.sd)+grand.mean)
  return(final.scores)
}
final.scores.ssl<-re.scale(
  Environment_score_score,Environment,fal.loadings)  

summary(final.scores.ssl)
View(final.scores.ssl)

# Social
Social_score<-factanal(x=Social, factors = 1, 
                           scores = "regression")
Social_score
Social_score_score<-as.vector(Social_score$scores)


fal.loadings<-Social_score$loadings[,1]
fal.loadings

weighted.sd<-function(x,w){
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w<- sum(x*w)/sum(w)
  x.sd.w<-sqrt((sum.w/(sum.w^2-sum.w2))*sum(w*(x-mean.w)^2))
  return(x.sd.w)
}

re.scale<- function(f.scores,raw.data,loadings){
  fz.scores <- (f.scores + mean(f.scores))/(sd(f.scores))
  means <- apply(raw.data,1, weighted.mean, w=loadings)
  sds <- apply(raw.data, 1, weighted.sd, w=loadings)
  grand.mean<-mean(means)
  grand.sd<-mean(sds)
  final.scores<-((fz.scores*grand.sd)+grand.mean)
  return(final.scores)
}
final.scores.ssl<-re.scale(
  Social_score_score,Social,fal.loadings)  

summary(final.scores.ssl)
View(final.scores.ssl)

# Marketing

Marketing_score<-factanal(x=Marketing, factors = 1, 
                           scores = "regression")
Marketing_score
Marketing_score_score<-as.vector(Marketing_score$scores)
View(Marketing_score_score)
summary(Marketing_score_score)

fal.loadings<-Marketing_score$loadings[,1]
fal.loadings

weighted.sd<-function(x,w){
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w<- sum(x*w)/sum(w)
  x.sd.w<-sqrt((sum.w/(sum.w^2-sum.w2))*sum(w*(x-mean.w)^2))
  return(x.sd.w)
}

re.scale<- function(f.scores,raw.data,loadings){
  fz.scores <- (f.scores + mean(f.scores))/(sd(f.scores))
  means <- apply(raw.data,1, weighted.mean, w=loadings)
  sds <- apply(raw.data, 1, weighted.sd, w=loadings)
  grand.mean<-mean(means)
  grand.sd<-mean(sds)
  final.scores<-((fz.scores*grand.sd)+grand.mean)
  return(final.scores)
}
final.scores.ssl<-re.scale(
  Marketing_score_score,Marketing,fal.loadings)  

summary(final.scores.ssl)
View(final.scores.ssl)