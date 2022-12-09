
setwd("C:/Users/Dell/Desktop/YD")

# Data
library(readxl)
final <- read_excel("Data.xlsx")

# Travel characteristics
Travel<- cbind (final$Frequency, 
                   final$Weekvisit,
                   final$Dayvisit,
                   final$`Duration Spent`,
                   final$Distance...15)


travel_score<-factanal(x=extension, factors = 1, 
                        scores = "regression")

travel_score_score<-as.vector(travel_score$scores)

summary(travel_score_score)

fal.loadings<-travel_score$loadings[,1]
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
  travel_score_score,travel,fal.loadings)  

summary(final.scores.ssl)

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

scores.and.loadings.1<-get.scores.fun(travel)
scores.and.loadings.1$factor.loadings

library(writexl)

write_xlsx(final.scores.ssl, 
           "C:/Users/Dell/Desktop/YD\\travel.xlsx")
df<-data.frame(final.scores.ssl)
write_xlsx(df, 
           "C:/Users/Dell/Desktop/YD\\travel.xlsx")

# Social cohesion 
social<- cbind (final$Nature,
final$`Physical activity`,
final$`Family and friends trip`,
final$`Children playtime`,
final$`Peace of mind and relaxation`,
final$`Educational purpose`,
final$`Away from busy town`)

social_score<-factanal(x=social, factors = 1, 
                          scores = "regression")
View(extension)
social_score_score<-as.vector(social_score$scores)

summary(social_score_score)

fal.loadings<-social_score$loadings[,1]
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
  social_score_score,social,fal.loadings)  

summary(final.scores.ssl)

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

scores.and.loadings.1<-get.scores.fun(social)
scores.and.loadings.1$factor.loadings

write_xlsx(final.scores.ssl, 
           "C:/Users/Dell/Desktop/YD\\social.xlsx")
df<-data.frame(final.scores.ssl)
write_xlsx(df, 
           "C:/Users/Dell/Desktop/YD\\social.xlsx")


# Facilities

facilities<- cbind (final$`Trails and paths...26`,
                   final$`Place to sit and rest...27`,
                   final$`Clean restrooms...28`,
                   final$`Childrens' playgrounds...29`,
                   final$`Rich natural plants...30`,
                   final$`Grass lawns and flowers...31`,
                   final$`Tall tress and canopy...32`,
                   final$`Landscape naturalness with clean air...43`,
                   final$Wastebin...44,
                   final$Illumination...35,
                   final$Safety)

facilities_score<-factanal(x=facilities, factors = 1, 
                          scores = "regression")

facilities_score_score<-as.vector(facilities_score$scores)

summary(facilities_score_score)

fal.loadings<-facilities_score$loadings[,1]
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
  facilities_score_score,facilities,fal.loadings)  

summary(final.scores.ssl)

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

scores.and.loadings.1<-get.scores.fun(facilities)
scores.and.loadings.1$factor.loadings

write_xlsx(final.scores.ssl, 
           "C:/Users/Dell/Desktop/YD\\facilities.xlsx")
df<-data.frame(final.scores.ssl)
write_xlsx(df, 
           "C:/Users/Dell/Desktop/YD\\facilities.xlsx")


# Socialbenefits
socialbenefits<- cbind (final$`health and wellbeing`,
                   final$nature,
                   final$`family-socail cohesion`,
                   final$`City image`)

socialbenefits_score<-factanal(x=socialbenefits, factors = 1, 
                          scores = "regression")

socialbenefits_score_score<-as.vector(socialbenefits_score$scores)

summary(socialbenefits_score_score)

fal.loadings<-socialbenefits_score$loadings[,1]
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
  socialbenefits_score_score,socialbenefits,fal.loadings)  

summary(final.scores.ssl)

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

scores.and.loadings.1<-get.scores.fun(socialbenefits)
scores.and.loadings.1$factor.loadings

write_xlsx(final.scores.ssl, 
           "C:/Users/Dell/Desktop/YD\\socialbenefits.xlsx")
df<-data.frame(final.scores.ssl)
write_xlsx(df, 
           "C:/Users/Dell/Desktop/YD\\socialbenefits.xlsx")

# Environment
environment<- cbind (final$`urban air pollution`,
                   final$`urban heat island`,
                   final$Carbondioxide,
                   final$`Biodiversity promotion`,
                   final$Noise)

environment_score<-factanal(x=environment, factors = 1, 
                          scores = "regression")

environment_score_score<-as.vector(environment_score$scores)

summary(environment_score_score)

fal.loadings<-environment_score$loadings[,1]
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
  environment_score_score,environment,fal.loadings)  

summary(final.scores.ssl)

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

scores.and.loadings.1<-get.scores.fun(environment)
scores.and.loadings.1$factor.loadings

write_xlsx(final.scores.ssl, 
           "C:/Users/Dell/Desktop/YD\\environment")
df<-data.frame(final.scores.ssl)
write_xlsx(df, 
           "C:/Users/Dell/Desktop/YD\\environmentalbenefits.xlsx")



