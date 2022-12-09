
setwd("C:/Users/Dell/Desktop/YD")

# Data
library(readxl)
final <- read_excel("Data.xlsx")

# Travel characteristics
extension<- cbind (final$Frequency, 
                   final$Weekvisit,
                   final$Dayvisit,
                   final$`Duration Spent`,
                   final$Distance...15)


extension_score<-factanal(x=extension, factors = 1, 
                        scores = "regression")

extension_score_score<-as.vector(extension_score$scores)

summary(extension_score_score)

fal.loadings<-extension_score$loadings[,1]
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
  extension_score_score,extension,fal.loadings)  

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

scores.and.loadings.1<-get.scores.fun(extension)
scores.and.loadings.1$factor.loadings

library(writexl)

write_xlsx(final.scores.ssl, 
           "C:/Users/Dell/Desktop/YD\\ext1.xlsx")
df<-data.frame(final.scores.ssl)
write_xlsx(df, 
           "C:/Users/Dell/Desktop/YD\\ext2.xlsx")

# Social cohesion 
extension<- cbind (final$Nature,
final$`Physical activity`,
final$`Family and friends trip`,
final$`Children playtime`,
final$`Peace of mind and relaxation`,
final$`Educational purpose`,
final$`Away from busy town`)

extension_score<-factanal(x=extension, factors = 1, 
                          scores = "regression")
View(extension)
extension_score_score<-as.vector(extension_score$scores)

summary(extension_score_score)

fal.loadings<-extension_score$loadings[,1]
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
  extension_score_score,extension,fal.loadings)  

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

scores.and.loadings.1<-get.scores.fun(extension)
scores.and.loadings.1$factor.loadings

library(writexl)

write_xlsx(final.scores.ssl, 
           "C:/Users/Dell/Desktop/YD\\ext12.xlsx")
df<-data.frame(final.scores.ssl)
write_xlsx(df, 
           "C:/Users/Dell/Desktop/YD\\ext22.xlsx")


# Facilities

extension<- cbind (final$`Trails and paths...26`,
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

extension_score<-factanal(x=extension, factors = 1, 
                          scores = "regression")

extension_score_score<-as.vector(extension_score$scores)

summary(extension_score_score)

fal.loadings<-extension_score$loadings[,1]
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
  extension_score_score,extension,fal.loadings)  

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

scores.and.loadings.1<-get.scores.fun(extension)
scores.and.loadings.1$factor.loadings

library(writexl)

write_xlsx(final.scores.ssl, 
           "C:/Users/Dell/Desktop/YD\\ext12.xlsx")
df<-data.frame(final.scores.ssl)
write_xlsx(df, 
           "C:/Users/Dell/Desktop/YD\\facilities2.xlsx")


# Socialbenefits
extension<- cbind (final$`health and wellbeing`,
                   final$nature,
                   final$`family-socail cohesion`,
                   final$`City image`)

extension_score<-factanal(x=extension, factors = 1, 
                          scores = "regression")

extension_score_score<-as.vector(extension_score$scores)

summary(extension_score_score)

fal.loadings<-extension_score$loadings[,1]
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
  extension_score_score,extension,fal.loadings)  

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

scores.and.loadings.1<-get.scores.fun(extension)
scores.and.loadings.1$factor.loadings

library(writexl)

write_xlsx(final.scores.ssl, 
           "C:/Users/Dell/Desktop/YD\\ext12.xlsx")
df<-data.frame(final.scores.ssl)
write_xlsx(df, 
           "C:/Users/Dell/Desktop/YD\\socialbenefits.xlsx")

# Environment
extension<- cbind (final$`urban air pollution`,
                   final$`urban heat island`,
                   final$Carbondioxide,
                   final$`Biodiversity promotion`,
                   final$Noise)

extension_score<-factanal(x=extension, factors = 1, 
                          scores = "regression")

extension_score_score<-as.vector(extension_score$scores)

summary(extension_score_score)

fal.loadings<-extension_score$loadings[,1]
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
  extension_score_score,extension,fal.loadings)  

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

scores.and.loadings.1<-get.scores.fun(extension)
scores.and.loadings.1$factor.loadings

library(writexl)

write_xlsx(final.scores.ssl, 
           "C:/Users/Dell/Desktop/YD\\ext12.xlsx")
df<-data.frame(final.scores.ssl)
write_xlsx(df, 
           "C:/Users/Dell/Desktop/YD\\environmentalbenefits.xlsx")



