# Jones Project

#### libraries ####
library(dplyr)
library(tidyr)
library(leaps)

#### read in data ####
setwd("/Users/craigheard/Documents/Miami Graduate School/Fall 2016/STA 660/Project 3/data")
load("Jones.RData") # complete dataset

#sum data by entity ID
JonesNA <- Jones
JonesNA$Credit[JonesNA$Credit == 0] <- NA

# create year.1st variable
year.1st <- Jones %>%
  filter(Gave == 1) %>%
  arrange(Entity, GradToGive) 

year.1st <- year.1st %>%
  group_by(Entity) %>%
  summarize(Year.1st.Give = min(GradToGive))


# summarise dataset 
# Due to violation we now try a transformation on Y and have to add 1 to total credit so that log works
giving.sum <- JonesNA %>%
  filter(Credit != 0) %>%
  group_by(Entity) %>%
  summarize(SumCredit = sum(Credit, na.rm = TRUE), Married = Married[1],
            Gender = Gender[1], School = School.Category[1] , Degree.Count = Degree.Count[1], 
            Degree.Year.1 = Degree.Year.1[1],  Athlete = Athlete[1], DegreeCat = Degree.Category[1],
            Relationship.Status = Relationship.Status[1], Region = Region[1],
            Service = SRVCE[1], AlumEvent = ALUEV[1],
            ChapEvent = CHAPT[1], Reunion = REUN[1], Greek = Greek[1], PartLevel = Part.Level[1]) %>%
  mutate(logSum = log(SumCredit))

giving.sum <- merge(giving.sum, year.1st, by="Entity", all.x = TRUE) %>%
              select(-Entity, -SumCredit)

# set random seed and create training and test datasets.
set.seed(769732)
train=sample(c(TRUE,FALSE), nrow(giving.sum),rep=TRUE)
test=(!train)

regfit.best <- regsubsets(logSum~.^2, data=giving.sum[train,], nvmax =150, method = "forward")

test.mat=model.matrix(logSum ~ .^2,data=giving.sum[test,])

val.errors=rep(NA,150)
exp.val.errors=rep(NA,150)


for(i in 1:150){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((giving.sum$logSum[test]-pred)^2) 
  exp.val.errors[i]=mean((exp(giving.sum$logSum[test])-exp(pred))^2) 
}

# plots validation errors to correct model can be chosen
val.errors
plot(val.errors ,type='b', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     ylab ="RMSE", xlab = "Model Size")

var(giving.sum$logSum)
coef(regfit.best ,40)

# create test ds and merge with predicted values to use for big donor inference
test.ds <- giving.sum[test,]


regfit.best <- regsubsets(logSum~.^2, data=giving.sum[train,], nvmax =40, method = "forward")

test.mat=model.matrix(logSum ~ .^2,data=giving.sum[test,])

val.errors=rep(NA,40)
exp.val.errors=rep(NA,40)


for(i in 1:150){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((giving.sum$logSum[test]-pred)^2) 
  exp.val.errors[i]=mean((exp(giving.sum$logSum[test])-exp(pred))^2) 
}

colnames(pred) <- "yhat"
test.pred <- cbind(test.ds, pred2)


# apply to full dataset - Note: not needed anymore as we are predicting using test DS above.
# 
# regfit.fwd = regsubsets(logSum ~ .^2, data = giving.sum, nvmax = 40, method = "forward")
# val.errors2 = rep(NA, 40)
# x.test2 = model.matrix(logSum ~ .^2, data = giving.sum)  # notice the -index!
# for (i in 1:40) {
#   coefi2 = coef(regfit.fwd, id = i)
#   pred2 = x.test2[, names(coefi2)] %*% coefi2
#   val.errors2[i] = mean((giving.sum$logSum - pred2)^2)
# }
# 
# colnames(pred2) <- "yhat"
# new <- cbind(giving.sum, pred2)

# looking at big donors using test dataset 
big.donors <- test.pred %>% filter(yhat > log(2000)) # donors greater than $2,000
table(big.donors$School) #farmer make up majority 65.85%
table(big.donors$Region) # midwest make up majority 70.12
table(big.donors$Athlete) # 27.4% athletes
table(big.donors$Greek) # roughly 52.43% Greek
table(big.donors$Gender) # 75.86% male
table(big.donors$Married) # 71.34% married
table(big.donors$Degree.Year.1)


hist(big.donors$Service) # uniform
hist(big.donors$AlumEvent) # right skewed
hist(big.donors$PartLevel) # uniform

# looking at small donors
small.donors <- test.pred %>% filter(yhat < 3.912023) # donors less than $50
table(small.donors$School)  # arts & science and education make up majority 62.7%
table(small.donors$Region) # midwest make up majority 71.69
table(small.donors$Athlete) # 2.57% athletes
table(small.donors$Greek) # roughly 29.47% Greek
table(small.donors$Gender) # 64.24% female
table(small.donors$Married) # 0.3% married
table(small.donors$Degree.Year.1)

hist(small.donors$Service) # no service
hist(small.donors$AlumEvent) # no alum events
hist(small.donors$PartLevel) # no participation if little





# 1st attempt to get regsubsets to get predicted values 

# # run regsubsets on best selective variables.
# # best 50 variable model on whole dataset
# finalMod <- regsubsets(logSum~.^2, data=giving.sum, nvmax = 40, nbest =1, method = "forward")
# finMod <- summary(finalMod)
# coef(finalMod ,40)
# 
# outMat <- finMod$outmat
# dim(outMat)
# outMat[41,]
# outMat <- cbind(rep(1, dim(outMat)[1]), outMat)
# 
# giv.sum <- model.matrix(logSum~.^2, data=giving.sum)
# 
# opt.giv.sum <- giv.sum[outMat[41,]==1]
# 
# # predict regsubsets output
# predict.regsubsets = function(object, newdata, id, ...){
#   temp_X <- cbind(rep(1, length(newdata[,1])), newdata)
#   colnames(temp_X) <- c("(Intercept)", colnames(newdata))
# 
#   coefi = coef(object, id=i)
#   my_pred = as.matrix(temp_X[ ,names(coefi)])%*%coefi
# 
#   return(my_pred)
# }
# predict.regsubsets(object = finalMod, newdata = giving.sum, id=2)

# ds <- cbind(giving.sum[,"logSum"],giv.sum)
# ds <- as.data.frame(ds) %>%
#       select(V1, GenderM, Degree.Year.1, AthleteNon-athlete,Service, PartLevel,
#              Married1:GenderM, Married1:Degree.Year.1, Married1:Year.1st.Give,
#              GenderM:SchoolEducation/Health, GenderM:AthleteNon-athlete, GenderM:AlumEvent,
#              GenderM:Year.1st.Give, SchoolEducation/Health:Degree.Count, SchoolOther:Degree.Count,
#              SchoolFarmer:AthleteNon-athlete, SchoolFarmer:DegreeCatUndergraduate, SchoolEducation/Health:Relationship.Statusliving,
#              SchoolOther:Relationship.Statusliving, SchoolEducation/Health:AlumEvent,SchoolFarmer:PartLevel,
#              SchoolOther:Year.1st.Give,Degree.Count:Degree.Year.1,Degree.Year.1:DegreeCatUndergraduate,
#              Degree.Year.1:Reunion, AthleteNon-athlete:DegreeCatOther, AthleteNon-athlete:RegionNortheast,
#              AthleteNon-athlete:RegionWest, AthleteNon-athlete:Greek, DegreeCatUndergraduate:Relationship.Statusliving,
#              DegreeCatUndergraduate:RegionNortheast, DegreeCatOther:RegionOther,Relationship.Statusliving:Service,
#              Relationship.Statusliving:Year.1st.Give, RegionWest:Service, Reunion:Year.1st.Give, Greek:PartLevel,
#              Married1:Relationship.Statusliving, AthleteNon-athlete:ChapEvent)

fit1 <- lm(V1 ~ .,
             data=giving.sum)
summary(fit1)
# 
# giving.sum$yhat <- predict(fit1, giving.sum, type = "response")





# predict function
predict.regsubsets =function(object ,newdata ,id ,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi 
}


# cross validation sets
k=20
set.seed(1)
folds=sample(1:k,nrow(giving.sum),replace=TRUE)
cv.errors=matrix(NA,k,17, dimnames=list(NULL, paste(1:17)))
exp.cv.errors=matrix(NA,k,17, dimnames=list(NULL, paste(1:17)))

for(j in 1:k){
  best.fit=regsubsets(logSum~.^2, data=giving.sum[folds!=j,], 
                      nvmax =17, method = "forward")
  for(i in 1:17){
    pred=predict.regsubsets(best.fit, giving.sum[folds ==j,], id=i)
    cv.errors[j,i]= mean( (giving.sum$logSum[folds==j] - pred)^2 )
    exp.cv.errors[j,i]= mean( (giving.sum$logSum[folds==j] - pred)^2 )
  }
}

mean.cv.errors = apply(cv.errors, 2, mean, na.rm = TRUE)
mean.cv.errors

plot(mean.cv.errors ,type='b', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

reg.best <- regsubsets(logSum~.^2, data=giving.sum, nvmax =17, method = "forward")
coef(reg.best ,9)

fit2 <- lm(logSum ~ Degree.Year.1+Region+Service+AlumEvent+Married:Relationship.Status+
             School:Degree.Year.1 +Athlete:GradToGive+Service:Greek+Reunion:PartLevel, data=giving.sum)
summary(fit2)

