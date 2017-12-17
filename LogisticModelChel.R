require(dplyr)
#install.packages("bestglm")
library(bestglm)
setwd("C:/Users/Palma/Desktop/MS STATS/Year2 Sem1/STA 660 Data Practicum/Jones/Finalanalysis/LatestFinal")

load("JonesChel.RData")

toMod<-Jones%>%select(-State, -Credit, -Primary.Relationship, 
                      -Degree.Year.2, -School, -Country, -State.Full, 
                      -SubRegion, -SOROR, -FRTTY, -GaveLastYear.Amt, 
                      -Entity, -Year, -City)
colnames(toMod)[1]<-"y"
toMod<-toMod%>%select(Athlete:GaveLastYear.Amt.Log, y)
prevColNames<-colnames(toMod)
colnames(toMod)[1:(ncol(toMod)-1)]<-paste("X", 1:(ncol(toMod)-1), sep="")
remove(Jones)

set.seed(1)
train<-sample(seq(1, nrow(toMod), 1), nrow(toMod)/2, rep=FALSE)
test<-seq(1, nrow(toMod), 1)[!(seq(1, nrow(toMod), 1)%in%train)]


mods<-bestglm(toMod[train,], family=binomial, method="backward", IC="BIC", nvmax=30)


#############################################################################################
#############################################################################################
setwd("C:/Users/Palma/Desktop/MS STATS/Year2 Sem1/STA 660 Data Practicum/Jones/Finalanalysis/LatestFinal")

load("JonesChel.RData")

toMod<-Jones%>%select(-State, -Credit, -Primary.Relationship, 
                      -Degree.Year.2, -School, -Country, -State.Full, 
                      -SubRegion, -SOROR, -FRTTY, -GaveLastYear.Amt, 
                      -Entity, -Year, -City)

modelLogistic <- glm(Gave~ GradToGive + GaveLastYear + propGave+ times.given +Gender + Athlete +Relationship.Status
                     + Married +Region + School.Category + LastGave+Degree.Category
                     + SRVCE + ALUEV + CHAPT + REUN + Greek +Part.Level+GradToGive.2 +
                      propGave*(GradToGive + GaveLastYear+ times.given +Gender + Athlete +Relationship.Status
                               + Married +Region + School.Category + LastGave+Degree.Category
                               + SRVCE + ALUEV + CHAPT + REUN + Greek +Part.Level+GradToGive.2 )  +
                       GradToGive*(GaveLastYear + propGave+ times.given +Gender + Athlete +Relationship.Status
                        + Married +Region + School.Category + LastGave+Degree.Category
                        + SRVCE + ALUEV + CHAPT + REUN + Greek +Part.Level+GradToGive.2 ) + 
                        GaveLastYear*(propGave+ times.given +Gender + Athlete +Relationship.Status
                                      + Married +Region + School.Category + LastGave+Degree.Category
                                      + SRVCE + ALUEV + CHAPT + REUN + Greek +Part.Level+GradToGive.2 )
                     , family = "binomial", 
                     data =toMod )


modFinal<- glm(Gave ~ GradToGive + GaveLastYear + Gender + Athlete +
                 Relationship.Status + Married + Region + School.Category +
                 LastGave + Degree.Category + SRVCE + ALUEV + CHAPT + REUN +
                 Greek + Part.Level + GradToGive.2 + propGave * (GradToGive +
                                                                   GaveLastYear + Gender + Athlete + Relationship.Status + Married +
                                                                   Region + Degree.Category + SRVCE + ALUEV + CHAPT + Greek +
                                                                   Part.Level + GradToGive.2) + GradToGive * (GaveLastYear +
                                                                                                                Gender + Athlete + Relationship.Status + Region + School.Category +
                                                                                                                LastGave + Degree.Category + SRVCE + REUN + Greek + Part.Level +
                                                                                                                GradToGive.2) + GaveLastYear * (Gender + Athlete + +Region +                                                                                                                              School.Category + ALUEV + CHAPT + Greek + Part.Level + GradToGive.2), 
               family = "binomial", data = Jones)


summary(modFinal)

roc.curve(modFinal, Jones)

fitteddat<- cbind(Jones,modFinal$fitted.values,Jones$Gave)
View(fitteddat)


# looking at 50% High  Probability Givers
highprob.donors <- fitteddat %>% filter(fitteddat$`modFinal$fitted.values` > 0.45)
table(highprob.donors$School) #farmer make up majority 77.14%
table(highprob.donors$Region) # midwest make up majority 68.57
table(highprob.donors$Athlete) # 29.6% athletes
table(highprob.donors$Greek) # roughly 75% Greek
table(highprob.donors$Gender) # 65.7% male
table(highprob.donors$Married) # 65.7% married

hist(highprob.donors$SRVCE) # uniform
hist(highprob.donors$ALUEV) # right skewed
hist(highprob.donors$Part.Level) # uniform

# looking at small donors
unlikely.donors <- fitteddat %>% filter(fitteddat$`modFinal$fitted.values` < 0.45) # donors less than $50
table(unlikely.donors$School)  # arts & science make up majority 45.5%
table(unlikely.donors$Region) # midwest make up majority 67.84
table(unlikely.donors$Athlete) # 2.65% athletes
table(unlikely.donors$Greek) # roughly 33.4% Greek
table(unlikely.donors$Gender) # 35.45% male
table(unlikely.donors$Married) # 0.3% married


hist(unlikely.donors$SRVCE) # no service
hist(unlikely.donors$ALUEV) # no alum events
hist(unlikely.donors$Part.Level) # no participation if little


##Selected individual  A00029111
selected <-fitteddat[fitteddat$Entity=="A00029111",]

View(selected)

fitteddat<- cbind(Jones,modFinal$fitted.values,Jones$Gave)

plot( selected$GradToGive , selected$`modFinal$fitted.values`, xlab="Years after Graduation", ylab="Probability of Giving", main="Profile of Selected Highly likely Donor")

ggplot(selected, aes( GradToGive,`modFinal$fitted.values`)) +
  geom_point() +
  geom_smooth()+labs(list( x = "Years after Graduation", y = "Probability of Giving")  )


