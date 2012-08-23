library(plotrix)
library(xtable)
library(multcomp)

logit <- function(x) {
  return(log(x) - log(1-x))
}


source('loadData.R')
complete <- loadData()




# Gender
#
# Wave 1

a1cByGenderPlot <- function(diabetes.measure, label) {

  female.a1c <- complete[complete$gender=="female", diabetes.measure]
  male.a1c <- complete[complete$gender=="male", diabetes.measure]
  
  par(mfrow=c(2,2))

  brks = seq(4, 15, 0.5)
  hist(female.a1c,
       breaks=brks,
       main=paste("Female, ", label),
       xlab="HbA1C")
  hist(male.a1c,
       breaks=brks,
       main=paste("Male,  ", label),
       xlab="HbA1C")
  
  multhist(list(female.a1c,
                male.a1c),
           breaks=brks,
           probability=TRUE,
           main=paste("Female/Male, ", label),
           xlab="HbA1C midpoints",
           ylab="Density")

  par(mfrow=c(1,1))
}
  
a1cByGenderFiveNumsTable <- function(diabetes.measure) {
  
  female.nums <- summary(complete[complete$gender=="female",
                                  diabetes.measure])
  male.nums <- summary(complete[complete$gender=="male",
                                diabetes.measure])
  
  xtable(rbind(female.nums, male.nums))
}


gender.t.test <- t.test(w1.hba1c.whole ~ gender, complete)
xtable(as.data.frame(unlist(gender.t.test[1:4])))
  


  white.a1c <- complete[complete$ethgrp=="white", diabetes.measure]
  black.a1c <- complete[complete$ethgrp=="black", diabetes.measure]
  hispanic.a1c <- complete[complete$ethgrp=="hispanic, non-black", diabetes.measure]
  other.a1c <- complete[complete$ethgrp=="other", diabetes.measure]
  na.a1c <- complete[is.na(complete$ethgrp), diabetes.measure]
  
  par(mfrow=c(2,2))

  brks = seq(4, 15, 0.5)
  hist(white.a1c,
       breaks=brks,
       main=paste("White, ", label),
       xlab="HbA1C")
  hist(black.a1c,
       breaks=brks,
       main=paste("Black,  ", label),
       xlab="HbA1C")
  hist(black.a1c,
       breaks=brks,
       main=paste("Hispanic Non-Black,  ", label),
       xlab="HbA1C")
  hist(other.a1c,
       breaks=brks,
       main=paste("Other,  ", label),
       xlab="HbA1C")

  boxplot(w1.hba1c.whole ~ ethgrp, complete, horizontal=TRUE, at=4:1, varwidth=TRUE)


  
  multhist(list(female.a1c,
                male.a1c),
           breaks=brks,
           probability=TRUE,
           main=paste("Female/Male, ", label),
           xlab="HbA1C midpoints",
           ylab="Density")

  par(mfrow=c(1,1))
}


lmod <- lm(w2.hba1c.whole ~ ethgrp, complete)
summary(glht(lmod, linfct=mcp(ethgrp= "Tukey")))

# Insert some language about difference in means
