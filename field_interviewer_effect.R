library(lme4)

source('loadData.R')
source('Sankey.R')

logit <- function(x) { log(x) - log(1-x) }


complete <- loadData()

summary(complete[complete$wave.2, "w2.hba1c.flag"])

wave2 <- complete[!is.na(complete$w2.hba1c.whole) &
                  !is.na(complete$ethgrp) &
                  !is.na(complete$gender) &
                  !is.na(complete$w2.age) &
                  !is.na(complete$w2.field.investigator), ]
                   
model.lm <- lm(logit(w2.hba1c.whole/100) ~ 1 + ethgrp + gender + w2.age,
               wave2)
model.lmer <- lmer(logit(w2.hba1c.whole/100) ~ 1 + ethgrp + gender + w2.age +(1|w2.field.investigator),
                wave2, REML=FALSE)

lrt.observed <- as.numeric(2*(logLik(model.lmer) - logLik(model.lm)))

nsim <- 999
lrt.sim <- rep(0, nsim)
for (i in 1:nsim) {
  y <- unlist(simulate(model.lm))
  null.model <- lm(y ~ 1 + ethgrp + gender + w2.age, wave2)
  summary(null.model)
  alt.model <- lmer(y ~ 1 + ethgrp + gender + w2.age + (1|w2.field.investigator),
                    wave2,
                    REML=FALSE)
  lrt.sim[i] <- as.numeric(2*(logLik(alt.model) - logLik(null.model)))
}

mean(lrt.sim > lrt.observed)
                

SankeyR(c(3377), c(1, 4, 234, 9, 16, 4, 3109), "",
        labels=c("Subjects",
          "Partial Interview",
          "HIV", "Refused",
          "Equipment problem",
          "Tried, Unable to Do",
          "Sample Not Received by Lab",
          "Samples Received by Lab"))

SankeyR(c(3377), c(1, 4, 234, 9, 16, 4, 42, 30, 3037), "",
        labels=c("Subjects",
          "Partial Interview",
          "HIV", "Refused",
          "Equipment problem",
          "Tried, Unable to Do",
          "Sample Not Received by Lab",
          "No Sample Left",
          "No Data Recorded",
          "HbA1C Measured"))

table(complete$w2.num.blood.spots)
table(complete$w2.num.blood.spots, complete$w2.hba1c.flag)
prop.table(table(complete$w2.num.blood.spots, complete$w2.hba1c.flag), 1) * 100

