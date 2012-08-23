library(foreign)
library(reshape)

loadData <- function() {
  ######
  # Wave 1 Diabetes Info
  wave1 <- read.dta("../Wave 1/Data/nshap-core.dta")

  diabetes.w1 <- wave1[, c("su_id",
                           "fi_id",
                           "age",
                           "conditns_7",
                           "antidiabeticagen",
                           "miscantidiabetic",
                           "antidiabeticcomb",
                           "hba1c",
                           "hba1c_flag",
                           "sec3_start"                         
                           )
                       ]

  names(diabetes.w1) <- c("subject.id",
                          "w1.field.investigator",
                          "w1.age",
                          "w1.diagnosed.diabetes",
                          "w1.antidiabetics",
                          "w1.misc.antidiabetics",
                          "w1.antidiabetics.comb",
                          "w1.hba1c.whole",
                          "w1.hba1c_flag",
                          "w1.date")

  diabetes.w1$wave.1 = TRUE
  diabetes.w1$w1.field.investigator <- factor(diabetes.w1$w1.field.investigator)

  # Wave 1 Medications
  diabetes.w1$w1.misc.antidiabetics <- ifelse(!is.na(diabetes.w1$w1.misc.antidiabetics) 
                                              & diabetes.w1$w1.misc.antidiabetics== "reported", 
                                              TRUE, 
                                              FALSE)
  
  diabetes.w1$w1.antidiabetics <- ifelse(!is.na(diabetes.w1$w1.antidiabetics) 
                                         & diabetes.w1$w1.antidiabetics== "reported", 
                                         TRUE, 
                                         FALSE)
  
  diabetes.w1$w1.antidiabetics.comb <- ifelse(!is.na(diabetes.w1$w1.antidiabetics.comb) 
                                              & diabetes.w1$w1.antidiabetics.comb== "reported", 
                                              TRUE, 
                                              FALSE)
  
  diabetes.w1$w1.any.diabetes.meds <- apply(diabetes.w1[, c("w1.antidiabetics.comb",
                                                            "w1.antidiabetics",
                                                            "w1.misc.antidiabetics")],
                                            MARGIN = 1,
                                            any)
  # Wave 1 Diabetes Status
  diabetes.w1$w1.diagnosed.diabetes <- factor(diabetes.w1$w1.diagnosed.diabetes)
  diabetes.w1$w1.diagnosed.or.meds <- (diabetes.w1$w1.diagnosed.diabetes == "yes" |
                                       diabetes.w1$w1.any.diabetes.meds)
  
  diabetes.w1$w1.diabetes.status <- ifelse(diabetes.w1$w1.diagnosed.or.meds, 
                                           "Diagnosed Diabetic", 
                                           ifelse(diabetes.w1$w1.hba1c > 6.5, 
                                                  "Undiagnosed Diabetic",
                                                  "Nondiabetic"))

  diabetes.w1$w1.hba1c.discrete <- cut(diabetes.w1$w1.hba1c.whole,
                                       c(-Inf,6.0, 7, 8, 9, 10, 11,Inf),
                                       right = FALSE
                                       )
  ######
  # Wave 2 Diabetes Info
  wave2 <- read.dta("../Wave 2/Data/capi/nshapw2_main_results.dta")
  diabetes.w2 <- wave2[, c("SU_ID",
                           "FI_ID",
                           "diabhbs",
                           "DIABETES_MO",
                           "DIABETES_YR",
                           "BLDSPOT_ID1",
                           "BS_INTRO",
                           "BLDSPOT",
                           "NUM_BS",
                           "CurrDate"
                         )
                       ]

  # Calculate the Age at Wave 2 For Wave 1 Respondents
  interview.year <- wave2$CurrYear
  w1.birth.year <- wave2$PRELOAD_BIRTHYR

  w1.birth.year[w1.birth.year <= 0] <- NA

  diabetes.w2$w2.age <- ifelse(wave2$age == -5,
                               interview.year - w1.birth.year,
                               wave2$age)


  names(diabetes.w2) <- c("subject.id",
                          "w2.field.investigator",
                          "w2.diagnosed.diabetes",
                          "w2.month.diagnosed",
                          "w2.year.diagnosed",
                          "blood.spot.lab.id",
                          "w2.blood.spot.intro",
                          "w2.blood.spot",
                          "w2.num.blood.spots",
                          "w2.date",
                          "w2.age")
  
  diabetes.w2$wave.2 = TRUE
  diabetes.w2$w2.field.investigator <- factor(diabetes.w2$w2.field.investigator)



  diabetes.w2$w2.blood.spot.intro <- factor(diabetes.w2$w2.blood.spot.intro,
                                            labels=c("partial interview",
                                              "hiv",
                                              "refused",
                                              "continued"))

  diabetes.w2$w2.blood.spot <- factor(diabetes.w2$w2.blood.spot,
                                      labels = c("partial",
                                        "missing",
                                        "at least one spot",
                                        "equipment problem",
                                        "tried, unable to do"))

  diabetes.w2$w2.num.blood.spots <- factor(diabetes.w2$w2.num.blood.spots,
                                           labels = c("partial",
                                             "missing",
                                             1,
                                             2,
                                             3,
                                             4,
                                             5))
  


  diabetes.w2$w2.diagnosed.diabetes <- factor(diabetes.w2$w2.diagnosed.diabetes,
                                              levels=c("NO",
                                                "YES",
                                                "DON'T KNOW",
                                                "REFUSED"))

  ######
  # Wave 2 HbA1C Measures
  wave2.a1c <- read.csv("../Wave 2/Data/A1c/Blood Spot_Final A1c_w2 main.csv")

  names(wave2.a1c) <- c("blood.spot.lab.id", 
                        "w2.hba1c.dried",
                        "w2.hba1c.whole",
                        "w2.hba1c.quality.note",
                        "w2.hba1c.assay.note",
                        "w2.hba1c.assay.id",
                        "w2.hba1c.assay.date",
                        "w2.hba1c.reassay.status",
                        "w2.hba1c.original.assay.id",
                        "w2.hba1c.reassay.card.status",
                        "w2.hba1c.original.assay.note",
                        "w2.hba1c.collected.in.error"
                       )

  wave2.a1c$w2.hba1c.flag <- ifelse(is.na(as.numeric(as.character(wave2.a1c$w2.hba1c.dried))),
                                    as.character(wave2.a1c$w2.hba1c.dried),
                                    "Measured")
  wave2.a1c$w2.hba1c.dried <- as.numeric(as.character(wave2.a1c$w2.hba1c.dried))
  wave2.a1c$w2.hba1c.whole <- as.numeric(as.character(wave2.a1c$w2.hba1c.whole))


  ######
  # Demographics
  w1.demographics <- wave1[, c("su_id",
                               "gender",
                               "ethgrp")
                           ]
  
  w2.demographics <- wave2[wave2$surveytype != "W1 Returning Respondent",
                           c("SU_ID",
                             "gender",
                             "race",
                             "hispanic")
                           ]



  w2.demographics$gender <- factor(w2.demographics$gender,
                                   labels = c("male", "female"))


  w2.demographics$ethgrp <- with(w2.demographics, {

    ifelse(race == "WHITE/CAUCASIAN"
           & hispanic == "NO",
           "white",
           ifelse(race == "BLACK/AFRICAN AMERICAN",
                  "black",
                  ifelse(hispanic == "YES",
                         "hispanic, non-black",
                         "other")))
  }
       )

  ethnicity.levels <- c("white", "black", "hispanic, non-black", "other")

  w2.demographics$ethgrp <- factor(w2.demographics$ethgrp,
                                   levels=ethnicity.levels)

  w2.demographics <- w2.demographics[, c("SU_ID", "gender", "ethgrp")]

  names(w1.demographics) <- c("subject.id",
                              "gender",
                              "ethgrp")
  names(w2.demographics) <- c("subject.id",
                              "gender",
                              "ethgrp")

  demographics <- rbind(w1.demographics, w2.demographics)

    

  ######
  # Dispositions of Wave 1 Subjects at Wave 2
  disposition <- read.csv("../Wave 2/Data/capi/NSHAP Wave 2 Final Dispositions.csv")

  names(disposition) <- c("subject.id",
                          "w2.disposition.code",
                          "w2.disposition") 

  # Summarize Dispositions
  disposition$w2.status <- "Alive, unsurveyed"
 
  disposition[disposition$w2.disposition
           %in%
           c("Complete - All",
             "Partial Complete**"),
           "w2.status"] <- "Participated"
  
  disposition[disposition$w2.disposition
           %in%
           c("ProxyAdmin'd-R deceased (47,51)",
           "ProxyNotAdmin'd- R Deceased (49)"),
           "w2.status"] <- "Deceased"
  
  disposition[disposition$w2.disposition
           %in%
           c("ProxyAdmin'd-RPoorHealth (48,52)",
           "ProxyNotAdmin'd-RPoorHealth (50)",
             "TEMP: R too poor health (25)"),
           "w2.status"] <- "Poor Health"
  
  disposition[disposition$w2.disposition
              %in%
              c("End of field period (46)",
                "OthNoIW(14,24,38,39,40,41,42,43)",
                "R final unlocatable (18,19,20)",
                "Unavail dur field pd (44)"
                ),
              "w2.status"] <- "Other"
  
  disposition$w2.status <- factor(disposition$w2.status, 
                                  levels=c("Participated", 
                                    "Deceased",
                                    "Poor Health",
                                    "Alive, unsurveyed",
                                    "Other")
                                  )

  disposition$w2.survived <- ifelse(disposition$w2.status == "Deceased", 
                                    FALSE,
                                    ifelse(disposition$w2.status == "Other",
                                           NA,
                                           TRUE))

  ######
  # Merge Data
  complete <- merge(diabetes.w1,
                    diabetes.w2,
                    by ="subject.id",
                    all=TRUE)
  
  complete <- merge(complete,
                    disposition,
                    by = "subject.id",
                    all.x = TRUE)
  
  complete <- merge(complete,
                    wave2.a1c,
                    by = "blood.spot.lab.id",
                    all.x = TRUE)

  complete <- merge(complete,
                    demographics,
                    by = "subject.id",
                    all.x = TRUE)
                    

  complete$interim.diagnosed <- with(complete, {

    ifelse(w1.diagnosed.or.meds == FALSE, 
           ifelse(w2.diagnosed.diabetes == "NO", FALSE,
                  ifelse(w2.diagnosed.diabetes == "YES", 
                         TRUE, 
                         NA)),
           FALSE)
  }
                                     )
  
  complete[complete$subject.id %in% c(10025270,
                                      10030930,
                                      10011720,
                                      10037890),
                                      'w2.hba1c.flag'] <- "Not Recieved"
  complete$w2.hba1c.flag <- as.factor(complete$w2.hba1c.flag)

  complete$wave.1[is.na(complete$wave.1)] <- FALSE
  complete$wave.2[is.na(complete$wave.2)] <- FALSE

  return(complete)
}

longData <- function(complete) {
  a1c.long <- melt(complete, 
                   id.vars = c("subject.id", 
                     "interim.diagnosed", 
                     "w1.diabetes.status",
                     "w1.age"),
                   measure.vars = c("w1.hba1c", "w2.hba1c.dried"), 
                   variable_name = "wave")
  
  names(a1c.long) <- c("subject.id", 
                       "interim.diagnosed",
                       "wave.1.diabetes.status",
                       "w1.age",
                       "wave",
                       "a1c")
  
  a1c.long$wave.1.diabetes.status <- factor(a1c.long$wave.1.diabetes.status,
                                            levels=c("Nondiabetic",
                                              "Diagnosed Diabetic",
                                              "Undiagnosed Diabetic"
                                              ))
  
  a1c.long$wave <- as.numeric(a1c.long$wave)
  a1c.long$a1c <- as.numeric(as.character(a1c.long$a1c))

  return(a1c.long)
}
  


