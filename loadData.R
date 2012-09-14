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

  # The dried blood spots were turned into DCCT/NGSP equivalents with
  # this equation
  #
  # (Dried Bloot Spot Ratio) * 87.6 + 2.27
  diabetes.w1$w1.hba1c.unregressed <- (diabetes.w1$w1.hba1c.whole - 2.27)/.876
  
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
                           "ts5bldspot1a"
                         )
                       ]

  # Calculate the Age at Wave 2 For Wave 1 Respondents
  names(diabetes.w2) <- c("subject.id",
                          "w2.field.investigator",
                          "w2.diagnosed.diabetes",
                          "w2.month.diagnosed",
                          "w2.year.diagnosed",
                          "blood.spot.lab.id",
                          "w2.blood.spot.eligibility",
                          "w2.blood.spot.collection",
                          "w2.num.blood.spots",
                          "w2.date"
                          )
  
  diabetes.w2$wave.2 = TRUE

  diabetes.w2$w2.date <- as.POSIXlt(strptime(diabetes.w2$w2.date,
                                             "%m/%d/%Y %H:%M:%S %p"))

  interview.year <- diabetes.w2$w2.date$year + 1900
  w1.birth.year <- wave2$PRELOAD_BIRTHYR

  w1.birth.year[w1.birth.year <= 0] <- NA

  diabetes.w2$w2.age <- ifelse(wave2$age == -5,
                               interview.year - w1.birth.year,
                               wave2$age)


  diabetes.w2$w2.field.investigator <- factor(diabetes.w2$w2.field.investigator)


  diabetes.w2$w2.blood.spot.eligibility <- factor(diabetes.w2$w2.blood.spot.eligibility,
                                            labels=c("Partial interview",
                                              "HIV",
                                              "Refused",
                                              "Continued"))

  diabetes.w2$w2.blood.spot.collection <- factor(diabetes.w2$w2.blood.spot.collection,
                                      labels = c("Partial interview",
                                        "Missing",
                                        "At least one spot",
                                        "Equipment problem",
                                        "Tried, unable to do"))

  diabetes.w2$w2.num.blood.spots <- factor(diabetes.w2$w2.num.blood.spots,
                                           labels = c("Partial interview",
                                             "Missing",
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

  wave2.a1c$w2.hba1c.assay.date <- as.Date(as.character(wave2.a1c$w2.hba1c.assay.date), "%m/%d/%Y")
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
  # Comorbidities

  # Wave 2 Variables
  w2.morbid.vars <- c('SU_ID',
                      'hrtattack', # HAS A DOCTOR EVER TOLD YOU THAT
                                   # YOU HAD a heart attack or
                                   # myocardial infarction?
                      'arthritis', # Arthritis
                      'osteo_rheu', # Which type of arthritis did your
                                    # doctor tell you that you have,
                                    # osteo or rheumat
                      'emphasth', # HAS A DOCTOR TOLD YOU THAT YOU
                                  # HAVE emphysema, asthma, chronic
                                  # bronchitis, or ch
                      'stroke', # Stroke, cerebrovascular accident,
                                # blood clot or bleeding in
                      'diabhbs', # {#_since} HAS A DOCTOR TOLD YOU
                                 # THAT YOU HAVE diabetes mellitus or
                                 # high blood su
                      'skincancer', # Skin cancer (including melanoma,
                                    # basal cell carcenoma, squam,
                      'othcan', # {#_since} HAS A DOCTOR TOLD YOU THAT
                                # YOU HAVE cancer (other than skin
                                # cancer)?
                      'highblpress' #  Has a medical doctor ever told
                                    #  you that you have any of the
                                    #  following conditions? High
                                    #  blood pressure or hypertension?
                      )

  w2.morbidities <- wave2[, w2.morbid.vars]
  names(w2.morbidities) <- c('subject.id',
                             'w2.heart.attack',
                             'w2.arthritis',
                             'w2.osteo.or.rheu.arthritis',
                             'w2.emphysema.or.asthma',
                             'w2.stroke',
                             'w2.diabetes',
                             'w2.skin.cancer',
                             'w2.other.cancer',
                             'w2.hypertension')
                             
                             

  # Wave 1 Morbidities
  w1.morbid.vars <- c('su_id', 
                      'hrtprob', #ever told by doctor that he/she had
                                 #heart attack?
                      'conditns_1', #ever had arthritis
                      'conditns_2', #ever had ulcers
                      'conditns_3', #ever had emphysema (not in CI)
                      'conditns_4', #ever had asthma
                      'conditns_5', #ever had stroke
                      'conditns_6', #ever had hypertension
                      'conditns_7', #ever had diabetes
                      'conditns_13', #ever had skin cancer
                      'conditns_14', #ever had other cancer
                      'conditns_17' #ever had enlarged prostate
                      )

  w1.morbidities <- wave1[, w1.morbid.vars]
  names(w1.morbidities) <- c('subject.id',
                             'w1.heart.attack',
                             'w1.arthritis',
                             'w1.ulcers',
                             'w1.emphysema',
                             'w1.asthma',
                             'w1.stroke',
                             'w1.hypertension',
                             'w1.diabetes',
                             'w1.skin.cancer',
                             'w1.other.cancer',
                             'w1.enlarged.prostate')

  morbidities <- merge(w1.morbidities,
                       w2.morbidities,
                       by = 'subject.id',
                       all.y = TRUE)
                      
  everCondition <- function(w1.condition, w2.condition) {
    ever.condition <- (ifelse(is.na(morbidities[, w1.condition]),
                             FALSE,
                             morbidities[, w1.condition] == 'yes')
                      | morbidities[, w2.condition] == 'YES')
    return(ever.condition)
  }
  
  morbidities$hypertension <- everCondition('w1.hypertension',
                                            'w2.hypertension')

  morbidities$heart.attack <- everCondition('w1.heart.attack',
                                            'w2.heart.attack')

  morbidities$arthritis <- everCondition('w1.arthritis',
                                         'w2.arthritis')

  morbidities$stroke <- everCondition('w1.stroke',
                                      'w2.stroke')

  morbidities$cancer <- (ifelse(is.na(morbidities[, 'w1.skin.cancer']),
                             FALSE,
                             morbidities[, 'w1.skin.cancer'] == 'yes')
                         | ifelse(is.na(morbidities[, 'w1.other.cancer']),
                                  FALSE,
                                  morbidities[, 'w1.other.cancer'] == 'yes')
                         | morbidities[, 'w2.skin.cancer'] == 'YES'
                         | morbidities[, 'w2.other.cancer'] == 'YES')

  morbidities$emphysema.or.asthma <- (ifelse(is.na(morbidities[, 'w1.emphysema']),
                                  FALSE,
                             morbidities[, 'w1.emphysema'] == 'yes')
                         | ifelse(is.na(morbidities[, 'w1.asthma']),
                                  FALSE,
                                  morbidities[, 'w1.asthma'] == 'yes')
                         | morbidities[, 'w2.emphysema.or.asthma'] == 'YES')

  morbidities$w2.comorbidity.index <- rowSums(morbidities[, c("heart.attack",
                                                              "hypertension",
                                                              "emphysema.or.asthma",
                                                              "stroke",
                                                              "arthritis",
                                                              "cancer")])
                      
  morbidities <- morbidities[, c("subject.id", "w2.comorbidity.index")]
  

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

  complete <- merge(complete,
                    morbidities,
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

  complete$w2.hba1c.flag <- with(complete, {
    ifelse(complete$wave.2,
           ifelse(is.na(w2.hba1c.flag),
                  ifelse(w2.blood.spot.collection != "Missing",
                         as.character(w2.blood.spot.collection),
                         as.character(w2.blood.spot.eligibility)),
                  w2.hba1c.flag),
           NA)
  }
                                 )
  
  complete[complete$subject.id %in% c(10025270,
                                      10030930,
                                      10011720,
                                      10037890),
                                      'w2.hba1c.flag'] <- "Not recieved"

  complete$w2.hba1c.flag <- factor(complete$w2.hba1c.flag,
                                   levels = c("Partial interview",
                                     "HIV",
                                     "Refused",
                                     "Equipment problem",
                                     "Tried, unable to do",
                                     "Not recieved",
                                     "No Sample",
                                     "No Data",
                                     "Measured"))

  interviewer.collection <- table(complete$w2.field.investigator,
                                  complete$w2.hba1c.flag)

  interviewer.collection <- data.frame(row.names(interviewer.collection),
                                       (interviewer.collection[,'Measured']
                                        /rowSums(interviewer.collection)
                                        )
                                       )
  names(interviewer.collection) <- c("w2.field.investigator",
                                     "w2.fi.prop.hba1c.measured")

  complete <- merge(complete,
                    interviewer.collection,
                    by="w2.field.investigator",
                    all.x = TRUE)
  
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
                   measure.vars = c("w1.hba1c.unregressed", "w2.hba1c.dried"), 
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
  



