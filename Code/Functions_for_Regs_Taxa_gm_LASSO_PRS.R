
library(dplyr)
library(data.table)

library(car)
library(tidyr)
library(tidyverse)
library(tibble)
library(tidystats)

library(caret)
library(glmnet)
set1<- 'Set 1: male ,AGE_GOLD, smoker.current.v1.v2, PCs' 


reg.function<-  function( outcome, dataset){
  m.res<-  glm( outcome~., family='binomial',data = dataset)
  return(m.res)
}

steps.func<- function(dataset, otu.set1, covs, outcome){
  d0<- dataset %>%    dplyr::select( all_of(covs))
  d1<- dataset %>%    dplyr::select( all_of(covs), one_of(otu.set1))
  d2<- dataset %>%    dplyr::select( all_of(covs), one_of(otu.set1), "prs.std.wang" )

  m0.1<-  reg.function(outcome, d0)
  m1.1<-  reg.function(outcome, d1)
  m2.1<-  reg.function(outcome, d2)
  
  model.res<- list(Step0.m1=m0.1, Step1.m1=m1.1,  Step2.m1=m2.1)
  return(model.res)
}


compare.func<- function(m0,m1,m2, outcome){
  
  
  m0.r<-  PseudoR2(m0, which='VeallZimmermann')
  m1.r<- PseudoR2(m1, which='VeallZimmermann')
  m2.r<- PseudoR2(m2, which='VeallZimmermann')
  
  
  m.aov.12<- anova(m1,  m2,test ="Chisq") %>%
    dplyr::select(.,'Pr(>Chi)')%>%
    dplyr::filter(is.na(`Pr(>Chi)`)== F)%>%
    dplyr::rename(p.add.prs=`Pr(>Chi)`)
  
  
  m.aov.10<- anova(m0,  m1,test ="Chisq") %>%
    dplyr::select(.,'Pr(>Chi)')%>%
    dplyr::filter(is.na(`Pr(>Chi)`)== F)%>%
    dplyr::rename(p.add.gm=`Pr(>Chi)`)
  
  (auc0<- predict(m0) %>%
      bigstatsr ::AUC( outcome ))
  
  (auc1<- predict(m1) %>%
      bigstatsr ::AUC( outcome ))
  (auc2<- predict(m2) %>%
      bigstatsr ::AUC( outcome ))
  # c.mat0<- table(predict(m0), outcome)
  # sen0<- sensitivity(c.mat0)
  # spec0<- specificity(c.mat0)
  # ppv0<- posPredValue(c.mat0)
  # 
  # c.mat1<- table(predict(m1), outcome)
  # sen1<- sensitivity(c.mat1)
  # spec1<- specificity(c.mat1)
  # ppv1<- posPredValue(c.mat1)
  # 
  # c.mat2<- table(predict(m2), outcome)
  # sen2<- sensitivity(c.mat2)
  # spec2<- specificity(c.mat2)
  # ppv2<- posPredValue(c.mat2)
  # sen0, sen1, sen2, spec0, spec1, spec2, ppv0,ppv1, ppv2,
  m.compare<-  data.frame(r2.0=m0.r,r2.1=m1.r,r2.2=m2.r,
                          auc0, auc1,  auc2, 
                          m.aov.10, 
                          m.aov.12
  )
  return(m.compare)
}



steps.pc.func<- function(dataset, GM.1, covs, outcome){
  
  d0<- dataset %>%    dplyr::select( all_of(covs))
  d1<- dataset %>%    dplyr::select(all_of(covs), "prs.std.wang" )
  d2<- dataset %>%    dplyr::select( all_of(covs), "prs.std.wang", one_of(GM.1))

  
  m0.1<-  reg.function(outcome, d0)
  m1.1<-  reg.function(outcome, d1)
  m2.1<-  reg.function(outcome, d2)
  
 
  
  model.res<- list(Step0.m1=m0.1, Step1.m1=m1.1,  Step2.m1=m2.1)
  return(model.res)
}



compare.func.pc<- function(m0,m1, m2, outcome, trash=0){
  # m0<- step.pc.OA$Step0.m1
  # m1<- step.pc.OA$Step1.m1
  # m2<- step.pc.OA$Step2.m1
  # 
  # m2b<- step.pc.OA$Step2b.m1
  # m2c<- step.pc.OA$Step2c.m1
  # outcome<- Model.OA$asthma.ever
  
  
  m0.r<-  PseudoR2(m0, which='VeallZimmermann')
  m1.r<- PseudoR2(m1, which='VeallZimmermann')
  m2.r<- PseudoR2(m2, which='VeallZimmermann')
  
  
  
  m.aov.12<- anova(m2,  m1,test ="Chisq") %>%
    dplyr::select(.,'Pr(>Chi)')%>%
    dplyr::filter(is.na(`Pr(>Chi)`)== F)%>%
    dplyr::rename(p.add.gm=`Pr(>Chi)`)
  
  m.aov.10<- anova(m0,  m1,test ="Chisq") %>%
    dplyr::select(.,'Pr(>Chi)')%>%
    dplyr::filter(is.na(`Pr(>Chi)`)== F)%>%
    dplyr::rename(p.add.prs=`Pr(>Chi)`)
  
  (auc0<- predict(m0) %>%
      bigstatsr ::AUC( outcome ))
  
  (auc1<- predict(m1) %>%
      bigstatsr ::AUC( outcome ))
  (auc2<- predict(m2) %>%
      bigstatsr ::AUC( outcome ))
  # c.mat0<- table(predict(m0), outcome)
  # sen0<- sensitivity(c.mat0)
  # spec0<- specificity(c.mat0)
  # ppv0<- posPredValue(c.mat0)
  # 
  # c.mat1<- table(predict(m1), outcome)
  # sen1<- sensitivity(c.mat1)
  # spec1<- specificity(c.mat1)
  # ppv1<- posPredValue(c.mat1)
  # 
  # c.mat2<- table(predict(m2), outcome)
  # sen2<- sensitivity(c.mat2)
  # spec2<- specificity(c.mat2)
  # ppv2<- posPredValue(c.mat2)
  # sen0, sen1, sen2, spec0, spec1, spec2, ppv0,ppv1, ppv2,
  
  m.compare<-  data.frame(r2.0=m0.r,r2.1=m1.r,r2.2=m2.r,
                          auc0, auc1,  auc2, 
                          m.aov.10,
                          m.aov.12
  )
  
  return(m.compare)
}



combine.res<- function(GM.1, Step0.m1, Step1.m1, Step2.m1,covs, out){
  
  m1.res<- compare.func(Step0.m1, Step1.m1, Step2.m1, out)
 
  
  model.compare<- m1.res %>%
    mutate(covariates=paste(covs, collapse=','))%>%
    mutate(GMFeatures=c(paste(GM.1, collapse=',')))
  return(model.compare)
}
combine.res.pc<- function(GM.1, Step0.m1, Step1.m1, Step2.m1,covs, out){
  
  m1.res<- compare.func.pc(Step0.m1, Step1.m1, Step2.m1, out)
  
  
  model.compare<- m1.res %>%
    mutate(covariates=paste(covs, collapse=','))%>%
    mutate(GMFeatures=c(paste(GM.1, collapse=',')))
  return(model.compare)
}




