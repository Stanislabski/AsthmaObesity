
library(data.table)
library(dplyr)
# library(ggtree)
library(tidyverse)
library(vegan)
library(tibble)
library(tidyr)
library(GUniFrac)
library(cluster)
library(dirmult)
library(MiRKAT)
library(MASS)
library(quadprog)
library(quantreg)
library(PearsonDS)
library(SKAT)
library(tibble)
#######################################################

load(file='Aitchison.dm.RData')

load( file='BDC-SOL-GOLD-0424.RData')


#######################################################

dm.df<- data.frame(as.matrix(dm))

gm.dat<- gold %>%
  dplyr::filter(goldID %in% rownames(dm.df))

#######################################################

## Current asthma

gm.dat.current<- gm.dat %>%
  dplyr::filter(is.na(asthma.current)==F)%>%
  dplyr::filter(goldID %in% rownames(dm.df))


dm.sub.current<-   dm.df %>% 
  dplyr::filter(row.names(dm.df) %in% gm.dat.current$goldID) %>%
  dplyr::select( one_of(gm.dat.current$goldID) )%>%
  as.matrix()
dim(dm.sub.current)

covs.1.c<-dplyr::select(gm.dat.current, gender_v1_v2 ,AGE_GOLD, smoker.current.v1.v2)
covs.2.c<- dplyr::select(gm.dat.current, gender_v1_v2 ,AGE_GOLD,  income,education_v1_v2, smoker.current.v1.v2)
covs.3.c<- dplyr::select(gm.dat.current,  gender_v1_v2 ,AGE_GOLD,  income,education_v1_v2, relocation_age, background, generation , smoker.current.v1.v2)


K.Ait.current= D2K(dm.sub.current) 


(asthma.ait1.c<- MiRKAT(gm.dat.current$asthma.current ,Ks = K.Ait.current,out_type ='D', X=covs.1.c, returnKRV=T, returnR2=T))
(asthma.ait2.c<- MiRKAT(gm.dat.current$asthma.current ,Ks = K.Ait.current,out_type ='D', X=covs.2.c, returnKRV=T, returnR2=T))
(asthma.ait3.c<- MiRKAT(gm.dat.current$asthma.current ,Ks = K.Ait.current,out_type ='D', X=covs.3.c, returnKRV=T, returnR2=T))

dm.sub.OA<-   dm.sub.current %>% 
  as.data.frame()%>%
  dplyr::filter(row.names(dm.sub.current) %in% gm.dat.current$goldID[gm.dat.current$asthma.current.obese %in% c('Obese asthma', 'Neither')]) %>%
  dplyr::select( one_of(gm.dat.current$goldID[gm.dat.current$asthma.current.obese %in% c('Obese asthma', 'Neither')]) )%>%
  as.matrix()


dm.sub.A<-   dm.sub.current %>% 
  as.data.frame()%>%
  dplyr::filter(row.names(dm.sub.current) %in% gm.dat.current$goldID[gm.dat.current$asthma.current.obese %in% c('Asthma only', 'Neither')]) %>%
  dplyr::select( one_of(gm.dat.current$goldID[gm.dat.current$asthma.current.obese %in% c('Asthma only', 'Neither')]) )%>%
  as.matrix()

dm.sub.O<-   dm.sub.current %>% 
  as.data.frame()%>%
  dplyr::filter(row.names(dm.sub.current) %in% gm.dat.current$goldID[gm.dat.current$asthma.current.obese %in% c('Obese only', 'Neither')]) %>%
  dplyr::select( one_of(gm.dat.current$goldID[gm.dat.current$asthma.current.obese %in% c('Obese only', 'Neither')]) )%>%
  as.matrix()

gm.OA<- gm.dat.current[gm.dat.current$asthma.current.obese %in% c('Obese asthma', 'Neither'),]
gm.A<- gm.dat.current[gm.dat.current$asthma.current.obese %in% c('Asthma only', 'Neither'),]
gm.O<- gm.dat.current[gm.dat.current$asthma.current.obese %in% c('Obese only', 'Neither'),]


table(colnames(dm.sub.OA)==gm.dat.current$goldID[gm.dat.current$asthma.current.obese %in% c('Obese asthma', 'Neither')])

K.Ait.OA= D2K(dm.sub.OA) 

covs.1<-dplyr::select(gm.OA, gender_v1_v2 ,AGE_GOLD, smoker.current.v1.v2)
covs.2<- dplyr::select(gm.OA, gender_v1_v2 ,AGE_GOLD,   income,education_v1_v2,smoker.current.v1.v2)
covs.3<- dplyr::select(gm.OA,  gender_v1_v2 ,AGE_GOLD,   income,education_v1_v2, relocation_age, generation,background_5 ,smoker.current.v1.v2)

(asthma.ait1.OA<- MiRKAT(gm.OA$asthma.current ,Ks = K.Ait.OA,out_type ='D', X=covs.1, returnKRV=T, returnR2=T))

(asthma.ait2.OA<- MiRKAT(gm.OA$asthma.current ,Ks = K.Ait.OA,out_type ='D', X=covs.2, returnKRV=T, returnR2=T))

(asthma.ait3.OA<- MiRKAT(gm.OA$asthma.current ,Ks = K.Ait.OA,out_type ='D', X=covs.3, returnKRV=T, returnR2=T))

K.Ait.A= D2K(dm.sub.A) 

covs.1<-dplyr::select(gm.A, gender_v1_v2 ,AGE_GOLD,, smoker.current.v1.v2)
covs.2<- dplyr::select(gm.A, gender_v1_v2 ,AGE_GOLD,  income,education_v1_v2,, smoker.current.v1.v2)
covs.3<- dplyr::select(gm.A,  gender_v1_v2 ,AGE_GOLD,  income,education_v1_v2, relocation_age, generation ,background_5, smoker.current.v1.v2)


(asthma.ait1.A<- MiRKAT(gm.A$asthma.current ,Ks = K.Ait.A,out_type ='D', X=covs.1, returnKRV=T, returnR2=T))

(asthma.ait2.A<- MiRKAT(gm.A$asthma.current ,Ks = K.Ait.A,out_type ='D', X=covs.2, returnKRV=T, returnR2=T))

(asthma.ait3.A<- MiRKAT(gm.A$asthma.current,Ks = K.Ait.A,out_type ='D', X=covs.3, returnKRV=T, returnR2=T))


K.Ait.O= D2K(dm.sub.O) 
covs.1<-dplyr::select(gm.O, gender_v1_v2 ,AGE_GOLD,, smoker.current.v1.v2)
covs.2<- dplyr::select(gm.O, gender_v1_v2 ,AGE_GOLD,   income,education_v1_v2,, smoker.current.v1.v2)
covs.3<- dplyr::select(gm.O,  gender_v1_v2 ,AGE_GOLD,   income,education_v1_v2, relocation_age, generation , background_5, smoker.current.v1.v2)
gm.O$obese<-ifelse(gm.O$asthma.current.obese=='Obese only', 1, 0)
table(gm.O$obese)

(asthma.ait1.O<- MiRKAT(gm.O$obese ,Ks = K.Ait.O,out_type ='D', X=covs.1, returnKRV=T, returnR2=T))
(asthma.ait2.O<- MiRKAT(gm.O$obese ,Ks = K.Ait.O,out_type ='D', X=covs.2, returnKRV=T, returnR2=T))
(asthma.ait3.O<- MiRKAT(gm.O$obese ,Ks = K.Ait.O,out_type ='D', X=covs.3, returnKRV=T, returnR2=T))

## Output
asthma.mirkat<- data.frame(Model=(c('1','2','3')), 
                           N=rep(c(nrow(gm.dat.current)), each=3),
                           Outcome=rep(c('Current Asthma'), each=3),
                           Covs=(c(paste(names(covs.1), collapse=','),
                                   paste(names(covs.2), collapse=','),
                                   paste(names(covs.3), collapse=','))), 
                           R2.ait=c(asthma.ait1.c$R2, asthma.ait2.c$R2, asthma.ait3.c$R2),
                           p.value.ait=c(asthma.ait1.c$p_values, asthma.ait2.c$p_values, asthma.ait3.c$p_values)
)




covs.1.names<- paste(names(covs.1), collapse=', ')
covs.2.names<- paste(names(covs.2), collapse=', ')
covs.3.names<- paste(names(covs.3), collapse=', ')


mirkat.multi.current<- data.frame(Model=rep(c(1,2,3), each=3), 
                                  N=rep(c(nrow(gm.OA), nrow(gm.A), nrow(gm.O)), 3),
                                  Covs=rep(c( covs.1.names, covs.2.names, covs.3.names), each=3), 
                                  asthma.current.Obese=rep(c('Obese asthma','Asthma only', 'Obese only'), 3), 
                                  R2.ait=c(asthma.ait1.OA$R2, asthma.ait1.A$R2, asthma.ait1.O$R2, 
                                           asthma.ait2.OA$R2, asthma.ait2.A$R2, asthma.ait2.O$R2, 
                                           asthma.ait3.OA$R2, asthma.ait3.A$R2, asthma.ait3.O$R2),
                                  p.value.ait=c( asthma.ait1.OA$p_values, asthma.ait1.A$p_values, asthma.ait1.O$p_values,
                                                 asthma.ait2.OA$p_values, asthma.ait2.A$p_values, asthma.ait2.O$p_values,
                                                 asthma.ait3.OA$p_values, asthma.ait3.A$p_values, asthma.ait3.O$p_values))
                                  
                                  


fwrite(asthma.mirkat,file= 'Mirkat_asthma_0424.csv')
fwrite(mirkat.multi.current,file= 'Mirkat_asthma_multi_0424.csv')



