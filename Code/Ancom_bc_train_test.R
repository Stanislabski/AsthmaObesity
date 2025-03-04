library(data.table)
library(dplyr)
library(tidyverse)
library(tibble)
library(tidyr)
# BiocManager::install("ANCOMBC")
library(ANCOMBC)
library(phyloseq)
library(microbiome)  


## ANCOM-BC

#This is a method from Shemal Pedada.  It is bias-corrected ANCOM.  

load( 'sp_phlyo.RData')
load( file='BDC-SOL-GOLD-0424.RData')

#######################

## Multiple test corrections
#The default (and recommended ) adjustment method for multiple comparisons in ANCOM-BC is Holm.  This is apparently more conservative than FDR - though less than Bonferroni.  


out_sp = ancombc(data = sp_data_train, p_adj_method='holm',formula="asthma.current+obese+gender_v1_v2+AGE_GOLD+income+education+ relocation_age+ background_5+generation+smoker.current.v1.v2+AHEI2010")
out_sp_int = ancombc(data = sp_data_train, p_adj_method='holm',formula="asthma.current+obese+asthma.current*obese+gender_v1_v2+AGE_GOLD+income+education+ relocation_age+ background_5+generation+smoker.current.v1.v2+AHEI2010")



save(out_sp,out_sp_int, file='ancom_objects_sp_holm_train.RData')

#######################

## Testing set of data
## Run ancom BC on testing data

load( file='sp_phlyo_test.RData')

sp.sub.As<- subset_samples(sp_data_test, asthma.current.obese %in%c('Asthma only','Obese asthma'))
sp.sub.sub<- subset_samples(sp_data_test, asthma.current.obese %in%c('Obese only','Asthma only'))
sp.sub.Ob<- subset_samples(sp_data_test, asthma.current.obese %in% c('Obese asthma','Obese only'))

out_sp_test = ancombc(data = sp_data_test, p_adj_method='holm',formula="asthma.current+obese+gender_v1_v2+AGE_GOLD+income+education+ relocation_age+ background_5+generation+smoker.current.v1.v2+AHEI2010")


out_sp_test.As = ancombc(data = sp.sub.As, p_adj_method='holm',formula="nonobese+gender_v1_v2+AGE_GOLD+income+education+ relocation_age+ background_5+generation+smoker.current.v1.v2+AHEI2010")
out_sp_test.Ob = ancombc2(data = sp.sub.Ob,p_adj_method='holm', fix_formula="asthma.current+gender_v1_v2+AGE_GOLD+income+education+
                       relocation_age+ background_5+generation+smoker.current.v1.v2+AHEI2010")
out_sp_test.sub = ancombc2(data = sp.sub.sub,p_adj_method='holm', fix_formula="asthma.current+gender_v1_v2+AGE_GOLD+income+education+
                       relocation_age+ background_5+generation+smoker.current.v1.v2+AHEI2010")



save(out_sp_test,out_sp_test.sub,out_sp_test.As,out_sp_test.Ob, file='ancom_objects_sp_holm_test.RData')

