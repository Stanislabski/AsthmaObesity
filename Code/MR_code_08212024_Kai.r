## Species for the BMI and bacteria species 
rm(list=ls())
#gc()
require(vroom)
require(tidyverse)

load(file="Maggie_asthma_sp_gwas_sig_03092024.Rdata")


## load the BMI summary dataset 

bmi_giant<-vroom("Meta-analysis_Locke_et_al+UKBiobank_2018_UPDATED.txt.gz")


## species 


dmp_Bacteroides_plebeius_sig<-dmp_Bacteroides_plebeius_sig%>%
  mutate(rsID=id,
         Est=beta,
         Est.SE=SE,
         freq=AF_Allele2,
         alleleA=alt,
         alleleB=ref,
         pval=pval,
         exposure="dmp_Bacteroides_plebeius")
dmp_Oxalobacter_formigenes_sig<-dmp_Oxalobacter_formigenes_sig%>%
  mutate(rsID=id,
         Est=beta,
         Est.SE=SE,
         freq=AF_Allele2,
         alleleA=alt,
         alleleB=ref,
         pval=pval,
         exposure="dmp_Oxalobacter_formigenes")
dmp_Pseudoflavonifractor_capillosus_sig<-dmp_Pseudoflavonifractor_capillosus_sig%>%
  mutate(rsID=id,
         Est=beta,
         Est.SE=SE,
         freq=AF_Allele2,
         alleleA=alt,
         alleleB=ref,
         pval=pval,
         exposure="dmp_Pseudoflavonifractor_capillosus")
dmp_Streptococcus_parasanguinis_sig<-dmp_Streptococcus_parasanguinis_sig%>%
  mutate(rsID=id,
         Est=beta,
         Est.SE=SE,
         freq=AF_Allele2,
         alleleA=alt,
         alleleB=ref,
         pval=pval,
         exposure="dmp_Streptococcus_parasanguinis")


## combine dataset yw
colnames(yw_Lactobacillus_salivarius_sig)

# for the alistipes
yw_Alistipes_sig<-yw_Alistipes_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Alistipes")


yw_Acidaminococcus_fermentans_sig<-yw_Acidaminococcus_fermentans_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Acidaminococcus_fermentans")

yw_Bacteroides_plebeius_sig<-yw_Bacteroides_plebeius_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Bacteroides_plebeius")
yw_Coprobacter_secundus_sig<-yw_Coprobacter_secundus_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Coprobacter_secundus")
yw_Fournierella_massiliensis_sig<-yw_Fournierella_massiliensis_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Fournierella_massiliensis")

yw_Intestinimonas_massiliensis_sig<-yw_Intestinimonas_massiliensis_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Intestinimonas_massiliensis")

yw_Lachnoanaerobaculum_saburreum_sig<-yw_Lachnoanaerobaculum_saburreum_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Lachnoanaerobaculum_saburreum")


yw_Lactobacillus_salivarius_sig%>%glimpse()
yw_Lactobacillus_salivarius_sig<-yw_Lactobacillus_salivarius_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Lactobacillus_salivarius")

## combine the speices 

# keep 
keep_names<-intersect(colnames(yw_Lactobacillus_salivarius_sig),
                      colnames(dmp_Bacteroides_plebeius_sig))


gmb_sp_sig<-bind_rows(dmp_Bacteroides_plebeius_sig,
                      dmp_Oxalobacter_formigenes_sig,
                      dmp_Pseudoflavonifractor_capillosus_sig,
                      dmp_Streptococcus_parasanguinis_sig,
                      yw_Alistipes_sig,
                      yw_Acidaminococcus_fermentans_sig,
                      yw_Bacteroides_plebeius_sig,
                      yw_Coprobacter_secundus_sig,
                      yw_Fournierella_massiliensis_sig,
                      yw_Intestinimonas_massiliensis_sig,
                      yw_Lachnoanaerobaculum_saburreum_sig,
                      yw_Lactobacillus_salivarius_sig)

gmb_sp_sig_keep<-gmb_sp_sig%>%dplyr::select(keep_names)


### for the 


sp_hm_names<-unique(gmb_sp_sig_keep$exposure)


# conducting the MR 
require(TwoSampleMR)
require(ieugwasr)
require(tidyverse)
require(readxl)
require(MRPRESSO)


## conducting the MR for species levels



# define the dataset for the a cycle and save
mr_homarize_byspecies_bmi<-list() # for harmonized data
mr_res_byspecies_bmi<-list() # for estimates, heterogenity, and pleiotropy 
mr_snp_byspecies_bmi<-list() # for single snp
mr_loovc_byspecies_bmi<-list() # for the Leave one-out analysis 
mr_plt_byspecies_bmi<-list() # saved for plt data


r2_list<-c(0.01)
kb_list<-c(1000)

pvalue_list<-c(5e-5,5e-6,5e-7,5e-8)




for(a in seq_along(sp_hm_names)){
  
  #**cycle1: by species 
  #**cycle1: by species 
  sp<-sp_hm_names[a]
  
  
  #*** cycel2: by p value 
  
  
  mr_homarize_by_pval<-list() # for harmonized data
  mr_res_by_pval<-list() # for estimates, heterogenity, and pleiotropy 
  mr_snp_by_pval<-list() # for single snp
  mr_loovc_bypval<-list() # for the Leave one-out analysis 
  mr_plt_bypval<-list() # saved for plt data
  
  
  for(b in seq_along(pvalue_list)){
    
    p<-pvalue_list[b]
    
    
    expo_data<-gmb_sp_sig_keep%>%dplyr::rename(pvalue=pval)%>%filter(exposure%in%sp)%>%
      filter(pvalue<pvalue_list[b])
    
    # expo_data<-expo_data[expo_data$Score.pval<pval,]
    
    if(nrow(expo_data)>1){
      
      #expo_data%>%glimpse()
      expo_data_format<-format_data(expo_data,
                                    type="exposure",
                                    phenotype_col = "exposure",
                                    snp_col = "rsID",
                                    beta_col = "Est",
                                    se_col = "Est.SE",
                                    eaf_col = "freq",
                                    effect_allele_col = "alleleA",
                                    other_allele_col = "alleleB",
                                    pval_col = "pvalue",
                                    min_pval = 1e-1000,
                                    log_pval = FALSE)
      #step2: format the outcome 
      
      bmi_keep<-bmi_giant%>%filter(SNP%in%expo_data_format$SNP)
      
      bmi_keep$outcome<-"bmi"
      #bmi_keep$outcome1<-"bmi"
      
      bmi_keep_IV_format<-format_data(bmi_keep,
                                      snps = NULL,
                                      header = TRUE,
                                      type="outcome",
                                      phenotype_col = "outcome",
                                      snp_col = "SNP",
                                      beta_col = "BETA",
                                      se_col = "SE",
                                      eaf_col = "Freq_Tested_Allele_in_HRS",
                                      effect_allele_col = "Tested_Allele",
                                      other_allele_col = "Other_Allele",
                                      pval_col = "P",
                                      #units_col = "units",
                                      #ncase_col = "ncase",
                                      #ncontrol_col = "ncontrol",
                                      samplesize_col = "N",
                                      #gene_col = "nearest_genes",
                                      id_col = "outcome1",
                                      min_pval = 1e-1000,
                                      # z_col = "z",
                                      # info_col = "info",
                                      # chr_col = "chr",
                                      # pos_col = "pos",
                                      log_pval = FALSE)
      
      
      
      
      
      expo_data_format<-expo_data_format%>%mutate(match_id=paste0(SNP,"_",exposure))
      
      #** cycle3: by r2
      mr_homarize_by_r2<-list() # for harmonized data
      mr_res_by_r2<-list() # for estimates, heterogenity, and pleiotropy 
      mr_snp_by_r2<-list() # for single snp
      mr_loovc_byr2<-list() # for the Leave one-out analysis 
      mr_plt_byr2<-list() # saved for plt data
      
      # c=1
      for(c in seq_along(r2_list)){
        
        r2_use<-r2_list[c]
        
        #** for cycle4: kb
        #*
        # replace from here
        
        # saved result by kb 
        mr_homarize_by_kb<-list() # for harmonized data
        mr_res_by_kb<-list() # for estimates, heterogenity, and pleiotropy 
        mr_snp_by_kb<-list() # for single snp
        mr_loovc_bykb<-list() # for the Leave one-out analysis 
        mr_plt_bykb<-list() # saved for plt data
        
        
        for(d in seq_along(kb_list)){
          
          kb_use<-kb_list[d]
          
          rm(expo_data_format_clump)
          
          expo_data_format_clump  <- tryCatch({
            ld_clump( dplyr::tibble(rsid=expo_data_format[,"SNP"], 
                                    pval=expo_data_format[,"pval.exposure"], 
                                    id=expo_data_format[,"exposure"]),
                      clump_kb =  kb_use,# define the kb 
                      clump_r2 = r2_use, # define the r2
                      plink_bin = "path/plink1.9.exe",
                      bfile = "path/AMR" # 1000 G AMR reference panel
            )
          }, error = function(e) {
            message(e)
            return(NULL)
          })
          
          
          
          if(!is.null(expo_data_format_clump)){
            expo_data_format_clump<-expo_data_format_clump%>%mutate(match_id=paste0(rsid,"_",id))
            # matched with the clumped data 
            expo_data_format_clump_use<-expo_data_format%>%filter(match_id%in%expo_data_format_clump$match_id)%>%
              dplyr::select(-match_id)
            
            # step4: harmonize the dataset ==> when the frequency was not availables, we set the action=1
            
            # this is based on TA and we do not have the allefrequency for TA, so we force all pilantropic codes were not included
            
            #if(pval==5e-8){
            bmi_vs_expo_harmonize<-harmonise_data(exposure_dat = expo_data_format_clump_use,
                                                  outcome_dat =bmi_keep_IV_format,action = 2)
            
            if(nrow(bmi_vs_expo_harmonize[bmi_vs_expo_harmonize$mr_keep=="TRUE",])>0){
              bmi_vs_expo_harmonize_save<-bmi_vs_expo_harmonize
              #bmi_vs_expo_harmonize_save
              bmi_vs_expo_harmonize_save$kb<-kb_use
              bmi_vs_expo_harmonize_save$r2<-r2_use
              bmi_vs_expo_harmonize_save$pthreshold<-pvalue_list[b]
              
              
              
              # step5: perform the MR 
              #> estimate 1
              mr_res<-mr(bmi_vs_expo_harmonize, method_list = c("mr_wald_ratio", "mr_ivw", #"mr_raps",
                                                                "mr_egger_regression",
                                                                "mr_weighted_median"))
              
              mr_res<-as.data.frame(mr_res)
              mr_res$type<-"estimate"
              
              #source("mr_modified.R")
              
              
              if(unique(mr_res$nsnp)>1){
                
                mr_res_modified<-mr(bmi_vs_expo_harmonize, method_list = c("mr_raps"))
                
                mr_res_modified<-as.data.frame(mr_res_modified)
                mr_res_modified$type<-"estimate"
              }else{
                mr_res_modified<-NULL
              }
              
              
              # for mr_presso 
              
              
              
              presso  <- tryCatch({
                rd<-mr_presso(BetaOutcome = "beta.outcome", 
                              BetaExposure = "beta.exposure", 
                              SdOutcome = "se.outcome", 
                              SdExposure = "se.exposure", 
                              OUTLIERtest = TRUE, 
                              DISTORTIONtest = TRUE, 
                              data =bmi_vs_expo_harmonize,
                              NbDistribution = 1000,  
                              SignifThreshold = 0.05)
                as.data.frame(rd$`Main MR results`)
                #return(rd_save)
              }, error = function(e) {
                message(e)
                return(NULL)
              })
              
              
              #> heterogenity statistics 
              
              if(unique(mr_res$nsnp)>1){
                
                mr_hetero<-mr_heterogeneity(bmi_vs_expo_harmonize,
                                            method_list = c( "mr_ivw", #"mr_raps",
                                                             "mr_egger_regression"))
                mr_hetero<-as.data.frame(mr_hetero)
                mr_hetero$type<-"heterogenity statistics"
                
                mr_hetero$kb<-kb_use
                mr_hetero$r2<-r2_use
                mr_hetero$pthreshold<-pvalue_list[b]
                mr_hetero$exposure<-sp
              }else{
                mr_hetero<-NULL
              }
              
              #> horizontal pleiotropy 
              #> 
              if(unique(mr_res$nsnp)>1){
                
                mr_pleiotropy<-mr_pleiotropy_test(bmi_vs_expo_harmonize)
                mr_pleiotropy<-as.data.frame(mr_pleiotropy)
                mr_pleiotropy$type<-"pleiotropy"
                
                mr_pleiotropy$kb<-kb_use
                mr_pleiotropy$r2<-r2_use
                mr_pleiotropy$pthreshold<-pvalue_list[b]
                mr_pleiotropy$exposure<-sp
              }else{
                mr_pleiotropy<-NULL
              }
              
              # save the estimates, hetero, and pleiotropy 
              mr_results<-bind_rows(mr_res,
                                    mr_hetero,
                                    mr_pleiotropy,
                                    presso,
                                    mr_res_modified)
              mr_results$kb<-kb_use
              mr_results$r2<-r2_use
              mr_results$pthreshold<-pvalue_list[b]
              mr_results$exposure<-sp
              
              
              #> save the single SNP 
              mr_snp<-mr_singlesnp(bmi_vs_expo_harmonize)
              mr_snp$kb<-kb_use
              mr_snp$r2<-r2_use
              mr_snp$pthreshold<-pvalue_list[b]
              mr_snp$exposure<-sp
              
              #> for the MR lvoot due to multiple SNP, we need to firt determine wheather the 
              #> number of selected SNPs were > 2
              #
              
              #> condcut the Loov and output--> 
              
              if(unique(mr_res$nsnp)>1){
                
                res_loo <- mr_leaveoneout(bmi_vs_expo_harmonize)
                mr_loovc<-res_loo
                
                mr_loovc$kb<-kb_use
                mr_loovc$r2<-r2_use
                mr_loovc$pthreshold<-pvalue_list[b]
                mr_loovc$exposure<-sp
              }else{
                mr_loovc<-NULL
              }
              
              #> save the plots
              if(unique(mr_res$nsnp)>1){
                mr_plt<-mr_scatter_plot(mr_res, bmi_vs_expo_harmonize)
                names(mr_plt)<-sp
                mr_plt_dt<-mr_plt[[1]]
                mr_plt_dt<-mr_plt_dt$data
              }else(
                mr_plt_dt<-NULL
              )
            } else{
              mr_loovc<-NULL
              mr_plt_dt<-NULL
              mr_snp<-NULL
              mr_results<-NULL
              bmi_vs_expo_harmonize_save<-NULL
            }
            
            # SAVE The above test
            mr_loovc_bykb[[d]]<-mr_loovc
            mr_plt_bykb[[d]]<-mr_plt_dt
            
            mr_snp_by_kb[[d]]<-mr_snp
            
            mr_res_by_kb[[d]]<-mr_results
            mr_homarize_by_kb[[d]]<-bmi_vs_expo_harmonize_save
            
          } else{
            mr_homarize_by_kb[[d]]<-NULL
            mr_res_by_kb[[d]]<-NULL
            mr_snp_by_kb[[d]]<-NULL
            mr_loovc_bykb[[d]]<-NULL
            mr_plt_bykb[[d]]<-NULL
          }
          
          print(paste0("This is ", a, " th species with", b," th p", " with ", c, "th r2", " ",d," th kb" ))
          gc()
        }
        
        
        # save for d cycle 
        mr_homarize_by_kb_save<-do.call(bind_rows,mr_homarize_by_kb)
        mr_res_by_kb_save<-do.call(bind_rows,    mr_res_by_kb)
        mr_snp_by_kb_save<-do.call(bind_rows,mr_snp_by_kb)
        mr_loovc_bykb_save<-do.call(bind_rows,mr_loovc_bykb)
        mr_plt_bykb_save<-do.call(bind_rows,mr_plt_bykb)
        
        mr_homarize_by_r2[[c]]<-mr_homarize_by_kb_save
        mr_res_by_r2[[c]]<-mr_res_by_kb_save# for estimates, heterogenity, and pleiotropy 
        mr_snp_by_r2[[c]]<-mr_snp_by_kb_save # for single snp
        mr_loovc_byr2[[c]]<-mr_loovc_bykb_save# for the Leave one-out analysis 
        mr_plt_byr2[[c]]<-mr_plt_bykb_save# saved for plt data
      } # combine the cycle c
      
      mr_homarize_by_r2_save<-do.call(bind_rows,mr_homarize_by_r2)
      mr_res_by_r2_save<-do.call(bind_rows,    mr_res_by_r2)
      mr_snp_by_r2_save<-do.call(bind_rows,mr_snp_by_r2)
      mr_loovc_byr2_save<-do.call(bind_rows,mr_loovc_byr2)
      mr_plt_byr2_save<-do.call(bind_rows,mr_plt_byr2)
      # save for b cycle 
      mr_homarize_by_pval[[b]]<-mr_homarize_by_r2_save
      mr_res_by_pval[[b]]<-mr_res_by_r2_save# for estimates, heterogenity, and pleiotropy 
      mr_snp_by_pval[[b]]<-mr_snp_by_r2_save # for single snp
      mr_loovc_bypval[[b]]<-mr_loovc_byr2_save# for the Leave one-out analysis 
      mr_plt_bypval[[b]]<-mr_plt_byr2_save# saved for plt data
      
    }
    
  } # combine the cycle b
  
  
  mr_homarize_by_pval_save<-do.call(bind_rows,mr_homarize_by_pval)
  mr_res_by_pval_save<-do.call(bind_rows,   mr_res_by_pval)
  mr_snp_by_pval_save<-do.call(bind_rows,mr_snp_by_pval)
  mr_loovc_bypval_save<-do.call(bind_rows,mr_loovc_bypval)
  mr_plt_bypval_save<-do.call(bind_rows,mr_plt_bypval)
  
  
  mr_homarize_byspecies_bmi[[a]]<-mr_homarize_by_pval_save
  mr_res_byspecies_bmi[[a]]<-mr_res_by_pval_save# for estimates, heterogenity, and pleiotropy 
  mr_snp_byspecies_bmi[[a]]<-mr_snp_by_pval_save # for single snp
  mr_loovc_byspecies_bmi[[a]]<-mr_loovc_bypval_save# for the Leave one-out analysis 
  mr_plt_byspecies_bmi[[a]]<-mr_plt_bypval_save# saved for plt data
  
}

## This is for the diagram EUR

mr_homarize_byspecies_bmi_save<-do.call(bind_rows,mr_homarize_byspecies_bmi)
mr_res_byspecies_bmi_save<-do.call(bind_rows,    mr_res_byspecies_bmi)
mr_snp_byspecies_bmi_save<-do.call(bind_rows,mr_snp_byspecies_bmi)
mr_loovc_byspecies_bmi_save<-do.call(bind_rows,mr_loovc_byspecies_bmi)
mr_plt_byspecies_bmi_save<-do.call(bind_rows,mr_plt_byspecies_bmi)

mr_homarize_byspecies_bmi_save$outcome_data<-"GIANT"
mr_res_byspecies_bmi_save$outcome_data<-"GIANT"
mr_snp_byspecies_bmi_save$outcome_data<-"GIANT"
mr_loovc_byspecies_bmi_save$outcome_data<-"GIANT"
mr_plt_byspecies_bmi_save$outcome_data<-"GIANT"




###
# Below are for the astham vs bacteria species: section 2
####


load(file="Maggie_asthma_sp_gwas_sig_03092024.Rdata")


## harmonized dataset from the exposure 

colnames(dmp_Bacteroides_plebeius_sig)
dmp_Bacteroides_plebeius_sig%>%glimpse()
yw_Acidaminococcus_fermentans_sig%>%glimpse()


##
# phenotype_col = "exposure",
# snp_col = "rsID",
# beta_col = "Est",
# se_col = "Est.SE",
# eaf_col = "freq",
# effect_allele_col = "alleleA",
# other_allele_col = "alleleB",
# pval_col = "Score.pval",
# min_pval = 1e-1000,

dmp_Bacteroides_plebeius_sig<-dmp_Bacteroides_plebeius_sig%>%
  mutate(rsID=id,
         Est=beta,
         Est.SE=SE,
         freq=AF_Allele2,
         alleleA=alt,
         alleleB=ref,
         pval=pval,
         exposure="dmp_Bacteroides_plebeius")
dmp_Oxalobacter_formigenes_sig<-dmp_Oxalobacter_formigenes_sig%>%
  mutate(rsID=id,
         Est=beta,
         Est.SE=SE,
         freq=AF_Allele2,
         alleleA=alt,
         alleleB=ref,
         pval=pval,
         exposure="dmp_Oxalobacter_formigenes")
dmp_Pseudoflavonifractor_capillosus_sig<-dmp_Pseudoflavonifractor_capillosus_sig%>%
  mutate(rsID=id,
         Est=beta,
         Est.SE=SE,
         freq=AF_Allele2,
         alleleA=alt,
         alleleB=ref,
         pval=pval,
         exposure="dmp_Pseudoflavonifractor_capillosus")
dmp_Streptococcus_parasanguinis_sig<-dmp_Streptococcus_parasanguinis_sig%>%
  mutate(rsID=id,
         Est=beta,
         Est.SE=SE,
         freq=AF_Allele2,
         alleleA=alt,
         alleleB=ref,
         pval=pval,
         exposure="dmp_Streptococcus_parasanguinis")


## combine dataset yw
colnames(yw_Lactobacillus_salivarius_sig)

# for the alistipes
yw_Alistipes_sig<-yw_Alistipes_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Alistipes")


yw_Acidaminococcus_fermentans_sig<-yw_Acidaminococcus_fermentans_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Acidaminococcus_fermentans")

yw_Bacteroides_plebeius_sig<-yw_Bacteroides_plebeius_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Bacteroides_plebeius")
yw_Coprobacter_secundus_sig<-yw_Coprobacter_secundus_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Coprobacter_secundus")
yw_Fournierella_massiliensis_sig<-yw_Fournierella_massiliensis_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Fournierella_massiliensis")

yw_Intestinimonas_massiliensis_sig<-yw_Intestinimonas_massiliensis_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Intestinimonas_massiliensis")

yw_Lachnoanaerobaculum_saburreum_sig<-yw_Lachnoanaerobaculum_saburreum_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Lachnoanaerobaculum_saburreum")


yw_Lactobacillus_salivarius_sig%>%glimpse()
yw_Lactobacillus_salivarius_sig<-yw_Lactobacillus_salivarius_sig%>%
  mutate(rsID=hm_rsid,
         Est=hm_beta,
         Est.SE=standard_error,
         freq=effect_allele_frequency,
         alleleA=hm_effect_allele,
         alleleB=hm_other_allele,
         pval=p_value,
         exposure="yw_Lactobacillus_salivarius")

## combine the speices 

# keep 
keep_names<-intersect(colnames(yw_Lactobacillus_salivarius_sig),
                      colnames(dmp_Bacteroides_plebeius_sig))


gmb_sp_sig<-bind_rows(dmp_Bacteroides_plebeius_sig,
                      dmp_Oxalobacter_formigenes_sig,
                      dmp_Pseudoflavonifractor_capillosus_sig,
                      dmp_Streptococcus_parasanguinis_sig,
                      yw_Alistipes_sig,
                      yw_Acidaminococcus_fermentans_sig,
                      yw_Bacteroides_plebeius_sig,
                      yw_Coprobacter_secundus_sig,
                      yw_Fournierella_massiliensis_sig,
                      yw_Intestinimonas_massiliensis_sig,
                      yw_Lachnoanaerobaculum_saburreum_sig,
                      yw_Lactobacillus_salivarius_sig)

gmb_sp_sig_keep<-gmb_sp_sig%>%dplyr::select(keep_names)

## for the harmonized names of species 

sp_hm_names<-unique(gmb_sp_sig_keep$exposure)

# for the asthma 



# conducting the MR 
require(TwoSampleMR)
require(ieugwasr)
require(tidyverse)
require(readxl)
require(MRPRESSO)


## conducting the MR for species levels



# define the dataset for the a cycle and save
mr_homarize_byspecies<-list() # for harmonized data
mr_res_byspecies<-list() # for estimates, heterogenity, and pleiotropy 
mr_snp_byspecies<-list() # for single snp
mr_loovc_byspecies<-list() # for the Leave one-out analysis 
mr_plt_byspecies<-list() # saved for plt data


r2_list<-c(0.01)
kb_list<-c(1000)

pvalue_list<-c(5e-5,5e-6,5e-7,5e-8)




for(a in seq_along(sp_hm_names)){
  
  #**cycle1: by species 
  #**cycle1: by species 
  sp<-sp_hm_names[a]
  
  
  #*** cycel2: by p value 
  
  
  mr_homarize_by_pval<-list() # for harmonized data
  mr_res_by_pval<-list() # for estimates, heterogenity, and pleiotropy 
  mr_snp_by_pval<-list() # for single snp
  mr_loovc_bypval<-list() # for the Leave one-out analysis 
  mr_plt_bypval<-list() # saved for plt data
  
  
  for(b in seq_along(pvalue_list)){
    
    p<-pvalue_list[b]
    
    
    expo_data<-gmb_sp_sig_keep%>%dplyr::rename(pvalue=pval)%>%filter(exposure%in%sp)%>%
      filter(pvalue<pvalue_list[b])
    
    # expo_data<-expo_data[expo_data$Score.pval<pval,]
    
    if(nrow(expo_data)>1){
      
      #expo_data%>%glimpse()
      expo_data_format<-format_data(expo_data,
                                    type="exposure",
                                    phenotype_col = "exposure",
                                    snp_col = "rsID",
                                    beta_col = "Est",
                                    se_col = "Est.SE",
                                    eaf_col = "freq",
                                    effect_allele_col = "alleleA",
                                    other_allele_col = "alleleB",
                                    pval_col = "pvalue",
                                    min_pval = 1e-1000,
                                    log_pval = FALSE)
      #step2: format the outcome 
      
      asthma_keep<-asthma%>%filter(rsid%in%expo_data_format$SNP)
      
      asthma_keep$outcome<-"asthma"
      #asthma_keep$outcome1<-"asthma"
      
      asthma_keep_IV_format<-format_data(asthma_keep,
                                         snps = NULL,
                                         header = TRUE,
                                         type="outcome",
                                         phenotype_col = "outcome",
                                         snp_col = "rsid",
                                         beta_col = "inv_var_meta_beta",
                                         se_col = "inv_var_meta_sebeta",
                                         eaf_col = "all_meta_AF",
                                         effect_allele_col = "ALT",
                                         other_allele_col = "REF",
                                         pval_col = "inv_var_meta_p",
                                         #units_col = "units",
                                         #ncase_col = "ncase",
                                         #ncontrol_col = "ncontrol",
                                         #samplesize_col = "N",
                                         #gene_col = "nearest_genes",
                                         id_col = "outcome1",
                                         min_pval = 1e-1000,
                                         # z_col = "z",
                                         # info_col = "info",
                                         # chr_col = "chr",
                                         # pos_col = "pos",
                                         log_pval = FALSE)
      
      
      
      
      
      expo_data_format<-expo_data_format%>%mutate(match_id=paste0(SNP,"_",exposure))
      
      #** cycle3: by r2
      mr_homarize_by_r2<-list() # for harmonized data
      mr_res_by_r2<-list() # for estimates, heterogenity, and pleiotropy 
      mr_snp_by_r2<-list() # for single snp
      mr_loovc_byr2<-list() # for the Leave one-out analysis 
      mr_plt_byr2<-list() # saved for plt data
      
      # c=1
      for(c in seq_along(r2_list)){
        
        r2_use<-r2_list[c]
        
        #** for cycle4: kb
        #*
        # replace from here
        
        # saved result by kb 
        mr_homarize_by_kb<-list() # for harmonized data
        mr_res_by_kb<-list() # for estimates, heterogenity, and pleiotropy 
        mr_snp_by_kb<-list() # for single snp
        mr_loovc_bykb<-list() # for the Leave one-out analysis 
        mr_plt_bykb<-list() # saved for plt data
        
        
        for(d in seq_along(kb_list)){
          
          kb_use<-kb_list[d]
          
          rm(expo_data_format_clump)
          
          expo_data_format_clump  <- tryCatch({
            ld_clump( dplyr::tibble(rsid=expo_data_format[,"SNP"], 
                                    pval=expo_data_format[,"pval.exposure"], 
                                    id=expo_data_format[,"exposure"]),
                      clump_kb =  kb_use,# define the kb 
                      clump_r2 = r2_use, # define the r2
                      plink_bin = "path/plink1.9.exe",
                      bfile = "path/AMR"
            )
          }, error = function(e) {
            message(e)
            return(NULL)
          })
          
          
          
          if(!is.null(expo_data_format_clump)){
            expo_data_format_clump<-expo_data_format_clump%>%mutate(match_id=paste0(rsid,"_",id))
            # matched with the clumped data 
            expo_data_format_clump_use<-expo_data_format%>%filter(match_id%in%expo_data_format_clump$match_id)%>%
              dplyr::select(-match_id)
            
            # step4: harmonize the dataset ==> when the frequency was not availables, we set the action=1
            
            # this is based on TA and we do not have the allefrequency for TA, so we force all pilantropic codes were not included
            
            #if(pval==5e-8){
            asthma_vs_expo_harmonize<-harmonise_data(exposure_dat = expo_data_format_clump_use,
                                                     outcome_dat =asthma_keep_IV_format,action = 2)
            
            if(nrow(asthma_vs_expo_harmonize[asthma_vs_expo_harmonize$mr_keep=="TRUE",])>0){
              asthma_vs_expo_harmonize_save<-asthma_vs_expo_harmonize
              #asthma_vs_expo_harmonize_save
              asthma_vs_expo_harmonize_save$kb<-kb_use
              asthma_vs_expo_harmonize_save$r2<-r2_use
              asthma_vs_expo_harmonize_save$pthreshold<-pvalue_list[b]
              
              
              
              # step5: perform the MR 
              #> estimate 1
              mr_res<-mr(asthma_vs_expo_harmonize, method_list = c("mr_wald_ratio", "mr_ivw", #"mr_raps",
                                                                   "mr_egger_regression",
                                                                   "mr_weighted_median"))
              
              mr_res<-as.data.frame(mr_res)
              mr_res$type<-"estimate"
              
              #source("mr_modified.R")
              
              
              if(unique(mr_res$nsnp)>1){
                
                mr_res_modified<-mr(asthma_vs_expo_harmonize, method_list = c("mr_raps"))
                
                mr_res_modified<-as.data.frame(mr_res_modified)
                mr_res_modified$type<-"estimate"
              }else{
                mr_res_modified<-NULL
              }
              
              
              # for mr_presso 
              
              
              
              presso  <- tryCatch({
                rd<-mr_presso(BetaOutcome = "beta.outcome", 
                              BetaExposure = "beta.exposure", 
                              SdOutcome = "se.outcome", 
                              SdExposure = "se.exposure", 
                              OUTLIERtest = TRUE, 
                              DISTORTIONtest = TRUE, 
                              data =asthma_vs_expo_harmonize,
                              NbDistribution = 1000,  
                              SignifThreshold = 0.05)
                as.data.frame(rd$`Main MR results`)
                #return(rd_save)
              }, error = function(e) {
                message(e)
                return(NULL)
              })
              
              
              #> heterogenity statistics 
              
              if(unique(mr_res$nsnp)>1){
                
                mr_hetero<-mr_heterogeneity(asthma_vs_expo_harmonize,
                                            method_list = c( "mr_ivw", #"mr_raps",
                                                             "mr_egger_regression"))
                mr_hetero<-as.data.frame(mr_hetero)
                mr_hetero$type<-"heterogenity statistics"
                
                mr_hetero$kb<-kb_use
                mr_hetero$r2<-r2_use
                mr_hetero$pthreshold<-pvalue_list[b]
                mr_hetero$exposure<-sp
              }else{
                mr_hetero<-NULL
              }
              
              #> horizontal pleiotropy 
              #> 
              if(unique(mr_res$nsnp)>1){
                
                mr_pleiotropy<-mr_pleiotropy_test(asthma_vs_expo_harmonize)
                mr_pleiotropy<-as.data.frame(mr_pleiotropy)
                mr_pleiotropy$type<-"pleiotropy"
                
                mr_pleiotropy$kb<-kb_use
                mr_pleiotropy$r2<-r2_use
                mr_pleiotropy$pthreshold<-pvalue_list[b]
                mr_pleiotropy$exposure<-sp
              }else{
                mr_pleiotropy<-NULL
              }
              
              # save the estimates, hetero, and pleiotropy 
              mr_results<-bind_rows(mr_res,
                                    mr_hetero,
                                    mr_pleiotropy,
                                    presso,
                                    mr_res_modified)
              mr_results$kb<-kb_use
              mr_results$r2<-r2_use
              mr_results$pthreshold<-pvalue_list[b]
              mr_results$exposure<-sp
              
              
              #> save the single SNP 
              mr_snp<-mr_singlesnp(asthma_vs_expo_harmonize)
              mr_snp$kb<-kb_use
              mr_snp$r2<-r2_use
              mr_snp$pthreshold<-pvalue_list[b]
              mr_snp$exposure<-sp
              
              #> for the MR lvoot due to multiple SNP, we need to firt determine wheather the 
              #> number of selected SNPs were > 2
              #
              
              #> condcut the Loov and output--> 
              
              if(unique(mr_res$nsnp)>1){
                
                res_loo <- mr_leaveoneout(asthma_vs_expo_harmonize)
                mr_loovc<-res_loo
                
                mr_loovc$kb<-kb_use
                mr_loovc$r2<-r2_use
                mr_loovc$pthreshold<-pvalue_list[b]
                mr_loovc$exposure<-sp
              }else{
                mr_loovc<-NULL
              }
              
              #> save the plots
              if(unique(mr_res$nsnp)>1){
                mr_plt<-mr_scatter_plot(mr_res, asthma_vs_expo_harmonize)
                names(mr_plt)<-sp
                mr_plt_dt<-mr_plt[[1]]
                mr_plt_dt<-mr_plt_dt$data
              }else(
                mr_plt_dt<-NULL
              )
            } else{
              mr_loovc<-NULL
              mr_plt_dt<-NULL
              mr_snp<-NULL
              mr_results<-NULL
              asthma_vs_expo_harmonize_save<-NULL
            }
            
            # SAVE The above test
            mr_loovc_bykb[[d]]<-mr_loovc
            mr_plt_bykb[[d]]<-mr_plt_dt
            
            mr_snp_by_kb[[d]]<-mr_snp
            
            mr_res_by_kb[[d]]<-mr_results
            mr_homarize_by_kb[[d]]<-asthma_vs_expo_harmonize_save
            
          } else{
            mr_homarize_by_kb[[d]]<-NULL
            mr_res_by_kb[[d]]<-NULL
            mr_snp_by_kb[[d]]<-NULL
            mr_loovc_bykb[[d]]<-NULL
            mr_plt_bykb[[d]]<-NULL
          }
          
          print(paste0("This is ", a, " th species with", b," th p", " with ", c, "th r2", " ",d," th kb" ))
          gc()
        }
        
        
        # save for d cycle 
        mr_homarize_by_kb_save<-do.call(bind_rows,mr_homarize_by_kb)
        mr_res_by_kb_save<-do.call(bind_rows,    mr_res_by_kb)
        mr_snp_by_kb_save<-do.call(bind_rows,mr_snp_by_kb)
        mr_loovc_bykb_save<-do.call(bind_rows,mr_loovc_bykb)
        mr_plt_bykb_save<-do.call(bind_rows,mr_plt_bykb)
        
        mr_homarize_by_r2[[c]]<-mr_homarize_by_kb_save
        mr_res_by_r2[[c]]<-mr_res_by_kb_save# for estimates, heterogenity, and pleiotropy 
        mr_snp_by_r2[[c]]<-mr_snp_by_kb_save # for single snp
        mr_loovc_byr2[[c]]<-mr_loovc_bykb_save# for the Leave one-out analysis 
        mr_plt_byr2[[c]]<-mr_plt_bykb_save# saved for plt data
      } # combine the cycle c
      
      mr_homarize_by_r2_save<-do.call(bind_rows,mr_homarize_by_r2)
      mr_res_by_r2_save<-do.call(bind_rows,    mr_res_by_r2)
      mr_snp_by_r2_save<-do.call(bind_rows,mr_snp_by_r2)
      mr_loovc_byr2_save<-do.call(bind_rows,mr_loovc_byr2)
      mr_plt_byr2_save<-do.call(bind_rows,mr_plt_byr2)
      # save for b cycle 
      mr_homarize_by_pval[[b]]<-mr_homarize_by_r2_save
      mr_res_by_pval[[b]]<-mr_res_by_r2_save# for estimates, heterogenity, and pleiotropy 
      mr_snp_by_pval[[b]]<-mr_snp_by_r2_save # for single snp
      mr_loovc_bypval[[b]]<-mr_loovc_byr2_save# for the Leave one-out analysis 
      mr_plt_bypval[[b]]<-mr_plt_byr2_save# saved for plt data
      
    }
    
  } # combine the cycle b
  
  
  mr_homarize_by_pval_save<-do.call(bind_rows,mr_homarize_by_pval)
  mr_res_by_pval_save<-do.call(bind_rows,   mr_res_by_pval)
  mr_snp_by_pval_save<-do.call(bind_rows,mr_snp_by_pval)
  mr_loovc_bypval_save<-do.call(bind_rows,mr_loovc_bypval)
  mr_plt_bypval_save<-do.call(bind_rows,mr_plt_bypval)
  
  
  mr_homarize_byspecies[[a]]<-mr_homarize_by_pval_save
  mr_res_byspecies[[a]]<-mr_res_by_pval_save# for estimates, heterogenity, and pleiotropy 
  mr_snp_byspecies[[a]]<-mr_snp_by_pval_save # for single snp
  mr_loovc_byspecies[[a]]<-mr_loovc_bypval_save# for the Leave one-out analysis 
  mr_plt_byspecies[[a]]<-mr_plt_bypval_save# saved for plt data
  
}

## This is for the diagram EUR

mr_homarize_byspecies_save<-do.call(bind_rows,mr_homarize_byspecies)
mr_res_byspecies_save<-do.call(bind_rows,    mr_res_byspecies)
mr_snp_byspecies_save<-do.call(bind_rows,mr_snp_byspecies)
mr_loovc_byspecies_save<-do.call(bind_rows,mr_loovc_byspecies)
mr_plt_byspecies_save<-do.call(bind_rows,mr_plt_byspecies)
