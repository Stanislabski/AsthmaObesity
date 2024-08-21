
library(phyloseq)
library(biomformat)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(data.table)
library(gridExtra)
library(cowplot)
library(tibble)
library(ecodist)

library(RColorBrewer)


########################## 


load( 'Aitchison.dm.RData')
load('species.data.RData')
load( 'BDC-SOL-GOLD-0424.RData')

########################## 


gold.imputed <- gold %>%
  dplyr::filter(is.na(asthma.current )==F) %>%
  dplyr::filter(goldID %in% row.names(clr.sp))

clr.sp.sub<- clr.sp %>%
  dplyr::filter(row.names(clr.sp) %in% gold.imputed$goldID)

table(gold.imputed$goldID==row.names(clr.sp.sub))
table(gold.imputed$asthma.status)


########################## Distance matrix & PCOA

dm.df<- data.frame(as.matrix(dm))

euclidean_sp_dist <- dm.df %>%
  dplyr::select(gold.imputed$goldID) %>%
  dplyr::filter(row.names(dm.df) %in% gold.imputed$goldID)


euclidean_sp_pcoa <- ecodist::pco(as.dist(euclidean_sp_dist))
euclidean_sp_pcoa_df <- data.frame(
  pcoa1 = euclidean_sp_pcoa$vectors[,1], 
  pcoa2 = euclidean_sp_pcoa$vectors[,2])

########################## plot




euclidean_asthma_pcoa_df <- cbind(euclidean_sp_pcoa_df,
                                  asthma.status = gold.imputed$asthma.current.obese) %>%
  arrange((asthma.status))%>%
  mutate(asthma.status=factor(asthma.status, lev=c('Neither', "Obese only","Obese asthma", "Asthma only"  ), labels=c( 'Non-obese non-asthmatic', "Non-asthmatic obesity", "Obese asthma" ,"Non-obese asthma" )))%>%
  arrange(asthma.status)
euclidean_asthma_pcoa_plot <- ggplot(data = euclidean_asthma_pcoa_df, 
                                     aes(x=pcoa1, y=pcoa2)) +
  geom_point(size=.75, aes(col= asthma.status)) +
  scale_color_manual(values=c('#999999','#67a9cf', '#b2182b', '#fb9a99'), name='', guide=guide_legend(reverse=T))+
  labs(x = "PC1",
       y = "PC2",
       title = "") +theme_bw()+
  theme(axis.text=element_blank(), 
        axis.title=element_text(size=19),
        legend.title = element_text(size=15),legend.text = element_text(size=14),
        axis.ticks=element_blank()) # makes titles smaller
euclidean_asthma_pcoa_plot




