library(tidyverse)
library(stringr)
library(tigris)
library(lubridate)
library(INLA)
library(data.table)
library(forcats)
library(DescTools)

#scratch_loc = '../../../../net/tmp/tscott1/tuolumne_scratch/'
base_list = readRDS('scratch/descriptive_paper/mods_baseline.RDS')
names(base_list) <- c("All",'Built','Institutional')

coef_df = rbindlist(lapply(seq_along(base_list),function(x) base_list[[x]]$summary.fixed[,c(1,3,5)] 
                               %>% as.data.frame() %>% mutate(Coef = rownames(.),Model = names(base_list)[x])))

coef_hyper = rbindlist(lapply(seq_along(base_list),function(x) base_list[[x]]$summary.hyperpar[,c(1,3,5)] 
                                  %>% as.data.frame() %>% mutate(Coef = rownames(.),Model = names(base_list)[x])))
coef_df <- rbind(coef_df,coef_hyper)

coef_df$CI = paste(formatC(round(coef_df$`0.025quant`,3),digits = 3,format = 'f',flag=0,width=3),formatC(round(coef_df$`0.975quant`,3),digits = 3,format = 'f',flag=0,width=3),sep=', ')
coef_df$Model <- as.factor(coef_df$Model)
coef_df$Model <- fct_relevel(coef_df$Model,'All','Built','Institutional')


library(htmlTable)
coef_df$Coef = as.factor(coef_df$Coef)
coef_df$Coef = fct_inorder(coef_df$Coef)
coef_df$Coef = fct_recode(coef_df$Coef,
                          "ln(total area)" = "Ln_Total_Area"  ,
                          "% wetlands area" = "Wetlands_Prop",
                          "Coastal zone" = "CZMA_County",
                          "ln(total population)" = "Ln_Total_Population" ,
                          "# local governments" =  "Local_Gov_Num" ,
                          ">10% urban area" = "Urban_County" ,
                          "# brownfields"  = "Brownfields_Count",
                          "Air pollution cancer risk (PPM)" = "Tot_Cancer_Risk_PPM" ,
                          "Critical habitat designation" = "Critical_Habitat",
                          "ln(median home value)" = "Ln_Median_Home_Value" ,
                          "% minority (non-white)" = "PERC_MINORITY",
                          "% below poverty level" = "Poverty_Rate",
                          "Precision for county" = "Precision for CFIPS_ID",
                          "Precision for state" =  "Precision for STATE")

file = 'output/descriptive_eis/tableA3.html'

coef_df$Coef = fct_inorder(coef_df$Coef)

require(htmlTable)
t1 = data.table::dcast(coef_dt[order(Coef),-c(1,2,3)],Coef ~ Model,value.vars = 'CI')
t1$Coef = fct_relevel(t1$Coef,levels(coef_df$Coef))
t1 = t1[order(Coef),]
t2 = dcast(data.table(Coef = 'WAIC',sapply(base_list,function(x) round(x$waic$waic,3))),Coef~V2)
names(t2) <- names(t1)
t1 = rbind(t1,t2,use.names = F)

#tab = rbind(spread(coef_df[,-c(1,2,3)],Model,CI),c('WAIC',sapply(base_list,function(x) round(x$waic$waic,3))))
sink(file = 'output/descriptive_eis/tableA3.html')
print(htmlTable(t1),type = 'html',useViewer = F)
sink()


sumdt = mod_list[[1]]$.args$data[,.(N,Built,Institutional,
                            Urban_County ,CZMA_County ,Critical_Habitat ,Ln_Total_Area, 
                            Wetlands_Prop, Ln_Total_Population, Local_Gov_Num, PERC_MINORITY ,Poverty_Rate ,
                            Ln_Median_Home_Value,Brownfields_Count,Tot_Cancer_Risk_PPM)]

sdt = apply(sumdt,2,summary)
sdt = data.table(Coef = colnames(sdt),t(round(sdt,3)))[,c(1,2,4,5,7)]
sink(file = 'output/descriptive_eis/descriptives_table.html')
print(htmlTable(sdt),type = 'html',useViewer = F)
sink()



plot_df = coef_df[!grepl('Precision|Intercept',coef_df$Coef),]
#plot_df$Coef <- as.factor(plot_df$Coef)
# plot_df$Coef = fct_recode(plot_df$Coef,
# "ln(total area)" = "Ln_Total_Area"  ,
# "% wetlands area" = "Wetlands_Prop",
# "Coastal zone" = "bin_CZMA_County1",
# "ln(total population)" = "Ln_Total_Population" ,
# "# local governments" =  "Local_Gov_Num" ,
# ">10% urban area" = "bin_Urban_County1" ,
# "# brownfields"  = "Brownfields_Count",
# "Air pollution cancer risk (PPM)" = "Tot_Cancer_Risk_PPM" ,
# "Critical habitat designation" = "bin_Critical_Habitat1",
# "ln(median home value)" = "Ln_Median_Home_Value" ,
# "% minority (non-white)" = "PERC_MINORITY",
# "% below poverty level" = "Poverty_Rate")
plot_df$Coef = fct_relevel(plot_df$Coef,
                           "ln(total area)",
                           "% wetlands area",
                           "Coastal zone",
                           "ln(total population)",
                           "# local governments",
                           ">10% urban area",
                           "# brownfields",
                           "Air pollution cancer risk (PPM)",
                           "Critical habitat designation",
                           "ln(median home value)",
                           "% minority (non-white)",
                           "% below poverty level")
plot_df$Coef = fct_rev(plot_df$Coef)   

plot_df$Sig = (plot_df$`0.025quant`<0 & plot_df$`0.975quant`>0) + 0
plot_df$Mod_Sig = paste(plot_df$Model,plot_df$Sig)
cols = c("#7fc97f","#beaed4","#fdc086")
library(viridis)
figure7 = ggplot(data = plot_df,
                 aes(y = mean,ymin = `0.025quant`,ymax = `0.975quant`,x = Coef,colour = Model,shape = as.character(Sig)))  +
  geom_hline(aes(yintercept=0),lty = 2, col = 'grey50') +
  geom_errorbar(position = position_dodge(width = 0.9),lwd=1) + 
  geom_point(position = position_dodge(width = 0.9),fill = 'white') + 
  coord_flip() + #scale_color_manual(values = c('')) + 
  scale_color_grey(name = 'Infrastructure')+
#  scale_color_viridis(discrete = T,name = 'Infrastructure',option = 'E')+
#  scale_color_manual(name = 'Infrastructure',values = cols) + 
  theme_minimal() + ggtitle('Predicting county EIS counts for 2013-2019') + 
  scale_shape_manual(values = c(19,21))  + 
  theme(legend.position = c(0.8,0.15),axis.text = element_text(size = 12),
        legend.background = element_rect(fill = alpha('white',0.3)),
        legend.title = element_text(size = 12,hjust = 0.5),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_blank(),text = element_text(family = 'Times'))+
  scale_y_continuous(name = 'Parameter estimate (95% credible interval)')  + 
  guides(shape = FALSE) + 
  NULL

ggsave(figure7,filename = 'output/descriptive_eis/figure7.tiff',dpi = 500, units = 'in',width=6,height=6)







