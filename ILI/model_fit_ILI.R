library(viridis)
library(scales)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(ggpubr)
setwd('/Users/yifanwang/Desktop/materials/HKU_RA/imprinting/')
load('ILI_new/CIs_1.RData')
load('ILI_new/model_fits_add_Vac2.RData')
# source('program/population_likelihood.R')
plot1 = 'ILI_new/aHAGroupfit3.pdf'
plot2 = 'ILI_new/aHASubfit_1.pdf'
plot3 = 'ILI_new/aNAfit.tiff'
plot4 = 'ILI_new/aNonefit.tiff'
plot5 = 'ILI_new/aFig1.tiff'
plot6 = 'ILI_new/vaccination_coverage.pdf'

sum_pp<-function(data){
  result=as.data.frame(rownames(data))
  colnames(result)<-'Year'
  for (n in (1:nrow(data))){
    temp=data[n,]
    index=min(which(temp>0))
    year=2020-index
    result$`<5`[result$Year==year]<-sum(temp[index:(index+3)])
    result$`5-24`[result$Year==year]<-sum(temp[(index+4):(index+23)])
    result$`25-44`[result$Year==year]<-sum(temp[(index+24):(index+43)])
    result$`45-64`[result$Year==year]<-sum(temp[(index+44):(index+64)])
    result$`>65`[result$Year==year]<-sum(temp[(index+65):107])
  }
  return(result[-1])
}
plotmod1 = function(pars, pro.H1 = 1, pro.H3 = 1,wVac=vac2){
  
  ## Parse paramter inputs ##
  rPro.H1 = ifelse(is.na(pars['rPro.H1']), 1, pars['rPro.H1'])# Relative risk given imprinting protection
  rPro.H3 = ifelse(is.na(pars['rPro.H3']), 1, pars['rPro.H3'])# Relative risk given imprinting protection
  b = 1 # Fix this as a free paramter. Then estimate all others as relative risk. Most should be lower, bounded at 0.
  r5.24 = pars['r5.24'] # Relative risk for 5 to 10 year olds (free paramter to estiamte)
  r25.44 = pars['r25.44'] # etc.
  r45.64 = pars['r45.64']
  r65plus = pars['r65plus']  
  rvac= pars['rvac']
  ## Calculate predicted case distributions, as in the likelihood ##
  age.baseline = demog_2009 * b * (
    a0.4_2009 +         # = demography * baseline age-specific risk * relative risk in age class
      r5.24 * a5.24_2009 +
      r25.44 * a25.44_2009 +
      r45.64 * a45.64_2009 +
      r65plus * a65plus_2009
  )
  age.baseline = age.baseline/rowSums(age.baseline)
  # age.demog = (age.baseline*demog_2009)/rowSums(age.baseline*demog_2009)
  imprinting.H1 = (pro.H1*rPro.H1+(1-pro.H1))
  imprinting.H3 = (pro.H3*rPro.H3+(1-pro.H3))
  
  # 2. calculate predicted distribution, pp, as a function of the parameters:
  # This step gives the model prediction
  pp.H1 = age.baseline * imprinting.H1*(wVac*rvac+(1-wVac))
  pp.H3 = age.baseline * imprinting.H3*(wVac*rvac+(1-wVac))
  # pp.H11<-as.matrix(sum_pp(pp.H1))
  # pp.H32<-as.matrix(sum_pp(pp.H3))
  ## Return the predicted age distributions of infection. Plot these below against the observed data
  
  
  return(list(pp.H11=as.matrix(sum_pp(pp.H1)),pp.H32=as.matrix(sum_pp(pp.H3))))
}

cal_predicted_cases<-function(pp,ob){
  storage=pp*0
  storage_1=pp*0
  ob=as.matrix(ob)
  for (jj in 1:dim(ob)[1]) {
    #Find the neg log density for each row (dim 1) and take the sum
    prob=pp[jj,]/sum(pp[jj,])
    storage[jj,]=prob
    prob_1=ob[jj,]/sum(ob[jj,])
    storage_1[jj,]=prob_1
  }
  temp=rbind(storage,storage_1)
  temp=as.data.frame(cbind(temp,rep(c(1:10),2)))
  temp=cbind(temp,rep(c(1,2),each=10))
  colnames(temp)=c('<5','5-24','25-44','45-64','>=65','Year','ID')
  temp1=gather(temp,age_group,count,c('<5','5-24','25-44','45-64','>=65'),factor_key = TRUE)
  temp1=temp1[order(temp1$Year),]
  return(temp1)
}

cal_predicted_cases1<-function(pp,ob){
  storage=pp*0
  storage_1=pp*0
  ob=as.matrix(ob)
  for (jj in 1:dim(ob)[1]) {
    #Find the neg log density for each row (dim 1) and take the sum
    prob=pp[jj,]/sum(pp[jj,])
    storage[jj,]=prob
    prob_1=ob[jj,]/sum(ob[jj,])
    storage_1[jj,]=prob_1
  }
  temp=rbind(storage,storage_1)
  return(temp)
}
# Get prediction for model AS, but don't save to tiff
HAGroup = plotmod1(lk2.HAGroup$par, pro.H1 = proH1.master_2009, pro.H3 = proH3.master_2009)
gpre=cal_predicted_cases(HAGroup$pp.H11,HAGroup$pp.H32)
gob=cal_predicted_cases(H1.master_2009_1,H3.master_2009_1)
HASub = plotmod1(lk2.HASub$par, pro.H1 = prog1.master_2009, pro.H3 = prog2.master_2009)
spre=cal_predicted_cases(HASub$pp.H11,HASub$pp.H32)
sob=cal_predicted_cases(H1.master_2009_1,H3.master_2009_1)
sH1=cal_predicted_cases(HASub$pp.H11,H1.master_2009_1)
sH3=cal_predicted_cases(HASub$pp.H32,H3.master_2009_1)
#NA_ = plotmod1(lk.NA$par, pro.H1 = proN1.master_2009, pro.H3 = proN2.master_2009, CIs = NA.CIs)
NA_ = plotmod1(lk2.NA$par, pro.H1 = proN1.master_2009, pro.H3 = proN2.master_2009)
aH1=cal_predicted_cases(NA_$pp.H11,H1.master_2009_1)
aH3=cal_predicted_cases(NA_$pp.H32,H3.master_2009_1)
# None = plotmod1(lk.None$par, pro.H1 = 0, pro.H3 = 0,  CIs = None.CIs) 
None = plotmod1(lk2.None$par, pro.H1 = 0, pro.H3 = 0) 
nH1=cal_predicted_cases(None$pp.H11,H1.master_2009_1)
nH3=cal_predicted_cases(None$pp.H32,H3.master_2009_1)

barwidth=0.35

plot_fuc<-function(data,data2){
  year_one <- filter(data, ID == 1) %>% 
    group_by(Year) %>% arrange(age_group) 
  
  year_two <- filter(data, ID == 2) %>% 
    group_by(Year) %>% arrange(age_group) 
  
  year_one_pre <- filter(data2, ID == 1) %>% 
    group_by(Year) %>% arrange(age_group) 
  
  year_two_pre <- filter(data2, ID == 2) %>% 
    group_by(Year) %>% arrange(age_group) 
  ggplot() + 
    geom_bar(data = filter(data, ID==2), 
             mapping = aes(x = Year + barwidth + 0.01, y = count, fill = as.factor(age_group)), 
             stat="identity", 
             position='stack' , 
             width = barwidth,
             alpha=0.5) +
    geom_bar(data = year_one, 
             mapping = aes(x = Year, y = count, fill = as.factor(age_group)), 
             stat="identity", 
             position='stack', 
             width = barwidth) +
    labs(fill  = "age_group",colour='black')+
    scale_fill_brewer(palette='Dark2')+
    scale_x_continuous(name = 'Year',breaks =c(1:10)+0.18,labels=c(2010:2019))+
    annotate('text',x=c(1:10)+0.18,y=c(rep(-.15,10)),label=c('H1N1 H3N2'),size=2.5)+
    coord_cartesian(ylim=c(0,1.1),xlim=c(0.5,10.8),clip="off")+
    scale_y_continuous(name = 'Proportion',breaks =c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1))+
    theme(axis.title.x = element_text(hjust=0.03,vjust=7))+
    annotate('text',x=c(0.5,c(1:10)),y=c(rep(-.2,11)),label=c('Cases',rowSums(as.data.frame(predicted[1:10,]))),size=2.5)+
    annotate('text',x=c(c(1:10)+0.35),y=c(rep(-.2,10)),label=c(rowSums(as.data.frame(predicted[11:20,]))),size=2.5)+
    geom_segment(aes(x = c(c(0.83,0.84,1.17,1.18,1.52,1.53),c(0.83,0.84,1.17,1.18,1.52,1.53)+1,c(0.83,0.84,1.17,1.18,1.52,1.53)+2,c(0.83,0.84,1.17,1.18,1.52,1.53)+3,c(0.83,0.84,1.17,1.18,1.52,1.53)+4,c(0.83,0.84,1.17,1.18,1.52,1.53)+5,c(0.83,0.84,1.17,1.18,1.52,1.53)+6,c(0.83,0.84,1.17,1.18,1.52,1.53)+7,c(0.83,0.84,1.17,1.18,1.52,1.53)+8,c(0.83,0.84,1.17,1.18,1.52,1.53)+9)
                     , y = 0, xend =c(c(0.83,0.84,1.17,1.18,1.52,1.53),c(0.83,0.84,1.17,1.18,1.52,1.53)+1,c(0.83,0.84,1.17,1.18,1.52,1.53)+2,c(0.83,0.84,1.17,1.18,1.52,1.53)+3,c(0.83,0.84,1.17,1.18,1.52,1.53)+4,c(0.83,0.84,1.17,1.18,1.52,1.53)+5,c(0.83,0.84,1.17,1.18,1.52,1.53)+6,c(0.83,0.84,1.17,1.18,1.52,1.53)+7,c(0.83,0.84,1.17,1.18,1.52,1.53)+8,c(0.83,0.84,1.17,1.18,1.52,1.53)+9)
                     , yend = 1))+
    geom_segment(aes(x=c(0.83,1.83,2.83,3.83,4.83,5.83,6.83,7.83,8.83,9.83),y=1,xend=c(1.53,2.53,3.53,4.53,5.53,6.53,7.53,8.53,9.53,10.53),yend=1))+
    geom_segment(aes(x=c(0.83,1.83,2.83,3.83,4.83,5.83,6.83,7.83,8.83,9.83),y=0,xend=c(1.53,2.53,3.53,4.53,5.53,6.53,7.53,8.53,9.53,10.53),yend=0))+
    geom_segment(aes(x=c(0.83,1.83,2.83,3.83,4.83,5.83,6.83,7.83,8.83,9.83),y=filter(year_one_pre,age_group=='>=65')$count,xend=c(1.17,2.17,3.17,4.17,5.17,6.17,7.17,8.17,9.17,10.17),yend=filter(year_one_pre,age_group=='>=65')$count))+
    geom_segment(aes(x=c(0.83,1.83,2.83,3.83,4.83,5.83,6.83,7.83,8.83,9.83),y=filter(year_one_pre,age_group=='45-64')$count+filter(year_one_pre,age_group=='>=65')$count,xend=c(1.17,2.17,3.17,4.17,5.17,6.17,7.17,8.17,9.17,10.17),yend=filter(year_one_pre,age_group=='45-64')$count+filter(year_one_pre,age_group=='>=65')$count))+
    geom_segment(aes(x=c(0.83,1.83,2.83,3.83,4.83,5.83,6.83,7.83,8.83,9.83),y=filter(year_one_pre,age_group=='45-64')$count+filter(year_one_pre,age_group=='>=65')$count+filter(year_one_pre,age_group=='25-44')$count,xend=c(1.17,2.17,3.17,4.17,5.17,6.17,7.17,8.17,9.17,10.17),yend=filter(year_one_pre,age_group=='45-64')$count+filter(year_one_pre,age_group=='>=65')$count+filter(year_one_pre,age_group=='25-44')$count))+
    geom_segment(aes(x=c(0.83,1.83,2.83,3.83,4.83,5.83,6.83,7.83,8.83,9.83),y=filter(year_one_pre,age_group=='45-64')$count+filter(year_one_pre,age_group=='>=65')$count+filter(year_one_pre,age_group=='25-44')$count+filter(year_one_pre,age_group=='5-24')$count,xend=c(1.17,2.17,3.17,4.17,5.17,6.17,7.17,8.17,9.17,10.17),yend=filter(year_one_pre,age_group=='45-64')$count+filter(year_one_pre,age_group=='>=65')$count+filter(year_one_pre,age_group=='25-44')$count+filter(year_one_pre,age_group=='5-24')$count))+
    geom_segment(aes(x=c(1.18,2.18,3.18,4.18,5.18,6.18,7.18,8.18,9.18,10.18),y=filter(year_two_pre,age_group=='>=65')$count,xend=c(1.53,2.53,3.53,4.53,5.53,6.53,7.53,8.53,9.53,10.53),yend=filter(year_two_pre,age_group=='>=65')$count))+
    geom_segment(aes(x=c(1.18,2.18,3.18,4.18,5.18,6.18,7.18,8.18,9.18,10.18),y=filter(year_two_pre,age_group=='45-64')$count+filter(year_two_pre,age_group=='>=65')$count,xend=c(1.53,2.53,3.53,4.53,5.53,6.53,7.53,8.53,9.53,10.53),yend=filter(year_two_pre,age_group=='45-64')$count+filter(year_two_pre,age_group=='>=65')$count))+
    geom_segment(aes(x=c(1.18,2.18,3.18,4.18,5.18,6.18,7.18,8.18,9.18,10.18),y=filter(year_two_pre,age_group=='45-64')$count+filter(year_two_pre,age_group=='>=65')$count+filter(year_two_pre,age_group=='25-44')$count,xend=c(1.53,2.53,3.53,4.53,5.53,6.53,7.53,8.53,9.53,10.53),yend=filter(year_two_pre,age_group=='45-64')$count+filter(year_two_pre,age_group=='>=65')$count+filter(year_two_pre,age_group=='25-44')$count))+
    geom_segment(aes(x=c(1.18,2.18,3.18,4.18,5.18,6.18,7.18,8.18,9.18,10.18),y=filter(year_two_pre,age_group=='45-64')$count+filter(year_two_pre,age_group=='>=65')$count+filter(year_two_pre,age_group=='25-44')$count+filter(year_two_pre,age_group=='5-24')$count,xend=c(1.53,2.53,3.53,4.53,5.53,6.53,7.53,8.53,9.53,10.53),yend=filter(year_two_pre,age_group=='45-64')$count+filter(year_two_pre,age_group=='>=65')$count+filter(year_two_pre,age_group=='25-44')$count+filter(year_two_pre,age_group=='5-24')$count))
}

observed<-rbind(H1.master_2009_1,H3.master_2009_1)
rownames(observed)<-c(paste0(2010:2019,'_H1'),paste0(2010:2019,'_H3'))
predicted=round(cal_predicted_cases1(HASub$pp.H11,HASub$pp.H32)*rowSums(observed),0)
rownames(predicted)<-c(paste0(2010:2019,'_H1'),paste0(2010:2019,'_H3'))
write.csv(observed,'/Users/yifanwang/Desktop/materials/HKU_RA/imprinting/ILI_new/observed_1.csv')
write.csv(predicted,'/Users/yifanwang/Desktop/materials/HKU_RA/imprinting/ILI_new/predicted_1.csv')

pdf(file = plot2, width = 10, height = 3.5)
plot_fuc(sob,spre)
dev.off()

c<-plot_fuc(sH1,H1.master)
d<-plot_fuc(sH3,H3.master)
plot<-ggarrange(c, d, 
                labels = c("A. H1N1", "B.H3N2"),
                ncol = 1, nrow = 2,label.x = c(0.015,0.015))
annotate_figure(plot, top = text_grob(expression(paste("HA Subtype model ", Delta, 'AIC=', 14.17)),
                                      color = "black", face = "bold", size = 14))
dev.off()
tiff(file = plot3, width = 10, height = 7, units = 'in', res = 400)
e<-plot_fuc(aH1,H1.master)
f<-plot_fuc(aH3,H3.master)
plot<-ggarrange(e, f, 
                labels = c("A. H1N1", "B.H3N2"),
                ncol = 1, nrow = 2,label.x = c(0.015,0.015))
annotate_figure(plot, top = text_grob(expression(paste("NA Group-level model ", Delta, 'AIC=', 45.63)),
                                      color = "black", face = "bold", size = 14))
dev.off()
tiff(file = plot4, width = 10, height = 7, units = 'in', res = 400)
h<-plot_fuc(nH1,H1.master)
g<-plot_fuc(nH3,H3.master)
plot<-ggarrange(a, b, 
                labels = c("A. H1N1", "B.H3N2"),
                ncol = 1, nrow = 2,label.x = c(0.015,0.015))
annotate_figure(plot, top = text_grob(expression(paste("No imprinting model ", Delta, 'AIC=', 78.53)),
                                      color = "black", face = "bold", size = 14))
dev.off()
colH1N1 = "#00688B7F"
colH2N2 = ('dodgerblue')
colH3N2 = "#FF00007F"
tiff(plot5, width = 7, height = 3.5, units = 'in', res = 400)
par(mar = c(6,4,4,1))
h1_imprinted = (proH1.master[12,])
h2_imprinted = (proH2.master[12,])
h3_imprinted = (proH3.master[12,]) 
names(h1_imprinted)=names(h2_imprinted)=names(h3_imprinted)=2010-(as.numeric(colnames(proH1.master)))
h1_imprinted =h1_imprinted[11:106]
h2_imprinted =h2_imprinted[11:106]
h3_imprinted =h3_imprinted[11:106]
naive = 1-h1_imprinted-h2_imprinted-h3_imprinted
xx = barplot(rbind(h1_imprinted, h2_imprinted, h3_imprinted, naive), col = c(colH1N1, colH2N2, colH3N2, 'gray'), border = NA, space = 0, axes = T, xlab = '', ylab = '', main = '', xaxt = 'n')
axis(side = 1, line = 0, at = xx[seq(1, 96, by = 5)]-.5, labels = xx[seq(1, 96, by = 5)]-.5)
mtext(text = 'age in 2010', side = 1, line = 1.75, cex = .8)
mtext(text = 'prob imprinting to subtype', side = 2, line = 2, cex = .8)
legend(x = 15, y = 1.4, legend = c('H3N2', 'H2N2', 'H1N1', 'naive'), col = c(colH3N2, colH2N2, colH1N1, 'gray'),  ncol = 4, xpd = NA, pch = 15)
dev.off()
####vaccination figure
v0_5<-c(9.7-(15.1-9.7)/5*2,9.7-(15.1-9.7)/5,9.7,12.5,12.9,18,15.1,17.4,23,40.4)
v6_11<-v0_5*(69.3/48.5)
v12_17<-v0_5*(32.9/48.5)
v18_19<-v0_5*(6.2/48.5)
v65<-c(31.7-(40.8-31.7)/5*2,31.7-(40.8-31.7)/5,31.7,32.8,32.7,35,40.8,40.8,43.5,43.5+(45.7-43.5)/2)
v20_29<-v65*(3.6/45.7)
v30_39<-v65*(8.1/45.7)
v40_49<-v65*(13.1/45.7)
v50_64<-v65*(14.3/45.7)
vac<-round(data.frame(year=2010:2019,v0_5,v6_11,v12_17,v18_19,v20_29,v30_39,v40_49,v50_64,v65),2)
colnames(vac)<-c('year','0-5','6-11','12-17','18-19','20-29','30-39','40-49','50-64','65+')
vac<-melt(vac,id='year')
colnames(vac)<-c('year','Age','vaccine')
pdf(file = plot6, width = 10, height = 7)
ggplot(data=vac,aes(x=year,y=vaccine,group=Age,color=Age,shape=Age))+
  geom_point()+
  geom_line()+
  scale_shape_manual(values=seq(0,9))+
  scale_x_continuous(name = 'Year',breaks =2010:2019,labels=c('2010-2011','2011-2012','2012-2013','2013-2014','2014-2015','2015-2016','2016-2017','2017-2018','2018-2019','2019-2020'))+
  ylab('Percent')+
  theme_classic()+
  theme(panel.grid =element_blank())+
  ggtitle('Vaccination coverage across age group')+
  theme(plot.title = element_text(hjust = 0.5,size=12))+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"))+
  theme(axis.title.x = element_text(margin=margin(t=10),size=12),
        axis.title.y = element_text(margin=margin(t=10),size=12))+
  theme(legend.text=element_text(size=12),legend.title = element_text(size=12))
dev.off()
