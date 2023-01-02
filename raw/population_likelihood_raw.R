#setwd("C:/Users/Yifan/Desktop/imprinting/")
setwd("/Users/yifanwang/Desktop/materials/HKU_RA/imprinting/tim/")

library(dplyr)
library(tidyr)
library(readxl)
population<-read.csv('other_data/population data.csv')
birthyear_1998<-read.csv('NPI/data/2021_12_22_birthyear_1998.csv')
birthyear_1999<-read.csv('NPI/data/2021_12_22_birthyear_1999.csv')
birthyear_2000<-read.csv('NPI/data/2021_12_22_birthyear_2000.csv')
birthyear_2001<-read.csv('NPI/data/2021_12_22_birthyear_2001.csv')
birthyear_2002<-read.csv('NPI/data/2021_12_22_birthyear_2002.csv')
birthyear_2003<-read.csv('NPI/data/2021_12_22_birthyear_2003.csv')
birthyear_2004<-read.csv('NPI/data/2021_12_22_birthyear_2004.csv')
birthyear_2005<-read.csv('NPI/data/2021_12_22_birthyear_2005.csv')
birthyear_2006<-read.csv('NPI/data/2021_12_22_birthyear_2006.csv')
birthyear_2007<-read.csv('NPI/data/2021_12_22_birthyear_2007.csv')
birthyear_2008<-read.csv('NPI/data/2021_12_22_birthyear_2008.csv')
birthyear_2009<-read.csv('NPI/data/2021_12_22_birthyear_2009.csv')
birthyear_2010<-read.csv('NPI/data/2021_12_22_birthyear_2010.csv')
birthyear_2011<-read.csv('NPI/data/2021_12_22_birthyear_2011.csv')
birthyear_2012<-read.csv('NPI/data/2021_12_22_birthyear_2012.csv')
birthyear_2013<-read.csv('NPI/data/2021_12_22_birthyear_2013.csv')
birthyear_2014<-read.csv('NPI/data/2021_12_22_birthyear_2014.csv')
birthyear_2015<-read.csv('NPI/data/2021_12_22_birthyear_2015.csv')
birthyear_2016<-read.csv('NPI/data/2021_12_22_birthyear_2016.csv')
birthyear_2017<-read.csv('NPI/data/2021_12_22_birthyear_2017.csv')
birthyear_2018<-read.csv('NPI/data/2021_12_22_birthyear_2018.csv')
birthyear_2019<-read.csv('NPI/data/2021_12_22_birthyear_2019.csv')

# add  WeeklyConsultations_GOPCPMP_2005-2019
raw.data.1<-read.csv('other_data/WeeklyConsultations_PMP_2005-2019.csv')
raw.data.2<-read_excel('other_data/WeeklyConsultations_GOPCPMP_2005-2019.xlsx')
year=seq(2005,2019)
temp<-list()
for (i in 1:15){
  print(i)
  if(i==6){
    a<-raw.data.1[raw.data.1$year==year[i],c(4,5)]
    b<-raw.data.2[raw.data.2$year==year[i],c(4,5)]
    c<-nrow(a)-nrow(b)
    temp1<-a[(c+1):nrow(a),]+b
    a[(c+1):nrow(a),]<-temp1
    temp[[i]]<-a
  }else if(i==5){
    a<-raw.data.1[raw.data.1$year==year[i],c(4,5)]
    b<-raw.data.2[raw.data.2$year==year[i],c(4,5)]
    temp1<-a[1:nrow(b),]+b
    a[1:nrow(b),]<-temp1
    temp[[i]]<-a
  }else{
    a<-raw.data.1[raw.data.1$year==year[i],c(4,5)]
    b<-raw.data.2[raw.data.2$year==year[i],c(4,5)]
    temp[[i]]<-a+b
  }
}
df<-do.call(rbind,temp)
tt<-raw.data.1
tt[,c(4,5)]<-df
raw.data<-tt
raw.data$rate<-round((raw.data$number.of.ILI.visits/raw.data$total.number.of.visits)*1000,1)

raw.data1<-read.csv('other_data/ILILAB_2020.csv')
raw.data2<-read.csv('other_data/ILI_1998 to 2009.csv')
raw.data3<-read.csv('other_data/CHP_ISOWEEK_FLU_1998-060113.csv')


#setwd("C:/Users/Yifan/Desktop/imprinting/tim/")
estimate<-read.csv('estimate.csv',row.names = 1)
healthcare_seeking<-read_excel('healthcare_seeking_ILI_ARI.xlsx')
estimate<-as.matrix(estimate)
Group_est = c(rPro.H1=estimate[6,1],rPro.H3=estimate[7,1], r5.24=estimate[2,1],r25.44=estimate[3,1],r45.64=estimate[4,1],r65plus=estimate[5,1],rvac=0.5)
Sub_est = c(rPro.H1=estimate[6,2],rPro.H3=estimate[7,2], r5.24=estimate[2,2],r25.44=estimate[3,2],r45.64=estimate[4,2],r65plus=estimate[5,2],rvac=0.5)
NA_est=c(rPro.H1=estimate[6,3],rPro.H3=estimate[7,3], r5.24=estimate[2,3],r25.44=estimate[3,3],r45.64=estimate[4,3],r65plus=estimate[5,3],rvac=0.5)
colnames(raw.data)[1]<-c('Year')
colnames(population)[1]<-c('Year')
colnames(raw.data)[2]<-'Week'
rownames(population)<-population$Year
population<-population[-1]
#surveillance data combination
ILI1998_2012<-as.data.frame(unique(raw.data2$Week.End))
ILI1998_2012$Year=substr(ILI1998_2012$`unique(raw.data2$Week.End)`,nchar(ILI1998_2012$`unique(raw.data2$Week.End)`)-4+1, nchar(ILI1998_2012$`unique(raw.data2$Week.End)`));ILI1998_2012$Week=NA;m=0
ILI1998_2012$'<5'=NA;ILI1998_2012$'5-24'=NA;ILI1998_2012$'25-44'=NA;ILI1998_2012$'45-64'=NA;ILI1998_2012$'>=65'=NA;ILI1998_2012$'All'=NA;
for (n in (1:nrow(ILI1998_2012))){
  temp=raw.data2[raw.data2$Week.End==ILI1998_2012[n,1],]
  ILI1998_2012$'<5'[n]=sum(temp$X.5)
  ILI1998_2012$'5-24'[n]=sum(temp$May.24)
  ILI1998_2012$'25-44'[n]=sum(temp$X25.44)
  ILI1998_2012$'45-64'[n]=sum(temp$X45.64)
  ILI1998_2012$'>=65'[n]=sum(temp$X.65)
  ILI1998_2012$'All'[n]=sum(temp$ALL)
  if (ILI1998_2012$Year[n]==ILI1998_2012$Year[n+1] & !is.na(ILI1998_2012$Year[n+1])){
    ILI1998_2012$Week[n]=m+1
    m=m+1
  } else{
    ILI1998_2012$Week[n]=m+1
    m=0
  }
}
ILI1998_2012=ILI1998_2012[-1]
ILI1998_2012=ILI1998_2012[-784:-786,]

# add GOPC
t<-spread(raw.data.2[,-c(5,6)],ageGroup,`number of ILI visits`)
tt<-t[234:369,-c(1,2)]+ILI1998_2012[648:783,-c(1,2)] # from week 22 in 2010 to week52 in 2012
ILI1998_2012[648:783,-c(1,2)]<-tt

ILI2012_2019<-spread(raw.data[c(-5,-6)],ageGroup,number.of.ILI.visits)[-1:-418,]
ILI1998_2019<-rbind(ILI1998_2012,ILI2012_2019)
# write.csv(ILI1998_2019,file='/Users/diana/Dropbox/Shared kiddivax/imprinting/other_data/ILI1998_2019.csv')
data_long <- gather(ILI1998_2019, 'AgeGroup', 'number.of.ILI.visits', '<5':'All', factor_key=TRUE)
data_long <-data_long[order(data_long$Year,data_long$Week),]
raw.data1=raw.data1[-9]%>%filter(Year>=2010);raw.data3=raw.data3[c(-7,-9)];raw.data3=raw.data3[-628:-786,];colnames(raw.data3)[1]='Year'
flu_type_1998_2019<-rbind(raw.data1,raw.data3)
# write.csv(flu_type_1998_2019,file='/Users/diana/Dropbox/Shared kiddivax/imprinting/other_data/flu_type_1998_2019.csv',row.names = FALSE)

#H1 and H3 cases
for (year in (2019:1998)){
  temp = data_long[data_long$Year == year, ]
  temp1 = flu_type_1998_2019[flu_type_1998_2019$Year == year, ]
  raw = left_join(temp, temp1, by = 'Week')
  raw$H1N1 <- raw$number.of.ILI.visits * raw$A.H1N1 / raw$Specimens.tested
  raw$H3N2 <- raw$number.of.ILI.visits * raw$A.H3N2 / raw$Specimens.tested
  raw$pdmH1N1 <-raw$number.of.ILI.visits * raw$A.H1N1pdm / raw$Specimens.tested
  raw <- raw[c(-4:-11)]
  if (year==2019){
    surveillance =raw
  } else{
    surveillance=rbind(raw[1:312,],surveillance)
  }
}#3643
surveillance=surveillance[-c(3643:3744),]
x=cbind(rep((1998:2019),each=5),rep(c('<5','5-24','25-44','45-64','>=65'),11))
raw<-as.data.frame(x)
colnames(raw)<-c('Year','AgeGroup')
for (year in (1998:2019)){
  for(age in c('<5','5-24','25-44','45-64','>=65')){
    raw$sH1N1[raw$AgeGroup==age&raw$Year==year]<-round(sum(surveillance[surveillance$AgeGroup==age&surveillance$Year==year,]$H1N1),0)
    raw$H3N2[raw$AgeGroup==age&raw$Year==year]<-round(sum(surveillance[surveillance$AgeGroup==age&surveillance$Year==year,]$H3N2),0)
    raw$pH1N1[raw$AgeGroup==age&raw$Year==year]<-round(sum(surveillance[surveillance$AgeGroup==age&surveillance$Year==year,]$pdmH1N1),0)
    raw$H1N1[raw$AgeGroup==age&raw$Year==year]<-round((sum(surveillance[surveillance$AgeGroup==age&surveillance$Year==year,]$H1N1)+sum(surveillance[surveillance$AgeGroup==age&surveillance$Year==year,]$pdmH1N1)),0)
  }
}
# write.csv(raw,file='/Users/diana/Dropbox/Shared kiddivax/imprinting/other_data/1998_2019_flu_data.csv')
H1.master<-spread(raw[c(-3,-4,-6)],AgeGroup,pH1N1)
H3.master<-spread(raw[c(-3,-5,-6)],AgeGroup,H3N2)
H1.master <- H1.master[c(1,5,3,4,2)+1]
H3.master <- H3.master[c(1,5,3,4,2)+1]




# hcs<-as.matrix(healthcare_seeking[,6])
# hcs<-as.numeric(hcs[c(-1,-2,-4)])
# H1.master_1<-round(t(apply(H1.master,1 , function(x) x/hcs)))
# H3.master_1<-round(t(apply(H3.master,1,function(x) x/hcs)))
H1.master_1<-H1.master
H3.master_1<-H3.master
#start from 1998 to 2019
## Define age bins to which we will fit a setup function
population=population[-23,]
population=population*1000
ssns<-rownames(population)
bys = 2019:1911
agemat = t(sapply(X = c(1998:2019), function(xx)
  xx - 2019:1911))
rownames(agemat) = 1998:2019; colnames(agemat) = 2019:1911
y09a5_24<-agemat*0
y09a5_24[12,16:35]=1
y09a5_24=y09a5_24[c(12:22), ]
a0.4 = a5.24 = a25.44 = a45.64=a65plus= agemat*0 # Initialize
a0.4[agemat %in% 0:4] = 1
a0.4[is.na(a0.4)]<-0
a5.24[agemat %in% 5:24] = 1
a5.24[is.na(a5.24)]<-0
a25.44[agemat %in% 25:44] = 1
a25.44[is.na(a25.44)]<-0
a45.64[agemat %in% (45:64)] = 1
a45.64[is.na(a45.64)]<-0
a65plus[agemat >=65] = 1
a65plus[is.na(a65plus)]<-0

v2020<-c(rep(48.5,6),rep(69.3,6),rep(32.9,6),rep(6.2,2),rep(3.6,10),rep(8.1,10),rep(13.1,10),rep(14.3,15),rep(45.7,44))/100
vac1<-matrix(rep(v2020,11),nrow=11,byrow=TRUE)

vac_function<-function(x){
  x<-append(0,x)
  x<-x[-length(x)]
  return(x)
}
for (n in 10:1){
  vac1[n,]<-vac_function(vac1[n+1,])
}
vac1<-vac1[-1,]
rownames(vac1) = 2010:2019; colnames(vac1) = 2019:1911
#0-12 years vaccination rate in 2020
v0_5<-c(9.7-(15.1-9.7)/5*2,9.7-(15.1-9.7)/5,9.7,12.5,12.9,18,15.1,17.4,23,40.4,48.5)
v6_11<-v0_5*(69.3/48.5)
v12_17<-v0_5*(32.9/48.5)
v18_19<-v0_5*(6.2/48.5)
v65<-c(31.7-(40.8-31.7)/5*2,31.7-(40.8-31.7)/5,31.7,32.8,32.7,35,40.8,40.8,43.5,43.5+(45.7-43.5)/2,45.7)
v20_29<-v65*(3.6/45.7)
v30_39<-v65*(8.1/45.7)
v40_49<-v65*(13.1/45.7)
v50_64<-v65*(14.3/45.7)
vac2<-vac1*0
for (n in 1:10){
  vac2[n,]<-c(rep(v0_5[n],6),rep(v6_11[n],6),rep(v12_17[n],6),rep(v18_19[n],2),rep(v20_29[n],10),rep(v30_39[n],10),rep(v40_49[n],10),rep(v50_64[n],15),rep(v65[n],44))/100
}
vac2<-vac2[10:1,]
for (n in 2:10){
  y<-append(rep(0,n-1),vac2[n,])
  vac2[n,]<-y[-seq(length(y)-n+2,length(y),1)]
}
vac2<-vac2[10:1,]
#Demographic Age Distribution
demog_1<- matrix(NA, nrow = length(ssns), ncol = length(bys), dimnames = list(ssns, bys))*0
for(rowindex in (1:nrow(demog_1))){
  rawdem =as.matrix(population[rowindex,])
  startcol = which(agemat[rowindex,]==0)
  pdf = data.frame(xx = 75:84, yy=rawdem[76:85]) 
  colnames(pdf)<-c('xx','yy')
  ft = lm(formula = yy~xx, data = pdf) ## Fit a linear model
  pd = predict(ft, data.frame(xx = c(85:97)))
  pd[which(pd<1500)] = 1000
  rawdem[86:98] = pd
  demog_1[rowindex, startcol:109] = rawdem[1:sum(agemat[rowindex,]>=0)]
}
demog_1[is.na(demog_1)] = 0
demog_1 = demog_1/rowSums(demog_1)


#birthyear imprinting effect
newz<-matrix(NA,1,3)
colnames(newz)<-c('Year','Value','Currentyear')
y<-as.data.frame(2019:1911)
colnames(y)<-'Year'
abc<-function(a,b){
  x<-as.data.frame(cbind(a[,1],a[,b]))
  colnames(x)<-c('Year','Value')
  x$Year<-(as.integer(x$Year))
  z<-left_join(y,x,by='Year')
  z$Value[is.na(z$Value)]<-0
  z<-z[-1]
  nz<-t(z)
  return(nz)
}

proH1.master<-rbind(abc(birthyear_1998,2),abc(birthyear_1999,2),abc(birthyear_2000,2),abc(birthyear_2001,2),abc(birthyear_2002,2),
                    abc(birthyear_2003,2),abc(birthyear_2004,2),abc(birthyear_2005,2),abc(birthyear_2006,2),abc(birthyear_2007,2),
                    abc(birthyear_2008,2),abc(birthyear_2009,2),abc(birthyear_2010,2),abc(birthyear_2011,2),
                    abc(birthyear_2012,2),abc(birthyear_2013,2),abc(birthyear_2014,2),abc(birthyear_2015,2),abc(birthyear_2016,2),
                    abc(birthyear_2017,2),abc(birthyear_2018,2),abc(birthyear_2019,2))
dimnames(proH1.master) = list(ssns, bys)# Individuals who imprinted to H1N1 will be protected against H1N1 at the HA subtype level
proH2.master<-rbind(abc(birthyear_1998,3),abc(birthyear_1999,3),abc(birthyear_2000,3),abc(birthyear_2001,3),abc(birthyear_2002,3),
                    abc(birthyear_2003,3),abc(birthyear_2004,3),abc(birthyear_2005,3),abc(birthyear_2006,3),abc(birthyear_2007,3),
                    abc(birthyear_2008,3),abc(birthyear_2009,3),abc(birthyear_2010,3),abc(birthyear_2011,3),
                    abc(birthyear_2012,3),abc(birthyear_2013,3),abc(birthyear_2014,3),abc(birthyear_2015,3),abc(birthyear_2016,3),
                    abc(birthyear_2017,3),abc(birthyear_2018,3),abc(birthyear_2019,3))
dimnames(proH2.master) = list(ssns, bys)
proH3.master<-rbind(abc(birthyear_1998,4),abc(birthyear_1999,4),abc(birthyear_2000,4),abc(birthyear_2001,4),abc(birthyear_2002,4),
                    abc(birthyear_2003,4),abc(birthyear_2004,4),abc(birthyear_2005,4),abc(birthyear_2006,4),abc(birthyear_2007,4),
                    abc(birthyear_2008,4),abc(birthyear_2009,4),abc(birthyear_2010,4),abc(birthyear_2011,4),
                    abc(birthyear_2012,4),abc(birthyear_2013,4),abc(birthyear_2014,4),abc(birthyear_2015,4),abc(birthyear_2016,4),
                    abc(birthyear_2017,4),abc(birthyear_2018,4),abc(birthyear_2019,4))
dimnames(proH3.master) = list(ssns, bys)# Individuals who imprinted to H3N2 will be protected against H3N2 at the HA subtype level
prog1.master=proH1.master+proH2.master# Individuals who imprinted to H1N1 OR H2N2 will be protected against H1N1 at the HA group level
prog2.master=proH3.master
proN1.master=proH1.master
proN2.master=proH2.master+proH3.master# Individuals who imprinted to H2N2 or H3N2 will be protected against H3N2 at the NA subtype level

##2009-2019                                        
a0.4_2009 = a0.4[c(13:22), ]
a5.24_2009 = a5.24[c(13:22), ]
a25.44_2009 = a25.44[c(13:22), ]
a45.64_2009 = a45.64[c(13:22), ]
a65plus_2009 = a65plus[c(13:22), ]
H1.master_2009 = H1.master[c(13:22), ]
H1.master_2009_1 = H1.master_1[c(13:22), ]
H3.master_2009_1 = H3.master_1[c(13:22), ]
prog1.master_2009 = prog1.master[c(13:22), ]
proH1.master_2009 = proH1.master[c(13:22), ]
proN1.master_2009 = proN1.master[c(13:22), ]
H3.master_2009 = H3.master[c(13:22), ]
prog2.master_2009 = prog2.master[c(13:22), ]
proH3.master_2009 = proH3.master[c(13:22), ]
proN2.master_2009 = proN2.master[c(13:22), ]
demog_2009 = demog_1[c(13:22), ]
#function
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
population_distribution<-sum_pp(demog_2009)
write.csv(population_distribution,'/Users/yifanwang/Desktop/materials/HKU_RA/imprinting/raw_new/populationdistribution.csv')
nll = function(pars,
               wPro.H1,
               dat.H1,
               wPro.H3,
               dat.H3,
               a0.4,
               a5.24,
               a25.44,
               a45.64,
               a65plus,
               dem,
               wVac) {
  # 1. Assign parameters to be fit
  rPro.H1 = ifelse(is.na(pars['rPro.H1']), 1,pars['rPro.H1'])# Relative risk given imprinting protection
  rPro.H3 = ifelse(is.na(pars['rPro.H3']), 1,pars['rPro.H3'])# Relative risk given imprinting protection
  b = 1 # Fix relative risk in the baseline group (Ages 0-4) at value 1. Then estimate all others as relative risk. Most should be lower, bounded at 0.
  r5.24 = pars['r5.24'] # Relative risk for 5 to 10 year olds (free paramter to estiamte)
  r25.44 = pars['r25.44'] # etc.
  r45.64 = pars['r45.64']
  r65plus = pars['r65plus']
  rvac= pars['rvac']
  ## Age-specific baseline prediction takes the same form for H1N1 and H3N2. Attempt to explain residual, subtype-specific differences through differences in imprinting history, etc. below.
  age.baseline = dem * b * (
    a0.4 +         # = demography * baseline age-specific risk * relative risk in age class
      r5.24 * a5.24 +
      r25.44 * a25.44 +
      r45.64 * a45.64 +
      r65plus * a65plus
  )
  if (is.null(dim(age.baseline))) {
    # If only one row
    age.baseline = age.baseline / sum(age.baseline)
  } else{
    age.baseline = age.baseline / rowSums(age.baseline)
  }# Normalize so that the fraction of cases predicted in each age group sums to 1 across all age groups
  
  
  # 2. calculate predicted distribution, pp, as a function of the parameters:
  # This step gives the model prediction for H1N1 cases
  pp.H1 = age.baseline * (wPro.H1 * rPro.H1 + (1 - wPro.H1))*(wVac*rvac+(1-wVac))
  pp.H11<-as.matrix(sum_pp(pp.H1))
  # This step gives the model prediction for H3N2 caeses
  pp.H3 = age.baseline * (wPro.H3 * rPro.H3 + (1 - wPro.H3))*(wVac*rvac+(1-wVac))
  pp.H32<-as.matrix(sum_pp(pp.H3))
  
  #  3. Likelihood is based on the multinomial density
  if (is.null(dim(dat.H1))) {
    #DO THIS IF DATA FROM ONE YEAR INPUT AS A VECTOR
    lk.H1 = -dmultinom(dat.H1,
                       size = sum(dat.H1),
                       prob = pp.H11,###try to change the probabiliyt matrix##
                       log = TRUE) #This line returns the log multinomial density of the observed data, with expected probabilities governed by model predictions.
  } else{
    #ELSE DO THIS IF MULTI-YEAR DATA INPUT IN A MATRIX
    storage = vector('numeric', dim(dat.H1)[1])
    for (jj in 1:dim(dat.H1)[1]) {
      #Find the neg log density for each row (dim 1) and take the sum
      storage[jj] = -dmultinom(
        dat.H1[jj, ],
        size = sum(dat.H1[jj,]),
        prob = pp.H11[jj,],###try to change the probabiliyt matrix##
        log = TRUE
      )
    }
    lk.H1 = sum(storage)
  }
  if (is.null(dim(dat.H3))) {
    #DO THIS IF DATA FROM ONE YEAR INPUT AS A VECTOR
    lk.H3 = -dmultinom(dat.H3,
                       size = sum(dat.H3),
                       prob = pp.H32,
                       log = TRUE) #This line returns the log multinomial density of the observed data, with expected probabilities governed by model predictions.
  } else{
    #ELSE DO THIS IF MULTI-YEAR DATA INPUT IN A MATRIX
    storage = vector('numeric', dim(dat.H3)[1])
    for (jj in 1:dim(dat.H3)[1]) {
      #Find the neg log density for each row (dim 1) and take the sum
      storage[jj] = -dmultinom(
        dat.H3[jj,],
        size = sum(dat.H3[jj,]),
        prob = pp.H32[jj,],
        log = TRUE
      )
    }
    lk.H3 = sum(storage)
  }
  # Total negative log likelihood is the sum of nll of H3N2 data, and of H1N1 data
  lk.H1 + lk.H3 # end function
}
wrapper_2009 = function(pars.in,lower.in,upper.in,pro.H1, pro.H3,vac) {
  optim(
    par = pars.in,
    fn = nll,
    wPro.H1 = pro.H1,
    dat.H1 = H1.master_2009,
    wPro.H3 =  pro.H3,
    dat.H3 = H3.master_2009,
    a0.4 = a0.4_2009,
    a5.24 = a5.24_2009,
    a25.44 = a25.44_2009,
    a45.64 = a45.64_2009,
    a65plus = a65plus_2009,
    dem = demog_2009,
    method = 'L-BFGS-B',
    lower = c(lower.in, rep(.001, 4),0.499),
    upper = c(upper.in, rep(1, 4),0.5),
    wVac = vac
  )
  ## Note: age-specific risk pars should be bounded at 0, but a lower bound of exactly 0 will return a NAN value and crash the optimizer.
  ##       use lower bound of 0.001, and choose a lower bound closer to 0 if you MLEs are crashing to the lower bound.
}
wrapper_2009_1 = function(pars.in,lower.in,upper.in,pro.H1, pro.H3,vac) {
  optim(
    par = pars.in,
    fn = nll,
    wPro.H1 = pro.H1,
    dat.H1 = H1.master_2009_1,
    wPro.H3 =  pro.H3,
    dat.H3 = H3.master_2009_1,
    a0.4 = a0.4_2009,
    a5.24 = a5.24_2009,
    a25.44 = a25.44_2009,
    a45.64 = a45.64_2009,
    a65plus = a65plus_2009,
    dem = demog_2009,
    method = 'L-BFGS-B',
    lower = c(lower.in, rep(.001, 4),0.499),
    upper = c(upper.in, rep(1, 4),0.5),
    wVac = vac
  )
  ## Note: age-specific risk pars should be bounded at 0, but a lower bound of exactly 0 will return a NAN value and crash the optimizer.
  ##       use lower bound of 0.001, and choose a lower bound closer to 0 if you MLEs are crashing to the lower bound.
}

# lk.HAGroup<-wrapper_2009(Group_est, lower.in = c(.001, .001), upper.in = c(1, 1),pro.H1=prog1.master_2009,pro.H3 = prog2.master_2009, vac=vac2)
# lk.HASub<-wrapper_2009(Sub_est, lower.in = c(.001, .001), upper.in = c(1, 1),pro.H1=proH1.master_2009,pro.H3 = proH3.master_2009,vac=vac2)
# lk.NA<-wrapper_2009(NA_est, lower.in = c(.001, .001), upper.in = c(1, 1),pro.H1=proN1.master_2009,pro.H3 = proN2.master_2009,vac=vac2)
# lk.None<-wrapper_2009(Group_est, lower.in = c(.999, .999), upper.in = c(1, 1),pro.H1=0,pro.H3 = 0,vac=vac2)
lk2.HAGroup<-wrapper_2009_1(Group_est, lower.in = c(.001, .001), upper.in = c(1, 1),pro.H1=prog1.master_2009,pro.H3 = prog2.master_2009, vac=vac2)
lk2.HASub<-wrapper_2009_1(Sub_est, lower.in = c(.001, .001), upper.in = c(1, 1),pro.H1=proH1.master_2009,pro.H3 = proH3.master_2009,vac=vac2)
lk2.NA<-wrapper_2009_1(NA_est, lower.in = c(.001, .001), upper.in = c(1, 1),pro.H1=proN1.master_2009,pro.H3 = proN2.master_2009,vac=vac2)
lk2.None<-wrapper_2009_1(Group_est, lower.in = c(.999, .999), upper.in = c(1, 1),pro.H1=0,pro.H3 = 0,vac=vac2)


# compute_aic<-function(mod){
#   mods = mget(ls(pattern = mod))
#   n = numeric(length(mods))
#   AICs = numeric(length(mods))
#   for(ii in 1:length(mods)){
#     n[ii] = mods[[ii]]$value
#     AICs[ii] = 2*length(mods[[ii]]$par)+2*mods[[ii]]$value
#   }
#   names(AICs) = names(mods)
#   names(n) = names(mods)
#   AICs = sort(AICs)
#   return(AICs)
# }
# AICs1<-compute_aic(mod="lk.")
# AICs2<-compute_aic(mod="lk2.")
  mod="lk2."
  mods = mget(ls(pattern = mod))
  n = numeric(length(mods))
  AICs = numeric(length(mods))
  for(ii in 1:length(mods)){
    n[ii] = mods[[ii]]$value
    AICs[ii] = 2*length(mods[[ii]]$par)+2*mods[[ii]]$value
  }
  names(AICs) = names(mods)
  names(n) = names(mods)
  AICs = sort(AICs)
#OUTPUT
# modelfits = '/Users/diana/Dropbox/Shared kiddivax/imprinting/summary/surveillance likelihood/model_fits_add_Vac.RData'
#modelfits2 = '/Users/diana/Dropbox/Shared kiddivax/imprinting/summary/surveillance likelihood/model_fits_add_Vac2.RData'
# save(AICs, lk.HAGroup, lk.HASub, lk.NA, lk.None, file = modelfits)
#save(AICs2, lk2.HAGroup, lk2.HASub, lk2.NA, lk2.None, file = modelfits2)

write.csv(AICs,'/Users/yifanwang/Desktop/materials/HKU_RA/imprinting/raw_new/AICs.csv')
modelfits2 = '/Users/yifanwang/Desktop/materials/HKU_RA/imprinting/raw_new/model_fits_add_Vac2.RData'
# save(AICs, lk.HAGroup, lk.HASub, lk.NA, lk.None, file = modelfits)
save(AICs, lk2.HAGroup, lk2.HASub, lk2.NA, lk2.None, file = modelfits2)

write.csv(H1.master_2009_1,'/Users/yifanwang/Desktop/materials/HKU_RA/imprinting/raw_new/PCR-H1N1_1.csv')
write.csv(H3.master_2009_1,'/Users/yifanwang/Desktop/materials/HKU_RA/imprinting/raw_new/PCR-H3N2_1.csv')
