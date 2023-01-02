#setwd('C:/Users/Yifan/Desktop/imprinting/')
setwd("/Users/yifanwang/Desktop/materials/HKU_RA/imprinting/")
load('raw_new/model_fits_add_Vac2.RData') # Model fits saved as lk.A, lk.AG, lk.AS, lk.AN
# source('program/population_likelihood.R')
library('parallel')

## OUTPUTS
outfile1 = 'raw_new/profiles.RData'
outfile1_2 = 'raw_new/profiles_1.RData'

outfile2 = 'raw_new/CIs.RData'
outfile2_2 = 'raw_new/CIs_1.RData'
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

########################################
# Write a function wrapper to calculate profiles
########################################
## Write a function wrapper to calculate one profile point value
## Input pvec, a named vector of best paramter estimates from a fitted likelihood. Should include the paramter to be fixed
##       fixpar, name of the parameter to be fixed
##       fixed.par.vale - value of paramter to be fixed
##       lows -  vector of lower par limits, corresponding to each entry in pvec, including the fixed parameter
##       highs - vector of upper par limits
profile_func = function(pars,
                        fixed.par.name,
                        fixed.par.value,
                        wPro.H1,
                        dat.H1,
                        wPro.H3,
                        dat.H3,
                        a0.4,
                        a5.24,
                        a25.44,
                        a45.64,
                        a65plus,
                        dem,wVac) {
  # 1. Assign parameters to be fit
  rPro.H1 = ifelse(is.na(pars['rPro.H1']), 1,pars['rPro.H1'])# Relative risk given imprinting protection
  rPro.H3 = ifelse(is.na(pars['rPro.H3']), 1,pars['rPro.H3'])# Relative risk given imprinting protection
  b = 1 # Fix relative risk in the baseline group (Ages 0-4) at value 1. Then estimate all others as relative risk. Most should be lower, bounded at 0.
  r5.24 = pars['r5.24'] # Relative risk for 5 to 10 year olds (free paramter to estiamte)
  r25.44 = pars['r25.44'] # etc.
  r45.64 = pars['r45.64']
  r65plus = pars['r65plus']
  rvac= pars['rvac']
  # 2. If the parameter of interest is meant to be fixed, replace its value with fixed.par.val
  #   Else, keep the value the same
  rPro.H1 = ifelse(fixed.par.name == 'rPro.H1', fixed.par.value, rPro.H1)
  rPro.H3 = ifelse(fixed.par.name == 'rPro.H3', fixed.par.value, rPro.H3)
  r5.24 = ifelse(fixed.par.name == 'r5.24', fixed.par.value, r5.24)
  r25.44 = ifelse(fixed.par.name == 'r25.44', fixed.par.value, r25.44)
  r45.64 = ifelse(fixed.par.name == 'r45.64', fixed.par.value, r45.64)
  r65plus = ifelse(fixed.par.name == 'r65plus', fixed.par.value, r65plus)
  rvac = ifelse(fixed.par.name == 'rvac', fixed.par.value, rvac)
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
                       prob = pp.H11,
                       log = TRUE) #This line returns the log multinomial density of the observed data, with expected probabilities governed by model predictions.
  } else{
    #ELSE DO THIS IF MULTI-YEAR DATA INPUT IN A MATRIX
    storage = vector('numeric', dim(dat.H1)[1])
    for (jj in 1:dim(dat.H1)[1]) {
      #Find the neg log density for each row (dim 1) and take the sum
      storage[jj] = -dmultinom(
        dat.H1[jj, ],
        size = sum(dat.H1[jj,]),
        prob = pp.H11[jj,],
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

one_prof_point = function(par.in,
                          fixpar,
                          fixed.par.value,
                          lows,
                          highs,
                          H1.protection.input,
                          H3.protection.input,vac) {
  drop = which(names(par.in) == fixpar) # Figure out index of fixed par
  # Optimize the likelihood with respect to all free paramters, other than the fixed par
  optim(
    par = par.in[-drop],
    fn = profile_func,
    fixed.par.name = fixpar,
    fixed.par.value = fixed.par.value,
    wPro.H1 = H1.protection.input,
    wPro.H3 =  H3.protection.input,
    dat.H1 = H1.master_2009,
    dat.H3 = H3.master_2009,
    a0.4 = a0.4_2009,
    a5.24 = a5.24_2009,
    a25.44 = a25.44_2009,
    a45.64 = a45.64_2009,
    a65plus = a65plus_2009,
    dem = demog_2009,
    method = 'L-BFGS-B',
    lower = lows[-drop],
    upper = highs[-drop],
    wVac = vac
  )$value
}
one_prof_point2 = function(par.in,
                           fixpar,
                           fixed.par.value,
                           lows,
                           highs,
                           H1.protection.input,
                           H3.protection.input,vac) {
  drop = which(names(par.in) == fixpar) # Figure out index of fixed par
  # Optimize the likelihood with respect to all free paramters, other than the fixed par
  optim(
    par = par.in[-drop],
    fn = profile_func,
    fixed.par.name = fixpar,
    fixed.par.value = fixed.par.value,
    wPro.H1 = H1.protection.input,
    wPro.H3 =  H3.protection.input,
    dat.H1 = H1.master_2009_1,
    dat.H3 = H3.master_2009_1,
    a0.4 = a0.4_2009,
    a5.24 = a5.24_2009,
    a25.44 = a25.44_2009,
    a45.64 = a45.64_2009,
    a65plus = a65plus_2009,
    dem = demog_2009,
    method = 'L-BFGS-B',
    lower = lows[-drop],
    upper = highs[-drop],
    wVac = vac
  )$value
}
# ## Test
one_prof_point(par.in = lk.NA$par, fixpar = 'r5.24', fixed.par.value = .5, lows = c(rep(.001, length(lk.NA$par)-1),0.499), highs = c(1,1, rep(5, 4),0.5), H1.protection.input = prog1.master_2009, H3.protection.input = prog2.master_2009,vac=vac2)




###########################
## Profiles for age-specific risk
###########################
###### Initialize storage
grid = seq(.005, 2, by = .005) # Define grid of relative risk points to test for each paramter
# Store provile neg log likelihood values in a matrix, with the fixed parameter listed on rows, and grid value listed on columns
HAGroup.age.prof = HASub.age.prof = NA.age.prof = None.age.prof = matrix(NA, nrow = 4, ncol = length(grid), dimnames = list(names(lk2.None$par)[-c(1,2,7)], grid))
HAGroup.age.prof2 = HASub.age.prof2 = NA.age.prof2 = None.age.prof2 =HAGroup.age.prof
## Run a profile grid for each age paramter
## Run grids in parallel
cl = makeCluster(detectCores()-1) # Make cluster
clusterExport(cl, ls()) # Export all variables to cluster
age.pars = names(lk2.NA$par)[-c(1,2,7)]

## For each age paramter
for(pp in age.pars){
  ## lk.A age profiles
  # 
  #   HAGroup.age.prof[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point, par.in = lk.HAGroup$par, fixpar = pp, lows = c(rep(.001, length(lk.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = prog1.master_2009, H3.protection.input = prog2.master_2009,vac=vac2)
  #   
  #   HASub.age.prof[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point, par.in = lk.HASub$par, fixpar = pp, lows = c(rep(.001, length(lk.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = proH1.master_2009, H3.protection.input = proH3.master_2009,vac=vac2)
  #   
  #   NA.age.prof[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point, par.in = lk.NA$par, fixpar = pp, lows = c(rep(.001, length(lk.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = proN1.master_2009, H3.protection.input = proN2.master_2009,vac=vac2)
  #   
  #   None.age.prof[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point, par.in = lk.None$par, fixpar = pp, lows = c(.999, .999,rep(.001, 4),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = 0, H3.protection.input = 0,vac=vac2)
  
  HAGroup.age.prof2[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point2, par.in = lk2.HAGroup$par, fixpar = pp, lows = c(rep(.001, length(lk2.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = prog1.master_2009, H3.protection.input = prog2.master_2009,vac=vac2)
  
  HASub.age.prof2[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point2, par.in = lk2.HASub$par, fixpar = pp, lows = c(rep(.001, length(lk2.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = proH1.master_2009, H3.protection.input = proH3.master_2009,vac=vac2)
  
  NA.age.prof2[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point2, par.in = lk2.NA$par, fixpar = pp, lows = c(rep(.001, length(lk2.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = proN1.master_2009, H3.protection.input = proN2.master_2009,vac=vac2)
  
  None.age.prof2[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point2, par.in = lk2.None$par, fixpar = pp, lows = c(.999, .999,rep(.001, 4),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = 0, H3.protection.input = 0,vac=vac2)
  
}
stopCluster(cl)



##########################
# Profiles for imprinting protection pars
###########################
###### Initialize storage
ll = length(grid)
grid = seq(.005, 1.2, by = 0.005) # Define grid of relative risk points to test for each paramter
# Store profile neg log likelihood values in a matrix, with the fixed parameter listed on rows, and grid value listed on columns
HAGroup.imp.prof = HASub.imp.prof = NA.imp.prof = None.imp.prof = matrix(NA, nrow = 2, ncol = length(grid), dimnames = list(c('rPro.H1', 'rPro.H3'), grid))
HAGroup.imp.prof2 = HASub.imp.prof2 = NA.imp.prof2 = None.imp.prof2 = None.imp.prof
cl = makeCluster(detectCores()-1) # Make cluster
clusterExport(cl, ls()) # Export all variables to cluster
pro.pars = c('rPro.H1', 'rPro.H3')
for(pp in pro.pars){
  # HAGroup.imp.prof[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point, par.in = lk.HAGroup$par, fixpar = pp, lows = c(rep(.001, length(lk.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = prog1.master_2009, H3.protection.input = prog2.master_2009,vac=vac2)
  # 
  # HASub.imp.prof[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point, par.in = lk.HASub$par, fixpar = pp, lows = c(rep(.001, length(lk.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = proH1.master_2009, H3.protection.input = proH3.master_2009,vac=vac2)
  # 
  # NA.imp.prof[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point, par.in = lk.NA$par, fixpar = pp, lows = c(rep(.001, length(lk.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = proN1.master_2009, H3.protection.input = proN2.master_2009,vac=vac2)
  # 
  # None.imp.prof[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point, par.in = lk.None$par, fixpar = pp, lows = c(.999, .999,rep(.001, 4),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = 0, H3.protection.input = 0,vac=vac2)
  
  HAGroup.imp.prof2[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point2, par.in = lk2.HAGroup$par, fixpar = pp, lows = c(rep(.001, length(lk2.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = prog1.master_2009, H3.protection.input = prog2.master_2009,vac=vac2)
  
  HASub.imp.prof2[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point2, par.in = lk2.HASub$par, fixpar = pp, lows = c(rep(.001, length(lk2.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = proH1.master_2009, H3.protection.input = proH3.master_2009,vac=vac2)
  
  NA.imp.prof2[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point2, par.in = lk2.NA$par, fixpar = pp, lows = c(rep(.001, length(lk2.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = proN1.master_2009, H3.protection.input = proN2.master_2009,vac=vac2)
  
  None.imp.prof2[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point2, par.in = lk2.None$par, fixpar = pp, lows = c(.999, .999,rep(.001, 4),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = 0, H3.protection.input = 0,vac=vac2)
  
}
stopCluster(cl)

## Add 0s for columns corresponding to par values >1. This will pad the matrices so they have the same number of columns and same column names as the age profile matrices above
fill = matrix(NA, nrow = 2, ncol = ll-length(grid)); colnames(fill) = seq(1.205, 2, by = .005)
HAGroup.imp.prof = cbind(HAGroup.imp.prof, fill)
HASub.imp.prof = cbind(HASub.imp.prof, fill)
NA.imp.prof = cbind(NA.imp.prof, fill)
None.imp.prof=cbind(None.imp.prof,fill)

HAGroup.imp.prof2 = cbind(HAGroup.imp.prof2, fill)
HASub.imp.prof2 = cbind(HASub.imp.prof2, fill)
NA.imp.prof2 = cbind(NA.imp.prof2, fill)
None.imp.prof2=cbind(None.imp.prof2,fill)

##
grid = seq(.005, 1.2, by = .005) # Define grid of relative risk points to test for each paramter
# Store provile neg log likelihood values in a matrix, with the fixed parameter listed on rows, and grid value listed on columns
HAGroup.vac.prof = HASub.vac.prof = NA.vac.prof = None.vac.prof = matrix(NA, nrow = 1, ncol = length(grid), dimnames = list(names(lk2.None$par)[c(7)], grid))
HAGroup.vac.prof2 = HASub.vac.prof2 = NA.vac.prof2 = None.vac.prof2 = None.vac.prof
## Run a profile grid for each age paramter
## Run grids in parallel
cl = makeCluster(detectCores()-1) # Make cluster
clusterExport(cl, ls()) # Export all variables to cluster
vac.pars = c('rvac')

for(pp in vac.pars){
  # HAGroup.vac.prof[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point, par.in = lk.HAGroup$par, fixpar = pp, lows = c(rep(.001, length(lk.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = prog1.master_2009, H3.protection.input = prog2.master_2009,vac=vac2)
  # 
  # HASub.vac.prof[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point, par.in = lk.HASub$par, fixpar = pp, lows = c(rep(.001, length(lk.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = proH1.master_2009, H3.protection.input = proH3.master_2009,vac=vac2)
  # 
  # NA.vac.prof[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point, par.in = lk.NA$par, fixpar = pp, lows = c(rep(.001, length(lk.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = proN1.master_2009, H3.protection.input = proN2.master_2009,vac=vac2)
  # 
  # None.vac.prof[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point, par.in = lk.None$par, fixpar = pp, lows = c(.999, .999,rep(.001, 4),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = 0, H3.protection.input = 0,vac=vac2)
  
  HAGroup.vac.prof2[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point2, par.in = lk2.HAGroup$par, fixpar = pp, lows = c(rep(.001, length(lk2.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = prog1.master_2009, H3.protection.input = prog2.master_2009,vac=vac2)
  
  HASub.vac.prof2[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point2, par.in = lk2.HASub$par, fixpar = pp, lows = c(rep(.001, length(lk2.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = proH1.master_2009, H3.protection.input = proH3.master_2009,vac=vac2)
  
  NA.vac.prof2[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point2, par.in = lk2.NA$par, fixpar = pp, lows = c(rep(.001, length(lk2.NA$par)-1),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = proN1.master_2009, H3.protection.input = proN2.master_2009,vac=vac2)
  
  None.vac.prof2[pp, ] = parSapply(cl = cl, X = grid, FUN = one_prof_point2, par.in = lk2.None$par, fixpar = pp, lows = c(.999, .999,rep(.001, 4),0.499),  highs = c(1,1, rep(5, 4),0.5), H1.protection.input = 0, H3.protection.input = 0,vac=vac2)
  
}
stopCluster(cl)

## Add 0s for columns corresponding to par values >1. This will pad the matrices so they have the same number of columns and same column names as the age profile matrices above
fill = matrix(NA, nrow =1, ncol = ll-length(grid)); colnames(fill) = seq(1.205, 2, by = .005)
HAGroup.vac.prof = cbind(HAGroup.vac.prof, fill)
HASub.vac.prof = cbind(HASub.vac.prof, fill)
NA.vac.prof = cbind(NA.vac.prof, fill)
None.vac.prof=cbind(None.vac.prof,fill)
HAGroup.vac.prof2 = cbind(HAGroup.vac.prof2, fill)
HASub.vac.prof2 = cbind(HASub.vac.prof2, fill)
NA.vac.prof2 = cbind(NA.vac.prof2, fill)
None.vac.prof2=cbind(None.vac.prof2,fill)

save(HAGroup.age.prof, HASub.age.prof, NA.age.prof, HAGroup.imp.prof, HASub.imp.prof, NA.imp.prof, HAGroup.vac.prof, HASub.vac.prof,NA.vac.prof,None.vac.prof,file = outfile1)
load('raw_new/profiles.RData')
save('raw_new/profiles_1.RData')

####################################
## Calculate 95% profile CIs for each model and parameter
####################################
# Define a function to calculate the likelihood ratio threshold, as a function of the best nll value, and the number of constrained pars (df = 1 if only one par is fixed in the profile)
LR.Threshold = function(NLL_best, df){
  #-2log(LR) is distributed chi2 with df given by the number of fixed parameters in the profile
  #algebraically, the threshold for being in the CI is given by:
  threshold = qchisq(.95, df)/2+NLL_best
  threshold
}

# Define a function to extract the CI bounds from the grid, and the profile values
LR.CI = function(threshold, nll.vec, pars.vec){
  if(any(which(nll.vec > threshold) < which.min(nll.vec))  &  any(which(nll.vec > threshold) > which.min(nll.vec)) ){ #If the minimum is not an end point
    #Find string before and after the min value
    lower = nll.vec[1:which.min(nll.vec)]
    upper = nll.vec[(which.min(nll.vec)-1):length(nll.vec)]
    #Extract low CI from first string, and upper CI from upper string
    CI = c(pars.vec[which.min(abs(lower-threshold))], pars.vec[length(lower)+which.min(abs(upper-threshold))])
  }else{
    #If the first value is the minimum
    if(any(which(nll.vec > threshold) < which.min(nll.vec)) == FALSE){ CI = c(pars.vec[1], pars.vec[which.min(abs(nll.vec-threshold))]) }
    #If the last value is the maximum
    if(any(which(nll.vec > threshold) > which.min(nll.vec)) == FALSE){ CI = c(pars.vec[which.min(abs(nll.vec-threshold))], pars.vec[length(nll.vec)])}
  }
  CI
}




## Write a wrapper that inputs the model name and paramter name and outputs the LR CI
get.LR.CI = function(mod.name, par.name){
  mod = get(paste('lk.', mod.name, sep = "")) # Extract model fit of interest
  prof.mat = rbind(get(paste(mod.name, '.age.prof', sep = "")), get0(paste(mod.name, '.imp.prof', sep = "")),get0(paste(mod.name, '.vac.prof', sep = "")))
  LR.CI(threshold = LR.Threshold(NLL_best = mod$value, df = 1), nll.vec = prof.mat[par.name,], pars.vec = as.numeric(colnames(prof.mat)))
}

get.LR.CI2 = function(mod.name, par.name){
  mod = get(paste('lk2.', mod.name, sep = "")) # Extract model fit of interest
  prof.mat = rbind(get(paste(mod.name, '.age.prof2', sep = "")), get0(paste(mod.name, '.imp.prof2', sep = "")),get0(paste(mod.name, '.vac.prof2', sep = "")))
  LR.CI(threshold = LR.Threshold(NLL_best = mod$value, df = 1), nll.vec = prof.mat[par.name,], pars.vec = as.numeric(colnames(prof.mat)))
}

lk.HAGroup=lk.HAGroup
lk.HASub=lk.HASub
lk.NA=lk.NA
lk.None=lk.None


## Get CIs for lk.HAGroup
HAGroup.CIs = sapply(names(lk.HAGroup$par), FUN = get.LR.CI, mod.name = "HAGroup")

## Get CIs for lk.HASub
HASub.CIs = sapply(names(lk.HASub$par), FUN = get.LR.CI, mod.name = "HASub")

## Get CIs for lk.NA
NA.CIs = sapply(names(lk.NA$par), FUN = get.LR.CI, mod.name = "NA")

## Get CIs for lk.None
None.CIs = sapply(names(lk.None$par), FUN = get.LR.CI, mod.name = "None")

### SAVE CIs
save(HAGroup.CIs, HASub.CIs, NA.CIs, None.CIs, file = outfile2)

HAGroup.CIs2 = sapply(names(lk2.HAGroup$par), FUN = get.LR.CI2, mod.name = "HAGroup")

## Get CIs2 for lk.HASub
HASub.CIs2 = sapply(names(lk2.HASub$par), FUN = get.LR.CI2, mod.name = "HASub")

## Get CIs2 for lk.NA
NA.CIs2 = sapply(names(lk2.NA$par), FUN = get.LR.CI2, mod.name = "NA")

## Get CIs2 for lk.None
None.CIs2 = sapply(names(lk2.None$par), FUN = get.LR.CI2, mod.name = "None")

### SAVE CIs2
save(HAGroup.CIs2, HASub.CIs2, NA.CIs2, None.CIs2, file = outfile2_2)

est_fun<-function(x,y){
  est<-NULL
  for(n in 7:1){
    m<-paste0(round(x$par[n],2), " (",round(y[1,n],2),",",round(y[2,n],2),")")
    est<-rbind(m,est)
  }
  return(est)
}
# HAGroup.est<-est_fun(lk.HAGroup,HAGroup.CIs)
# HASub.est<-est_fun(lk.HASub,HASub.CIs)
# NA.est<-est_fun(lk.NA,NA.CIs)
# None.est<-est_fun(lk.None,None.CIs)
# est<-cbind(HASub.est,HAGroup.est,NA.est,None.est)
# est<-as.matrix(est,dimnames=list(names(lk.None$par), c(1,2,3,4)))
# rownames(est)<-names(lk.None$par);colnames(est)<-c('HASub','HAGroup','NA','None')
# write.csv(est,'/Users/diana/Dropbox/Shared kiddivax/imprinting/summary/surveillance likelihood/model_est_and_vac.csv')
HAGroup.est2<-est_fun(lk2.HAGroup,HAGroup.CIs2)
HASub.est2<-est_fun(lk2.HASub,HASub.CIs2)
NA.est2<-est_fun(lk2.NA,NA.CIs2)
None.est2<-est_fun(lk2.None,None.CIs2)
est2<-cbind(HASub.est2,HAGroup.est2,NA.est2,None.est2)
est2<-as.matrix(est2,dimnames=list(names(lk.None$par), c(1,2,3,4)))
rownames(est2)<-names(lk2.None$par);colnames(est2)<-c('HASub','HAGroup','NA','None')
write.csv(est2,'/Users/yifanwang/Desktop/materials/HKU_RA/imprinting/raw_new/model_est_and_vac_1.csv')
