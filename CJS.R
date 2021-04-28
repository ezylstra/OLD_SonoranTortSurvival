###################################################################################################################################
#CJS models for capture-recapture data
###################################################################################################################################

library(plyr)
library(reshape2)
library(jagsUI)
library(runjags)

rm(list=ls())

#-----------------------------------------------------------------------------------------------# 
# Load data
#-----------------------------------------------------------------------------------------------# 
dat <- read.csv('CapRecapData.csv',header=TRUE,stringsAsFactors=FALSE,na.strings=c('',NA))
survs <- read.csv('PlotSurveySchedule.csv',header=TRUE,stringsAsFactors=FALSE)
pdsi <- read.csv('PDSI.csv',header=TRUE,stringsAsFactors=FALSE)
precip.norms <- read.csv('Precip_norms.csv',header=TRUE,stringsAsFactors=FALSE)
precip <- read.csv('Precip_Monthly.csv',header=TRUE,stringsAsFactors=FALSE)
disttocity <- read.csv('PlotDistToCity.csv',header=TRUE,stringsAsFactors=FALSE)

#-----------------------------------------------------------------------------------------------# 
# Formatting survey schedule
#-----------------------------------------------------------------------------------------------# 
#This is an excel file sent to me in late 2020, with a summary of plot survey effort from 1987-future
#In each column labeled yYYYY, values are the number of person-days spent surveying

  surv.l <- melt(survs,id.vars=c('plot','code','area.sqmi'),value.name='persondays')
  surv.l$yr <- as.numeric(substr(surv.l$variable,2,5))
  surv.l <- surv.l[!is.na(surv.l$persondays),c('plot','code','area.sqmi','yr','persondays')]

#Adding entry for Harcuvar Mtns survey in 2020. In DataQuestions.docx file, it's noted there were 62 person days
  surv.l <- rbind(surv.l,data.frame(plot='Harcuvar Mtns',code='HM',area.sqmi=surv.l$area.sqmi[surv.l$code=='HM'][1],yr=2020,persondays=62))

#Change name of Four Peaks plot to match that in live tortoise database:
  surv.l$plot[surv.l$plot=='FS Four Peaks'] <- 'Four Peaks'
  
#Create a wide version of the survey data table, with 1/0 indicating when surveys were done
  surv.w <- dcast(surv.l, plot + code + area.sqmi ~ yr, value.var='persondays')
  surv.w.mat <- surv.w[,4:ncol(surv.w)]
  surv.w.mat[!is.na(surv.w.mat)] <- 1
  surv.w.mat[is.na(surv.w.mat)] <- 0
  names(surv.w.mat) <- paste0('y',names(surv.w.mat))
  #Add columns for years when no plots were surveyed (1989, 2009, 2011-2014)
  surv.w.mat <- cbind(surv.w.mat,data.frame(y1989=rep(0,nrow(surv.w.mat)),y2009=0,y2011=0,y2012=0,y2013=0,y2014=0))
  surv.w.mat <- surv.w.mat[,order(names(surv.w.mat))]
  surv.w <- cbind(plot=surv.w[,2],surv.w.mat)
    
#Summarizing survey effort by plot
  surv.p <- ddply(surv.l,.(plot,code),summarize,area.sqmi=mean(area.sqmi),n.surveys=sum(!is.na(persondays)),
                  first.surv=min(yr[!is.na(persondays)]),last.surv=max(yr[!is.na(persondays)]))

#-----------------------------------------------------------------------------------------------# 
# Create capture histories for each individual (only when captured as adults)
#-----------------------------------------------------------------------------------------------# 
#Format date
  dat$obsdate <- as.Date(dat$obsdate,format='%Y-%m-%d')

#Fill in MCL, where needed, to determine adult/juvenile status
  dat <- dat[with(dat,order(plot,tort,obsdate)),]
  count(dat$captype[is.na(dat$MCL)]) #Only a few instances where MCL is missing on first capture every or on year
  dat[dat$tort %in% c(dat$tort[which(is.na(dat$MCL) & dat$captype!='SecondOfYr')]),]
  #HF-122, FP-149: first listed MCL is >180, so we know they're adults. 
  #FP-209, HM-220: never had MCL listed (so unclear if adults).  Remove.
  dat <- dat[!dat$tort %in% c('HM-220','FP-209'),]
  
  #Using previous measurement if MCL missing.
  for(i in 2:nrow(dat)){
    dat$MCL[i] <- ifelse(dat$tort[i]==dat$tort[i-1] & is.na(dat$MCL[i]),dat$MCL[i-1],dat$MCL[i])
  }
  
#Remove captures when MCL<180 (juveniles)
  dat <- dat[dat$MCL>=180,]
  
#Clean up sex assignments (1=female, 2=male, 3=juv/unk)
  sexes <- ddply(dat,.(plot,tort),summarize,n.sex=length(unique(sex)),sex.first=sex[1],sex.last=sex[length(sex)])
  sexes[sexes$n.sex>1,] #25 individuals given different sex assignments
  #With no other information, I'll assume sex at last capture is correct
  dat$sex <- sexes$sex.last[match(dat$tort,sexes$tort)]
  #3494 fem, 3675 male, 7 unk
  dat[dat$sex==3,] #Most unk sex are smaller adults (MCL<=210), and all were only captured once

#Retain only a single capture each year (using MCL at first capture)
  datyr <- ddply(dat,.(plot,tort,sex,yr),summarize,MCL=MCL[1])
  nrow(datyr) #3429 captures
  length(unique(datyr$tort)) #1557 individuals
  #Dataframe with number of tortoises caught at least once in each year a plot was surveyed
  plotyr <- ddply(datyr,.(plot,yr),summarize,ntorts=length(tort))

#Create matrix with capture histories (1=captured; 0=plot surveyed but tortoise not captured; NA=plot not surveyed)  
  cr <- dcast(datyr, plot + tort + sex ~ yr, value.var='MCL')
  cr.mat <- cr[,4:ncol(cr)]
  names(cr.mat) <- paste0('y',names(cr.mat))
  #Add columns for years when no plots were surveyed (1989, 2009, 2011-2014)
  cr.mat <- cbind(cr.mat,data.frame(y1989=rep(NA,nrow(cr.mat)),y2009=NA,y2011=NA,y2012=NA,y2013=NA,y2014=NA))
  cr.mat <- cr.mat[,order(names(cr.mat))]
  #Change MCL entries to 1 (indicates tortoise was captured that year)
  #Change NAs to 0 when the plot was surveyed, but tortoise wasn't captured
  #NAs indicate plot wasn't surveyed that year
  for(i in 1:nrow(cr)){
    years <- which(surv.w.mat[surv.w$plot==cr$plot[i],]==1)
    for(j in years){
      cr.mat[i,j] <- ifelse(is.na(cr.mat[i,j]),0,1)
    }
  }
  cr <- cbind(cr[,1:3],cr.mat)
  
  # #Check: number of tortoises captured each year at each plot (same totals from datyr and cr dataframes?)
  # crcheck <- melt(cr,id.vars=c('plot','tort','sex'))
  # names(crcheck)[4] <- 'yr'
  # crcheck$yr <- as.numeric(substr(crcheck$yr,2,5))
  # crcheck <- ddply(crcheck,.(plot,yr),summarize,ntorts=sum(value))
  # crcheck <- crcheck[!is.na(crcheck$ntorts),]
  # all.equal(crcheck$ntorts,plotyr$ntorts)

#-----------------------------------------------------------------------------------------------# 
# Function to create initial values for JAGS
#-----------------------------------------------------------------------------------------------# 
#Create a matrix of initial values for latent states (z)
#NAs up to and including the first occasion, 1's for the remainder
  ch.init <- function(y,f){
    for (i in 1:length(f)){
      if(f[i]==ncol(y)) {y[i,] <- NA} else
        {y[i,1:f[i]] <- NA; y[i,(f[i]+1):ncol(y)] <- 1}}
    return(y)
  }

#-----------------------------------------------------------------------------------------------# 
# Create vector indicating the first year each torotise was captured
#-----------------------------------------------------------------------------------------------# 
#Create vector indicating the first year each tortoise was caught:
  first <- rep(NA,nrow(cr.mat))
  for(i in 1:nrow(cr.mat)){
    first[i] <- (1:ncol(cr.mat))[!is.na(cr.mat[i,]) & cr.mat[i,]>0][1]
  }

#-----------------------------------------------------------------------------------------------# 
# Run simple CJS model in JAGS, no covariates, no random effects
#-----------------------------------------------------------------------------------------------# 
#Prep data objects for JAGS (no covariates, no random effects)
  tortdata <- list(y=as.matrix(cr.mat),
                   ntorts=nrow(cr.mat),
                   nyears=ncol(cr.mat),
                   first=first)

#JAGS model: no covariates, no random effects	
  sink("CJS_NoCovars_NoREs.txt")
  cat("
    model{
      
      #-- Priors and constraints
      
      for (i in 1:ntorts){
        for(t in first[i]:(nyears-1)){
        
          logit(phi[i,t]) <- beta.phi0
          logit(p[i,t]) <- alpha.p0

        } #t
      }#i   
      
      beta.phi0 ~ dlogis(0,1)
      alpha.p0 ~ dlogis(0,1)

      #-- Likelihood
      
      for(i in 1:ntorts){
        z[i,first[i]] <- 1
        
        for (t in (first[i]+1):nyears){              
        
          #State process
          z[i,t] ~ dbern(p_alive[i,t])
          p_alive[i,t] <- phi[i,t-1]*z[i,t-1]
          
          #Observation process
          y[i,t] ~ dbern(p_obs[i,t])
          p_obs[i,t] <- p[i,t-1]*z[i,t]               
          
        } #t
      } #i

      #-- Derived parameters
      
      logit(phi0) <- beta.phi0
      logit(p0) <- alpha.p0

    } #model
  ",fill=TRUE)
  sink()
  
#MCMC settings, parameters, initial values  
  ni <- 2000; na <- 1000; nb <- 8000; nc <- 3; ni.tot <- ni + nb
	
	params <- c('beta.phi0','alpha.p0','phi0','p0')
  
  inits <- function() {list(beta.phi0=runif(1,0,3),
                            alpha.p0=runif(1,-2,2),
                            z=ch.init(as.matrix(cr.mat),first))}

#Run model
	fit.cjs0 <- jags(data=tortdata, inits=inits, parameters.to.save=params,
                   model.file='CJS_NoCovars_NoREs.txt',
                   n.chains=nc, n.adapt=na, n.iter=ni.tot, n.burnin=nb,
                   parallel=T, n.cores=3, DIC=FALSE)  
  #Took about 17 min on laptop with above MCMC settings.
	print(fit.cjs0)
  
#-----------------------------------------------------------------------------------------------# 
# Run CJS model in JAGS with covariates (fixed effects), no random effects
#-----------------------------------------------------------------------------------------------# 
#Note: considered adding a trend in survival (like we did in the 2013 paper), but these models
#had a lot of convergence issues.  Likely problematic given how sparse the data are in later years 
#(including many consecutive years where no surveys were completed)
	
#Formatting individual covariates
	sex <- cr$sex
	sex[sex==3] <- NA
	male.ind <- sex-1
	
#Formatting site covariate: distance to nearest "major" city (population > 10,000)
	names(disttocity)[names(disttocity)=='code'] <- 'plot'
	disttocity <- disttocity[order(disttocity$plot),]
	# #Check that plots are in the same order they're found in capture histories
	# all.equal(unique(cr$plot),unique(disttocity$plot))
	dist.mn <- mean(disttocity$dist.km)
	dist.sd <- sd(disttocity$dist.km)
	distance <- (disttocity$dist.km - dist.mn)/dist.sd
	
#Formatting site covariate: precipitation normals
	precip.norms <- precip.norms[order(precip.norms$plot),]
	# #Check that plots are in the same order they're found in capture histories
	# all.equal(unique(cr$plot),unique(precip.norms$plot))
	precipnorm.mn <- mean(precip.norms$ppt.mm)
	precipnorm.sd <- sd(precip.norms$ppt.mm)
	precip.norm <- (precip.norms$ppt.mm - precipnorm.mn)/precipnorm.sd
	
#Formatting site*year covariate: drought
  #Going to calculate mean PDSI averaged over the previous 12 and 24 months (used 24-mon index in 2013 paper)
	pdsi <- pdsi[with(pdsi,order(div,yr,mon)),]
	pdsi$pdsi.12 <- NA
	pdsi$pdsi.24 <- NA
	jul.index <- which(pdsi$yr %in% 1988:2020 & pdsi$mon==7)
	for(i in jul.index){
	  pdsi$pdsi.12[i] <- mean(pdsi$pdsi[(i-11):i])
	  pdsi$pdsi.24[i] <- mean(pdsi$pdsi[(i-23):i])
	}
	pdsi.mn <- pdsi[!is.na(pdsi$pdsi.12),c('div','yr','pdsi.12','pdsi.24')]
  #Link plot to the appropriate climate division
	plotdiv <- data.frame(plot=unique(cr$plot),div=c(1,1,3,1,6,6,6,1,3,6,3,6,5,6,6,3,7),stringsAsFactors=FALSE)
	plotdiv
	pdsi.plot <- expand.grid(yr=1988:2020,plot=plotdiv$plot)
	pdsi.plot$div <- plotdiv$div[match(pdsi.plot$plot,plotdiv$plot)]
	pdsi.plot <- join(pdsi.plot,pdsi.mn,by=c('div','yr'),type='left')
	pdsi.12 <- dcast(pdsi.plot,plot~yr,mean,value.var='pdsi.12')
	pdsi.24 <- dcast(pdsi.plot,plot~yr,mean,value.var='pdsi.24')
	# #Check that plots are in the same order they're found in capture histories
	# all.equal(unique(cr$plot),as.character(pdsi.12$plot))	
	# all.equal(unique(cr$plot),as.character(pdsi.24$plot))	
	pdsi.12.mat <- as.matrix(pdsi.12[,2:ncol(pdsi.12)])
	pdsi.24.mat <- as.matrix(pdsi.24[,2:ncol(pdsi.24)])
	pdsi12.mn <- mean(pdsi.12.mat)
	pdsi12.sd <- sd(pdsi.12.mat)
	pdsi24.mn <- mean(pdsi.24.mat)
	pdsi24.sd <- sd(pdsi.24.mat)	
	pdsi12.z <- (pdsi.12.mat - pdsi12.mn)/pdsi12.sd
	pdsi24.z <- (pdsi.24.mat - pdsi24.mn)/pdsi24.sd

#Formatting site*year covariate: precipitation	
	precip <- precip[with(precip,order(plot,season)),]
	# #Check that plots are in the same order they're found in capture histories
	# all.equal(unique(cr$plot),unique(precip$plot))
	#Cumulative precipitation (mm) at each plot from Aug-Jul
	precip.aj <- ddply(precip[precip$season %in% 1988:2020,],.(plot,season),summarize,ppt=sum(ppt))
	#Standardizing values by plot mean/SDs (can then compare effects of precipitation that's 1-SD above average)
	precipbyplot <- ddply(precip.aj,.(plot),summarize,ppt.mn=mean(ppt),ppt.sd=sd(ppt))
	precip.aj <- join(precip.aj,precipbyplot,by='plot',type='left')
	precip.aj$ppt.z <- (precip.aj$ppt - precip.aj$ppt.mn)/precip.aj$ppt.sd
	# #check:
	# ddply(precip.aj,.(plot),summarize,mn=mean(ppt.z),sd=sd(ppt.z))
	ppt.df <- dcast(precip.aj,plot~season,value.var='ppt.z')
	ppt.mat <- as.matrix(ppt.df[,2:ncol(ppt.df)])
	
#Formatting site*year covariate: survey effort (person days or person days scaled by plot area in sqkm)
	surv.l$area.sqkm <- surv.l$area.sqmi*2.59
	surv.l$effort.sc <- surv.l$persondays/surv.l$area.sqkm
	#Standarize values
	surv.l$effort.z <- (surv.l$persondays - mean(surv.l$persondays))/sd(surv.l$persondays)
	surv.l$effort.sc.z <- (surv.l$effort.sc - mean(surv.l$effort.sc))/sd(surv.l$effort.sc)
	surv.l$plot <- NULL
	names(surv.l)[names(surv.l)=='code'] <- 'plot'
	surv.l <- surv.l[order(surv.l$code),]	
	#Put into wide form (plot ~ yr)
	eff <- dcast(surv.l, plot ~ yr, value.var='effort.z')
	names(eff)[2:ncol(eff)] <- paste0('y',names(eff[,2:ncol(eff)]))
	effsc <- dcast(surv.l, plot ~ yr, value.var='effort.sc.z')
	names(effsc)[2:ncol(effsc)] <- paste0('y',names(effsc[,2:ncol(effsc)]))
  #Add columns for years when no plots were surveyed (1989, 2009, 2011-2014)
  eff <- cbind(eff,data.frame(y1989=rep(NA,nrow(eff)),y2009=NA,y2011=NA,y2012=NA,y2013=NA,y2014=NA))
  eff <- eff[,order(names(eff))]
  effsc <- cbind(effsc,data.frame(y1989=rep(NA,nrow(eff)),y2009=NA,y2011=NA,y2012=NA,y2013=NA,y2014=NA))
	effsc <- effsc[,order(names(effsc))]
	#Convert from dataframe to matrix and replace all NAs with 0s
	#Don't need 1987 data, so removing
	eff <- as.matrix(eff[,3:ncol(eff)])
	eff[is.na(eff)] <- 0
	effsc <- as.matrix(effsc[,3:ncol(effsc)])
	effsc[is.na(effsc)] <- 0
	
#Plot index for each tortoise
	plots <- data.frame(plot=unique(cr$plot),plot.index=1:length(unique(cr$plot)))
  plot.index <- plots$plot.index[match(cr$plot,plots$plot)]	

#Prep data objects for JAGS
  tortdata <- list(y=as.matrix(cr.mat),
                   ntorts=nrow(cr.mat),
                   nyears=ncol(cr.mat),
                   first=first,
                   male=male.ind,
                   plot=plot.index,
                   distance=distance,
                   mean.precip=precip.norm,
                   drought=pdsi24.z,
                   precip=ppt.mat)

#JAGS model: covariates (without effort), no random effects
  sink("CJS_Covars_NoREs.txt")
  cat("
    model{
      
      #-- Priors and constraints
      
      for (i in 1:ntorts){
        for(t in first[i]:(nyears-1)){
        
          logit(phi[i,t]) <- beta.phi0 + b.male*male[i] + b.distance*distance[plot[i]] + b.mnprecip*mean.precip[plot[i]] + 
                             b.drought*drought[plot[i],t] + b.int*mean.precip[plot[i]]*drought[plot[i],t] 
          logit(p[i,t]) <- alpha.p0 + a.male*male[i] + a.precip*precip[plot[i],t]

        } #t
      }#i   
      
      beta.phi0 ~ dlogis(0,1)
      b.male ~ dnorm(0,0.1)
      b.distance ~ dnorm(0,0.1)
      b.mnprecip ~ dnorm(0,0.1)
      b.drought ~ dnorm(0,0.1)
      b.int ~ dnorm(0,0.1)
      alpha.p0 ~ dlogis(0,1)
      a.male ~ dnorm(0,0.1)
      a.precip ~ dnorm(0,0.1)      
      psi ~ dunif(0,1)
  
      #-- Likelihood
      
      for(i in 1:ntorts){
        z[i,first[i]] <- 1
        male[i] ~ dbern(psi)
        
        for (t in (first[i]+1):nyears){              
        
          #State process
          z[i,t] ~ dbern(p_alive[i,t])
          p_alive[i,t] <- phi[i,t-1]*z[i,t-1]
          
          #Observation process
          y[i,t] ~ dbern(p_obs[i,t])
          p_obs[i,t] <- p[i,t-1]*z[i,t]               
          
        } #t
      } #i

      #-- Derived parameters
      
      logit(phi0.female) <- beta.phi0
      logit(p0.female) <- alpha.p0
      phi0.male <- exp(beta.phi0 + b.male)/(1 + exp(beta.phi0 + b.male))
      p0.male <- exp(alpha.p0 + a.male)/(1 + exp(alpha.p0 + a.male))
      

    } #model
  ",fill=TRUE)
  sink()
  
#MCMC settings, parameters, initial values  
  #ni <- 2000; na <- 1000; nb <- 8000; nc <- 3; ni.tot <- ni + nb
	ni <- 500; na <- 500; nb <- 500; nc <- 3; ni.tot <- ni + nb
  
	params <- c('beta.phi0','b.male','b.distance','b.mnprecip','b.drought','b.int',
	            'alpha.p0','a.male','a.precip','psi','phi0.female','phi0.male','p0.female','p0.male')
  
  inits <- function() {list(beta.phi0=runif(1,0,3),
                            b.male=runif(1,-1,1),
                            b.distance=runif(1,-1,1),
                            b.mnprecip=runif(1,-1,1),
                            b.drought=runif(1,-1,1),
                            b.int=runif(1,-1,1),
                            alpha.p0=runif(1,-2,2),
                            a.male=runif(1,-1,1),
                            a.precip=runif(1,-1,1),
                            psi=dunif(1,0,1),
                            male=ifelse(is.na(male.ind),1,NA),
                            z=ch.init(as.matrix(cr.mat),first))}

#Run model
	fit.cjs1 <- jags(data=tortdata, inits=inits, parameters.to.save=params,
                   model.file='CJS_Covars_NoREs.txt',
                   n.chains=nc, n.adapt=na, n.iter=ni.tot, n.burnin=nb,
                   parallel=T, n.cores=3, DIC=FALSE)  
  print(fit.cjs1)

#-----------------------------------------------------------------------------------------------# 
# Run CJS model in JAGS with fixed effects and a random site effect in survival model
#-----------------------------------------------------------------------------------------------# 
#Prep data objects for JAGS
  ntorts <- nrow(cr.mat)             #number of tortoises
  nyears <- ncol(cr.mat)             #number of occasions
  nplots <- length(unique(cr$plot))  #number of plots
  
  tortdata <- list(y=as.matrix(cr.mat),
                   ntorts=ntorts,
                   nyears=nyears,
                   nplots=nplots,
                   first=first,
                   male=male.ind,
                   plot=plot.index,
                   distance=distance,
                   mean.precip=precip.norm,
                   drought=pdsi24.z,
                   precip=ppt.mat)

#JAGS model: covariates, site random effects	
  sink("CJS_Covars_siteREs.txt")
  cat("
    model{
      
      #-- Priors and constraints
      
      for (i in 1:ntorts){
        for(t in first[i]:(nyears-1)){
        
          logit(phi[i,t]) <- beta.phi0 + b.male*male[i] + b.distance*distance[plot[i]] + b.mnprecip*mean.precip[plot[i]] + 
                             b.drought*drought[plot[i],t] + b.int*mean.precip[plot[i]]*drought[plot[i],t] + e.site[plot[i]]
          logit(p[i,t]) <- alpha.p0 + a.male*male[i] + a.precip*precip[plot[i],t]

        } #t
      }#i   
      
      for(p in 1:nplots){
        e.site[p] ~ dnorm(0,tau.site)
      }
      
      beta.phi0 ~ dlogis(0,1)
      b.male ~ dnorm(0,0.1)
      b.distance ~ dnorm(0,0.1)
      b.mnprecip ~ dnorm(0,0.1)
      b.drought ~ dnorm(0,0.1)
      b.int ~ dnorm(0,0.1)
      alpha.p0 ~ dlogis(0,1)
      a.male ~ dnorm(0,0.1)
      a.precip ~ dnorm(0,0.1)      
      psi ~ dunif(0,1)
      
      sigma.site ~ dt(0,pow(2.5,-2),1)T(0,)  #Half-cauchy prior
      tau.site <- 1/(sigma.site*sigma.site)

      #-- Likelihood
      
      for(i in 1:ntorts){
        z[i,first[i]] <- 1
        male[i] ~ dbern(psi)
        
        for (t in (first[i]+1):nyears){              
        
          #State process
          z[i,t] ~ dbern(p_alive[i,t])
          p_alive[i,t] <- phi[i,t-1]*z[i,t-1]
          
          #Observation process
          y[i,t] ~ dbern(p_obs[i,t])
          p_obs[i,t] <- p[i,t-1]*z[i,t]               
          
        } #t
      } #i

      #-- Derived parameters
      
      logit(phi0.female) <- beta.phi0
      logit(p0.female) <- alpha.p0
      phi0.male <- exp(beta.phi0 + b.male)/(1 + exp(beta.phi0 + b.male))
      p0.male <- exp(alpha.p0 + a.male)/(1 + exp(alpha.p0 + a.male))
      

    } #model
  ",fill=TRUE)
  sink()

#MCMC settings, parameters, initial values  
  ni <- 20000; na <- 2000; nb <- 20000; nt <- 20; nc <- 3; ni.tot <- ni + nb
	#ni <- 500; na <- 500; nb <- 500; nc <- 3; ni.tot <- ni + nb
  
	params <- c('beta.phi0','b.male','b.distance','b.mnprecip','b.drought','b.int',
	            'alpha.p0','a.male','a.precip','psi','sigma.site','e.site',
	            'phi0.female','phi0.male','p0.female','p0.male')
  
  inits <- function() {list(beta.phi0=runif(1,1,3),
                            b.male=runif(1,-0.5,0.5),
                            b.distance=runif(1,-1,1),
                            b.mnprecip=runif(1,-0.5,0.5),
                            b.drought=runif(1,-0.5,0.5),
                            b.int=runif(1,-0.5,0.5),
                            alpha.p0=runif(1,0,2),
                            a.male=runif(1,-0.5,0.5),
                            a.precip=runif(1,-0.5,0.5),
                            psi=dunif(1,0,1),
                            sigma.site=dunif(1,0,3),
                            male=ifelse(is.na(male.ind),1,NA),
                            z=ch.init(as.matrix(cr.mat),first))}

#Run model
	fit.cjs2 <- jags(data=tortdata, inits=inits, parameters.to.save=params,
                   model.file='CJS_Covars_siteREs.txt',
                   n.chains=nc, n.adapt=na, n.iter=ni.tot, n.burnin=nb,
                   parallel=T, n.cores=3, DIC=FALSE)  
	print(fit.cjs2)

#-----------------------------------------------------------------------------------------------# 
# Run CJS model in JAGS with fixed effects, including effort, and a random site effect in survival model
#-----------------------------------------------------------------------------------------------# 
#Prep data objects for JAGS
  ntorts <- nrow(cr.mat)             #number of tortoises
  nyears <- ncol(cr.mat)             #number of occasions
  nplots <- length(unique(cr$plot))  #number of plots
  
  tortdata <- list(y=as.matrix(cr.mat),
                   ntorts=ntorts,
                   nyears=nyears,
                   nplots=nplots,
                   first=first,
                   male=male.ind,
                   plot=plot.index,
                   distance=distance,
                   mean.precip=precip.norm,
                   drought=pdsi24.z,
                   precip=ppt.mat,
                   effort=eff)

#JAGS model: covariates (including effort), site random effects	
  sink("CJS_CovarsWithEffort_siteREs.txt")
  cat("
    model{
      
      #-- Priors and constraints
      
      for (i in 1:ntorts){
        for(t in first[i]:(nyears-1)){
        
          logit(phi[i,t]) <- beta.phi0 + b.male*male[i] + b.distance*distance[plot[i]] + b.mnprecip*mean.precip[plot[i]] + 
                             b.drought*drought[plot[i],t] + b.int*mean.precip[plot[i]]*drought[plot[i],t] + e.site[plot[i]]
          logit(p[i,t]) <- alpha.p0 + a.male*male[i] + a.precip*precip[plot[i],t] + a.effort*effort[plot[i],t]

        } #t
      }#i   
      
      for(p in 1:nplots){
        e.site[p] ~ dnorm(0,tau.site)
      }
      
      beta.phi0 ~ dlogis(0,1)
      b.male ~ dnorm(0,0.1)
      b.distance ~ dnorm(0,0.1)
      b.mnprecip ~ dnorm(0,0.1)
      b.drought ~ dnorm(0,0.1)
      b.int ~ dnorm(0,0.1)
      alpha.p0 ~ dlogis(0,1)
      a.male ~ dnorm(0,0.1)
      a.precip ~ dnorm(0,0.1)
      a.effort ~ dnorm(0,0.1)
      psi ~ dunif(0,1)
      
      sigma.site ~ dt(0,pow(2.5,-2),1)T(0,)  #Half-cauchy prior
      tau.site <- 1/(sigma.site*sigma.site)

      #-- Likelihood
      
      for(i in 1:ntorts){
        z[i,first[i]] <- 1
        male[i] ~ dbern(psi)
        
        for (t in (first[i]+1):nyears){              
        
          #State process
          z[i,t] ~ dbern(p_alive[i,t])
          p_alive[i,t] <- phi[i,t-1]*z[i,t-1]
          
          #Observation process
          y[i,t] ~ dbern(p_obs[i,t])
          p_obs[i,t] <- p[i,t-1]*z[i,t]               
          
        } #t
      } #i

      #-- Derived parameters
      
      logit(phi0.female) <- beta.phi0
      logit(p0.female) <- alpha.p0
      phi0.male <- exp(beta.phi0 + b.male)/(1 + exp(beta.phi0 + b.male))
      p0.male <- exp(alpha.p0 + a.male)/(1 + exp(alpha.p0 + a.male))
      

    } #model
  ",fill=TRUE)
  sink()

#MCMC settings, parameters, initial values  
  #ni <- 20000; na <- 2000; nb <- 20000; nt <- 20; nc <- 3; ni.tot <- ni + nb
	ni <- 500; na <- 500; nb <- 500; nc <- 3; nt <- 1; ni.tot <- ni + nb
  
	params <- c('beta.phi0','b.male','b.distance','b.mnprecip','b.drought','b.int',
	            'alpha.p0','a.male','a.precip','a.effort','psi','sigma.site','e.site',
	            'phi0.female','phi0.male','p0.female','p0.male')
  
  inits <- function() {list(beta.phi0=runif(1,1,3),
                            b.male=runif(1,-0.5,0.5),
                            b.distance=runif(1,-1,1),
                            b.mnprecip=runif(1,-0.5,0.5),
                            b.drought=runif(1,-0.5,0.5),
                            b.int=runif(1,-0.5,0.5),
                            alpha.p0=runif(1,0,2),
                            a.male=runif(1,-0.5,0.5),
                            a.precip=runif(1,-0.5,0.5),
                            a.effort=runif(1,-0.5,0.5),
                            psi=dunif(1,0,1),
                            sigma.site=dunif(1,0,3),
                            male=ifelse(is.na(male.ind),1,NA),
                            z=ch.init(as.matrix(cr.mat),first))}

#Run model
	fit.cjs3 <- jags(data=tortdata, inits=inits, parameters.to.save=params,
                   model.file='CJS_CovarsWithEffort_siteREs.txt',
                   n.chains=nc, n.adapt=na, n.iter=ni.tot, n.burnin=nb, n.thin=nt,
                   parallel=T, n.cores=3, DIC=FALSE)  
	print(fit.cjs3)
