###################################################################################################################################
#CJS model for capture-recapture data
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
pdsi <- read.csv('PDSI_July.csv',header=TRUE,stringsAsFactors=FALSE)
precip.norms <- read.csv('Precip_norms.csv',header=TRUE,stringsAsFactors=FALSE)
precip.aj <- read.csv('Precip_AJ.csv',header=TRUE,stringsAsFactors=FALSE)

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
#Formatting individual covariates
	sex <- cr$sex
	sex[sex==3] <- NA
	male <- sex-1
	
#Formatting site covariates
	precip.norms <- precip.norms[order(precip.norms$plot),]
	# #Check that plots are in the same order they're found in capture histories
	# all.equal(unique(cr$plot),unique(precip.norms$plot))
	precipnorm.mn <- mean(precip.norms$ppt.mm)
	precipnorm.sd <- sd(precip.norms$ppt.mm)
	precip.norm <- (precip.norms$ppt.mm - precipnorm.mn)/precipnorm.sd
	
#Format site*year covariates
	pdsi <- pdsi[with(pdsi,order(plot,yr)),]
	# #Check that plots are in the same order they're found in capture histories
	# all.equal(unique(cr$plot),unique(pdsi$plot))
	pdsi.mn <- mean(pdsi$pdsi)
	pdsi.sd <- sd(pdsi$pdsi)
	pdsi$pdsi.z <- (pdsi$pdsi - pdsi.mn)/pdsi.sd
	pdsi.mat <- dcast(pdsi,plot~yr,value.var='pdsi.z')
	
	names(precip.aj)[names(precip.aj)=='season'] <- 'yr'
	precip.aj <- precip.aj[precip.aj$yr>1986,]
	precip.aj <- precip.aj[with(precip.aj,order(plot,yr)),]
	# #Check that plots are in the same order they're found in capture histories
	# all.equal(unique(cr$plot),unique(precip.aj$plot))	
	precipbyplot <- ddply(precip.aj,.(plot),summarize,ppt.mn=mean(ppt),ppt.sd=sd(ppt))
	precip.aj <- join(precip.aj,precipbyplot,by='plot',type='left')
	precip.aj$ppt.z <- (precip.aj$ppt - precip.aj$ppt.mn)/precip.aj$ppt.sd
	# #check:
	# ddply(precip.aj,.(plot),summarize,mn=mean(ppt.z),sd=sd(ppt.z))
	ppt.mat <- dcast(precip.aj,plot~yr,value.var='ppt.z')
	
#Plot index for each tortoise
	plots <- data.frame(plot=unique(cr$plot),plot.index=1:length(unique(cr$plot)))
  plot.index <- plots$plot.index[match(cr$plot,plots$plot)]	

	