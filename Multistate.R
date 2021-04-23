###################################################################################################################################
#Multi-state models for capture-recapture data
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
# Create capture histories for each individual
#-----------------------------------------------------------------------------------------------# 
#Format date
  dat$obsdate <- as.Date(dat$obsdate,format='%Y-%m-%d')

#Fill in MCL, where needed, to determine adult/juvenile status (cutoff = 180mm)
  dat <- dat[with(dat,order(plot,tort,obsdate)),]
  count(dat$captype[is.na(dat$MCL)]) #Only a few instances where MCL is missing on first capture ever or on year
  dat[dat$tort %in% c(dat$tort[which(is.na(dat$MCL) & dat$captype!='SecondOfYr')]),]
  #HF-122, FP-149: first listed MCL is >180, so we know they're adults. 
  #FP-209, HM-220: never had MCL listed (so unclear if adults).  Remove.
  dat <- dat[!dat$tort %in% c('HM-220','FP-209'),]
  
  #Using previous measurement if MCL missing.
  for(i in 2:nrow(dat)){
    dat$MCL[i] <- ifelse(dat$tort[i]==dat$tort[i-1] & is.na(dat$MCL[i]),dat$MCL[i-1],dat$MCL[i])
  }
  
#Clean up sex assignments (1=female, 2=male, 3=juv/unk)
  sexes <- ddply(dat,.(plot,tort),summarize,n.sex=length(unique(sex)),sex.first=sex[1],sex.last=sex[length(sex)])
  #With no other information, I'll assume sex at last capture is correct
  dat$sex <- sexes$sex.last[match(dat$tort,sexes$tort)]
  #3630 fem, 3734 male, 739 unk
  dat[dat$MCL>=180 & dat$sex==3,]  #only 7 individuals that were captured as adults but not assigned a sex

#Retain only a single capture each year (using MCL at first capture)
  datyr <- ddply(dat,.(plot,tort,sex,yr),summarize,MCL=MCL[1])
  nrow(datyr) #4146 captures
  length(unique(datyr$tort)) #2035 individuals
  #Add stage (adult/juvenile) assignment
  datyr$stage <- ifelse(datyr$MCL<180,1,2)
  
  #Dataframe with number of tortoises caught at least once in each year a plot was surveyed
  plotyr <- ddply(datyr,.(plot,yr),summarize,ntorts=length(tort))

#Create matrix with capture histories 
#1=captured as juvenile; 2=captured as adult; 3=plot surveyed but tortoise not captured; NA=plot not surveyed)  
  cr <- dcast(datyr, plot + tort + sex ~ yr, value.var='stage')
  cr.mat <- cr[,4:ncol(cr)]
  names(cr.mat) <- paste0('y',names(cr.mat))
  #Add columns for years when no plots were surveyed (1989, 2009, 2011-2014)
  cr.mat <- cbind(cr.mat,data.frame(y1989=rep(NA,nrow(cr.mat)),y2009=NA,y2011=NA,y2012=NA,y2013=NA,y2014=NA))
  cr.mat <- cr.mat[,order(names(cr.mat))]
  #Change NAs to 3 when the plot was surveyed, but tortoise wasn't captured
  #NAs indicate plot wasn't surveyed that year
  for(i in 1:nrow(cr)){
    years <- which(surv.w.mat[surv.w$plot==cr$plot[i],]==1)
    for(j in years){
      cr.mat[i,j] <- ifelse(is.na(cr.mat[i,j]),3,cr.mat[i,j])
    }
  }
  cr <- cbind(cr[,1:3],cr.mat)

  # #Check: number of tortoises captured each year at each plot (same totals from datyr and cr dataframes?)
  # crcheck <- melt(cr,id.vars=c('plot','tort','sex'))
  # names(crcheck)[4] <- 'yr'
  # crcheck$yr <- as.numeric(substr(crcheck$yr,2,5))
  # crcheck <- ddply(crcheck,.(plot,yr),summarize,ntorts=length(which(value!=3)))
  # crcheck <- crcheck[crcheck$ntorts!=0,]
  # all.equal(crcheck$ntorts,plotyr$ntorts)
  
#-----------------------------------------------------------------------------------------------# 
# Function to create initial values for JAGS
#-----------------------------------------------------------------------------------------------# 

#Create vector indicating the first year each tortoise was caught as juvenile:
  first1 <- rep(NA,nrow(cr.mat))
  for(i in 1:nrow(cr.mat)){
    first1[i] <- (1:ncol(cr.mat))[!is.na(cr.mat[i,]) & cr.mat[i,]==1][1]
  }

#Create vector indicating the first year each tortoise was caught as adult:  
  first2 <- rep(NA,nrow(cr.mat))
  for(i in 1:nrow(cr.mat)){
    first2[i] <- (1:ncol(cr.mat))[!is.na(cr.mat[i,]) & cr.mat[i,]==2][1]
  }

#Create a matrix of initial values for latent states (z)
#NAs up to and including the first occasion, then 1's or 2's through the remainder
  ch.init <- function(y,f1,f2){
    for (i in 1:length(f1)){
      if(!is.na(f1[i]) & f1[i]==ncol(y)) {y[i,] <- NA} else
        if(is.na(f1[i]) & !is.na(f2[i]) & f2[i]==ncol(y)) {y[i,] <- NA} else
          if(is.na(f1[i]) & !is.na(f2[i]) & f2[i]!=ncol(y)) {y[i,1:f2[i]] <- NA; y[i,(f2[i]+1):ncol(y)] <- 2} else
            if(!is.na(f1[i]) & f1[i]!=ncol(y) & is.na(f2[i])) {y[i,1:f1[i]] <- NA; y[i,(f1[i]+1):ncol(y)] <- 1} else
              if(!is.na(f1[i]) & !is.na(f2[i]) & (f2[i]-f1[i]==1)) {y[i,1:f1[i]] <- NA; y[i,f2[i]:ncol(y)] <- 2} else
              {y[i,1:f1[i]] <- NA; y[i,(f1[i]+1):(f2[i]-1)] <- 1; y[i,f2[i]:ncol(y)] <- 2}}
    return(y)
  }

#-----------------------------------------------------------------------------------------------# 
# Create vector indicating the first year each torotise was captured
#-----------------------------------------------------------------------------------------------# 
#Create vector indicating the first year each tortoise was caught:
  first <- rep(NA,nrow(cr.mat))
  for(i in 1:nrow(cr.mat)){
    first[i] <- (1:ncol(cr.mat))[!is.na(cr.mat[i,]) & cr.mat[i,]!=3][1]
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
  sink("MS_NoCovars_NoREs.txt")
  cat("
    model{
      
      #-- Priors and constraints

      alpha.p1 ~ dlogis(0,1)
      alpha.p2 ~ dlogis(0,1)
      beta.phi1 ~ dlogis(0,1)
      beta.phi2 ~ dlogis(0,1)
      gamma.psi ~ dlogis(0,1)      
      
      for (i in 1:ntorts){
        for(t in first[i]:(nyears-1)){

          logit(p1[i,t]) <- alpha.p1
          logit(p2[i,t]) <- alpha.p2
          logit(phi1[i,t]) <- beta.phi1
          logit(phi2[i,t]) <- beta.phi2
          logit(psi12[i,t]) <- gamma.psi

          #Define state transition probabilities
          #First index = states at time t-1, last index = states at time t
          ps[1,i,t,1] <- phi1[i,t] * (1-psi12[i,t])
          ps[1,i,t,2] <- phi1[i,t] * psi12[i,t]
          ps[1,i,t,3] <- 1-phi1[i,t]
          ps[2,i,t,1] <- 0
          ps[2,i,t,2] <- phi2[i,t]
          ps[2,i,t,3] <- 1-phi2[i,t]
          ps[3,i,t,1] <- 0
          ps[3,i,t,2] <- 0
          ps[3,i,t,3] <- 1
    
          #Define stage-dependent detection probabilities
          #First index = states at time t, last index = detection type at time t
          po[1,i,t,1] <- p1[i,t]
          po[1,i,t,2] <- 0
          po[1,i,t,3] <- 1-p1[i,t]
          po[2,i,t,1] <- 0
          po[2,i,t,2] <- p2[i,t]
          po[2,i,t,3] <- 1-p2[i,t]
          po[3,i,t,1] <- 0
          po[3,i,t,2] <- 0
          po[3,i,t,3] <- 1      

        } #t
      } #i

      #-- Likelihood
      
      for(i in 1:ntorts){
        z[i,first[i]] <- y[i,first[i]]
        
        for (t in (first[i]+1):nyears){              
        
          #State process: draw State[t] given State[t-1]
          z[i,t] ~ dcat(ps[z[i,t-1],i,t-1,])
          
          #Observation process: draw Obs[t] given State[t]
          y[i,t] ~ dcat(po[z[i,t],i,t-1,])             
          
        } #t
      } #i

      #-- Derived parameters
      
      logit(psi12.mn) <- gamma.psi
      logit(phi1.mn) <- beta.phi1
      logit(phi2.mn) <- beta.phi2
      logit(p1.mn) <- alpha.p1
      logit(p2.mn) <- alpha.p2

    } #model
  ",fill=TRUE)
  sink()

#MCMC settings, parameters, initial values  
  ni <- 500; na <- 500; nb <- 500; nc <- 3; ni.tot <- ni + nb
	
	params <- c('alpha.p1','alpha.p2','beta.phi1','beta.phi2','gamma.psi',
	            'p1.mn','p2.mn','phi1.mn','phi2.mn','psi12.mn')
  
  inits <- function() {list(alpha.p1=runif(1,-1,1),
                            alpha.p2=runif(1,-1,2),
                            beta.phi1=runif(1,-1,1),
                            beta.phi2=runif(1,1,3),
                            gamma.psi=runif(1,-2,0),
                            z=ch.init(as.matrix(cr.mat),first1,first2))}

#Run model
	fit.ms0 <- jags(data=tortdata, inits=inits, parameters.to.save=params,
                  model.file='MS_NoCovars_NoREs.txt',
                  n.chains=nc, n.adapt=na, n.iter=ni.tot, n.burnin=nb,
                  parallel=T, n.cores=3, DIC=FALSE)  
	print(fit.ms0)
