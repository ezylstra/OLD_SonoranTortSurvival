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
# Run simple multistate model in JAGS, no covariates, no random effects
#-----------------------------------------------------------------------------------------------# 
#Prep data objects for JAGS (no covariates, no random effects)
  tortdata <- list(y=as.matrix(cr.mat),
                   ntorts=nrow(cr.mat),
                   nyears=ncol(cr.mat),
                   first=first)
  
#JAGS model: no covariates, no random effects	
  # sink("MS_NoCovars_NoREs.txt")
  # cat("
  #   model{
  #     
  #     #-- Priors and constraints
  # 
  #     alpha.p1 ~ dlogis(0,1)
  #     alpha.p2 ~ dlogis(0,1)
  #     beta.phi1 ~ dlogis(0,1)
  #     beta.phi2 ~ dlogis(0,1)
  #     gamma.psi ~ dlogis(0,1)      
  #     
  #     for (i in 1:ntorts){
  #       for(t in first[i]:(nyears-1)){
  # 
  #         logit(p1[i,t]) <- alpha.p1
  #         logit(p2[i,t]) <- alpha.p2
  #         logit(phi1[i,t]) <- beta.phi1
  #         logit(phi2[i,t]) <- beta.phi2
  #         logit(psi12[i,t]) <- gamma.psi
  # 
  #         #Define state transition probabilities
  #         #First index = states at time t-1, last index = states at time t
  #         ps[1,i,t,1] <- phi1[i,t] * (1-psi12[i,t])
  #         ps[1,i,t,2] <- phi1[i,t] * psi12[i,t]
  #         ps[1,i,t,3] <- 1-phi1[i,t]
  #         ps[2,i,t,1] <- 0
  #         ps[2,i,t,2] <- phi2[i,t]
  #         ps[2,i,t,3] <- 1-phi2[i,t]
  #         ps[3,i,t,1] <- 0
  #         ps[3,i,t,2] <- 0
  #         ps[3,i,t,3] <- 1
  #   
  #         #Define stage-dependent detection probabilities
  #         #First index = states at time t, last index = detection type at time t
  #         po[1,i,t,1] <- p1[i,t]
  #         po[1,i,t,2] <- 0
  #         po[1,i,t,3] <- 1-p1[i,t]
  #         po[2,i,t,1] <- 0
  #         po[2,i,t,2] <- p2[i,t]
  #         po[2,i,t,3] <- 1-p2[i,t]
  #         po[3,i,t,1] <- 0
  #         po[3,i,t,2] <- 0
  #         po[3,i,t,3] <- 1      
  # 
  #       } #t
  #     } #i
  # 
  #     #-- Likelihood
  #     
  #     for(i in 1:ntorts){
  #       z[i,first[i]] <- y[i,first[i]]
  #       
  #       for (t in (first[i]+1):nyears){              
  #       
  #         #State process: draw State[t] given State[t-1]
  #         z[i,t] ~ dcat(ps[z[i,t-1],i,t-1,])
  #         
  #         #Observation process: draw Obs[t] given State[t]
  #         y[i,t] ~ dcat(po[z[i,t],i,t-1,])             
  #         
  #       } #t
  #     } #i
  # 
  #     #-- Derived parameters
  #     
  #     logit(psi12.mn) <- gamma.psi
  #     logit(phi1.mn) <- beta.phi1
  #     logit(phi2.mn) <- beta.phi2
  #     logit(p1.mn) <- alpha.p1
  #     logit(p2.mn) <- alpha.p2
  # 
  #   } #model
  # ",fill=TRUE)
  # sink()

#MCMC settings, parameters, initial values  
  ni <- 20000; na <- 2000; nb <- 10000; nc <- 3; ni.tot <- ni + nb
  # ni <- 500; na <- 500; nb <- 500; nc <- 3; ni.tot <- ni + nb
	
	params <- c('alpha.p1','alpha.p2','beta.phi1','beta.phi2','gamma.psi',
	            'p1.mn','p2.mn','phi1.mn','phi2.mn','psi12.mn')
  
  inits <- function() {list(alpha.p1=runif(1,-1,1),
                            alpha.p2=runif(1,-1,2),
                            beta.phi1=runif(1,-1,1),
                            beta.phi2=runif(1,1,3),
                            gamma.psi=runif(1,-2,0),
                            z=ch.init(as.matrix(cr.mat),first1,first2))}

#Run model
# 	fit.ms0 <- jags(data=tortdata, inits=inits, parameters.to.save=params,
#                   model.file='MS_NoCovars_NoREs.txt',
#                   n.chains=nc, n.adapt=na, n.iter=ni.tot, n.burnin=nb,
#                   parallel=T, n.cores=3, DIC=FALSE)  
# 	print(fit.ms0)

#-----------------------------------------------------------------------------------------------# 
# Run multistate model in JAGS, with covariates (fixed effects), no random effects
#-----------------------------------------------------------------------------------------------#
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
	
#Plot index for each tortoise
	plots <- data.frame(plot=unique(cr$plot),plot.index=1:length(unique(cr$plot)))
  plot.index <- plots$plot.index[match(cr$plot,plots$plot)]	

#Prep data objects for JAGS
  ntorts <- nrow(cr.mat)             #number of tortoises
  nyears <- ncol(cr.mat)             #number of occasions
  nplots <- length(unique(cr$plot))  #number of plots	
  
  tortdata <- list(y=as.matrix(cr.mat),
                   ntorts=ntorts,
                   nyears=nyears,
                   first=first,
                   male=male.ind,
                   plot=plot.index,
                   distance=distance,
                   mean.precip=precip.norm,
                   drought=pdsi24.z,
                   precip=ppt.mat)

#JAGS model: covariates on adult and juvenile parameters (assuming constant transition rates)	
  # sink("MS_CovarsAdultJuv.txt")
  # cat("
  #   model{
  # 
  #     #-- Priors and constraints
  # 
  #     alpha.p1 ~ dlogis(0,1)
  #     alpha.p2 ~ dlogis(0,1)
  #     beta.phi1 ~ dlogis(0,1)
  #     beta.phi2 ~ dlogis(0,1)
  #     gamma.psi ~ dlogis(0,1)
  # 
  #     a1.precip ~ dnorm(0,0.1)
  #     a2.male ~ dnorm(0,0.1)
  #     a2.precip ~ dnorm(0,0.1)
  #     b1.mnprecip ~ dnorm(0,0.1)
  #     b2.male ~ dnorm(0,0.1)
  #     b2.distance ~ dnorm(0,0.1)
  #     b2.mnprecip ~ dnorm(0,0.1)
  #     b2.drought ~ dnorm(0,0.1)
  #     b2.int ~ dnorm(0,0.1)
  #     psi.male ~ dunif(0,1)
  # 
  #     for (i in 1:ntorts){
  #       for(t in first[i]:(nyears-1)){
  # 
  #         logit(p1[i,t]) <- alpha.p1 + a1.precip*precip[plot[i],t]
  #         logit(phi1[i,t]) <- beta.phi1 + b1.mnprecip*mean.precip[plot[i]]
  # 
  #         logit(psi12[i,t]) <- gamma.psi
  # 
  #         logit(p2[i,t]) <- alpha.p2 + a2.male*male[i] + a2.precip*precip[plot[i],t]
  #         logit(phi2[i,t]) <- beta.phi2 + b2.male*male[i] + b2.distance*distance[plot[i]] +
  #                             b2.mnprecip*mean.precip[plot[i]] + b2.drought*drought[plot[i],t] +
  #                             b2.int*mean.precip[plot[i]]*drought[plot[i],t]
  # 
  #         #Define state transition probabilities
  #         #First index = states at time t-1, last index = states at time t
  #         ps[1,i,t,1] <- phi1[i,t] * (1-psi12[i,t])
  #         ps[1,i,t,2] <- phi1[i,t] * psi12[i,t]
  #         ps[1,i,t,3] <- 1-phi1[i,t]
  #         ps[2,i,t,1] <- 0
  #         ps[2,i,t,2] <- phi2[i,t]
  #         ps[2,i,t,3] <- 1-phi2[i,t]
  #         ps[3,i,t,1] <- 0
  #         ps[3,i,t,2] <- 0
  #         ps[3,i,t,3] <- 1
  # 
  #         #Define stage-dependent detection probabilities
  #         #First index = states at time t, last index = detection type at time t
  #         po[1,i,t,1] <- p1[i,t]
  #         po[1,i,t,2] <- 0
  #         po[1,i,t,3] <- 1-p1[i,t]
  #         po[2,i,t,1] <- 0
  #         po[2,i,t,2] <- p2[i,t]
  #         po[2,i,t,3] <- 1-p2[i,t]
  #         po[3,i,t,1] <- 0
  #         po[3,i,t,2] <- 0
  #         po[3,i,t,3] <- 1
  # 
  #       } #t
  #     } #i
  # 
  #     #-- Likelihood
  # 
  #     for(i in 1:ntorts){
  #       z[i,first[i]] <- y[i,first[i]]
  #       male[i] ~ dbern(psi.male)
  # 
  #       for (t in (first[i]+1):nyears){
  # 
  #         #State process: draw State[t] given State[t-1]
  #         z[i,t] ~ dcat(ps[z[i,t-1],i,t-1,])
  # 
  #         #Observation process: draw Obs[t] given State[t]
  #         y[i,t] ~ dcat(po[z[i,t],i,t-1,])
  # 
  #       } #t
  #     } #i
  # 
  #     #-- Derived parameters
  # 
  #     logit(psi12.mn) <- gamma.psi
  #     logit(phi1.mn) <- beta.phi1
  #     logit(p1.mn) <- alpha.p1
  # 
  #     phi2.f <- exp(beta.phi2)/(1 + exp(beta.phi2))
  #     phi2.m <- exp(beta.phi2 + b2.male)/(1 + exp(beta.phi2 + b2.male))
  #     p2.f <- exp(alpha.p2)/(1 + exp(alpha.p2))
  #     p2.m <- exp(alpha.p2 + a2.male)/(1 + exp(alpha.p2 + a2.male))
  # 
  #   } #model
  # ",fill=TRUE)
  # sink()
  
#MCMC settings, parameters, initial values  
  ni <- 20000; na <- 2000; nb <- 10000; nc <- 3; ni.tot <- ni + nb
  # ni <- 500; na <- 500; nb <- 500; nc <- 3; ni.tot <- ni + nb
	
	params <- c('alpha.p1','a1.precip',
	            'beta.phi1','b1.mnprecip',
	            'gamma.psi',
	            'alpha.p2','a2.male','a2.precip',
	            'beta.phi2','b2.male','b2.distance','b2.mnprecip','b2.drought','b2.int',
	            'psi.male',
	            'p1.mn','phi1.mn','psi12.mn','phi2.f','phi2.m','p2.f','p2.m')

  inits <- function() {list(alpha.p1=runif(1,-1,1),
                            alpha.p2=runif(1,-1,2),
                            beta.phi1=runif(1,-1,1),
                            beta.phi2=runif(1,1,3),
                            gamma.psi=runif(1,-2,0),
                            a1.precip=runif(1,-0.5,0.5),
                            a2.male=runif(1,-0.5,0.5),
                            a2.precip=runif(1,-0.5,0.5),
                            b1.mnprecip=runif(1,-0.5,0.5),
                            b2.male=runif(1,-0.5,0.5),
                            b2.distance=runif(1,-0.5,0.5),
                            b2.mnprecip=runif(1,-0.5,0.5),
                            b2.drought=runif(1,-0.5,0.5),
                            b2.int=runif(1,-0.5,0.5),
                            psi.male=runif(1,0,1),
                            male=ifelse(is.na(male.ind),1,NA),
                            z=ch.init(as.matrix(cr.mat),first1,first2))}
  
#Run model
# 	fit.ms1 <- jags(data=tortdata, inits=inits, parameters.to.save=params,
#                   model.file='MS_CovarsAdultJuv.txt',
#                   n.chains=nc, n.adapt=na, n.iter=ni.tot, n.burnin=nb,
#                   parallel=T, n.cores=3, DIC=FALSE)  
# 	print(fit.ms1)  
	
#-----------------------------------------------------------------------------------------------# 
# Run multistate model in JAGS, with covariates (fixed effects) and random site effects
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

#JAGS model: covariates on adult and juvenile parameters, random site effects	
  # sink("MS_CovarsAdultJuv_siteREs.txt")
  # cat("
  #   model{
  # 
  #     #-- Priors and constraints
  # 
  #     alpha.p1 ~ dlogis(0,1)
  #     alpha.p2 ~ dlogis(0,1)
  #     beta.phi1 ~ dlogis(0,1)
  #     beta.phi2 ~ dlogis(0,1)
  #     gamma.psi ~ dlogis(0,1)
  # 
  #     a1.precip ~ dnorm(0,0.1)
  #     a2.male ~ dnorm(0,0.1)
  #     a2.precip ~ dnorm(0,0.1)
  #     b1.mnprecip ~ dnorm(0,0.1)
  #     b2.male ~ dnorm(0,0.1)
  #     b2.distance ~ dnorm(0,0.1)
  #     b2.mnprecip ~ dnorm(0,0.1)
  #     b2.drought ~ dnorm(0,0.1)
  #     b2.int ~ dnorm(0,0.1)
  #     psi.male ~ dunif(0,1)
  # 
  #     sigma.site ~ dt(0,pow(2.5,-2),1)T(0,)  #Half-cauchy prior
  #     tau.site <- 1/(sigma.site*sigma.site)
  # 
  #     for(p in 1:nplots){
  #       e.site[p] ~ dnorm(0,tau.site)
  #     }
  # 
  #     for (i in 1:ntorts){
  #       for(t in first[i]:(nyears-1)){
  # 
  #         logit(p1[i,t]) <- alpha.p1 + a1.precip*precip[plot[i],t]
  #         logit(phi1[i,t]) <- beta.phi1 + b1.mnprecip*mean.precip[plot[i]]
  # 
  #         logit(psi12[i,t]) <- gamma.psi
  # 
  #         logit(p2[i,t]) <- alpha.p2 + a2.male*male[i] + a2.precip*precip[plot[i],t]
  #         logit(phi2[i,t]) <- beta.phi2 + b2.male*male[i] + b2.distance*distance[plot[i]] +
  #                             b2.mnprecip*mean.precip[plot[i]] + b2.drought*drought[plot[i],t] +
  #                             b2.int*mean.precip[plot[i]]*drought[plot[i],t] + e.site[plot[i]]
  # 
  #         #Define state transition probabilities
  #         #First index = states at time t-1, last index = states at time t
  #         ps[1,i,t,1] <- phi1[i,t] * (1-psi12[i,t])
  #         ps[1,i,t,2] <- phi1[i,t] * psi12[i,t]
  #         ps[1,i,t,3] <- 1-phi1[i,t]
  #         ps[2,i,t,1] <- 0
  #         ps[2,i,t,2] <- phi2[i,t]
  #         ps[2,i,t,3] <- 1-phi2[i,t]
  #         ps[3,i,t,1] <- 0
  #         ps[3,i,t,2] <- 0
  #         ps[3,i,t,3] <- 1
  # 
  #         #Define stage-dependent detection probabilities
  #         #First index = states at time t, last index = detection type at time t
  #         po[1,i,t,1] <- p1[i,t]
  #         po[1,i,t,2] <- 0
  #         po[1,i,t,3] <- 1-p1[i,t]
  #         po[2,i,t,1] <- 0
  #         po[2,i,t,2] <- p2[i,t]
  #         po[2,i,t,3] <- 1-p2[i,t]
  #         po[3,i,t,1] <- 0
  #         po[3,i,t,2] <- 0
  #         po[3,i,t,3] <- 1
  # 
  #       } #t
  #     } #i
  # 
  #     #-- Likelihood
  # 
  #     for(i in 1:ntorts){
  #       z[i,first[i]] <- y[i,first[i]]
  #       male[i] ~ dbern(psi.male)
  # 
  #       for (t in (first[i]+1):nyears){
  # 
  #         #State process: draw State[t] given State[t-1]
  #         z[i,t] ~ dcat(ps[z[i,t-1],i,t-1,])
  # 
  #         #Observation process: draw Obs[t] given State[t]
  #         y[i,t] ~ dcat(po[z[i,t],i,t-1,])
  # 
  #       } #t
  #     } #i
  # 
  #     #-- Derived parameters
  # 
  #     logit(psi12.mn) <- gamma.psi
  #     logit(phi1.mn) <- beta.phi1
  #     logit(p1.mn) <- alpha.p1
  # 
  #     phi2.f <- exp(beta.phi2)/(1 + exp(beta.phi2))
  #     phi2.m <- exp(beta.phi2 + b2.male)/(1 + exp(beta.phi2 + b2.male))
  #     p2.f <- exp(alpha.p2)/(1 + exp(alpha.p2))
  #     p2.m <- exp(alpha.p2 + a2.male)/(1 + exp(alpha.p2 + a2.male))
  # 
  #   } #model
  # ",fill=TRUE)
  # sink()

#MCMC settings, parameters, initial values  
  ni <- 20000; na <- 2000; nb <- 10000; nc <- 3; ni.tot <- ni + nb
  # ni <- 500; na <- 500; nb <- 500; nc <- 3; ni.tot <- ni + nb

	params <- c('alpha.p1','a1.precip',
	            'beta.phi1','b1.mnprecip',
	            'gamma.psi',
	            'alpha.p2','a2.male','a2.precip',
	            'beta.phi2','b2.male','b2.distance','b2.mnprecip','b2.drought','b2.int',
	            'psi.male','sigma.site','e.site',
	            'p1.mn','phi1.mn','psi12.mn',
	            'phi2.f','phi2.m','p2.f','p2.m')

  inits <- function() {list(alpha.p1=runif(1,-1,1),
                            alpha.p2=runif(1,-1,2),
                            beta.phi1=runif(1,-1,1),
                            beta.phi2=runif(1,1,3),
                            gamma.psi=runif(1,-2,0),
                            a1.precip=runif(1,-0.5,0.5),
                            a2.male=runif(1,-0.5,0.5),
                            a2.precip=runif(1,-0.5,0.5),
                            b1.mnprecip=runif(1,-0.5,0.5),
                            b2.male=runif(1,-0.5,0.5),
                            b2.distance=runif(1,-0.5,0.5),
                            b2.mnprecip=runif(1,-0.5,0.5),
                            b2.drought=runif(1,-0.5,0.5),
                            b2.int=runif(1,-0.5,0.5),
                            psi.male=runif(1,0,1),
                            sigma.site=runif(1,0,3),
                            male=ifelse(is.na(male.ind),1,NA),
                            z=ch.init(as.matrix(cr.mat),first1,first2))}

#Run model
# 	fit.ms2 <- jags(data=tortdata, inits=inits, parameters.to.save=params,
#                   model.file='MS_CovarsAdultJuv_siteREs.txt',
#                   n.chains=nc, n.adapt=na, n.iter=ni.tot, n.burnin=nb,
#                   parallel=T, n.cores=3, DIC=FALSE)  
# 	print(fit.ms2)  
	
#-----------------------------------------------------------------------------------------------# 
# Calculating population growth rates 
#-----------------------------------------------------------------------------------------------#	
#Start with a simple case, where we assume a known, fixed recruitment rate: 0.32 female/female/yr (Campbell et al. 2018)

#Load JAGS object from previous run of a multi-state model:
  load('JAGSFit_MS_CovarsAdultJuv_2021-04-24.Rdata')
  
#Create a matrix of posterior samples:
  out <- fit.ms$samples
  comb <- combine.mcmc(out)
  niter <- 1000
  sample <- sample(1:nrow(comb),size=niter,replace=FALSE)
  phi1.s <- comb[sample,c('beta.phi1','b1.mnprecip')]
  phi2.s <- comb[sample,c('beta.phi2','b2.distance','b2.mnprecip','b2.drought','b2.int')]
  psi12.s <- comb[sample,c('gamma.psi')]

#Gather plot-specific covariate values (and standardize values)
  plotcovs <- join(disttocity[,c('plot','dist.km')],precip.norms,by='plot')
  plotcovs$dist.z <- (plotcovs$dist.km - dist.mn)/dist.sd
  plotcovs$mnprecip.z <- (plotcovs$ppt.mm - precipnorm.mn)/precipnorm.sd
  #For each plot, using 3 drought values (-3, 0, +3)
  phi.Xdf <- plotcovs[rep(seq_len(nrow(plotcovs)),each=3),] 
  phi.Xdf$drought <- rep(c(-3,0,3),nrow(plotcovs))
  phi.Xdf$drought.z <- (phi.Xdf$drought - pdsi24.mn)/pdsi24.sd
  phi.Xdf$int.z <- phi.Xdf$mnprecip.z*phi.Xdf$drought.z
  
  #Create a matrix of covariates values for juvenile survival (including a column of 1's for the intercept)
  phi1.X <- as.matrix(phi2.Xdf[,c('mnprecip.z')])
  phi1.X <- cbind(rep(1,nrow(phi1.X)),phi1.X)
  
  #Create a matrix of covariates values for adult survival (including a column of 1's for the intercept)  
  phi2.X <- as.matrix(phi2.Xdf[,c('dist.z','mnprecip.z','drought.z','int.z')])
  phi2.X <- cbind(rep(1,nrow(phi2.X)),phi2.X)
  
#Calculate survival estimates for each plot-drought combination, iteration
  lphi1 <- phi1.X %*% t(phi1.s)
  phi1 <- exp(lphi1)/(1+exp(lphi1))
  lphi2 <- phi2.X %*% t(phi2.s)
  phi2 <- exp(lphi2)/(1+exp(lphi2))
  
#Calculate transition estimates for each iteration
  psi12 <- exp(psi12.s)
  
#Calculate population projection matrices, and lambda values, for each plot-drought combination & iteration
  lambda <- matrix(NA,nrow=nrow(phi2),ncol=niter)
  
  for(i in 1:nrow(phi2)){ 
    for (j in 1:niter){
      proj.mat <- matrix(c(phi1[i,j]*(1-psi12[j]), 0.32,
                           phi1[i,j]*psi12[j], phi2[i,j]),
                         nrow=2,ncol=2,byrow=TRUE)
      lambda[i,j] <- eigen(proj.mat)$values[1]
    }
  }
  
#Summarize distributions of lambda values for each plot-drought combination
  lambda.df <- phi.Xdf[,c('plot','drought')]
  lambda.df$mn <- apply(lambda,1,mean)
  lambda.df$q0.025 <- apply(lambda,1,quantile,0.025)
  lambda.df$q0.05 <- apply(lambda,1,quantile,0.05)
  lambda.df$q0.5 <- apply(lambda,1,quantile,0.5)
  lambda.df$q0.95 <- apply(lambda,1,quantile,0.95)
  lambda.df$q0.975 <- apply(lambda,1,quantile,0.975)
  (lambda.df <- lambda.df[with(lambda.df,order(drought,plot)),])

	
	