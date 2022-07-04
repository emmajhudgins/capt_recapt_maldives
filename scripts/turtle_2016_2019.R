## Robust design analyses for Maldivian turtles - Emma J. Hudgins 2018. Adapted from:
#Santostasi et al.2016 
#doi:10.1371/journal.pone.0166650

library(RMark) 
# follow download instructions from http://www.phidot.org/software/mark/rmark/
require(plotrix)
require(R2ucare) # checks for violation of assumptions

# read in the matrix-day data
input=read.csv("./data/gr_covid.csv", header=TRUE, stringsAsFactors = F) #green turtle capture history
input2=read.csv("./data/hk_covid.csv", header=TRUE, stringsAsFactors = F) #hawksbill turtle capture history
colnames(input2)<-colnames(input)

#convert NA to 0
for (i in 6:ncol(input))
{
  input[which(is.na(input[,i])),i]<-0
  input2[which(is.na(input2[,i])),i]<-0
}

shoulder<-c(seq(5,ncol(input), by=6)) # set monsoon/non-monsoon intervals 


#function to run all Santostasi et al. models on turtles
runModels<-function(site,split, data, spp, time.intervals=time.intervals)
{
  #ROBUST DESIGN ANALYSES#
  # I run this entire script at once and just look at the summary of the output to see which model fits best. I refer to the paper I sent you to interpret the parameters.
  
  ### P(.)S(.)
  
  # Markovian emigration
  S.dot=list(formula=~1)
  p.dot=list(formula=~1,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.01=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaPrime=GammaPrime.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.dot),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  
  # Random emigration
  S.dot=list(formula=~1)
  p.dot=list(formula=~1,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE) # gamma' = gamma''
  model.02=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.dot),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  # No emigration
  S.dot=list(formula=~1)
  p.dot=list(formula=~1,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE,fixed=0) # gamma'=gamma''=0
  model.03=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.dot),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  
  ### P(T)S(.)
  
  # Markovian emigration
  S.dot=list(formula=~1)
  p.time=list(formula=~time,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.04=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaPrime=GammaPrime.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.time),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  
  # Random emigration
  S.dot=list(formula=~1)
  p.time=list(formula=~time,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE) # gamma' = gamma''
  model.05=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.time),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  # No emigration
  S.dot=list(formula=~1)
  p.time=list(formula=~time,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE,fixed=0) # gamma'=gamma''=0
  model.06=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.time),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  ### P(HET)S(.)
  
  # Markovian emigration with heterogeneity (mixture) on detection
  S.dot=list(formula=~1)
  pi.dot=list(formula=~1)
  p.het=list(formula=~mixture,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.07=mark(data, model = "RDHet",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaPrime=GammaPrime.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.het),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  # Random emigration with heterogeneity (mixture) on detection
  S.dot=list(formula=~1)
  pi.dot=list(formula=~1)
  p.het=list(formula=~mixture,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE)
  model.08=mark(data, model = "RDHet",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.het),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  # No emigration with heterogeneity (mixture) on detection
  S.dot=list(formula=~1)
  pi.dot=list(formula=~1)
  p.het=list(formula=~mixture,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE,fixed=0)
  model.09=mark(data, model = "RDHet",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.het),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  
  ### P (SESSION) S(.)
  
  # Markovian emigration
  S.dot=list(formula=~1)
  p.session=list(formula=~session,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.10=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaPrime=GammaPrime.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.session),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  
  # Random emigration
  S.dot=list(formula=~1)
  p.session=list(formula=~session,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE) # gamma' = gamma''
  model.11=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.session),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  # No emigration
  S.dot=list(formula=~1)
  p.session=list(formula=~session,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE,fixed=0) # gamma'=gamma''=0
  model.12=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.session),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  
  
  
  ### P (TIME.SESSION) S(.)
  
  # Markovian emigration
  S.dot=list(formula=~1)
  p.time.session=list(formula=~-1+session:time,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.13=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaPrime=GammaPrime.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.time.session),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  
  # Random emigration
  S.dot=list(formula=~1)
  p.time.session=list(formula=~-1+session:time,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE) # gamma' = gamma''
  model.14=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.time.session),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  # No emigration
  S.dot=list(formula=~1)
  p.time.session=list(formula=~-1+session:time,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE,fixed=0) # gamma'=gamma''=0
  model.15=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.time.session),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  ### P(.)S(T)
  
  # Markovian emigration
  S.time=list(formula=~time)
  p.dot=list(formula=~1,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.16=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaPrime=GammaPrime.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.dot),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  
  # Random emigration
  S.time=list(formula=~time)
  p.dot=list(formula=~1,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE) # gamma' = gamma''
  model.17=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.dot),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  # No emigration
  S.time=list(formula=~time)
  p.dot=list(formula=~1,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE,fixed=0) # gamma'=gamma''=0
  model.18=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.dot),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  ###P(T)S(T)
  
  # Markovian emigration
  S.time=list(formula=~time)
  p.time=list(formula=~time,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.19=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaPrime=GammaPrime.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.time),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  
  # Random emigration
  S.time=list(formula=~time)
  p.time=list(formula=~time,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE) # gamma' = gamma''
  model.20=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.time),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  # No emigration
  S.time=list(formula=~time)
  p.time=list(formula=~1,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE,fixed=0) # gamma'=gamma''=0
  model.21=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.time),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  ###P(HET)S(T)
  
  # Markovian emigration with heterogeneity (mixture) on detection
  S.time=list(formula=~time)
  pi.dot=list(formula=~1)
  p.het=list(formula=~mixture,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.22=mark(data, model = "RDHet",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaPrime=GammaPrime.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.het),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  # Random emigration with heterogeneity (mixture) on detection
  S.time=list(formula=~time)
  pi.dot=list(formula=~1)
  p.het=list(formula=~mixture,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE)
  model.23=mark(data, model = "RDHet",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.het),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  # No emigration with heterogeneity (mixture) on detection
  S.time=list(formula=~time)
  pi.dot=list(formula=~1)
  p.het=list(formula=~mixture,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE,fixed=0)
  model.24=mark(data, model = "RDHet",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.het),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  
  
  ## P (SESSION) S(T)
  
  # Markovian emigration
  S.time=list(formula=~time)
  p.session=list(formula=~session,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.25=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaPrime=GammaPrime.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.session),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  
  # Random emigration
  S.time=list(formula=~time)
  p.session=list(formula=~session,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE) # gamma' = gamma''
  model.26=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.session),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  # No emigration
  S.time=list(formula=~time)
  p.session=list(formula=~session,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE,fixed=0) # gamma'=gamma''=0
  model.27=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.session),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  ### P (TIME.SESSION) S(T)
  
  # Markovian emigration
  S.time=list(formula=~time)
  p.time.session=list(formula=~-1+session:time,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1)
  GammaPrime.dot=list(formula=~1)
  model.28=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaPrime=GammaPrime.dot,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.time.session),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  
  # Random emigration
  S.time=list(formula=~time)
  p.time.session=list(formula=~-1+session:time,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE) # gamma' = gamma''
  model.29=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.time.session),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  # No emigration
  S.time=list(formula=~time)
  p.time.session=list(formula=~-1+session:time,share=TRUE)
  GammaDoublePrime.dot=list(formula=~1,share=TRUE,fixed=0) # gamma'=gamma''=0
  model.30=mark(data, model = "Robust",
                time.intervals=time.intervals,
                model.parameters=list(S=S.time,
                                      GammaDoublePrime=GammaDoublePrime.dot,
                                      p=p.time.session),threads=2, output=F, delete=T, silent=T, brief=T, external=F)
  
  results<<-collect.models()
  saveRDS(results, file=paste0(site, "models2016", spp,split,".rds"))
  
  #once you look at the results, see which model number has the lowest AIC, 
  #then look at it directly with (summary(model.#))
}

observations<-list()
spp="hk"
for (i in c(6,16,92,95, 41,56))
{
pseudo.input<-subset(input2, Site==unique(input2$Site)[i])
pseudo.input<-pseudo.input[,c(6:53)] #remove unnecessary colums
if (nrow(pseudo.input)<10)
{
  next
}
split="full"
time.intervals<-rep(0,47)
time.intervals[shoulder[which(shoulder<length(time.intervals))]]<-1
observations[[i]]<-c(unique(input2$Site)[i],colnames(pseudo.input)[shoulder[which(shoulder<length(time.intervals))]])


# format for RMark analysis
# remove the lines of 0s
mask <- apply(pseudo.input,1,sum, na.rm=T)
pseudo.input <- pseudo.input[as.logical(mask),]

pseudo.input[is.na(pseudo.input)]<-0 
pseudo.input$ch <- apply(pseudo.input, 1 , paste , collapse = "" ) 

runModels(unique(input2$Site)[i],split, data=pseudo.input,spp, time.intervals=time.intervals)
times<-which(time.intervals==1)
times<-c(0, times,ncol(pseudo.input)-1)
pooled<-apply(pseudo.input,1, function(x){
  newvec<-0
  for (j in 1:(length(times)-1)){
    newvec[j]<-sum(as.numeric(x[(times[j]+1):times[j+1]]))
  }
  return(newvec)
})
pooled<-t(as.matrix(pooled))
pooled<-pooled[seq(1,nrow(pooled)-1,2),]+pooled[seq(2,nrow(pooled),2),]
overall<-overall_CJS(pooled, rep(2, nrow(pooled)))
c_hat<-unlist(overall[1]/overall[2])
if(overall[3]<0.05)
{results<<-adjust.chat(c_hat, results)}
bestmod<-as.integer(rownames(results$model.table)[1])
saveRDS(results[[bestmod]],file=paste0(unique(input2$Site)[i], "bestmod2016", spp,split,".rds") )
}
saveRDS(observations,file="timept_hk_observations2016.RDS")

spp="gr"
uniquesite<-as.data.frame(unique(cbind(input$Site, input$Atoll)))
twoatolls<-which(uniquesite[,2]%in%c("Laamu", "Lhaviyani"))
##create list of timepoints captured for each site

observations<-list()
for (i in twoatolls[c(15,14,21,3,2,1)])
{

  pseudo.input<-subset(input, Site==uniquesite[i,1])
  pseudo.input<-pseudo.input[,c(6:53)] #remove unnecessary colums
if (nrow(pseudo.input)<10)
{
  next
}
  split="full"
 time.intervals<-rep(0,47)
    time.intervals[shoulder[which(shoulder<length(time.intervals))]]<-1
    
    observations[[i]]<-c(as.character(uniquesite[i,1]),colnames(pseudo.input)[shoulder[which(shoulder<length(time.intervals))]])
  
    # format for RMark analysis
    # remove the lines of 0s
    mask <- apply(pseudo.input,1,sum, na.rm=T)
    pseudo.input <- pseudo.input[as.logical(mask),]
    
    pseudo.input[is.na(pseudo.input)]<-0 
    pseudo.input$ch <- apply(pseudo.input, 1 , paste , collapse = "" ) 
    
    runModels(uniquesite[i,1],split, data=pseudo.input, spp, time.intervals=time.intervals)
    times<-which(time.intervals==1)
    times<-c(0, times,ncol(pseudo.input)-1)
    pooled<-apply(pseudo.input,1, function(x){
      newvec<-0
      for (j in 1:(length(times)-1)){
        newvec[j]<-sum(as.numeric(x[(times[j]+1):times[j+1]]))
      }
      return(newvec)
    })
    pooled<-t(as.matrix(pooled))
    pooled<-pooled[seq(1,nrow(pooled)-1,2),]+pooled[seq(2,nrow(pooled),2),]
    overall<-overall_CJS(pooled, rep(2, nrow(pooled)))
    c_hat<-unlist(overall[1]/overall[2])
      if(overall[3]<0.05)
    {results<<-adjust.chat(c_hat, results)}
    bestmod<-as.integer(rownames(results$model.table)[1])
    saveRDS(results[[bestmod]],file=paste0(uniquesite[i,1], "bestmod2016", spp,split,".rds") )
}


saveRDS(observations,file="timept_gr_observations2016.RDS")

spp="hk_atoll"
observations<-list()
for (i in 1:2)
 {
  pseudo.input<-subset(input2, Atoll==c("Laamu", "Lhaviyani")[i])
  
  pseudo.input<-pseudo.input[,c(6:53)] #remove unnecessary colums
  if (nrow(pseudo.input)<10)
  {
    next
  }
    split="full"
    time.intervals<-rep(0, 47)
    time.intervals[shoulder[which(shoulder<length(time.intervals))]]<-1
    
    observations[[i]]<-c(c("Laamu", "Lhaviyani")[i],colnames(pseudo.input)[shoulder[which(shoulder<length(time.intervals))]])
    # format for RMark analysis
    # remove the lines of 0s
    mask <- apply(pseudo.input,1,sum, na.rm=T)
    pseudo.input <- pseudo.input[as.logical(mask),]
    
    pseudo.input[is.na(pseudo.input)]<-0 
    pseudo.input$ch <- apply(pseudo.input, 1 , paste , collapse = "" ) 
    
    runModels(site=c("Laamu", "Lhaviyani")[i],split, data=pseudo.input,spp, time.intervals = time.intervals)
    
    bestmod<-as.integer(rownames(results$model.table)[1])
    times<-which(time.intervals==1)
    times<-c(0, times,ncol(pseudo.input)-1)
    pooled<-apply(pseudo.input,1, function(x){
      newvec<-0
      for (j in 1:(length(times)-1)){
        newvec[j]<-sum(as.numeric(x[(times[j]+1):times[j+1]]))
      }
      return(newvec)
    })
    pooled<-t(as.matrix(pooled))
    pooled<-pooled[seq(1,nrow(pooled)-2,3),]+pooled[seq(2,nrow(pooled)-1,3),]+pooled[seq(3,nrow(pooled),3),]#upped to 3 for Laamu
    overall<-overall_CJS(pooled, rep(3, nrow(pooled)))
    c_hat<-unlist(overall[1]/overall[2])
    if(overall[3]<0.05)
    {results<<-adjust.chat(c_hat, results)}
    bestmod<-as.integer(rownames(results$model.table)[1])
    saveRDS(results[[bestmod]],file=paste0(c("Laamu", "Lhaviyani")[i], "bestmod2016", spp,split,".rds") )
  }

saveRDS(observations,file="timept_atollhk_observations2016.RDS")

spp="gr_atoll"
observations<-list()
for (i in 1:2)
{
  pseudo.input<-subset(input, Atoll==c("Laamu", "Lhaviyani")[i])
  
  pseudo.input<-pseudo.input[,c(6:53)] #remove unnecessary colums
  if (nrow(pseudo.input)<10)
  {
    next
  }
 
    split="full"
    time.intervals<-rep(0, 47)
    time.intervals[shoulder[which(shoulder<length(time.intervals))]]<-1
    observations[[i]]<-c(c("Laamu", "Lhaviyani")[i],colnames(pseudo.input)[shoulder[which(shoulder<length(time.intervals))]])
    
    # format for RMark analysis
    # remove the lines of 0s
    mask <- apply(pseudo.input,1,sum, na.rm=T)
    pseudo.input <- pseudo.input[as.logical(mask),]
    
    pseudo.input[is.na(pseudo.input)]<-0 
    pseudo.input$ch <- apply(pseudo.input, 1 , paste , collapse = "" ) 
    
    runModels(site=c("Laamu", "Lhaviyani")[i],split, data=pseudo.input,spp, time.intervals = time.intervals)
    bestmod<-as.integer(rownames(results$model.table)[1])
    times<-which(time.intervals==1)
    times<-c(0, times,ncol(pseudo.input)-1)
    pooled<-apply(pseudo.input,1, function(x){
      newvec<-0
      for (j in 1:(length(times)-1)){
        newvec[j]<-sum(as.numeric(x[(times[j]+1):times[j+1]]))
      }
      return(newvec)
    })
    pooled<-t(as.matrix(pooled))
    pooled<-pooled[seq(1,nrow(pooled)-2,3),]+pooled[seq(2,nrow(pooled)-1,3),]+pooled[seq(3,nrow(pooled),3),]#upped to 3 for Laamu
    overall<-overall_CJS(pooled, rep(3, nrow(pooled)))
    c_hat<-unlist(overall[1]/overall[2])
    if(overall[3]<0.05)
    {results<<-adjust.chat(c_hat, results)}
    bestmod<-as.integer(rownames(results$model.table)[1])
    saveRDS(results[[bestmod]],file=paste0(c("Laamu", "Lhaviyani")[i], "bestmod2016", spp,split,".rds") )
  }
saveRDS(observations,file="timept_atollgr_observations2016.RDS")


