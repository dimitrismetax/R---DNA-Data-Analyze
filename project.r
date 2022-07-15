#PROJECT R HY-390
#METAXAKIS-PRWIMAKHS
#install.packages("abc")
#install.packages("bayestestR")
library('bayestestR')
library('abc')
#edw oriste to working directory
#setwd("/home/metaxakis/Desktop/390_project") 
rm(list=ls())
#rithmizw to megisto output
options(max.print=1000000)
#diavazw ta simulation datasets
simulation <- read.table('ms_sim_final.out', header=FALSE,colClasses ="character")
#splitarw thn lista stoixeio pros stoixeio
for(i in 1:length(simulation))
{
  for(j in 1:length(simulation[[i]]))
  {
    sim=strsplit(simulation[[i]],"")
  }
}
sim

w = 0
a1 = 0
counter = 0 #metraei tis diafores metaxi allhlouxiwn
counter2 = 0 #vriskei ka8e pote exw 50ada apo allhlouxies
#matrix 2 sthlwn pou krataei ta 10000 k, w pou upologizontai parakatw sthn for
KW=matrix(1:10000,nrow=10000,ncol=2)
for(i in 1:10000){ #upologismos k, w
  for(j in 1:49){
    for(x in (j+1):50){
      for(y in 1:length(sim[[(i-1)*50+j]])){
        if(sim[[((i-1)*50)+j]][y] != sim[[((i-1)*50)+x]][y]){
          counter = counter + 1
          counter2=counter2+1
          if(counter2==50)
          {
            counter2=0
            # ypologizw W
            a1 = 0
            s = length(sim[[(i-1)*50+1]]) 
            for(m in 1:49){
              a1 = a1 + (1/m)
            }
            w=s/a1
            
          }
        }
      }
    }
  }  
  k=(counter/50)
  counter=0
  print(paste(k, w))
  KW[i,1] = k
  KW[i,2] = w
  KW
}
#grafw ta 10000 k, w se ena arxeio
write.table(KW, file="mymatrix2.txt", row.names=FALSE, col.names=FALSE)

#diavazw to observation dataset
observation <- read.table('ms_obs_final.out', header=FALSE,colClasses ="character")
#splitarw thn lista 
for(i in 1:length(observation))
{
  for(j in 1:length(observation[[i]]))
  {
    obs=strsplit(observation[[i]],"")
  }
}
obs
k=0
w = 0
counter = 0 #metraei tis diafores metaxi allhlouxiwn
KW2=matrix(1:1,nrow=1,ncol=2) # matrix 2 sthlwn gia to k kai w tou observation dataset
#upologizw to k kai w gia to observation dataset
for(j in 1:49){
  for(x in (j+1):50){
    for(y in 1:length(obs[[j]])){
      #print(paste((i)*50+j, k))
      if(obs[[j]][y] != obs[[x]][y]){
        counter = counter + 1
        # ypologizw W
        s = length(obs[[j]])
        a1=0
        for(m in 1:49){
          a1 = a1 + (1/m)
        }
        w=s/a1
      }
    }
  }
}  
k=(counter/50)
print(paste(k, w))
KW2[i,1] = k
KW2[i,2] = w
KW2
#grafw se ena arxeio to apotelesma twn k kai w
write.table(KW2, file="mymatrix.txt", row.names=FALSE, col.names=FALSE)
#diavazw to arxeio me ta dedomena pou apo8ikefsa gia to observation dataset
observation <- read.table('mymatrix.txt', header=FALSE)
observation
#diavazw to arxeio me ta dedomena pou apo8ikefsa gia ta 10000 simulations dataset
sims <- read.table("mymatrix2.txt", header=FALSE)
#dim(sims)
sims
#diavazw to arxeio me ta dedomena gia ta 10000 pars dataset
pars <- read.table("pars_final.txt", header=FALSE)
#sxdiazw ta dedomena gia ta 10000 datasets
plot(density(as.numeric(as.character(sims[,1])), as.numeric(as.character(sims[,2]))))
simstats <- sims[,]
simstats
names(simstats)

dim(observation)
dim(simstats)

#observation <- observation[,-1]
#simstats <- simstats[,-1]

dim(observation)
dim(simstats)
#erwthma1 upologizw ton ru8mo auxisis 
myabc = abc(target=observation, param=pars, sumstat=simstats, method="loclinear", tol=0.01)
summary(myabc)

#erwthma3 scediazw ta diagrammata 
d.prior = density(pars[,1])
d.pos1 <- density( myabc$unadj.values[,1])
d.pos = density( myabc$adj.values[,1])
#adjval <- density( myabc$adj.values[,1])
plot(d.prior$x, d.prior$y, ylim=c(0, max(d.prior$y, d.pos1$y, d.pos$y)), type='l', col='black')
points(d.pos1$x, d.pos1$y,  type='l', col='blue')
points(d.pos$x, d.pos$y,  type='l', col='red')

#erwthma2 upologizw tis times me to confidence level 95%
ci(myabc$adj.values[,1] , confidence=0.95, method = "HDI")

#tupwnw ta diagrammata se pdf arxeio
pdf("my_plot.pdf",         # File name
    width = 8, height = 7, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for most publications)
    paper = "A4")     
plot(density(as.numeric(as.character(sims[,1])), as.numeric(as.character(sims[,2]))))
plot(d.prior$x, d.prior$y, ylim=c(0, max(d.prior$y, d.pos1$y, d.pos$y)), type='l', col='black')
points(d.pos1$x, d.pos1$y,  type='l', col='blue')
points(d.pos$x, d.pos$y,  type='l', col='red')
dev.off() 

