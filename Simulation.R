library(data.table)


lamda_cm<-11 # expected mean number of observed falls at 12 months
lamda_cg<-lamda_cm*1.3 # increase by 30% (according to sample size calculation in the "Evaluationskonzept")
n<-1000

## Simulate number of falls in cm and cg group at 3, 6, 9, and 12 months
set.seed(1)
rr<-rbind(data.table("id"=1:n, "month"=3, "cm"=rpois(n,lamda_cm/4),"cg"=rpois(n,lamda_cg/4)),
          data.table("id"=1:n, "month"=6,"cm"=rpois(n,lamda_cm/4),"cg"=rpois(n,lamda_cg/4)),
          data.table("id"=1:n, "month"=9,"cm"=rpois(n,lamda_cm/4),"cg"=rpois(n,lamda_cg/4)),
          data.table("id"=1:n, "month"=12,"cm"=rpois(n,lamda_cm/4),"cg"=rpois(n,lamda_cg/4)))

rrl<-melt(rr,id=c("id","month"),variable.name="Group",value.name="Falls")
rrl[,id2:=paste(Group,id,sep="_")]
setorder(rrl,id2,month)
rrl[,cumFalls:=cumsum(Falls),by="id2"]
setnames(rrl,old="month",new="time")
rrl[,entry:=shift(time,type="lag",fill = 0),by="id2"]
rrl[,status:=as.numeric(cumFalls>0)]
table(rrl$status)
head(rrl)

## Poisson regression
fitpois<-glm(cumFalls~ Group+offset(log(time)),data=rrl, family="poisson" )
summary(fitpois)


# Test zero inflation
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = fitpois)
# the plot function runs 4 tests
# i) KS test i) Dispersion test iii) Outlier test iv) quantile test
testZeroInflation(simulationOutput)
