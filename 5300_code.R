library(data.table)
library(DT)
set.seed(1031)
library(dplyr)

B<-1000

#FUNCTION TWO SIDED T-TEST
analyze.experiment.twoside <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x=the.dat[Group=='Treatment', response_mean], y=the.dat[Group=='Control', response_mean],
                     alternative='two.sided')
  the.effect <- the.test$estimate[1]-the.test$estimate[2]
  upper.bound <- the.test$conf.int[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, upper_ci = upper.bound, p = p)
  return(result)
  
}

#FUNCTION ONE TAIL T-TEST (LESS)
analyze.experiment.less <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x=the.dat[Group=='Treatment', response_mean], y=the.dat[Group=='Control', response_mean],
                     alternative='less')
  the.effect <- the.test$estimate[1]-the.test$estimate[2]
  upper.bound <- the.test$conf.int[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, upper_ci = upper.bound, p = p)
  return(result)
  
}

#FUNCTION OF ONE-SIDED T-TEST (TWO SIDED)
# For Q3 Purchase Intention
analyze.experiment.oneside <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x=the.dat$response_mean, alternative='less', mu = 4)
  the.effect <- the.test$estimate[1]-the.test$estimate[2]
  upper.bound <- the.test$conf.int[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, upper_ci = upper.bound, p = p)
  return(result)
  
}

# For Q3 Perception

analyze.experiment.percp_3 <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x=the.dat$response_mean, alternative='two.sided', mu = 5)
  the.effect <- the.test$estimate[1]-5
  upper.bound <- the.test$conf.int[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, upper_ci = upper.bound, p = p)
  return(result)
  
}

#FUNCTION OF ONE-SAMPLE T-TEST (TWO SIDED)
analyze.experiment.oneside <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x=the.dat$response_mean, alternative='two.sided', mu = 4)
  the.effect <- the.test$estimate[1]-4
  upper.bound <- the.test$conf.int[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, upper_ci = upper.bound, p = p)
  return(result)
  
}

#Effect Code

#First Research Question

## Purchase Intent
n<-600

ecig1.dat <- data.table(Group = c(rep.int(x='Treatment', times=n*2/3), rep.int(x='Control', times=n/3)))

ecig1.dat[Group=='Control', response_mean:= round(x=rnorm(n=.N, mean=4, sd=2.8), digits=1)]
ecig1.dat[Group=='Treatment', response_mean:= round(x=rnorm(n=.N, mean=3.2, sd=3.1), digits=1)]

datatable(data = ecig1.dat)

analyze.experiment.twoside(the.dat = ecig1.dat)

B <- 1000
RNGversion(vstr=3.6)
Experiment <- 1:B
Group <- c(rep.int(x='Treatment', times = n*2/3), rep.int(x='Control', times=n/3))

sim.dat1 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x=sim.dat1, cols = c('Experiment', 'Group'), order=c(1,1))
sim.dat1[Group == "Control", response_mean := round(x = rnorm(n = .N, mean = 4, sd = 2.8), digits = 3)]
sim.dat1[Group == "Treatment", response_mean := round(x = rnorm(n = .N, mean = 3.2, sd = 3.1), digits = 3)]
dim(sim.dat1)

exp.results1 <- sim.dat1[, analyze.experiment.twoside(the.dat = .SD), 
                       keyby = "Experiment"]
DT::datatable(data = round(x = exp.results1, digits = 4), 
              rownames = F)

exp.results1[, mean(p < 0.05)]
exp.results1[, summary(effect)]
exp.results1[, summary(upper_ci)]

DT::datatable(data = round(x = exp.results1, digits = 3), 
              rownames = F)

## Perception
ecig1_2.dat <- data.table(Group = c(rep.int(x='Treatment', times=n*2/3), rep.int(x='Control', times=n/3)))

ecig1_2.dat[Group=='Control', response_mean:= round(x=rnorm(n=.N, mean=5, sd=2.6), digits=1)]
ecig1_2.dat[Group=='Treatment', response_mean:= round(x=rnorm(n=.N, mean=4.5, sd=2.5), digits=1)]

datatable(data=ecig1_2.dat)

analyze.experiment.twoside(the.dat=ecig1_2.dat)

RNGversion(vstr=3.6)
Experiment <- 1:B
Group <- c(rep.int(x='Treatment', times = n*2/3), rep.int(x='Control', times=n/3))

sim.dat1_2 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x=sim.dat1_2, cols = c('Experiment', 'Group'), order=c(1,1))
sim.dat1_2[Group == "Control", response_mean := round(x = rnorm(n = .N, mean = 5, sd = 2.6), digits = 3)]
sim.dat1_2[Group == "Treatment", response_mean := round(x = rnorm(n = .N, mean = 4.5, sd = 2.5), digits = 3)]
dim(sim.dat1_2)

exp.results1_2 <- sim.dat1_2[, analyze.experiment.twoside(the.dat = .SD), 
                       keyby = "Experiment"]
DT::datatable(data = round(x = exp.results1_2, digits = 4), 
              rownames = F)

exp.results1_2[, mean(p < 0.05)]
exp.results1_2[, summary(effect)]
exp.results1_2[, summary(upper_ci)]

DT::datatable(data = round(x = exp.results1_2, digits = 3), 
              rownames = F)


#Second Research Question
n<-400

## Purchase Intent

ecig2.dat <- data.table(Group = c(rep.int(x='Treatment', times=n/2), rep.int(x='Control', times=n/2)))

ecig2.dat[Group=='Control', response_mean:= round(x=rnorm(n=.N, mean=3.5, sd=2.7), digits=1)]
ecig2.dat[Group=='Treatment', response_mean:= round(x=rnorm(n=.N, mean=2.8, sd=2.5), digits=1)]

datatable(data=ecig2.dat)

analyze.experiment.less(the.dat=ecig2.dat)

B <- 1000
RNGversion(vstr=3.6)
Experiment <- 1:B
Group <- c(rep.int(x='Treatment', times = n/2), rep.int(x='Control', times=n/2))

sim.dat2 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x=sim.dat2, cols = c('Experiment', 'Group'), order=c(1,1))
sim.dat2[Group == "Control", response_mean := round(x = rnorm(n = .N, mean = 3.5, sd = 2.7), digits = 3)]
sim.dat2[Group == "Treatment", response_mean := round(x = rnorm(n = .N, mean = 2.7, sd = 2.5), digits = 3)]
dim(sim.dat2)

exp.results2 <- sim.dat2[, analyze.experiment.less(the.dat = .SD), 
                       keyby = "Experiment"]
DT::datatable(data = round(x = exp.results2, digits = 4), 
              rownames = F)

exp.results2[, mean(p < 0.05)]
exp.results2[, summary(effect)]
exp.results2[, summary(upper_ci)]

DT::datatable(data = round(x = exp.results2, digits = 3), 
              rownames = F)

## Perception

ecig2_2.dat <- data.table(Group = c(rep.int(x='Treatment', times=n/2), rep.int(x='Control', times=n/2)))

ecig2_2.dat[Group=='Control', response_mean:= round(x=rnorm(n=.N, mean=3.8, sd=2.8), digits=1)]
ecig2_2.dat[Group=='Treatment', response_mean:= round(x=rnorm(n=.N, mean=3.3, sd=2.7), digits=1)]

datatable(data=ecig2_2.dat)

analyze.experiment.less(the.dat=ecig2_2.dat)

B <- 1000
RNGversion(vstr=3.6)
Experiment <- 1:B
Group <- c(rep.int(x='Treatment', times = n/2), rep.int(x='Control', times=n/2))

sim.dat2_2 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x=sim.dat2_2, cols = c('Experiment', 'Group'), order=c(1,1))
sim.dat2_2[Group == "Control", response_mean := round(x = rnorm(n = .N, mean = 3.8, sd = 2.8), digits = 3)]
sim.dat2_2[Group == "Treatment", response_mean := round(x = rnorm(n = .N, mean = 3.3, sd = 2.7), digits = 3)]
dim(sim.dat2_2)

exp.results2_2 <- sim.dat2_2[, analyze.experiment.less(the.dat = .SD), 
                         keyby = "Experiment"]
DT::datatable(data = round(x = exp.results2_2, digits = 4), 
              rownames = F)

exp.results2_2[, mean(p < 0.05)]
exp.results2_2[, summary(effect)]
exp.results2_2[, summary(upper_ci)]

DT::datatable(data = round(x = exp.results2_2, digits = 3), 
              rownames = F)

#Third Research Question

## Purchase Behavior
n <- 600

ecig3_1.dat <- data.table(Group = rep.int(x = "Treatment", times = n), response_mean=round(rnorm(n=600, mean=3.5, sd=2.6), digits=1))

datatable(ecig3_1.dat)

analyze.experiment.oneside(the.dat = ecig3_1.dat)

B <- 1000
n <- 600
RNGversion(vstr = 3.6)
set.seed(seed = 4172)
Experiment <- 1:B

Group = rep.int(x = "Treatment", times = n)
response_mean=round(rnorm(n=600, mean=3.5, sd=2.6), digits=1)

sim.dat3_1 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))

setorderv(x = sim.dat3_1, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat3_1[Group == "Treatment", response_mean:=round(rnorm(n=.N, mean=3.5, sd=2.6), digits=1)]
dim(sim.dat3_1)

exp.results <- sim.dat3_1[, analyze.experiment.oneside(the.dat = .SD), 
                          keyby = "Experiment"]
DT::datatable(data = round(x = exp.results[1:100, ], digits = 3), 
              rownames = F)
exp.results[, mean(p < 0.05)]

## Perception
n <- 600

ecig3_2.dat <- data.table(Group = rep.int(x = "Treatment", times = n), response_mean=round(rnorm(n=600, mean=4.5, sd=2.6), digits=1))

datatable(ecig3_2.dat)

analyze.experiment.oneside(the.dat = ecig3_2.dat)

B <- 1000
n <- 600
RNGversion(vstr = 3.6)
set.seed(seed = 4172)
Experiment <- 1:B

Group = rep.int(x = "Treatment", times = n)
response_mean=round(rnorm(n=600, mean=4.5, sd=2.6), digits=1)

sim.dat3_2 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))

setorderv(x = sim.dat3_2, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat3_2[Group == "Treatment", response_mean:=round(rnorm(n=.N, mean=4.5, sd=2.6), digits=1)]
dim(sim.dat3_2)

exp.results <- sim.dat3_2[, analyze.experiment.percp_3(the.dat = .SD), 
                          keyby = "Experiment"]
DT::datatable(data = round(x = exp.results[1:100, ], digits = 3), 
              rownames = F)
exp.results[, mean(p < 0.05)]

#-----------------------------------------------------------------------------------------------------
#No Effect Code

## Research Question 1
n<-600

ecig1.dat <- data.table(Group = c(rep.int(x='Treatment', times=n*2/3), rep.int(x='Control', times=n/3)))

ecig1.dat[Group=='Control', response_mean:= round(x=rnorm(n=.N, mean=4, sd=2.8), digits=1)]
ecig1.dat[Group=='Treatment', response_mean:= round(x=rnorm(n=.N, mean=4, sd=3.1), digits=1)]

datatable(data=ecig1.dat)

analyze.experiment.twoside(the.dat=ecig1.dat)

B <- 1000
RNGversion(vstr=3.6)
Experiment <- 1:B
Group <- c(rep.int(x='Treatment', times = n*2/3), rep.int(x='Control', times=n/3))

sim.dat1 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x=sim.dat1, cols = c('Experiment', 'Group'), order=c(1,1))
sim.dat1[Group == "Control", response_mean := round(x = rnorm(n = .N, mean = 4, sd = 2.8), digits = 3)]
sim.dat1[Group == "Treatment", response_mean := round(x = rnorm(n = .N, mean = 4, sd = 3.1), digits = 3)]
dim(sim.dat1)

exp.results1 <- sim.dat1[, analyze.experiment.twoside(the.dat = .SD), 
                        keyby = "Experiment"]
DT::datatable(data = round(x = exp.results1, digits = 4), 
              rownames = F)

exp.results1[, mean(p < 0.05)]
exp.results1[, summary(effect)]
exp.results1[, summary(upper_ci)]

DT::datatable(data = round(x = exp.results1, digits = 3), 
              rownames = F)

## Perception
ecig1_2.dat <- data.table(Group = c(rep.int(x='Treatment', times=n*2/3), rep.int(x='Control', times=n/3)))

ecig1_2.dat[Group=='Control', response_mean:= round(x=rnorm(n=.N, mean=5, sd=2.6), digits=1)]
ecig1_2.dat[Group=='Treatment', response_mean:= round(x=rnorm(n=.N, mean=5, sd=2.3), digits=1)]

datatable(data=ecig1_2.dat)

analyze.experiment.twoside(the.dat=ecig1_2.dat)

RNGversion(vstr=3.6)
Experiment <- 1:B
Group <- c(rep.int(x='Treatment', times = n*2/3), rep.int(x='Control', times=n/3))

sim.dat1_2 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x=sim.dat1_2, cols = c('Experiment', 'Group'), order=c(1,1))
sim.dat1_2[Group == "Control", response_mean := round(x = rnorm(n = .N, mean = 5, sd = 2.6), digits = 3)]
sim.dat1_2[Group == "Treatment", response_mean := round(x = rnorm(n = .N, mean = 5, sd = 2.3), digits = 3)]
dim(sim.dat1_2)

exp.results1_2 <- sim.dat1_2[, analyze.experiment.twoside(the.dat = .SD), 
                             keyby = "Experiment"]
DT::datatable(data = round(x = exp.results1_2, digits = 4), 
              rownames = F)

exp.results1_2[, mean(p < 0.05)]
exp.results1_2[, summary(effect)]
exp.results1_2[, summary(upper_ci)]

DT::datatable(data = round(x = exp.results1_2, digits = 3), 
              rownames = F)

#Second Research Question
n<-400

## Purchase Intent

ecig2.dat <- data.table(Group = c(rep.int(x='Treatment', times=n/2), rep.int(x='Control', times=n/2)))

ecig2.dat[Group=='Control', response_mean:= round(x=rnorm(n=.N, mean=3.5, sd=2.7), digits=1)]
ecig2.dat[Group=='Treatment', response_mean:= round(x=rnorm(n=.N, mean=2.8, sd=2.5), digits=1)]

datatable(data=ecig2.dat)

analyze.experiment.less(the.dat=ecig2.dat)

B <- 1000
RNGversion(vstr=3.6)
Experiment <- 1:B
Group <- c(rep.int(x='Treatment', times = n/2), rep.int(x='Control', times=n/2))

sim.dat2 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x=sim.dat2, cols = c('Experiment', 'Group'), order=c(1,1))
sim.dat2[Group == "Control", response_mean := round(x = rnorm(n = .N, mean = 3.5, sd = 2.7), digits = 3)]
sim.dat2[Group == "Treatment", response_mean := round(x = rnorm(n = .N, mean = 3.5, sd = 2.5), digits = 3)]
dim(sim.dat2)

exp.results2 <- sim.dat2[, analyze.experiment.less(the.dat = .SD), 
                         keyby = "Experiment"]
DT::datatable(data = round(x = exp.results2, digits = 4), 
              rownames = F)

exp.results2[, mean(p < 0.05)]
exp.results2[, summary(effect)]
exp.results2[, summary(upper_ci)]

DT::datatable(data = round(x = exp.results2, digits = 3), 
              rownames = F)

## Perception

ecig2_2.dat <- data.table(Group = c(rep.int(x='Treatment', times=n/2), rep.int(x='Control', times=n/2)))

ecig2_2.dat[Group=='Control', response_mean:= round(x=rnorm(n=.N, mean=3.8, sd=2.8), digits=1)]
ecig2_2.dat[Group=='Treatment', response_mean:= round(x=rnorm(n=.N, mean=3.8, sd=2.7), digits=1)]

datatable(data=ecig2_2.dat)

analyze.experiment.less(the.dat=ecig2_2.dat)

B <- 1000
RNGversion(vstr=3.6)
Experiment <- 1:B
Group <- c(rep.int(x='Treatment', times = n/2), rep.int(x='Control', times=n/2))

sim.dat2_2 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))
setorderv(x=sim.dat2_2, cols = c('Experiment', 'Group'), order=c(1,1))
sim.dat2_2[Group == "Control", response_mean := round(x = rnorm(n = .N, mean = 3.8, sd = 2.8), digits = 3)]
sim.dat2_2[Group == "Treatment", response_mean := round(x = rnorm(n = .N, mean = 3.3, sd = 2.7), digits = 3)]
dim(sim.dat2_2)

exp.results2_2 <- sim.dat2_2[, analyze.experiment.less(the.dat = .SD), 
                             keyby = "Experiment"]
DT::datatable(data = round(x = exp.results2_2, digits = 4), 
              rownames = F)

exp.results2_2[, mean(p < 0.05)]
exp.results2_2[, summary(effect)]
exp.results2_2[, summary(upper_ci)]

DT::datatable(data = round(x = exp.results2_2, digits = 3), 
              rownames = F)

#Third Question 3
## Purchasing Behavior
n <- 600

ecig3_1.dat <- data.table(Group = rep.int(x = "Treatment", times = n), response_mean=round(rnorm(n=600, mean=4, sd=2.6), digits=1))

datatable(ecig3_1.dat)

analyze.experiment.oneside(the.dat = ecig3_1.dat)

B <- 1000
n <- 600
RNGversion(vstr = 3.6)
set.seed(seed = 4172)
Experiment <- 1:B

Group = rep.int(x = "Treatment", times = n)
response_mean=round(rnorm(n=600, mean=4, sd=2.6), digits=1)

sim.dat3_1 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))

setorderv(x = sim.dat3_1, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat3_1[Group == "Treatment", response_mean:=round(rnorm(n=.N, mean=4, sd=2.6), digits=1)]
dim(sim.dat3_1)

exp.results <- sim.dat3_1[, analyze.experiment.oneside(the.dat = .SD), 
                          keyby = "Experiment"]
DT::datatable(data = round(x = exp.results[1:100, ], digits = 3), 
              rownames = F)
exp.results[, mean(p < 0.05)]

## Perception
## there is no effect
n <- 600

ecig3_2.dat <- data.table(Group = rep.int(x = "Treatment", times = n), response_mean=round(rnorm(n=600, mean=5, sd=2.6), digits=1))

datatable(ecig3_2.dat)

analyze.experiment.oneside(the.dat = ecig3_2.dat)

B <- 1000
n <- 600
RNGversion(vstr = 3.6)
set.seed(seed = 4172)
Experiment <- 1:B

Group = rep.int(x = "Treatment", times = n)
response_mean=round(rnorm(n=600, mean=5, sd=2.6), digits=1)

sim.dat3_2 <- as.data.table(expand.grid(Experiment = Experiment, Group = Group))

setorderv(x = sim.dat3_2, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat3_2[Group == "Treatment", response_mean:=round(rnorm(n=.N, mean=5, sd=2.6), digits=1)]
dim(sim.dat3_2)

exp.results <- sim.dat3_2[, analyze.experiment.percp_3(the.dat = .SD), 
                          keyby = "Experiment"]
DT::datatable(data = round(x = exp.results[1:100, ], digits = 3), 
              rownames = F)
exp.results[, mean(p < 0.05)]


