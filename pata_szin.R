library(dplyr)
library(data.table)
library(RcmdrMisc)
library(ggplot2)

# Asvanyi anyagok
# Data cleaning

# Read and transform data
asvanyi <- readXL("E:/amunka/BartosA/pata_asvanyi_rbe.xlsx", rownames=FALSE,
                  header=TRUE, na="", sheet="Munka2", stringsAsFactors=FALSE)

asvanyi$lo <- as.integer(substr(asvanyi$minta,1,1))  # number of horse
asvanyi$color <- substr(asvanyi$minta,3,nchar(asvanyi$minta))  # color of hoot
asvanyi$eh <- substr(asvanyi$eh,1,1)  # keep only the first character, remove accented characters
asvanyi$group <- paste0(asvanyi$eh,asvanyi$color) # names of the groups to be compared
# Reshaping for paired sample t-test
asvanyi <- asvanyi[c("sza","nyzsir","ca","p","cu","zn","lo","group")] # remove unnecessary variables
asv <- dcast(setDT(asvanyi),lo~group,value.var = c("sza","nyzsir","ca","p","cu","zn"))
# save or load the dataframe
setwd("E:/amunka/BartosA")
save(asv,file="asv.RData")
load("asv.RData")

# Analysis

# Different between from and rear hoots in case of the same color (grey)
t.test(asv$sza_eSz,asv$sza_hSz,paired=TRUE) # szárazanyagtartalom p=0.75
t.test(asv$nyzsir_eSz,asv$nyzsir_hSz,paired=TRUE) # nyerszsír p=0.62
t.test(asv$ca_eSz,asv$ca_hSz,paired=TRUE)  # kalcium p=0.58
t.test(asv$p_eSz,asv$p_hSz,paired=TRUE)    # foszfor p=0.77
t.test(asv$cu_eSz,asv$cu_hSz,paired=TRUE)   # réz p=0.53
t.test(asv$zn_eSz,asv$zn_hSz,paired=TRUE)   # p=0.42

# Different between grey and yellow hoots in case of the same front/rear leg
t.test(asv$sza_hS,asv$sza_hSz,paired=TRUE) # szárazanyagtartalom p=0.55
t.test(asv$nyzsir_hS,asv$nyzsir_hSz,paired=TRUE) # nyerszsír p=0.10
t.test(asv$ca_hS,asv$ca_hSz,paired=TRUE)  # kalcium p=0.14
<<<<<<< HEAD
t.test(asv$p_hS,asv$p_hSz,paired=TRUE)    # foszfor p=1
=======
t.test(asv$p_hS,asv$p_hSz,paired=TRUE)    # foszfor p=0.0076
>>>>>>> 97ba5751252cd46ab9502346ae4cf88a7dc19a8a
t.test(asv$cu_hS,asv$cu_hSz,paired=TRUE)   # réz p=0.22
t.test(asv$zn_hS,asv$zn_hSz,paired=TRUE)   # p=0.06


# Kemenyseg es rugalmassag

kr <- readXL("E:/amunka/BartosA/pata_kemenyseg_rbe.xlsx", rownames=FALSE,
                  header=TRUE, na="", sheet="Sheet2", stringsAsFactors=FALSE)
setwd("E:/amunka/BartosA")
save(kr,file="kr.RData")
<<<<<<< HEAD
load("kr.RData")
# patánként átlagszámolás
kr$szineh=paste0(kr$szin,kr$eh)
kragg <- aggregate(kr$rugalmassag,by=list(kr$szineh,kr$pata),FUN="mean")
colnames(kragg) <- c("csop","lo","rug")
# Reshaping for paired sample t-test
asvanyi <- asvanyi[c("sza","nyzsir","ca","p","cu","zn","lo","group")] # remove unnecessary variables
kemrug <- dcast(setDT(kragg),lo~csop,value.var = c("rug"))
save(kemrug,file="kemrug.RData")
load("kemrug.RData")

# t-test for rugalmassag
t.test(kemrug$sze,kemrug$szh,paired=TRUE)   # p=0.41
t.test(kemrug$sh,kemrug$szh,paired=TRUE)   # p=0.98

=======
load("asv.RData")
>>>>>>> 97ba5751252cd46ab9502346ae4cf88a7dc19a8a

# histogram
hist(as.numeric(kr$kemenyseg))
boxplot(as.numeric(kr$kemenyseg))


p=ggplot(kr,aes(x=szin,y=kemenyseg,color=eh))+geom_point()
p
<<<<<<< HEAD

p=ggplot(kr,aes(x=szin,y=rugalmassag,color=eh))+geom_point()
p
=======
>>>>>>> 97ba5751252cd46ab9502346ae4cf88a7dc19a8a
