library(tidyverse)
library(broom)

#import
data <- read.csv("R/Doctor/import_total.csv",na.strings="NULL")　%>%
  mutate(group_f = as_factor(group))
data

data_pt <- read.csv("R/Doctor/import_MES_pt.csv",na.strings="NULL")　%>%
  mutate(group_f = as_factor(group))
data_pt

data_self <- read.csv("R/Doctor/import_MES_self.csv", na.strings="NULL")　%>%
  mutate(group_f = as_factor(group))
data_self

data_other <-   read.csv("R/Doctor/import_MES_other.csv", na.strings="NULL")　%>%
  mutate(group_f = as_factor(group))
data_other

data_concept <- read.csv("R/Doctor/import_TSSQ_concept.csv", na.strings="NULL")　%>%
  mutate(group_f = as_factor(group))
data_concept



#IRI
##PT
var.test(data$PT_pre, data$PT_post)
pt <- t.test(data$PT_post, data$PT_pre, paired=TRUE, alternative = "greater", var.equal = TRUE)
pt

###効果量
pt_d <- abs(pt$statistic)*sqrt(1/41)
pt_d


##EC
var.test(data$EC_pre, data$EC_post)
ec <- t.test(data$EC_post, data$EC_pre, paired=TRUE, alternative = "two.sided", var.equal = TRUE)
ec

###効果量
ec_d <- abs(ec$statistic)*sqrt(1/41)
ec_d

##FS
var.test(data$FS_pre, data$FS_post)
fs<- t.test(data$FS_post, data$FS_pre, paired=TRUE, alternative = "two.sided", var.equal = TRUE)
fs

###効果量
fs_d <- abs(fs$statistic)*sqrt(1/41)
fs_d

##PD
var.test(data$PD_pre, data$PD_post)
pd <- t.test(data$PD_post , data$PD_pre, paired=TRUE, alternative = "two.sided", var.equal = TRUE)
pd

###効果量
pd_d <- abs(pd$statistic)*sqrt(1/41)
pd_d


#MES
##視点取得
var.test(data_pt$視点取得_pre, data_pt$視点取得_post)
pt2 <- t.test(data_pt$視点取得_post, data_pt$視点取得_pre, paired=TRUE, alternative = "greater", var.equal = TRUE)
pt2

###効果量
pt2_d <- abs(pt2$statistic)*sqrt(1/35)
pt2_d

##自己
var.test(data_self$自己_pre, data_self$自己_post)
self <- t.test(data_self$自己_post, data_self$自己_pre, paired=TRUE, alternative = "two.sided", var.equal = TRUE)
self

###効果量
self_d <- abs(self$statistic)*sqrt(1/40)
self_d

##他者
var.test(data_other$他者_pre, data_other$他者_post)
other <- t.test(data_other$他者_post, data_other$他者_pre, paired=TRUE, alternative = "two.sided", var.equal = FALSE)
other

###効果量
other_d <- abs(other$statistic)*sqrt(1/40)
other_d

##被影響性
var.test(data$被影響性_pre, data$被影響性_post)
inf <- t.test(data$被影響性_post, data$被影響性_pre, paired=TRUE, alternative = "two.sided", var.equal = F)
inf

###効果量
inf_d <- abs(inf$statistic)*sqrt(1/41)
inf_d

##想像性
var.test(data$想像性_pre, data$想像性_post)
img <- t.test(data$想像性_post, data$想像性_pre, paired=TRUE, alternative = "two.sided", var.equal = TRUE)
img

###効果量
img_d <- abs(img$statistic)*sqrt(1/41)
img_d


#TSSQ
##アクティブ
var.test(data$アクティブ_pre, data$アクティブ_post)
active <- t.test(data$アクティブ_post , data$アクティブ_pre, paired=TRUE, alternative = "two.sided", var.equal = TRUE)
active

###効果量
active_d <- abs(active$statistic)*sqrt(1/41)
active_d

##視点取り
var.test(data$視点取り_pre, data$視点取り_post)
pt3 <- t.test(data$視点取り_post, data$視点取り_pre, paired=TRUE, alternative = "two.sided", var.equal = TRUE) ## ian note: shouldn't this be "greater"?
pt3

###効果量
pt3_d <- abs(pt3$statistic)*sqrt(1/41)
pt3_d

##概念化
var.test(data_concept$概念化_pre, data_concept$概念化_post)
concep <- t.test(data_concept$概念化_post, data_concept$概念化_pre, paired=TRUE, alternative = "two.sided", var.equal = TRUE)
concep

###効果量
concep_d <- abs(concep$statistic)*sqrt(1/40)
concep_d

##今この瞬間
var.test(data$今.この瞬間_pre, data$今.この瞬間_post)
pres <- t.test(data$今.この瞬間_post , data$今.この瞬間_pre, paired=TRUE, alternative = "two.sided", var.equal = TRUE)
pres

###効果量
pres_d <- abs(pres$statistic)*sqrt(1/41)
pres_d
