# title: "Mosuo Inequality and Gender Analysis 2022-2023"
# authors: "PMM","BB"
# date: "11/29/2022"

rm(list = ls())

options(warnPartialMatchDollar=TRUE)

##Load packages:
library(MASS) #ordered logistic regression
#Load dplyr for data management
library(dplyr)
#Load regression and stats packages
library(car) #confidence intervals
library(lmtest) #robust standard errors
library(sandwich) #robust standard errors
library(marginaleffects) #SE for ordered regression predictions
#load graphics packages
library(ggplot2)
library(jtools)
library(ggpubr)
#mixed models tools
library(broom.mixed)
#NOTE: 1/31/2023: The dplyr "select" function is conflicted among several of these packages and was behaving unpredictably, so I substituted it with base R "df[rows,c("x","y","etc")]" notation (one could also use "dplyr::select...")
# Data description:
# Working from  csv files "2017IND.csv" and "2017HHMOD.csv". 
#Import files into R below
ind<-read.csv("AnalysisCSVs/2017IND.csv")
hh<-read.csv("AnalysisCSVs/2017HHMOD.csv")

stopifnot(nrow(hh) == 505) # households
stopifnot(nrow(ind) == 2967) # individuals

stopifnot(!any(duplicated(ind$UID)))
stopifnot(!any(duplicated(hh$HomeID)))
stopifnot(length(unique(ind$HomeID)) == 506)

# drop the person missing a HomeID
ind <- ind[which(ind$HomeID > -999),]
stopifnot(nrow(ind) == 2964)

#left join ind to hh
mdata<-merge(ind, hh, by = "HomeID", all.x = T)
names(mdata)

#Delete Water.x column (only three data points) and rename Water.y to "Water".
mdata<-mdata[,!names(mdata)%in% c("Water.x")]
names(mdata)[names(mdata)=="Water.y"]<-"Water"

#looks like junk; remove
mdata<-mdata[,!names(mdata)%in% c("X")]

#Explore variables
summary(mdata$Age)
summary(mdata$Gender)
#Lots of negative values (which are effectively NA codes) in age and gender. 
#We'll remove individuals missing any of those characteristics (i.e., select those with known age and gender)
keep <- which(mdata$Age > 0 & mdata$Gender >= 0)
mdata<-mdata[keep,]
stopifnot(length(mdata$UID)==2926)

#CODE file shows that households with "Area" fields of 1-3 are matrilineal, and 4-5 are patrilineal. Create "kinship" column with "pat" or "mat" based on these values.
mdata$kinship<-""
mdata$kinship[mdata$Area %in% c(1,2,3)]<-"mat"
mdata$kinship[mdata$Area %in% c(4,5)]<-"pat"
mdata[which(is.na(mdata$Area)),]
#UID=1230,1233: Yi family. Remove b/c missing 'Area'
mdata <- mdata[which(!is.na(mdata$Area)),]
stopifnot(length(mdata$UID)==2924)

#create binary variable for kinship
mdata$binkinship <- as.numeric(mdata$kinship == "pat")

#summary of individual income amounts
#note the presence of -999 and -998 values: convert these to NAs for calculation
mdata$IndividualIncome[mdata$IndividualIncome<0]<-NA
mdata$HighestEducation[mdata$HighestEducation %in% c("-998", "-999")]<-NA
mdata$FamilyConditionComparedOtherHouseholds[as.numeric(mdata$FamilyConditionComparedOtherHouseholds)<0]<-NA
#Gender
#create gender label variable
mdata$genlab<-""
mdata$genlab[mdata$Gender==0]<-"F"
mdata$genlab[mdata$Gender==1]<-"M"
summary(mdata$Gender)
#select only adults (Age>16)
mdata<-mdata[mdata$Age>16,]
stopifnot(length(mdata$UID)==2386)

#create binary variable for having an income or not
mdata$iind<-NA
mdata$iind[which(mdata$IndividualIncome>0)]<-1
mdata$iind[which(mdata$IndividualIncome==0)]<-0

#recode variables to numeric values, changing negative values to NA
mdata$binkinship <- as.numeric(mdata$kinship == "pat")

mdata$fluencychinese<-NA
mdata$fluencychinese[mdata$FluencyChinese %in% c("zero", "Zero")]<-0
mdata$fluencychinese[mdata$FluencyChinese=="Below Average"]<-1
mdata$fluencychinese[mdata$FluencyChinese=="Average"]<-3
mdata$fluencychinese[mdata$FluencyChinese=="Fluent Dialect"]<-4
mdata$fluencychinese[mdata$FluencyChinese=="Fluent Mandarin"]<-5
mdata$fluencychinese[mdata$FluencyChinese=="-998"]<-NA
#
mdata$lit<-NA
mdata$lit[mdata$Literacy=="No"]<-0
mdata$lit[mdata$Literacy=="Yes"]<-1
mdata$lit[mdata$Literacy=="A little"]<-0.5
mdata$lit[mdata$Literacy=="-998"]<-NA
mdata$lit[mdata$Literacy=="-999"]<-NA
unique(mdata$lit)
#
unique(mdata$HighestEducation)
mdata$ed<-NA
mdata$ed[mdata$HighestEducation=="None"]<-0
mdata$ed[mdata$HighestEducation=="Kindergarten"]<-1
mdata$ed[mdata$HighestEducation=="Primary"]<-2
mdata$ed[mdata$HighestEducation=="Secondary"]<-3
mdata$ed[mdata$HighestEducation=="Secondary High"]<-4
mdata$ed[mdata$HighestEducation=="Technical-Professional School"]<-5
mdata$ed[mdata$HighestEducation=="Religious School"]<-5
mdata$ed[mdata$HighestEducation=="Tertiary"]<-6
mdata$ed[mdata$HighestEducation=="Postgraduate"]<-7
mdata$ed[mdata$HighestEducation=="-999"]<-NA
mdata$ed[mdata$HighestEducation=="-998"]<-NA
mdata$ed[is.na(mdata$HighestEducation)]<-NA
mdata$ed[mdata$HighestEducation=="Other"]<-NA
unique(mdata$ed)
#
unique(mdata$WealthRanking)
mdata$wealth<-NA
mdata$wealth[mdata$WealthRanking=="Very Poor"]<-1
mdata$wealth[mdata$WealthRanking=="Poor"]<-2
mdata$wealth[mdata$WealthRanking=="Average"]<-3
mdata$wealth[mdata$WealthRanking=="Rich"]<-4
mdata$wealth[mdata$WealthRanking=="Very Rich"]<-5
mdata$wealth[mdata$WealthRanking=="-997"]<-NA
mdata$wealth[mdata$WealthRanking=="-999"]<-NA
unique(mdata$wealth)

#create factors for Chinese fluency and education to reduce these variables to 3 bins
mdata$cf <- case_when(
  mdata$fluencychinese == 0 ~ 0,
  mdata$fluencychinese > 0 & mdata$fluencychinese < 5 ~ 1,
  mdata$fluencychinese >= 5 ~ 2
)
mdata$edcat <- case_when(
  mdata$ed == 0 ~ 0,
  mdata$ed > 0 & mdata$ed < 4 ~ 1,
  mdata$ed >= 4 ~ 2
)


#create cohort variable based on the following (roughly 20 year) time periods:1=1920-1955 (pre Communist predominance);2=1956-1975 (early Communism);3=1976-1995 (Cultural revolution);4=1995-2015 (effectively 2000 for the adult dataset)
#Ages:
mdata$cohort <- case_when(
  mdata$Age >= 62 ~ 1,
  mdata$Age >= 42 & mdata$Age < 62 ~ 2,
  mdata$Age >= 22 & mdata$Age < 42 ~ 3,
  mdata$Age < 22 ~ 4
)

mdata$cohort <- as.factor(mdata$cohort) # reference group is the oldest cohort
#check in 30 lines randomly to make sure this worked as expected
x<-sample(mdata$UID,30)
mdata[mdata$UID %in% x,c("Age","cohort")]
#look at cohort distribution
table(mdata$cohort)

mdata$agesq<-mdata$Age^2

#perform some other data cleanup
mdata$Height<-as.numeric(mdata$Height)
mdata$Height[which(mdata$Height<0)]<-NA
mdata$Weight[which(mdata$Weight<0)]<-NA

#Create models with outcomes of education. Also adding predictor of parents speaking Chinese
#figure out how to find parents of a given individual: thoughts are to create a dataset of parents based on Mother and Father UIDs, then merge their "fluencychinese" data to main dataset as a"parentcf" variable

#clean up Mother and Father UIDs
mdata$MotherUID[mdata$MotherUID<0]<-NA
mdata$FatherUID[mdata$FatherUID<0]<-NA
moms<-mdata[which(!is.na(mdata$MotherUID)),]
dads<-mdata[which(!is.na(mdata$FatherUID)),]

stopifnot(!any(!is.na(mdata$MotherUID) & mdata$MotherUID %in% mdata$FatherUID))
stopifnot(!any(!is.na(mdata$MotherUID) & mdata$MotherUID %in% mdata$UID[which(mdata$Gender == 1)]))
stopifnot(!any(!is.na(mdata$FatherUID) & mdata$FatherUID %in% mdata$UID[which(mdata$Gender == 0)]))

# both fail....
#stopifnot(all(is.na(mdata$MotherUID) | mdata$MotherUID %in% mdata$UID))
# stopifnot(all(is.na(mdata$FatherUID) | mdata$FatherUID %in% mdata$UID))

mdata$FatherUID[which(!is.na(mdata$FatherUID) & !mdata$FatherUID %in% mdata$UID)]
#  1104 1104 1270 1270 1270 1270 1662 1662

mdata$MotherUID[which(!is.na(mdata$MotherUID) & !mdata$MotherUID %in% mdata$UID)]
#  [1] 1102 1102 1102 1103 1103 1144 1167 1367 1367 1367 1663 1663 1782 1782 1840
# [16] 1840 1840 2612 2891

# Eight fathers and 19 mothers don't appear as ego rows in this table

mdata$dadcfcat <- mdata$cf[match(mdata$FatherUID, mdata$UID)]
mdata$momcfcat <- mdata$cf[match(mdata$MotherUID, mdata$UID)]
mdata$parentcfcat <- pmax(mdata$momcfcat, mdata$dadcfcat, na.rm = TRUE)
#check in 30 lines randomly to make sure this worked as expected
x<-sample(mdata$UID,30)
mdata[mdata$UID %in% x,c("dadcfcat","momcfcat","parentcfcat")]
# drop people with cf = 0; they can't be used in the" models b/c their parentcfcat are almost always missing
drop <- which(mdata$cf == 0)
mdata <- mdata[-drop,]

# now convert these three variables to factors
mdata$cf<-as.factor(mdata$cf)
mdata$edcat<-as.factor(mdata$edcat)
mdata$parentcfcat<-as.factor(mdata$parentcfcat)

#Now create the regression models

cat("analyze education levels by cohort\n")

#perform ordinal logistic regression
#regressions predicting education as an outcome and using cohort instead of age as a predictor
cat("create eddata1 subset from mdata as complete cases for edcat and parentcfcat (n = 1278)\n")
eddata1<-mdata[complete.cases(mdata[,c("cohort","Gender","kinship","edcat","parentcfcat")]),]
stopifnot(length(eddata1$UID)==1272)
#educational outcomes model
edmod1<-polr(edcat~Gender+kinship+parentcfcat+cohort+Gender:kinship,data=eddata1,Hess=TRUE,method="logistic")
summary(edmod1)
#begin stepwise selection of edmod1 model
#make null model for forward stepwise regression
intercept_only<-polr(edcat~1, data = eddata1)
fwd <- step(intercept_only,direction="forward",scope=formula(edmod1), trace = 0)#edcat ~ cohort + parentcfcat + Gender + kinship + Gender:kinship
both <- step(intercept_only,direction="both",scope=formula(edmod1), trace = 0)#edcat ~ cohort + parentcfcat + Gender + kinship + Gender:kinship
bwd <- step(edmod1,direction="backward", trace = 0)#edcat ~ Gender + kinship + parentcfcat + cohort + Gender:kinship
#stepwise selection all selected the full model (edmod1)
#calculate p values etc.(methods lifted from UCLA page: https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/)
ctable1<-coef(summary(edmod1))
## calculate and store p values
p <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
## combined table
ctable1 <- cbind(ctable1, "p value" = p)
ci1<-confint(edmod1)
#Odds Ratios and CI
OR1<-exp(cbind(OR = coef(edmod1), ci1))

cat("build logistic/binomial models of income\n")
#first, we need to decide our predictor variables of interest
#kinship+Gender+Age+ agesq+fluencychinese+ed+parentcf+interaction terms: removed lit on 1/6/2023 after discussion with SMM; added parentcf on 1/10/2023. Added agesq on 01/30/2023

#Now create a dataset with complete cases in all variables

cat("create df stepdata with complete cases on cf, edcat and parentcfcat (n = 1035)\n")
stepdata<-mdata[complete.cases(mdata[,c("iind","Age","Gender","cf","kinship","edcat","parentcfcat")]),]
stopifnot(length(stepdata$UID)==1035)

# glms
glmmod<-glm(iind~Age+agesq+Gender+kinship+cf+parentcfcat+edcat+kinship:Gender+Gender:edcat+Gender:cf+Gender:parentcfcat,family="binomial",data=stepdata,na.action = na.omit)
#add age squared component as of 1/28/2023
#try stepwise approach:
#make null model for forward stepwise regression
intercept_only<-glm(iind~1, data = stepdata)
fwd <- step(intercept_only,direction="forward",scope=formula(glmmod), trace = 0)#iind ~ cf + Gender + edcat + cf:Gender + Gender:edcat
both <- step(intercept_only,direction="both",scope=formula(glmmod), trace = 0)#iind ~ cf + Gender + edcat + cf:Gender + Gender:edcat
bwd <- step(glmmod,direction="backward", trace = 0)#iind ~ Age + agesq + Gender + kinship + cf + edcat + Gender:kinship + Gender:edcat + Gender:cf

#evaluate chosen models
mod1<-glm(formula(fwd),family="binomial",data=stepdata,na.action = na.omit)
mod2<-glm(formula(bwd),family="binomial",data=stepdata,na.action = na.omit)
AIC(mod1,mod2)#mod2 has lower and  more interesting terms (e.g, kinship)
summary(mod1)# looks like this model isn't doing well (large standard errors, probably correlated variables)
vif(mod1);vif(mod2)
#mod1 has blown out VIF values for Gender, and Gender x fluencychinese. Should probably ditch it
#mod 2 has marginal VIF values for Gender (and gender-related interaction terms) and Age, Agesq.
#looking to remove either Age or agesq to lower VIF values
mod3<-glm(iind ~ Gender + kinship + agesq + cf  + edcat + Gender:kinship + Gender:edcat + Gender:cf,family="binomial",data=stepdata,na.action = na.omit)
mod4<-glm(iind ~ Gender + kinship + Age + cf  + edcat + Gender:kinship + Gender:edcat + Gender:cf,family="binomial",data=stepdata,na.action = na.omit)
AIC(mod2,mod3,mod4)#mod 2 still best
vif(mod2);vif(mod3);vif(mod4)#mod3 and mod4 identically reduce the age variable VIFs, but AIC for mod2 is lowest, so we'll go with that.

#make some predictions. based on various factors
#input parameters for income likelihood prediction here:
agel<-35
genderl<-c(0,0,1,1)
kinshipl<-c("mat","pat","mat","pat")
cfl<-as.factor(1)
edcatl<-as.factor(1)
logpredict<-data.frame(Age=agel,agesq=agel^2,Gender=genderl,kinship=kinshipl,cf=cfl,edcat=edcatl)
logpredict<-cbind(logpredict,"prob"=predict(mod2,newdata=logpredict,type="response"))
logpredict
logpredict$odds<-logpredict$prob/(1-logpredict$prob)

cat("analyze linear models of (sqrt)income\n")

#linear models using transformed income variable as outcome. Note that this is a zero-heavy distribution
#We'll only look at those individuals with income data over 0, having removed the outliers (incomedata dataset)
#remove individuals with income over 30,000 and create new dataframe "incomedata" for linear models. These outlying individuals can be included in the binary/logistic models
cat("create incomedata subset with income less than 30000 (n=2032)\n")
keep <- which(mdata$IndividualIncome < 30000)
incomedata<-mdata[keep,]
stopifnot(nrow(incomedata) == 1942)

cat("create lmdata subset with income less than 30000 and not 0 (=NA?) (n=801)\n")
keep <- which(mdata$IndividualIncome < 30000 & mdata$IndividualIncome > 0)
lmdata<-mdata[keep,]
stopifnot(length(lmdata$IndividualIncome)==775)

#look at summary stats by kinship and gender
summary(lmdata$IndividualIncome)
summary(lmdata$IndividualIncome[lmdata$kinship=="mat"])
summary(lmdata$IndividualIncome[lmdata$kinship=="pat"])
summary(lmdata$IndividualIncome[lmdata$genlab=="F"& lmdata$kinship=="mat"])
summary(lmdata$IndividualIncome[lmdata$genlab=="M"& lmdata$kinship=="mat"])
summary(lmdata$IndividualIncome[lmdata$genlab=="F"& lmdata$kinship=="pat"])
summary(lmdata$IndividualIncome[lmdata$genlab=="M"& lmdata$kinship=="pat"])


cat("create stepdata as complete-case subset of lmdata (n = 484)\n")
stepdata<-lmdata[complete.cases(lmdata[,c("IndividualIncome","Age","Gender","cf","kinship","edcat","parentcfcat")]),]
stopifnot(length(stepdata$UID)==483)

#The square root of IndividualIncome appears to normalize the distribution (histogram).
stepdata$iisqrt <- sqrt(stepdata$IndividualIncome)
histii<-hist(stepdata$IndividualIncome)
histsqrtii<-hist(sqrt(stepdata$IndividualIncome))
#kitchen Sink Model
lmmod<-glm(iisqrt~Gender+Age+agesq+kinship+cf+edcat+parentcfcat+kinship:Gender+Gender:ed+Gender:cf +Gender:parentcfcat,data=stepdata,na.action = na.omit)

#stepwise regression
#intercept only model:
int_only<-lm(iisqrt~1, data = stepdata)
fwd <- step(int_only,direction="forward",scope=formula(lmmod), trace = 0)#iisqrt ~ edcat + cf
both <- step(int_only,direction="both",scope=formula(lmmod), trace = 0)#iisqrt ~ edcat + cf
bwd <- step(lmmod,direction="backward", trace = 0)#iisqrt ~ Age + agesq + cf + edcat
#compare chosen models
lm1<-lm(formula(fwd),data=stepdata,na.action=na.omit)
lm2<-lm(formula(bwd),data=stepdata,na.action = na.omit)
AIC(lm1,lm2)#lm 2 has lower AIC
vif(lm1,terms="response");vif(lm2,terms="response")
#VIF for lm2 Age and agesq are high (not surprising)
summary(lm1)
summary(lm2)

cat("explore model fits\n")

#Let's look at numbers of people with ind incomes
#subset dataset to individuals with incomes
cat("create incomes subset of incomedata with income > 0\n")
incomes<-incomedata[which(incomedata$IndividualIncome>0),]
stopifnot(nrow(incomes) == 775)

#Individual income broken down my gender and kinship (0=mat,1=pat)
indinctab<-table(factor(incomes$kinship),factor(incomes$Gender))
# 0   1
# mat 175 281
# pat  88 231
indinckintab<-prop.table(indinctab,margin=1)
# 0         1
# mat 0.3837719 0.6162281
# pat 0.2758621 0.7241379
indincgentab<-prop.table(indinctab,margin=2)
# 0         1
# mat 0.6653992 0.5488281
# pat 0.3346008 0.4511719
#
#create robust standard errors for the chosen models
#look at residuals in data to see which types of robust correction are appropriate
cat("recreate stepdata as subset of incomedata with complete cases on cf and edcat (n = 2017)\n")
stepdata<-incomedata[complete.cases(incomedata[,c("IndividualIncome","Age","cf","edcat")]),]
stepdata$iisqrt <- sqrt(stepdata$IndividualIncome)

fitted_lm<-augment(lm2,newdata=stepdata) 
fitlmplot<-ggplot(fitted_lm,aes(x=.fitted,y=.resid))+
  geom_point()+
  geom_smooth(method=lm)

#logistic income model
cat("create stepdata from incomedata as subset on cf, edcat, and parentcfcat (n = 1035)\n")
stepdata<-mdata[complete.cases(mdata[,c("iind","Age","Gender","cf","kinship","edcat","parentcfcat")]),]
stopifnot(nrow(stepdata) == 1035)
fitted_log<-augment(mod2,data=stepdata)
fitlogplot<-ggplot(fitted_log,aes(x=.fitted,y=.resid))+
  geom_point()+
  geom_smooth(method=lm)

summary(lm2)
summary(mod2)
summary(edmod1)


cat("make models robust and visualize\n")

lm2robust<-coeftest(lm2,vcovHC(lm2,type="HC"))
mod2robust<-coeftest(mod2,vcovHC(mod2,type="HC"))
edmod1robust<-coeftest(edmod1)

#prediction plots for education model (edmod1)
#input parameters for education likelihood prediction here:
cohorte<-as.factor(2)
gendere<-c(0,1,0,1)
kinshipe<-c("pat","pat","mat","mat")
parentcfcate<-as.factor(1)
edpredict1<-data.frame(cohort=cohorte,Gender=gendere,kinship=kinshipe,parentcfcat=parentcfcate)
edpredictsum1<-cbind(edpredict1,"prob"=predict(edmod1,newdata=edpredict1,type="probs"))
edpredictsum1
#look at younger cohort
#input parameters for education likelihood prediction here:
cohorte<-as.factor(4)
gendere<-c(0,1,0,1)
kinshipe<-c("pat","pat","mat","mat")
parentcfcate<-as.factor(1)
edpredict2<-data.frame(cohort=cohorte,Gender=gendere,kinship=kinshipe,parentcfcat=parentcfcate)
edpredictsum2<-cbind(edpredict2,"prob"=predict(edmod1,newdata=edpredict2,type="probs"))
edpredictsum2

#calculate average standard errors/CIs of polr object using "marginal effects" package and create data frame for plotting
probs<-marginaleffects::avg_predictions(edmod1,newdata=edpredict1,type="probs")
#transpose marginal effects table
tprobs<-t(probs)
#extract and bind standard errors
edpredictsum<-cbind(edpredictsum1,"SE0"=rep(as.numeric(tprobs[4,1]),4),"SE1"=rep(as.numeric(tprobs[4,2]),4),"SE2"=rep(as.numeric(tprobs[4,3]),4))
#create confidence intervals with prediction standard errors
edpredictsum$lower0<-edpredictsum$prob.0-1.96*as.numeric(edpredictsum$SE0)
edpredictsum$upper0<-edpredictsum$prob.0+1.96*as.numeric(edpredictsum$SE0)
edpredictsum$lower1<-edpredictsum$prob.1-1.96*as.numeric(edpredictsum$SE1)
edpredictsum$upper1<-edpredictsum$prob.1+1.96*as.numeric(edpredictsum$SE1)
edpredictsum$lower2<-edpredictsum$prob.2-1.96*as.numeric(edpredictsum$SE2)
edpredictsum$upper2<-edpredictsum$prob.2+1.96*as.numeric(edpredictsum$SE2)

edpredictsum$genlab<-case_when(
  edpredictsum$Gender == 0 ~ "F",
  edpredictsum$Gender == 1 ~ "M")
edpredictsum$kinlab<-case_when(
  edpredictsum$kinship == "mat" ~ "Matriliny",
  edpredictsum$kinship == "pat" ~ "Patriliny")
#education prediction plot
#graphics parameters for tweaking appearance of plot
pointsize=8
barwidth=0.1
wideline=1
edpredictionplot<-ggplot(edpredictsum,aes(x=kinlab,y=prob.0,group=genlab,fill=genlab))+
  ylim(0,0.75)+
  geom_point(aes(shape="None"),size=pointsize)+
  geom_errorbar(aes(ymin=lower0,ymax=upper0,color=genlab),width=barwidth,linewidth=wideline,show.legend = F)+
  geom_point(aes(y=prob.1,shape="Medium"),size=pointsize,position=position_nudge(x=0.1))+
  geom_errorbar(aes(ymin=lower1,ymax=upper1,color=genlab),position=position_nudge(x=0.1),width=barwidth,linewidth=wideline,show.legend = F)+
  geom_point(aes(y=prob.2,shape="High"),size=pointsize,position=position_nudge(x=0.2))+
  geom_errorbar(aes(ymin=lower2,ymax=upper2,color=genlab),position=position_nudge(x=0.2),width=barwidth,linewidth=wideline,show.legend = F)+
  scale_shape_manual(name="Education Level:",
                       breaks=c("None","Medium","High"),
                       values=c(21,22,23))+
  scale_fill_manual(name="Gender:",
                    values=c("F"="black","M"="grey"),
                    breaks=c("F","M"),
                    guide=guide_legend(override.aes=list(color=c("black","grey"))))+
  scale_color_manual(name="",
                    values=c("black","grey"),
                    breaks=c("F","M"))+
  labs(x="Kinship System",y="Predicted Probability")+
  theme_classic()+
  theme(legend.text = element_text(size=12),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),legend.position = "top",
        legend.title=element_text(size=14,face="bold"),
        axis.title=element_text(size=16,face="bold"))

#Education model
edforest<-plot_summs(edmod1robust,inner_ci_level = 0.95,
                     model.names=c("Ordered Logistic"),
                     coefs=c("Male"="Gender","Patriliny"="kinshippat","Parents Speak Some Mandarin"="parentcfcat1","Parents Fluent in Mandarin"="parentcfcat2","Cohort 42-61 y.o."="cohort2","Cohort 22-41 y.o."="cohort3","Cohort 17-21 y.o."="cohort4","Male x Patriliny"="Gender:kinshippat"),
                     legend.title="",color.class=c("blue"),exp=T)+
  labs(x="Odds Ratios")+
  theme(text=element_text(size=18,face="bold"),legend.text = element_text(size=16),axis.text.y=element_text(size=16))

#look at education plot predictors separately (with and without cohort effects)
edforest1<-plot_summs(edmod1robust,
                      model.names=c("Ordered Logistic"),
                      coefs=c("Male"="Gender","Patriliny (Ref=Matriliny)"="kinshippat","Parents' Fluency in Mandarin=Some (Ref=None)"="parentcfcat1","Parents' Fluency in Mandarin=Fluent"="parentcfcat2","Male x Patriliny"="Gender:kinshippat"),
                      legend.title="",color.class=c("blue"),exp=T)+
  labs(x="")
edforest2<-plot_summs(edmod1robust,
                      model.names=c("Ordered Logistic"),
                      coefs=c("Cohort 42-61 y.o. (Ref=62+ y.o.)"="cohort2","Cohort 22-41 y.o."="cohort3","Cohort 17-21 y.o."="cohort4"),
                      legend.title="",color.class=c("blue"),exp=T)+
  labs(x="Odds Ratio and 95% CI of Increasing Education Level")
edforestplots<-ggarrange(edforest1,edforest2,labels=c("C1","C2"),ncol=1,nrow=2)


#quick export package for logistic and linear models
summlm<-summ(lm2,confint=T,robust="HC1",transform.response = F,exp=T,digits=3)
summod2<-summ(mod2,confint=T,robust="HC1",transform.response = F,exp=T,digits=3)

#Create forest plots for income models
#Forest plot of linear income model
lmforest<-plot_summs(lm2robust,inner_ci_level = 0.95,
                     coefs=c("Age"="Age","agesq"="Age-squared","Some Mandarin"="cf1","Fluent in Mandarin"="cf2","Some Education"="edcat1","High Education"="edcat2"),
                     color=c("magenta"))+
  labs(x="Beta Coefficients")+
  theme(text=element_text(size=18,face="bold"),legend.text = element_text(size=16),axis.text.y=element_text(size=16))

#Forest plot of logistic income model
logforest<-plot_summs(mod2robust,inner_ci_level = 0.95,
                      coefs=c("Male"="Gender","Patriliny"="kinshippat","Age"="Age","Age Squared"="agesq","Some Mandarin"="cf1","Fluent in Mandarin"="cf2","Some Education"="edcat1","High Education"="edcat2","Male x Patriliny"="Gender:kinshippat","Male x Some Education"="Gender:edcat1","Male x High Education"="Gender:edcat2","Male x Some Mandarin"="Gender:cf1","Male x Fluent Mandarin"="Gender:cf2"),
                      color.class=c("red"),exp=T,point.size = 10)+
  labs(x="Odds Ratios",y="Variables")+
  theme(text=element_text(size=18,face="bold"),legend.text = element_text(size=16),axis.text.y=element_text(size=16))
#create separate forest plot for log model
logforest2<-plot_summs(mod2robust,
                       coefs=c("Patriliny(Ref=Matriliny)"="kinshippat","Age"="Age","Age Squared"="agesq","Some Mandarin"="cf1","Medium Education"="edcat1","High Education"="edcat2","Male x Patriliny"="Gender:kinshippat","Male x Medium Education"="Gender:edcat1","Male x High Education"="Gender:edcat2"),
                       color.class=c("red"),exp=T)+
  labs(x="")
logforest3<-plot_summs(mod2robust,
                       coefs=c("Male"="Gender","Male x Some Mandarin"="Gender:cf1","Male x Fluent Mandarin"="Gender:cf2","Fluent Mandarin"="cf2"),
                       color.class=c("red"),exp=T)+
  labs(x="Odds Ratio and 95% CI of Having Income",y="Variables")

#make panel plot for presentation
forestplots<-ggarrange(logforest,lmforest,edforest,labels=c("A","B","C"),ncol=1,nrow=3)

#linear model predictions
#first looking at age and education levels
agelm<-rep(c(20,30,40,50,60,70,80,90),3)
edcatlm<-c(rep(as.factor(0),8),rep(as.factor(1),8),rep(as.factor(2),8))
cflm<-as.factor(1)
lmpredict1<-data.frame(Age=agelm,agesq=agelm^2,edcat=edcatlm,cf=cflm)
lmpredict1<-cbind(lmpredict1,lmp=predict(lm2,newdata=lmpredict1))
incpredplot1<-ggplot(data=lmpredict1,aes(x=Age,y=lmp,color=edcat,linetype=edcat),group=edcat)+
  geom_smooth(linewidth=wideline,method="loess",se=F)+
  scale_y_continuous(limits=c(-40,55))+
  scale_color_grey(start=0.7,end=0.3)+
  scale_linetype_manual(values=c("dotted","dotdash","twodash"))+
  labs(x="Age",y="Square Root of Monthly Income (CNY)",col="Education Level")+
  guides(linetype=guide_legend("Education Level"))+
  theme_classic()+
  theme(text=element_text(size=16,face="bold"),
        legend.text = element_text(size=14),
        axis.text=element_text(size=12),
        legend.position="top",
        legend.key.size = unit(3,"line"))
#now varying age and Mandarin proficiency levels
agelm1<-rep(c(20,30,40,50,60,70,80,90),2)
edcatlm1<-as.factor(1)
cflm1<-c(rep(as.factor(1),8),rep(as.factor(2),8))
lmpredict2<-data.frame(Age=agelm1,agesq=agelm1^2,edcat=edcatlm1,cf=cflm1)
lmpredict2<-cbind(lmpredict2,lmp=predict(lm2,newdata=lmpredict2))
incpredplot2<-ggplot(data=lmpredict2,aes(x=Age,y=lmp,color=cf,linetype=cf),group=cf)+
  geom_smooth(linewidth=wideline,method="loess",se=F)+
  scale_y_continuous(limits=c(-40,55))+
  scale_color_grey(start=0.3,end=0.8)+
  scale_linetype_manual(values=c("dotted","dotdash"))+
  labs(x="Age",y="",col="Mandarin Fluency Level")+
  guides(linetype=guide_legend("Mandarin Fluency Level"))+
  theme_classic()+
  theme(text=element_text(size=16,face="bold"),
        legend.text = element_text(size=14),
        axis.text=element_text(size=12),
        legend.position="top",
        legend.key.size = unit(2,"line"))
lmpredplots<-ggarrange(incpredplot1,incpredplot2,nrow=1,ncol=2)

#set graphical parameters here:
plotcex<-1.5
tiffh=800
tiffw=800
tiffpointsize=14

#recreate figure 1 (Trivers Willard figure)
x <- seq(1,10,.5)
y <- x*.7+2
par(font=2,cex=plotcex,font.axis=2,font.lab=2)
plot(x,y,type="l",lwd=5,col="black",xlab="Wealth", ylab="Reproductive Success",axes=F, ylim=c(2,8),xlim=c(2,10))
axis(1,at=c(2,10), labels=c("Low","High"),lwd=2)
axis(2,at=c(2,8), labels=c("Low","High"),lwd=2)
yfem <- x*.2+4
lines(x,yfem,lwd=5,lty=2)
legend(2,7, lty=c(1,2), lwd=c(3,3), c("Males","Females"),bty="n")

#Output file for Trivers Willard Figure
tiff(file="output/twplot.tiff",width=tiffw,height=tiffh,pointsize=tiffpointsize)
x <- seq(1,10,.5)
y <- x*.7+2
par(font=2,cex=plotcex,font.axis=2,font.lab=2)
plot(x,y,type="l",lwd=5,col="black",xlab="Wealth", ylab="Reproductive Success",axes=F, ylim=c(2,8),xlim=c(2,10))
axis(1,at=c(2,10), labels=c("Low","High"),lwd=2)
axis(2,at=c(2,8), labels=c("Low","High"),lwd=2)
yfem <- x*.2+4
lines(x,yfem,lwd=5,lty=2)
legend(2,7, lty=c(1,2), lwd=c(3,3), c("Males","Females"),bty="n")
dev.off()

#Other candidate paper figures
tiff(file="output/forestplots.tiff",width=tiffw,height=tiffh,pointsize=tiffpointsize)
forestplots
dev.off()
tiff(file="output/edforestplot.tiff",width=tiffw,height=tiffh,pointsize=tiffpointsize)
edforest
dev.off()
tiff(file="output/lmforestplot.tiff",width=tiffw,height=tiffh,pointsize=tiffpointsize)
lmforest
dev.off()
tiff(file="output/logforestplot.tiff",width=tiffw,height=tiffh,pointsize=tiffpointsize)
logforest
dev.off()
tiff(file="output/lmpredictplots.tiff",width=tiffw,height=tiffh,pointsize=tiffpointsize)
lmpredplots
dev.off()
tiff(file="output/edpredictplot.tiff",width=tiffw,height=tiffh,pointsize=tiffpointsize)
edpredictionplot
dev.off()

plotcolors<-palette.colors(n=8,palette="R4")
#compilation of all plots
pdf("output/Mosuo2022-2023plots.pdf",paper="USr")
hist(mdata$FamilyConditionComparedOtherHouseholds)
ggplot(mdata,aes(x=Gender,fill=kinship))+geom_histogram(binwidth=0.5)+
  stat_bin(binwidth=1, geom='text', color='black', size=6,aes(label=..count.., group=kinship), position=position_stack(vjust=0.25))
ggplot(mdata,aes(x=genlab,y=Age,fill=kinship))+geom_boxplot()+
  theme_pubclean()
ggplot(mdata,aes(x=ed,fill=kinship))+geom_histogram(binwidth=0.5)+
  stat_bin(binwidth=1, geom='text', color='black', size=6,aes(label=..count.., group=kinship), position=position_stack(vjust=0.5))+
  theme_pubclean()
ggplot(mdata,aes(x=edcat,fill=genlab))+stat_count()+
  theme_pubclean()
ggplot(mdata,aes(x=edcat,fill=kinship))+stat_count()+
  theme_pubclean()
ggplot(mdata,aes(x=edcat,fill=kinship))+
  geom_bar(width=0.25,position="dodge")+
  geom_bar(width=0.25,aes(x=as.numeric(edcat)+0.25,fill=genlab),position="dodge")+
  fill_palette(plotcolors)+
  theme_pubclean()
ggplot(mdata,aes(y=IndividualIncome))+geom_boxplot()+
  theme_pubclean()
ggplot(incomedata,aes(y=IndividualIncome))+geom_boxplot()+
  theme_pubclean()
ggplot(incomedata,aes(x=kinship,y=IndividualIncome))+geom_boxplot()+
  theme_pubclean()
ggplot(incomedata,aes(x=genlab,y=IndividualIncome))+geom_boxplot()+
  theme_pubclean()
ggplot(incomedata,aes(x=kinship,y=IndividualIncome,fill=genlab))+
  geom_boxplot()+
  theme_pubclean()
plot(histii)
plot(histsqrtii)
boxplot(mdata$Height~mdata$kinship+mdata$genlab,notch=T,xlab="Kinship and Gender",ylab="Height(cm)")
ggplot(mdata,aes(x=genlab,y=Height,fill=kinship))+geom_violin()+
  theme_pubclean()
boxplot(mdata$Weight~mdata$kinship+mdata$genlab,notch=T,xlab="Kinship and Gender",ylab="Weight(kg)")
fitlmplot
fitlogplot
edforest
edforest
edforest1
edforestplots
lmforest
logforest
logforest2
logforest3
incpredplot1
incpredplot2
forestplots
lmpredplots
edpredictionplot
dev.off()
#Summary Stats
#Descriptive tables of Mosuo adults
table(mdata$genlab)

table(mdata$genlab,factor(mdata$kinship))

#Age statistics
summary(mdata$Age)
summary(mdata$Age[mdata$kinship=="mat"])
summary(mdata$Age[mdata$kinship=="pat"])
summary(mdata$Age[mdata$genlab=="F"& mdata$kinship=="mat"])
summary(mdata$Age[mdata$genlab=="M"& mdata$kinship=="mat"])
summary(mdata$Age[mdata$genlab=="F"& mdata$kinship=="pat"])
summary(mdata$Age[mdata$genlab=="M"& mdata$kinship=="pat"])
#Education statistics
summary(mdata$edcat)
summary(mdata$edcat[mdata$kinship=="mat"])
summary(mdata$edcat[mdata$kinship=="pat"])
summary(mdata$edcat[mdata$genlab=="F"& mdata$kinship=="mat"])
summary(mdata$edcat[mdata$genlab=="M"& mdata$kinship=="mat"])
summary(mdata$edcat[mdata$genlab=="F"& mdata$kinship=="pat"])
summary(mdata$edcat[mdata$genlab=="M"& mdata$kinship=="pat"])
#Mandarin fluency statistics
summary(mdata$cf)
summary(mdata$cf[mdata$kinship=="mat"])
summary(mdata$cf[mdata$kinship=="pat"])
summary(mdata$cf[mdata$genlab=="F"& mdata$kinship=="mat"])
summary(mdata$cf[mdata$genlab=="M"& mdata$kinship=="mat"])
summary(mdata$cf[mdata$genlab=="F"& mdata$kinship=="pat"])
summary(mdata$cf[mdata$genlab=="M"& mdata$kinship=="pat"])
#Parents' Mandarin statistics
summary(mdata$parentcfcat)
summary(mdata$parentcfcat[mdata$kinship=="mat"])
summary(mdata$parentcfcat[mdata$kinship=="pat"])
summary(mdata$parentcfcat[mdata$genlab=="F"& mdata$kinship=="mat"])
summary(mdata$parentcfcat[mdata$genlab=="M"& mdata$kinship=="mat"])
summary(mdata$parentcfcat[mdata$genlab=="F"& mdata$kinship=="pat"])
summary(mdata$parentcfcat[mdata$genlab=="M"& mdata$kinship=="pat"])
#Cohort statistics
summary(mdata$cohort)
summary(mdata$cohort[mdata$kinship=="mat"])
summary(mdata$cohort[mdata$kinship=="pat"])
summary(mdata$cohort[mdata$genlab=="F"& mdata$kinship=="mat"])
summary(mdata$cohort[mdata$genlab=="M"& mdata$kinship=="mat"])
summary(mdata$cohort[mdata$genlab=="F"& mdata$kinship=="pat"])
summary(mdata$cohort[mdata$genlab=="M"& mdata$kinship=="pat"])
#Income amounts for those that have income overall and by kinship and gender
summary(lmdata$IndividualIncome)
summary(lmdata$IndividualIncome[lmdata$kinship=="mat"])
summary(lmdata$IndividualIncome[lmdata$kinship=="pat"])
summary(lmdata$IndividualIncome[lmdata$genlab=="F"& lmdata$kinship=="mat"])
summary(lmdata$IndividualIncome[lmdata$genlab=="M"& lmdata$kinship=="mat"])
summary(lmdata$IndividualIncome[lmdata$genlab=="F"& lmdata$kinship=="pat"])
summary(lmdata$IndividualIncome[lmdata$genlab=="M"& lmdata$kinship=="pat"])
#Tables of presence of incomes by gender and kinship
table(mdata$iind,useNA="no")

table(mdata$iind,factor(mdata$genlab),factor(mdata$kinship))

#determine proportions of people with and without individual incomes
prop.table(table(mdata$iind,factor(mdata$genlab)),1)

table(mdata$iind[mdata$kinship=="mat"],useNA="no")