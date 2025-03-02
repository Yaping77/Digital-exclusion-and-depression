
  library(openxlsx)
  library(dplyr)

  data_dig<-read.xlsx("HRS_sample.xlsx",colNames=TRUE)
  
  #### baseline characteristics #### 
  library(table1)

  table1(~RACE+activity+lonely3_score+lonely_2+
           age+gender+marital_g+
           ses_education+ses_income+ses_work+
           bmi_g+smoke+drink+physical_2+ncdever_g|digital_ex,data=data_dig,
         overall = 'Total')
  
  tapply(data_dig$outcome_2, data_dig$digital_ex, sum)
  tapply(data_dig$hhidpn, data_dig$digital_ex, length)

  
  #### association ####
  library(survival)
  ##cox
  cox_dig1<-coxph(Surv(time,outcome_2)~digital_ex,data=data_dig)  ##crude
  cox_dig2<-coxph(Surv(time,outcome_2)~digital_ex+
                    age_g+gender+marital_g+
                    ses_income+ses_education+ses_work,data=data_dig)  ##demographic, SES
  cox_dig3<-coxph(Surv(time,outcome_2)~digital_ex+
                    age_g+gender+marital_g+
                    ses_income+ses_education+ses_work+
                    activity+lonely_2,data=data_dig)    ##MSAS
  cox_dig4<-coxph(Surv(time,outcome_2)~digital_ex+
                    age_g+gender+marital_g+
                    ses_income+ses_education+ses_work+
                    bmi_g+smoke+drink+physical_2+ncdever_g+
                    activity+lonely_2,data=data_dig)  ##all covariates
  summary(cox_dig1); summary(cox_dig2); summary(cox_dig3); summary(cox_dig4);
  
  ##PH test
  cox.zph(cox_dig3)
  
  data_dig<-within(data_dig,{
    age_group<-NA
    age_group[age<60]<-1
    age_group[age>=60&age<70]<-2
    age_group[age>=70&age<80]<-3
    age_group[age>=80]<-4
    age_group<-factor(age_group,levels = c(1:4),labels = c("<60","60-69","70-79","80+"))
  })
  

  #### sensitivity analyses ####
  #age→age_group; loneliness
  cox_dig_sens1<-coxph(Surv(time,outcome_2)~digital_ex+
                         age_group+gender+marital_g+
                         ses_income+ses_education+ses_work+activity+lonely3_score,data=data_dig)
  
  #fixed cohort
  cox_dig_sens2<-coxph(Surv(time,outcome_2)~digital_ex+
                         age_g+gender+marital_g+
                         ses_income+ses_education+ses_work+activity+lonely_2,
                       weights = int_weight,
                       data=data_dig[which(data_dig$int_start==2010|data_dig$int_start==2011),])  
  summary(cox_dig_sens1);summary(cox_dig_sens2)
  tapply(data_dig$outcome_2[which(data_dig$int_start==2010|data_dig$int_start==2011)],
         data_dig$digital_ex[which(data_dig$int_start==2010|data_dig$int_start==2011)],sum)
  tapply(data_dig$hhidpn[which(data_dig$int_start==2010|data_dig$int_start==2011)],
         data_dig$digital_ex[which(data_dig$int_start==2010|data_dig$int_start==2011)],length)
  
  #competitive risk model
  library(tidycmprsk)
  cox_dig_sens3<-tidycmprsk::crr(Surv(time,as.factor(outcome))~digital_ex+
                                   age_g+gender+marital_g+
                                   ses_income+ses_education+ses_work+activity+lonely_2,data_dig,failcode = 1,cencode = 0)
  cox_dig_sens3
  
  
  #### interaction ####
  library(interactionR)
  cox_int3.1<-coxph(Surv(time,outcome_2)~digital_ex*activity+
                      age_g+gender+marital_g+
                      ses_income+ses_education+ses_work+activity+lonely_2,data=data_dig)
  cox_int3.2<-coxph(Surv(time,outcome_2)~digital_ex*lonely_2+
                      age_g+gender+marital_g+
                      ses_income+ses_education+ses_work+activity+lonely_2,data=data_dig)
  
  inter1<-interactionR(cox_int3.1,exposure_names = c("digital_ex","activity"),ci.type = "delta",em=FALSE,ci.level = 0.95) ##em = FALSE为交互作用，TURE为修饰
  inter1
  
  inter2<-interactionR(cox_int3.2,exposure_names = c("digital_ex","lonely_2"),ci.type = "delta",em=FALSE,ci.level = 0.90) ##em = FALSE为交互作用，TURE为修饰
  inter2
  
  #### mediation ####
  library(mediation)
  set.seed(123)

  data_dig<-within(data_dig,{
    digital_num<-NA
    digital_num[digital_ex=="digital_no"]<-1
    digital_num[digital_ex=="digital_use"]<-0
    digital_num<-factor(digital_num)
    
    activity_num<-as.numeric(isolation_act)-1  ##1: inactivity, 0: active
    isolation_act<-factor(isolation_act)
    
    lone_num<-NA
    lone_num[lonely_2=="nonlonely"]<-0
    lone_num[lonely_2=="lonely"]<-1
  })
  
  cox_dig5<-survreg(Surv(time,outcome_2)~digital_num+
                      age_g+gender+marital_g+
                      ses_income+ses_education+ses_work+
                      activity_num+lone_num,data=data_dig,dist = "exp")
  
  dig_m1<-glm(activity_num~digital_num+age_g+gender+marital_g+
                ses_income+ses_education+ses_work+lone_num,family = binomial(),data=data_dig)
  
  dig_med<-mediate(dig_m1,cox_dig5,treat = "digital_num",mediator = "activity_num")
  summary(dig_med) 
  
  dig_m2<-glm(lone_num~digital_num+age_g+gender+marital_g+
                ses_income+ses_education+ses_work+activity_num,family = binomial(),data=data_dig)
  
  dig_med2<-mediate(dig_m2,cox_dig5,treat = "digital_num",mediator = "lone_num")
  summary(dig_med2) 
  
  dig_med$d.avg.ci;dig_med2$d.avg.ci
  dig_med$z.avg.ci;dig_med2$z.avg.ci
  dig_med$n.avg.ci;dig_med2$n.avg.ci
  dig_med$n.avg.p;dig_med2$n.avg.p
  
  
  #### joint analysis ####
  data_dig<-within(data_dig,{
    joint_group<-NA
    joint_group[digital_ex=="digital_use"&activity=="yes"&lonely_2=="nonlonely"]<-1  ##inclusion-active-not
    joint_group[digital_ex=="digital_use"&activity=="yes"&lonely_2=="lonely"]<-2     ##inclusion-active-lone
    joint_group[digital_ex=="digital_use"&activity=="no"&lonely_2=="nonlonely"]<-3   ##inclusion-inactive-not
    joint_group[digital_ex=="digital_use"&activity=="no"&lonely_2=="lonely"]<-4      ##inclusion-inactive-lone
    
    joint_group[digital_ex=="digital_no"&activity=="yes"&lonely_2=="nonlonely"]<-5  ##exclusion-active-not
    joint_group[digital_ex=="digital_no"&activity=="yes"&lonely_2=="lonely"]<-6     ##exclusion-active-lone
    joint_group[digital_ex=="digital_no"&activity=="no"&lonely_2=="nonlonely"]<-7   ##exclusion-inactive-not
    joint_group[digital_ex=="digital_no"&activity=="no"&lonely_2=="lonely"]<-8      ##exclusion-inactive-lone
    
    joint_group<-factor(joint_group,levels = c(1:8),labels = c("use_act_non","use_act-lone","use-inact-non","use-inact-lone",
                                                               "ex_act_non","ex_act-lone","ex-inact-non","ex-inact-lone"))
  })
  cox_dig_joint<-coxph(Surv(time,outcome_2)~joint_group+
                         age_g+gender+marital_g+
                         ses_income+ses_education+ses_work,
                       data=data_dig) 
  summary(cox_dig_joint)
  
  
  
  
  
  