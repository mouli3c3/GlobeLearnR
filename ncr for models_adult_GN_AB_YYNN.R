###  MODELING PROCESS COMBINING INPUT DATA FROM SAS EMINER###
#USING CARET/MLR
library(reshape2)
library(ClustOfVar)
library(feather)
library(DBI)
library(odbc)
library(dbplyr)
library(tidyr)
library(dplyr)
library(tidyverse) 
library(gridExtra)
library(ggplot2)
## Modeling Libraries
library(Information)
library(feather)
#library(h2o)
library(cluster)
library(fpc)
#install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)
library('RANN')
library(plotly);library(haven)
getwd()
### CHECK THESE PARAMETERS BEFORE EVERY RUN###
### LINE 30||# LINE 56||# LINE 59||# LINE 63||# LINE 65||# LINE 70||# LINE 79||    ###number changes until this code is finalized!!

###TRAINING DATA -SUPPOSED TO BE IMPUTED/REPLACED/GROUPED DATA SET FROM SAS
# NO NEED TO DO IV CALCULATION AND VARCLUSTERING OR MAY BE NOT!!
train<-read_sas("gn_ab_yynn_train.sas7bdat")
str(train)
#This may be optional depending on the data importing style
#vars_to_be_numeric<-c("MTEmpProvHealthInsPHs","MTWrkSmallCompOffHealthIns","MTMedicaidPQH","MTMedicareAdvantagePP","MTACAHealthInsPurch","MTRetiredbutStillWorking","MTBurialInsurancePurchaser","MTUninsuredforHealth","MTInsuranceSwitcher","TargetIncomeIndex","InvestmentResources","ShortTermLiability","TargetNetWorth","LiquidResources","WealthResources","MTWholeLife","MTTermLife")
#train<-train%>%mutate_at(vars_to_be_numeric,as.character)%>%mutate_at(vars_to_be_numeric,as.numeric)
mycol_names=colnames(train)              
vars_to_be_numeric<-mycol_names[c(5:7,9:15)] 
train<-train%>%mutate_at(vars_to_be_numeric,as.numeric)%>%
  mutate_at(setdiff(mycol_names,vars_to_be_numeric),as.factor)%>%as.data.frame()
str(train)
trainSet<-train
###validation data sets
valid_reduced_proc<-read_sas("gn_ab_yynn_score.sas7bdat") ## BRING IN THE SAS VALIDATION/SCORING DATASET
#setting the data types on validation data
valid_reduced_proc<-valid_reduced_proc%>%mutate_at(vars_to_be_numeric,as.numeric)%>%
mutate_at(setdiff(mycol_names,vars_to_be_numeric),as.factor)%>%as.data.frame()

### creatiing new profile variables for analysis on validation data
ScoringSet<-valid_reduced_proc%>%
  mutate(AppN=ifelse(App_flag=="Y",1,0),IssN=ifelse(Response=="Y",1,0),PaidsN=ifelse(Persistency=="Y",1,0))%>%
  select(-App_flag,-Response,-Persistency)

### SPECIFY YOUR TOP 10 VARIABLES IN TWO MUTUALLY EXCLUSIVE OR SEMI EXCLUSIVE GROUPS
TopX1<-c("CYEPPETHNBLACK",
         "CYEPPFEMHDFAMHH",
         "IMP_EZ769",
         "IMP_EZ454",
         "IMP_Paids_A_Sum_PerM_Zip",
         "Months_since_Inq",
         "GRP_Niches"
)
prof_var=c("Pv_Margin",
           "Response",
           "Persistency",
           "Pv_Expense",
           "PV_Premium",
           "DirectCostPerUnit",
           "App_flag"
)
## RUN BELOW FOR TopX1 SERIES AND THEN TopX2 SERIES MANUALLY AS OF NOW!
outcomeName<-"Persistency"

Top10_predictors<-TopX1 ## CHANGE HERE IF WANT TO RUN SECOND TOP 10 VARS
#CHANGE below accordingly if m=8 then V1:V8||if m=6 then V1:V6
m=5 # No of combinations needed. Sould always be <=  Top10_predictors            
predictors_df<-as.data.frame(combn(Top10_predictors,m,simplify=TRUE))%>%gather(.,V1:V4) #CHANGE HERE accordingly
names(predictors_df)<-c("Model_Num","X")
## BELOW DF_T CREATES A DATA FRAME OF ALL COMBINATIONS OF MODELS WHICH WILL BE LATER USED TO PREDICT ON VALIDATION

running_model<-"glm" # CHANGE HERE TO NNET/RF/GBM AS PER REQUIREMENT

df_t<-predictors_df%>%
  group_by(Model_Num)%>%
  do(model=train(trainSet[,.$X],trainSet[,outcomeName],method=running_model
                 ,maxit = 200,trControl = trainControl(method="none",number=0)))





####PREDICTIONS
df_probs<-data.frame(matrix(ncol = 5,nrow=nrow(ScoringSet)))
df_vars<-data.frame(matrix(ncol = 5,nrow=15)) # nrow=15 is to be changed if more than 15 vars used to start with!
for (i in 1:nrow(df_t)) {
  df_probs[,i]<-predict.train(df_t[i,"model"]$model[[1]], newdata=ScoringSet,type="prob")[,2]
  df_vars[1:length(df_t[i,"model"]$model[[1]]$finalModel$xNames),i]<-df_t[i,"model"]$model[[1]]$finalModel$xNames
}
colnames(df_vars)<-paste0("Model_Num ",1:ncol(df_vars))
write.csv(df_vars,paste0(running_model,"_VARS_GN_AB_YYNN",length(Top10_predictors),"comb",m,".csv"))
###DECILING
#dec_df<-data.frame(matrix(ncol = 5,nrow=100))
#ncol(df_probs)
for (i in 1:ncol(df_probs)) {
  final_df<-data.frame(predict=df_probs[,i]
                       ,AppN=ScoringSet[,"AppN"]
                       ,IssN=ScoringSet[,"IssN"]
                       ,PaidsN=ScoringSet[,"PaidsN"]
                       ,PV_Premium=ScoringSet[,"PV_Premium"]
                       ,PV_margin=ScoringSet[,"Pv_Margin"]
                       ,DirCostPerUnit=ScoringSet[,"DirectCostPerUnit"]
  )
  final_df%>%
    mutate(ranks=rank(-predict,ties.method = "min"))%>%
    arrange(ranks)%>%
    mutate(quantile=ntile(ranks,10))%>%
    group_by(quantile)%>%
    dplyr::summarize(sumApp=sum(AppN),sumIss=sum(IssN),sumPaids=sum(PaidsN),sumcount=n()
                     ,Tot_cost=sum(DirCostPerUnit),Tot_margin=sum(PV_margin)
                     ,mean_margin=Tot_margin/sumPaids,Tot_premium=sum(PV_Premium))%>%
    mutate(cumApps=cumsum(sumApp)
           ,cumIss=cumsum(sumIss)
           ,cumPaids=cumsum(sumPaids)
           ,AppM=sumApp/sumcount*1000
           ,IssM=sumIss/sumcount*1000
           ,PaidM=sumPaids/sumcount*1000
           ,FEP=sumPaids/sumIss*100
           ,Tot_profit=Tot_margin-Tot_cost
           ,cumTProfit=cumsum(Tot_profit)
           ,PROFIT_PCNT_PREMIUM=(Tot_profit*100)/Tot_premium
           ,Model_Num=rep(paste0("Model_Num ",i),10)
    )%>%write.table(.,paste0(running_model,"_dec_GN_AB_YYNN",length(Top10_predictors),"comb",m,".csv"),append = TRUE,row.names = FALSE)
}

print("DONE!Look for 2 csv's:: 1. vars 2. deciles")
