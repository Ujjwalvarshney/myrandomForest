library(tree)
library(randomForest)
library(dplyr)
ci=read.csv("census_income.csv",stringsAsFactors = F)
glimpse(ci)
ci=ci %>% select(-education)
ci$Y=as.numeric(ci$Y== " >50K")
ci$Y=as.factor(ci$Y)
glimpse(ci)
lapply(ci, function(x) length(unique(x)))
names(ci)[sapply(ci, function(x) is.character(x))]
table(ci$marital.status)
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}
cat_var=names(ci)[sapply(ci, function(x) is.character(x))]
for(cat in cat_var)
{
  ci=CreateDummies(ci,cat,500)
}

glimpse(ci)
set.seed(3)
s=sample(1:nrow(ci),0.8*nrow(ci))
ci_train=ci[s,]
ci_test=ci[-s,]

tree.model=tree(Y~.,data =ci_train)
plot(tree.model)
text(tree.model)
test.score=predict(tree.model,newdata = ci_test,type='vector')[,1]
library(pROC)
auc(roc(ci_test$Y,test.score))
random.model=randomForest(Y~.,data =ci_train)
rf.score=predict(random.model,newdata = ci_test,type='prob')[,1]
auc(roc(ci_test$Y,rf.score))
## parameter tunning
library(cvTools)
mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}
param=data.frame(mtry=5,ntree=10)
k=cvTuning(randomForest,Y~., 
           data =ci_train,
           tuning =param,
           folds = cvFolds(nrow(ci_train), K=5, type ="random"),
           cost =mycost_auc, seed =2,
           predictArgs = list(type="prob")
)
score.this=k$cv[,2]
score.this


param=list(mtry=c(5,10,15,20,25,35),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10))
subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

## 

num_trials=10
my_params=subset_paras(param,num_trials)
my_params
myauc=0
for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,Y~., 
             data =ci_train,
             tuning =params,
             folds = cvFolds(nrow(ci_train), K=5, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  score.this
  
  if(score.this>myauc){
    #print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}


best_params

## Values obtained from an earlier run 

# myauc=0.8945485
 best_params=data.frame(mtry=5,
                       ntree=700,
                       maxnodes=15,
                       nodesize=1)

## Model on the entire training data
 rf.tuned.model=randomForest(Y~.,data=ci_train,mtry=5,
                             ntree=700,
                             maxnodes=15,
                             nodesize=1,do.trace=T)
 test.score=predict(rf.tuned.model,newdata = ci_test,type="prob")[,1]
auc(roc(ci_test$Y,test.score))
####