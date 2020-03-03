#loasd libraries
library("e1071")
library("reshape2")
library("plyr")
library("Metrics")
library("caret")
library("rminer")

###Load Data
Path_to_save<-"C:/Users/Dmitry/Desktop/R work/20160630_all_Kyoto/"
setwd(paste0(Path_to_save))
data_norm_Mock<-read.csv("20160630_all_Kyoto_FRET_Good_cleanedSpikes_badMean_flip_NormToMock.csv")

#Select data
data_norm_Mock$X<-NULL
unique(data_norm_Mock$Stimulus)
Stim<-c("Gas6_100ngml","WNT5A_100ngml","EGF_IGF_Akadaic_100ngml_166nM",
        "PDGF_100ngml","EGF_1ngml","EGF_200ngml")
data<-subset(data_norm_Mock,time>-1&time<16200&Sensor%in%c("Abl","S6K","Src")&
                     !Stimulus%in%Stim)

#normalize data to the control and find Max value
data_Mean=ddply(subset(data),.variables=.(Sensor,Stimulus,time),
                summarize,Mean=mean(Norm.to.Mock.0),.progress="text")
data_Mean_max=ddply(subset(data_Mean),.variables=.(Sensor),
                    summarize,Max_mean=max(Mean),.progress="text")
data_Mean<-merge(data_Mean,data_Mean_max,by=c("Sensor"))
data_Mean$Mean_normTomax<-data_Mean$Mean/data_Mean$Max_mean
names(data_Mean)
data_Mean$Max_mean<-NULL
data_Mean$Mean<-NULL
data_Mean<-rename(data_Mean, c("Mean_normTomax"="Mean"))
data_max=ddply(subset(data_Mean),.variables=.(Sensor,Stimulus),
                    summarize,Max=max(Mean),.progress="text")

#Encode concentratio between 0-1
unique(data_max$Stimulus)
data_max$EGF<-0
data_max$IGF<-0
data_max$HGF<-0
data_max$TGF<-0
data_max$TNFa<-0

data_max$EGF[which(data_max$Stimulus=="EGF_100ngml")]<-1
data_max$IGF[which(data_max$Stimulus=="EGF_100ngml")]<-0
data_max$HGF[which(data_max$Stimulus=="EGF_100ngml")]<-0
data_max$TGF[which(data_max$Stimulus=="EGF_100ngml")]<-0
data_max$TNFa[which(data_max$Stimulus=="EGF_100ngml")]<-0
data_max$EGF[which(data_max$Stimulus=="EGF_12_5ngml")]<-0.125
data_max$EGF[which(data_max$Stimulus=="EGF_25ngml")]<-0.25
data_max$EGF[which(data_max$Stimulus=="EGF_50ngml")]<-0.5
data_max$EGF[which(data_max$Stimulus=="EGF_6_25ngml")]<-0.0625
data_max$EGF[which(data_max$Stimulus=="EGF_HGF_100ngml_100ngml")]<-1
data_max$HGF[which(data_max$Stimulus=="EGF_HGF_100ngml_100ngml")]<-1
data_max$EGF[which(data_max$Stimulus=="EGF_HGF_100ngml_6_25ngml")]<-1
data_max$HGF[which(data_max$Stimulus=="EGF_HGF_100ngml_6_25ngml")]<-0.0625
data_max$EGF[which(data_max$Stimulus=="EGF_HGF_6_25ngml_100ngml")]<-0.0625
data_max$HGF[which(data_max$Stimulus=="EGF_HGF_6_25ngml_100ngml")]<-1
data_max$EGF[which(data_max$Stimulus=="EGF_IGF_100ngml_100ngml")]<-1
data_max$IGF[which(data_max$Stimulus=="EGF_IGF_100ngml_100ngml")]<-1
data_max$EGF[which(data_max$Stimulus=="EGF_IGF_100ngml_6_25ngml")]<-1
data_max$IGF[which(data_max$Stimulus=="EGF_IGF_100ngml_6_25ngml")]<-0.0625
data_max$EGF[which(data_max$Stimulus=="EGF_IGF_12_5ngml_12_5ngml")]<-0.125
data_max$IGF[which(data_max$Stimulus=="EGF_IGF_12_5ngml_12_5ngml")]<-0.125
data_max$EGF[which(data_max$Stimulus=="EGF_IGF_25ngml_25ngml")]<-0.25
data_max$IGF[which(data_max$Stimulus=="EGF_IGF_25ngml_25ngml")]<-0.25
data_max$EGF[which(data_max$Stimulus=="EGF_IGF_6_25ngml_100ngml")]<-0.0625
data_max$IGF[which(data_max$Stimulus=="EGF_IGF_6_25ngml_100ngml")]<-1
data_max$EGF[which(data_max$Stimulus=="EGF_IGF_6_25ngml_6_25ngml")]<-0.0625
data_max$IGF[which(data_max$Stimulus=="EGF_IGF_6_25ngml_6_25ngml")]<-0.0625
data_max$EGF[which(data_max$Stimulus=="EGF_TGFa_100ngml_100ngml")]<-1
data_max$TGF[which(data_max$Stimulus=="EGF_TGFa_100ngml_100ngml")]<-1
data_max$EGF[which(data_max$Stimulus=="EGF_TNFa_100ngml_100ngml")]<-1
data_max$TNFa[which(data_max$Stimulus=="EGF_TNFa_100ngml_100ngml")]<-1
data_max$HGF[which(data_max$Stimulus=="HGF_100ngml")]<-1
data_max$HGF[which(data_max$Stimulus=="HGF_25ngml")]<-0.25
data_max$HGF[which(data_max$Stimulus=="HGF_6_25ngml")]<-0.0625
data_max$HGF[which(data_max$Stimulus=="HGF_TNFa_100ngml_100ngml")]<-1
data_max$TNFa[which(data_max$Stimulus=="HGF_TNFa_100ngml_100ngml")]<-1
data_max$IGF[which(data_max$Stimulus=="IGF_100ngml")]<-1
data_max$IGF[which(data_max$Stimulus=="IGF_12_5ngml")]<-0.125
data_max$IGF[which(data_max$Stimulus=="IGF_25ngml")]<-0.25
data_max$IGF[which(data_max$Stimulus=="IGF_6_25ngml")]<-0.0625
data_max$IGF[which(data_max$Stimulus=="IGF_HGF_100ngml_100ngml")]<-1
data_max$HGF[which(data_max$Stimulus=="IGF_HGF_100ngml_100ngml")]<-1
data_max$IGF[which(data_max$Stimulus=="IGF_TGFa_100_100ngml")]<-1
data_max$TGF[which(data_max$Stimulus=="IGF_TGFa_100_100ngml")]<-1
data_max$IGF[which(data_max$Stimulus=="IGF_TNFa_100ngml_100ngml")]<-1
data_max$TNFa[which(data_max$Stimulus=="IGF_TNFa_100ngml_100ngml")]<-1
data_max$TGF[which(data_max$Stimulus=="TGFa_100ngml")]<-1
data_max$TNFa[which(data_max$Stimulus=="TNFa_100ngml")]<-1
data_max<-subset(data_max,TGF==0)
data_max<-subset(data_max,TNFa==0)

#remove TGF and TNFa data
data_max$TGF<-NULL
data_max$TNFa<-NULL

#encode dummy variable for sensors
data_max$Abl<-0
data_max$S6K<-0
data_max$Src<-0
data_max$Abl[which(data_max$Sensor=="Abl")]<-1
data_max$S6K[which(data_max$Sensor=="S6K")]<-1
data_max$Src[which(data_max$Sensor=="Src")]<-1

##combine sensor anme and Stimulus ID
data_max$Stim_sens<-paste(data_max$Stimulus,data_max$Sensor,sep="__")

Stim_sens<-unique(data_max$Stim_sens)

# separatin data into Train, test 
Train_stim_sens_ind<-sample(Stim_sens,48)
train<-subset(data_max,Stim_sens%in%Train_stim_sens_ind)
test<-subset(data_max,!Stim_sens%in%Train_stim_sens_ind)

#remove useless columns 
train$Stimulus<-NULL
test$Stimulus<-NULL
train$Sensor<-NULL
test$Sensor<-NULL
train$Stim_sens<-NULL
test$Stim_sens<-NULL

data_ful_max<-rbind(train,test)
names(data_param)
data_ful_max$EGF<-NULL
data_ful_max$IGF<-NULL
data_ful_max$HGF<-NULL
data_ful_max$Abl<-NULL
data_ful_max$S6K<-NULL
data_ful_max$Src<-NULL
data_ful_max$Stim_sens<-NULL
data_ful_max$test_train<-NULL

data_Mean<-merge(data_Mean,data_ful_max,by=c("Sensor","Stimulus"))

unique(data_Mean$Stimulus)
data_Mean$EGF<-0
data_Mean$IGF<-0
data_Mean$HGF<-0
data_Mean$TGF<-0
data_Mean$TNFa<-0

data_Mean$EGF[which(data_Mean$Stimulus=="EGF_100ngml")]<-1
data_Mean$IGF[which(data_Mean$Stimulus=="EGF_100ngml")]<-0
data_Mean$HGF[which(data_Mean$Stimulus=="EGF_100ngml")]<-0
data_Mean$TGF[which(data_Mean$Stimulus=="EGF_100ngml")]<-0
data_Mean$TNFa[which(data_Mean$Stimulus=="EGF_100ngml")]<-0
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_12_5ngml")]<-0.125
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_25ngml")]<-0.25
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_50ngml")]<-0.5
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_6_25ngml")]<-0.0625
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_HGF_100ngml_100ngml")]<-1
data_Mean$HGF[which(data_Mean$Stimulus=="EGF_HGF_100ngml_100ngml")]<-1
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_HGF_100ngml_6_25ngml")]<-1
data_Mean$HGF[which(data_Mean$Stimulus=="EGF_HGF_100ngml_6_25ngml")]<-0.0625
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_HGF_6_25ngml_100ngml")]<-0.0625
data_Mean$HGF[which(data_Mean$Stimulus=="EGF_HGF_6_25ngml_100ngml")]<-1
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_IGF_100ngml_100ngml")]<-1
data_Mean$IGF[which(data_Mean$Stimulus=="EGF_IGF_100ngml_100ngml")]<-1
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_IGF_100ngml_6_25ngml")]<-1
data_Mean$IGF[which(data_Mean$Stimulus=="EGF_IGF_100ngml_6_25ngml")]<-0.0625
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_IGF_12_5ngml_12_5ngml")]<-0.125
data_Mean$IGF[which(data_Mean$Stimulus=="EGF_IGF_12_5ngml_12_5ngml")]<-0.125
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_IGF_25ngml_25ngml")]<-0.25
data_Mean$IGF[which(data_Mean$Stimulus=="EGF_IGF_25ngml_25ngml")]<-0.25
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_IGF_6_25ngml_100ngml")]<-0.0625
data_Mean$IGF[which(data_Mean$Stimulus=="EGF_IGF_6_25ngml_100ngml")]<-1
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_IGF_6_25ngml_6_25ngml")]<-0.0625
data_Mean$IGF[which(data_Mean$Stimulus=="EGF_IGF_6_25ngml_6_25ngml")]<-0.0625
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_TGFa_100ngml_100ngml")]<-1
data_Mean$TGF[which(data_Mean$Stimulus=="EGF_TGFa_100ngml_100ngml")]<-1
data_Mean$EGF[which(data_Mean$Stimulus=="EGF_TNFa_100ngml_100ngml")]<-1
data_Mean$TNFa[which(data_Mean$Stimulus=="EGF_TNFa_100ngml_100ngml")]<-1
data_Mean$HGF[which(data_Mean$Stimulus=="HGF_100ngml")]<-1
data_Mean$HGF[which(data_Mean$Stimulus=="HGF_25ngml")]<-0.25
data_Mean$HGF[which(data_Mean$Stimulus=="HGF_6_25ngml")]<-0.0625
data_Mean$HGF[which(data_Mean$Stimulus=="HGF_TNFa_100ngml_100ngml")]<-1
data_Mean$TNFa[which(data_Mean$Stimulus=="HGF_TNFa_100ngml_100ngml")]<-1
data_Mean$IGF[which(data_Mean$Stimulus=="IGF_100ngml")]<-1
data_Mean$IGF[which(data_Mean$Stimulus=="IGF_12_5ngml")]<-0.125
data_Mean$IGF[which(data_Mean$Stimulus=="IGF_25ngml")]<-0.25
data_Mean$IGF[which(data_Mean$Stimulus=="IGF_6_25ngml")]<-0.0625
data_Mean$IGF[which(data_Mean$Stimulus=="IGF_HGF_100ngml_100ngml")]<-1
data_Mean$HGF[which(data_Mean$Stimulus=="IGF_HGF_100ngml_100ngml")]<-1
data_Mean$IGF[which(data_Mean$Stimulus=="IGF_TGFa_100_100ngml")]<-1
data_Mean$TGF[which(data_Mean$Stimulus=="IGF_TGFa_100_100ngml")]<-1
data_Mean$IGF[which(data_Mean$Stimulus=="IGF_TNFa_100ngml_100ngml")]<-1
data_Mean$TNFa[which(data_Mean$Stimulus=="IGF_TNFa_100ngml_100ngml")]<-1
data_Mean$TGF[which(data_Mean$Stimulus=="TGFa_100ngml")]<-1
data_Mean$TNFa[which(data_Mean$Stimulus=="TNFa_100ngml")]<-1
data_Mean<-subset(data_Mean,TGF==0)
data_Mean<-subset(data_Mean,TNFa==0)
data_Mean$TGF<-NULL
data_Mean$TNFa<-NULL
unique(data_Mean$Stimulus)
data_Mean$Abl<-0
data_Mean$S6K<-0
data_Mean$Src<-0


data_Mean$Abl[which(data_Mean$Sensor=="Abl")]<-1
data_Mean$S6K[which(data_Mean$Sensor=="S6K")]<-1
data_Mean$Src[which(data_Mean$Sensor=="Src")]<-1


data_Mean$Stim_sens<-paste(data_Mean$Stimulus,data_Mean$Sensor,sep="__")

Stim_sens<-unique(data_Mean$Stim_sens)


Train_stim_sens_ind<-sample(Stim_sens,48)
train<-subset(data_Mean,Stim_sens%in%Train_stim_sens_ind)
test<-subset(data_Mean,!Stim_sens%in%Train_stim_sens_ind)
train$Stimulus<-NULL
test$Stimulus<-NULL
train$Sensor<-NULL
test$Sensor<-NULL
train$Stim_sens<-NULL
test$Stim_sens<-NULL

train$Max<-NULL
test$Max<-NULL
################################Model tuning
tune.par.svm<-function(train_time=train,test_time=test,
                       nu=c(0.3),
                       epsilon=c(0.5) ,
                       degree=c(3),
                       cost=c(1),
                       cross = 4,
                       type="nu-regression",Kernel="radial"){
        Param<-expand.grid(cost,nu,type,Kernel,cross,degree,epsilon)
        colnames(Param)<-c("cost","nu","type","Kernel","cross","degree","epsilon")
        Param$TotMSE_train<-1
        Param$MSE_test<-1
        Param$MAE_test<-1
        Param$RMSE_test<-1
        Param$RMSPE_test<-1
        Param$RRSE_test<-1
        Param$RAE_test<-1
        Param$COR_test<-1
        Param$R2_test<-1
        for(i in seq(length(Param[,1]))){
                model <- svm(Max ~ . , train_time, type = Param$type[i],
                             kernel = Param$Kernel[i],
                             scale = F,nu=Param$nu[i],
                             cost = Param$cost[i],
                             cross = Param$cross[i],
                             epsilon = Param$epsilon[i],
                             degree = Param$degree[i]
                )
                Test_predict<- predict(model, test_time)
                Param$TotMSE_train[i]<-model$tot.MSE
                Met<-mmetric(test$Max,Test_predict,c("MSE","MAE","RMSE","RMSPE",
                                                     "RRSE","RAE","COR","R2"))
                Param$MSE_test[i]<-Met["MSE"]
                Param$MAE_test[i]<-Met["MAE"]
                Param$RMSE_test[i]<-Met["RMSE"]
                Param$RMSPE_test[i]<-Met["RMSPE"]
                Param$RRSE_test[i]<-Met["RRSE"]
                Param$RAE_test[i]<-Met["RAE"]
                Param$COR_test[i]<-Met["COR"]
                Param$R2_test[i]<-Met["R2"]
                print(paste0(i,"/",length(Param[,1])," Procent complited: ",i/length(Param[,1])*100,"%"))
        }
        return((Param))
}

data_param<-tune.par.svm(train_time=train,test_time=test, 
                         nu=c(0.0001,0.001,0.005,0.05,0.01,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1), 
                         cost=c(0.0001,0.001,0.01,0.05,0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 6, 6.5, 8, 10,15,20,25,30,100,65,150, 254, 512, 1000,2000, 4000, 6000), 
                         epsilon=c(0.5),
                         cross = 4,
                         type=c("nu-regression"),
                         Kernel=c("radial"))
Path_to_save<-"C:/Users/Dmitry/Desktop/R work/20160630_all_Kyoto/Timeserias_all conditions/SVM_modeling/e1071_package/"
setwd(paste0(Path_to_save))
write.csv(data_param,"20160924_SVM_param_selection_grid_search_Max_predictio.csv")



