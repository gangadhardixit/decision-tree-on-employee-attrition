employee_data=read.csv(file="c://Users/gandixit/Desktop/WA_Fn-UseC_-HR-Employee-Attrition.csv")
View(employee_data)
attach(employee_data)
summary(employee_data)
if(sum(is.na(employee_data))>0)   #check if there are any missing values or NULL fields 
{     
print("there are missing values")
}
else
  print("no missing values!!")

set.seed(111)    #set seed to avoid randomeness 
rownumber1=sample(1:nrow(employee_data),0.7*nrow(employee_data))

employee_traindata=employee_data[rownumber1,]
employee_testdata=employee_data[-rownumber1,]

nrow(employee_data)
nrow(employee_traindata)
nrow(employee_testdata)

cartmodel=rpart(formula=Attrition~.,data = employee_data)  #build model 
summary(cartmodel)


cartpredict=predict(object = cartmodel,newdata = employee_testdata,type = "class")

table(testdata=employee_testdata$Attrition,preictedvalues=cartpredict)
sum(diag(table(employee_testdata$Attrition,cartpredict)))/nrow(employee_testdata)


library(RGtk2)




