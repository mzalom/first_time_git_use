library(RMySQL)
library(RODBC)
library(data.table)
library(e1071)
library(knitr)
library(miniUI)
library(shiny)
library(taskscheduleR)

#df_one <- read.csv("C:/Users/alom/Desktop/R practice/Questions_difference.csv", header = T)

#con<-dbConnect(RMySQL::MySQL(),dbname="root",user = "root",password = "",host = "localhost")
con<-dbConnect(RMySQL::MySQL(),dbname="dbaz",user = "dbaz_user", password = "Apm7hTdhm2to",host = "193.206.170.140")
dbListTables(con)
userinfo=data.frame()
userinfo <- dbReadTable(con,"users_feedback")
#userinfo <- dbReadTable(con,"users_feedback") feedback_questions


##########  getting new context from dataset  ###########

questions_set <- dbReadTable(con,"new_questions")
new_context_dataset<- data.frame("loc"= integer(0), "tm"= integer(0), "ac"= integer(0), "sl"= integer(0))
new_context_dataset[1:50,1:4]<- questions_set[1:50,19:22]

snr=nrow(userinfo)

df_questions=data.frame()
df_questions <- read.csv("C:/Users/alom/Desktop/R practice/qa.csv", header = T)



indv<-8 # per each user access start. 
while (indv <=snr ){
  
 
########## avoiding null values ############
  
n_check<- as.character(userinfo[indv,252])

if (is.na(n_check)== FALSE){
  
########### context closing checking #########################

user_given_data<- data.frame("lc1"= integer(0), "tm1"= integer(0), "sl1"= integer(0), "ac1"= integer(0), "p1"= integer(0), 
                             "d1"= integer(0), "ret1"= integer(0), "rec1"= integer(0), "lc2"= integer(0), "tm2"= integer(0),
                             "sl2"= integer(0), "ac2"= integer(0), "p2"= integer(0), "d2"= integer(0), "ret2"= integer(0), 
                             "rec2"= integer(0))

user_given_data[1,1:16] <- userinfo[indv,268:283]
  
################# Taking user setup privacy preferences###############

################# Comparing user context with new context##################






#####################end this phase##################




  
  
userinformation<- data.frame("ans"= integer(0), "p"= integer(0), "d"= integer(0), "ret"= integer(0), "rec"= integer(0))
####### get all questions answer from database for one user  ####### 
#userinformation <- userinfo[indv,1:5]
  i<-3
  j<-7
  m<-1
  while (j <= 252){
  userinformation[m,1:5] <- userinfo[indv,i:j]
  
  i<-i+5
  j<-j+5
  m<-m+1
  
  }
####### End all questions answer from database for one user  #######   

userinformation <- data.frame(lapply(userinformation, trimws), stringsAsFactors = FALSE)  #### removing white space of all variable
  
Cl_dataset<- data.frame( "f1" = integer(0), "f2" = integer(0), "f3" = integer(0), "f4" = integer(0), 
                           "f5" = integer(0), "f6" = integer(0), "f7" = integer(0), "f8" = integer(0), "l" = integer(0))

##### for each questions check with user feedback
q <-1 
while (q <= 50){
  
Cl_dataset[q, 1:4]<-df_questions[q, 1:4]

if(userinformation[q,1]== 'yes'){ ######## when user accepted suggested privacy preferences
  Cl_dataset[q, 5:8]<-0
  Cl_dataset[q, 9]<-1 ### label yes means 1
} else{ # when user didn't accepted suggested privacy preferences
 
   Cl_dataset[q, 9]<-0 ### label N means 0
  
  #for purpose
  if(as.character(userinformation[q,2])== as.character(df_questions[q,5])){ Cl_dataset[q, 5]<-0 }else{ Cl_dataset[q, 5]<-0.5}
  
  
  #for data
  if(as.character(userinformation[q,3])== as.character(df_questions[q,6])){ Cl_dataset[q, 6]<-0 }else{ 
    A<-as.character(userinformation[q,3])
    B<- as.character(df_questions[q,6])
    Aa<-as.list(strsplit(A, ",")[[1]])
    Bb<-as.list(strsplit(B, ",")[[1]])
    nu<-union(Aa,Bb)
    du<-intersect(Aa,Bb)
    rs<- 1- (length(du)/length(nu))
    Cl_dataset[q, 6]<-rs}
  
  
  #for retention
  if(as.character(userinformation[q,4])== as.character(df_questions[q,7])){ Cl_dataset[q, 7]<-0 }else{ 
    a<-as.integer(userinformation[q,4])
    b<- df_questions[q,7]
    c<- ((max(a,b) - min(a,b))/max(a,b))
    Cl_dataset[q, 7]<-c}
  
  #for recepient
  if(as.character(userinformation[q,5])== as.character(df_questions[q,8])){ Cl_dataset[q, 8]<-0 }else{ Cl_dataset[q, 8]<-1}
}

q<-q+1
}###### Loop closing for questions 

############ dataset generation #####################

fnm<-userinfo[indv,2]
file_name<-paste(fnm,".csv", sep = "")
write.csv(Cl_dataset, file =file_name, row.names = FALSE)
############ dataset generation #####################

}#else{a<-2}

indv= indv+1 
}