df<-read_csv("patient_data.csv")


############################################
#function does not work, because the for-loop through the table columns via var in names(df_1) does not work.
getPercentage<-function(data){
  df_1<-data %>% select(all_of(colnames))
  for (var in names(df_1)){
    x<-df_1 %>% count(var)  
    if(length(which(x[,1] == c('PI', 'P'))) !=0) {
      percentage<-(x[which(x[,1] == 'PI'),"n"]/(x[which(x[,1] == 'P'),"n"]+x[which(x[,1] == 'PI'),"n"]))*100
    } else if (length(which(x[,1] == 'P')) !=0){
      percentage<-0
    } else{
      percentage<-100
    } 
    
    print(percentage)
  }
}

getPercentage(df)
#############################################################
# If you assign x for each column the if clause calculates the right percentage
# assign any column to x and execute the if statement and print(percentage)
# I don' know why the for-loop does not work
df_1<-df %>% select(all_of(colnames))
names(df_1)
x<-df_1 %>% count( `No ACT`)
x<-df_1 %>% count( `Sepsis/Tidal`)
x<-df_1 %>% count(`C19/Tidal`)
x<-df_1 %>% count(`PEEP`)
x<-df_1 %>% count(`p-ACT`)
x<-df_1 %>% count(`t-ACT`)
x<-df_1 %>% count(`Proning`)


  if(length(which(x[,1] == c('PI', 'P'))) !=0) {
    percentage<-(x[which(x[,1] == 'PI'),"n"]/(x[which(x[,1] == 'P'),"n"]+x[which(x[,1] == 'PI'),"n"]))*100
  } else if (length(which(x[,1] == 'P')) !=0){
    percentage<-0
  } else{
    percentage<-1
  } 
  
  print(percentage)


getPercentage(df)

