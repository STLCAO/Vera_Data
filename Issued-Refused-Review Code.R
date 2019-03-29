
#Use linquiry report in PBK and use the referral data in PBK to get the desired dataset


#____________________________FINDING ISSUED AND REFUSED CASES______________________________________________ 
#loading library's
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
#-------setting working directory and obtaining csv file------
wd<-"C:\\Users\\Gunathilakel\\Desktop\\Vera Institute Monthly Report\\MonthlyDataReport\\Datafiles"
setwd(wd)


#-------Reading the CSV files--(These Need to be loaded before we do analysis)
dec.datain<-read.csv("dec-2018-linquiry-data.csv",stringsAsFactors = FALSE)%>%mutate(Year='18',Month='Dec')
jan.datain<-read.csv("jan-2019-linquiry-data.csv",stringsAsFactors = FALSE)%>%mutate(Year='19',Month='Jan')  
feb.datain<-read.csv("feb-2019-linquiry-data.csv",stringsAsFactors = FALSE)%>%mutate(Year='19',Month='feb')  


datain<-rbind(dec.datain,jan.datain,feb.datain)%>%select(FileNumber,Stage,CaseStatus,NAME,Severity,ProsAttorneyName,Year,Month)


#--------Refused Cases------
# December#

dec.refused.cases<-datain %>% filter(CaseStatus=='REFUSED', Stage!='CLOSED',Month=='Dec')%>%distinct(FileNumber,.keep_all = TRUE)
(dec.refused.misdem.cases<-datain%>% filter(CaseStatus=='REFUSED', Stage!='CLOSED',Severity=='M',Month=='Dec')%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow())#Overall misdemeanor cases refused in Dec )
(dec.refused.misdem.cases<-datain%>% filter(CaseStatus=='REFUSED', Stage!='CLOSED',Severity=='F',Month=='Dec')%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow())#Overall Felony cases refused in Dec )

dec.refused.cases<-datain %>% filter(CaseStatus=='REFUSED', Stage!='CLOSED',Month=='Dec')%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow()


#January#
jan.refused.cases<-datain%>% filter(CaseStatus=='REFUSED', Stage!='CLOSED',Month=='Jan')%>%distinct(FileNumber,.keep_all = TRUE) #%>%nrow() #Overall Cases Refused in Jan

(jan.refused.misdem.cases<-datain%>% filter(CaseStatus=='REFUSED', Stage!='CLOSED',Severity=='M',Month=='Jan')%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow())#Overall misdemeanor cases refused in Jan )
(jan.refused.felony.cases<-datain%>% filter(CaseStatus=='REFUSED', Stage!='CLOSED',Severity=='F',Month=='Jan')%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow() )#Overall Felony Cases refused in Jan

jan.refused.cases<-datain%>% filter(CaseStatus=='REFUSED', Stage!='CLOSED',Month=='Jan')%>%distinct(FileNumber,.keep_all = TRUE) %>%nrow() #Overall Cases Refused in Jan


#Feb
feb.refused.cases<-datain%>% filter(CaseStatus=='REFUSED', Stage!='CLOSED',Month=='feb')%>%distinct(FileNumber,.keep_all = TRUE)# %>%nrow()) #Overall Cases Refused in Jan
(feb.refused.misdem.cases<-datain%>% filter(CaseStatus=='REFUSED', Stage!='CLOSED',Severity=='M',Month=='feb')%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow())#Overall misdemeanor cases refused in feb )
(feb.refused.felony.cases<-datain%>% filter(CaseStatus=='REFUSED', Stage!='CLOSED',Severity=='F',Month=='feb')%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow())#Overall misdemeanor cases refused in feb )

feb.refused.cases<-datain%>% filter(CaseStatus=='REFUSED', Stage!='CLOSED',Month=='feb')%>%distinct(FileNumber,.keep_all = TRUE) %>%nrow() #Overall Cases Refused in Jan



#______________________________________Issued Cases_________________________________
#####Dec
dec.issued.cases<-dec.datain%>% filter(CaseStatus %in% c('OPEN'),Month=='Dec')%>%distinct(FileNumber,.keep_all = TRUE)

#dec.closed.cases<-dec.datain%>% filter(CaseStatus %in% c('CLOSED'))%>%distinct(FileNumber,.keep_all = TRUE)

dec.issued.misdem.cases<-dec.issued.cases%>%filter(Severity=='M')%>%nrow()
dec.isssued.felony.cases<-dec.issued.cases%>%filter(Severity=='F')%>%nrow()

dec.issued.cases<-dec.datain%>% filter(CaseStatus %in% c('OPEN'),Month=='Dec')%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow()

#####January
jan.issued.cases<-datain%>%filter(CaseStatus %in% c('OPEN'),Month=='Jan')%>%distinct(FileNumber,.keep_all = TRUE)#%>%nrow())
(jan.issued.misdem.cases<-datain%>%filter(CaseStatus %in% c('OPEN'),Month=='Jan',Severity=="M")%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow())
(jan.issued.felony.cases<-datain%>%filter(CaseStatus %in% c('OPEN'),Month=='Jan',Severity=="F")%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow())

jan.issued.misdem.cases<-datain%>%filter(CaseStatus %in% c('OPEN'),Month=='Jan',Severity=="M")%>%distinct(FileNumber,.keep_all = TRUE)#%>%nrow())

write.csv(jan.issued.misdem.cases,"jan.misdem.csv")
jan.issued.cases<-datain%>%filter(CaseStatus %in% c('OPEN'),Month=='Jan')%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow()


#Feb
feb.issued.cases<-datain%>%filter(CaseStatus %in% c('OPEN'),Stage!="POST CONV",Month=='feb')%>%distinct(FileNumber,.keep_all = TRUE)#%>%nrow())
feb.issued.misdem.cases<-datain%>%filter(CaseStatus %in% c('OPEN'),Stage!="POST CONV",Month=='feb',Severity=="M")%>%distinct(FileNumber,.keep_all = TRUE)#%>%nrow())
write.csv(feb.issued.misdem.cases,"feb.misdem.csv")
feb.issued.felony.cases<-datain%>%filter(CaseStatus %in% c('OPEN'),Stage!="POST CONV",Month=='feb',Severity=="F")%>%distinct(FileNumber,.keep_all = TRUE)#%>%nrow())
feb.issued.cases<-datain%>%filter(CaseStatus %in% c('OPEN'),Stage!="POST CONV",Month=='feb')%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow()


#_______Cases Under Review__________
Dec.review.cases<-datain%>% filter(CaseStatus=='REVIEW',Month=="Dec")%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow()

Jan.review.cases<-datain%>% filter(CaseStatus=='REVIEW',Month=='Jan')%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow()

feb.review.cases<-datain%>%filter(CaseStatus=="REVIEW",Month=="feb")%>%distinct(FileNumber,.keep_all = TRUE)%>%nrow() #Cases in Review Status in February




#________Case Totals________
#December
Dec.accepted.cases<-dec.issued.cases+Dec.review.cases
print(Dec.accepted.cases)

Dec.case.total<-dec.issued.cases+Dec.review.cases+dec.refused.cases
print(Dec.case.total)

#January
Jan.accepted.cases<-jan.issued.cases+Jan.review.cases
print(Jan.accepted.cases)

Jan.case.total<-jan.issued.cases+Jan.review.cases+jan.refused.cases
print(Jan.case.total)

#February
Feb.accepted.cases<-feb.issued.cases+feb.review.cases
Feb.case.total<-feb.issued.cases+feb.review.cases+feb.refused.cases

#----Refusal Rate---------------
Dec.refusal.rate<-round((dec.refused.cases/Dec.case.total)*100)
Jan.refusal.rate<-round((jan.refused.cases/Jan.case.total)*100)
Feb.refusal.rate<-round((feb.refused.cases/Feb.case.total)*100)

#----finding cases where the severity is missing--------

#feb.severity.missing<-datain%>%filter(Severity=='',Month=="feb")%>%distinct(FileNumber,.keep_all=TRUE)



#------------Creating a dataframe that is can be used for graphical representation
refused<-c(dec.refused.cases,jan.refused.cases,feb.refused.cases)
accepted<-c(Dec.accepted.cases,Jan.accepted.cases,Feb.accepted.cases)
totalcases<-c(Dec.case.total,Jan.case.total,Feb.case.total)
refusal_rate<-c(Dec.refusal.rate,Jan.refusal.rate,Feb.refusal.rate)
df<-data.frame(refused,accepted,totalcases,refusal_rate)
df<-t(df)
colnames(df)<-c("Dec-18","Jan-19","Feb-19")
df

df.melt<-melt(df)
df.melt.refusalrate<-df.melt%>%filter(Var1=='refusal_rate')
df.melt.totals<-df.melt%>%filter(Var1!='refusal_rate')

#-------------Generating Visualization----------------

pl.refusalrate<-ggplot(data=df.melt.refusalrate,aes(x=Var2,y=value,group=1))+geom_point(color='red',size=2)+geom_line(size=1)
pl.refusalrate+coord_cartesian(ylim=c(0,100))+xlab("Time")+ylab("Refusal Rate")+ggtitle("CAO-Refusal Rate")+ scale_y_continuous(labels = function(x) paste0(x, "%"))

pl.totalcases<-ggplot(data=df.melt.totals,aes(x=Var2,y=value,group=Var1))+geom_point(aes(color=Var1),size=3)+geom_line(aes(color=Var1),size=1)
pl.totalcases+coord_cartesian(ylim=c(0,1000))+xlab("Time")+ylab("Number of Cases")+ggtitle("CAO-Issued,Refused and Total Cases")