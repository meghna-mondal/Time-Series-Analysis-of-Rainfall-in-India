data1=read.csv(file="C:/Users/MONDAL/Desktop/SEM 6/Project/Final/northeast.csv")           #read the file
data1
matplot(data1[,1:2],data1[,3],type="l",xlab="Year--->",ylab="North East India Rainfall",main="Time Series Plot")                                                    
matplot(data1[,1:2],data1[,4],type="l",xlab="Year--->",ylab="Moving Average",main="Plot for Moving Average")                                                   
Org=data1$NorthEast                                                             #Amount of Rainfall
MA=data1$Moving.Average                                                                    #Moving Average Values
Ratio=(Org/MA)
Ratio
data=data.frame(data1,Ratio)                                                         #attaching the Percentages in dataframe
data
USI=array(dim=1)
PerJan=data[data$Month == "Jan",5]                                                         #Extracting Percentages for Jan
M1=PerJan[-1]                                                                              #discarding the NA from array
USI[1]=mean(M1)

PerFeb=data[data$Month == "Feb",5]                                                         #Repeating for Feb
M2=PerFeb[-1]                                                                              
USI[2]=mean(M2)

PerMar=data[data$Month == "Mar",5]                                                         #Repeating for March
M3=PerMar[-1]                                                                              
USI[3]=mean(M3)

PerApr=data[data$Month == "Apr",5]                                                         #Repeating for Apr
M4=PerJan[-1]                                                                              
USI[4]=mean(M4)

PerMay=data[data$Month == "May",5]                                                         #Repeating for May
M5=PerMay[-1]                                                                              
USI[5]=mean(M5)

PerJun=data[data$Month == "Jun",5]                                                         #Repeating for June
M6=PerJun[-1]                                                                              
USI[6]=mean(M6)

PerJul=data[data$Month == "Jul",5]                                                         #Repeating for July
M7=PerJul[-146]                                                                              
USI[7]=mean(M7)

PerAug=data[data$Month == "Aug",5]                                                         #Repeating for Aug
M8=PerAug[-146]                                                                              
USI[8]=mean(M8)

PerSept=data[data$Month == "Sept",5]                                                       #Repeating for Sept
M9=PerSept[-146]                                                                              
USI[9]=mean(M9)

PerOct=data[data$Month == "Oct",5]                                                         #Repeating for Oct
M10=PerOct[-146]                                                                              
USI[10]=mean(M10)

PerNov=data[data$Month == "Nov",5]                                                         #Repeating for Nov
M11=PerNov[-146]                                                                              
USI[11]=mean(M11)

PerDec=data[data$Month == "Dec",5]                                                         #Repeating for Dec
M12=PerDec[-146]                                                                           
USI[12]=mean(M12)

USI                                                    #Unadjusted Seasonal Indices
Af=1200/sum(USI)                                       #Adjustment Factor
ASI=USI*Af
ASI
Month=1:12
plot(Month,ASI,type="l",xlab="Month Index",ylab="Seasonal Indices",main="Plot for Seasonal Indices")

SeasonalIndices=rep(ASI,each=1,times=146)              #Adjusted Seasonal Indices
ut=Ratio/SeasonalIndices                               #Eliminating Seasonality
Ratio
ut
matplot(data1[,1:2],ut,type="l",xlab="Year--->",ylab="ut",main="Periodogram")                       #Periodogram
u=ut[-(1:6)]
vt=u[-(1741:1746)]                                     #Eliminating NA Values
k=as.array(vt)
k
   
sum1=sum2=0
a=array(dim=1)
b=array(dim=1)
A=array(dim=1)
B=array(dim=1)
Rmu=array(dim=1)

for(i in 1:135)
{
  for(t in 1:1740)
  { 
     a[t]=k[t]*cos(2*3.147*t/i)
     sum1=sum1+a[t]
     b[t]=k[t]*sin(2*3.147*t/i)
     sum2=sum2+b[t]
  }
  A[i]=(2/1740)*sum1
  B[i]=(2/1740)*sum2
  Rmu[i]=(A[i]^2)+(B[i]^2)
}
Rmu
x=as.vector(Rmu)
max=max(Rmu)
lambda=which(x==max,T)                                 #true period
lambda
A0=mean(k)
A0
A1=A[lambda]
A1
B1=B[lambda]
Ut=array(dim=1)
for(t in -5:1746)
{
  Ut[t]=A0+(A1*cos(2*3.14*t/lambda))+(B1*sin(2*3.14*t/lambda))
}
Ut
It=ut/Ut
It
matplot(data1[,1:2],It,type="l",xlab="Year--->",ylab="It",main="Irregular Fluctuation")    

library(trend)
mk.test(Org)                        
