###1
set.seed(104823)
ct = 0  # counter for the number of intervals that contain the no practical difference
delta<-4.5  ##delta value
for (i in 1:10^6){ ##makes a loop to repeat the process 10^6 times
  d1 = rnorm(8, mean = 280, sd = 2.5) # generates random samples for the scene
  d2 = rnorm(8, mean = 280, sd = 2.5)  # generates random samples for the suspect
  s.1<-sd(d1)^2  ##gets the s1 value for suspect sample
  s.2<-sd(d2)^2  ##gets the s2 value for scene sample
  df=floor((((s.1)/8)+((s.2)/8))^2/((((s.1/8)^2)/(8-1))+(((s.2/8)^2)/(8-1)))) ##calculates the df that will be used for tstar calculation
  tstar<-qt(0.005,df=df,lower.tail=FALSE)   ##calculates the tstar
  lcl = mean(d1)-mean(d2) - tstar*sqrt((s.1+s.2)/8)   ##finds the lower confidence interval
  ucl = mean(d1) - mean(d2) +tstar*sqrt((s.1+s.2)/8)    ##finds the upper confidence interval
  if (lcl > -delta & ucl < delta){  ##adds 1 to ct if lcl is greater than the negative delta and ucl is less than positive delta
    ct = ct + 1
  }
}
ct/10^6
##0.43102       ##proportion that result in "no practical difference" for ##1

##2    ##same function as above but suspect sample has a different mean
ct = 0  # counter for the number of intervals that contain the no practical difference
delta<-4.5  ##delta value
for (i in 1:10^6){ ##makes a loop to repeat the process 10^6 times
  d1 = rnorm(8, mean = 280, sd = 2.5) # generates random samples for the scene
  d2 = rnorm(8, mean = 275, sd = 2.5)  # generates random samples for the suspect
  s.1<-sd(d1)^2  ##gets the s1 value for suspect sample
  s.2<-sd(d2)^2  ##gets the s2 value for scene sample
  df=floor((((s.1)/8)+((s.2)/8))^2/((((s.1/8)^2)/(8-1))+(((s.2/8)^2)/(8-1)))) ##calculates the df that will be used for tstar calculation
  tstar<-qt(0.005,df=df,lower.tail=FALSE)   ##calculates the tstar
  lcl = mean(d1)-mean(d2) - tstar*sqrt((s.1+s.2)/8)   ##finds the lower confidence interval
  ucl = mean(d1) - mean(d2) +tstar*sqrt((s.1+s.2)/8)    ##finds the upper confidence interval
  if (lcl > -delta & ucl < delta){  ##adds 1 to ct if lcl is greater than the negative delta and ucl is less than positive delta
    ct = ct + 1
  }
}
ct/10^6

## 0.001371     ##proportion that result in "no practical difference" for ##2. 