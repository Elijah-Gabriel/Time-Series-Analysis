#Problem 1#
y=MaunaLoa$interpolated
x= 1:length(y)
LinearModel=lm (y~x)
plot(y~x)
lines(LinearModel$fitted,type="l", col="red")
detrend=LinearModel$residual
plot(detrend)

#Problem 2#
subset_y=y[203:394]
subset_x=c(1:192)
LinearModel2= lm(subset_y ~ subset_x)
plot(subset_y~subset_x)
lines(LinearModel2$fitted,type="l", col="red")
detrend2=LinearModel2$residual
plot(detrend2)

#Problem 3#
monthlyAverage=function(detrend,month)
{
  average = which (detrend$Month==month)
  detrendsum=sum(detrend[average,]$interpolated)
  detrendlength=length(detrend[average,]$interpolated)
  monthaverage= detrendsum/detrendlength
  return (monthaverage)
}

deCycle = function(detrend)
{
    monthlist=seq(from=1, to=12,by=1)
    for (i in 1:12)
      monthlist [i] = monthlyAverage (detrend,i)
    print (detrend)
    for (i in 1:192)
      detrend [i,]$interpolated = detrend [i,]$interpolated - monthlist [detrend[i,]$Month]
    return (detrend)
}

MaunaLoasubset=MaunaLoa[203:394,] 
MaunaLoasubset$interpolated=LinearModel2$residual
MaunaLoasubsetD=deCycle(MaunaLoasubset)
plot(LinearModel2$residual)
points(MaunaLoasubsetD$interpolated, col ="red")