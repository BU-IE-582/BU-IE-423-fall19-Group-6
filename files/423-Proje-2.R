setwd('/Users/kadir/Downloads/')
library("jpeg", lib.loc="~/R/win-library/3.5")
library("imager", lib.loc="~/R/win-library/3.5")
library("qcc", lib.loc="~/R/win-library/3.5")

imgg <- readJPEG('group_6.jpg')
img<-imgg
img1<-as.cimg(imgg)
imgg <- grayscale(img1) 
imgold <-imgg  

for(i in 1:400){
  meanrow <- mean(imgg[,i,,])
  sdrow <- sd(imgg[,i,,])
  uplim <- qnorm(0.9995,meanrow,sdrow)
  lowlim <- qnorm(0.0005,meanrow,sdrow)
  for(j in 1:400){
    if(imgg[j,i,,] <= lowlim){
      imgg[j,i,,] = 0
    } 
    if(imgg[j,i,,] >= uplim){
      imgg[j,i,,] = 0
    } 
  }
}

imgrow <- imgg
par(mfrow=c(1,2))
plot(imgold, main ="Original Image")
plot(imgrow, main ="Altered Image")
imgg <- imgold
for(i in 1:400){
  meancol <- mean(imgg[i,,,])
  sdcol <- sd(imgg[i,,,])
  uplim <- qnorm(0.9995,meancol,sdcol)
  lowlim <- qnorm(0.0005,meancol,sdcol)
  for(j in 1:400){
    if(imgg[i,j,,] <= lowlim){
      imgg[i,j,,] = 0
    } 
    if(imgg[i,j,,] >= uplim){
      imgg[i,j,,] = 0
    } 
  }
  
}

imgcol <- imgg
par(mfrow=c(1,2))
plot(imgold, main ="Original Image")
plot(imgcol, main ="Altered Image")

require(data.table)
bar<- img[,,1]+img[,,2]+img[,,3]
bar <- bar/max(bar)
plot(c(0,1),c(0,1),t='n')
rasterImage(bar, 0,0,1,1)
memory.limit()
memory.limit(size=56000)
r<-matrix(0,122500,2601)
for (i in 26:375)
{
  for (j in 26:375)
  {
    for (k in 1:51)
    {
      for (l in 1:51)
      {
        r[(i-1)*350+j-8775,(k-1)*51+l] <-bar[i + k - 26, j + l - 26]
      }
    }
  }
}
table<-data.table(r)
remove(r)
model<-lm(V1301~.,data=table)
table[,predicted:=predict(model,table)]
table[,residual:=table$V1301-predicted]
plot(table$residual)
hist(table$residual)
mean(table$residual)
sd(table$residual)
UCL=mean(table$residual)+3*sd(table$residual)
LCL=mean(table$residual)-3*sd(table$residual)
newbar <-matrix(0,350,350)
for (i in 26:375)
{
  for (j in 26:375)
  {
    newbar[i-25,j-25]<-bar[i,j]
  }
}
residual<-matrix(0,350,350)
for (i in 26:375)
{
  for (j in 26:375)
  {
    residual[i-25,j-25]  <-table$residual[(i-1)*350+j-8775]
  }
}

for (i in 1:350)
{
  for (j in 1:350)
  {
    if(residual[i,j]>UCL){
      newbar[i,j]<-0;}
    else if(residual[i,j]<LCL){
      newbar[i,j]<-0;}
    
  }
}
par(mfrow=c(1,2))
plot(c(0,1),c(0,1),t='n')
rasterImage(newbar, 0,0,1,1)
plot(c(0,1),c(0,1),t='n')
rasterImage(bar, 0,0,1,1)