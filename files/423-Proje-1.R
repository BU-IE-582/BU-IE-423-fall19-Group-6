library("jpeg", lib.loc="C:/Program Files/R/R-3.4.4/library")
img <- readJPEG("C:/Trial7.jpeg")

str(img)

dim(img)

plot(0:512+0.5, 0:512+0.5, type="n", asp=1)
rasterImage(img, 0.5, 0.5, 512.5, 512.5,interpolate=FALSE)

splitimg <- img
plot(0:512+0.5, 0:512+0.5, type="n", asp=1)
splitimg[,,] <- 0
splitimg[,,1] <- img[,,1]
rasterImage(splitimg[,,], 0.5, 0.5, 512.5, 512.5,interpolate=FALSE)
plot(0:512+0.5, 0:512+0.5, type="n", asp=1)
splitimg[,,] <- 0
splitimg[,,2] <- img[,,2]
rasterImage(splitimg[,,], 0.5, 0.5, 512.5, 512.5,interpolate=FALSE)
plot(0:512+0.5, 0:512+0.5, type="n", asp=1)
splitimg[,,] <- 0
splitimg[,,3] <- img[,,3]
rasterImage(splitimg[,,], 0.5, 0.5, 512.5, 512.5,interpolate=FALSE)

chan1<-1:512
chan2<-1:512
chan3<-1:512
for(i in 1:512)
  chan1[i]<-mean(img[,i,1])
for(i in 1:512)
  chan2[i]<-mean(img[,i,2])
for(i in 1:512)
  chan3[i]<-mean(img[,i,3])
par(mfrow=c(2,2))
plot(chan1,type="l", col="red")
plot(chan2,type="l", col="green")
plot(chan2,type="l", col="blue")


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
d2 <- matrix(1, 512, 256)
for (i in 1:512)
{
  for (j in 1:256)
  {
    d2[i, j] <- img[i, j, 1]
    
  }
}
d3 <- matrix(1, 512, 256)
for (i in 1:512)
{
  for (j in 1:256)
  {
    d3[i, j] <- img[i, j + 256, 1]
    
  }
}
d5 <- matrix(1, 512, 256)
for (i in 1:512)
{
  for (j in 1:256)
  {
    d5[i, j] <- d3[i, j] - d2[i, j]
    
  }
}
d6 <- matrix(1, 512, 256)
d6 <- normalize(d5)
plot(0:512 + 0.5, 0:512 + 0.5, type = "n", asp=1)
splitimg[, , ] <- 0
splitimg[, , 1] <- d6[, ]
rasterImage(splitimg[,1:256,], 0.5, 0.5, 256.5, 512.5, interpolate = FALSE)



c2 <- matrix(1, 512, 256)
for (i in 1:512)
{
  for (j in 1:256)
  {
    c2[i, j] <- img[i, j, 2]
    
  }
}
c3 <- matrix(1, 512, 256)
for (i in 1:512)
{
  for (j in 1:256)
  {
    c3[i, j] <- img[i, j + 256, 2]
    
  }
}
c5 <- matrix(1, 512, 256)
for (i in 1:512)
{
  for (j in 1:256)
  {
    c5[i, j] <- c3[i, j] - c2[i, j]
    
  }
}
c6 <- matrix(1, 512, 256)
c6 <- normalize(c5)
plot(0:512 + 0.5, 0:512 + 0.5, type = "n", asp = 1)
splitimg[, , ] <- 0
splitimg[, , 2] <- c6[, ]
rasterImage(splitimg[,1:256,], 0.5, 0.5, 256.5, 512.5, interpolate = FALSE)

e2 <- matrix(1, 512, 256)
for (i in 1:512)
{
  for (j in 1:256)
  {
    e2[i, j] <- img[i, j, 3]
    
  }
}
e3 <- matrix(1, 512, 256)
for (i in 1:512)
{
  for (j in 1:256)
  {
    e3[i, j] <- img[i, j + 256, 3]
    
  }
}
e5 <- matrix(1, 512, 256)
for (i in 1:512)
{
  for (j in 1:256)
  {
    e5[i, j] <- e3[i, j] - e2[i, j]
    
  }
}
e6 <- matrix(1, 512, 256)
e6 <- normalize(e5)
plot(0:512 + 0.5, 0:512 + 0.5, type = "n", asp=1)
splitimg[, , ] <- 0
splitimg[, , 3] <- e6[, ]
rasterImage(splitimg[,1:256,], 0.5, 0.5, 256.5, 512.5, interpolate = FALSE)

vec2 <- 1:9
newimage3 <- matrix(vec2, 3, 3)
uc1 <- img[, , 1]
for (i in 2:511)
{
  for (j in 2:511)
  {
    for (k in 1:3)
    {
      for (l in 1:3)
      {
        newimage3[k, l] <- img[i + k - 2, j + l - 2, 1]
        
      }
    }
    uc1[i, j] <- median(newimage3)
    
  }
}
uc2 <- img[, , 2]
for (i in 2:511)
{
  for (j in 2:511)
  {
    for (k in 1:3)
    {
      for (l in 1:3)
      {
        newimage3[k, l] <- img[i + k - 2, j + l - 2, 2]
        
      }
    }
    uc2[i, j] <- median(newimage3)
    
  }
}
uc3 <- img[, , 3]
for (i in 2:511)
{
  for (j in 2:511)
  {
    for (k in 1:3)
    {
      for (l in 1:3)
      {
        newimage3[k, l] <- img[i + k - 2, j + l - 2, 3]
        
      }
    }
    uc3[i, j] <- median(newimage3)
    
  }
}
kaa <- array(1:786432, dim = c(512, 512, 3))
kaa[, , 1] <- uc1
kaa[, , 2] <- uc2
kaa[, , 3] <- uc3
plot(0:512 + 0.5, 0:512 + 0.5, type = "n", asp=1)
rasterImage(kaa, 0.5, 0.5, 512.5, 512.5, interpolate = FALSE)

vec1 <- 1:25
newimage5 <- matrix(vec1, 5, 5)

besebes1 <- img[, , 1]
for (i in 3:510)
{
  for (j in 3:510)
  {
    for (k in 1:5)
    {
      for (l in 1:5)
      {
        newimage5[k, l] <- img[i + k - 3, j + l - 3, 1]
        
      }
    }
    besebes1[i, j] <- median(newimage5)
    
  }
}

besebes2 <- img[, , 2]
for (i in 3:510)
{
  for (j in 3:510)
  {
    for (k in 1:5)
    {
      for (l in 1:5)
      {
        newimage5[k, l] <- img[i + k - 3, j + l - 3, 2]
        
      }
    }
    besebes2[i, j] <- median(newimage5)
    
  }
}

besebes3 <- img[, , 3]
for (i in 3:510)
{
  for (j in 3:510)
  {
    for (k in 1:5)
    {
      for (l in 1:5)
      {
        newimage5[k, l] <- img[i + k - 3, j + l - 3, 3]
        
      }
    }
    besebes3[i, j] <- median(newimage5)
    
  }
}

aks <- array(1:786432, dim = c(512, 512, 3))
aks[, , 1] <- besebes1
aks[, , 2] <- besebes2
aks[, , 3] <- besebes3
plot(0:512 + 0.5, 0:512 + 0.5, type = "n", asp = 1)
rasterImage(aks, 0.5, 0.5, 512.5, 512.5, interpolate = FALSE)

vec3 <- 1:121
newimage11 <- matrix(vec3, 11, 11)
onbir1 <- img[, , 1]
for (i in 6:507)
{
  for (j in 6:507)
  {
    for (k in 1:11)
    {
      for (l in 1:11)
      {
        newimage11[k, l] <- img[i + k - 6, j + l - 6, 1]
        
      }
    }
    onbir1[i, j] <- median(newimage11)
    
  }
}
onbir2 <- img[, , 2]
for (i in 6:507)
{
  for (j in 6:507)
  {
    for (k in 1:11)
    {
      for (l in 1:11)
      {
        newimage11[k, l] <- img[i + k - 6, j + l - 6, 2]
        
      }
    }
    onbir2[i, j] <- median(newimage11)
    
  }
}
onbir3 <- img[, , 3]
for (i in 6:507)
{
  for (j in 6:507)
  {
    for (k in 1:11)
    {
      for (l in 1:11)
      {
        newimage11[k, l] <- img[i + k - 6, j + l - 6, 3]
        
      }
    }
    onbir3[i, j] <- median(newimage11)
    
  }
}
caa <- array(1:786432, dim = c(512, 512, 3))
caa[, , 1] <- onbir1
caa[, , 2] <- onbir2
caa[, , 3] <- onbir3
plot(0:512 + 0.5, 0:512 + 0.5, type = "n", asp = 1)
rasterImage(caa, 0.5, 0.5, 512.5, 512.5, interpolate = FALSE)

vec4 <- 1:961
newimage31 <- matrix(vec4, 31, 31)
otuzbir1 <- img[, , 1]
for (i in 16:497)
{
  for (j in 16:497)
  {
    for (k in 1:31)
    {
      for (l in 1:31)
      {
        newimage31[k, l] <- img[i + k - 16, j + l - 16, 1]
        
      }
    }
    otuzbir1[i, j] <- median(newimage31)
    
  }
}
otuzbir2 <- img[, , 2]
for (i in 16:497)
{
  for (j in 16:497)
  {
    for (k in 1:31)
    {
      for (l in 1:31)
      {
        newimage31[k, l] <- img[i + k - 16, j + l - 16, 2]
        
      }
    }
    otuzbir2[i, j] <- median(newimage31)
    
  }
}
otuzbir3 <- img[, , 3]
for (i in 16:497)
{
  for (j in 16:497)
  {
    for (k in 1:31)
    {
      for (l in 1:31)
      {
        newimage31[k, l] <- img[i + k - 16, j + l - 16, 3]
        
      }
    }
    otuzbir3[i, j] <- median(newimage31)
    
  }
}
faa <- array(1:786432, dim = c(512, 512, 3))
faa[, , 1] <- otuzbir1
faa[, , 2] <- otuzbir2
faa[, , 3] <- otuzbir3
plot(0:512 + 0.5, 0:512 + 0.5, type = "n", asp = 1)
rasterImage(faa, 0.5, 0.5, 512.5, 512.5, interpolate = FALSE)

library("imager", lib.loc="C:/Program Files/R/R-3.4.4/library")
img <- readJPEG("C:/last.jpg")
img1<-as.cimg(img)
img <- grayscale(img1)
hist(img)

imgold<-img
library("MASS", lib.loc="C:/Program Files/R/R-3.4.4/library")
fitdistr(imgold,"normal")
mean(imgold)
sd(imgold)

upperlim<- qnorm(0.9995,mean(imgold),sd(imgold))
lowerlim<- qnorm(0.0005,mean(imgold),sd(imgold))
img[img<=lowerlim]=0
img[img>=upperlim]=0
plot(img)
plot(imgold)


imgg<-grayscale(img1)
limits<- array(dim=c(10,10,2))
for(i in 1:10){
for(j in 1:10){
k<- 51*(j-1)+1
l<- 51*(i-1)+1
patch<- imgg[k:(k+50),l:(l+50),,]
upp<-qnorm(0.9995,mean(patch),sd(patch))   
low<-qnorm(0.0005,mean(patch),sd(patch))
limits[j,i,1]<-low
limits[j,i,2]<-upp


}
}

for(k in 1:509){
for(l in 1:509){
w<- floor(k/51)+1
z<- floor(l/51)+1
if(imgg[k,l,,]<=limits[w,z,1]){
imgg[k,l,,]=0
}
if(imgg[k,l,,]>=limits[w,z,2]){
imgg[k,l,,]=0
}
}
}

imgfinal<-imgg
plot(imgfinal) 
plot(imgold)