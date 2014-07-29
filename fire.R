##########################
#by: Kelly
##########################

library(mgcv)
library(rgl)
library(akima)
dist<-function(a,b=0){
	if(length(dim(a))==0){return(sqrt(sum((a-b)^2)))}
	else{return(apply(a-b,1,dist))}
}
rescale<-function(x,min,max){
	if(length(dim(x))!=0){
		min<-matrix(rep(min,length(x[,1])),ncol=length(min),byrow=T)
		max<-matrix(rep(max,length(x[,1])),ncol=length(max),byrow=T)
	}
	return((x-min)*100/(max-min))
}
change<-function(x){
	center<-matrix(c(50,150,250,50,200,180),nrow=2,byrow=T)
	temp<-list()
	for(i in 1:3){temp[[i]]<-which(abs(x[,1]-center[1,i])<=50)}
	for(i in 1:3){
		len<-length(temp[[i]])
		r<-runif(len)
		r<-r^2*70
		the<-runif(len)*2*pi
		x[temp[[i]],1]<-cos(the)*r+center[1,i]
		x[temp[[i]],2]<-sin(the)*r+center[2,i]
	}
	rm(temp)
	return(round(x))
}
ran<-function(){
	temp<-matrix(0,nrow=8*360*30,ncol=5)
	for(i in 1:8){
		temp[((i-1)*360*30+1):(i*360*30),1]<-i+1988
		temp[((i-1)*360*30+1):(i*360*30),2]<-rep(1:360,30)
		temp[((i-1)*360*30+1):(i*360*30),3]<-round(runif(360*30)*100)
		temp[((i-1)*360*30+1):(i*360*30),4]<-round(runif(360*30)*100)
	}
	return(temp)
}
d<-read.csv("/fire-1.csv",sep=";")
d<-as.matrix(d)
d<-rbind(d,d,d)
d[,3]<-d[,3]+(d[,2]-1)*30
d<-cbind(d[,-c(2,4)],rep(1,length(d[,1])))
min<-apply(d[,3:4],2,min)
max<-apply(d[,3:4],2,max)
d[,3:4]<-round(rescale(d[,3:4],min,max))


dat<-readShapePoly('/china-province-border-data/bou2_4p.shp')
hk<-dat[which(dat$NAME=="\xcf\xe3\xb8\xdb\xccر\xf0\xd0\xd0\xd5\xfe\xc7\xf8"),]
plot(hk)
par(new=T)
plot(c(0,0),xlim=c(0,100),ylim=c(0,100))
grid(nx=10,ny=10)

point<-matrix(c(10,15,20,18,45,60,62,80,80,84,62,30,30,30,56,60,75,50,60,64,59,28,2,2,2,3,5,3,2,2,2,2,1),ncol=3)
simu<-function(x){
	temp<-c()
	for(i in 1:length(point[,1])){
		the<-runif(x*point[i,3])*2*pi
		r<-runif(x*point[i,3])*point[i,3]
		temp<-rbind(temp,matrix(c(r*sin(the)+point[i,1],r*cos(the)+point[i,2]),ncol=2))
	}
	return(temp)
}
temp<-simu(1800)
temp<-temp[sample(length(temp[,1]),length(d[,1])),]
d[,3:4]<-temp
d<-rbind(d,ran())

d<-data.frame(d)
d<-d[!duplicated(d[,1:4]),]
names(d)<-c("year","day","long","lat","response")
rate<-length(d[,1])/(100*100*360*8)
pp<-gam(response~year+s(day)+s(long,lat),data=d,offset=rep(log(1/rate),length(d[,1])),family="binomial")
temp<-c()
for(i in 1:50){temp<-c(temp,rep(i*2,50))}
k<-data.frame(year=1996,day=1,long=rep((1:50)*2,50),lat=temp)
qq<-predict(pp,newdata=k)
surface3d(1:50,1:50,qq,front="line",back="line")
axes3d()

