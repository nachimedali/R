require(xlsx)
require(doBy) 
data<-read.xlsx("C:/datafile.xlsx",sheetIndex=1)

x=as.character(data[1,1])

####################################################"récupération et traitement lhs
mydecoup_lhs=function(x){
  t=gsub(x,pattern = "}",replacement = "")
  t=gsub(t,pattern = "{",replacement = "",fixed = T)
  t=gsub(t,pattern = " ",replacement = "")  
  t=strsplit(t,split = "=>")[[1]]
  y=t[1]
  z=t[2]
  y=strsplit(y,",")[[1]]
  return(y)
}
Xs=sapply(1:nrow(data),function(x) mydecoup_lhs(data[x,1]))
Xz=as.data.frame(Xs)
mx=lapply(Xs,length)
mx=max(unlist(mx))
Xd_lhs=data.frame(matrix(0,nrow = length(Xs),ncol = mx))
for(i in 1:nrow(Xd_lhs)){
  l=length(Xs[[i]])
  Xd_lhs[i,1:l]=Xs[[i]]
}
View(Xd_lhs)

######################################################################récupération et traitement rhs
mydecoup_rhs=function(x){
  t=gsub(x,pattern = "}",replacement = "")
  t=gsub(t,pattern = "{",replacement = "",fixed = T)
  t=gsub(t,pattern = " ",replacement = "")  
  t=strsplit(t,split = "=>")[[1]]
  y=t[1]
  z=t[2]
  y=strsplit(y,",")[[1]]
  return(z)
}
Xs=sapply(1:nrow(data),function(x) mydecoup_rhs(data[x,1]))
Xz=as.data.frame(Xs)
mx=lapply(Xs,length)
mx=max(unlist(mx))
Xd_rhs=data.frame(matrix(0,nrow = length(Xs),ncol = mx))
for(i in 1:nrow(Xd_rhs)){
  l=length(Xs[[i]])
  Xd_rhs[i,1:l]=Xs[[i]]
}
colnames(Xd_rhs)<-"rhs"
View(Xd_rhs)
#######################################concat rhs & lhs
Xd_lhs_rhs<-data.frame(Xd_lhs,Xd_rhs)
View(Xd_lhs_rhs)

############################somme de lhs + rhs
#u<-c()
#for(i in 1:length(Xd_lhs_rhs[,1]))
  #u[i]<-sum(as.numeric(Xd_lhs_rhs[i,]))

########################################################### The data.frame constructions
uu<-data.frame(Xd_lhs_rhs,support=data$support,confidence=data$confidence,lift=data$lift)
data1<-uu
View(data1)

#####################################################classification 
1## Agglomerate Hierarchical Clustering
for(i in 1:length(data1[1,])) data1[,i]<-as.numeric(data1[,i])##########convert the rows format into num

data1_new<-data1[1:300,]
class_new<-scale(data1_new, center = TRUE, scale = TRUE)
dc<-dist(class_new, method ="euclidean", diag=FALSE, upper=FALSE)
hier<-hclust(dc,"ward.D")

plot(hier,hang=-1)
cl4<-cutree(hier,8)
rect.hclust(hier,k=8)
################################################number of rules in each cluster
a<-table(cl4)
hier$order
a1<-c()
for(k in 1:length(a)) a1[k]<-a[[k]]
###############################################recuperate the rules for each cluster

uu2<-data.frame(uu[1:300,],cluster=as.data.frame(cl4))
aa<-list(data.frame()) 
for(k in 1:length(uu2[,1])) aa[[k]]<-uu2[uu2$cl4==k,]
aa[[2]]
#################################################### unique Trigger for each cluster 
aaa<-list()
for (i in 1:8) aaa[[i]]<-unique(aa[[i]]$X1) 

####################################################list of Trigger
hh<-unlist(aaa)

############################""120 rules
class<-dbSendQuery(con,"Select distinct fusion, Hashing from temp3 where HASHING in ('824539','1754771','2642968','3563654','1693139','2258331','3835194','2402316','3226101','2987736','1977428'
,'1155898','264365','3371051','2247246','1258648','1047901','1354939','202011','401833','116964','173055'
,'756045','910511','3591606','3806214','3516206','2444611','2548995','2456198','2097923','1665161','2281410'
,'1090267','179673','206875','709630','213693','391256','8178','2338565','2737392','1840672','830116'
,'281402','3370642','3516206','2444611','2548995','2456198','1665161','2281410','2426236','2843603','3276092'
,'3466501','1851912','2548995','2456198','2097923','1665161','2281410','2426236','2843603','3276092','3466501','1851912')")
class_Trigger<-fetch(class)


###############################300 rules 
class<-dbSendQuery(con,"Select distinct fusion, Hashing  from temp3 where HASHING in ('824539','3563654','1693139','2258331','3835194','2402316','3226101','2987736','1977428','1155898','264365', 
                   '3371051','2247246','1258648','1047901','1354939','202011','3195573','3657505','1326796','401833','116964', 
                   '173055','756045','910511','1665161','1851912','1262287','1288017','3591606','3806214','2642968','3516206',
                   '2843603','3276092','3466501','2281410','2426236','3688504','3462255','3782497','3071602','2805041','2812105',
                   '1090267','179673','206875','709630','2444611','2548995','2456198','213693','391256','2097923','1665161',
                   '1851912','2281410','2426236','2843603','756045','910511','1262287','1288017','1754771','2444611','2548995',
                   '2456198','2097923','1665161','2281410','2426236','1851912','2843603','3276092','3466501','3688504','3462255',
                   '3782497','3071602','2190694','2805041','2812105','8178' ,'2204610','2343259','1285144','2800468','2510771',
                   '2344624','2232749','573508','2179688','2018216','2010839','255508','1404748','2420690','895344','272190', 
                   '2415313','2335732','2338565','2737392','1840672','830116','281402','3370642','1205363','506848','3450935',
                   '2484593','1769071','2204610','3123942','1262287','3688504','3462255','1288017','3782497','3071602')")

class<-fetch(class)
class<-write.table(class,"C:/Users/nachi/Desktop/class.txt",sep="\t") 

i=hier$order
class=uu[i,]    
class<-write.xlsx(class,"C:/Users/nachi/Desktop/class.xlsx") 



###########################################recuperate list of triggers in each set of rules
###representation of clusters
library(cluster)
clusplot(dc,cl4,diss=T,shade=T,color=T,labels=4,main="")
abline(v=0,h=0)


2## k-means classification
library(RWekajars)
library(partykit)
library(rJava)
class_new<-na.omit(class_new)
class_new<-class_new[,-2]
dim(class_new)
cl2<-kmeans(class_new,7)
cl2
cl2$cluster
cl2$size
############PCA with ggplot2 to determinate the classess
library(FactoMineR)
acpa=PCA(class_new,scale.unit = TRUE, ncp = 5, quali.sup =5,graph=F)
PCA2=data.frame(dim1=acpa$ind$coord[,1],dim2=acpa$ind$coord[,2],qlte=acpa$ind$cos2[,1]+acpa$ind$cos2[,2],class=class_new)
library(ggplot2)
gkm=ggplot(PCA2)+geom_point(aes(x=dim1,y=dim2,colour=factor(cl2$cluster),size=2))
gkm=gkm+geom_hline(yintercept = 0, colour = "gray65") +geom_vline(xintercept = 0, colour = "gray65")
gkm=gkm+theme(legend.position="none")+labs(title = "K-Means")
require(gridExtra)
grid.arrange(gkm)

3## Silhouettes

Dist=dist(class_new)
silKM=silhouette(cl2$cluster,Dist)

4## XMeans
library(rJava)
library(RWekajars)
library(partykit)
library(mlbench)
library(e1071)
library(RWeka)
WPM("install-package", "XMeans")

cl3 <- XMeans(class_new)
