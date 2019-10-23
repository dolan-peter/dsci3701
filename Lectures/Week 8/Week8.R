library(rpart)
df <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
df$rank=factor(df$rank) # Best for rank to be a factor
admit.tree=rpart(admit ~ ., data=df,method="class") #Notice the class method
#Plotting
library(rpart.plot)
prp(admit.tree)
prp(admit.tree,extra=TRUE)
#Predictions
head(predict(admit.tree,type="prob"))
head(predict(admit.tree,type="class"))

tree.p=predict(admit.tree,type="prob")[,2]
tree.class=predict(admit.tree,type="class")
ov=admit$df
boxplot(tree.p~ov) #See the separation?

(tb<-table(ov,pv=tree.class))
sum(diag(tb))/sum(tb)

# Entropy subtleties
curve(x*log(base=2,x),0,1)
#Split example:
partitions=split(df$admit,df$gpa<3.4)
sapply(partitions,length)
lapply(partitions,table) # Proportions
(entropys<-sapply(partitions,function(d){tb<-table(d);p<-tb/sum(tb);-sum(p*log(base=2,p))}))
(new.entropy<-mean(entropys)) #Since lengths are of equal size
#0.8728
tb<-table(df$admit)
p=tb/sum(tb)
(old.entropy<- -1*sum(p*log(base=2,p)))
#0.9016421
old.entropy-new.entropy

#Different split
partitions=split(df$admit,df$gpa<3.0) #Split on 3.0
sapply(partitions,length)
lapply(partitions,table) # Proportions
(entropys<-sapply(partitions,function(d){tb<-table(d);p<-tb/sum(tb);-sum(p*log(base=2,p))}))
(lengths=sapply(partitions,length))
(weights<-lengths/sum(lengths))
(new.entropy<-sum(entropys*weights))
(info.gain<-old.entropy-new.entropy)

#Consider a function to calculate the new entropy based a variable and a threshold

partition.entropy=function(feature,threshold){
  partitions=split(df$admit,df[[feature]]<threshold)
  entropys<-sapply(partitions,function(d){tb<-table(d);p<-tb/sum(tb);-sum(p*log(base=2,p))})  
  lengths=sapply(partitions,length)
  weights<-lengths/sum(lengths)
  return(sum(entropys*weights))
}

PE=Vectorize(partition.entropy,"threshold")
curve(PE("gpa",x),0,4)

#Bagging