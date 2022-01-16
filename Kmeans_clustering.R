library(RCurl)

url <- "https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/snsdata.csv"
sns <- read.csv(url)

str(sns)

table(sns$gender, useNA = "ifany")

prop.table(table(sns$gender, useNA = "ifany")) * 100

#get the real teenager age
sns$age <- ifelse(sns$age >= 13 & sns$age <20, sns$age, NA)

summary(sns$age)


sns$female <- ifelse(sns$gender == "F" & !is.na(sns$gender), 1, 0)

sns$no_gender <- ifelse(is.na(sns$gender), 1, 0)

table(sns$gender, useNA = "ifany")

aggregate(data = sns, age ~ gradyear,mean, na.rm = T)



ave_age <- ave(sns$age, sns$gradyear, FUN = function(x) mean(x, na.rm =T))

sns$age <- ifelse(is.na(sns$age), ave_age, sns$age)

summary(sns$age)

##Clustering

#Selecting the useful info
interests <- sns[5:40]

interestsZ <- as.data.frame(lapply(interests, scale))

set.seed(2345)
snsCluster <- kmeans(interestsZ, 5)
  
snsCluster$size
snsCluster$centers

snsPlusCluster <- pracma::kmeanspp(interestsZ, centers = 5)


#adding clusters to the main df

sns$clusters <- snsCluster$cluster

aggregate(data = sns, age ~clusters, mean)

aggregate(data = sns, female~clusters, mean)


fviz_cluster(snsCluster, data = interestsZ, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(interestsZ)

table(sns$gradyear, snsCluster$cluster)

table(snsCluster$cluster, snsCluster$withinss)

# ClusterR
library(ClusterR)


clusterR <- MiniBatchKmeans(data = interests, clusters = 5, num_init = 50)

library(factoextra)

snsEclust<-  eclust(interestsZ,"kmeans",nstart = 25)






##Anxiety

library(dplyr)
library(factoextra)
library(plotly)

aggregate(data = datarium::anxiety, cbind(t1,t2,t3)~ group, mean, na.rm =T)

anxietyAll <- datarium::anxiety
anxiety <- datarium::anxiety %>% select(-group, -id)


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

#Lets check for the elbow point according to withing groups sum of squares
wssplot(anxiety)
#2 or 3 seem to be good elbow points but lets do 2,3 and 4 clusters for sport

#k =2
set.seed(1234)
anxietyCluster2 <- kmeans(anxiety, 2, nstart = 20, algorithm = "Lloyd")

fviz_cluster(anxietyCluster2, data = anxiety,
             geom = "point", ellipse.type = "convex")

anxietyAll$Cluster2 <- as.factor(anxietyCluster2$cluster)

table(anxietyAll[,c("Cluster2", "group")])

p2 <- plot_ly(anxietyAll, 
              x=~t1, y=~t2, z=~t3, 
              color=~Cluster2,
              symbol = ~group,
              symbols = c('circle','x','o'),
              colors = "Set1",
              text = ~paste("Paricipant: ", id), 
              marker = list(size= 10))

print(p2)

#it seems like we don't have logical clusters

#k = 3
set.seed(1234)
anxietyCluster3 <- kmeans(anxiety, 3)

a<- fviz_cluster(anxietyCluster3, data = anxiety,
             geom = "point", ellipse.type = "convex")

anxietyAll$Cluster3 <- as.factor(anxietyCluster3$cluster)


table(anxietyAll[,c("Cluster3", "group")])

p3 <- plot_ly(anxietyAll, 
              x=~t1, y=~t2, z=~t3, 
              color=~Cluster3,
              symbol = ~group,
              symbols = c('circle','x','o'),
              colors = "Set1",
              text = ~paste("Paricipant: ", id), 
              marker = list(size= 10))
print(p3)

library(animation) 

set.seed(1234)
kmeans.ani(anxiety,2) 

# We can safely say that all of the participants in the 3rd group are clustered as 3. However,
# there are some participants in the 2nd group who have been clustered as 3 as well...
# Additionally, there are some participants in the 1st group that have been clustered as 3.

#k = 4
set.seed(1234)
anxietyCluster4 <- kmeans(anxiety, 4)

fviz_cluster(anxietyCluster4, data = anxiety,
             geom = "point", ellipse.type = "convex")

anxietyAll$Cluster4 <- as.factor(anxietyCluster4$cluster)

table(anxietyAll[,c("Cluster4", "group")])

p4 <- plot_ly(anxietyAll, 
              x=~t1, y=~t2, z=~t3, 
              color=~Cluster4,
              symbol = ~group,
              symbols = c('circle','x','o'),
              colors = "Set1",
              text = ~paste("Paricipant: ", id), 
              marker = list(size= 10))

print(p4)

#4 Clusters don't seem to make sense.. It's better to leave it for now..

#ClusterR
library(ClusterR)

km_model <- MiniBatchKmeans(anxiety, clusters = 3, batch_size = 20, num_init = 5, max_iters = 100, 
                            init_fraction = 0.2, initializer = 'kmeans++', early_stop_iter = 10,
                            verbose = F)

km_model$WCSS_per_cluster

anxietyAll$clusters <- anxietyCluster$cluster


fviz_cluster(km_model, data = anxiety,
             geom = "point", ellipse.type = "convex")

quickplot(km_model)
