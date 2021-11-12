

library("readxl")
install.packages("EnvStats")
install.packages('e1071', dependencies=TRUE)
install.packages('caret')
install.packages('MASS')
install.packages('fpc')
install.packages("NbClust") 
install.packages("corrplot")

install.packages("magrittr") 
install.packages("dplyr")   

library(magrittr) 
library(dplyr)    

library("EnvStats")
library('e1071', dependencies=TRUE)
library('caret')
library('MASS')
library('fpc')
library("NbClust")
library("corrplot")


library("readxl")
data_v <- read_excel("~/Desktop/RProject/vehicles.xlsx")

tbl <- with(data_v, table(Class))
barplot(tbl, beside = TRUE, legend = TRUE)

#Missing values removal

library(EnvStats)

#Circ

out_Circ <- boxplot.stats(data_v$Circ)$out
boxplot(data_v$Circ,
  ylab = "Circ"
)
mtext(paste("Outliers: ", paste(out_Circ, collapse = ", ")))

#Comp

out_Comp <- boxplot.stats(data_v$Comp)$out
boxplot(data_v$Comp,
  ylab = "Comp"
)
mtext(paste("Outliers: ", paste(out_Comp, collapse = ", ")))

#D.Circ

out_D.Circ <- boxplot.stats(data_v$D.Circ)$out
boxplot(data_v$D.Circ,
  ylab = "D.Circ"
)
mtext(paste("Outliers: ", paste(out_D.Circ, collapse = ", ")))

#Rad.Ra

out_Rad.Ra <- boxplot.stats(data_v$Rad.Ra)$out
boxplot(data_v$Rad.Ra,
  ylab = "Rad.Ra"
)
mtext(paste("Outliers: ", paste(out_Rad.Ra, collapse = ", ")))


plot(data_v$Rad.Ra, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 2*mean(data_v$Rad.Ra, na.rm=T), col="red")  # add cutoff line
text(x=1:length(data_v$Rad.Ra)+1, y=data_v$Rad.Ra, labels=ifelse(data_v$Rad.Ra>4*mean(data_v$Rad.Ra, na.rm=T),names(data_v$Rad.Ra),""), col="red") 

test1 <- rosnerTest(data_v$Rad.Ra,
  k = 3
)
test1

data_v1<-data_v[(data_v$Rad.Ra<303),]


# Pr.Axis.Ra

out_Pr.Axis.Ra <- boxplot.stats(data_v1$Pr.Axis.Ra)$out
boxplot(data_v1$Pr.Axis.Ra,
  ylab = "Pr.Axis.Ra"
)
mtext(paste("Outliers: ", paste(out_Pr.Axis.Ra, collapse = ", ")))

test2 <- rosnerTest(data_v1$Pr.Axis.Ra,
  k = 5
)
test2

data_v2<-data_v1[(data_v1$Pr.Axis.Ra<97),]

#Max.L.Ra

hist(data_v2$Max.L.Ra,
  xlab = "Max.L.Ra",
  main = "Histogram of Max.L.Ra",
  breaks = sqrt(nrow(data_v2))
) # set number of bins

out_Max.L.Ra <- boxplot.stats(data_v2$Max.L.Ra)$out
boxplot(data_v2$Max.L.Ra,
  ylab = "Max.L.Ra"
)
mtext(paste("Outliers: ", paste(out_Max.L.Ra, collapse = ", ")))

test3 <- rosnerTest(data_v2$Max.L.Ra,
  k = 5
)
test3

data_v3<-data_v2[(data_v2$Max.L.Ra<19),]

# Scat.Ra

out_Scat.Ra <- boxplot.stats(data_v3$Scat.Ra)$out
boxplot(data_v3$Scat.Ra,
  ylab = "Scat.Ra"
)
mtext(paste("Outliers: ", paste(out_Scat.Ra, collapse = ", ")))

# None 

# Elong

out_Elong <- boxplot.stats(data_v3$Elong)$out
boxplot(data_v3$Elong,
  ylab = "Elong"
)
mtext(paste("Outliers: ", paste(out_Elong, collapse = ", ")))

# None

# Pr.Axis.Rect

out_Pr.Axis.Rect <- boxplot.stats(data_v3$Pr.Axis.Rect)$out
boxplot(data_v3$Pr.Axis.Rect,
  ylab = "Pr.Axis.Rect"
)
mtext(paste("Outliers: ", paste(out_Pr.Axis.Rect, collapse = ", ")))

# None

# Max.L.Rect

out_Max.L.Rect <- boxplot.stats(data_v3$Max.L.Rect)$out
boxplot(data_v3$Max.L.Rect,
  ylab = "Max.L.Rect"
)
mtext(paste("Outliers: ", paste(out_Max.L.Rect, collapse = ", ")))

# None

# Sc.Var.Maxis

out_Sc.Var.Maxis <- boxplot.stats(data_v3$Sc.Var.Maxis)$out
boxplot(data_v3$Sc.Var.Maxis,
  ylab = "Sc.Var.Maxis"
)
mtext(paste("Outliers: ", paste(out_Sc.Var.Maxis, collapse = ", ")))

# None

# Sc.Var.maxis

out_Sc.Var.maxis <- boxplot.stats(data_v3$Sc.Var.maxis)$out
boxplot(data_v3$Sc.Var.maxis,
  ylab = "Sc.Var.maxis"
)
mtext(paste("Outliers: ", paste(out_Sc.Var.maxis, collapse = ", ")))

test4 <- rosnerTest(data_v3$Sc.Var.maxis,
  k = 1
)

data_v4<-data_v3[(data_v3$Sc.Var.maxis<1018),]


# Ra.Gyr

out_Ra.Gyr <- boxplot.stats(data_v4$Ra.Gyr)$out
boxplot(data_v4$Ra.Gyr,
  ylab = "Ra.Gyr"
)
mtext(paste("Outliers: ", paste(out_Ra.Gyr, collapse = ", ")))


# No outliers

# Skew.Maxis

out_Skew.Maxis <- boxplot.stats(data_v4$Skew.Maxis)$out
boxplot(data_v4$Skew.Maxis,
  ylab = "Skew.Maxis"
)
mtext(paste("Outliers: ", paste(out_Skew.Maxis, collapse = ", ")))

test5 <- rosnerTest(data_v4$Skew.Maxis,k = 6, alpha=0.85)
test5

# zadne niby nie jest outlierem ale bym wyjebal na probe


# Skew.maxis

out_Skew.maxis <- boxplot.stats(data_v4$Skew.maxis)$out
boxplot(data_v4$Skew.maxis,
  ylab = "Skew.maxis"
)
mtext(paste("Outliers: ", paste(out_Skew.maxis, collapse = ", ")))

test6 <- rosnerTest(data_v4$Skew.maxis,
  k = 10
)
test6



# Kurt.maxis

out_Kurt.maxis <- boxplot.stats(data_v4$Kurt.maxis)$out
boxplot(data_v4$Kurt.maxis,
  ylab = "Kurt.maxis"
)
mtext(paste("Outliers: ", paste(out_Kurt.maxis, collapse = ", ")))

test7 <- rosnerTest(data_v4$Kurt.maxis,
  k = 1
)
test7


# Kurt.Maxis 

out_Kurt.Maxis <- boxplot.stats(data_v4$Kurt.Maxis)$out
boxplot(data_v4$Kurt.Maxis,
  ylab = "Kurt.Maxis"
)
mtext(paste("Outliers: ", paste(out_Kurt.Maxis, collapse = ", ")))


# Holl.Ra

out_Holl.Ra <- boxplot.stats(data_v4$Holl.Ra)$out
boxplot(data_v4$Holl.Ra,
  ylab = "Holl.Ra"
)
mtext(paste("Outliers: ", paste(out_Holl.Ra, collapse = ", ")))


# Deleting the outliers that were not indicated by rosner test, but looked like highly insignificant
data_v5<-data_v4[(data_v4$Skew.Maxis<88),]
data_v6<-data_v5[(data_v5$Skew.maxis<20),]
data_v7<-data_v6[(data_v6$Kurt.maxis<41),]
data_v8<-data_v7[(data_v7$Kurt.Maxis<210),]

# Categorical data

counts <- table(data_v8$Class)
barplot(counts, main="Classes distribution",
   xlab="Classes")


data_v8[,2:19] = as.data.frame(data_v8[,2:19])

data_v8[,2:19] <- lapply(data_v8[,2:19], as.integer)

data_v8[,2:19] <- as.data.frame(data_v8[,2:19])


data_v9 <- data_v8
data_v8$Class = factor(data_v8$Class,
	levels = c('saab','bus','van','opel'),
	labels = c(1,2,3,4))

 M <- cor(data_v8[,2:19])
corrplot(M, method = "circle")

X <- as.data.frame(data_v8[,2:19])
y <- data_v8[,20]


library(NbClust)

# Scale

library("dplyr")
library(dplyr)

X_sc <- X %>%           # Applying functions of dplyr
mutate_at(c("Comp","Circ","D.Circ","Rad.Ra","Pr.Axis.Ra","Max.L.Ra","Scat.Ra","Elong","Pr.Axis.Rect",
	"Max.L.Rect","Sc.Var.Maxis","Sc.Var.maxis","Ra.Gyr","Skew.Maxis","Skew.maxis","Kurt.maxis","Kurt.Maxis","Holl.Ra"), ~(scale(.) %>% as.vector))
head(X_sc)    


normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

X_normalized <- as.data.frame(lapply(X, normalize))

# Having tried both normalized and scaled, I chose the scaling as my final procedure as it is more accurate.


# Comparing Eunclidean vs Manhattan number of cluster suggestions
clusterNoE=NbClust(X_sc,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")


table(clusterNoE$Best.n[1,])
barplot(table(clusterNo$Best.n[1,]),    
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters meausured by Euclidean distance")

# Euclidean 2,3,4 , but most probably 3

clusterNoM=NbClust(X_sc,distance="manhattan", min.nc=2,max.nc=10,method="kmeans",index="all")

table(clusterNoM$Best.n[1,])
barplot(table(clusterNoM$Best.n[1,]),    # provide bar charts####
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters meausured by Manhattan distance")

# Manhattan says all 2-3-4 are equally possible


# K 2 3 4 
set.seed(420)
km4 <- kmeans(X_sc, 4, nstart = 25)
km3 <- kmeans(X_sc, 3, nstart = 25)
km2 <- kmeans(X_sc, 2, nstart = 25)

# plots to compare
p4 <- fviz_cluster(km4, geom = "point", data = X_sc) + ggtitle("k = 4")
p3 <- fviz_cluster(km3, geom = "point",  data = X_sc) + ggtitle("k = 3")
p2 <- fviz_cluster(km2, geom = "point",  data = X_sc) + ggtitle("k = 2")
p4
p3
p2


# FOR K = 4 

km4

#Within cluster sum of squares by cluster:
# (between_SS / total_SS =  59.3 %)

plotcluster(X_sc, km4$cluster)
confuseTable.km4 <- table(data_v8$Class, km4$cluster)
confuseTable.km4
confusionMatrix(confuseTable.km4)  
#Accuracy : 0.3612

# K = 3 

# Now for k = 3 we will consider merging two classes intwo one

plotcluster(X_sc, data_v8$Class)

# THEFORE WE WILL MERGE OPEL AND SAAB INTO ONE CLASS

data_v10 <- data_v9
data_v9$Class[data_v9$Class=='opel']<-'saab'
data_v9$Class = factor(data_v9$Class,
	levels = c('van','bus','saab'),
	labels = c(1,2,3))

# OPEL AND SAAB IS ONE CLASS NOW 

km3
#Within cluster sum of squares by cluster:
# (between_SS / total_SS =  55.2 %)

plotcluster(X_sc, km3$cluster)
confuseTable.km3 <- table(data_v9$Class, km3$cluster)
confuseTable.km3
confusionMatrix(confuseTable.km3)  

#Accuracy : 0.4865

# K =2 

# We will yet again consider merging two classes into one 
plotcluster(X_sc, data_v9$Class)
# THEREFORE WE WILL MERGE VAN AND BUS INTO ONE CLASS

data_v11 <- data_v10
data_v10$Class[data_v10$Class=='bus']<-'van'
data_v10$Class[data_v10$Class=='opel']<-'saab'
data_v10$Class = factor(data_v10$Class,
	levels = c('saab','van'),
	labels = c(1,2))

# NOW OPEL AND SAAB ARE ONE CLASS, AND VAN AND BUS ARE ALSO ONE CLASS

km2

#Within cluster sum of squares by cluster:
#(between_SS / total_SS =  42.7 %)

plotcluster(X_sc, km2$cluster)

plotcluster(X_sc, data_v10$Class)



confuseTable.km2 <- table(y2, km2$cluster)
confuseTable.km2
confusionMatrix(confuseTable.km2)  

# Accuracy : 0.6978   


