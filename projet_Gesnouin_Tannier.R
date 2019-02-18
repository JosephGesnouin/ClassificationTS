library(cluster)
library(TSclust)
library(TSdist)
library(xts)
library(ggplot2)
library(TSrepr)
library(data.table)
library(clusterCrit)
library(PReMiuM)
library(forecast)

library(readr)
parkings <- read_csv("~/Documents/M2/M2_MLDS/parking_day.csv")
parkings <- read_csv("~/Documents/M2/M2_MLDS/parking_week.csv")
parking=parkings[,-c(1)]
View(parking)

#"ACF", "PACF", "AR.MAH", "AR.PIC", "AR.LPC.CEPS", "PER", "INT.PER", "COR", "CORT", "DWT", 
#"PDC", "PRED", "MINDIST.SAX", "SPEC.LLR", "SPEC.GLK", "SPEC.ISD", "CDM", "CID", "NCD",
#"DTWARP", "FRECHET", "EUCL"
number.max.cluster=8
method="COR"

D1 <- diss(parking, method)
summary(D1)
sort(rowMeans(as.matrix(D1)))
C1 <- hclust(D1)
plot(C1)

asw <- numeric(number.max.cluster)
for (k in 2:number.max.cluster)
  asw[k] <- pam(diss(parking, method), k=k) $ silinfo $ avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")
wss=asw


plot(2:number.max.cluster, wss[c(-1)],
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Value of the silhouette")

data_seasprof=t(as.matrix(parking))
clustering=pam(diss(data_seasprof,method), k.best)
cat("Clustering medoids: ", clustering$medoids, "\n")
wss=asw
# prepare data for plotting

data_plot <- data.table(melt(data.table(class = as.factor(clustering$clustering),
                                        data_seasprof)))
data_plot[, Time := rep(1:ncol(data_seasprof), each = nrow(data_seasprof))]
data_plot[, ID := rep(1:nrow(data_seasprof), ncol(data_seasprof))]

# prepare medoids
centers <- data.table(melt(data_seasprof[clustering$medoids,]))
setnames(centers, c("Var1", "Var2"), c("class", "Time"))
centers[, ID := class]
centers[, class:=rep(1:k.best,length(centers[,class])/k.best)]


# plot the results
ggplot(data_plot, aes(Time, value, group = ID)) +
  facet_wrap(~class, ncol = 2, scales = "free_y") +
  geom_line(color = "grey10", alpha = 0.65) +
  geom_line(data = centers, aes(Time,value),
            color = "firebrick1", alpha = 0.80, size = 1.2) +
  labs(x = "Time", y = "Load (normalised)") +
  theme_bw()

###Numbre de series par cluster:
as.data.frame(table(clustering$clustering))
clustering$id.med
clustering$clustering
clustering$medoids
parking[,26]
ts.plot(parking[,26])
f=auto.arima(parking[,26])
f


###Visu:
arrivals <- read_csv("~/Documents/M2/M2_MLDS/parking_nan.csv")
library(forecast)
library(fpp2)
arrivals=parking
arrivals=as.ts(arrivals)
autoplot(arrivals, facets = TRUE)


library(mclust)
mod4 <- Mclust(t(parking))
drmod4<- MclustDR(mod4)
summary(drmod4)
plot(drmod4, what = "boundaries", ngrid = 200)
summary(mod4, newdata = dt)

data_plot <- data.table(melt(data.table(class = as.factor(mod4$classification),
                                        data_seasprof)))
data_plot[, Time := rep(1:ncol(data_seasprof), each = nrow(data_seasprof))]
data_plot[, ID := rep(1:nrow(data_seasprof), ncol(data_seasprof))]

# plot the results
ggplot(data_plot, aes(Time, value, group = ID)) +
  facet_wrap(~class, ncol = 2, scales = "free_y") +
  geom_line(color = "grey10", alpha = 0.65) +
  labs(x = "Time", y = "Load (normalised)") +
  theme_bw()
plot(mod4, what = "density", type = "persp")


