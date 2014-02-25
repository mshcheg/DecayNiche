CWD.data <- read.csv("2012.CWD.clean.v0.6.csv", header=T)
DC.data <- CWD.data[,c("X.1", "Y", "DC")]
DC.data <- na.omit(DC.data)
DC.ppp <- ppp(DC.data$X.1, DC.data$Y, xrange=c(min(DC.data$X.1), max(DC.data$X.1)), yrange=c(min(DC.data$Y), max(DC.data$Y)), marks=factor(DC.data$DC))

DC.ppp.split <- split(DC.ppp)
names(DC.ppp.split) <- c("one", "two", "three", "four", "five")
#One simple diagnostic for dependence between points is a Morishita plot. The spatial domain
#is divided into quadrats, and the χ2 statistic based on quadrat counts is computed. The quadrats
#are repeatedly subdivided. The Morishita plot shows the χ2 statistic against the linear size of
#the quadrats.
par(mfrow=c(2,3))
miplot(DC.ppp.split$one)
miplot(DC.ppp.split$two)
miplot(DC.ppp.split$three)
miplot(DC.ppp.split$four)
miplot(DC.ppp.split$five)
miplot(DC.ppp)
#clustered??

#Test for clustering
#https://www.e-education.psu.edu/geog586/book/export/html/1734

g_one_env <-envelope(DC.ppp.split$one, Gest, nsim=99, nrank=1)
g_two_env <-envelope(DC.ppp.split$two, Gest, nsim=99, nrank=1)
g_three_env <-envelope(DC.ppp.split$three, Gest, nsim=99, nrank=1)
g_four_env <-envelope(DC.ppp.split$four, Gest, nsim=99, nrank=1)
g_five_env <-envelope(DC.ppp.split$five, Gest, nsim=99, nrank=1)

par(mfrow=c(2,3))
plot(g_one_env)
plot(g_two_env)
plot(g_three_env)
plot(g_four_env)
plot(g_five_env)

#DC 1,2,3,4 show clustering?


