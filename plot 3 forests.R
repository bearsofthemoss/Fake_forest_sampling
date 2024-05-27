

setwd("D:/Users/bears/Downloads")

#fake_F/fake.tree.Wayson.csv"

arb<-read.csv("fakeforest_arbogast.csv")
uni<-read.csv("fakeforest_uniform.csv")
v2<-read.csv("fakeforest_V2.csv")

par(mfrow=c(3,2))
plot(uni$x~ uni$y, main="uniform", )
plot(arb$x~ arb$y, main="reverse-J")
plot(v2$biomass~ v2$dbh, main="Fake forest v2")

hist(uni$y)
hist(arb$y)
hist(v2$dbh)


 par(mfrow=c(2,3),mai=c(0.3,0.3,0.3,0.3))
 plot(uni$x~ uni$y, main="uniform", )
 plot(arb$x~ arb$y, main="even-aged")
 plot(v2$biomass~ v2$dbh, main="Reverse J")
 hist(uni$y, breaks=100)
 hist(arb$y, breaks=100)
 hist(v2$dbh, breaks=100)
