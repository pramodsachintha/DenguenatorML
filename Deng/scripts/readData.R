require(data.table)
require("sfsmisc")


#Read "dengueCases2014.csv"
dengue2014 = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Dengue/dengueCases2014.csv", data.table = F, header = F, col.names = c("id", "MOH_name", c(1:52), "Total"))

#Read "temp.csv"
tempCsv = fread("/media/suchira/0A9E051F0A9E051F/CSE 2012/Semester 07-08/FYP/Denguenator/Dengunator 2.0/Data/Met_data/temp/temp.csv", data.table = F, header = T)

#Read results
mfrow = c(1,1)
par(mfrow=mfrow)                                              # divides the plot area into two up and two down
mult.fig(mfrow = mfrow,main="MetaPop model of Dengue")

results = fread("myfile.csv", data.table = F, header = F, col.names = c(c(1:52), "Total"))
results[results < 0] = 0
plot(x = c(1:52), y = results[1,][1:52], lwd=3, xlab="Week",ylab="Total Infected", type = "l", col = "red", main = "Actual vs Predicted")
lines(x = c(1:52), y = results[2,][1:52], xlab = "", ylab = "", lwd=3, type = "l", col = "black")
axis(4)
#mtext("y2",side=4,line=3)
legend("topleft",col=c("red","black"),lty=1,legend=c("Actual","Predicted"))



#Plot diagrams
mfrow = c(2,1)
par(mfrow=mfrow)                                              # divides the plot area into two up and two down
mult.fig(mfrow = mfrow,main="SIR model of Dengue")

plot(x = c(1:52), y = currentMOH[3:54], lwd=3, xlab="Week",ylab="Total Infected", type = "l", col = "red", main = "Dengue 2013")
par(new = T)
plot(x = c(1:52), y = tempCsv[138,][3:54], xaxt="n",yaxt="n", xlab = "", ylab = "", lwd=3, type = "l", col = "blue")
axis(4)
#mtext("y2",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("Dengue 2013","Temperature"))

plot(c(1:52), dengue2014[181,][3:54], xlab = "Week", ylab = "Total Infected", type = "l", col = "red", lwd=3, , main = "Dengue 2014")