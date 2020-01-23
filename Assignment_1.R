# HW#1

# put this in your .Rprofile, but just in case:
getdata = function(x) read.table(file=paste("http://www.umich.edu/~dnoll/BME503/",x,sep=""), header=T)

# problem 1
# The populations are reported in thousands of people 
# The open space in acres.

# Understand data
parkspace = getdata("parkspace.txt");
# parkspace
city = parkspace$City;
population = parkspace$Population;
area = parkspace$OpenSpace;
#city
#population
#area

# --------------------------------------------------------------------
# problem 1
# 1.a Make a bar graph for population.

# use help(barplot) to examine function
# try las=3 option to rotate name labels (this may not be in help page)
# and cex.names to shrink or magnify labels

class(population)
par(mai=c(1.7,0.8,0.5,0.6))
p1<-barplot(population, col=rainbow(length(population)), ylim=c(0,10000),cex.axis=0.85,cex.names = 0.85, beside=T, legend.text=T, main= 'Population distribution in cities',ylab="Population (thousands of people) ", names.arg = city, las=3, xlab="", font.main=20, cex.main=1.5, cex.lab=1.2)
mtext("City", side=1, line = 6, font=20, cex =1.2)
p2<-as.matrix(population)
text(p1,p2+400,labels=as.character(p2), cex = 0.7)

# 1.b Make a bar graph for open space.

class(area)
par(mai=c(1.7,0.8,0.5,0.6))
a1<-barplot(area, col=rainbow(length(area)),ylim=c(0,60000),cex.axis=0.85,cex.names = 0.85, beside=T, legend.text=T, main= 'Open spaces in cities',ylab="Open Spaces (acres)", names.arg = city, las=3, xlab="", font.main=20, cex.main=1.5, cex.lab=1.2)
mtext("City", side=1, line = 6, font=20, cex =1.2)
a2<-as.matrix(area)
text(a1,a2+2700,labels=as.character(a2), cex = 0.7)

# 1.c Obtain rates which is acres of open space per thousand residents

parkspace$Rate <- area/population;
rate <- parkspace$Rate
#options(digits=3)
#parkspace
#rate<-as.numeric(rate)

# 1.d Make a bar graph for rates

class(rate)
par(mai=c(1.5,0.8,0.5,0.7))
r1<-barplot(rate, col=rainbow(length(rate)),ylim=c(0,20),cex.axis=1,cex.names = 0.75, beside=T, legend.text=T, main= 'Acres of open space per thousand residents in cities',ylab="Open space to Population rate", names.arg = city, las=3, xlab="", font.main=25, cex.main=1.2, cex.lab=1)
mtext("City", side=1, line = 5, cex =1)
#r2<-as.matrix(Rate)
#text(r1,r2+1,labels=r2, cex = 0.7)
#parkspace

# 1.e In 1.d, order the cities by their open space to population rate
# parkspace<-parkspace[order(parkspace$Rate),]

r <- parkspace[order(parkspace$Rate),]
class(r)
par(mai=c(1.5,0.8,0.5,0.7))
barplot(r$Rate, col=rainbow(length(r$Rate)),ylim=c(0,20),cex.axis=1,cex.names = 0.75, beside=T, legend.text=T, main= 'Acres of open space per thousand residents in cities',ylab="Open space to Population rate", names.arg = city, las=3, xlab="", font.main=25, cex.main=1.2, cex.lab=1)
mtext("City", side=1, line = 5, cex =1)

# ------------------------------------------------------------------

# problem 2
#The WHO criterion for BMD is a BMD that is 2.5 standard deviations 
#below the mean for young adults.  BMD follows a Normal distribution.
#For each of the following, find the solution 2 different ways: 
#1) calculate from theoretical probabilities obtained using R 
#functions,  - pnorm/qnorm and
#2) simulate by generating, for example, 1000 samples from the 
#distribution and calculating the percentage.-rnorm

# 2.a What percent of healthy young adults have osteoporosis based
# on the WHO definition? 

#Theoretical Probability

# OYT - Young adults having osteoporosis theoretically
# PYAT - percent of young adults with osteoporosis theoretically
# Using pnorm((-2.5-mean(stress))/sd(stress))
OYT = pnorm((-2.5-0)/1)
PYAT =100*OYT

# Random Sampling

# OYR Young adults having osteoporosis deduced from randomly generated data

YA_random<-rnorm(1000,mean=0,sd=1)

# Method 1

OYR1=sum(YA_random<(-2.5))

# or Method 2

CountPercent <- function(x,y) {
  count = 0
  for(i in 1:length(x))
  {
    if(x[i] < y)
    {
      count = count + 1
    }
  }
  return(count)
}
OYR2=CountPercent(YA_random,-2.5)

PYAR =100*OYR1/length(YA_random)

# Output of Problem 2.a
cat(PYAT,"% (theoretically) and",
    PYAR,"% (randomly) \n of young adult population has osteoporosis.")
# 2.b Women aged 70 to 79 are of course not young adults.
# The mean BMD for this group is about -2 on the standard 
# (normalized) scale for young adults.  Suppose the standard 
# deviation is the same as for young adults.  What percent of this 
# older population has osteoporosis? 

#Theoretical Probability

# OOT - Older adults having osteoporosis deduced theoretically
# POAT - Percent of Older adults with osteoporosis theoreticaly 
# pnorm((-2.5-mean(stress))/sd(stress))
OOT = pnorm((-2.5-(-2))/1)
POAT = 100*OOT

# OOR - Older adults having osteoporosis deduced from randomly generated data

OA_random<-rnorm(1000,mean=-2,sd=1)

#Method 1

OOR1=sum(OA_random<(-2.5))

#or Method 2

OOR2=CountPercent(OA_random,-2.5)

POAR = 100*OOR1/length(OA_random)

# Output of Problem 2.b

cat(POAT,"% (theoretically) and",POAR,
    "% (randomly) \n of Older population has osteoporosis.")


# problem 3
# Calculate from theoretical probabilities obtained using R functions. 
# 3.a Value of the IQR for the standard Normal distribution .i.e. sd=1, mean=0

P25=qnorm(.25, mean =0, sd =1, lower.tail=TRUE)
P75=qnorm(.75, mean =0, sd =1, lower.tail=TRUE)
IQR <- P75 - P25
cat("The Interquartile range is ",IQR)

# 3.c  The percent of observations that are potential outliers 
# according to the 1.5 x IQR rule is the same for any Normal 
# distribution.  What is this percent? 

# For standard normal distribution

L=1.5*IQR
low_limit=P25-L
high_limit=P75+L
low_outliers = pnorm(low_limit,mean = 0, sd=1, lower.tail=TRUE)
high_outliers = pnorm(high_limit,mean = 0, sd=1, lower.tail=FALSE)
percent_outliers = 100*(low_outliers+high_outliers);
cat(percent_outliers,"% of observations are potential outliers");

# Written for any normal distribution
# -----------------------------END--------------------------------------------







#2.a 
# plot(seq(-4,4,0.0001),dnorm(seq(-4,4,0.0001), mean = 0, sd = 1),type="l", col=3, lwd=6,xlab="T-score", ylab ="Normal Distribution")
# abline(v=-2.5,col="red",lwd=3)
# pnorm(-2.5, mean=0, sd=1, lower.tail=TRUE)

#plot(seq(-4,4,len=1000),dnorm(seq(-4,4,len = 1000),mean=0,sd=1),type="l", col=6, lwd=6,xlab="T-score", ylab ="Normal Distribution")
#abline(v=-2.5,col="red",lwd=3)
#sa = pnorm(-2.5, mean=0, sd=1, lower.tail=TRUE)
#2.b
#Sampling
#plot(seq(-6,2,0.0001),dnorm(seq(-6,2,0.0001), mean = -2, sd = 1),type="l", col=3, lwd=3,xlab="T-score", ylab ="Normal Distribution")
#abline(v=-2.5,col="red",lwd=3)
#pnorm(-2.5, mean=0, sd=1, lower.tail=TRUE)

#plot(seq(-6,2,len=1000),dnorm(seq(-6,2,len = 1000),mean=-2,sd=1),type="l", col=3, lwd=3,xlab="T-score", ylab ="Normal Distribution")
#abline(v=-2.5,col="red",lwd=3)
#sb=pnorm(-2.5, mean=-2, sd=1, lower.tail=TRUE)
#tpb
#sb
