#### Visual comparison of whether sampling distribution is close to Normal via Bootstrap# a function to compare the bootstrap sampling distribution with
# a normal distribution with mean and SEM estimated from the data
bs.one.samp.dist <- function(dat,N= 1e4) {
  n <- length(dat) ;
  # resample from data
  sam <- matrix(sample(dat,size=N* n,replace=TRUE),ncol=N);
  # draw a histogram of the means
  sam.mean <- colMeans(sam) ;
  #save par() settings
  old.par <-par(no.readonly = TRUE)
  #make smaller margins
  par(mfrow=c(2,1),mar=c(3,2,2,1),oma=c(1,1,1,1))
  # Histogram overlaid with kernel density curve
  hist(dat,freq = FALSE,breaks=6
       ,main = "Plot of data withsmootheddensitychurve")
  points (density(dat),type="1")
  rug(dat)
  hist(sam.mean,freq = FALSE,breaks=25
       ,main = "Bootstrap sampling distribution of the mean"
       ,xlab = paste("Data:n=",n
                     ,"mean=",signif (mean(dat),digits-5)
                     ,",se=",signif (sa(dat)/sqrt(n)),digits=5))
  # overlay a density curve for the sample means
  points (density (sam.mean) ,type="I")
  # overlay a normal dis tmibutaom,bordand
  x<- seq(min(sam.mean),max(sam.mean),length=1000)
  points(x,dnorm(x,mean=mean(dat),sdfsd (dat)/sqrt(n))
         ,type = "1",lwd=2,col="red")
  # place a rug of points under the plot
  rug(sam.mean)
# restore par() settings
par(old.par)
}
# example data,skewed-----try others datasets to develop your intuit2on
x<-rgamma(10,shape=.5,scale= 20)
#install.packages("bs.one.samp.dist")
bs.one.samp.dist(x)