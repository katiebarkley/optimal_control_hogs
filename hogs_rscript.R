

###################################################################################################################

############## CONSTANT ALPHA ###########################

# Set parameters
M      <- 0.1                   # price per pound of crop
Y      <- 12320                 # pounds of crop
d      <- 0.25                  # damage
delta  <- 0.1                   # discount rate
theta  <- 100                   # carrying capacity
r      <- 0.184                 # intrinsic growth rate
c1     <- 20                    # variable cost for harvest
c0     <- 0                     # fixed cost for harvest
b      <- 2                     # damage intensity
a      <- 1.1                   # intelligence
s      <- 900                   # fixed cost for farming
rho    <- (1/(1+delta))         # discount rate
time   <- seq(1,100)             # time horizon
N      <- rep(30,length(time))  # stock
H      <- rep(0, length(time))  # harvest
profit <- rep(0, length(time))  # profit

# Function to be optimized

totalprofit <- function (H) {
  for (i in 2:length(time)){
    if (N[i-1]<=0){
      N[i]<-0
      H[i]<-0
    }
    else{
      N[i]=N[i-1]+r*N[i-1]*(1-N[i-1]/theta)-H[i]                  # stock equation
      profit[i] = M*(Y-d*(N[i]-H[i])^b)-s-c0- (a*c1*(H[i]/N[i]))  # profit equation
    }
  }
  result <- sum(profit)
  return(result)
}

# Optimize system by choosing harvest

optimharv <- optimx(par = H, fn = totalprofit, lower = 0, upper = theta,
                    method = "L-BFGS-B",
                    control = list( maximize = TRUE ) )

# grab the optimal harvest
hoptim <- coef(optimharv)

# Simulate the system with the optimal harvest

for (i in 2:length(time)){
  N[i]=N[i-1]+r*N[i-1]*(1-N[i-1]/theta)-hoptim[i]                      # stock equation
  profit[i] = M*(Y-d*(N[i]-hoptim[i])^b)-s-c0- (a*c1*(hoptim[i]/N[i])) # profit equation
}

# plot population and harvest on same graph, then plot profit
layout(matrix(c(1, 2), nrow = 1))  #this makes it so graphs print out on the same screen. 
#allows to view all graphs at once

plot(c(1:length(time)), N[1:length(time)], type='l',ylab = "Hogs",xlab = "Year",ylim = c(0,30))
lines(c(2:length(time)), hoptim[2:length(time)], type='l',col="red")
legend("topright",legend=c(expression(paste("Population(",N[t],")")), expression(paste("Harvest (",K[t],")"))),
       col=c("black","red"),lty=c(1,1),lwd=c(2,2),
       bty="n", cex = 1.25)
plot (c(2:length(time)), profit[2:length(time)], type='l',ylab = "Profit",xlab = "Year")




###################### VARYING ALPHA #########################################

# Set parameters
M      <- 0.1                    # price per pound of crop
Y      <- 12320                  # pounds of crop
d      <- 0.25                   # damage
delta  <- 0.1                    # discount rate
theta  <- 100                    # carrying capacity
r      <- 0.184                  # intrinsic growth rate
c1     <- 25                     # variable cost for harvest
c0     <- 0                      # fixed cost for harvest
b      <- 2                      # damage intensity
s      <- 900                    # fixed cost for farming
rho    <- (1/(1+delta))          # discount rate
time   <- seq(1,50)              # time horizon
N      <- rep(30,length(time))   # stock
H      <- rep(0, length(time))   # harvest
profit <- rep(0, length(time))   # profit
a      <- rep(1, length(time)) # intelligence
ahigh  <- 2                      # intel. max
alow   <- 1                      # intel. min
deg    <- 0.1                    # memory loss
z      <- 0.4                    # learned memory
# Function to be optimized

totalprofit <- function (H) {
  for (i in 2:length(time)){
    if (N[i-1]<=0|a[i-1]<=0){
      N[i]<-0
      a[i]<-1
      H[i]<-0
    }
    else{
      N[i]=N[i-1]+r*N[i-1]*(1-N[i-1]/theta)-H[i]                                             # stock equation
      a[i]=(1-deg)*z*(H[i]/(N[i]+r*N[i]*(1-N[i]/theta)))*(ahigh-a[i])-deg*(a[i]-alow)  # intel. equation
      profit[i] = M*(Y-d*(N[i]-H[i])^b)-s-c0- (a[i]*c1*(H[i]/N[i]))                          # profit equation
    }
  }
  result <- sum(profit)
  return(result)
}

# Optimize system by choosing harvest

optimharv <- optimx(par = H, fn = totalprofit, lower = 0, upper = theta,
                    method = "L-BFGS-B",
                    control = list( maximize = TRUE ) )

# grab the optimal harvest
hoptim <- coef(optimharv)

# Simulate the system with the optimal harvest

for (i in 2:length(time)){
  if (N[i-1]<=0){
    N[i]<-0
    a[i]<-1
  }
  else{
    N[i]=N[i-1]+r*N[i-1]*(1-N[i-1]/theta)-hoptim[i]                                       # stock equation
    a[i]=(1-deg)*z*(hoptim[i]/(N[i]+r*N[i]*(1-N[i]/theta)))*(ahigh-a[i])-deg*(a[i]-alow)  # intel. equation
    profit[i] = M*(Y-d*(N[i]-hoptim[i])^b)-s-c0- (a[i]*c1*(hoptim[i]/N[i]))               # profit equation
  }
}


# plot population and harvest on same graph, then plot profit
layout(matrix(c(1,2,3,4), ncol = 2))  #this makes it so graphs print out on the same screen. 
#allows to view all graphs at once

plot(c(1:length(time)), N[1:length(time)], type='l',ylab = "Hogs",xlab = "Year")
# lines(c(2:length(time)), hoptim[2:length(time)], type='l',col="red")
# legend("topright",legend=c(expression(paste("Population(",N[t],")")), expression(paste("Harvest (",K[t],")"))),
#        col=c("black","red"),lty=c(1,1),lwd=c(2,2),
#        bty="n", cex = 1.25)
plot(c(1:length(time)), hoptim[1:length(time)], type='l',ylab = "Harvested Hogs",xlab = "Year")
plot(c(1:length(time)), a[1:length(time)], type='l',ylab = "Alpha",xlab = "Year",ylim = c(0,2))
plot (c(2:length(time)), profit[2:length(time)], type='l',ylab = "Profit",xlab = "Year")



###############################################################################################################



