loadings <- matrix(c (
  .33, .58, .00, .00,  # Ease of Making Reservation 
  .35, .55, .00, .00,  # Availability of Preferred Seats
  .30, .52, .00, .00,  # Variety of Flight Options
  .40, .50, .00, .00,  # Ticket Prices
  .50, .00, .55, .00,  # Seat Comfort
  .41, .00, .51, .00,  # Roominess of Seat Area
  .45, .00, .57, .00,  # Availability of Overhead Storage
  .32, .00, .54, .00,  # Cleanliness of Aircraft
  .35, .00, .00, .50,  # Courtesy
  .38, .00, .00, .57,  # Friendliness
  .60, .00, .00, .50,  # Helpfulness
  .52, .00, .00, .58,  # Service
  .43, .10, .20, .30,  # Overall Satisfaction
  .35, .50, .40, .20,  # Purchase Intention
  .25, .50, .50, .00), # Willingness to Recommend
  nrow=15,ncol=4, byrow=TRUE)

# Matrix multiplication produces the correlation matrix, 
# except for the diagonal.
cor_matrix<-loadings %*% t(loadings)
# Diagonal set to ones.
diag(cor_matrix)<-1

library(mvtnorm)
N=1000
set.seed(7654321) #needed in order to reproduce the same data each time
std_ratings<-as.data.frame(rmvnorm(N, sigma=cor_matrix))

# Creates a mixture of two data sets:
# first 50 observations assinged uniformly lower scores.
ratings<-data.frame(matrix(rep(0,15000),nrow=1000))
ratings[1:50,]<-std_ratings[1:50,]*2
ratings[51:1000,]<-std_ratings[51:1000,]*2+7.0

# Ratings given different means
ratings[1]<-ratings[1]+2.2
ratings[2]<-ratings[2]+0.6
ratings[3]<-ratings[3]+0.3
ratings[4]<-ratings[4]+0.0
ratings[5]<-ratings[5]+1.5
ratings[6]<-ratings[6]+1.0
ratings[7]<-ratings[7]+0.5
ratings[8]<-ratings[8]+1.5
ratings[9]<-ratings[9]+2.4
ratings[10]<-ratings[10]+2.2
ratings[11]<-ratings[11]+2.1
ratings[12]<-ratings[12]+2.0
ratings[13]<-ratings[13]+1.5
ratings[14]<-ratings[14]+1.0
ratings[15]<-ratings[15]+0.5

# Truncates Scale to be between 1 and 9
ratings[ratings>9]<-9
ratings[ratings<1]<-1
# Rounds to single digit.
ratings<-round(ratings,0)

# Assigns names to the variables in the data frame called ratings
names(ratings)=c(
  "Easy_Reservation",
  "Preferred_Seats",
  "Flight_Options",
  "Ticket_Prices",
  "Seat_Comfort",
  "Seat_Roominess",
  "Overhead_Storage",
  "Clean_Aircraft",
  "Courtesy",
  "Friendliness",
  "Helpfulness",
  "Service",
  "Satisfaction",
  "Fly_Again",
  "Recommend")

# Calls qgraph package to run Network Map
library(qgraph)
# creates grouping of variables to be assigned different colors.
gr<-list(1:4,5:8,9:12,13:15) 
qgraph(cor(ratings),layout="spring", groups=gr, labels=names(ratings), label.scale=FALSE, minimum=0.50)

# Calculates z-scores so that regression analysis will yield 
# standardized regression weights
scaled_ratings<-data.frame(scale(ratings))
ols.sat<-lm(Satisfaction~Easy_Reservation + Preferred_Seats + 
              Flight_Options + Ticket_Prices + Seat_Comfort + Seat_Roominess + 
              Overhead_Storage + Clean_Aircraft + Courtesy + Friendliness + 
              Helpfulness + Service, data=scaled_ratings)
summary(ols.sat)

ols.rec<-lm(Recommend ~ Easy_Reservation + Preferred_Seats + 
              Flight_Options + Ticket_Prices + Seat_Comfort + Seat_Roominess + 
              Overhead_Storage + Clean_Aircraft + Courtesy + Friendliness + 
              Helpfulness + Service, data=scaled_ratings)
summary(ols.rec)

ols.fly<-lm(Fly_Again ~ Easy_Reservation + Preferred_Seats + 
              Flight_Options + Ticket_Prices + Seat_Comfort + Seat_Roominess + 
              Overhead_Storage + Clean_Aircraft + Courtesy + Friendliness + 
              Helpfulness + Service, data=scaled_ratings)
summary(ols.fly)