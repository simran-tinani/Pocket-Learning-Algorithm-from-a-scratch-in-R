
# First define the limits of the space to work in: (x1,y1) and (x2, y2)
x1<-(-2)
y1<-(-2)
x2<-2
y2<-2

# Define randomly selected (but fixed) points x and y between (x1,y1) and (x2,y2)
x <- runif(2, min = x1, max = x2)
y <- runif(2, min = y1, max = y2)

# Define the unique line passing through points x and y
fit <- (lm(y~x))

# Extract the intercept and slope of the line generated
t <- summary(fit)$coefficients[,1] 

t[1] # Intercept = c
t[2] # Slope = m

# Define the function f of variable z to evaluate the m*z + c. 
# The target classifies based on the function f.
f <- function(x){
  t[2]*x + t[1]
}

N<-1000 # Number of data points in the training set

A <- matrix(ncol=N, nrow=2) # A contains N random points of the space
b <- matrix(ncol=N, nrow=1) # b contains the labels of the points in A: i.e. which side of the line they lie on

for(i in 1:N){ # populating the matrices A and b with random entries in the defined subspace
  A[, i] <- c(runif(2, min = x1, max = x2)) 
  b[1, i] <- sign(A[2, i] - f(A[1, i])) # whether the point in A[,i] lies above or below the line
}

# Visualization
plot(A[1, ], A[2, ]) 
abline(fit,col="red") 
k=which(b==1)
points(A[1,k], A[2,k], col="blue")

# Initialization
w <- rep(0,3) # initial values in the weight vector
g <- function(z){ # Defining the function g, which will attempt to approximate f
  t(w) %*% z
}

# Pocket learning algorithm: Running the perceptron algorithm, but storing (and later using)
# the weight vector that gives the smallest training error 

# For N data points, we run the algorithm for 2*N iterations

w_pocket <- w # Pocket (best) weight vector
training_accuracy<-0 # at each iteration, this will be added to
pocket_accuracy<-0 # the best achievable accuracy
i_pocket<-0 # index at which pocket accuracy is achieved

i <- 1

while(i < 2*N+1){
  j = sample(1:N, 1)
  if((sign(g(c(1, A[, j]))) == b[1, j]) == 0){ 
    # update the weight vector at each misclassified point
    w <- w + b[1, j]*c(1, A[, j])
  }
  training_accuracy<-c(training_accuracy,0)
  for(k in 1:N){
    if(sign(g(c(1, A[,k ]))) == b[, k]){
      training_accuracy[length(training_accuracy)] <- training_accuracy[length(training_accuracy)] + 1/N
    }
  }
  if(tail(training_accuracy,1)>=max(head(training_accuracy,-1))){
    w_pocket<-w
    i_pocket<-i
    pocket_accuracy<- tail(training_accuracy,1)
  }
  i = i + 1
}

# Calculating the simple Perceptron model's classification accuracy on the training set
final_training_accuracy <- tail(training_accuracy,1) # training accuracy at the last iteration

# Preparing a larger sample data set to approximate test accuracy
S <- matrix(ncol=(N*100), nrow=2) # matrix with 10000 random data entries
for(v in 1:(N*100)){
  S[, v] <- c(runif(2, min = min(x1,y1)*10, max = max(x2,y2)*100)) 
  # entries are random numbers between lying in a much larger 2D subspace than the training domain
}

## Calculating the test accuracy 

# Simple Perceptron
test_accuracy <-0
v <- 1
while(v <= (ncol(S))){ 
  if(sign(g(c(1, S[, v]))) == sign(S[, v][2] - f(S[, v][1]))){
    test_accuracy <- test_accuracy + 1/ncol(S)
  }
  v <- v + 1
}

print(test_accuracy) # number of times g classified correctly on the test set


# Pocket Learning Algorithm 
pocket_test_accuracy <-0
v <- 1

g_pocket <- function(z){ # New function g_pocket multiples by w_pocket
  t(w_pocket) %*% z
}

while(v <= (ncol(S))){ 
  if(sign(g_pocket(c(1, S[, v]))) == sign(S[, v][2] - f(S[, v][1]))){
    pocket_test_accuracy <- pocket_test_accuracy + 1/ncol(S)
  }
  v <- v + 1
}

# Results
print(i_pocket) # The iteration at which training accuracy stopped increasing
print(final_training_accuracy*100) # Perceptron training accuracy
print(pocket_accuracy*100)  # Pocket training accuracy
print(test_accuracy*100) # Perceptron test accuracy
print(pocket_test_accuracy*100) # Pocket test accuracy






