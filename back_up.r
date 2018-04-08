library(VIM)
library("jsonlite")
library("ggplot2")
library(MCMCpack)
library(dplyr)
library("splitstackshape")
library("readr")

########################################### Input Data #######################################################
df_bus_TO <- stream_in(file("https://www.scss.tcd.ie/~arwhite/Teaching/CS7DS3/business_open_Toronto.json"))
df_bus_TO <- flatten(df_bus_TO)
df_bus_TO["modified_neigbour"] <- 0
df_bus_TO$neighborhood <- factor(df_bus_TO$neighborhood)

##########################Neighborhodd with unique values###############################################
str(df_bus_TO)
unique_neighbours <- unique(df_bus_TO$neighborhood)
for(i in 1:length(unique_neighbours)){
  for(j in 1:nrow(df_bus_TO)){
    if(length(df_bus_TO[j,]$neighborhood) != 0){
      if(unique_neighbours[i] == df_bus_TO[j,]$neighborhood){
        df_bus_TO[j,]$modified_neigbour = i
      }
    }
  }
}


########################################### Missing Values #######################################################
head(df_bus_TO)
oaggr <-aggr(df_bus_TO)
summary(oaggr)

########################################### Data Examination #######################################################

ggplot(df_bus_TO) + geom_boxplot(aes(x = reorder(modified_neigbour, stars, median), stars, fill = reorder(modified_neigbour, stars, median)), show.legend=FALSE)
ggplot(df_bus_TO, aes(x = reorder(modified_neigbour, modified_neigbour, length))) + stat_count()
hist(df_bus_TO$stars,col="darkred")
ggplot(df_bus_TO, aes(stars)) + stat_bin()

############################################# Gibbs Sampling ######################################################

compare_m_gibbs <- function(y, ind, maxiter = 5000)
{
  
  ### weakly informative priors
  a0 <- 1 ; b0 <- 1  ## tau_w hyperparameters  a0 <- 1/2 ; b0 <- 1   a0 <- 0 ; b0 <- 0  a0 <- 0.5 ; b0 <- 0.5 a0 <- 1 ; b0 <- 1
  eta0 <-1 ; t0 <- 1 ## tau_b hyperparameters eta0 <-1/2 ; t0 <- 1  eta0 <-0 ; t0 <- 0 eta0 <-0.5 ; t0 <- 0.5 eta0 <-1 ; t0 <- 1
  mu0<-4 ; gamma0 <- 1/25
  ###
  
  ### starting values
  m <- nlevels(ind)
  print("m")
  print(m)
  print("-------")
  ybar <- theta <- tapply(y, ind, mean)
  print("ybar")
  print(ybar)
  print("-------")
  a<-1 / tapply(y, ind, var)
  a[is.na(a)] <- 0
  tau_w <- mean(a) ##within group precision
  print("tau_w")
  print(tau_w)
  print("-------")
  theta[is.na(theta)] <- 0
  mu <- mean(theta)
  print("mu")
  print(mu)
  print("-------")
  tau_b <-var(theta) ##between group precision
  print("tau_b")
  print(tau_b)
  print("-------")
  n_m <- tapply(y, ind, length)
  print("n_m")
  print(n_m)
  print("-------")
  an <- a0 + sum((n_m[is.na(n_m)] <- 0))/2
  print("an")
  print(an)
  print("-------")
  ###
  
  ### setup MCMC
  theta_mat <- matrix(0, nrow=maxiter, ncol=m)
  mat_store <- matrix(0, nrow=maxiter, ncol=3)
  ###
  
  ### MCMC algorithm
  for(s in 1:maxiter) 
  {
    
    # sample new values of the thetas
    for(j in 1:m) 
    {
      taun <- n_m[j] * tau_w + tau_b
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
      theta[j]<-rnorm(1, thetan, taun)
    }
    
    #sample new value of tau_w
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[ind == j] - theta[j])^2)
    }
    bn <- b0 + ss/2
    tau_w <- rgamma(1, an, bn)
    
    #sample a new value of mu
    gammam <- m * tau_b + gamma0
    mum <- (mean(theta) * m * tau_b + mu0 * gamma0) / gammam
    mu <- rnorm(1, mum, 1/ sqrt(gammam)) 
    
    # sample a new value of tau_b
    etam <- eta0 + m/2
    tm <- t0 + sum((theta-mu)^2)/2
    tau_b <- rgamma(1, etam, tm)
    
    #store results
    theta_mat[s,] <- theta
    mat_store[s, ] <- c(mu, tau_w, tau_b)
  }
  colnames(mat_store) <- c("mu", "tau_w", "tau_b")
  return(list(params = mat_store, theta = theta_mat))
}

fit2 <- compare_m_gibbs(df_bus_TO$stars, df_bus_TO$neighborhood)
fit2.mcmc <- as.mcmc(as.data.frame(fit2)) ## makes output compatible with functions in MCMCpack
plot(fit2.mcmc)
raftery.diag(fit2.mcmc)
apply(fit2$params, 2, mean)
apply(fit2$params, 2, sd)
##############################
mean(1/sqrt(fit2$params[, 3]))
sd(1/sqrt(fit2$params[, 3]))
##############################
theta_hat <- apply(fit2$theta, 2, mean)
ggplot(data.frame(size = tapply(df_bus_TO$stars, df_bus_TO$neighborhood, length), theta_hat = theta_hat), aes(size, theta_hat)) + geom_point()

a <- data.frame(size = tapply(df_bus_TO$stars, df_bus_TO$neighborhood, length),neighborhood = levels(unique(df_bus_TO$neighborhood)))
a["theta_hat"]<-theta_hat
ggplot(a, aes(size, theta_hat, label = neighborhood))+geom_label() #geom_text(check_overlap = TRUE)

#Useful Reference nrow(filter(df_bus_TO, neighborhood == "Yonge and Eglinton"))

##############################################################################################################
############need to explain more plots when the priors are changed and conclusion
##############################################################################################################

#################################### Question 2 ############################################################

df_business_with_reviews<-  read.csv("/Users/sethuram/Desktop/ASM_ASSIGNMENT/code/merged_reviews_business.csv", header=TRUE)

pre_cleaned_features <-c("ratings_given_by_this_user","useful", "funny", "cool","review_count", "stars")

df_df_business_with_reviews_cleaned <- subset(df_business_with_reviews, select=pre_cleaned_features)


#extra..
cor(df_merged_cleaned)
library(corrplot)
cex.before <- par("cex")

corrplot(cor(df_merged_cleaned[-6]),method = "number",
         tl.col = "black",type = "lower",tl.cex = 1.5,
         cl.cex = 1,tl.srt=45)



lm1 <- lm(stars~., df_df_business_with_reviews_cleaned)
summary(lm1)

##Feature Selection #########

step_AIC <- step(lm1)
step_BIC <- step(lm1, k=log(nrow(df_df_business_with_reviews_cleaned))) 

print(step_AIC)
print(step_BIC)

#--------Adding Interaction-----#

step_AIC2 <- step(lm(stars ~ (ratings_given_by_this_user + useful + funny + cool + review_count )^2, data = df_df_business_with_reviews_cleaned)) ## interaction terms 
step_BIC2 <- step(lm(stars ~ (ratings_given_by_this_user + useful + funny + cool + review_count )^2, data = df_df_business_with_reviews_cleaned), k = log(nrow(df_df_business_with_reviews_cleaned)))## interaction terms
print(step_AIC2)
print(step_BIC2)


# ----Adding Quadratic effect along with interaction-------#
step_AIC3 <- step(lm(stars ~ratings_given_by_this_user + useful + funny + cool + review_count + I(useful^2) + I(ratings_given_by_this_user^2) + useful:ratings_given_by_this_user, data=df_df_business_with_reviews_cleaned)) ## quadratic effects
step_BIC3 <- step(lm(stars ~ratings_given_by_this_user + useful + funny + cool + review_count + I(useful^2) + I(ratings_given_by_this_user^2) + useful:ratings_given_by_this_user, data=df_df_business_with_reviews_cleaned), k = log(nrow(df_df_business_with_reviews_cleaned)))

print(step_AIC3)
print(step_BIC3)

##... Model ----------

fit_stars <- MCMCregress(stars~. , data=df_df_business_with_reviews_cleaned, mcmc=1000) #subset of 1000

summary(fit_stars)
plot(fit_stars)
beta_mean <- apply(fit_stars, 2, mean)
beta_mean <- beta_mean[-length(beta_mean)] # getting rid of stars

df_dummy <- subset(df_merged_cleaned, select=merged_features[-6]) # remove response variable
df_dummy <- cbind(1, as.matrix(df_dummy)) # add dummy variable for intercept, convert to matrix class
pred_fit <- df_dummy %*% as.matrix(beta_mean) # make prediction, ignore variance parameter
plot(pred_fit, df_merged_cleaned$stars)

RMSE(pred_fit,df_merged_cleaned$stars)

#################################### Question 3 ############################################################

library(BayesLCA)

df_bus_TO <- stream_in(file("https://www.scss.tcd.ie/~arwhite/Teaching/CS7DS3/business_open_Toronto.json"))
df_bus_TO <- flatten(df_bus_TO)
df_test<- df_bus_TO;
test <- subset(df_test,select = c("neighborhood","categories"))


enocode_neighbour <- as.numeric(as.factor(test$neighborhood))
#enocode_categories <- as.numeric(as.factor(test$categories))
df_bus_TO_encoded = cbind(enocode_neighbour,splitstackshape:::charMat(test$categories, fill = 0))

#df_bus_TO_encoded = cbind(test,splitstackshape:::charMat(test$categories, fill = 0),splitstackshape:::charMat(test$neighborhood, fill = 0))
df_bus_TO_encoded$categories <- NULL
df_bus_TO_encoded$neighborhood <- NULL 
fit_lca72 <- blca.em(df_bus_TO_encoded, 72,restarts = 30)
fit_lca5 <- blca.em(df_bus_TO_encoded, 5)
plot(fit_lca, which = 1)
table(MAP(Zscore(df_bus_TO_encoded[,-1], fit_lca5)), MAP(Zscore(df_bus_TO_encoded[,-1], fit_lca72)))

ncol(df_bus_TO_encoded)



