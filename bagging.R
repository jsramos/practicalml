library(dplyr)
ozone <- read.csv('./ozone.data.txt', sep=' ', header = T)
ozone <- ozone %>% arrange(ozone)

# Manual bagging in R
ll <- matrix(NA, nrow=100, ncol=155) # a matrix of 10 iterations and 155 predictions
for (i in 1:100) {
        subozone <- ozone %>% slice(sample(n(), replace = T)) %>% arrange(ozone)
        loessOzone <- loess(temperature ~ ozone, subozone, span=.5) # span is a measure of the smoothness of the curve
        ll[i,] <- predict(loessOzone, newdata=data.frame(ozone=1:155))
}
plot(ozone$ozone, ozone$temperature, pch=19)
for (i in 1:100) {
        lines(1:155, ll[i,], col='grey')
}
lines(1:155, apply(ll,2,mean), col='red')

# With CARET
treebag <- bag(data.frame(ozone=ozone$ozone),
               ozone$temperature,
               B=100,
               bagControl = bagControl(fit=ctreeBag$fit, # fit a model
                                        predict=ctreeBag$pred, # predict response
                                        aggregate=ctreeBag$aggregate # aggregate all predictions
                                        )
               )
# original plot
plot(ozone$ozone, ozone$temperature, pch=19, col='lightgray')
# observe that if we use ONLY 1 FIT, the error is considerable and the fit is poor
points(ozone$ozone,
       predict(treebag$fits[[1]]$fit, 
               data.frame(ozone=ozone$ozone)), 
       pch=19, col='red') 
# but when we use ALL FITS, the error is reduced, the bias remains, and the fit is better
points(ozone$ozone, 
       predict(treebag, 
               data.frame(ozone=ozone$ozone)), 
       pch=19, col='blue') 