library(ggplot2)
library(gridExtra)
library(car)
library(lmtest)
library(tseries)

# Loading the dataset into a DataFrame.
df <- read.table("../data/HW-diamonds.txt", quote="\"", comment.char="")
colnames(df) <- c("carat", "color", "clarity", "institution", "price")

observations = nrow(df)
variables = ncol(df)
sprintf("observations: %s and variables: %s", observations, variables)

# carat vs. log(carat).
p1 <- ggplot(df, aes(x=carat, y=price))+geom_point()+ ggtitle('carat vs. price') 
p2 <- ggplot(df, aes(x=carat, y=log(price)))+geom_point() + ggtitle('carat vs. log(price)')
grid.arrange(p1, p2, ncol=2)

# Releveling factors.
df$color=relevel(df$color, ref="I")
df$clarity=relevel(df$clarity, ref="VS2")
df$institution=relevel(df$institution, ref="HRD")

# Simple linear regression model.
model1 = lm(log(price)~carat+color+clarity+institution, data=df)
summary(model1)

# Residuals
par(mfrow=c(2,2))
plot(model1, which=c(1:4), ask=F)

# Rainbow test
raintest(model1)

# Independence of residuals.
dwtest(model1, alternative="two.sided")

# Constant variance.
bptest(model1)

# Normality
df <- dplyr::mutate(df, resid=residuals(model1), fv=fitted(model1), predwage=exp(fv)) 
jarque.bera.test(df$resid)

### REMEDIAL ACTIONS ###

# Clustering diamonds by carat

# Discretizing and releveling factors.
df$carat2 <- df$carat
df$carat2[df$carat < 0.5] <- "small"
df$carat2[df$carat >= 0.5 & df$carat < 1] <- "medium"
df$carat2[df$carat >= 1] <- "large"
df$carat2 <- as.factor(df$carat2)

df$carat2=relevel(df$carat2, ref="small")

# Linear model with clusters.
model2 = update(model1, ~.+carat2+carat:carat2)
summary(model2)

# Residuals
par(mfrow=c(2,2))
plot(model2, which=c(1:4), ask=F)

# Rainbow test
raintest(model2)

# Independence of residuals.
dwtest(model2, alternative="two.sided")

# Constant variance.
bptest(model2)

# Normality.
df <- dplyr::mutate(df, resid=residuals(model2), fv=fitted(model2), predwage=exp(fv)) 
jarque.bera.test(df$resid)

# Square of carat
# Linear model with centered square of carat.
model3 = update(model1, ~.+I(scale(carat, scale = F)^2))
summary(model3)

# Independence of residuals.
dwtest(model3, alternative="two.sided")

# Constant variance.
bptest(model3)

# Considering the model as a time series.
acf2(model3$residuals)

# Generalized least square model.
model4 = gls(log(price)~carat+color+clarity+institution+I(scale(carat, scale=FALSE)^2), data=df, correlation=corARMA(p=2), method="ML")
summary(model4)

AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)
summary(model4)