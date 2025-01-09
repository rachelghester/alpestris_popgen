dat <- read.csv(file.choose())

res_aov <- aov(Fis ~ Locus_ID, data = dat)
summary(res_aov)

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals)

# QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(res_aov$residuals[0:5000])