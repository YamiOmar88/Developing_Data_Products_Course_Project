# How to plot for the BMI

# BMI = weight/height^2
# weight = BMI * height^2

ht <- seq(from = 1.2, to = 2.0, by = 0.1)

wt_18.5 <- 18.5 * ht^2
wt_25 <- 25 * ht^2
wt_30 <- 30 * ht^2

ht <- c(ht, ht, ht)
wt <- c(wt_18.5, wt_25, wt_30)
cat <- c(rep("underweight", 9), rep("healthy weight", 9), rep("overweight", 9))

df <- data.frame(ht = ht, wt = wt, Category = cat)
remove(ht, wt_18.5, wt_25, wt_30, wt, cat)

library(ggplot2)
g <- ggplot(data = df, aes(x = ht, y = wt, fill = Category))
g <- g + geom_ribbon(aes(ymin = 25, ymax = wt, fill = Category), alpha = 0.2)
g <- g + geom_line(aes(color = Category))
g <- g + ylab("Weight (kg)") + xlab("Height (m)")
g <- g + geom_text(aes(x = 1.9, y = 120, label = "Obese")) +
         geom_text(aes(x = 1.9, y = 100, label = "Overweight")) +
         geom_text(aes(x = 1.9, y = 80, label = "Healthy Weight")) +
         geom_text(aes(x = 1.9, y = 50, label = "Underweight"))
g <- g + theme(legend.position = "none")
g