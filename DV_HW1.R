summary_table <- data.frame(
  dataset = c("1", "2", "3", "4"),
  mean_x = sapply(anscombe[, 1:4], mean),
  mean_y = round(sapply(anscombe[, 5:8], mean), 2),
  sd_y = round(sapply(anscombe[, 5:8], sd), 2),
  cor_xy = round(c(cor(anscombe$x1, anscombe$y1), cor(anscombe$x2, anscombe$y2),
                   cor(anscombe$x3, anscombe$y3), cor(anscombe$x4, anscombe$y4)), 2)
)
rownames(summary_table) <- NULL
summary_table
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
for (i in 1:4) {
  x <- anscombe[[paste0("x", i)]]
  y <- anscombe[[paste0("y", i)]]
  plot(x, y, pch = 19, col = "darkred",
       xlim = c(3, 20), ylim = c(2, 14),
       main = paste("Dataset", i), xlab = "x", ylab = "y")
  abline(lm(y ~ x), col = "grey40")
}
d <- data.frame(
  quarter = 1:6,
  rate = c(5.8, 5.9, 5.9, 6.0, 6.1, 6.1)
)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(d$quarter, d$rate, type = "b", pch = 19, col = "darkred",
     ylim = c(5.75, 6.15), # axis starts near the data
     main = "Crisis!", xlab = "Quarter", ylab = "Unemployment rate (%)")
plot(d$quarter, d$rate, type = "b", pch = 19, col = "darkred",
     ylim = c(0, 10), 
     main = "The same six numbers", xlab = "Quarter", ylab = "Unemployment rate (%)")
values <- c("5.8", "5.9", "6.0", "not reported", "6.1")
numbers <- as.numeric(values)
numbers     
mean(numbers, na.rm = TRUE)   

library(ggplot2)
ggplot(data = mtcars, aes(x = wt, y = mpg))
ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point()
ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
  geom_point(size = 2) +
  labs(x = "Weight (1000 lbs)", y = "Miles per gallon",
       colour = "Cylinders",
       title = "Heavier cars use more fuel")
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(colour = "grey40") +
  geom_smooth(method = "lm", se = TRUE, colour = "darkred") +
  labs(x = "Weight (1000 lbs)", y = "Miles per gallon")
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar() +
  labs(x = "Cylinders", y = "Number of cars",
       title = "geom_bar counts rows for you")
avg <- aggregate(mpg ~ cyl, data = mtcars, FUN = mean)
avg
ggplot(avg, aes(x = factor(cyl), y = mpg)) +
  geom_col(fill = "steelblue") +
  labs(x = "Cylinders", y = "Mean miles per gallon",
       title = "geom_col draws the values you supply")
p1 <- ggplot(mtcars, aes(x = mpg)) + geom_histogram(bins = 5) + labs(title = "5 bins")
p2 <- ggplot(mtcars, aes(x = mpg)) + geom_histogram(bins = 30) + labs(title = "30 bins")
print(p1)
print(p2)
ggplot(d, aes(x = quarter, y = rate)) +
  geom_line(colour = "grey50") +
  geom_point(colour = "darkred", size = 2) +
  scale_y_continuous(limits = c(0, 10)) +
  labs(x = "Quarter", y = "Unemployment rate (%)",
       title = "Unemployment rose 0.3 points over six quarters",
       caption = "Axis from 0 to 10. Source: illustrative.")



#Homework
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(cyl), shape = factor(cyl))) +
  geom_point(size = 3) +
  labs(
    title = "Heavier cars in this sample tend to have lower fuel economy",
    x = "Weight (1,000 lb)",
    y = "Fuel economy (miles per gallon)",
    colour = "Cylinders",
    shape = "Cylinders"
  )
#The points suggest that heavier cars tend to have lower fuel economy. This figure could make the relationship look more certain or general than it is because mtcars contains only 32 cars; I would add a note stating the sample size and that the data are observational