library(ggplot2)
library(ggplot2)
library(tidyverse)
library(broom)
###########################Introduction###########################################################################
set.seed(1806)
#A function f(x)
f <- function(x){
  f_x <- sin(2*x) + x -.1*x^2 + 2 + rnorm(length(x), sd = .2)
  return(f_x)
}

# For 500 equally spaced values of x between 0 & 1, let's compute and plot f(x) in red.
# Recall that f(x) is the systematic component, or "the signal"
values <- data_frame(
  x = seq(from=0, to=6, length = 500),
  f_x = f(x)
)

# Create the data
values <- tibble(
  x = seq(from=0, to=6, length = 500),
  f_x = f(x)
) %>%
  mutate(
    epsilon = rnorm(500, 0, sd = 2),
    y = f_x + epsilon
  )

# Get the y-axis limits that will work for both plots
y_min <- min(c(values$f_x, values$y))
y_max <- max(c(values$f_x, values$y))

# Create plot 1: Just f(x)
p1 <- values %>%
  ggplot(aes(x=x)) +
  stat_function(fun = f, colour = "red") +
  ylim(y_min, y_max) +  # Set consistent y limits
  theme_bw() +
  labs(title = "Systematic component (Signal only)")

# Create plot 2: f(x) with points
p2 <- values %>%
  ggplot(aes(x=x)) +
  stat_function(fun = f, colour = "red") +
  geom_point(aes(y=y)) +
  ylim(y_min, y_max) +  # Set consistent y limits
  theme_bw() +
  labs(title = "Systematic and the Unsystematic Component (Noise)")

# Display plots side by side
library(gridExtra)
grid.arrange(p1, p2, ncol=2)

p3 <- values %>%
  ggplot(aes(x=x)) +
  geom_point(aes(y=y)) +
  ylim(y_min, y_max) +  # Set consistent y limits
  theme_bw() +
  labs(title = "Real Life Data Visualization")
p3
#You only have the above in real life, then it's up to you as a data analyst or data scientist to estimate and fit the red curve,
#i.e. your task is to seprate the signal from the noise

#################################################################################################################################

#Link to data (https://www.kaggle.com/datasets/emilianito/saheart)
Heart <- read.csv("SA_heart.csv", header = TRUE)


#simple scatter
obe.age.plot <- ggplot(Heart, aes(x=age, y=obesity)) +
  geom_point(alpha=0.55, color="black") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))
obe.age.plot

# Create the plot
obe.age.plot2 <- ggplot(Heart, aes(x = age, y = obesity)) +
  geom_point(alpha = 0.55, color = "black") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add linear fit line
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))  # Add thick border

# Display the plot
obe.age.plot2

# Create the plot with a second-degree polynomial fit
obe.age.plot3 <- ggplot(Heart, aes(x = age, y = obesity)) +
  geom_point(alpha = 0.55, color = "black") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue", se = FALSE) +  # Add second-degree polynomial fit line
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))  # Add thick border

# Display the plot
obe.age.plot3

# Create the plot with a second-degree polynomial fit
obe.age.plotnew <- ggplot(Heart, aes(x = age, y = obesity)) +
  geom_point(alpha = 0.55, color = "black") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "blue", se = FALSE) +  # Add second-degree polynomial fit line
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))  # Add thick border

# Display the plot
obe.age.plotnew

###Piecewise regression

pred1 <- predict(lm(obesity~age+ I(age^2),
                    data = Heart[Heart$age<25,]))
pred2 <- predict(lm(obesity~age+ I(age^2),
                    data = Heart[Heart$age >=25 & Heart$age<35,]))
pred3 <- predict(lm(obesity~age+ I(age^2),
                    data = Heart[Heart$age>=35 & Heart$age<45,]))
pred4 <- predict(lm(obesity~age+ I(age^2),
                    data = Heart[Heart$age>=45 & Heart$age<55,]))
pred5 <- predict(lm(obesity~age+ I(age^2),
                    data = Heart[Heart$age>=55,]))


obe.age.plot +
  geom_line(data=Heart[Heart$age<25,],
            aes(y = pred1, x=age), size = 1, col="blue") +
  geom_line(data=Heart[Heart$age >=25 & Heart$age<35,],
            aes(y = pred2, x=age), size = 1, col="blue") +
  geom_line(data=Heart[Heart$age>=35 & Heart$age<45,],
            aes(y = pred3, x=age), size = 1, col="blue") +
  geom_line(data=Heart[Heart$age>=45 & Heart$age<55,],
            aes(y = pred4, x=age), size = 1, col="blue") +
  geom_line(data=Heart[Heart$age>=55,,],
            aes(y = pred5, x=age), size = 1, col="blue")


pred6 <- predict(lm(obesity ~ age + I(age^2) +
                      I((age - 25) * (age >= 25)) + I((age - 25)^2 * (age >= 25)) +
                      I((age - 35) * (age >= 35)) + I((age - 35)^2 * (age >= 35)) +
                      I((age - 45) * (age >= 45)) + I((age - 45)^2 * (age >= 45)) +
                      I((age - 55) * (age >= 55)) + I((age - 55)^2 * (age >= 55)),
                    data = Heart))

obe.age.plot +
  geom_line(data=Heart,
            aes(y = pred6, x=age), size = 1, col="blue")

pred7 <- predict(lm(obesity ~ age + I(age^2) +
                       I((age - 25)^2 * (age >= 25)) +
                       I((age - 35)^2 * (age >= 35)) +
                       I((age - 45)^2 * (age >= 45)) +
                       I((age - 55)^2 * (age >= 55)),
                     data = Heart))
obe.age.plot +
  geom_line(data=Heart,
            aes(y = pred7, x=age), size = 1, col="red")


#Cubic Spline

pred8 <- predict(lm(obesity ~ age + I(age^2) + I(age^3) +
                      I((age - 25)^3 * (age >= 25)) +
                      I((age - 35)^3 * (age >= 35)) +
                      I((age - 45)^3 * (age >= 45)) +
                      I((age - 55)^3 * (age >= 55)),
                    data = Heart))
obe.age.plot +
  geom_line(data=Heart,
            aes(y = pred8, x=age), size = 1, col="red")

#B-splines
library(splines)
pred9 <- predict(lm(obesity ~ bs(age, knots = c(25,35,45,55)), data = Heart ))

obe.age.plot +
  geom_line(data=Heart,
            aes(y = pred9, x=age), size = 1, col="red")

#simple scatter
obe.age.plot <- ggplot(Heart, aes(x=age, y=obesity)) +
  geom_point(alpha=0.55, color="black") +
  theme_minimal()

tri.age.plot +
  stat_smooth(method = "lm",
              formula = y~bs(x,knots = c(25, 35, 45, 55)),
              lty = 1, col = "blue") +
  stat_smooth(method = "lm",
              formula = y~ns(x,knots = c(25, 35, 45, 55)),
              lty = 1, col = "red")


#simple scatter
obe.age.plot <- ggplot(Heart, aes(x = age, y = obesity)) +
  geom_point(alpha = 0.55, color = "black") +
  theme_minimal()

obe.age.plot +
  stat_smooth(method = "lm",
              formula = y ~ bs(x, knots = c(25, 35, 45, 55)),
              aes(color = "Cubic-splines"),  # Add color to aes() for legend
              lty = 1) +
  stat_smooth(method = "lm",
              formula = y ~ ns(x, knots = c(25, 35, 45, 55)),
              aes(color = "Natural splines"),  # Add color to aes() for legend
              lty = 1)  +
  scale_color_manual(values = c("Cubic-splines" = "blue", "Natural splines" = "red")) +  # Map colors
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    panel.border = element_rect(color = "black", fill = NA, size = 2)  # Add thick border
  )























