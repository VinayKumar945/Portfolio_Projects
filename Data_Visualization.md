---
title: "Data Visuaization"
output: html_document
---

**Assignment 1**

**Q1.**

```{r}
# Set the CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Then install the rvest package
install.packages("rvest")

## Data Visualization
## Objective: Identify data or model problems using visualization
## Anscombe (1973) Quartlet

data(anscombe)  # Load Anscombe's data
View(anscombe) # View the data
summary(anscombe)

## Simple version
plot(anscombe$x1,anscombe$y1)
summary(anscombe)

# Create four model objects
lm1 <- lm(y1 ~ x1, data=anscombe)
summary(lm1)
lm2 <- lm(y2 ~ x2, data=anscombe)
summary(lm2)
lm3 <- lm(y3 ~ x3, data=anscombe)
summary(lm3)
lm4 <- lm(y4 ~ x4, data=anscombe)
summary(lm4)
plot(anscombe$x1,anscombe$y1)
abline(coefficients(lm1))
plot(anscombe$x2,anscombe$y2)
abline(coefficients(lm2))
plot(anscombe$x3,anscombe$y3)
abline(coefficients(lm3))
plot(anscombe$x4,anscombe$y4)
abline(coefficients(lm4))


## Fancy version (per help file)

ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))

# Plot using for loop
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  ## or   ff[[2]] <- as.name(paste0("y", i))
  ##      ff[[3]] <- as.name(paste0("x", i))
  mods[[i]] <- lmi <- lm(ff, data = anscombe)
  print(anova(lmi))
}

sapply(mods, coef)  # Note the use of this function
lapply(mods, function(fm) coef(summary(fm)))

# Preparing for the plots
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))

# Plot charts using for loop
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
       xlim = c(3, 19), ylim = c(3, 13))
  abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)



```

**Q2.**

Generative art refers to artwork created through algorithms or systems, the artist defines some rules and then computer or other systems execute them to create unique pieces.The examples of the generative art are pioneer like Harold Cohen, who developed computer generated art through his program AARON, which could autonomously create drawings. Herbert W. Franke and others started exploring computer generated art using analog systems.

In recent years, artists like Dmitri Chernaik and Tyler Hobbs inflated generative art through the use of block chain and NFTs.Cherniak's series Ringers explores geometric designs, while Hobbs' Fidenza series is known for its organics flowing shapes.

**Q3.**

```{r}
## Data Visualization
## Objective: Create graphics with R
## Title: Fall color
# Credit: https://fronkonstin.com

# Install packages
# The gsubfn package is designed for advanced string manipulation and pattern matching using regular expressions.
# The proto package provides a framework for prototype-based object-oriented programming in R.
options(repos = c(CRAN = "https://cran.rstudio.com/"))
install.packages(c("gsubfn", "proto", "tidyverse"))

library(gsubfn)
library(tidyverse)

# Define elements in plant art
# Each image corresponds to a different axiom, rules, angle and depth

# Leaf of Fall

axiom="X"
rules=list("X"="F-[[X]+X]+F[+FX]-X", "F"="FF")
angle=22.5
depth=6


for (i in 1:depth) axiom=gsubfn(".", rules, axiom)

actions=str_extract_all(axiom, "\\d*\\+|\\d*\\-|F|L|R|\\[|\\]|\\|") %>% unlist

status=data.frame(x=numeric(0), y=numeric(0), alfa=numeric(0))
points=data.frame(x1 = 0, y1 = 0, x2 = NA, y2 = NA, alfa=90, depth=1)


# Generating data
# Note: may take a minute or two

for (action in actions)
{
  if (action=="F")
  {
    x=points[1, "x1"]+cos(points[1, "alfa"]*(pi/180))
    y=points[1, "y1"]+sin(points[1, "alfa"]*(pi/180))
    points[1,"x2"]=x
    points[1,"y2"]=y
    data.frame(x1 = x, y1 = y, x2 = NA, y2 = NA,
               alfa=points[1, "alfa"],
               depth=points[1,"depth"]) %>% rbind(points)->points
  }
  if (action %in% c("+", "-")){
    alfa=points[1, "alfa"]
    points[1, "alfa"]=eval(parse(text=paste0("alfa",action, angle)))
  }
  if(action=="["){
    data.frame(x=points[1, "x1"], y=points[1, "y1"], alfa=points[1, "alfa"]) %>%
      rbind(status) -> status
    points[1, "depth"]=points[1, "depth"]+1
  }

  if(action=="]"){
    depth=points[1, "depth"]
    points[-1,]->points
    data.frame(x1=status[1, "x"], y1=status[1, "y"], x2=NA, y2=NA,
               alfa=status[1, "alfa"],
               depth=depth-1) %>%
      rbind(points) -> points
    status[-1,]->status
  }
}

ggplot() +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
               lineend = "round",
               color="forestgreen", # Set your own Fall color?
               data=na.omit(points)) +
  coord_fixed(ratio = 1) +
  theme_void() # No grid nor axes


```

**Q4.**

This chart from the death rates versus birth rates for selected countries with contour lines drawn and a zero population reference line is a visually stimulating way to convey demographic patterns. The contour lines allow for grouping countries with similar dynamics of birth and death rates, and therefore help identify clusters among countries. The diagonal reference line gives one immediate visual evidence about zero population growth, which can complement the interpretation of population stability. Direct labeling of countries in the chart would add context without needing to resort to another legend. The view, however, has shortcomings. Annotations can become quite cluttered in some regions , which is densely populated, thus rendering it hard to read quickly. Sometimes shifting or log-transforming the axis could also clarify variations more strongly. Although the color palette is useful, it certainly fails to cater for color-blind audiences, and hence calls for some higher-contrast or color-blind-friendly alternatives. It also does not have a title or subtitle that fixes the understanding on first glance, and it could do better by a bolder approach on the zero population growth line. Finally, though contours express statutorily essential information, the chart does not state the statistical method by which such contours were drawn, which would render it so much more interpretable. Overall, though the chart contains informative and appealing data, it would render itself clearer and more accessible in that regard.

**Assignment 2**

Q1.

```{r}
### Paul Murrell's R examples (selected)

## Start plotting from basics 
# Note the order
plot(pressure, pch=22)  # Can you change pch?
# pch is used for different kind of symbols
text(150, 600, 
     "Pressure (mm Hg)\nversus\nTemperature (Celsius)")
View(pressure)
#  Examples of standard high-level plots 
#  In each case, extra output is also added using low-level 
#  plotting functions.
# 

# Setting the parameter (3 rows by 2 cols)
par(mfrow=c(3, 2))

# Scatterplot
# Note the incremental additions

x <- c(0.5, 2, 4, 8, 12, 16)
y1 <- c(1, 1.3, 1.9, 3.4, 3.9, 4.8)
y2 <- c(4, .8, .5, .45, .4, .3)

# Setting label orientation, margins c(bottom, left, top, right) & text size
par(las=1, mar=c(4, 4, 2, 4), cex=.7) 
plot.new()
plot.window(range(x), c(0, 6))
lines(x, y1)
lines(x, y2)
points(x, y1, pch=16, cex=1.5) # Try different cex value?  
points(x, y2, pch=21, bg="red", cex=2)  # Different background color
par(col="gray50", fg="gray50", col.axis="gray50")
# overall color(col), foreground(fg), color axis(col.axis)
axis(1, at=seq(0, 16, 4)) # What is the first number standing for? 
# The first number is for the axis, if the number is 1 that means x axis(below line),if it 2 then Y axis(left vertical line)
# the sequence(seq) is from 0 to 16 with a lag of 4
axis(2, at=seq(0, 6, 2))
axis(4, at=seq(0, 6, 2))
# U shaped box 
box(bty="u")
mtext("Travel Time (s)", side=1, line=2, cex=0.8)
mtext("Responses per Travel", side=2, line=2, las=0, cex=0.8)
mtext("Responses per Second", side=4, line=2, las=0, cex=0.8)
text(4, 5, "Bird 131")
par(mar=c(5.1, 4.1, 4.1, 2.1), col="black", fg="black", col.axis="black")

# Histogram
# Random data
Y <- rnorm(50)
# Make sure no Y exceed [-3.5, 3.5]
Y[Y < -3.5 | Y > 3.5] <- NA # Selection/set range
x <- seq(-3.5, 3.5, .1)
# the density of the normal distribution for a given set of values
dn <- dnorm(x)
par(mar=c(4.5, 4.1, 3.1, 0))
hist(Y, breaks=seq(-3.5, 3.5), ylim=c(0, 0.5), 
     col="gray80", freq=FALSE)
lines(x, dnorm(x), lwd=2)
# line width (lwd)
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Barplot
par(mar=c(2, 3.1, 2, 2.1)) 
midpts <- barplot(VADeaths, 
                  col=gray(0.1 + seq(1, 9, 2)/11), 
                  names=rep("", 4))
mtext(sub(" ", "\n", colnames(VADeaths)),
      at=midpts, side=1, line=0.5, cex=0.5)
text(rep(midpts, each=5), apply(VADeaths, 2, cumsum) - VADeaths/2,
     VADeaths, 
     col=rep(c("white", "black"), times=3:2), 
     cex=0.8)
par(mar=c(5.1, 4.1, 4.1, 2.1))  

# Boxplot
par(mar=c(3, 4.1, 2, 0))
boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset= supp == "VC", col="white",
        xlab="",
        ylab="tooth length", ylim=c(0,35))
mtext("Vitamin C dose (mg)", side=1, line=2.5, cex=0.8)
boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        
        subset= supp == "OJ")
legend(1.5, 9, c("Ascorbic acid", "Orange juice"), 
       fill = c("white", "gray"), 
       bty="n")
par(mar=c(5.1, 4.1, 4.1, 2.1))
View(ToothGrowth)
# Persp
x <- seq(-10, 10, length= 30)
y <- x
f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1
# 0.5 to include z axis label
par(mar=c(0, 0.5, 0, 0), lwd=0.5)
persp(x, y, z, theta = 30, phi = 30, 
      expand = 0.5)
par(mar=c(5.1, 4.1, 4.1, 2.1), lwd=1)

# Piechart
par(mar=c(0, 2, 1, 2), xpd=FALSE, cex=0.5)
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
                      "Apple", "Boston Cream", "Other", "Vanilla")
pie(pie.sales, col = gray(seq(0.3,1.0,length=6))) 

```

```{r}
df <- read.csv("/Users/vinay/Downloads/happy_dataset.csv")
View(df)

par(mfrow = c(3,2))
# Setting label orientation, margins c(bottom, left, top, right) & text size
par(las = 1 , mar=c(4,4,2,4) , cex=0.7)
plot.new()
plot.window(range(df$X), c(25,80))
lines(df$X,df$Life.Expectancy..years.)
lines(df$X,df$HPI)
points(df$X,df$Life.Expectancy..years., pch=16, cex=1.5)
points(df$X,df$HPI, pch=21,bg = "red", cex=1.5)
par(col="gray50", fg="gray50", col.axis="gray50")
axis(1,at = seq(2006,2021,1))
axis(2,at = seq(25,80,5))
axis(4,at = seq(25,80,5))
# U shaped box 
box(bty = "u")
mtext("Years",side = 1,line = 2, cex = 0.7)
mtext("Life Expectancy",col="black",side = 2,line = 2, las = 0,cex = 0.7)
mtext("HPI", col = "darkred",las = 0,side = 4,line = 2,cex = 0.7)
text(2008, 77,"HPI(2006-2021)", cex = 0.7)
par(mar= c(5.1,4.1,4.1,2.1), col = "black", fg = "black", col.axis = "black")

# Histogram
# Random data
Y <- rnorm(30)
# Make sure no Y exceed [-3.5, 3.5]
Y[Y < -4 | Y > 4] <- NA # Selection/set range
x <- seq(-4, 4, .1)
# the density of the normal distribution for a given set of values
dn <- dnorm(x)
par(mar=c(4.5, 4.1, 3.1, 0))
hist(Y, breaks=seq(-4, 4), ylim=c(0, 0.5), 
     col="gray80", freq=FALSE)
lines(x, dnorm(x), lwd=2)
# line width (lwd)
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Boxplot
par(mar = c(3,4.1,2,0))

install.packages("palmerpenguins")

# Load the package
library(palmerpenguins)

View(penguins)
boxplot(bill_length_mm ~ year,data = penguins, boxwex = 0.25, at = 1:3 - 0.2,
        subset = species == "Gentoo", col = "white",xlab = " " ,ylab = "penguine_length")
mtext("year",side = 1,line = 2,cex = 0.7)
boxplot(bill_length_mm ~ year,data = penguins, add= TRUE, boxwex = 0.25, at = 1:3 + 0.2,
        subset = species == "Chinstrap")
# Specify the position of the legend and the corresponding labels
legend("topright",cex=0.5, c("Gentoo", "Chinstrap"), fill = c("white", "grey"))
par(mar=c(5.1, 4.1, 4.1, 2.1))

# perps
x <- seq(-20, 20, length= 30)
y <- x
f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1
# 0.5 to include z axis label
par(mar=c(0, 0.5, 0, 0), lwd=0.5)
persp(x, y, z, theta = 30, phi = 30, 
      expand = 0.5)
par(mar=c(5.1, 4.1, 4.1, 2.1), lwd=1)

# pie
par(mar=c(0, 2, 1, 2), xpd=FALSE, cex=0.5)
pie.sales <- c(0.10, 0.40, 0.26, 0.13, 0.04, 0.07)
names(pie.sales) <- c("Blueberry", "Cherry",
                      "Apple", "Boston Cream", "Other", "Vanilla")
# This generates a sequence of 6 values between 0.3 and 1.0
pie(pie.sales, col = gray(seq(0.3,1.0,length=6))) 


```

**Assignment 3**

Q1.

```{r}
# Histogram

# Step 1: Generating random data
Y <- rnorm(50,0,1) # Generate 50 random numbers from a standard normal distribution (mean = 0, sd = 1)

# Step 2: Handling outliers
Y[Y < -3.5 | Y > 3.5] <- NA 

# Step 3: Setting x-values for the normal distribution curve
x <- seq(-3.5, 3.5, .1) 

# Step 4: Calculate the normal density for the x-values
dn <- dnorm(x) 

# Step 5: Create the histogram
hist(Y, 
     breaks=seq(-3.5, 3.5), # Breaks of the histogram are set to match the range of the data
     ylim=c(0, 0.5),        # Set the y-axis limits for the histogram to go from 0 to 0.5
     col="#00abff",           # Color the histogram bars light gray
     freq=FALSE)             # Display the histogram with densities (not frequencies)

# Step 6: Overlay the normal distribution curve
lines(x, dnorm(x), lwd=2)    # Draw a line for the normal density over the histogram, with a line width of 2

```

Q2. a)The descriptive statistics of all four regression models generated using Anscombe's quartet data point to the fact that the regression coefficients are the same as well as the p-values and the adjusted R-squared were values in the 1, 2, 3, and 4 linear models. Furthermore, four models, lm1, lm2, lm3, and lm4, are almost the same in this statistical matter, which implies nearly 63% of the variation in each dependent variable (y1, y2, y3, y4) is accounted for by the corresponding independent variables (x1, x2, x3, x4). This is a matter of the fact that such an analysis has been made without considering the different underlying distributions,which can be revealed only by visual means.

b)  

```{r}
## Data Visualization
## Objective: Identify data or model problems using visualization
## Anscombe (1973) Quartlet

data(anscombe)  # Load Anscombe's data
View(anscombe) # View the data
summary(anscombe)

## Simple version
plot(anscombe$x1,anscombe$y1)
summary(anscombe)

# Create four model objects
# Using x1 as the independent variable and y1 as dependent variable
lm1 <- lm(y1 ~ x1, data=anscombe)
# Coefficient of x1 is 0.5001 and intercept y1 is 3.0001. 
# The over all p-value(0.00217) is <= 0.05 that means the model is significant.
# The Adjusted R-squared:0.6295 that means the overall model explain around 62% of variation in the data.
summary(lm1)
# Using x2 as the independent variable and y2 as dependent variable
lm2 <- lm(y2 ~ x2, data=anscombe)
# Coefficient of x2 is 0.500 and intercept y2 is 3.001. 
# The over all p-value(0.002179) is <= 0.05 that means the model is significant.
# The Adjusted R-squared:0.6292 that means the overall model explain around 62.92% of variation in the data.
summary(lm2)
# Using x3 as the independent variable and y3 as dependent variable
lm3 <- lm(y3 ~ x3, data=anscombe)
# Coefficient of x3 is 0.4997 and intercept y3 is 3.0025. 
# The over all p-value(0.002176) is <= 0.05 that means the model is significant.
# The Adjusted R-squared:0.6292 that means the overall model explain around 62.92% of variation in the data.
summary(lm3)
# Using x4 as the independent variable and y4 as dependent variable
lm4 <- lm(y4 ~ x4, data=anscombe)
# Coefficient of x4 is 0.4999 and intercept y4 is 3.0017. 
# The over all p-value(0.002165) is <= 0.05 that means the model is significant.
# The Adjusted R-squared:0.6297 that means the overall model explain around 62.97% of variation in the data.
summary(lm4)

# Plot for x1 y1 relationship
plot(anscombe$x1,anscombe$y1,pch = 20, col = "blue")
abline(coefficients(lm1), col = "red",lwd = 2)
# Plot for x2 y2 relationship
plot(anscombe$x2,anscombe$y2,pch = 16, col = "darkgreen")
abline(coefficients(lm2),lwd = 1)
# Plot for x3 y3 relationship
plot(anscombe$x3,anscombe$y3,pch = 21, col = "purple")
abline(coefficients(lm3), col = "red",lwd = 3)
# Plot for x4 y4 relationship
plot(anscombe$x4,anscombe$y4,pch = 18, col = "coral4")
abline(coefficients(lm4),lwd = 2)



```

Q3.

```{r}
# using Women data 
View(women)
# Set the plotting parameters for the entire graphics device
par(family = "serif", bg = "lightgrey")  # Use a serif font and light grey background
# First dataset plot
plot(women$weight, women$height,
     main = "Women",
     xlab = "weight(lbs)",
     ylab = "height(in)",
     col = "steelblue",   # Custom color for points
     pch = 19,            # Filled circle
     cex = 1.5)           # Adjust size of points
abline(lm(height ~ weight, data = women), col = "tomato")  # Custom color for regression line
```

Q4.

```{r}
# Installing the tidyverse package if needed
install.packages("tidyverse")

# Loading the tidyverse package
library(tidyverse)

# Creating the scatterplot with a regression line
ggplot(data = trees, aes(x = Girth, y = Height)) +
  geom_point(color = "blue", size = 3, na.rm = TRUE) +      
  geom_smooth(method = "lm", color = "forestgreen", na.rm = TRUE) +  
  labs(title = " Girth vs Height with Regression Line", 
       x = "Girth (inc)", y = "Height (ft)") +
  theme_minimal()  

```

**Assignment 4**

Q1. a)

```{r}
# necessary packages
install.packages("ggplot2")
library(ggplot2)

# Sample data with variable width and height
data <- data.frame(
  category = c("A", "B", "C", "D"),
  values = c(15, 10, 20, 08),
  widths = c(0.5, 0.8, 0.5, 0.8) 
)

# Ploting the variable-width column chart
ggplot(data, aes(x = category, y = values)) +
  geom_col(aes(width = widths), fill = "skyblue", color = "black") +
  labs(title = "Variable Width Column Chart", x = "Category", y = "Values") +
  theme_minimal()

# 
```

b)  

```{r}
### Paul Murrell's R examples (selected)

par(mfrow = c(3,2))
x <- c(0.5, 2, 4, 8, 12, 16)
y1 <- c(1, 1.3, 1.9, 3.4, 3.9, 4.8)
y2 <- c(4, .8, .5, .45, .4, .3)

par(las = 1, mar = c(4,4,2,4), cex = 0.7)
plot.new()
plot.window( range(x),c(0,6))

lines(x,y1)
lines(x,y2)
points(x,y1,pch = 18, cex = 1,col = "red")
points(x,y2,pch = 20, cex = 1,col = "blue")
par(col = "gray50", fg = "gray50",col.axis = "gray50" )
axis(1,at = seq(0,16,4))
axis(2,at = seq(0,6,2))
axis(4,at = seq(0,6,2))
box(bty = "U")
mtext("Travel Time(s)",side = 1,cex=0.8,line = 2 )
mtext("Responses per Travel",side = 2,cex=0.8,line = 2, las = 0 )
mtext("Responses per Seconds",side = 4,cex=0.8,line = 2,las = 0)
par(mar = c(4,4.1,4.1,2),col = "black",fg = "black", col.axis = "black")
###################
Y = rnorm(50)
Y[Y < -3.5 | Y > 3.5 ] <- NA
x <- seq( -3.5, 3.5, 0.1)
dn <- dnorm(x)
par(mar = c(3.5, 4.1, 3.1, 0))
hist(Y, breaks = seq(-3.5,3.5), ylim = c(0, 0.5), col = "gray80", freq = FALSE)
lines(x,dnorm(x),lwd = 2)
par(mar = c(5.1, 4.1, 4.1, 2.1))

#####################
par(mar=c(2, 3.1, 2, 2.1))
midpts <- barplot(VADeaths, col = gray(0.1 + seq(1,9,2)/11), names = rep("", 4))
mtext(sub(" ", "\n", colnames(VADeaths)),
      at=midpts, side=1, line=0.5, cex=0.5)
text(rep(midpts, each=5), apply(VADeaths, 2, cumsum) - VADeaths/2,
     VADeaths, 
     col=rep(c("white", "black"), times=3:2), 
     cex=0.8)
par(mar=c(5.1, 4.1, 4.1, 2.1))  
#####################################
# Piechart
par(mar=c(0, 2, 1, 2), xpd=FALSE, cex=0.5)
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
                      "Apple", "Boston Cream", "Other", "Vanilla")
pie(pie.sales, col = gray(seq(0.3,1.0,length=6))) 

```

c)  

```{r}
install.packages("ggplot2")
library(ggplot2)
apple_data <- data.frame(Product = c("iPhone", "iPad", "MacBook", "AirPods"),
                         Price = c(30, 35, 40, 17))
ggplot(apple_data, aes(x = Price, y = Product)) +
  geom_bar(stat = "Identity", fill = "skyblue", width = 0.5) +
  labs(x = "Price", y = "Apple", title = "Price of Apple") +
  theme_minimal()
```

**Assignment 5**

Q1.

```{r}
install.packages("palmerpenguins")
library(palmerpenguins)
View(penguins)

############### Histogram ###############

par(family = "serif") # Set font family to serif
hist(penguins$bill_length_mm, col = "blue", xlab = "bill_length_mm", main = NULL)
title("Penguins Length", cex.main = 2) # Adjust cex.main for title size

################# Vertical Bar Chart #####################################

par(family = "serif") # Set font family to serif

# Aggregate body mass by species for bar plot
species_mass <- tapply(penguins$body_mass_g, penguins$species, mean, na.rm = TRUE)

barplot(species_mass, col = "darkgreen", xlab = "Species", ylab = "Average Body Mass (g)", main = NULL)

title("Penguins Body Mass by Species", cex.main = 2) # Custom title with adjusted size

##################### Horizontal Bar chart ################################

par(family = "Impact") # Set font family to serif

# Aggregate body mass by species for bar plot
species_depth <- tapply(penguins$bill_depth_mm, penguins$species, mean, na.rm = TRUE)

barplot(species_mass, col = "maroon", ylab = "Species", xlab = "Average Depth (mm)", main = NULL, horiz = TRUE)

title("Penguins depth by Species", cex.main = 1.5) # Custom title with adjusted size

###################### Pie Chart ###########################

# Sample data
fruits <- c("Apples", "Bananas", "Cherries", "Dates", "Orange")
consumption <- c(0.30,0.17, 0.23, 0.13, 0.17)

# Create a pie chart
pie(consumption, labels = fruits, main = "Fruit Consumption", col = rainbow(length(fruits)))

################ Box Plot ######################

# Sample data
set.seed(123)  # For reproducibility
scores <- data.frame(
  Student = rep(1:20, each = 5),
  Subject = rep(c("Math", "Science", "English", "History", "Art"), times = 20),
  Score = rnorm(100, mean = 75, sd = 10)  # Normally distributed scores
)

# Create a boxplot
boxplot(Score ~ Subject, data = scores,
        main = "Boxplot of Test Scores by Subject",
        xlab = "Subject",
        ylab = "Score",
        col = "lightblue")

################## Scatter Plot ##########################

par(family = "serif") 
scatter.smooth(x=penguins$body_mass_g, y=penguins$bill_length_mm,col = "red", xlab = "Body Mass", ylab = "Height", main = "Length Vs Body Mass")


```

Q2.

```{r}
################# GGPLOT ################################################

################# Histogram ##########################
install.packages("palmerpenguins")
install.packages("ggplot2")
library(ggplot2)
library(palmerpenguins)

  ggplot(penguins, aes(x = bill_length_mm)) +
  geom_histogram(color = "darkgreen", fill = "lightgreen", bins = 6) +
  labs(title = "Histogram of Bill Length (mm)",
       x = "Bill Length (mm)",
       y = "Count")+
  theme_minimal()

################# Bar Plot ##########################

ggplot(penguins, aes(x = species, y = body_mass_g)) + 
  geom_bar(color = "red", fill = "orange", stat = "Identity") +
  labs(title = "Barplot of Bill Length (mm)",
       x = "Body Mass (g)",
       y = "Count")+
  theme_minimal()

################# Horizontal Bar Plot ##########################

ggplot(penguins, aes(x = species, y = body_mass_g)) + 
  geom_bar(color = "blue", fill = "lightblue", stat = "Identity") +
  labs(title = "Barplot of Bill Length (mm)",
       x = "Body Mass (g)",
       y = "Count")+
  coord_flip()+
  theme_minimal()

################# Pie Chart ##################

fruits <- data.frame(
  type = c("Apple", "Banana", "Cherry", "Date"),
  count = c(30, 20, 25, 15)
)

# Create a pie chart
ggplot(fruits, aes(x = "", y = count, fill = type)) +
  geom_bar(width = 1, color = "white", stat = "identity") + 
  coord_polar(theta = "y") +
  labs(title = "Distribution of Fruits", fill = "Fruit Type") + 
  theme_void()

################### Box Plot ##########################

# Load necessary libraries
library(ggplot2)
library(palmerpenguins)

# Create a boxplot of body mass by species
ggplot(penguins, aes(x = species, y = body_mass_g)) + 
  geom_boxplot(fill = "lightblue", color = "darkblue") + 
  labs(title = "Boxplot of Body Mass by Species", 
       x = "Species", 
       y = "Body Mass (g)") + 
  theme_minimal()

################### Scatter Plot ##########################

# Create a scatterplot of bill length vs bill depth
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
  geom_point(size = 2.5, alpha = 0.7) + 
  labs(title = "Scatterplot of Bill Length vs Bill Depth", 
       x = "Bill Length (mm)", 
       y = "Bill Depth (mm)") + 
  theme_minimal() + 
  scale_color_brewer(palette = "Set1") 
```

Q3.

Here’s a concise summary of the file formats:

a)  PDF: Vector-based, high-quality, ideal for reports and presentations; retains scalability without quality loss.

b)  JPEG: Compressed raster format, small file size, suitable for web use but may lose quality with compression.

c)  SVG: Vector format, infinitely scalable, great for web graphics and interactivity; editable in vector software.

d)  TIFF: High-quality, lossless raster format used in professional printing and imaging; large file size.

e)  BMP: Uncompressed raster format, retains raw image data; rarely used due to its large size and lack of features.

---
title: "Assignment 7"
output: html_document
---

**Assignment 7**

a)  

```{r}
# Load necessary libraries
library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Scatter Plot: Faithful Dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Select X Variable:", 
                  choices = names(faithful), selected = "eruptions"),
      selectInput("yvar", "Select Y Variable:", 
                  choices = names(faithful), selected = "waiting"),
      sliderInput("pointSize", "Point Size:", min = 1, max = 5, value = 3),
      sliderInput("alpha", "Transparency:", min = 0.1, max = 1, value = 0.7)
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Define Server
server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    ggplot(faithful, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(size = input$pointSize, alpha = input$alpha, color = "blue") +
      labs(title = "Scatter Plot of Faithful Dataset",
           x = input$xvar,
           y = input$yvar) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)


```

b)  When I am uploading this on and it's running succesfully, but I am not able to see this on my website.

```{r}
# Loading necessary libraries
library(shiny)
library(ggplot2)

# Defining UI
ui <- fluidPage(
  titlePanel("Bubble Chart: mtcars Dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Select X Variable:", 
                  choices = names(mtcars), selected = "mpg"),
      selectInput("yvar", "Select Y Variable:", 
                  choices = names(mtcars), selected = "hp"),
      sliderInput("sizeRange", "Bubble Size Range:",
                  min = 1, max = 10, value = 5),
      sliderInput("alpha", "Transparency:", 
                  min = 0.1, max = 1, value = 0.7)
    ),
    mainPanel(
      plotOutput("bubbleChart")
    )
  )
)

# Defining Server
server <- function(input, output) {
  output$bubbleChart <- renderPlot({
    ggplot(mtcars, aes_string(x = input$xvar, y = input$yvar, 
                              size = "wt", color = "cyl")) +
      geom_point(alpha = input$alpha) +
      scale_size(range = c(1, input$sizeRange)) +
      labs(title = "Bubble Chart of mtcars Dataset",
           x = input$xvar, 
           y = input$yvar,
           size = "Weight (wt)",
           color = "Number of Cylinders") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}


# Runnig the application
shinyApp(ui = ui, server = server)

```
# Load necessary libraries
library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Scatter Plot: Faithful Dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Select X Variable:", 
                  choices = names(faithful), selected = "eruptions"),
      selectInput("yvar", "Select Y Variable:", 
                  choices = names(faithful), selected = "waiting"),
      sliderInput("pointSize", "Point Size:", min = 1, max = 5, value = 3),
      sliderInput("alpha", "Transparency:", min = 0.1, max = 1, value = 0.7)
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Define Server
server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    ggplot(faithful, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(size = input$pointSize, alpha = input$alpha, color = "blue") +
      labs(title = "Scatter Plot of Faithful Dataset",
           x = input$xvar,
           y = input$yvar) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)


##############################################

# Load necessary libraries
library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Bubble Chart: mtcars Dataset"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Select X Variable:", 
                  choices = names(mtcars), selected = "mpg"),
      selectInput("yvar", "Select Y Variable:", 
                  choices = names(mtcars), selected = "hp"),
      sliderInput("sizeRange", "Bubble Size Range:",
                  min = 1, max = 10, value = 5),
      sliderInput("alpha", "Transparency:", 
                  min = 0.1, max = 1, value = 0.7)
    ),
    mainPanel(
      plotOutput("bubbleChart")
    )
  )
)

# Define Server
server <- function(input, output) {
  output$bubbleChart <- renderPlot({
    ggplot(mtcars, aes_string(x = input$xvar, y = input$yvar, 
                              size = "wt", color = "cyl")) +
      geom_point(alpha = input$alpha) +
      scale_size(range = c(1, input$sizeRange)) +
      labs(title = "Bubble Chart of mtcars Dataset",
           x = input$xvar, 
           y = input$yvar,
           size = "Weight (wt)",
           color = "Number of Cylinders") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

# Run the application
shinyApp(ui = ui, server = server)


#####################


# Loading necessary libraries
library(shiny)
library(datasets)

# Pre-process the data using the iris dataset
irisData <- iris

# Define UI for the combined app
ui <- fluidPage(
  
  # App title
  titlePanel("Shiny App with Multiple Features"),
  
  # Tabbed layout
  tabsetPanel(
    
    # First Tab: Histogram
    tabPanel("Mtcars Histogram",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "bins",
                             label = "Number of bins:",
                             min = 1,
                             max = 15,
                             value = 10)
               ),
               mainPanel(
                 plotOutput(outputId = "distPlot")
               )
             )
    ),
    
    # Second Tab: Dataset Viewer
    tabPanel("Dataset Viewer",
             sidebarLayout(
               sidebarPanel(
                 textInput(inputId = "caption",
                           label = "Caption:",
                           value = "Data Summary"),
                 selectInput(inputId = "dataset",
                             label = "Choose a dataset:",
                             choices = c("iris")),
                 numericInput(inputId = "obs",
                              label = "Number of observations to view:",
                              min = 0,
                              value = 10)
               ),
               mainPanel(
                 h3(textOutput("caption", container = span)),
                 verbatimTextOutput("summary"),
                 tableOutput("view")
               )
             )
    ),
    
    # Third Tab: Iris Dataset Boxplot
    tabPanel("Iris Dataset Boxplot",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Variable:",
                             c("Sepal Length" = "Sepal.Length",
                               "Sepal Width" = "Sepal.Width",
                               "Petal Length" = "Petal.Length",
                               "Petal Width" = "Petal.Width",
                               "Species" = "Species")),
                 checkboxInput("outliers", "Show outliers", TRUE)
               ),
               mainPanel(
                 h3(textOutput("captionBoxplot")),
                 plotOutput("hpPlot")
               )
             )
    )
  )
)

# Define server logic to handle all three tabs
server <- function(input, output) {
  
  # Histogram - Mtcars Dataset
  output$distPlot <- renderPlot({
    x <- mtcars$mpg
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Miles Per Gallon (mpg)",
         main = "Histogram of MPG from mtcars")
  })
  
  # Dataset Viewer
  datasetInput <- reactive({
    iris
  })
  
  output$caption <- renderText({
    input$caption
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  # Boxplot for Iris Dataset
  formulaText <- reactive({
    paste("Petal.Length ~", input$variable)
  })
  
  output$captionBoxplot <- renderText({
    formulaText()
  })
  
  output$hpPlot <- renderPlot({
    boxplot(as.formula(formulaText()), 
            data = irisData,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
