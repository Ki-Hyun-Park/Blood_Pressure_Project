---
title: "Write-up"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library("tidyverse")
library("pwr")
library("sjstats")
library("phia")
```

# 1. Sampling 

## Power Tests

Given our power analysis, a group size of 12 was necessary for each combination of treatments.

```{r power}
pwr.anova.test(k = 4, f = 0.5, power = .8)
```

## Random Number Generation

For this one, we're just generating a lot of pairs of numbers. Each pair will represent a city and household to get data from.

``` {r sampling}
cities <- data.frame("City" = c("Vardo", "Hofn", "Helvig", "Bjurholm", "Blunduos", 
                                "Helluland", "Harano","Reading", "Akkeshi", 
                                "Nelson", "Birowa", "Shinobi", "Arcadia", 
                                "Kiyobico", "Takazaki", "Nidoma", "Talu", "Pauma", 
                                "Riroua", "Valai", "Gordes", "Maeva", "Kinsale", 
                                "Colmar", "Vaiku", "Mahuti", "Eden"),
                     "Households" = c(1035, 1255, 836, 695, 797, 650, 753, 1182, 700, 
                                      468, 805, 557, 1353, 926, 314, 698, 589, 745, 
                                      831, 426, 433, 799, 231, 1631, 520, 1461, 838))
                     
selections <- data.frame("City" = character(150), "Household" = numeric(150))

selections$City <- sample(cities$City, size = 150, replace = TRUE)
for(i in 1:150) {
  selections$Household[i] <- sample(seq_len(
    filter(cities, City == selections$City[i])$Households), 1)
}
```


# 2. Getting the Data ready

## Data Import

Here, we just read in the data, focus on the procedural (exercise and stimulant) and blocking (age and gender) factors and the response variables, remove the NA's at the bottom of the list, and name the variables.

```{r data_setup}
data_bp <- read_csv("Blood_Pressure_Data.csv")

head(data_bp)

data_bp <- data_bp[,6:12]
data_bp <- data_bp[-c(49,50,51),]
names(data_bp) <- c("B1", "B2", "A", "B", "BPS", "BPE", "BPD")

head(data_bp)

```

## Data Coding

Once we switch from a numerical age and a character for gender to factors with (-1,0, and 1), we're going to be ready to do analysis. We'll also order the exercise and stimulant factors. 

``` {r data_coding}
data_bp$B1 <- (rep(c(rep(-1,8), rep(0,8), rep(1,8)), 2))
data_bp$B2 <- (c(rep(-1, 24), rep(1,24)))

data_bp <- arrange(data_bp, B2, B1, A, B)

data_bp <- data_bp[,c(3,4,1,2,5,6,7)]

head(data_bp)

```

# 3. Analyzing the Data

## ANOVA

We're going to generate two different ANOVAs. The first `aov_bp` will just be the difference in blood pressure from start to finish as predicted by exercise(A), stimulant(B), and their interaction(A:B). The second will be the same but with the addition of blocking for age (B1) and gender (B2) as predictors.

```{r ANOVA}
aov_bpd <- aov(BPD ~ A*B, data = data_bp)
aov_bpd_blocked <- aov(BPD ~ A*B + B1 + B2, data = data_bp)

summary(aov_bpd)

par(mfrow = c(2,2))
plot(aov_bpd)

knitr::kable(data_bp %>% head(10))
```

Here are some graphs analyzing the equality of variance in our model, and the normality of our data.

``` {r ANOVA_fit}
par(mfrow = c(1,2))
plot(fitted(aov_bpd), resid(aov_bpd), main = "Unblocked: Residual Plot", 
     xlab= "Fitted", ylab = "Residuals")
qqnorm(data_bp$BPD, main = "Unblocked: Normality Plot")
qqline(data_bp$BPD)

par(mfrow = c(1,2))
plot(fitted(aov_bpd_blocked), resid(aov_bpd), main = "Blocked: Residual Plot", 
     xlab= "Fitted", ylab = "Residuals")
qqnorm(data_bp$BPD, main = "Blocked: Normality Plot")
qqline(data_bp$BPD)
```


## Interaction

This is just the plot of the interaction effects.

```{r interaction_plot}

with(data_bp, {interaction.plot(A, B, BPD, main = "Blood Pressure: Exercise vs. Stimulant", 
                                xlab = "Exercise", trace.label = "Stimulant")})

```


## Pairwise Comparisons

Here, we see the results of a Tukey test, which indicates that the difference of no particular pair of means rises to the level of significance.

```{r tukey}
TukeyHSD(aov_bpd)
```



