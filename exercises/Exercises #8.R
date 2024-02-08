library(tidyverse)
library(ggplot2)
# Load data from MASS into a tibble
birthwt <- as_tibble(MASS::birthwt)

# getting data ready

# rename variables
birthwt <- birthwt %>%
  rename(birthwt.below.2500 = low, 
         mother.age = age,
         mother.weight = lwt,
         mother.smokes = smoke,
         previous.prem.labor = ptl,
         hypertension = ht,
         uterine.irr = ui,
         physician.visits = ftv,
         birthwt.grams = bwt)

# and change factor level names
birthwt <- birthwt %>%
  mutate(race = recode_factor(race, `1` = "white", `2` = "black", `3` = "other")) %>%
  mutate_at(c("mother.smokes", "hypertension", "uterine.irr", "birthwt.below.2500"),
            ~ recode_factor(.x, `0` = "no", `1` = "yes"))


# now create boxplot showing how birthwt.grams varies between smoking status groups
qplot(x = mother.smokes, y = birthwt.grams,
      geom = "boxplot", data = birthwt,
      xlab = "Mother smokes", 
      ylab = "Birthweight (grams)",
      fill = I("lightblue"))

# make a summary table including standard errors
birthwt %>%
  group_by(mother.smokes) %>%
  summarize(num.obs = n(),
            mean.birthwt = round(mean(birthwt.grams), 0),
            sd.birthwt = round(sd(birthwt.grams), 0),
            se.birthwt = round(sd(birthwt.grams) / sqrt(num.obs), 0))

# run t-test #t-test works because one of the variables is categorical
birthwt.t.test <- t.test(birthwt.grams ~ mother.smokes, data = birthwt)

# see results of t-test
birthwt.t.test

# see only p-value
birthwt.t.test$p.value

# see only group means
birthwt.t.test$estimate

