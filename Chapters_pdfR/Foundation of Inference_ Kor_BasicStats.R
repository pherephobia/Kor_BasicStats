# Foundation of Inference

# install.packages("WDI")
library(WDI)
WDI.data <-  
  WDI(country = "all", 
      indicator = c("SH.DYN.NMRT","DC.DAC.USAL.CD", "SH.VAC.TTNS.ZS",
                    "SP.URB.TOTL.IN.ZS", "NE.TRD.GNFS.ZS"), 
      start = 1990, end = 2005, extra = FALSE, cache = NULL)


par(mfrow = c(1, 3))
hist(WDI.data$SH.DYN.NMRT, main = "Mortality rate")
hist(WDI.data$DC.DAC.USAL.CD, main = "Aid Raw")
hist(log(WDI.data$DC.DAC.USAL.CD+.1), main = "Logged Aid")
hist(WDI.data$SH.VAC.TTNS.ZS, main = "Newborns vaccinated for tetanus")
hist(WDI.data$SP.URB.TOTL.IN.ZS, main = "Urban population rate")
hist(WDI.data$NE.TRD.GNFS.ZS, main = "Trade openness_raw")
hist(log(WDI.data$NE.TRD.GNFS.ZS), main = "Logged trade openness")

samp1.mort <- sample(WDI.data$SH.DYN.NMRT, 50)
mean(samp1.mort, na.rm = TRUE) #
mean(WDI.data$SH.DYN.NMRT, na.rm = TRUE)


sample_means50.mort <- rep(NA, 5000)
for(i in 1:5000) {
  samp <- sample(WDI.data$SH.DYN.NMRT, 50) 
  sample_means50.mort[i] <- mean(samp, na.rm = TRUE) 
}

mean(sample_means50.mort, na.rm = TRUE)
mean(WDI.data$SH.DYN.NMRT, na.rm = TRUE)

samp.pop <- sample(WDI.data$SP.URB.TOTL.IN.ZS, 50)
mean.pop <- mean(samp.pop, na.rm = TRUE) # í‘œë³¸ì˜ í‰ê· 
se.pop <- sd(samp.pop, na.rm = TRUE)/sqrt(length(samp.pop)) # í‘œì¤€ì˜¤ì°¨

lower.pop <- mean.pop - (1.96 * se.pop)
upper.pop <- mean.pop + (1.96 * se.pop)
c(lower.pop, mean.pop, upper.pop)

library(tidyverse)
estimates.df <- bind_rows(tibble(lower.pop, mean.pop, upper.pop))
estimates.df$sample.no <- 1
library(ggplot2)
ggplot(data = estimates.df, aes(x = sample.no)) +
  geom_pointrange(aes(y = mean.pop, ymin = lower.pop, ymax = upper.pop))


mean.pop.20 <- rep(NA, 20)
lower.pop.20 <- rep(NA, 20)
upper.pop.20 <- rep(NA, 20)

for(x in 1:20) {
  samp.pop <- sample(WDI.data$SP.URB.TOTL.IN.ZS, 50)
  mean.pop.20[x] <- mean(samp.pop, na.rm = TRUE)
  lower.pop.20[x] <- mean.pop.20[x] - 
    (1.96 * (sd(samp.pop, na.rm = TRUE)/sqrt(length(samp.pop))))
  upper.pop.20[x] <- mean.pop.20[x] + 
    (1.96 * (sd(samp.pop, na.rm = TRUE)/sqrt(length(samp.pop))))
}

estimates.df.20 <- tibble(lower.pop.20, mean.pop.20,upper.pop.20)
estimates.df.20$sample.no <- c(1:20)
ggplot(data = estimates.df.20, aes(x = sample.no)) +
  geom_pointrange(aes(y = mean.pop.20, 
                      ymin = lower.pop.20, 
                      ymax = upper.pop.20))

ggplot(data = estimates.df.20, aes(x = sample.no)) +
  geom_pointrange(aes(y = mean.pop.20, 
                      ymin = lower.pop.20, ymax = upper.pop.20)) + 
  geom_hline(data = WDI.data, 
             aes(yintercept = mean(SP.URB.TOTL.IN.ZS, na.rm = TRUE))) + 
  coord_flip() + 
  theme_bw() + 
  xlab("Sample number") + ylab("Estimates") + 
  ggtitle("Sample means for urban pop, with 95% confidence intervals")


estimates.df.20$outside <- 
  ifelse(estimates.df.20$lower.pop.20 > 
           mean(WDI.data$SP.URB.TOTL.IN.ZS, na.rm = TRUE) | 
           estimates.df.20$upper.pop.20 < 
           mean(WDI.data$SP.URB.TOTL.IN.ZS, na.rm = TRUE), 1, 0)
ggplot(data = estimates.df.20, 
       aes(x = sample.no, color = as.factor(outside))) +
  geom_pointrange(aes(y = mean.pop.20, 
                      ymin = lower.pop.20, 
                      ymax = upper.pop.20)) + 
  geom_hline(data = WDI.data, 
             aes(yintercept = mean(SP.URB.TOTL.IN.ZS, na.rm = TRUE))) + 
  coord_flip() + 
  theme_bw() + 
  scale_color_manual(name = "", values=c("#9999CC", "#CC6666")) +
  theme(legend.position="none") +
  xlab("Sample number") + ylab("Estimates") + 
  ggtitle("Sample means for urban pop, with 95% confidence intervals")



library(ezpickr)
QOG <- pick(file = "http://www.qogdata.pol.gu.se/data/qog_bas_ts_jan19.dta")
QOG <- QOG[QOG$year==2010, ]

knitr::kable(table(is.na(QOG$p_polity2)))
QOG$democracy <- ifelse(QOG$p_polity2 >= 7, 1, 0)
knitr::kable(table(QOG$democracy))
by(QOG$gle_cgdpc, QOG$democracy, mean, na.rm = TRUE)
boxplot(gle_cgdpc ~ democracy, data = QOG) 
QOG  %>% drop_na(democracy) %>% 
  ggplot(aes(y = gle_cgdpc, x = as.factor(democracy))) + 
  geom_boxplot() + 
  labs(x = "Regime type", y = "GDPPC by constant 2010 US dollars") +
  scale_x_discrete(labels=c("0" = "Non democracy", "1" = "Democracy")) + 
  scale_y_continuous(labels = scales::dollar_format()) + 
  theme(
    axis.title.x = element_text(margin = margin(t = 20, b = 10)),
    axis.title.y = element_text(margin = margin(r = 20, l = 20))) +
  guides(fill=FALSE) + theme_bw()


mean.dem <- mean(QOG$gle_cgdpc[QOG$democracy == 1], 
                 na.rm = TRUE)
mean.nondem <- mean(QOG$gle_cgdpc[QOG$democracy == 0],
                    na.rm = TRUE)
sd.dem <- sd(QOG$gle_cgdpc[QOG$democracy == 1], 
             na.rm = TRUE)                      
sd.nondem <- sd(QOG$gle_cgdpc[QOG$democracy == 0],
                na.rm = TRUE)

n.dem <- length(QOG$wdi_gdpcapcon2010[QOG$democracy == 1 & 
                                        is.na(QOG$democracy) == FALSE &
                                        is.na(QOG$wdi_gdpcapcon2010) == FALSE])
n.nondem <- length(QOG$wdi_gdpcapcon2010[QOG$democracy == 0 & 
                                           is.na(QOG$democracy) == FALSE &
                                           is.na(QOG$wdi_gdpcapcon2010) == FALSE])

se.dnd <- sqrt((sd.dem^2 / n.dem) + (sd.nondem^2/n.nondem))
t <- ((mean.dem - mean.nondem) - 0) / se.dnd
t


2 * (1 - pt(t, df = min(n.dem - 1, n.nondem - 1)))
(1 - pt(t, df = min(n.dem - 1, n.nondem - 1)))

t.alt <- (mean(QOG$gle_cgdpc[QOG$democracy == 1], 
               na.rm = TRUE) - 
            mean(QOG$gle_cgdpc[QOG$democracy == 0], 
                 na.rm = TRUE) - 0) / 
  sqrt(
    (
      sd(QOG$gle_cgdpc[QOG$democracy == 1], 
         na.rm = TRUE)^2 / 
        length(QOG$gle_cgdpc[QOG$democracy == 1 & 
                               is.na(QOG$democracy) == FALSE &
                               is.na(QOG$gle_cgdpc) == FALSE])
    ) +
      (
        sd(QOG$gle_cgdpc[QOG$democracy == 0], 
           na.rm = TRUE)^2 / 
          length(QOG$gle_cgdpc[QOG$democracy == 0 & 
                                 is.na(QOG$democracy) == FALSE &
                                 is.na(QOG$gle_cgdpc) == FALSE])
      )
  )
t.alt


ttest1 <- t.test(QOG$gle_cgdpc[QOG$democracy == 1],
                 QOG$gle_cgdpc[QOG$democracy == 0])
ttest1

ttest2 <- t.test(gle_cgdpc ~ democracy, data = QOG)
ttest2

knitr::kable(table(QOG$ciri_tort))
boxplot(gle_cgdpc ~ ciri_tort, data = QOG)
QOG  %>% drop_na(ciri_tort) %>% 
  ggplot(aes(y = gle_cgdpc, x = as.factor(ciri_tort))) + 
  geom_boxplot() + 
  labs(x = "Torture type", y = "GDPPC by constant 2010 US dollars") +
  scale_x_discrete(labels=c("0" = "Torture (+50)", 
                            "1" = "Torture (1-49)",
                            "2" = "No torture")) +
  scale_y_continuous(labels = scales::dollar_format()) + 
  theme(
    axis.title.x = element_text(margin = margin(t = 20, b = 10)),
    axis.title.y = element_text(margin = margin(r = 20, l = 10))) +
  guides(fill=FALSE) + theme_bw()


model1 <- aov(gle_cgdpc ~ as.factor(ciri_tort), data = QOG)
summary(model1) 
boxplot(gle_cgdpc ~ ciri_tort:democracy, data = QOG) 
QOG %>% drop_na(ciri_tort) %>% drop_na(democracy) %>%
  ggplot(aes(y=gle_cgdpc, x=as.factor(ciri_tort):as.factor(democracy),
             group=interaction(democracy, ciri_tort))) + geom_boxplot() + 
  labs(x = "Torture X Regime type", y = "GDPPC by constant 2010 US dollars") +
  scale_x_discrete(labels=c("0:0" = "Torture (+50)\nNon Democracy", 
                            "0:1" = "Torture (+50)\nDemocracy", 
                            "1:0" = "Torture (1-49)\nNon Democracy",
                            "1:1" = "Torture (1-49)\nDemocracy",
                            "2:0" = "No torture\nNon Democracy",
                            "2:1" = "No torture\nDemocracy")) +
  scale_y_continuous(labels = scales::dollar_format()) + 
  theme(
    axis.title.x = element_text(margin = margin(t = 15, b = 10)),
    axis.title.y = element_text(margin = margin(r = 20, l = 10)),
    axis.text.x = element_text(margin = margin(t = 10))) +
  guides(fill=FALSE) + theme_bw()


model2 <- aov(gle_cgdpc ~ as.factor(ciri_tort):as.factor(democracy), data = QOG)
summary(model2) 


here::here() %>% setwd()
download.file("http://www.openintro.org/stat/data/atheism.RData", 
              destfile = "atheism.RData")
load("atheism.RData")
glimpse(atheism)

knitr::kable(table(atheism$response))
table1 <- table(atheism$response, atheism$year)

margin.table(table1, 1) %>% knitr::kable()
margin.table(table1, 2) %>% knitr::kable()

prop.table(table1, 1) %>% knitr::kable()
prop.table(table1, 2) %>% knitr::kable()


us <- subset(atheism, nationality == "United States")
prop.test(table(us$year, us$response), correct = FALSE) 
prop.test(table(us$response, us$year), correct = FALSE)
ustab <- table(us$year, us$response)
prop.test(ustab, correct = FALSE)
prop.test(table(atheism$year[atheism$nationality == 
                               "United States"], 
                atheism$response[atheism$nationality == 
                                   "United States"]), 
          correct = FALSE)


anes <- pick("example.data/anes_timeseries_2012_Stata12.dta")
xtabs( ~ pid_x + presvote2012_x, data = anes) %>% knitr::kable() 
table(anes$pid_x, anes$presvote2012_x) %>% knitr::kable() 

table(anes$pid_x) %>% knitr::kable()
table(anes$presvote2012_x) %>% knitr::kable()


anes.nomissing <- subset(anes, pid_x != "-2" & 
                           presvote2012_x != "-9" &
                           presvote2012_x != "-6" & 
                           presvote2012_x != "-2")
table(anes.nomissing$pid_x) %>% knitr::kable()
table(anes.nomissing$presvote2012_x) %>% knitr::kable()

anes.nomissing$pid_x <- factor(anes.nomissing$pid_x)
anes.nomissing$presvote2012_x <- 
  factor(anes.nomissing$presvote2012_x, 
         labels = c("Obama", "Romney", "Other"))

table(anes.nomissing$pid_x) %>% t() %>% knitr::kable()
table(anes.nomissing$presvote2012_x) %>% knitr::kable()

xtabs( ~ pid_x + presvote2012_x, data = anes.nomissing) %>% knitr::kable()

chisq.test(xtabs( ~ pid_x + presvote2012_x, data = anes.nomissing)) 

model1 <- xtabs( ~ pid_x + presvote2012_x, data = anes.nomissing)
chisq.test(model1)

premade.table <- as_tibble(as.data.frame(table(anes.nomissing$pid_x, 
                                               anes.nomissing$presvote2012_x)))
premade.table

xtabs(Freq ~ Var1 + Var2, data = premade.table)
xtabs(weight_full ~ pid_x + presvote2012_x, data = anes.nomissing)

chisq.test(xtabs(weight_full ~ pid_x + presvote2012_x, data = anes.nomissing))

