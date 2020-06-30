
library(quickpsy)
library(dplyr)
library(cowplot)
library(xlsx)

datByContrastA <- group_by(PC_a, QT_nomogram_difference_ms)
datByContrastB <- group_by(PC_b, QT_nomogram_difference_ms)
datByContrastC <- group_by(PC_c, QT_nomogram_difference_ms)
datByContrastD <- group_by(PC_d, QT_nomogram_difference_ms)

averagesA <- summarise(datByContrastA, n = n(), nYes = sum(resp), nNo = n - nYes, p = nYes / n)
averagesA <- averagesA %>% mutate(Visualisation = 'Cartesian')
averagesA

averagesB <- summarise(datByContrastB, n = n(), nYes = sum(resp), nNo = n - nYes, p = nYes / n)
averagesB <- averagesB %>% mutate(Visualisation = 'Cartesian Coloured')

averagesC <- summarise(datByContrastC, n = n(), nYes = sum(resp), nNo = n - nYes, p = nYes / n)
averagesC <- averagesC %>% mutate(Visualisation = 'Polar')

averagesD <- summarise(datByContrastD, n = n(), nYes = sum(resp), nNo = n - nYes, p = nYes / n)
averagesD <- averagesD %>% mutate(Visualisation = 'Polar Coloured')

averagesAll <- rbind(averagesA, averagesB,averagesC,averagesD)
averagesAll

fitAll <- quickpsy(averagesAll, QT_nomogram_difference_ms, nYes, n, grouping =.(Visualisation),prob=.75, lapses = TRUE, fun = logistic_fun)

plot_grid(plot(fitAll), plotthresholds(fitAll), labels = c('A', 'B')) 


