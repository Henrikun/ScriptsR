library(pwt8)
pwt8.0$cap <- pwt8.0$rgdpe / pwt8.0$pop
library(ggplot2)
dta <- subset(pwt8.0, isocode %in% c("USA", "JPN", "KOR", "CHN","BRA"))
BRA <- log(dta$rgdpe)
qplot(BRA)
qplot(dta$year,
      dta[, 'cap'], 
      geom = "line", 
      group = dta$isocode,
      color = as.factor(dta$isocode)
) + 
  theme(legend.title = element_blank()) +
  scale_y_log10()
library(manipulate)
dta <- subset(pwt8.0, isocode %in% c("USA", "JPN", "KOR", "CHN","BRA"))

manipulate(
  qplot(dta$year,
        dta[, vars], 
        geom = "line", 
        group = dta$isocode,
        color = as.factor(dta$isocode)
  ) + theme(legend.title = element_blank()),
  vars = picker(as.list(colnames(pwt8.0)[-(1:3)]))
)

