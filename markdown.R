library(tidyverse)
library(dplyr)
library(apaTables)

raw_data <- read_csv(file="raw_data.csv")

raw_data <- read_csv(file="raw_data.csv",na=c("","NA","-999"))


raw_data$sex <- as.factor(raw_data$sex)
levels(raw_data$sex) <- list("Male"=1, "Female"=2)

sex <- select(raw_data, sex)
Neuroticism <- select(raw_data, Neuroticism)
Extraversion <- select(raw_data, Extraversion)

pa_affect <- select(raw_data, delighted, elated, enthusiastic, excited)
na_affect <- select(raw_data, afraid, angry, anxious, ashamed)

psych::describe(Neuroticism)
psych::describe(Extraversion)
psych::describe(pa_affect)
psych::describe(na_affect)

is_bad_value <- na_affect<0 | na_affect>3
na_affect[is_bad_value] <- NA

pos_affect <- psych::alpha(as.data.frame(pa_affect), check.keys=FALSE)$score
neg_affect <- psych::alpha(as.data.frame(na_affect), check.keys=FALSE)$score

analytic_data <- cbind(sex,Neuroticism,Extraversion,pos_affect,neg_affect)

save(analytic_data,file="analytic_data.RData")

write_csv(analytic_data,path="analytic_data.csv")

analytic_data.m.f <- filter(analytic_data, sex=="Male")
analytic_data_male <- select(analytic_data.m.f,Neuroticism,Extraversion,pos_affect,neg_affect)

write_csv(analytic_data,path="analytic_data_male.csv")

apa.cor.table(analytic_data, filename="Table_1_Overall.doc", table.number=1)
apa.cor.table(analytic_data_male, filename="Table_2_Male.doc", table.number=2)

psych::pairs.panels(as.data.frame(analytic_data),lm=TRUE)
psych::pairs.panels(as.data.frame(analytic_data_male),lm=TRUE)

my.hist.1 <- ggplot(analytic_data_male,aes(Neuroticism))
my.hist.1 <- my.hist.1 + geom_histogram(aes(y= ..count..), binwidth=.7,fill="grey55",color="grey55")
my.hist.1 <- my.hist.1 + labs(x="Neuroticism", y="Frequency")
my.hist.1 <- my.hist.1 + coord_cartesian(xlim=c(0,25), ylim=c(0,175))
my.hist.1 <- my.hist.1 + theme_classic()
my.hist.1 <- my.hist.1 + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                                       axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
my.hist.1 <- my.hist.1 + scale_y_continuous( expand = c(0,0))

print(my.hist.1)

ggsave(filename="Figure_4_Neuroticism_Histogram_Male.tiff",plot=my.hist.1, width=6,height=6,units="in")


my.hist.2 <- ggplot(analytic_data_male,aes(neg_affect))
my.hist.2 <- my.hist.2 + geom_histogram(aes(y= ..count..), binwidth=.25,fill="grey55",color="grey55")
my.hist.2 <- my.hist.2 + labs(x="Negative Affect", y="Frequency")
my.hist.2 <- my.hist.2 + coord_cartesian(xlim=c(0,3), ylim=c(0,2000))
my.hist.2 <- my.hist.2 + theme_classic()
my.hist.2 <- my.hist.2 + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                               axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
my.hist.2 <- my.hist.2 + scale_y_continuous( expand = c(0,0))

print(my.hist.2)

ggsave(filename="Figure_5_Negative_Affect_Histogram_Male.tiff",plot=my.hist.2, width=6,height=6,units="in")

my.scatter <- qplot(neg_affect,Neuroticism,data=analytic_data_male)
my.scatter <- my.scatter + theme_classic()
my.scatter <- my.scatter + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                                 axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

my.scatter <- my.scatter + labs(x="Negative Affect",y="Neuroticism")
my.scatter <- my.scatter + coord_cartesian(xlim=c(0,3),ylim=c(0,25))

my.scatter <- my.scatter + geom_smooth(method = "lm", se=FALSE, color="black")
print(my.scatter)

ggsave(filename = "Figure_6_NA_Neuroticism_Scatter_Male.tiff",plot=my.scatter, width=6,height=6,units="in")
