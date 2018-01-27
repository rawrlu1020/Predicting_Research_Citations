library(ggplot2)
setwd("/Users/Rachel/Documents/New York University/Senior 2015-2016/Spring 2016/SPUR")
professors = read.csv("professors.csv")
professors$X = NULL

#t test of Current_citations and last citations
t.test(professors$Current_Citations, professors$Last_citations)
#t test of Current_citations and last citations
t.test(professors$Current_Citations, professors$Last2_citations)
#t test of Current_citations and last citations
t.test(professors$Last_citations, professors$Last2_citations)

#linear regression
current = lm(professors$Last_citations ~ professors$Current_Citations)
summary(current)
#generate predicted values and plot them
plot(professors$Last_citations, professors$Current_Citations, na.rm=TRUE)
points(professors$Last_citations, predict(lm(professors$Current_Citations~professors$Last_citations)), type="p", col="red")

#linear regression
last = lm(professors$Last_citations ~ professors$Last2_citations)
summary(last)
#linear regression
last2 = lm(professors$Current_Citations ~ professors$Last2_citations)
summary(last2)

#creating a binary variable for increase or decrease
inc_dec = as.numeric(professors$Current_Citations > professors$Last_citations)
professors$inc_dec = inc_dec

#testing a group of professors first
prof = professors[1:88,]
year = as.factor(prof$Year)
name = as.factor(prof$Name)
active = as.factor(prof$Years.Active)
professors.g = ggplot(prof, aes(year, prof$Current_Citations, group=name, colour=name)) + geom_line()
show(professors.g)

lm(prof$Current_Citations ~ prof$Last_citations+prof$Years.Active+prof$Last2_citations)
summary(lm(prof$Current_Citations ~ prof$Last_citations+prof$Years.Active+prof$Last2_citations))

#testing the logistic classifier on the 3 professors
fit = glm(inc_dec ~ Last_citations+Last2_citations, data = prof, family=binomial)
summary(fit)
citations_predict= predict(fit, type="response")
citations_predict.f = as.numeric(citations_predict > .67)
accurate = as.numeric(citations_predict.f == professors$inc_dec)
accuracy = mean(accurate)
