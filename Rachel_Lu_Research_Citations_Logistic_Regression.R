#set working directory to wherever csv is stored on computer
setwd("/Users/Rachel/Documents/New York University/Senior 2015-2016/Spring 2016/SPUR")
professors = read.csv("professors.csv")
professors$X = NULL

#create binary variable 'inc_dec', 1 for increase, 0 for decrease
professors$inc_dec = as.numeric(professors$Current_Citations > professors$Last_citations, na.rm=TRUE)

#look at only professors who are active for more than 10 and less than 20 years
p_ten = professors[which(professors$Years.Active > 10 & professors$Years.Active < 20),]
ind_prof = professors[55:88,] #for testing on Robert Anderson, individual professor

#use this fit logistic model on professors between 10 and 20 years, with all 5 years of citations
fit = glm(inc_dec~Last_citations+Last2_citations+Last3_citations+Last4_citations+Last5_citations, data=p_ten, family=binomial)
summary(fit)
#results show Last5_citations not statistically significant, so it will be taken out later, but here is the accuracy anyways
citations_predict= predict(fit, type="response")
citations_predict.f = as.numeric(citations_predict > .2)
accurate = as.numeric(citations_predict.f == p_ten$inc_dec)
accuracy = mean(accurate, na.rm=TRUE)

#benchmark prediction at zero for 10-20 years
zero = runif(length(citations_predict), 0, 0)
zero_accuracy = mean(zero == p_ten$inc_dec, na.rm = TRUE)
#benchmark prediction at random for 10-20 years
r = round(runif(length(citations_predict),0,1))
r_accuracy = mean(r == p_ten$inc_dec, na.rm=TRUE)
#benchmark prediction at 1 for 10-20 years
one = runif(length(citations_predict), 1, 1)
one_accuracy=mean(one == p_ten$inc_dec, na.rm=TRUE)

#use this fit logistic model on ENTIRE DATASET
fit = glm(inc_dec~Last_citations+Last2_citations+Last3_citations+Last4_citations+Last5_citations, data=professors, family=binomial)
summary(fit)
#on the entire dataset, all five years are statistically important
citations_predict= predict(fit, type="response")
citations_predict.f = as.numeric(citations_predict > .24)
accurate = as.numeric(citations_predict.f == professors$inc_dec)
accuracy = mean(accurate, na.rm=TRUE)

#benchmark prediction at zero for entire dataset
zero = runif(length(citations_predict), 0, 0)
zero_accuracy = mean(zero == professors$inc_dec, na.rm = TRUE)
#benchmark prediction at random for entire dataset
r = round(runif(length(citations_predict),0,1))
r_accuracy = mean(r == professors$inc_dec, na.rm=TRUE)
#benchmark prediction at 1 for entire dataset
one = runif(length(citations_predict), 1, 1)
one_accuracy=mean(one == professors$inc_dec, na.rm=TRUE)

#test model with Years.Active, Age and Last_citations on 10-20 years active
fit = glm(inc_dec~Last_citations + Age + Years.Active, data=p_ten, family=binomial)
summary(fit)
citations_predict= predict(fit, type="response")
citations_predict.f = as.numeric(citations_predict > .2)
accurate = as.numeric(citations_predict.f == p_ten$inc_dec)
accuracy = mean(accurate, na.rm=TRUE)

#test model with Years.Active, Age and Last_citations on entire dataset 
fit = glm(inc_dec~Last_citations + Age + Years.Active, data=professors, family=binomial)
summary(fit)
citations_predict= predict(fit, type="response")
citations_predict.f = as.numeric(citations_predict > .2)
accurate = as.numeric(citations_predict.f == professors$inc_dec)
accuracy = mean(accurate, na.rm=TRUE)


#Testing logistical regresion with the CHANGE in citations
professors$Change1 = professors$Last_citations - professors$Last2_citations
professors$Change2 = professors$Current_Citations - professors$Last_citations
professors$Change3 = professors$Last2_citations - professors$Last3_citations

#testing change on entire dataset
fit = glm(inc_dec~Change1, data=professors, family=binomial)
summary(fit)
citations_predict= predict(fit, type="response")
citations_predict.f = as.numeric(citations_predict > .24)
accurate = as.numeric(citations_predict.f == professors$inc_dec)
accuracy = mean(accurate, na.rm=TRUE)

#conclusion --> accuracy is similar to if you just guessed that they would all increase

