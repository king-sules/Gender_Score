#Importing Files
f1 <- read.csv('Final_Sample.csv')
head(f1)
sapply(f1, mode)
getwd()
#Finding math gender gap
install.packages("dplyr")
library(dplyr)
f11 <- f1 %>% group_by(background) %>% 
  group_map(~coef(lm(pv1math ~ female, data = .))[2])
f11 <- do.call("rbind", f11)
f12 <- f1 %>% group_by(background) %>% 
  dplyr::summarize(ggii = mean(ggi))
f12 <- subset(f12, select = -c(background))
f12 <- do.call("rbind", f12)
f12 <- t(f12)
print(f12)

#Making The Graph
plot(f12, f11, xlab = "gender gap index", ylab = "Math Gender Gap", pch=1, col="red")
abline(lm(f11~f12), col="blue")

#Running Regressions 
colnames(f1)
f1[is.na(f1)] <- 0

olss1= lm(pv1math ~ female + I(ggi*female) + age +I(age*female) + diffgrade + I(diffgrade*female) + female*factor(background) + factor(country) +  factor(year), weights = f1$stweights, data = f1)
olss2= lm(pv1math ~ female + I(ggi*female) + age +I(age*female) + diffgrade + I(diffgrade*female) + I(gdppc*female)+ female*factor(background) + factor(country),weights = f1$stweights,  data = f1)
olss3= lm(pv1math ~ female + I(ggi*female) + age +I(age*female) + diffgrade + I(diffgrade*female) + I(gdppc*female) + ggi + gdppc + female*factor(background) + factor(country) + factor(year), weights = f1$stweights, data = f1)
olss4= lm(pv1math ~ female + I(ggi*female) + age +I(age*female) + diffgrade + I(diffgrade*female) + I(gdppc*female) + fisced + I(fisced*female) + misced + I(misced*female)+ female*factor(background) + factor(country) + factor(year), weights = f1$stweights, data = f1)
olss5= lm(pv1math ~ female + I(ggi*female) + age +I(age*female) + diffgrade + I(diffgrade*female) + I(gdppc*female) + fisced + I(fisced*female) + misced + I(misced*female)+ dadwork + I(dadwork*female) + momwork + I(momwork*female) + homepos + I(homepos*female)+ female*factor(background) + factor(country) + factor(year), weights = f1$stweight, data = f1)
olss6= lm(pv1math ~ female + I(ggi*female) + age +I(age*female) + diffgrade + I(diffgrade*female) + I(gdppc*female) + fisced + I(fisced*female) + misced + I(misced*female)+ dadwork + I(dadwork*female) + momwork + I(momwork*female) + homepos + I(homepos*female) + pcgirls + I(pcgirls*female) + private + I(private*female)+metropolis + I(metropolis*female) + female*factor(background) + factor(country) + factor(year),weights = f1$stweights, data = f1)

#Manipulating Stargazer
install.packages("stargazer")
library(stargazer)
stargazer(olss1, olss2, olss3, olss4, olss5, olss6, title = 'Regression1', type = 'text', omit = c("Constant", "country", "background", "stweight", "year"), omit.stat= c("f", "adj.rsq", "ser"), order = c(1,2,3,4,5,6,7,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,8,9),
          covariate.labels = c("Female", "GGI x Female", "Age of student",
                               "Age x Female", "Diff. grade", "Diff. grade x Female", "GDP x Female", 
                               "Dad educ.", "Dad educ. x Female", "Mom educ.", "Mom educ. x Female",
                               "Dad work", "Dad work x Female"," Mom work", "Mom work x Female",
                               "Home possessions", "Home possessions x Females", "Proportion of girls at school",
                               "Prop. girls x Female", "Private school", "Private school x Female", "school is in a metropolis",
                               "school in metro x Female", "GGI", "GDP"),
          add.lines = list(c("Year FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Ancestory country FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Host country FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Host country FE x fem", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")))

