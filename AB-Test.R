# A/B Testing
# Ethan Nusbaum

#### Determing sample-size ####
# We know that our current conversion rate (p1) is 20%
# We want to detect a 5% increase so p2 should be 25%
# We are using a statistical significance level of 5% and a power of 80% with a 2-sided test
sample.size <- power.prop.test(p1 = .2, p2 = .25, power = 0.8, sig.level = 0.05, alternative = 'two.sided')

#### Generating Data and Control and Treatment Groups ####
set.seed(1234)
# 10,000 customers with a mean of 7 impressions. We use a poisson here, but could use normal or uniform. Likely will see poison in reality though
# data <- data.frame(treatment = rep(0,10000), impressions = ceiling(rnorm(10000, mean = 7, sd = 2)))
data <- data.frame(treatment = rep(0,10000), impressions = rpois(10000, 7))
# Now we can randomly select a treatment group of 1094 customers and a control group of the remaining
treatment.group <- data[sample(rownames(data), ceiling(sample.size$n)),]
treatment.group$treatment <- 1
control.group <- data[sample(which(rownames(data)!=rownames(treatment.group)), ceiling(sample.size$n)),]
# Combine back into 1 dataframe
AB.test.data <- rbind(treatment.group, control.group)
rownames(AB.test.data) <- NULL
# Compare the impression numbers in the control and treatment groups to make sure they are statistically the same
# Given the large sample size we can assume normality thanks to CLT and use a t-test
t.test(impressions~treatment, data = AB.test.data)
# From the output we can see that the 95% confidence interval crosses 0 and we have a fairly large t-statistic and p-value. All of this means we cannot reject the null hypothesis of this test so we assume the difference in means of these 2 groups is equal to 0
plot(density(AB.test.data[AB.test.data$treatment<1, "impressions"]), main = "Density Plot of Impressions", xlab = "Impressions")
lines(density(AB.test.data[AB.test.data$treatment==1, "impressions"]), col = "red")
legend("topright", c("Control", "Treatment"), lty = 1, col = c("black","red"))


#### AB Test With Logistic Regression ####
# We want to simulate that users who see more ads are more likely to purchase. This is often true in the real world and also helps with interpreting our models here
# Need to simulate conversion data for control group with 20% of users
AB.test.data$conversion <- 0
AB.test.data$conversion[sample(which(AB.test.data$treatment==0 & AB.test.data$impressions>6),ceiling(sample.size$n*.2))] <- 1
# Need to simulate conversion data for treatment group with 28% of users
AB.test.data$conversion[sample(which(AB.test.data$treatment!=0 & AB.test.data$impressions>6),ceiling(sample.size$n*.28))] <- 1
# Now we can run a logistic regression on our output and see if our treatment group is different from our control group. Given that we simulated the data we should get a differnce.
model.1 <- glm(conversion ~ as.factor(treatment), family = binomial(link = "logit"), data = AB.test.data)
summary(model.1)
# Can we also tell if the total number of our new ads is more impactful than the total number of old?
model.2 <- glm(conversion ~ impressions + impressions:as.factor(treatment), family = binomial(link = "logit"), data = AB.test.data)
summary(model.2)
# More impressions in general means a higher chance of conversion, but more exposure to our new ads increases likelihood of exposure more than the old ad, so this is definitely a better ad.
