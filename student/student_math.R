# the absence effect on the math educational performance
# 1. assess the impact of >=3 absences v.s. <3 absence on math final scores
# 2. identify student attributes that contribute to absences

# Note: absences occur before- and after- the first and second period grades,
# theses covariates should bot be included with the pre-treatment variables
library(ggplot2)

student = read.csv("student-mat.csv",sep=";",header=TRUE)
student$abs_grp<- ifelse(student$absences >= 3,"Treatment", "Control")
student$g3_grades <- ifelse(student$G3>=16, "A",
                            ifelse(student$G3>=14, "B",
                            ifelse(student$G3>=12, "C",
                            ifelse(student$G3>=10, "D", "F"))))

# on a categorical level: absence~grade
table(student$abs_grp, student$g3_grades)
chisq.test(student$abs_grp, student$g3_grades)

# on a continuous level: absence~grade
tapply(student$G3, student$abs_grp, mean)# 9.748634 10.990566 
t.test(student$G3 ~ student$abs_grp) # significant

ggplot(student, aes(x=G3, fill=abs_grp)) +  geom_density(alpha=0.25)


student$Pstatus = factor(student$Pstatus)
student$reason = factor(student$reason)
student$higher = factor(student$higher)

# IPW
student$treat = ifelse(student$abs_grp=="Treatment", 1, 0)
prop.mod <- glm(treat ~ age + Pstatus + Medu + reason +
                  higher + Walc, data=student, family=binomial())
summary(prop.mod)
treat_prop_logistic = predict(prop.mod, type = "response")
student$logistic_prob <- treat_prop_logistic

student$logistic_weight <- ifelse(student$treat, 1/(1-treat_prop_logistic), 1/treat_prop_logistic)
hist(student$logistic_weight)
boxplot(logistic_prob ~ treat, data=student)

library(twang)
boosted.mod <- ps(treat ~ age + Pstatus + Medu + reason + higher + Walc,
                  data=student,
                  estimand = "ATE",
                  n.trees = 5000, 
                  interaction.depth=2, 
                  perm.test.iters=0, 
                  verbose=FALSE, 
                  stop.method = c("es.mean"))
summary(boosted.mod)

summary(boosted.mod$gbm.obj,
        n.trees=boosted.mod$desc$es.mean.ATE$n.trees, 
        plot=FALSE)

student$boosted <- get.weights(boosted.mod)
hist(student$boosted)

plot(boosted.mod)
plot(boosted.mod, plots=2)


plot(boosted.mod, plots=3)

bal.table(boosted.mod)


# estimate ATE
library(survey)
design <- svydesign(ids=~1, weights=~boosted, data=student)
glm1 <- svyglm( G3~ treat, design=design)
summary(glm1)

summary(lm(G3 ~ treat + age + Pstatus + Medu + reason + higher + Walc, data=student))





