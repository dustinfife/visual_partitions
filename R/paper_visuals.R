require(flexplot)
require(patchwork)
require(tidyverse)
# colors/symbols/lines on the left, paneled on the right
a = flexplot(weight.loss~motivation + therapy.type, data=exercise_data, method="lm")
b = flexplot(weight.loss~motivation | therapy.type, data=exercise_data, method="lm",ghost.line="black",)
a + b + plot_layout(widths=c(1,2))
ggsave(filename = "plots/colors_panels.jpg")

# coplot
a = flexplot(weight.loss~motivation | therapy.type + gender, 
             
             data=exercise_data, 
             method="lm", 
             ghost.line = "black")
ggsave(filename = "plots/mv_panels.jpg")


# avp
a = added.plot(weight.loss~motivation + therapy.type, 
             data=exercise_data)
ggsave(filename = "plots/avp.jpg")


d = read.csv("data/health_depression.csv")
d = d %>% dplyr::select(Age, BMI, Smoking, Drinking, Religion, CESD, IATTotalscores, HPLPTotalscores) %>% 
  set_names("Age", "BMI", "Smoking", "Drinking", "Religion", "CESD", "Internet", "Health")

# zero-order analysis
flexplot(Internet~CESD, data=d, method="quadratic")
flexplot(Internet~CESD, data=d, method="quadratic")
anova(lm(Internet~CESD, data=d))
with(d,cor.test(Internet, CESD))

# moderation analysis
anova(lm(CESD~Internet*Religion, data=d))
mod = flexplot(CESD~Internet | Religion, data=d, method="quadratic", ghost.line="gray")
ggsave(filename = "plots/moderation.jpg")

# conditional analysis
anova(lm(CESD~Internet+Age + Health, data=d))
marginal_plot(flexplot(CESD~Internet | Health + Age, data=d, ghost.line="gray"))
added.plot(CESD~Age + Health + Internet, data=d, ghost.line="gray")
head(d)

# meiation plot example
require(flexplot)
require(ggplot2)
Z = rnorm(500)
X = .5*Z + rnorm(500, 0, sqrt(1-.5^2))
Y = .7*X + rnorm(500, 0, sqrt(1-.75^2))
d = data.frame(X = rescale(X, 10, 2), Z = rescale(Z, 10, 2), Y = rescale(Y, 0, 2))
mediation = flexplot::mediate_plot(Y~X+Z, data=d) + labs(x="X", y="Y|Z")
ggsave(filename = "plots/mediation.jpg")
# mediation: iat -> health -> Depression
# Step 1: Fit a model predicting social from quarantine
require(mediation)
mod_mediator = lm(CESD~Internet, data=d)

# Step 2: Fit a model with everything
mod_full = lm(CESD~Internet + Health, data=d)

# Step 3: do the mediation analysis
mediation_model = mediate(mod_mediator, mod_full,
                          boot=TRUE,
                          treat="Internet", mediator = "Health")
summary(mediation_model)
flexplot::mediate_plot(CESD~Internet + Health, data=d)


mod_full = lm(CESD~Internet*Religion + Health + I(Internet^2) + I(Internet^2):Religion, data=d)
reduced  = lm(CESD~Internet+ Religion + Health, data=d)
model.comparison(mod_full, reduced)
