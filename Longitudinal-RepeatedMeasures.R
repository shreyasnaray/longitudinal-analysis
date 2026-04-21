library(nlme)
library(ggplot2)
Oxboys
ggplot(Oxboys, aes(x = age, y = height, 
                 group = Subject,
                 color = factor(Subject))) +
  geom_line(alpha = 0.4) +
  geom_point(size = 1) + 
  labs(
    title = 'Height Trajectories by Subject',
    x = 'Age',
    y = 'Height'
  ) +
  theme_minimal()

ggplot(Oxboys, aes(x = age, y = height, 
                   group = Subject,
                   color = factor(Subject))) +
  geom_line(alpha = 0.4) +
  facet_wrap(~ Subject) + 
  labs(
    title = 'Height Trajectories by Subject',
    x = 'Age',
    y = 'Height'
  ) +
  theme_minimal()

# Spaghetti plot
ggplot(Oxboys, aes(x = age, y = height, group = Subject)) +
  geom_line(alpha = 0.3) +
  geom_smooth(aes(group = 1), method = 'loess', linewidth = 1.2) + 
  theme_minimal()

# Mean height per subject
subject_means <- aggregate(height ~ Subject, Oxboys, mean)
# Between-subject variability
var(subject_means$height)

Oxboys$subject_mean <- ave(Oxboys$height, Oxboys$Subject)
Oxboys

library(lme4)
model <- lmer(height ~ 1 + (1 | Subject), data = Oxboys)
var_components <- as.data.frame(VarCorr(model))

between_var <- var_components$vcov[1]
within_var <- attr(VarCorr(model), 'sc')^2

ICC <- between_var / (between_var + within_var)
ICC

model2 <- lmer(height ~ age + (age | Subject), data = Oxboys)
summary(model2)

### Fixed effects model
fixed_effects <- lm(height ~ age, data = Oxboys)
summary(fixed_effects)
### Mixed effects model
mixed_effects1 <- lmer(height ~ age + (1 | Subject), data = Oxboys)
summary(mixed_effects1)

mixed_effects2 <- lmer(height ~ age + (age | Subject), data = Oxboys)
summary(mixed_effects2)

anova(mixed_effects1, mixed_effects2)
### Random effects
# represents the effect for repeated measurements
# within individuals. The first mixed-effect model (random-intercept model)
# allows each subject to have their own baseline height, while a random 
# intercept-and-slope model allows each subject to have their own baseline height
# and their own rate of height change with age. 

Oxboys_treatment <- data.frame(
  Subject = unique(Oxboys$Subject),
  treatment = sample(c('Control', 'Drug'),
                     length(unique(Oxboys$Subject)),
                     replace = TRUE)
)
Oxboys_treatment
Oxboys
df <- merge(Oxboys, Oxboys_treatment, by = 'Subject')
df
# Treatment as a main effect
main_treat <- lmer(height ~ age + treatment + (1 | Subject), data = df)
summary(main_treat)
# Adding interaction term because treatment can affect
# baseline height or growth rate over time
interact_treat <- lmer(height ~ age * treatment + (1 | Subject), data = df)
summary(interact_treat)
# age:treatmentDrug - difference in growth rates between treatment groups
# Adding random slopes to model random intercepts, random slopes, 
# treatment effect on growth
model_treat <- lmer(height ~ age * treatment + (age | Subject), data = df)
summary(model_treat)



ggplot(df, aes(x = age, y = height, group = Subject,
              color = treatment)) +
  geom_line(alpha = 0.3) +
  geom_smooth(aes(group = treatment), linewidth = 1.2) +
  theme_minimal()
# thick smooth lines show the average trend by treatment group



































