library(tidyverse)
library(afex)
library(emmeans)

#Q1
my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data1.csv")
head(my_data)

my_data_tidied <- my_data %>%
  mutate(Condition = factor(Condition))
head(my_data_tidied)

my_data_tidied %>%
  group_by(Condition) %>%
  summarise(mean = mean(RT), sd = sd(RT))

my_data_tidied %>% 
  ggplot(aes(x = Condition, y = RT, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13))

model <- aov_4(RT ~ Condition + (1 | Subject), data = my_data_tidied)

summary(model)

emmeans(model, pairwise ~ Condition)

emmeans(model, pairwise ~ Condition, adjust = "bonferroni")

#Q2

my_data2 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data2.csv")
head(my_data2)

my_data_tidied2 <- my_data2 %>%
  mutate(Condition = factor(Condition))
head(my_data_tidied2)

my_data_tidied2 %>%
  group_by(Condition) %>%
  summarise(mean = mean(RT), sd = sd(RT))

my_data_tidied2 %>% 
  ggplot(aes(x = Condition, y = RT, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13))

model2 <- aov_4(RT ~ Condition + (1 | Subject), data = my_data_tidied2)

summary(model2)

emmeans(model2, pairwise ~ Condition)

emmeans(model2, pairwise ~ Condition, adjust = "bonferroni")


#Q3
my_data3 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data3.csv")
head(my_data3)

my_data3tidy <- my_data3 %>%
  mutate(Size = factor(Size), Colour = factor(Colour))
head(my_data3tidy)

my_data3tidy %>% group_by(Size, Colour) %>%
  summarise(mean = mean(RT), sd = sd (RT))

my_data3tidy%>%
ggplot(aes(x = Colour, y = RT, colour = Size)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Subject", y = "RT (s)")

rm_model <- aov_4(RT ~ Subject + (1 + Colour + Size | Subject), data = my_data3tidy)
summary(rm_model)
anova(rm_model)
emmeans(rm_model, pairwise ~ Colour + Size, adjust = "Bonferroni")

#Q4
my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data4.csv")
my_data %>%
  group_by(Difficulty, Time_Pressure, Group) %>%
  summarise(mean_rt = mean(RT), sd_rt = sd(RT)) %>%
  as.data.frame()
my_data %>%
  mutate(Difficulty = factor(Difficulty),
         Time_Pressure = factor(Time_Pressure)) %>%
  ggplot(aes(x = Difficulty:Time_Pressure, 
             y = RT, 
             colour = Difficulty:Time_Pressure)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .55) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  facet_wrap(~ Group) +
  guides(colour = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Difficulty x Time Pressure",
       y = "Reaction Time (ms)")
three_way_model <- aov_4(RT ~ Difficulty * Time_Pressure * Group + 
                           (1 + Difficulty * Time_Pressure | Subject),
                         data = my_data)
summary(three_way_model)
arts_data <- my_data %>%
  filter(Group == "Arts_Students")
maths_data <- my_data %>%
  filter(Group == "Maths_Students")
psych_data <- my_data %>%
  filter(Group == "Psychology_Students")
arts_model <- aov_4(RT ~ Difficulty * Time_Pressure + 
                      (1 + Difficulty * Time_Pressure | Subject),
                    data = arts_data)
maths_model <- aov_4(RT ~ Difficulty * Time_Pressure + 
                       (1 + Difficulty * Time_Pressure | Subject),
                     data = maths_data)
psych_model <- aov_4(RT ~ Difficulty * Time_Pressure + 
                       (1 + Difficulty * Time_Pressure | Subject),
                     data = psych_data)
summary(arts_model)
summary(maths_model)
summary(psych_model)
emmeans(psych_model, pairwise ~ Difficulty:Time_Pressure, adjust = "Bonferroni")


#Q1.1
data_1 <- my_data_tidied %>%
  mutate(Condition = factor(Condition))
head(data_1)

contrasts(data_1$Condition)

data_intercept <- data_1 %>%
  mutate(Condition = fct_relevel(Condition, levels = c("low", "high")))

contrasts$data_intercept
model_lm <- lm(RT ~ Condition, data = data_intercept)
model_lm

model_null <- lm(RT ~ 1, data = data_intercept)
summary(model_lm)
anova(model_lm)
anova(model_lm, model_null)

# RT = Intercept + 1(high)
Low RT = 1178.2
RT = 1178.2 - 313.5(1)
High RT = 864.7

# q2.1
data_2 <- my_data_tidied2 %>%
  mutate(Condition = factor(Condition))
head(data_2)

contrasts(data_2$Condition)

data_intercept2 <- data_2 %>% mutate(Condition = fct_relevel(Condition, levels = c ("very low", "low", "high", "very high")))

contrasts(data_intercept2$Condition)

      
contrasts(data_intercept2$Condition) = contr.sum(4)
contrasts(data_intercept2$Condition)

model_lm2 <- lm(RT ~ Condition, data=data_intercept2)
model_lm2 

model_null2 <- lm(RT ~ 1, data=data_intercept2)

summary(model_lm2)

anova(model_lm2)
anova(model_lm2, model_null2)
