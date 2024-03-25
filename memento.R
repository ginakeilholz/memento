setwd("C:/Users/ginak/OneDrive/Dokumente/Uni/Freiwillig/Neuro/Free Field/reversed speech/statistics R")


library(dplyr)


master=read.table(file="C:/Users/ginak/OneDrive/Dokumente/Uni/Freiwillig/Neuro/Free Field/reversed speech/statistics R/updated_table.csv",header=TRUE,sep=",")
master[ master == "" ] <- NA

#add two collumns for age and gender 


# Filter the data for only the "experiment" task phase
masterexp <- master %>%
  filter(task_phase == "experiment")

#filter data for only single source
mastersingle <- masterexp %>%
  filter(task_type == "single_source")
#filter data for only multi source
mastermulti <- masterexp %>%
  filter(task_type == "multi_source")

#filter for each setting 
masterazi <- masterexp %>%
  filter(task_plane == "azimuth")
masterelev <- masterexp %>%
  filter(task_plane == "elevation")
masterfb <- masterexp %>%
  filter(task_plane == "front-back")
mastercol <- masterexp %>%
  filter(task_plane == "collocated")

# Filter data for participant & talker_gender
females_femalet <- subset(mastermulti, sub_gender == "female" & target_talker_gender == "female")
females_malet <- subset(mastermulti, sub_gender == "female" & target_talker_gender == "male")

males_malet <- subset(mastermulti, sub_gender == "male" & target_talker_gender == "male")
males_femalet <- subset(mastermulti, sub_gender == "male" & target_talker_gender == "female")

female_talker <- subset(mastermulti, target_talker_gender == "female")
male_talker  <- subset(mastermulti, target_talker_gender == "male")

female_sub_multi <- subset(mastermulti, sub_gender == "female")
male_sub_multi <- subset(mastermulti, sub_gender == "male")

#calculate mean scores for single source
# Filter the data for the specified conditions
singlesource_meanscores <- master %>%
  filter(task_type == "single_source" & task_phase == "experiment") %>%
  group_by(subject_id) %>%
  summarise(mean_score = mean(score, na.rm = TRUE))

# Output the filtered data
print(singlesource_meanscores)


#Mixed Measure Model

library(lme4)

# Convert 'task_plane' to a factor variable
mastermulti$task_plane <- factor(mastermulti$task_plane)

# Change the reference level of 'task_plane' to 'collocated'
mastermulti$task_plane <- relevel(mastermulti$task_plane, ref = "collocated")


rs_full.mod <- lmer(score~1 + task_plane + (1 + task_plane|subject_id) + 
                      (1 + task_plane|masker_segment_length), data=mastermulti, REML=FALSE)


rs_reduced.mod <- lmer(score~1  + (1 + task_plane|subject_id) + 
                        (1 + task_plane|masker_segment_length), data=mastermulti, REML=FALSE)

anova(rs_reduced.mod, rs_full.mod)
#Data: mastermulti
#Models:
#rs_reduced.mod: score ~ 1 + (1 + task_plane | subject_id) + (1 + task_plane | masker_segment_length)
#rs_full.mod: score ~ 1 + task_plane + (1 + task_plane | subject_id) + (1 + task_plane | masker_segment_length)
#               npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
#rs_reduced.mod   22 5389.5 5538.4 -2672.7   5345.5                        
#rs_full.mod      25 5380.5 5549.8 -2665.2   5330.5 14.972  3   0.001841 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# significant p value (0.001841) indicated significant effect of task_plane


# mixed() function takes a model specification as input and conducts 
#likelihood-ratio tests on all fixed (but not random) effects in the model 
#when the argument method = ‘LRT’ is included
#good to avoid p-hacking because reduced model outcomes are not individually listed

install.packages("afex")
library(afex)

mixed(score~1 + task_plane + (1 + task_plane|subject_id) + 
        (1 + task_plane|masker_segment_length), data=mastermulti, method='LRT')

#tests indicate significance but not magnitude of effect for that:

summary(rs_full.mod)

#Random effects:
#Groups                Name                 Variance Std.Dev. Corr             
#subject_id            (Intercept)          0.002181 0.04670                   
#                      task_planeazimuth    0.011122 0.10546  0.57             
#                      task_planeelevation  0.010999 0.10488  0.75  0.32       
#                      task_planefront-back 0.013829 0.11760  0.14  0.23  0.27 
#masker_segment_length (Intercept)          0.013701 0.11705                   
#                      task_planeazimuth    0.003233 0.05686  -0.99            
#                      task_planeelevation  0.001105 0.03325  -0.97  0.99      
#                      task_planefront-back 0.001163 0.03410  -0.96  0.99  1.00
#Residual                                   0.130601 0.36139 -> “random” deviations from the predicted values  
#                                                                 not due to sub or sml


coef(rs_full.mod)

save.image("memento.Rdata")

