# Eugen Klein: Relation between acoustic and
# articulatory dimensions of speech sounds, 2020
# chapter 5 analysis

# employed libraries

library(Boruta)
library(randomForest)
library(ggplot2)
library(reshape2)
library(mgcv)
library(itsadug)

####################################################
# baseline vs shift vs post-shift production (Section 5.3.3)
####################################################

# apply the same code for a_diffMidLowRMS, a_diffMidLowMinAmp,
# a_CoG for Table 5.5

# skewness 
tapply(fricEma[fricEma$phase == 'baseline', ]$a_skew, list(fricEma[fricEma$phase == 'baseline', ]$repetition), mean)
tapply(fricEma[fricEma$phase == 'baseline', ]$a_skew, list(fricEma[fricEma$phase == 'baseline', ]$repetition), sd)
tapply(fricEma[fricEma$phase == 'shift_4', ]$a_skew, list(fricEma[fricEma$phase == 'shift_4', ]$repetition), mean)
tapply(fricEma[fricEma$phase == 'shift_4', ]$a_skew, list(fricEma[fricEma$phase == 'shift_4', ]$repetition), sd)
tapply(fricEma[fricEma$phase == 'post_5', ]$a_skew, list(fricEma[fricEma$phase == 'post_5', ]$repetition), mean)
tapply(fricEma[fricEma$phase == 'post_5', ]$a_skew, list(fricEma[fricEma$phase == 'post_5', ]$repetition), sd)

# t-tests for the same data
# apply the same code for a_diffMidLowRMS, a_diffMidLowMinAmp,
# a_CoG
t.test(fricEma[fricEma$phase == 'baseline' & fricEma$repetition == 1, ]$a_skew, fricEma[fricEma$phase == 'shift_4' & fricEma$repetition == 1, ]$a_skew)
t.test(fricEma[fricEma$phase == 'baseline' & fricEma$repetition == 2, ]$a_skew, fricEma[fricEma$phase == 'shift_4' & fricEma$repetition == 2, ]$a_skew)

# kinematic data without subject f13 who has TB sensor broken
# apply the same code for k_TT_y, k_JAW_z, k_TT_z, k_JAW_y
# k_TB_z for Table 5.6
# k_LL_z
tapply(fricEma[fricEma$phase == 'baseline' & fricEma$subject != 'f13', ]$k_LL_z, list(fricEma[fricEma$phase == 'baseline' & fricEma$subject != 'f13', ]$repetition), mean)
tapply(fricEma[fricEma$phase == 'baseline' & fricEma$subject != 'f13', ]$k_LL_z, list(fricEma[fricEma$phase == 'baseline' & fricEma$subject != 'f13', ]$repetition), sd)
tapply(fricEma[fricEma$phase == 'shift_4' & fricEma$subject != 'f13', ]$k_LL_z, list(fricEma[fricEma$phase == 'shift_4' & fricEma$subject != 'f13', ]$repetition), mean)
tapply(fricEma[fricEma$phase == 'shift_4' & fricEma$subject != 'f13', ]$k_LL_z, list(fricEma[fricEma$phase == 'shift_4' & fricEma$subject != 'f13', ]$repetition), sd)
tapply(fricEma[fricEma$phase == 'post_5' & fricEma$subject != 'f13', ]$k_LL_z, list(fricEma[fricEma$phase == 'post_5' & fricEma$subject != 'f13', ]$repetition), mean)
tapply(fricEma[fricEma$phase == 'post_5' & fricEma$subject != 'f13', ]$k_LL_z, list(fricEma[fricEma$phase == 'post_5' & fricEma$subject != 'f13', ]$repetition), sd)

# t-tests for the same data
# apply the same code for k_TT_y, k_JAW_z, k_TT_z, k_JAW_y
# k_TB_z
t.test(fricEmaTemp[fricEmaTemp$phase == 'baseline' & fricEmaTemp$subject != 'f13' & fricEmaTemp$repetition == 1, ]$k_LL_z, fricEmaTemp[fricEmaTemp$phase == 'shift_4' & fricEmaTemp$subject != 'f13' & fricEmaTemp$repetition == 1, ]$k_LL_z)
t.test(fricEmaTemp[fricEmaTemp$phase == 'baseline' & fricEmaTemp$subject != 'f13' & fricEmaTemp$repetition == 2, ]$k_LL_z, fricEmaTemp[fricEmaTemp$phase == 'shift_4' & fricEmaTemp$subject != 'f13' & fricEmaTemp$repetition == 2, ]$k_LL_z)

####################################################
# Random forest modeling of compensatory productions
# (Section 5.3.2)
####################################################

# identify relevant acoustic parameters for each phase (baseline, shift phases,
# and post-shift phase): Boruta computations for Table 5.3

# model 1: baseline phase
set.seed(71)
acoustic.base = Boruta(fricEma[fricEma$phase %in% c('baseline'),
	which(names(fricEma) %in% aParameter)],
	fricEma[fricEma$phase  %in% c('baseline'),]$repetition,
	doTrace = 2, maxRuns=500)

acoustic.baseRelevant = getSelectedAttributes(acoustic.base, withTentative = FALSE)

# examine results
# par(mar=c(12,5,1,1))
# plot(acoustic.base, las = 3)
# plotImpHistory(acoustic.base)
# summary(acoustic.base$ImpHistory)

# model 2: shift 1 phase
set.seed(71)
acoustic.shift1 = Boruta(fricEma[fricEma$phase %in% c('shift_2'),
	which(names(fricEma) %in% aParameter)],
	fricEma[fricEma$phase  %in% c('shift_2'),]$repetition,
	doTrace = 2, maxRuns=500)

acoustic.shift1Relevant = getSelectedAttributes(acoustic.shift1, withTentative = FALSE)

#model 3: shift 2 phase
set.seed(71)
acoustic.shift2 = Boruta(fricEma[fricEma$phase %in% c('shift_3'),
	which(names(fricEma) %in% aParameter)],
	fricEma[fricEma$phase  %in% c('shift_3'),]$repetition,
	doTrace = 2, maxRuns=500)

acoustic.shift2Relevant = getSelectedAttributes(acoustic.shift2, withTentative = FALSE)

#model 4: shift 3 phase
set.seed(71)
acoustic.shift3 = Boruta(fricEma[fricEma$phase %in% c('shift_4'),
	which(names(fricEma) %in% aParameter)],
	fricEma[fricEma$phase  %in% c('shift_4'),]$repetition,
	doTrace = 2, maxRuns=500)

acoustic.shift3Relevant = getSelectedAttributes(acoustic.shift3, withTentative = FALSE)

# model 5: post phase
set.seed(71)
acoustic.post = Boruta(fricEma[fricEma$phase %in% c('post_5'),
	which(names(fricEma) %in% aParameter)],
	fricEma[fricEma$phase  %in% c('post_5'),]$repetition,
	doTrace = 2, maxRuns=500)

acoustic.postRelevant = getSelectedAttributes(acoustic.post, withTentative = FALSE)

# plot relevant parameters
plot(acoustic.base, las = 3)
plot(acoustic.shift1, las = 3)
plot(acoustic.shift2, las = 3)
plot(acoustic.shift3, las = 3)
plot(acoustic.post, las = 3)

# define acoustic Random forest models for each phase including only
# relevant parameters (Section 5.3.1)
acoustic.vars1 = as.formula(paste('as.factor(repetition) ~', paste(acoustic.baseRelevant, collapse = " + "), sep = ""))
acoustic.vars2 = as.formula(paste('as.factor(repetition) ~', paste(acoustic.shift1Relevant, collapse = " + "), sep = ""))
acoustic.vars3 = as.formula(paste('as.factor(repetition) ~', paste(acoustic.shift2Relevant, collapse = " + "), sep = ""))
acoustic.vars4 = as.formula(paste('as.factor(repetition) ~', paste(acoustic.shift3Relevant, collapse = " + "), sep = ""))
acoustic.vars5 = as.formula(paste('as.factor(repetition) ~', paste(acoustic.postRelevant, collapse = " + "), sep = ""))

# compure acoustic RF models
set.seed(71)
rf.acoustic.base = randomForest(acoustic.vars1,
                    data = fricEma[fricEma$phase == 'baseline',],
                    importance = T, mtry = length(acoustic.baseRelevant),
                    ntree = 10000)

set.seed(71)
rf.acoustic.shift1 = randomForest(acoustic.vars2,
                    data = fricEma[fricEma$phase == 'shift_2',],
                    importance = T, mtry = length(acoustic.shift1Relevant),
                    ntree = 10000)

set.seed(71)
rf.acoustic.shift2 = randomForest(acoustic.vars3,
                    data = fricEma[fricEma$phase == 'shift_3',],
                    importance = T, mtry = length(acoustic.shift2Relevant),
                    ntree = 10000)
set.seed(71)
rf.acoustic.shift3 = randomForest(acoustic.vars4,
                    data = fricEma[fricEma$phase == 'shift_4',],
                    importance = T, mtry = length(acoustic.shift3Relevant),
                    ntree = 10000)

set.seed(71)
rf.acoustic.post = randomForest(acoustic.vars5,
                    data = fricEma[fricEma$phase == 'post_5',],
                    importance = T, mtry = length(acoustic.postRelevant),
                    ntree = 10000)

# compile prediction accuracies for acoustics
acoustic.accuracies = matrix(c("Baseline", "Shift 1", "Shift 2", "Shift 3", "Post",
                      1 - (rf.acoustic.base$confusion[5] + rf.acoustic.base$confusion[6])/2,
                      1 - (rf.acoustic.shift1$confusion[5] + rf.acoustic.shift1$confusion[6])/2,
                      1 - (rf.acoustic.shift2$confusion[5] + rf.acoustic.shift2$confusion[6])/2,
                      1 - (rf.acoustic.shift3$confusion[5] + rf.acoustic.shift3$confusion[6])/2,
                      1 - (rf.acoustic.post$confusion[5] + rf.acoustic.post$confusion[6])/2),
                    ncol = 2, byrow = F)

colnames(acoustic.accuracies) = c("phase","accuracy")
acoustic.accuracies = as.data.frame(acoustic.accuracies)

acoustic.accuracies$accuracy = as.numeric(as.character(acoustic.accuracies$accuracy))
acoustic.accuracies$phase = factor(acoustic.accuracies$phase, levels=c("Baseline", "Shift 1", "Shift 2", "Shift 3", "Post"))

# bar plot showing all prediction accuracy scores for acoustics (Figure 5.4)
(p = ggplot(data=acoustic.accuracies, aes(x=phase, y=accuracy))
  + geom_bar(stat="identity", width=0.7, fill = "#B0AFAF")
  + geom_text(aes(label=round(accuracy*100, digits =2)), vjust=-.7, size=5)
  + scale_y_continuous(breaks=c(0.45, 0.5, 0.55, 0.6, 0.65),
                       labels=c('45', '50', '55', '60', '65'))
  + geom_hline(yintercept = 0.5, linetype = 2)
  + theme_bw()
  + theme(text = element_text(size=18),
          axis.text = element_text(size=16),
          axis.title = element_text(size=18, vjust=-1),
          legend.text = element_text(size=18),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  + theme(legend.position = "bottom",
          legend.text=element_text(size=16))
  + guides(linetype=guide_legend(override.aes=list(fill=NA)))
  + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  + labs(x = "Phase", y = "Prediction accuracy (%)")
  + coord_cartesian(ylim=c(0.45, 0.65)))

# identify relevant kinematic parameters for each phase (baseline, shift phases,
# and post-shift phase): Boruta computations for Table 5.4

# model 1: baseline phase
set.seed(71)
kinematic.base = Boruta(fricEma[fricEma$phase %in% c('baseline'),
	which(names(fricEma) %in% kParameter)],
	fricEma[fricEma$phase  %in% c('baseline'),]$repetition,
	doTrace = 2, maxRuns=500)

kinematic.baseRelevant = getSelectedAttributes(kinematic.base, withTentative = FALSE)

# model 2: shift 1 phase
set.seed(71)
kinematic.shift1 = Boruta(fricEma[fricEma$phase %in% c('shift_2'),
	which(names(fricEma) %in% kParameter)],
	fricEma[fricEma$phase  %in% c('shift_2'),]$repetition,
	doTrace = 2, maxRuns=500)

kinematic.shift1Relevant = getSelectedAttributes(kinematic.shift1, withTentative = FALSE)

#model 3: shift 2 phase
set.seed(71)
kinematic.shift2 = Boruta(fricEma[fricEma$phase %in% c('shift_3'),
	which(names(fricEma) %in% kParameter)],
	fricEma[fricEma$phase  %in% c('shift_3'),]$repetition,
	doTrace = 2, maxRuns=500)

kinematic.shift2Relevant = getSelectedAttributes(kinematic.shift2, withTentative = FALSE)

#model 4: shift 3 phase
set.seed(71)
kinematic.shift3 = Boruta(fricEma[fricEma$phase %in% c('shift_4'),
	which(names(fricEma) %in% kParameter)],
	fricEma[fricEma$phase  %in% c('shift_4'),]$repetition,
	doTrace = 2, maxRuns=500)
kinematic.shift3Relevant = getSelectedAttributes(kinematic.shift3, withTentative = FALSE)

# model 5: post phase
set.seed(71)
kinematic.post = Boruta(fricEma[fricEma$phase %in% c('post_5'),
	which(names(fricEma) %in% kParameter)],
	fricEma[fricEma$phase  %in% c('post_5'),]$repetition,
	doTrace = 2, maxRuns=500)

kinematic.postRelevant = getSelectedAttributes(kinematic.post, withTentative = FALSE)

# plot relevant parameters
plot(kinematic.base, las = 3)
plot(kinematic.shift1, las = 3)
plot(kinematic.shift2, las = 3)
plot(kinematic.shift3, las = 3)
plot(kinematic.post, las = 3)

# define kinematic Random forest models for each phase including only relevant parameters
kinematic.vars1 = as.formula(paste('as.factor(repetition) ~', paste(kinematic.baseRelevant, collapse = " + "), sep = ""))
kinematic.vars2 = as.formula(paste('as.factor(repetition) ~', paste(kinematic.shift1Relevant, collapse = " + "), sep = ""))
kinematic.vars3 = as.formula(paste('as.factor(repetition) ~', paste(kinematic.shift2Relevant, collapse = " + "), sep = ""))
kinematic.vars4 = as.formula(paste('as.factor(repetition) ~', paste(kinematic.shift3Relevant, collapse = " + "), sep = ""))
kinematic.vars5 = as.formula(paste('as.factor(repetition) ~', paste(kinematic.postRelevant, collapse = " + "), sep = ""))

# compute kinematic RF models
set.seed(71)
rf.kinematic.base = randomForest(kinematic.vars1,
                    data = fricEma[fricEma$phase == 'baseline',],
                    importance = T, mtry = length(kinematic.baseRelevant),
                    ntree = 10000)

set.seed(71)
rf.kinematic.shift1 = randomForest(kinematic.vars2,
                    data = fricEma[fricEma$phase == 'shift_2',],
                    importance = T, mtry = length(kinematic.shift1Relevant),
                    ntree = 10000)

set.seed(71)
rf.kinematic.shift2 = randomForest(kinematic.vars3,
                    data = fricEma[fricEma$phase == 'shift_3',],
                    importance = T, mtry = length(kinematic.shift2Relevant),
                    ntree = 10000)
set.seed(71)
rf.kinematic.shift3 = randomForest(kinematic.vars4,
                    data = fricEma[fricEma$phase == 'shift_4',],
                    importance = T, mtry = length(kinematic.shift3Relevant),
                    ntree = 10000)

set.seed(71)
rf.kinematic.post = randomForest(kinematic.vars5,
                    data = fricEma[fricEma$phase == 'post_5',],
                    importance = T, mtry = length(kinematic.postRelevant),
                    ntree = 10000)

# compile the data frame for kinematics
kinematic.accuracies = matrix(c("Baseline", "Shift 1", "Shift 2", "Shift 3", "Post",
              1 - (rf.kinematic.base$confusion[5] + rf.kinematic.base$confusion[6])/2,
              1 - (rf.kinematic.shift1$confusion[5] + rf.kinematic.shift1$confusion[6])/2,
              1 - (rf.kinematic.shift2$confusion[5] + rf.kinematic.shift2$confusion[6])/2,
              1 - (rf.kinematic.shift3$confusion[5] + rf.kinematic.shift3$confusion[6])/2,
              1 - (rf.kinematic.post$confusion[5] + rf.kinematic.post$confusion[6])/2),
            ncol = 2, byrow = F)

colnames(kinematic.accuracies) = c("phase","accuracy")
kinematic.accuracies = as.data.frame(kinematic.accuracies)

kinematic.accuracies$accuracy = as.numeric(as.character(kinematic.accuracies$accuracy))
kinematic.accuracies$phase = factor(kinematic.accuracies$phase, levels=c("Baseline", "Shift 1", "Shift 2", "Shift 3", "Post"))

# bar plot showing all prediction accuracy scores for kinematics (Figure 5.5)
(p = ggplot(data=kinematic.accuracies, aes(x=phase, y=accuracy))
  + geom_bar(stat="identity", width=0.7, fill = "#B0AFAF")
  + geom_text(aes(label=round(accuracy*100, digits =2)), vjust=-.7, size=5)
  + scale_y_continuous(breaks=c(0.45, 0.5, 0.55, 0.65, 0.75, 0.85),
                       labels=c('45', '50', '55', '65', '75', '85'))
  + geom_hline(yintercept = 0.5, linetype = 2)
  + theme_bw()
  + theme(text = element_text(size=18),
          axis.text = element_text(size=16),
          axis.title = element_text(size=18, vjust=-1),
          legend.text = element_text(size=18),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  + theme(legend.position = "bottom",
          legend.text=element_text(size=16))
  + guides(linetype=guide_legend(override.aes=list(fill=NA)))
  + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  + labs(x = "Phase", y = "Prediction accuracy (%)")
  + coord_cartesian(ylim=c(0.45, 0.85)))


## get variable importance scores for all RF models
summary(acoustic.base$ImpHistory)
summary(acoustic.shift1$ImpHistory)
summary(acoustic.shift2$ImpHistory)
summary(acoustic.shift3$ImpHistory)
summary(acoustic.post$ImpHistory)

summary(kinematic.base$ImpHistory)
summary(kinematic.shift1$ImpHistory)
summary(kinematic.shift2$ImpHistory)
summary(kinematic.shift3$ImpHistory)
summary(kinematic.post$ImpHistory)

####################################################
# Induvidual importance scores for shift 3 phase
####################################################

subjectList = levels(fricEma$subject)

# acoustic scores for Table 5.7

saveDirBoruta = "C:/individual_boruta_chap5/acoustics/"
dir.create(saveDirBoruta)

for (i in 1:length(subjectList)){
  
  set.seed(71)
  pert.bor_temp = Boruta(fricEma[fricEma$subject == subjectList[i] &
                    fricEma$phase %in% c('shift_4'),
                    which(names(fricEma) %in% aParameter)],
                    fricEma[fricEma$subject == subjectList[i] &
                    fricEma$phase  %in% c('shift_4'),]$repetition, doTrace = 2, maxRuns=500)
  
  pert.bor_temp_relevant = getSelectedAttributes(pert.bor_temp,
                                                 withTentative = FALSE)
  
  # save the Boruta plot
  png(filename = paste(saveDirBoruta, subjectList[i],".png", sep = "",
                       collapse = NULL))
  par(mar=c(12,5,1,1))
  plot(pert.bor_temp, las = 3)
  dev.off()
  
  # save Boruta results to a data.frame
  bor_data_temp = as.data.frame(pert.bor_temp$ImpHistory)
  
  # reshape and compute means of the table
  bor_data_temp = melt(apply(bor_data_temp, 2, mean),
                       value.name = "boruta_score")
  
  # rename columns
  bor_data_temp = setNames(cbind(rownames(bor_data_temp),
                    bor_data_temp, row.names = NULL),
                    c("parameter", "boruta_score"))
  
  #sort the parameters by score and create new index numbers
  bor_data_temp = bor_data_temp[order(-bor_data_temp$boruta_score), c(1,2)]
  rownames(bor_data_temp) = NULL
  
  #save as table for individual subject
  assign(paste("boruta_table_", subjectList[i], "_acoustics", sep = ""), bor_data_temp)
}

# check the results for each speaker in the corresponding table and visually
# in "C:/individual_boruta_chap5/acoustics/"
boruta_table_f1_acoustics
boruta_table_f2_acoustics

# kinematics scores for Tables 5.8, 5.9, and 5.10

saveDirBoruta = "C:/individual_boruta_chap5/kinematics/"
dir.create(saveDirBoruta)

for (i in 1:length(subjectList)){
  
  set.seed(71)
  pert.bor_temp = Boruta(fricEma[fricEma$subject == subjectList[i] &
                    fricEma$phase %in% c('shift_4'),
                    which(names(fricEma) %in% kParameter)],
                    fricEma[fricEma$subject == subjectList[i] &
                    fricEma$phase  %in% c('shift_4'),]$repetition,
                    doTrace = 2, maxRuns=500)
  
  pert.bor_temp_relevant = getSelectedAttributes(pert.bor_temp,
                                                 withTentative = FALSE)
  
  # save the Boruta plot
  png(filename = paste(saveDirBoruta, subjectList[i],"_kinematics.png", sep = "",
                       collapse = NULL))
  par(mar=c(12,5,1,1))
  plot(pert.bor_temp, las = 3)
  dev.off()
  
  # save Boruta results to a data.frame
  bor_data_temp = as.data.frame(pert.bor_temp$ImpHistory)
  
  # reshape and compute means of the table
  bor_data_temp = melt(apply(bor_data_temp, 2, mean),
                       value.name = "boruta_score")
  
  # rename columns
  bor_data_temp = setNames(cbind(rownames(bor_data_temp),
                    bor_data_temp, row.names = NULL),
                    c("parameter", "boruta_score"))
  
  #sort the parameters by score and create new index numbers
  bor_data_temp = bor_data_temp[order(-bor_data_temp$boruta_score), c(1,2)]
  rownames(bor_data_temp) = NULL
  
  #save as table for individual subject
  assign(paste("boruta_table_", subjectList[i], "_kinematics", sep = ""), bor_data_temp)
}

# check the results for each speaker in the corresponding table and visually
# in "C:/individual_boruta_chap5/kinematics/"
boruta_table_f1_kinematics
boruta_table_f2_kinematics

####################################################
# GAMM modeling of compensatory productions (Section 5.3.5)
####################################################

# acoustic models for a_midPearFr (FreqMid), a_diffMidLowRMS (LevelDMidLow),
# a_diffMidLowMinAmp (AmpDMid-MinLow), a_rmsLow (LevelLow) 

midPeakFrm = bam(a_midPeakFr ~ repetition +
                       s(trial, by = repetition, k = 10) +
                       s(trial, subject, by = repetition, bs = 'fs', m=1),
                     data = fricEma, method = "ML")

diffMidLowRMSm = bam(a_diffMidLowRMS ~ repetition +
                       s(trial, by = repetition, k = 10) +
                       s(trial, subject, by = repetition, bs = 'fs', m=1),
                        data = fricEma, method = "ML")

diffMidLowMinAmpm = bam(a_diffMidLowMinAmp ~ repetition +
                       s(trial, by = repetition, k = 10) +
                       s(trial, subject, by = repetition, bs = 'fs', m=1),
                     data = fricEma, method = "ML")

LowRMSm = bam(a_rmsLow ~ repetition +
                       s(trial, by = repetition, k = 10) +
                       s(trial, subject, by = repetition, bs = 'fs', m=1),
                     data = fricEma, method = "ML")

# kinematic models for k_LL_z, k_TT_z, k_TT_y, k_JAW_z, k_TT_x, k_TB_x, k_UL_y
LL_Z_m = bam(k_LL_z ~ repetition +
                       s(trial, by = repetition, k = 10) +
                       s(trial, subject, by = repetition, bs = 'fs', m=1),
                     data = fricEma, method = "ML")

TT_Z_m = bam(k_TT_z ~ repetition +
                       s(trial, by = repetition, k = 10) +
                       s(trial, subject, by = repetition, bs = 'fs', m=1),
                     data = fricEma, method = "ML")

TT_Y_m = bam(k_TT_y ~ repetition +
                       s(trial, by = repetition, k = 10) +
                       s(trial, subject, by = repetition, bs = 'fs', m=1),
                     data = fricEma, method = "ML")

JAW_Z_m = bam(k_JAW_z ~ repetition +
                       s(trial, by = repetition, k = 10) +
                       s(trial, subject, by = repetition, bs = 'fs', m=1),
                     data = fricEma, method = "ML")

TT_X_m = bam(k_TT_x ~ repetition +
                       s(trial, by = repetition, k = 10) +
                       s(trial, subject, by = repetition, bs = 'fs', m=1),
                     data = fricEma, method = "ML")

TB_X_m = bam(k_TB_x ~ repetition +
                       s(trial, by = repetition, k = 10) +
                       s(trial, subject, by = repetition, bs = 'fs', m=1),
                     data = fricEma, method = "ML")

UL_Y_m = bam(k_UL_y ~ repetition +
                       s(trial, by = repetition, k = 10) +
                       s(trial, subject, by = repetition, bs = 'fs', m=1),
                     data = fricEma, method = "ML")

# visualize models (Figures 5.6 and 5.7)

# define colors and subject lists
colorSet = c('turquoise', 'mediumseagreen', 'red3', 'royalblue', 'orange1',
             'brown', 'violetred2', 'yellow3', 'turquoise3', 'seagreen2',
             'red1', 'navyblue', 'orange4', 'brown2', 'violetred', 'yellow4',
             'skyblue', 'darkred', 'magenta3')

# subjects for acoustic parameters (f2 has no important parameters)
subjectListDiffMidLowRMS = subjectList[subjectList %in% c('f5', 'f7', 'f10', 'f11', 'f13', 'f14')]
subjectListDiffMidLowMinAmp = subjectList[subjectList %in% c('f1', 'f4', 'f8', 'f16')]
subjectListFreqMid = subjectList[subjectList %in% c('f3', 'f6', 'f9', 'f12', 'f15', 'f17')]
subjectListLowRMS = subjectList[subjectList %in% c('f18')]

par(oma=c(10,5,1,1), mar=c(2,2,5,2), mfrow=c(1,4))

plot_smooth(diffMidLowRMSm, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListDiffMidLowRMS[1]),
            plot_all = c("repetition", "subject"),
            main = expression("LevelD"[Mid-Low]*" (dB) (n = 6)"),  xlab = '',
            ylab = "", ylim = c(-3.5, 4.5), cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[1],
            lty = c("dotdash", "solid"), lwd = 2)

abline(v = 32, lty = 2)
abline(v = 64, lty = 2)
abline(v = 96, lty = 2)
abline(v = 128, lty = 2)
abline(v = 160, lty = 2)
text(x = 16, y = 4.5, labels = "Base", cex = 1.5)
text(x = 48, y = 4.5, labels = "Shift 1", cex = 1.5)
text(x = 80, y = 4.5, labels = "Shift 2", cex = 1.5) 
text(x = 112, y = 4.5, labels = "Shift 3", cex = 1.5)
text(x = 144, y = 4.5, labels = "Post", cex = 1.5)

for (i in 2:length(subjectListDiffMidLowRMS)){
  plot_smooth(diffMidLowRMSm, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListDiffMidLowRMS[i]),
            plot_all = c("repetition", "subject"),
            main = '',  xlab = '',
            ylab = '', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[i],
            lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(diffMidLowMinAmpm, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListDiffMidLowMinAmp[1]),
            plot_all = c("repetition", "subject"),
            main = expression("AmpD"[Mid-MinLow]*" (dB) (n = 4)"),  xlab = '',
            ylim = c(-7, 3.5), ylab = '', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[5],
            lty = c("dotdash", "solid"), lwd = 2)

abline(v = 32, lty = 2)
abline(v = 64, lty = 2)
abline(v = 96, lty = 2)
abline(v = 128, lty = 2)
abline(v = 160, lty = 2)
text(x = 16, y = 3.5, labels = "Base", cex = 1.5)
text(x = 48, y = 3.5, labels = "Shift 1", cex = 1.5)
text(x = 80, y = 3.5, labels = "Shift 2", cex = 1.5) 
text(x = 112, y = 3.5, labels = "Shift 3", cex = 1.5)
text(x = 144, y = 3.5, labels = "Post", cex = 1.5)
mtext("A", adj=-1.35, line=1, cex=1.5) 
mtext("C", adj=1.2, line=1, cex=1.5)

for (i in 2:length(subjectListDiffMidLowMinAmp)){
  plot_smooth(diffMidLowMinAmpm, view = "trial", rug = F,
              shade = T, legend_plot_all = F, se = F,
              cond = list(subject= subjectListDiffMidLowMinAmp[i]),
              plot_all = c("repetition", "subject"),
              main = '',  xlab = '',
              ylab = '', cex.lab=2,
              cex.axis=2, cex.main=2, cex.sub=2,
              rm.ranef = F, col = colorSet[i+4],
              lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(midPeakFrm, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListFreqMid[1]),
            plot_all = c("repetition", "subject"),
            main = expression("Freq"[Mid]*" (Hz) (n = 6)"),  xlab = '',
            ylab = "", ylim = c(-1400, 600), cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[1],
            lty = c("dotdash", "solid"), lwd = 2)

abline(v = 32, lty = 2)
abline(v = 64, lty = 2)
abline(v = 96, lty = 2)
abline(v = 128, lty = 2)
abline(v = 160, lty = 2)
text(x = 16, y = 600, labels = "Base", cex = 1.5)
text(x = 48, y = 600, labels = "Shift 1", cex = 1.5)
text(x = 80, y = 600, labels = "Shift 2", cex = 1.5) 
text(x = 112, y = 600, labels = "Shift 3", cex = 1.5)
text(x = 144, y = 600, labels = "Post", cex = 1.5)
mtext("B", adj=-1.35, line=1, cex=1.5) 
mtext("D", adj=1.2, line=1, cex=1.5)

for (i in 2:length(subjectListFreqMid)){
  plot_smooth(midPeakFrm, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListFreqMid[i]),
            plot_all = c("repetition", "subject"),
            main = '',  xlab = '',
            ylab = '', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[i],
            lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(LowRMSm, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListLowRMS[1]),
            plot_all = c("repetition", "subject"),
            main = expression("Level"[Low]*" (dB) (n = 1)"),  xlab = '',
            ylab = "", ylim = c(-10.5, 0.5), cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[1],
            lty = c("dotdash", "solid"), lwd = 2)

abline(v = 32, lty = 2)
abline(v = 64, lty = 2)
abline(v = 96, lty = 2)
abline(v = 128, lty = 2)
abline(v = 160, lty = 2)
text(x = 16, y = 0.5, labels = "Base", cex = 1.5)
text(x = 48, y = 0.5, labels = "Shift 1", cex = 1.5)
text(x = 80, y = 0.5, labels = "Shift 2", cex = 1.5) 
text(x = 112, y = 0.5, labels = "Shift 3", cex = 1.5)
text(x = 144, y = 0.5, labels = "Post", cex = 1.5)

mtext('Trial', side = 1, outer = TRUE, line = 2, cex = 1.5)
mtext('Normalized parameter', side = 2, outer = TRUE, line = 2, cex = 1.5)

# reset the plotting area
par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)

legend("bottom", title="Perturbation condition", cex=1.3, pt.cex=1,
       legend=c("unperturbed", "perturbed"), lty =  c("solid", "dotdash"), lwd = 2,
       ncol=2, xpd=NA, bty="n")

# subject lists for kinematic parameters
subjectListLL_Z_m  = subjectList[subjectList %in% c('f2', 'f6', 'f8', 'f15', 'f16')]
subjectListJAW_Z_m = subjectList[subjectList %in% c('f7', 'f9', 'f13', 'f14')]
subjectListTT_Y_m  = subjectList[subjectList %in% c('f3', 'f10', 'f12', 'f18')]
subjectListTT_Z_m  = subjectList[subjectList %in% c('f4', 'f17')]

# subjectListTT_X_m  = subjectList[subjectList %in% c('f5')]
# subjectListTB_X_m  = subjectList[subjectList %in% c('f11')]
# subjectListUL_Y_m  = subjectList[subjectList %in% c('f1')]

par(oma=c(10,5,1,1), mar=c(2,2,5,2), mfrow=c(1,4))

plot_smooth(LL_Z_m, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListLL_Z_m[1]),
            plot_all = c("repetition", "subject"),
            main = expression("LL"[Z]*" (n = 5)"),  xlab = '',
            ylab = "", ylim = c(-1.8, 0.7), cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[1],
            lty = c("dotdash", "solid"), lwd = 2)

abline(v = 32, lty = 2)
abline(v = 64, lty = 2)
abline(v = 96, lty = 2)
abline(v = 128, lty = 2)
abline(v = 160, lty = 2)
text(x = 16, y = 0.7, labels = "Base", cex = 1.5)
text(x = 48, y = 0.7, labels = "Shift 1", cex = 1.5)
text(x = 80, y = 0.7, labels = "Shift 2", cex = 1.5) 
text(x = 112, y = 0.7, labels = "Shift 3", cex = 1.5)
text(x = 144, y = 0.7, labels = "Post", cex = 1.5)

for (i in 2:length(subjectListLL_Z_m)){
  plot_smooth(LL_Z_m, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListLL_Z_m[i]),
            plot_all = c("repetition", "subject"),
            main = '',  xlab = '',
            ylab = '', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[i],
            lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(JAW_Z_m, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListJAW_Z_m[1]),
            plot_all = c("repetition", "subject"),
            main = expression("JAW"[Z]*" (n = 4)"),  xlab = '',
            ylim = c(-1.6, 0.7), ylab = '', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[5],
            lty = c("dotdash", "solid"), lwd = 2)

abline(v = 32, lty = 2)
abline(v = 64, lty = 2)
abline(v = 96, lty = 2)
abline(v = 128, lty = 2)
abline(v = 160, lty = 2)
text(x = 16, y = 0.7, labels = "Base", cex = 1.5)
text(x = 48, y = 0.7, labels = "Shift 1", cex = 1.5)
text(x = 80, y = 0.7, labels = "Shift 2", cex = 1.5) 
text(x = 112, y = 0.7, labels = "Shift 3", cex = 1.5)
text(x = 144, y = 0.7, labels = "Post", cex = 1.5)
mtext("A", adj=-1.35, line=1, cex=1.5) 
mtext("C", adj=1.2, line=1, cex=1.5)

for (i in 2:length(subjectListJAW_Z_m)){
  plot_smooth(JAW_Z_m, view = "trial", rug = F,
              shade = T, legend_plot_all = F, se = F,
              cond = list(subject= subjectListJAW_Z_m[i]),
              plot_all = c("repetition", "subject"),
              main = '',  xlab = '',
              ylab = '', cex.lab=2,
              cex.axis=2, cex.main=2, cex.sub=2,
              rm.ranef = F, col = colorSet[i+4],
              lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(TT_Y_m, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListTT_Y_m[1]),
            plot_all = c("repetition", "subject"),
            main = expression("TT"[Y]*" (n = 4)"),  xlab = '',
            ylab = "", ylim = c(-1, 3.8), cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[1],
            lty = c("dotdash", "solid"), lwd = 2)

abline(v = 32, lty = 2)
abline(v = 64, lty = 2)
abline(v = 96, lty = 2)
abline(v = 128, lty = 2)
abline(v = 160, lty = 2)
text(x = 16, y = 3.8, labels = "Base", cex = 1.5)
text(x = 48, y = 3.8, labels = "Shift 1", cex = 1.5)
text(x = 80, y = 3.8, labels = "Shift 2", cex = 1.5) 
text(x = 112, y = 3.8, labels = "Shift 3", cex = 1.5)
text(x = 144, y = 3.8, labels = "Post", cex = 1.5)
mtext("B", adj=-1.35, line=1, cex=1.5) 
mtext("D", adj=1.2, line=1, cex=1.5)

for (i in 2:length(subjectListTT_Y_m)){
  plot_smooth(TT_Y_m, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListTT_Y_m[i]),
            plot_all = c("repetition", "subject"),
            main = '',  xlab = '',
            ylab = '', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[i],
            lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(TT_Z_m, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListTT_Z_m[1]),
            plot_all = c("repetition", "subject"),
            main = expression("TT"[Z]*" (n = 2)"),  xlab = '',
            ylab = "", ylim = c(-1.7, 0.7), cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[1],
            lty = c("dotdash", "solid"), lwd = 2)

for (i in 2:length(subjectListTT_Z_m)){
  plot_smooth(TT_Z_m, view = "trial", rug = F,
            shade = T, legend_plot_all = F, se = F,
            cond = list(subject= subjectListTT_Z_m[i]),
            plot_all = c("repetition", "subject"),
            main = '',  xlab = '',
            ylab = '', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = F, col = colorSet[i],
            lty = c("dotdash", "solid"), lwd = 2, add = T)
}
abline(v = 32, lty = 2)
abline(v = 64, lty = 2)
abline(v = 96, lty = 2)
abline(v = 128, lty = 2)
abline(v = 160, lty = 2)
text(x = 16, y = 0.7, labels = "Base", cex = 1.5)
text(x = 48, y = 0.7, labels = "Shift 1", cex = 1.5)
text(x = 80, y = 0.7, labels = "Shift 2", cex = 1.5) 
text(x = 112, y = 0.7, labels = "Shift 3", cex = 1.5)
text(x = 144, y = 0.7, labels = "Post", cex = 1.5)

mtext('Trial', side = 1, outer = TRUE, line = 2, cex = 1.5)
mtext('Normalized parameter (mm)', side = 2, outer = TRUE, line = 2, cex = 1.5)

# reset the plotting area
par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)

legend("bottom", title="Perturbation condition", cex=1.3, pt.cex=1,
       legend=c("unperturbed", "perturbed"), lty =  c("solid", "dotdash"), lwd = 2,
       ncol=2, xpd=NA, bty="n")

# Tables 5.11 and 5.12 
GAMMmeanCoeffAcoustic
GAMMdiffCoeffAcoustic

GAMMmeanCoeffArticulatory
GAMMdiffCoeffArticulatory
