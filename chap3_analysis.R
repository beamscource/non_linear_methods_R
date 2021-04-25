# Eugen Klein: Relation between acoustic and
# articulatory dimensions of speech sounds, 2020 
# chapter 3 analysis

# employed libraries

library(MASS)
library(lmerTest)
library(Boruta)
library(randomForest)
library(ggplot2)
library(reshape2)
library(mgcv)
library(itsadug)


####################################################
# to get baseline averages for the three sounds (Table 3.5)
####################################################

# CoG_mean, SD_mean, skew_mean, midPeakFr_mean,
# diffMidLowMinAmp_mean, rmsLow, rmsMid_mean,
# rmsHigh_mean, diffHighMidAmp_mean, diffHighMidRMS_mean,
# diffMidLowRMS_mean, F1_mean, F2_mean

# for male speakers
tapply(baselineMeans[baselineMeans$gender == 'male' &
            baselineMeans$valid == '1',]$CoG_mean, # change acoustic parameter here
            list(baselineMeans[baselineMeans$gender == 'male',]$sound_category,
            droplevels(baselineMeans[baselineMeans$gender == 'male',]$subject)), mean)

# for female speakers
tapply(baselineMeans[baselineMeans$gender == 'female' &
            baselineMeans$valid == '1',]$CoG_mean, # change acoustic parameter here
            list(baselineMeans[baselineMeans$gender == 'female',]$sound_category,
            droplevels(baselineMeans[baselineMeans$gender == 'female',]$subject)), mean)

####################################################
# linear mixed modeling of the baseline productions (Section 3.3.1)
####################################################

# models for F1, F2, and COG for the three sound
# categories [s], [sj], [Sj]

# define sliding contrasts
contrasts(baselineMeans$sound_category) = contr.sdif(3)

# models with random intercepts by speaker
mBaseCOG = lmer(CoG_mean ~ sound_category  + gender:sound_category +
                  (1+sound_category|subject), data = baselineMeans, REML = T)
summary(mBaseCOG)

mBaseF1 = lmer(F1_mean ~ sound_category + gender:sound_category +
                 (1+sound_category|subject), data = baselineMeans, REML = T)
summary(mBaseF1)

mBaseF2 = lmer(F2_mean ~ sound_category + gender:sound_category +
                 (1+sound_category|subject), data = baselineMeans, REML = T)
summary(mBaseF2)

####################################################
# Random forest modeling of baseline productions (Section 3.3.1)
####################################################

# identify relevant parameters (Table 3.6)
set.seed(71)
pert.bor_s_category = Boruta(baselineMeans[baselineMeans$phase %in%
                      c('baseline') & baselineMeans$valid == '1',
                      which(names(baselineMeans) %in%
                      c("CoG_mean", "SD_mean", "skew_mean", "midPeakFr_mean",
                        "diffMidLowMinAmp_mean", "diffHighMidAmp_mean",
                        "rmsLow_mean", "rmsMid_mean", "rmsHigh_mean",
                        "diffHighMidRMS_mean", "diffMidLowRMS_mean",
                        "F1_mean", "F2_mean"))],
                      baselineMeans[baselineMeans$phase  %in% c('baseline') &
                      baselineMeans$valid == '1',]$sound_category, doTrace = 2,
                      maxRuns=500)

pert.bor_s_categoryRelevant = getSelectedAttributes(pert.bor_s_category,
                                                    withTentative = FALSE)

par(mar=c(12,5,1,1))
plot(pert.bor_s_category, las = 3)

# define the RF model
topThreeParam = c("F1_mean", "F2_mean", "CoG_mean")
vars_sound_category = as.formula(paste('as.factor(sound_category) ~',
	paste(topThreeParam, collapse = " + "), sep = ""))

# predict the sound category [s] vs [sj] vs [Sj]
set.seed(71)
pert_phase0_sound_category.rf = randomForest(vars_sound_category,
    data = baselineMeans[baselineMeans$phase == 'baseline' &
    baselineMeans$valid == '1',], importance = T, mtry = 3,
    ntree = 10000)

print(pert_phase0_sound_category.rf)

####################################################
# Random forest modeling of compensatory productions
####################################################

# identify relevant parameters for each phase (baseline, shift phases,
# and noise phases): Boruta computations for Tables 3.7 and 3.9

acousticParameters = c("CoG_normmean", "SD_normmean", "skew_normmean",
                       "midPeakFr_normmean", "diffMidLowMinAmp_normmean",
                       "diffHighMidAmp_normmean", "rmsLow_normmean",
                       "rmsMid_normmean", "rmsHigh_normmean",
                       "diffHighMidRMS_normmean", "diffMidLowRMS_normmean",
                       "F1_normmean", "F2_normmean")

# model 0: baseline phase
set.seed(71)
pert.bor0 = Boruta(phaseMeans[phaseMeans$phase %in% c('baseline') &
                  phaseMeans$valid == '1', which(names(phaseMeans) %in%
                  acousticParameters)], phaseMeans[phaseMeans$phase %in%
                  c('baseline') & phaseMeans$valid == '1',]$pert_direct,
                  doTrace = 2, maxRuns=500)

pert.bor0Relevant = getSelectedAttributes(pert.bor0, withTentative = FALSE)

# model 1: 1st shift phase
set.seed(71)
pert.bor1 = Boruta(phaseMeans[phaseMeans$phase %in% c('shift_2') &
                  phaseMeans$valid == '1', which(names(phaseMeans) %in%
                  acousticParameters)], phaseMeans[phaseMeans$phase %in%
                  c('shift_2') & phaseMeans$valid == '1',]$pert_direct,
                  doTrace = 2, maxRuns=500)

pert.bor1Relevant = getSelectedAttributes(pert.bor1, withTentative = FALSE)

#model 2: 2nd shift phase
set.seed(71)
pert.bor2 = Boruta(phaseMeans[phaseMeans$phase %in% c('shift_3') &
                  phaseMeans$valid == '1', which(names(phaseMeans) %in%
                  acousticParameters)], phaseMeans[phaseMeans$phase %in%
                  c('shift_3') & phaseMeans$valid == '1',]$pert_direct,
                  doTrace = 2, maxRuns=500)

pert.bor2Relevant = getSelectedAttributes(pert.bor2, withTentative = FALSE)

# model 3: 3rd shift phase
set.seed(71)
pert.bor3 = Boruta(phaseMeans[phaseMeans$phase %in% c('shift_4') &
                  phaseMeans$valid == '1', which(names(phaseMeans) %in%
                  acousticParameters)], phaseMeans[phaseMeans$phase %in%
                  c('shift_4') & phaseMeans$valid == '1',]$pert_direct,
                  doTrace = 2, maxRuns=500)

pert.bor3Relevant = getSelectedAttributes(pert.bor3, withTentative = FALSE)

# model 4: 1st noise phase
set.seed(71)
pert.borN1 = Boruta(phaseMeans[phaseMeans$phase %in% c('shift_2_mask') &
                    phaseMeans$valid == '1', which(names(phaseMeans) %in%
                    acousticParameters)], phaseMeans[phaseMeans$phase %in%
                    c('shift_2_mask') & phaseMeans$valid == '1',]$pert_direct,
                    doTrace = 2, maxRuns=500)

pert.borN1Relevant = getSelectedAttributes(pert.borN1, withTentative = FALSE)

# model 5: 2nd noise phase
set.seed(71)
pert.borN2 = Boruta(phaseMeans[phaseMeans$phase %in% c('shift_3_mask') &
                    phaseMeans$valid == '1', which(names(phaseMeans) %in%
                    acousticParameters)], phaseMeans[phaseMeans$phase %in%
                    c('shift_3_mask') & phaseMeans$valid == '1',]$pert_direct,
                    doTrace = 2, maxRuns=500)

pert.borN2Relevant = getSelectedAttributes(pert.borN2, withTentative = FALSE)

# model 6: 3rd noise phase
set.seed(71)
pert.borN3 = Boruta(phaseMeans[phaseMeans$phase %in% c('shift_4_mask') &
                    phaseMeans$valid == '1', which(names(phaseMeans) %in%
                    acousticParameters)], phaseMeans[phaseMeans$phase %in%
                    c('shift_4_mask') & phaseMeans$valid == '1',]$pert_direct,
                    doTrace = 2, maxRuns=500)

pert.borN3Relevant = getSelectedAttributes(pert.borN3, withTentative = FALSE)

# plot relevant parameters
plot(pert.bor0, las = 3)
plot(pert.bor1, las = 3)
plot(pert.bor2, las = 3)
plot(pert.bor3, las = 3)
plot(pert.borN1, las = 3)
plot(pert.borN2, las = 3)
plot(pert.borN3, las = 3)

# define Random forest models for each phase including only relevant
# parameters (Section 3.3.6)
varsAll = as.formula(paste('pert_direct ~', paste(acousticParameters, collapse = " + "), sep = "")) 

vars0 = as.formula(paste('as.factor(pert_direct) ~', paste(pert.bor0Relevant, collapse = " + "), sep = ""))
vars1 = as.formula(paste('as.factor(pert_direct) ~', paste(pert.bor1Relevant, collapse = " + "), sep = ""))
vars2 = as.formula(paste('as.factor(pert_direct) ~', paste(pert.bor2Relevant, collapse = " + "), sep = ""))
vars3 = as.formula(paste('as.factor(pert_direct) ~', paste(pert.bor3Relevant, collapse = " + "), sep = ""))

varsN1 = as.formula(paste('pert_direct ~', paste(pert.borN1Relevant, collapse=" + "), sep = ""))
varsN2 = as.formula(paste('pert_direct ~', paste(pert.borN2Relevant, collapse=" + "), sep = ""))
varsN3 = as.formula(paste('pert_direct ~', paste(pert.borN3Relevant, collapse=" + "), sep = ""))

# compure RF models
set.seed(71)
pert_phase0.rf = randomForest(varsAll, data = phaseMeans[phaseMeans$phase ==
                              'baseline' & phaseMeans$valid == '1',],
                              importance = T, mtry = length(acousticParameters),
                              ntree = 10000)
set.seed(71)
pert_phase1.rf = randomForest(varsAll, data = phaseMeans[phaseMeans$phase ==
                              'shift_2' & phaseMeans$valid == '1',],
                              importance = T, mtry = length(acousticParameters),
                              ntree = 10000)

set.seed(71)
pert_phase2.rf = randomForest(vars2, data = phaseMeans[phaseMeans$phase ==
                              'shift_3' & phaseMeans$valid == '1',],
                              importance = T, mtry = length(pert.bor2Relevant),
                              ntree = 10000)
set.seed(71)
pert_phase3.rf = randomForest(vars3, data = phaseMeans[phaseMeans$phase ==
                              'shift_4' & phaseMeans$valid == '1',],
                              importance = T, mtry = length(pert.bor3Relevant),
                              ntree = 10000)

set.seed(71)
pert_phaseN1.rf = randomForest(varsN1, data = phaseMeans[phaseMeans$phase ==
                              'shift_2_mask' & phaseMeans$valid == '1',],
                              importance = T, mtry = length(pert.borN1Relevant),
                              ntree = 10000)

set.seed(71)
pert_phaseN2.rf = randomForest(varsN2, data = phaseMeans[phaseMeans$phase ==
                              'shift_3_mask' & phaseMeans$valid == '1',],
                              importance = T, mtry = length(pert.borN2Relevant),
                              ntree = 10000)
set.seed(71)
pert_phaseN3.rf = randomForest(varsN3, data = phaseMeans[phaseMeans$phase ==
                              'shift_4_mask' & phaseMeans$valid == '1',],
                              importance = T, mtry = length(pert.borN3Relevant),
                              ntree = 10000)

# compile the data frame with accuracy scores
accuracies = matrix(c("Baseline", "Shift 1", "Noise 1", "Shift 2", "Noise 2", "Shift 3", "Noise 3",
            1 - (pert_phase0.rf$confusion[5] + pert_phase0.rf$confusion[6])/2,
            1 - (pert_phase1.rf$confusion[5] + pert_phase1.rf$confusion[6])/2,
            1 - (pert_phaseN1.rf$confusion[5] + pert_phaseN1.rf$confusion[6])/2,
            1 - (pert_phase2.rf$confusion[5] + pert_phase2.rf$confusion[6])/2,
            1 - (pert_phaseN2.rf$confusion[5] + pert_phaseN2.rf$confusion[6])/2,
            1 - (pert_phase3.rf$confusion[5] + pert_phase3.rf$confusion[6])/2,
            1 - (pert_phaseN3.rf$confusion[5] + pert_phaseN3.rf$confusion[6])/2,
            "normal", "normal", "noise", "normal", "noise", "normal", "noise"),
          	ncol = 3, byrow = F)

colnames(accuracies) = c("phase","accuracy", "feedback")
accuracies = as.data.frame(accuracies)

accuracies$accuracy = as.numeric(as.character(accuracies$accuracy))
accuracies$feedback = as.factor(accuracies$feedback)
accuracies$feedback = factor(accuracies$feedback, levels=c("normal", "noise"))
accuracies$phase = factor(accuracies$phase, levels=c("Baseline", "Shift 1",
	"Noise 1", "Shift 2", "Noise 2", "Shift 3", "Noise 3"))

# bar plot showing all prediction accuracy scores (Figure 3.7)
(p = ggplot(data=accuracies, aes(x=phase, y=accuracy, fill = feedback))
  + geom_bar(stat="identity", width=0.7)
  + geom_text(aes(label=round(accuracy*100, digits =2)), vjust=-.7, size=5)
  + scale_y_continuous(breaks=c(0.4, 0.5, 0.6, 0.7),
                       labels=c('40', '50', '60', '70'))
  + scale_fill_manual(values=c("#B0AFAF", "#7F7F7F"), labels=c("non-masked", "masked"))
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
  + labs(x = "Phase", y = "Prediction accuracy (%)", fill = "Feedback")
  + coord_cartesian(ylim=c(0.4, 0.7)))


####################################################
# individual variable importance computations for the
# last shift phase (Table 3.10)
####################################################

subjectList = levels(phaseMeans$subject)

saveDirBoruta = "C:/individual_boruta_chap3/"
dir.create(saveDirBoruta)

for (i in 1:length(subjectList)){
  
  set.seed(71)
  pert.bor_temp = Boruta(phaseMeans[phaseMeans$subject == subjectList[i] &
                        phaseMeans$phase %in% c('shift_4') &
                        phaseMeans$valid == '1', which(names(phaseMeans) %in%
                        acousticParameters)], phaseMeans[phaseMeans$subject ==
                        subjectList[i] & phaseMeans$phase  %in% c('shift_4') &
                        phaseMeans$valid == '1',]$pert_direct, doTrace = 2,
                        maxRuns=500)
  
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
  assign(paste("boruta_table_", subjectList[i], sep = ""), bor_data_temp)
}

# check the results for each speaker in the corresponding table and visually
# in "C:/individual_boruta_chap3/"
boruta_table_f1
boruta_table_f2
boruta_table_m1
boruta_table_m2

####################################################
# GAMM modeling of compensatory productions
# (Sections 3.3.2 and 3.3.4)
####################################################

# models for rmsLow (LevelLow), COG, SD, skewness, F1, F2 

rmsLowm = bam(rmsLow_normmean ~ pert_direct + s(ex_trial, by = pert_direct,
           k = 10) + s(ex_trial, subject, by = pert_direct, bs = 'fs', m=1),
           data = phaseMeansNoNoise[phaseMeansNoNoise$valid == '1',],
           method = "ML")

COGm = bam(CoG_normmean ~ pert_direct + s(ex_trial, by = pert_direct, k = 10) +
           s(ex_trial, subject, by = pert_direct, bs = 'fs', m=1),
           data = phaseMeansNoNoise[phaseMeansNoNoise$valid == '1',],
           method = "ML")

SDm = bam(SD_normmean ~ pert_direct + s(ex_trial, by = pert_direct, k = 10) +
           s(ex_trial, subject, by = pert_direct, bs = 'fs', m=1),
           data = phaseMeansNoNoise[phaseMeansNoNoise$valid == '1',],
           method = "ML")

skewm = bam(skew_normmean ~ pert_direct + s(ex_trial, by = pert_direct,
           k = 10) + s(ex_trial, subject, by = pert_direct, bs = 'fs', m=1),
           data = phaseMeansNoNoise[phaseMeansNoNoise$valid == '1',],
           method = "ML")

F1m = bam(F1_normmean ~ pert_direct + s(ex_trial, by = pert_direct, k = 10) +
           s(ex_trial, subject, by = pert_direct, bs = 'fs', m=1),
           data = phaseMeansNoNoise[phaseMeansNoNoise$valid == '1' &
           phaseMeansNoNoise$subject != 'm5',], method = "ML")

F2m = bam(F2_normmean ~ pert_direct + s(ex_trial, by = pert_direct, k = 10) +
           s(ex_trial, subject, by = pert_direct, bs = 'fs', m=1),
           data = phaseMeansNoNoise[phaseMeansNoNoise$valid == '1' &
           phaseMeansNoNoise$subject != 'f6',], method = "ML")

diffMidHighLowm = bam(diffMidHighLowRMS_normmean ~ pert_direct + s(ex_trial,
                      by = pert_direct, k = 10) + s(ex_trial, subject,
                      by = pert_direct, bs = 'fs', m=1),
                      data = phaseMeansNoNoise[phaseMeansNoNoise$valid == '1',],
                      method = "ML")


# visualize models (Figure 3.5 and 3.6)

# define colors and subject lists
colorSet = c('turquoise', 'mediumseagreen', 'red3', 'royalblue', 'orange1',
             'brown', 'violetred2', 'yellow3', 'turquoise3', 'seagreen2',
             'red1', 'navyblue', 'orange4', 'brown2', 'violetred', 'yellow4',
             'skyblue', 'darkred', 'magenta3')

subjectList = levels(phaseMeansNoNoise$subject)
subjectListEdited = c("f1", "f2", "f3", "m1", "f4", "f5",
                      "f6", "f7", "m2", "f8", "f9", "f10",
                      "f11", "m3", "f12", "f13", "m4", "f14")   
subjectListEdited2 = c("f1", "f2", "f3", "m1", "f4", "f5",
                       "f7", "m2", "f8", "f9", "f10", "f11",
                       "m3", "f12", "f13", "m4", "m5", "f14")   

# set up canvas
par(oma=c(10,5,1,1), mar=c(2,2,5,2), mfrow=c(2,3))

plot_smooth(F1m, view = "ex_trial", rug = F,
            shade = T, legend_plot_all = F, plot_all = c("pert_direct"),
            main = expression("F1 response"),  xlab = 'Trial',
            ylab = 'Normalized F1 (Hz)', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = T,
            lty = c("dotdash", "solid"), lwd = 2, col = "black")

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 14, labels = "Base", cex = 1.5)
text(x = 65, y = 14, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 14, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 14, labels = "Shift 3", cex = 1.5)

plot_smooth(F2m, view = "ex_trial", rug = F,
            shade = T, legend_plot_all = F, plot_all = c("pert_direct"),
            main = expression("F2 response"),  xlab = 'Trial',
            ylab = 'Normalized F1 (Hz)', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = T,
            lty = c("dotdash", "solid"), lwd = 2, col = "black")

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 80, labels = "Base", cex = 1.5)
text(x = 65, y = 80, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 80, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 80, labels = "Shift 3", cex = 1.5)
mtext("A", adj=-1.35, line=1, cex=1.5) 
mtext("C", adj=1.2, line=1, cex=1.5) 

plot_smooth(COGm, view = "ex_trial", rug = F,
            shade = T, legend_plot_all = NA, plot_all = c("pert_direct"),
            main = expression("COG response"),  xlab = 'Trial',
            ylab = 'Normalized COG (Hz)', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = T,
            lty = c("dotdash", "solid"), lwd = 2, col = "black")

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 410, labels = "Base", cex = 1.5)
text(x = 65, y = 410, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 410, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 410, labels = "Shift 3", cex = 1.5)
mtext("B", adj=-1.35,line=1, cex=1.5)

plot_smooth(F1m, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectListEdited[1]),
            main = expression("Participant smooths"), plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized F1 (Hz)',
            ylim=c(-65, 65), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 65, labels = "Base", cex = 1.5)
text(x = 65, y = 65, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 65, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 65, labels = "Shift 3", cex = 1.5)

for (i in 2:length(subjectListEdited)){
  plot_smooth(F1m, view = "ex_trial", rug = F,
              shade = F, legend_plot_all = F, se = F,
              cond = list(subject= subjectListEdited[i]),
              main = expression("Participant smooths"), plot_all = c("pert_direct", "subject"),
              xlab = 'Trial', ylab = 'Normalized F1 (Hz)',
              ylim=c(-45, 65), cex.lab=2, cex.axis=2, cex.main=2,
              cex.sub=2, col = colorSet[i],
              rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(F2m, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectListEdited2[1]),
            main = expression("Participant smooths"), plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized F2 (Hz)',
            ylim=c(-100, 210), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 210, labels = "Base", cex = 1.5)
text(x = 65, y = 210, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 210, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 210, labels = "Shift 3", cex = 1.5)
mtext("D", adj=-1.35, line=1, cex=1.5) 
mtext("F", adj=1.2, line=1, cex=1.5)

for (i in 2:length(subjectListEdited2)){
  plot_smooth(F2m, view = "ex_trial", rug = F,
              shade = F, legend_plot_all = F, se = F,
              cond = list(subject= subjectListEdited2[i]),
              main = expression("Participant smooths"), plot_all = c("pert_direct", "subject"),
              xlab = 'Trial', ylab = 'Normalized F2 (Hz)',
              ylim=c(-100, 200), cex.lab=2, cex.axis=2, cex.main=2,
              cex.sub=2, col = colorSet[i],
              rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(COGm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectList[1]),
            main = expression("Participant smooths"), plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized COG (Hz)',
            ylim=c(-1600, 1650), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 1600, labels = "Base", cex = 1.5)
text(x = 65, y = 1600, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 1600, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 1600, labels = "Shift 3", cex = 1.5)
mtext("E", adj=-1.35, line=1, cex=1.5) 

for (i in 2:length(subjectList)){
  plot_smooth(COGm, view = "ex_trial", rug = F,
              shade = F, legend_plot_all = F, se = F,
              cond = list(subject= subjectList[i]),
              main = expression("Participant smooths"), plot_all = c("pert_direct", "subject"),
              xlab = 'Trial', ylab = 'Normalized COG (Hz)',
              ylim=c(-1500, 1500), cex.lab=2, cex.axis=2, cex.main=2,
              cex.sub=2, col = colorSet[i],
              rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
}

mtext('Trial', side = 1, outer = TRUE, line = 2, cex = 1.5)
mtext('Normalized parameter (Hz)', side = 2, outer = TRUE, line = 2, cex = 1.5)

# reset the plotting area
par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)

legend("bottom", title="Perturbation direction", cex=1.3, pt.cex=1,
       legend=c("down", "up"), lty =  c("solid", "dotdash"), lwd = 2,
       ncol=2, xpd=NA, bty="n")


# relevant parameters for shift 3 phase
# set up canvas
par(oma=c(10,5,1,1), mar=c(2,2,5,2), mfrow=c(2,3))

plot_smooth(SDm, view = "ex_trial", rug = F,
            shade = T, legend_plot_all = NA, plot_all = c("pert_direct"),
            main = expression("SD response (Hz)"),  xlab = 'Trial',
            ylab = 'Normalized F1 (Hz)', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = T,
            lty = c("dotdash", "solid"), lwd = 2, col = "black")

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 210, labels = "Base", cex = 1.5)
text(x = 65, y = 210, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 210, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 210, labels = "Shift 3", cex = 1.5)

plot_smooth(skewm, view = "ex_trial", rug = F,
            shade = T, legend_plot_all = NA, plot_all = c("pert_direct"),
            main = expression("Skewness response"),  xlab = 'Trial',
            ylab = 'Normalized F1 (Hz)', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = T,
            lty = c("dotdash", "solid"), lwd = 2, col = "black")

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 0.21, labels = "Base", cex = 1.5)
text(x = 65, y = 0.21, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 0.21, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 0.21, labels = "Shift 3", cex = 1.5)
mtext("A", adj=-1.35, line=1, cex=1.5) 
mtext("C", adj=1.2, line=1, cex=1.5) 

plot_smooth(rmsLowm, view = "ex_trial", rug = F,
            shade = T, legend_plot_all = NA, plot_all = c("pert_direct"),
            main = expression("Level"[Low]*" (dB)"),  xlab = 'Trial',
            ylab = 'Normalized COG (Hz)', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = T,
            lty = c("dotdash", "solid"), lwd = 2, col = "black")

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 1.5, labels = "Base", cex = 1.5)
text(x = 65, y = 1.5, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 1.5, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 1.5, labels = "Shift 3", cex = 1.5)
mtext("B", adj=-1.35,line=1, cex=1.5)

plot_smooth(SDm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectList[1]),
            main = expression("Participant smooths"), plot_all = c("pert_direct", "subject"),
            ylim = c(-500, 580), xlab = 'Trial', ylab = 'Normalized F1 (Hz)',
            cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 580, labels = "Base", cex = 1.5)
text(x = 65, y = 580, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 580, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 580, labels = "Shift 3", cex = 1.5)

for (i in 2:length(subjectList)){
  plot_smooth(SDm, view = "ex_trial", rug = F,
              shade = F, legend_plot_all = F, se = F,
              cond = list(subject= subjectList[i]),
              main = expression("Participant smooths"), plot_all = c("pert_direct", "subject"),
              ylim = c(-500, 500), xlab = 'Trial', ylab = 'Normalized F1 (Hz)',
              cex.lab=2, cex.axis=2, cex.main=2,
              cex.sub=2, col = colorSet[i],
              rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(skewm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectList[1]),
            main = expression("Participant smooths"), plot_all = c("pert_direct", "subject"),
            ylim = c(-1.1, 1), xlab = 'Trial', ylab = 'Normalized F2 (Hz)',
            cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 1, labels = "Base", cex = 1.5)
text(x = 65, y = 1, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 1, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 1, labels = "Shift 3", cex = 1.5)
mtext("D", adj=-1.35, line=1, cex=1.5) 
mtext("F", adj=1.2, line=1, cex=1.5)

for (i in 2:length(subjectList)){
  plot_smooth(skewm, view = "ex_trial", rug = F,
              shade = F, legend_plot_all = F, se = F,
              cond = list(subject= subjectList[i]),
              main = expression("Participant smooths"), plot_all = c("pert_direct", "subject"),
              ylim = c(-1.1, 0.8), xlab = 'Trial', ylab = 'Normalized F2 (Hz)',
              cex.lab=2, cex.axis=2, cex.main=2,
              cex.sub=2, col = colorSet[i],
              rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(rmsLowm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectList[1]),
            main = expression("Participant smooths"), plot_all = c("pert_direct", "subject"),
            ylim = c(-6.5, 6), xlab = 'Trial', ylab = 'Normalized COG (Hz)',
            cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 5.9, labels = "Base", cex = 1.5)
text(x = 65, y = 5.9, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 5.9, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 5.9, labels = "Shift 3", cex = 1.5)
mtext("E", adj=-1.35, line=1, cex=1.5)

for (i in   2:length(subjectList)){
  plot_smooth(rmsLowm, view = "ex_trial", rug = F,
              shade = F, legend_plot_all = F, se = F,
              cond = list(subject= subjectList[i]),
              main = expression("Participant smooths"), plot_all = c("pert_direct", "subject"),
              ylim = c(-6, 6), xlab = 'Trial', ylab = 'Normalized COG (Hz)',
              cex.lab=2, cex.axis=2, cex.main=2,
              cex.sub=2, col = colorSet[i],
              rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
} 

mtext('Trial', side = 1, outer = TRUE, line = 2, cex = 1.5)
mtext('Normalized parameter', side = 2, outer = TRUE, line = 2, cex = 1.5)

# reset the plotting area
par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)

legend("bottom", title="Perturbation direction", cex=1.3, pt.cex=1,
       legend=c("down", "up"), lty =  c("solid", "dotdash"), lwd = 2,
       ncol=2, xpd=NA, bty="n")


####################################################
# GAMM visualization for individually relevant parameters
# (Figure 3.8)
####################################################

# subject groups
subjectsLevelLow = c('f1', 'f5', 'f7', 'f8', 'f12', 'f14', 'm3', 'm4')
subjectsCoG = c('f4', 'f9', 'f10', 'f13', 'm5')
subjectsSD = c('f3', 'f11', 'm1')
subjectsLevelDMidLow = c('f2', 'f6')

colorSet = c('turquoise', 'mediumseagreen', 'red3', 'royalblue', 'orange1',
             'brown', 'violetred2', 'yellow3') 

# set up canvas
par(oma=c(10,5,1,1), mar=c(2,2,5,2), mfrow=c(1,4))

plot_smooth(rmsLowm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectsLevelLow[1]),
            main = expression("Level"[Low]*" (dB) (n = 8)"),
            plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized F1 (Hz)',
            ylim=c(-5.5, 5.5), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 5.5, labels = "Base", cex = 1.5)
text(x = 65, y = 5.5, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 5.5, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 5.5, labels = "Shift 3", cex = 1.5)

for (i in 2:length(subjectsLevelLow)){
  plot_smooth(rmsLowm, view = "ex_trial", rug = F,
              shade = F, legend_plot_all = F, se = F,
              cond = list(subject= subjectsLevelLow[i]),
              main = expression("Level"[Low]*" (dB)"),
              plot_all = c("pert_direct", "subject"),
              xlab = 'Trial', ylab = 'Normalized F1 (Hz)',
              cex.lab=2, cex.axis=2, cex.main=2,
              cex.sub=2, col = colorSet[i],
              rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(COGm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectsCoG[1]),
            main = expression("COG (Hz) (n = 5)"),
            plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized F2 (Hz)',
            ylim=c(-800, 1200), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 1200, labels = "Base", cex = 1.5)
text(x = 65, y = 1200, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 1200, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 1200, labels = "Shift 3", cex = 1.5)
mtext("A", adj=-1.35, line=1, cex=1.5) 
mtext("C", adj=1.2, line=1, cex=1.5)

for (i in 2:length(subjectsCoG)){
  plot_smooth(COGm, view = "ex_trial", rug = F,
              shade = F, legend_plot_all = F, se = F,
              cond = list(subject= subjectsCoG[i]),
              main = expression("COG"),
              plot_all = c("pert_direct", "subject"),
              xlab = 'Trial', ylab = 'Normalized F2 (Hz)',
              cex.lab=2, cex.axis=2, cex.main=2,
              cex.sub=2, col = colorSet[i],
              rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(SDm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectsSD[1]),
            main = expression("SD (Hz) (n = 3)"),
            plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized COG (Hz)',
            ylim=c(-150, 450), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 450, labels = "Base", cex = 1.5)
text(x = 65, y = 450, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 450, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 450, labels = "Shift 3", cex = 1.5)
mtext("B", adj=-1.35, line=1, cex=1.5) 
mtext("D", adj=1.2, line=1, cex=1.5)

for (i in 2:length(subjectsSD)){
  plot_smooth(SDm, view = "ex_trial", rug = F,
              shade = F, legend_plot_all = F, se = F,
              cond = list(subject= subjectsSD[i]),
              main = expression("SD"),
              plot_all = c("pert_direct", "subject"),
              xlab = 'Trial', ylab = 'Normalized COG (Hz)',
              cex.lab=2, cex.axis=2, cex.main=2,
              cex.sub=2, col = colorSet[i],
              rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(diffMidHighLowm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectsLevelDMidLow[1]),
            main = expression("LevelD" [Mid-Low]*" (dB) (n = 2)"),
            plot_all = c("subject", "pert_direct"),
            xlab = 'Trial', ylab = 'Normalized COG (Hz)',
            ylim=c(-6, 3), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 3, labels = "Base", cex = 1.5)
text(x = 65, y = 3, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 3, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 3, labels = "Shift 3", cex = 1.5)

for (i in 2:length(subjectsLevelDMidLow)){
  plot_smooth(diffMidHighLowm, view = "ex_trial", rug = F,
              shade = F, legend_plot_all = F, se = F,
              cond = list(subject= subjectsLevelDMidLow[i]),
              main = expression("Participant"),
              plot_all = c("subject", "pert_direct"),
              xlab = 'Trial', ylab = 'Normalized COG (Hz)',
              cex.lab=2, cex.axis=2, cex.main=2,
              cex.sub=2, col = colorSet[i],
              rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
}

mtext('Trial', side = 1, outer = TRUE, line = 2, cex = 1.5)
mtext('Normalized parameter', side = 2, outer = TRUE, line = 2, cex = 1.5)

# reset the plotting area
par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)

legend("bottom", title="Perturbation direction", cex=1.3, pt.cex=1,
       legend=c("down", "up"), lty =  c("solid", "dotdash"), lwd = 2,
       ncol=2, xpd=NA, bty="n")

# individual GAMM coefficients for the last shift phase (Table 3.11)
GAMMmeanCoeff
GAMMdiffCoeff

####################################################
# shift effects over the course of the experiment (Figure 3.3)
####################################################

# models of shifted (perceived by speakers) parameters for
# rmsLow (LevelLow), COG, and rmsHigh (LevelHigh) 

rmsLowShiftm = bam(rmsLowShift_normmean ~ pert_direct + s(ex_trial,
                  by = pert_direct, k = 10) + s(ex_trial, subject,
                  by = pert_direct, bs = 'fs', m=1), data =
                  phaseMeansNoNoise[phaseMeansNoNoise$valid == '1',],
                  method = "ML")

CoGShiftm = bam(CoGShift_normmean ~ pert_direct + s(ex_trial, by = pert_direct,
                k = 10) + s(ex_trial, subject, by = pert_direct, bs = 'fs', m=1),
                data = phaseMeansNoNoise[phaseMeansNoNoise$valid == '1',],
                method = "ML")


rmsHighShiftm = bam(rmsHighShift_normmean ~ pert_direct + s(ex_trial,
                   by = pert_direct, k = 10) + s(ex_trial, subject,
                   by = pert_direct, bs = 'fs', m=1), data =
                   phaseMeansNoNoise[phaseMeansNoNoise$valid == '1',],
                   method = "ML")


# visualize the models for individual speakers
par(oma=c(10,5,1,1), mar=c(2,2,5,2), mfrow=c(2,3))

plot_smooth(CoGShiftm, view = "ex_trial", rug = F,
            shade = T, legend_plot_all = F, plot_all = c("pert_direct"),
            main = expression("COG (Hz) shift effect"),
            xlab = 'Trial', ylab = 'Normalized shifted COG (Hz)',
            cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, rm.ranef = T,
            lty = c("dotdash", "solid"), lwd = 2, col = "black")

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 2500, labels = "Base", cex = 1.5)
text(x = 65, y = 2500, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 2500, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 2500, labels = "Shift 3", cex = 1.5)

plot_smooth(rmsLowShiftm, view = "ex_trial", rug = F,
            shade = T, legend_plot_all = F, plot_all = c("pert_direct"),
            main = expression("Level"[Low]*" (dB) shift effect"),
            xlab = 'Trial', ylab = 'Normalized LevelLow (dB)',
            cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, rm.ranef = T,
            lty = c("dotdash", "solid"), lwd = 2, col = "black")

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 6, labels = "Base", cex = 1.5)
text(x = 65, y = 6, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 6, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 6, labels = "Shift 3", cex = 1.5)
mtext("A", adj=-1.35, line=1, cex=1.5) 
mtext("C", adj=1.2, line=1, cex=1.5) 

plot_smooth(rmsHighShiftm, view = "ex_trial", rug = F,
            shade = T, legend_plot_all = NA,
            plot_all = c("pert_direct"),
            main = expression("Level"[HIgh]*" (dB) shift effect"),
            xlab = 'Trial', ylab = 'Normalized LevelHigh (dB)',
            cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, rm.ranef = T,
            lty = c("dotdash", "solid"), lwd = 2, col = "black")

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 10, labels = "Base", cex = 1.5)
text(x = 65, y = 10, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 10, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 10, labels = "Shift 3", cex = 1.5)
mtext("B", adj=-1.35,line=1, cex=1.5)

plot_smooth(CoGShiftm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectListEdited[1]),
            main = expression("Participant smooths"),
            plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized F1 (Hz)',
            ylim=c(-3300, 3400), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 3300, labels = "Base", cex = 1.5)
text(x = 65, y = 3300, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 3300, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 3300, labels = "Shift 3", cex = 1.5)

for (i in 2:length(subjectList)){
  plot_smooth(CoGShiftm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectListEdited[i]),
            main = expression("Participant smooths"),
            plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized F1 (Hz)',
            ylim=c(-45, 65), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[i],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(rmsLowShiftm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectListEdited2[1]),
            main = expression("Participant smooths"),
            plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized F2 (Hz)',
            ylim=c(-17, 17), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 17, labels = "Base", cex = 1.5)
text(x = 65, y = 17, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 17, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 17, labels = "Shift 3", cex = 1.5)
mtext("D", adj=-1.35, line=1, cex=1.5) 
mtext("F", adj=1.2, line=1, cex=1.5)

for (i in 2:length(subjectList)){
  plot_smooth(rmsLowShiftm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectListEdited2[i]),
            main = expression("Participant smooths"),
            plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized F2 (Hz)',
            ylim=c(-100, 200), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[i],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
}

plot_smooth(rmsHighShiftm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectList[1]),
            main = expression("Participant smooths"),
            plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized COG (Hz)',
            ylim=c(-25, 20), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[1],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)

abline(v = 48, lty = 2)
abline(v = 78, lty = 2)
abline(v = 108, lty = 2)
abline(v = 138, lty = 2)
text(x = 30, y = 20, labels = "Base", cex = 1.5)
text(x = 65, y = 20, labels = "Shift 1", cex = 1.5)
text(x = 95, y = 20, labels = "Shift 2", cex = 1.5) 
text(x = 125, y = 20, labels = "Shift 3", cex = 1.5)
mtext("E", adj=-1.35, line=1, cex=1.5) 

for (i in 2:length(subjectList)){
  plot_smooth(rmsHighShiftm, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = F, se = F,
            cond = list(subject= subjectList[i]),
            main = expression("Participant smooths"),
            plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized COG (Hz)',
            ylim=c(-1500, 1500), cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = colorSet[i],
            rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2, add = T)
}

mtext('Trial', side = 1, outer = TRUE, line = 2, cex = 1.5)
mtext('Normalized parameter', side = 2, outer = TRUE, line = 2, cex = 1.5)

par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)

legend("bottom", title="Perturbation direction", cex=1.3, pt.cex=1,
       legend=c("down", "up"), lty =  c("solid", "dotdash"), lwd = 2,
       ncol=2, xpd=NA, bty="n")
