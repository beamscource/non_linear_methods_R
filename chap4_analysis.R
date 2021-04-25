# Eugen Klein: Relation between acoustic and
# articulatory dimensions of speech sounds, 2020
# chapter 4 analysis

# employed libraries

library(Boruta)
library(randomForest)
library(ggplot2)
library(reshape2)

####################################################
# Experiment 1
####################################################

# identify relevant parameters for each phase (baseline,
# shift phases): Boruta computations for Table 4.1 

#####################
# model 0: baseline phase
set.seed(71)
pert.bor0.Vowel = Boruta(phaseMeansVowels[phaseMeansVowels$phase %in% c('baseline'),
  which(names(phaseMeansVowels) %in% acousticParametersVowels)],
  phaseMeansVowels[phaseMeansVowels$phase  %in% c('baseline'),]$pert_direct,
  doTrace = 2, maxRuns=500)

pert.bor0.VowelRelevant = getSelectedAttributes(pert.bor0.Vowel, withTentative = FALSE)

#####################
# model 1: 220 Hz
set.seed(71)
pert.bor1.Vowel = Boruta(phaseMeansVowels[phaseMeansVowels$phase %in% c('220'),
  which(names(phaseMeansVowels) %in% acousticParametersVowels)],
  phaseMeansVowels[phaseMeansVowels$phase  %in% c('220'),]$pert_direct,
  doTrace = 2, maxRuns=500)

pert.bor1.VowelRelevant = getSelectedAttributes(pert.bor1.Vowel, withTentative = FALSE)

######################
#model 2: 370 Hz
set.seed(71)
pert.bor2.Vowel = Boruta(phaseMeansVowels[phaseMeansVowels$phase %in% c('370'),
  which(names(phaseMeansVowels) %in% acousticParametersVowels)],
  phaseMeansVowels[phaseMeansVowels$phase  %in% c('370'),]$pert_direct,
  doTrace = 2, maxRuns=500)

pert.bor2.VowelRelevant = getSelectedAttributes(pert.bor2.Vowel, withTentative = FALSE)

##########################
#model 3: 520 Hz
set.seed(71)
pert.bor3.Vowel = Boruta(phaseMeansVowels[phaseMeansVowels$phase %in% c('520'),
  which(names(phaseMeansVowels) %in% acousticParametersVowels)],
  phaseMeansVowels[phaseMeansVowels$phase  %in% c('520'),]$pert_direct,
  doTrace = 2, maxRuns=500)

pert.bor3.VowelRelevant = getSelectedAttributes(pert.bor3.Vowel, withTentative = FALSE)

# plot relevant parameters
plot(pert.bor0.Vowel, las = 3)
plot(pert.bor1.Vowel, las = 3)
plot(pert.bor2.Vowel, las = 3)
plot(pert.bor3.Vowel, las = 3)

# define Random forest models for each phase including only relevant
# parameters (Section 4.3.1)
#varsAllVowel = as.formula(paste('pert_direct ~', paste(acousticParametersVowels, collapse = " + "), sep = "")) 

vars0Vowel = as.formula(paste('as.factor(pert_direct) ~', paste(pert.bor0.VowelRelevant, collapse = " + "), sep = ""))
vars1Vowel = as.formula(paste('as.factor(pert_direct) ~', paste(pert.bor1.VowelRelevant, collapse = " + "), sep = ""))
vars2Vowel = as.formula(paste('as.factor(pert_direct) ~', paste(pert.bor2.VowelRelevant, collapse = " + "), sep = ""))
vars3Vowel = as.formula(paste('as.factor(pert_direct) ~', paste(pert.bor3.VowelRelevant, collapse = " + "), sep = ""))

# compure RF models
set.seed(71)
pert_phase0.Vowel.rf = randomForest(vars0Vowel,
                              data = phaseMeansVowels[phaseMeansVowels$phase == 'baseline',],
                              importance = T, mtry = length(pert.bor0.VowelRelevant), ntree = 10000)
set.seed(71)
pert_phase1.Vowel.rf = randomForest(vars1Vowel,
                              data = phaseMeansVowels[phaseMeansVowels$phase == '220',],
                              importance = T, mtry = length(pert.bor1.VowelRelevant), ntree = 10000)

set.seed(71)
pert_phase2.Vowel.rf = randomForest(vars2Vowel,
                              data = phaseMeansVowels[phaseMeansVowels$phase == '370',],
                              importance = T, mtry = length(pert.bor2.VowelRelevant), ntree = 10000)
set.seed(71)
pert_phase3.Vowel.rf = randomForest(vars3Vowel,
                              data = phaseMeansVowels[phaseMeansVowels$phase == '520',],
                              importance = T, mtry = length(pert.bor3.VowelRelevant), ntree = 10000)


# compile prediction accuracies for Experiment 1
accuraciesVowel = matrix(c("Baseline", "Shift 1", "Shift 2", "Shift 3",
                      1 - (pert_phase0.Vowel.rf$confusion[5] + pert_phase0.Vowel.rf$confusion[6])/2,
                      1 - (pert_phase1.Vowel.rf$confusion[5] + pert_phase1.Vowel.rf$confusion[6])/2,
                      1 - (pert_phase2.Vowel.rf$confusion[5] + pert_phase2.Vowel.rf$confusion[6])/2,
                      1 - (pert_phase3.Vowel.rf$confusion[5] + pert_phase3.Vowel.rf$confusion[6])/2,
                      "normal", "shift", "shift", "shift"),
                    ncol = 3, byrow = F)

colnames(accuraciesVowel) = c("phase","accuracy", "feedback")
accuraciesVowel = as.data.frame(accuraciesVowel)

accuraciesVowel$accuracy = as.numeric(as.character(accuraciesVowel$accuracy))
accuraciesVowel$feedback = as.factor(accuraciesVowel$feedback)
accuraciesVowel$feedback = factor(accuraciesVowel$feedback, levels=c("normal", "shift"))
accuraciesVowel$phase = factor(accuraciesVowel$phase, levels=c("Baseline", "Shift 1", "Shift 2", "Shift 3"))

# accuracies for the whole experiment (Figure 4.1)
(pVowel = ggplot(data=accuraciesVowel, aes(x=phase, y=accuracy, fill=feedback))
  + geom_bar(stat="identity", width=0.7)
  + geom_text(aes(label=round(accuracy*100, digits =2)), vjust=-.7, size=5)
  + scale_y_continuous(breaks=c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                       labels=c('40', '50', '60', '70', '80', '90'))
  + scale_fill_manual(values=c("#B0AFAF", "#7F7F7F"), labels=c("normal", "shifted"))
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
  + coord_cartesian(ylim=c(0.4, 0.9)))

####################################################
# Experiment 2
####################################################

# identify relevant parameters for each phase (baseline,
# shift phases): Boruta computations for Table 4.2

# model 0: baseline phase
set.seed(71)
pert.fric.bor0 = Boruta(phaseMeansFricatives[phaseMeansFricatives$phase %in% c('baseline'), 
  which(names(phaseMeansFricatives) %in% acousticParametersFricativesLong)],
  phaseMeansFricatives[phaseMeansFricatives$phase  %in% c('baseline'),]$pert_direct,
  doTrace = 2, maxRuns=500)

pert.fric.bor0Relevant = getSelectedAttributes(pert.fric.bor0, withTentative = FALSE)

# model 1: shift 2 phase
set.seed(71)
pert.fric.bor1 = Boruta(phaseMeansFricatives[phaseMeansFricatives$phase %in% c('shift_2'),
  which(names(phaseMeansFricatives) %in% acousticParametersFricativesLong)],
  phaseMeansFricatives[phaseMeansFricatives$phase  %in% c('shift_2'),]$pert_direct,
  doTrace = 2, maxRuns=500)

pert.fric.bor1Relevant = getSelectedAttributes(pert.fric.bor1, withTentative = FALSE)

#model 2: shift 3 phase
set.seed(71)
pert.fric.bor2 = Boruta(phaseMeansFricatives[phaseMeansFricatives$phase %in% c('shift_3'),
  which(names(phaseMeansFricatives) %in% acousticParametersFricativesLong)],
  phaseMeansFricatives[phaseMeansFricatives$phase  %in% c('shift_3'),]$pert_direct,
  doTrace = 2, maxRuns=500)

pert.fric.bor2Relevant = getSelectedAttributes(pert.fric.bor2, withTentative = FALSE)

#model 3: shift 4 phase
set.seed(71)
pert.fric.bor3 = Boruta(phaseMeansFricatives[phaseMeansFricatives$phase %in% c('shift_4'),
  which(names(phaseMeansFricatives) %in% acousticParametersFricativesLong)],
  phaseMeansFricatives[phaseMeansFricatives$phase %in% c('shift_4'),]$pert_direct,
  doTrace = 2, maxRuns=500)

pert.fric.bor3Relevant = getSelectedAttributes(pert.fric.bor3, withTentative = FALSE)

# plot relevant parameters
plot(pert.fric.bor0, las = 3)
plot(pert.fric.bor1, las = 3)
plot(pert.fric.bor2, las = 3)
plot(pert.fric.bor3, las = 3)

# define Random forest models for each phase including only relevant
# parameters (Section 4.3.1)

# compile prediction accuracies for Experiment 2
# pert.fric.bor0Relevant, pert.fric.bor1Relevant are empty
varsAllFricatives = as.formula(paste('as.factor(pert_direct) ~', paste(acousticParametersFricativesLong, collapse = " + "), sep = "")) 

# define Random forest models for each phase including only relevant parameters
#vars0Fric = as.formula(paste('as.factor(pert_direct) ~', paste(pert.fric.bor0Relevant, collapse = " + "), sep = ""))
#vars1Fric = as.formula(paste('as.factor(pert_direct) ~', paste(pert.fric.bor1Relevant, collapse = " + "), sep = ""))
vars2Fric = as.formula(paste('as.factor(pert_direct) ~', paste(pert.fric.bor2Relevant, collapse = " + "), sep = ""))
vars3Fric = as.formula(paste('as.factor(pert_direct) ~', paste(pert.fric.bor3Relevant, collapse = " + "), sep = ""))

# compute RF models
set.seed(71)
pert_phase0.fric.rf = randomForest(varsAllFricatives,
                        data = phaseMeansFricatives[phaseMeansFricatives$phase == 'baseline',],
                        importance = T, mtry = length(acousticParametersFricativesLong), ntree = 10000)

set.seed(71)
pert_phase1.fric.rf = randomForest(varsAllFricatives,
                        data = phaseMeansFricatives[phaseMeansFricatives$phase == 'shift_2',],
                        importance = T, mtry = length(acousticParametersFricativesLong), ntree = 10000)

set.seed(71)
pert_phase2.fric.rf = randomForest(vars2Fric,
                        data = phaseMeansFricatives[phaseMeansFricatives$phase == 'shift_3',],
                        importance = T, mtry = length(pert.fric.bor2Relevant), ntree = 10000)
set.seed(71)
pert_phase3.fric.rf = randomForest(vars3Fric,
                        data = phaseMeansFricatives[phaseMeansFricatives$phase == 'shift_4',],
                        importance = T, mtry = length(pert.fric.bor3Relevant), ntree = 10000)


# compile prediction accuracies for Experiment 2
accuraciesFricatives = matrix(c("Baseline", "Shift 1", "Shift 2", "Shift 3",
                      1 - (pert_phase0.fric.rf$confusion[5] + pert_phase0.fric.rf$confusion[6])/2,
                      1 - (pert_phase1.fric.rf$confusion[5] + pert_phase1.fric.rf$confusion[6])/2,
                      1 - (pert_phase2.fric.rf$confusion[5] + pert_phase2.fric.rf$confusion[6])/2,
                      1 - (pert_phase3.fric.rf$confusion[5] + pert_phase3.fric.rf$confusion[6])/2,
                      "normal", "shift", "shift", "shift"),
                    ncol = 3, byrow = F)

colnames(accuraciesFricatives) = c("phase","accuracy", "feedback")
accuraciesFricatives = as.data.frame(accuraciesFricatives)

accuraciesFricatives$accuracy = as.numeric(as.character(accuraciesFricatives$accuracy))
accuraciesFricatives$feedback = as.factor(accuraciesFricatives$feedback)
accuraciesFricatives$feedback = factor(accuraciesFricatives$feedback, levels=c("normal", "shift"))
accuraciesFricatives$phase = factor(accuraciesFricatives$phase, levels=c("Baseline", "Shift 1", "Test 1", "Shift 2", "Test 2", "Shift 3", "Test 3"))

# accuracies for the whole experiment (Figure 4.2)
(pFric = ggplot(data=accuraciesFricatives, aes(x=phase, y=accuracy, fill=feedback))
  + geom_bar(stat="identity", width=0.7)
  + geom_text(aes(label=round(accuracy*100, digits =2)), vjust=-.7, size=5)
  + scale_y_continuous(breaks=c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                       labels=c('40', '50', '60', '70', '80', '90'))
  + scale_fill_manual(values=c("#B0AFAF", "#7F7F7F"), labels=c("normal", "shifted"))
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
  + coord_cartesian(ylim=c(0.4, 0.9)))

####################################################
# individual RF models for the last shift phase 
####################################################

subjectList = levels(phaseMeansVowels$subject)

# Experiment 1

# directory for Boruta plots
saveDirBorVowels = "C:/individual_boruta_chap4/vowels/"
dir.create(saveDirBorVowels)

for (i in 1:length(subjectList)){
  
  set.seed(71)
  
  # Boruta computation
  pert.bor_temp = Boruta(phaseMeansVowels[phaseMeansVowels$subject == subjectList[i] &
                         phaseMeansVowels$phase %in% c('520'),
                         which(names(phaseMeansVowels) %in% acousticParametersVowels)],
                         phaseMeansVowels[phaseMeansVowels$subject == subjectList[i] &
                         phaseMeansVowels$phase %in% c('520'),]$pert_direct,
                         doTrace = 2, maxRuns=500)
  
  pert.bor_temp_relevant = getSelectedAttributes(pert.bor_temp, withTentative = FALSE)
  
  # save the Boruta plots
  png(filename = paste(saveDirBorVowels, subjectList[i],".png", sep = "", collapse = NULL))
  par(mar=c(12,5,1,1))
  plot(pert.bor_temp, las = 3)
  dev.off()
  
  # save Boruta results to a data.frame
  bor_data_temp = as.data.frame(pert.bor_temp$ImpHistory)
  
  # reshape and compute means of the table
  bor_data_temp = melt(apply(bor_data_temp, 2, mean), value.name = "boruta_score")
  
  # rename columns
  bor_data_temp = setNames(cbind(rownames(bor_data_temp), bor_data_temp, row.names = NULL), c("parameter", "boruta_score"))
  
  #sort the parameters by score and create new idex numbers
  bor_data_temp = bor_data_temp[order(-bor_data_temp$boruta_score), c(1,2)]
  rownames(bor_data_temp) = NULL
  
  #save as table for individual subject
  assign(paste("boruta_table_vowel_", subjectList[i], sep = ""), bor_data_temp)
  
  # RF model with relevant parameters only
  
  if (length(pert.bor_temp_relevant)){
    vars_temp = as.formula(paste('pert_direct ~', paste(pert.bor_temp_relevant, collapse=" + "), sep = ""))  
  } else {
    
    pert.bor_temp_relevant = acousticParametersVowels
    vars_temp = as.formula(paste('pert_direct ~', paste(pert.bor_temp_relevant, collapse = " + "), sep = "")) 
  }
  
  set.seed(71)
  
  pert_phase_temp.rf = randomForest(vars_temp,
                       data = phaseMeansVowels[phaseMeansVowels$subject == subjectList[i] &
                       phaseMeansVowels$phase == '520',], importance = T,
                       mtry = length(pert.bor_temp_relevant), ntree = 10000)
  
  # add subject's model error scores to a table
  if (i == 1){
    
    id = subjectList[i]
    derr = pert_phase_temp.rf$confusion[5]
    uerr = pert_phase_temp.rf$confusion[6]
    error_scores_temp = data.frame(id, derr, uerr)
    
    #rename columns
    colnames(error_scores_temp)[1] = "subject"
    colnames(error_scores_temp)[2] = "down_error"
    colnames(error_scores_temp)[3] = "up_error"
    
    rf.error_scores_ind_vowel = error_scores_temp
    
  } else {
    
    id = subjectList[i]
    derr = pert_phase_temp.rf$confusion[5]
    uerr = pert_phase_temp.rf$confusion[6]
    error_scores_temp = data.frame(id, derr, uerr)
    
    #rename columns
    colnames(error_scores_temp)[1] = "subject"
    colnames(error_scores_temp)[2] = "down_error"
    colnames(error_scores_temp)[3] = "up_error"
    
    rf.error_scores_ind_vowel = rbind(rf.error_scores_ind_vowel, error_scores_temp)
    
  }  
}

# Experiment 2

# directory for Boruta plots
saveDirBorFricatives = "C:/individual_boruta_chap4/fricatives/"
dir.create(saveDirBorFricatives)

for (i in 1:length(subjectList)){
  
  set.seed(71)
  
  # Boruta computation
  pert.bor_temp = Boruta(phaseMeansFricatives[phaseMeansFricatives$subject == subjectList[i] &
                         phaseMeansFricatives$phase %in% c('shift_4'),
                         which(names(phaseMeansFricatives) %in% acousticParametersFricativesLong)],
                         phaseMeansFricatives[phaseMeansFricatives$subject == subjectList[i] &
                         phaseMeansFricatives$phase %in% c('shift_4'),]$pert_direct,
                         doTrace = 2, maxRuns=500)
  
  pert.bor_temp_relevant = getSelectedAttributes(pert.bor_temp, withTentative = FALSE)
  
  # save the Boruta plots
  png(filename = paste(saveDirBorFricatives, subjectList[i],".png", sep = "", collapse = NULL))
  par(mar=c(12,5,1,1))
  plot(pert.bor_temp, las = 3)
  dev.off()
  
  # # save Boruta results to a data.frame
  bor_data_temp = as.data.frame(pert.bor_temp$ImpHistory)
  
  # reshape and compute means of the table
  bor_data_temp = melt(apply(bor_data_temp, 2, mean), value.name = "boruta_score")
  
  # rename columns
  bor_data_temp = setNames(cbind(rownames(bor_data_temp), bor_data_temp, row.names = NULL), c("parameter", "boruta_score"))
  
  #sort the parameters by score and create new idex numbers
  bor_data_temp = bor_data_temp[order(-bor_data_temp$boruta_score), c(1,2)]
  rownames(bor_data_temp) = NULL
  
  #save as table for individual subject
  assign(paste("boruta_table_fricative_", subjectList[i], sep = ""), bor_data_temp)
  
  # RF model with relevant parameters only
  
  if (length(pert.bor_temp_relevant)){
    vars_temp = as.formula(paste('pert_direct ~', paste(pert.bor_temp_relevant, collapse=" + "), sep = ""))  
  } else {
    
    pert.bor_temp_relevant = acousticParametersFricativesLong
    vars_temp = as.formula(paste('pert_direct ~', paste(pert.bor_temp_relevant, collapse = " + "), sep = "")) 
  }
  
  
  set.seed(71)
  
  pert_phase_temp.rf = randomForest(vars_temp,
                        data = phaseMeansFricatives[phaseMeansFricatives$subject == subjectList[i] &
                        phaseMeansFricatives$phase == 'shift_4',], importance = T,
                        mtry = length(pert.bor_temp_relevant), ntree = 10000)
  
  # add subject's model error scores to a table
  
  if (i == 1){
    
    id = subjectList[i]
    derr = pert_phase_temp.rf$confusion[5]
    uerr = pert_phase_temp.rf$confusion[6]
    error_scores_temp = data.frame(id, derr, uerr)
    
    #rename columns
    colnames(error_scores_temp)[1] = "subject"
    colnames(error_scores_temp)[2] = "down_error"
    colnames(error_scores_temp)[3] = "up_error"
    
    rf.error_scores_ind_fric = error_scores_temp
    
  } else{
    
    id = subjectList[i]
    derr = pert_phase_temp.rf$confusion[5]
    uerr = pert_phase_temp.rf$confusion[6]
    error_scores_temp = data.frame(id, derr, uerr)
    
    #rename columns
    colnames(error_scores_temp)[1] = "subject"
    colnames(error_scores_temp)[2] = "down_error"
    colnames(error_scores_temp)[3] = "up_error"
    
    rf.error_scores_ind_fric = rbind(rf.error_scores_ind_fric, error_scores_temp)
    
  } 
}

####################################################
# correlation of the accuracies in percent
# between Experiment 1 and Experiment 2 (Section 4.3.1)
####################################################

# vowels

# melt the frame
x = melt(rf.error_scores_ind_vowel, id=c("subject"))
x$variable = as.character(x$variable)
x[x$variable == "up_error",]$variable = "up"
x[x$variable == "down_error",]$variable = "down"
x$variable = as.factor(x$variable)
colnames(x)[2] = "pert_direct"
colnames(x)[3] = "vowel_err"

# add accuracy
x$vowel_acc = 1 - x$vowel_err

# mean table
x_mean = rf.error_scores_ind_vowel
x_mean$vowel_mean_err = (rf.error_scores_ind_vowel$down_error + rf.error_scores_ind_vowel$up_error)/2
x_mean$vowel_mean_acc = 1 - x_mean$vowel_mean_err
x_mean$up_error = NULL
x_mean$down_error = NULL

# fricatives

# melt the frame
y = melt(rf.error_scores_ind_fric, id=c("subject"))
y$variable = as.character(y$variable)
y[y$variable == "up_error",]$variable = "up"
y[y$variable == "down_error",]$variable = "down"
y$variable = as.factor(y$variable)
colnames(y)[2] = "pert_direct"
colnames(y)[3] = "fric_err"

# add accuracy
y$fric_acc = 1 - y$fric_err

# mean table
y_mean = rf.error_scores_ind_fric
y_mean$fric_mean_err = (rf.error_scores_ind_fric$down_error + rf.error_scores_ind_fric$up_error)/2
y_mean$fric_mean_acc = 1 - y_mean$fric_mean_err
y_mean$up_error = NULL
y_mean$down_error = NULL

# combine accuracy scores from Experiment 1 and 2 for plotting
acc_err_ind_mean_vowel_fric = merge(x_mean, y_mean, by = c('subject'))

# compute Person's correlation
acc = cor.test(acc_err_ind_mean_vowel_fric$fric_mean_acc,
  acc_err_ind_mean_vowel_fric$vowel_mean_acc)
r = format(acc$estimate, digits = 2)

eq = substitute(italic(r)~"="~rv*","~~italic(p)~">"~pv, list(rv = r, pv = .05))

# Figure 4.3
(pCorr = ggplot(acc_err_ind_mean_vowel_fric, aes(x = fric_mean_acc*100, y = vowel_mean_acc*100))
  + geom_point(shape = 1, size = 3, stroke = 1.5)
  + scale_y_continuous(breaks=c(80, 85, 90, 95, 100),
                       labels=c('80', '85', '90', '95', '100'))
  + scale_x_continuous(breaks=c(50, 60, 70, 80, 90, 100),
                       labels=c('50', '60', '70', '80', '90', '100'))
  + scale_shape_manual(values=c(2, 3))
  + scale_color_manual(values=c("#000000", "#838b8b"))
  + geom_smooth(method="lm", se=F, fullrange=F, size = 1, color = "black")
  + annotate("text", x = 55, y = 89, label = as.character(as.expression(eq)), parse = T, size = 6)
  + labs(x = "Fricative adaptation (%)", y = "Vowel adaptation (%)")
  + theme_bw()
  + theme(text = element_text(size=18),
          axis.text = element_text(size=16),
          axis.title = element_text(size=18, vjust=-1),
          legend.text = element_text(size=18),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  + theme(legend.position = "bottom",
          legend.text=element_text(size=16))
  + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  + coord_cartesian(ylim=c(80,100), xlim=c(50,95)))