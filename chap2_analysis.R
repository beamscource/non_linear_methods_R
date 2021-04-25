# Eugen Klein: Relation between acoustic and
# articulatory dimensions of speech sounds, 2020
# chapter 2 analysis

# employed libraries
library(ggplot2)
library(cowplot)
library(mgcv)
library(itsadug)

####################################################
# initial formant space (Figure 2.3)
####################################################

(p = ggplot()
  + geom_boxplot(data = baselineMeans, aes(x = formants, y = freq, fill = stimulus), outlier.shape = NA) # removing outliers here outlier.shape=NA
  + scale_fill_grey(start = .7, end = .4, "Syllable", breaks = levels(baselineMeans$stimulus),
                    labels=stimulusIPA)
  + theme_bw()
  + theme(text = element_text(size=18),
          axis.text = element_text(size=16),
          axis.title = element_text(size=18, vjust=-1),
          legend.text = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  + theme(legend.position = "bottom",
          legend.text=element_text(size=16))
  + labs(x = "Formants", y = "Frequency (Hz)", fill = "Syllable")
  + guides(fill=guide_legend(order = 1),
           color=guide_legend(override.aes=list(fill=NA), order = 2))
  + scale_x_discrete(breaks = levels(baselineMeans$formants),
                     labels = c('F1', 'F2', 'F3'))
  + scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000))
  + coord_cartesian(ylim=c(280,3800)))

# adapted formant space

# config A (Figure 2.4)
(p = ggplot()
  + geom_boxplot(data = adaptedMeans[adaptedMeans$stimulus %in% c("dy", "gy") & adaptedMeans$config == "A",],
    aes(x = formants, y = freq, fill = stimulus, linetype = phase), outlier.shape = NA)
  
  + scale_fill_grey(start = .7, end = .4, "Syllable", breaks = levels(adaptedMeans$stimulus),
                    labels=stimulusIPA2)
  
  + scale_x_discrete(breaks = levels(adaptedMeans$formants),
                     labels = c('F1', 'F2', 'F3'))
  + scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000))
  + scale_linetype(breaks = levels(adaptedMeans$phase),
                   labels = c('Base', 'Shift 3'))
  + theme_bw()
  + theme(text = element_text(size=18),
          axis.text = element_text(size=16),
          axis.title = element_text(size=18, vjust=-1),
          legend.text = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  + theme(legend.position = "bottom",
          legend.text=element_text(size=16),
          legend.box = "vertical")
  + labs(x = "Formants", y = "Frequency (Hz)", fill = "Syllable", linetype = "Phase")
  + guides(fill=guide_legend(order = 1),
           color=guide_legend(override.aes=list(fill=NA), order = 1))
  + coord_cartesian(ylim=c(280,3200)))

# config B (Figure 2.5)
(p = ggplot()
  + geom_boxplot(data = adaptedMeans[adaptedMeans$stimulus %in% c("dy", "gy") & adaptedMeans$config == "B",], aes(x = formants, y = freq, fill = stimulus, linetype = phase), outlier.shape = NA) # removing outliers here outlier.shape=NA
  
  #+ facet_wrap(~config, labeller=labeller(gender = labels))
  + scale_fill_grey(start = .7, end = .4, "Syllable", breaks = levels(adaptedMeans$stimulus),
                    labels=stimulusIPA2)
  
  + scale_x_discrete(breaks = levels(adaptedMeans$formants),
                     labels = c('F1', 'F2', 'F3'))
  + scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000))
  + scale_linetype(breaks = levels(adaptedMeans$phase),
                   labels = c('Base', 'Shift 3'))
  + theme_bw()
  + theme(text = element_text(size=18),
          axis.text = element_text(size=16),
          axis.title = element_text(size=18, vjust=-1),
          legend.text = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  + theme(legend.position = "bottom",
          legend.text=element_text(size=16),
          legend.box = "vertical")
  + labs(x = "Formants", y = "Frequency (Hz)", fill = "Syllable", linetype = "Phase")
  + guides(fill=guide_legend(order = 1),
           color=guide_legend(override.aes=list(fill=NA), order = 1))
  + coord_cartesian(ylim=c(280,3200)))

####################################################
# t-tests assesing initial formant differences and
# changes after adaptation (Sections 2.3.1 and 2.3.2)
####################################################

# baseline
t.test(baseMeans[baseMeans$stimulus == 'di',]$F1_mean, baseMeans[baseMeans$stimulus == 'dy',]$F1_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'dy',]$F1_mean, baseMeans[baseMeans$stimulus == 'gy',]$F1_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'gy',]$F1_mean, baseMeans[baseMeans$stimulus == 'gu',]$F1_mean, p.adjust = 'bonferroni')

t.test(baseMeans[baseMeans$stimulus == 'di',]$F2_mean, baseMeans[baseMeans$stimulus == 'dy',]$F2_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'dy',]$F2_mean, baseMeans[baseMeans$stimulus == 'gy',]$F2_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'gy',]$F2_mean, baseMeans[baseMeans$stimulus == 'gu',]$F2_mean, p.adjust = 'bonferroni')

t.test(baseMeans[baseMeans$stimulus == 'di',]$F3_mean, baseMeans[baseMeans$stimulus == 'dy',]$F3_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'dy',]$F3_mean, baseMeans[baseMeans$stimulus == 'gy',]$F3_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'gy',]$F3_mean, baseMeans[baseMeans$stimulus == 'gu',]$F3_mean, p.adjust = 'bonferroni')

# within-sound format differences
t.test(baseMeans[baseMeans$stimulus == 'di',]$F2_mean, baseMeans[baseMeans$stimulus == 'di',]$F3_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'gu',]$F2_mean, baseMeans[baseMeans$stimulus == 'gu',]$F3_mean, p.adjust = 'bonferroni')

t.test(baseMeans[baseMeans$stimulus == 'dy',]$F2_mean, baseMeans[baseMeans$stimulus == 'dy',]$F3_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'gy',]$F2_mean, baseMeans[baseMeans$stimulus == 'gy',]$F3_mean, p.adjust = 'bonferroni')

# adapted space

t.test(baseMeans[baseMeans$stimulus == 'dy' & baseMeans$config == "A",]$F1_mean, phaseMeans[phaseMeans$stimulus == 'dy' & phaseMeans$phase == "520" & phaseMeans$config == "A",]$F1_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'gy' & baseMeans$config == "A",]$F1_mean, phaseMeans[phaseMeans$stimulus == 'gy' & phaseMeans$phase == "520" & phaseMeans$config == "A",]$F1_mean, p.adjust = 'bonferroni')

t.test(baseMeans[baseMeans$stimulus == 'dy' & baseMeans$config == "A",]$F2_mean, phaseMeans[phaseMeans$stimulus == 'dy' & phaseMeans$phase == "520" & phaseMeans$config == "A",]$F2_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'gy' & baseMeans$config == "A",]$F2_mean, phaseMeans[phaseMeans$stimulus == 'gy' & phaseMeans$phase == "520" & phaseMeans$config == "A",]$F2_mean, p.adjust = 'bonferroni')

t.test(baseMeans[baseMeans$stimulus == 'dy' & baseMeans$config == "A",]$F3_mean, phaseMeans[phaseMeans$stimulus == 'dy' & phaseMeans$phase == "520" & phaseMeans$config == "A",]$F3_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'gy' & baseMeans$config == "A",]$F3_mean, phaseMeans[phaseMeans$stimulus == 'gy' & phaseMeans$phase == "520" & phaseMeans$config == "A",]$F3_mean, p.adjust = 'bonferroni')

t.test(baseMeans[baseMeans$stimulus == 'dy' & baseMeans$config == "B",]$F1_mean, phaseMeans[phaseMeans$stimulus == 'dy' & phaseMeans$phase == "520" & phaseMeans$config == "B",]$F1_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'gy' & baseMeans$config == "B",]$F1_mean, phaseMeans[phaseMeans$stimulus == 'gy' & phaseMeans$phase == "520" & phaseMeans$config == "B",]$F1_mean, p.adjust = 'bonferroni')

t.test(baseMeans[baseMeans$stimulus == 'dy' & baseMeans$config == "B",]$F2_mean, phaseMeans[phaseMeans$stimulus == 'dy' & phaseMeans$phase == "520" & phaseMeans$config == "B",]$F2_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'gy' & baseMeans$config == "B",]$F2_mean, phaseMeans[phaseMeans$stimulus == 'gy' & phaseMeans$phase == "520" & phaseMeans$config == "B",]$F2_mean, p.adjust = 'bonferroni')

t.test(baseMeans[baseMeans$stimulus == 'dy' & baseMeans$config == "B",]$F3_mean, phaseMeans[phaseMeans$stimulus == 'dy' & phaseMeans$phase == "520" & phaseMeans$config == "B",]$F3_mean, p.adjust = 'bonferroni')
t.test(baseMeans[baseMeans$stimulus == 'gy' & baseMeans$config == "B",]$F3_mean, phaseMeans[phaseMeans$stimulus == 'gy' & phaseMeans$phase == "520" & phaseMeans$config == "B",]$F3_mean, p.adjust = 'bonferroni')

####################################################
# compensation table for Group A and Group B (Table 2.2)
####################################################
compTable

#####################################################
# corelation between compensation in F2 and F3 (Section 2.3.4)
#####################################################

# compute Person's correlation for upwards and down
corUp = cor.test(compTable[compTable$pert_direct == 'up',]$F2_comp, compTable[compTable$pert_direct == 'up',]$F3_comp)
rUp = format(corUp$estimate, digits = 2)

eqUp = substitute(italic(r)~"="~rv*","~~italic(p)~"="~pv, list(rv = rUp, pv = .51))

corDown = cor.test(compTable[compTable$pert_direct == 'down',]$F2_comp, compTable[compTable$pert_direct == 'down',]$F3_comp)
rDown = format(corDown$estimate, digits = 2)

eqDown = substitute(italic(r)~"="~rv*","~~italic(p)~"<"~pv, list(rv = rDown, pv = .001))

# vizualize correlation between F2 and F3
subjectList = levels(compTable$subject)

# correlation for different groups
cor.test(compTable[compTable$pert_direct == 'up' & compTable$learn_group == 'symmetrical',]$F2_comp, compTable[compTable$pert_direct == 'up' & compTable$learn_group == 'symmetrical',]$F3_comp)
cor.test(compTable[compTable$pert_direct == 'down' & compTable$learn_group == 'symmetrical',]$F2_comp, compTable[compTable$pert_direct == 'down' & compTable$learn_group == 'symmetrical',]$F3_comp)

cor.test(compTable[compTable$pert_direct == 'up' & compTable$learn_group == 'asymmetrical',]$F2_comp, compTable[compTable$pert_direct == 'up' & compTable$learn_group == 'asymmetrical',]$F3_comp)
cor.test(compTable[compTable$pert_direct == 'down' & compTable$learn_group == 'asymmetrical',]$F2_comp, compTable[compTable$pert_direct == 'down' & compTable$learn_group == 'asymmetrical',]$F3_comp)

cor.test(compTable[compTable$pert_direct == 'up' & compTable$learn_group == 'negative',]$F2_comp, compTable[compTable$pert_direct == 'up' & compTable$learn_group == 'negative',]$F3_comp)
cor.test(compTable[compTable$pert_direct == 'down' & compTable$learn_group == 'negative',]$F2_comp, compTable[compTable$pert_direct == 'down' & compTable$learn_group == 'negative',]$F3_comp)

# scatter plot
(p = ggplot(compTable[compTable$learn_group != 'none',], aes(x = F2_comp, y = F3_comp, shape = pert_direct, color = pert_direct, label = subject))
  + geom_point(size = 3, stroke = 1.5)
  + scale_y_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 100),
                       labels=c('-75', '-50', '-25', '0', '25',  '50', '100'))
  + scale_x_continuous(breaks=c(-100, -75, -50, -25, 0, 25, 50),
                       labels=c('-100', '-75', '-50', '-25', '0', '25', '50'))
  + scale_shape_manual(values=c(2, 3))
  + scale_color_manual(values=c("#838b8b", "#000000"))
  + geom_smooth(method="lm", se=F, fullrange=F, linetype = 1, size = 0.8)
  + geom_vline(xintercept=0, linetype = 'dashed')
  + geom_hline(yintercept=0, linetype = 'dashed')
  + annotate("text", x = -85, y = -25, label = as.character(as.expression(eqUp)), parse = T, size = 6)
  + annotate("text", x = -70, y = -50, label = as.character(as.expression(eqDown)), parse = T, size = 6)
  + labs(x = "F2 compensation (%)", y = "F3 change (%)", color = "Perturbation direction", shape = "Perturbation direction")
  + theme_bw()
  + theme(text = element_text(size=18),
          axis.text = element_text(size=16),
          axis.title = element_text(size=18, vjust=-1),
          legend.text = element_text(size=18),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  + theme(legend.position = "bottom",
          legend.text=element_text(size=16))
  + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))

##################################################
# evolution plots by learner groups (Figure 2.6)
##################################################

(pSymm = ggplot(phaseMeans[phaseMeans$learn_group == 'symmetrical',], aes(x= ex_trial, y = F2_normmean, color = pert_direct, linetype = stimulus))
  + geom_point(size = 0.9, alpha = 0.7)
  + geom_smooth()
  + scale_x_continuous(breaks=c(0, 60, 110, 160, 210))
  + scale_y_continuous(breaks=c(400, 300, 200, 100, 0, -100, -200, -300, -400, -500))
  + scale_linetype_discrete(breaks=levels(phaseMeans$stimulus),
                            labels=c(stimulusIPA2))
  + scale_color_manual(values=c("#000000", "#838b8b"))
  + geom_vline(xintercept = c(0,60,110,160,210), linetype = 2)
  + annotate("text", x = 25, y = 240, label = "Base", size = 5)
  + annotate("text", x = 85, y = 240, label = "Shift 1", size = 5)
  + annotate("text", x = 135, y = 240, label = "Shift 2", size = 5)
  + annotate("text", x = 185, y = 240, label = "Shift 3", size = 5)
  + geom_hline(yintercept = c(0), linetype = 1)
  + theme_bw()
  + theme(text = element_text(size=18),
          axis.text = element_text(size=16),
          axis.title = element_text(size=18, vjust=-1),
          legend.text = element_text(size=18),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  + theme(legend.position = "bottom",
          legend.text=element_text(size=16))
  + guides(color=guide_legend(override.aes=list(fill=NA), order = 1),
           linetype=guide_legend(override.aes=list(fill=NA), order = 2))
  + ggtitle('Symmetrical (n = 10)')
  + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  + labs(x = "Trial", y = "Normalized F2 (Hz)", linetype = 'Syllable', color = 'Shift direction')
  + coord_cartesian(ylim=c(-470, 350)))

(pAsymm = ggplot(phaseMeans[phaseMeans$learn_group == 'asymmetrical',], aes(x= ex_trial, y = F2_normmean, color = pert_direct, linetype = stimulus))
  + geom_point(size = 0.9, alpha = 0.7)
  + geom_smooth()
  + scale_x_continuous(breaks=c(0, 60, 110, 160, 210))
  + scale_y_continuous(breaks=c(400, 300, 200, 100, 0, -100, -200, -300, -400, -500, -600))
  + scale_linetype_discrete(breaks=levels(phaseMeans$stimulus),
                            labels=c(stimulusIPA2))
  + scale_color_manual(values=c("#000000", "#838b8b"))
  + geom_vline(xintercept = c(0,60,110,160,210), linetype = 2)
  + annotate("text", x = 25, y = 140, label = "Base", size = 5)
  + annotate("text", x = 85, y = 140, label = "Shift 1", size = 5)
  + annotate("text", x = 135, y = 140, label = "Shift 2", size = 5)
  + annotate("text", x = 185, y = 140, label = "Shift 3", size = 5)
  + geom_hline(yintercept = c(0), linetype = 1)
  + theme_bw()
  + theme(text = element_text(size=18),
          axis.text = element_text(size=16),
          axis.title = element_text(size=18, vjust=-1),
          legend.text = element_text(size=18),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  + theme(legend.position = "bottom",
          legend.text=element_text(size=16))
  + guides(color=guide_legend(override.aes=list(fill=NA), order = 1),
           linetype=guide_legend(override.aes=list(fill=NA), order = 2))
  + ggtitle('Asymmetrical (n = 13)')
  + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  + labs(x = "Trial", y = "Normalized F2 (Hz)", linetype = 'Syllable', color = 'Shift Direction')
  + coord_cartesian(ylim=c(-650, 350)))

(pNo = ggplot(phaseMeans[phaseMeans$learn_group == 'negative',], aes(x= ex_trial, y = F2_normmean, color = pert_direct, linetype = stimulus))
  + geom_point(size = 0.9, alpha = 0.7)
  + geom_smooth()
  + scale_x_continuous(breaks=c(0, 60, 110, 160, 210))
  + scale_y_continuous(breaks=c(200, 100, 0, -100, -200, -300, -400))
  + scale_linetype_discrete(breaks=levels(phaseMeans$stimulus),
                            labels=c(stimulusIPA2))
  + scale_color_manual(values=c("#000000", "#838b8b"))
  + geom_vline(xintercept = c(0,60,110,160,210), linetype = 2)
  + annotate("text", x = 25, y = 140, label = "Base", size = 5)
  + annotate("text", x = 85, y = 140, label = "Shift 1", size = 5)
  + annotate("text", x = 135, y = 140, label = "Shift 2", size = 5)
  + annotate("text", x = 185, y = 140, label = "Shift 3", size = 5)
  + geom_hline(yintercept = c(0), linetype = 1)
  + theme_bw()
  + theme(text = element_text(size=18),
          axis.text = element_text(size=16),
          axis.title = element_text(size=18, vjust=-1),
          legend.text = element_text(size=18),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  + theme(legend.position = "bottom",
          legend.text=element_text(size=16))
  + guides(color=guide_legend(override.aes=list(fill=NA), order = 1),
           linetype=guide_legend(override.aes=list(fill=NA), order = 2))
  + ggtitle('Negative (n = 6)')
  + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  + labs(x = "Trial", y = "Normalized F2 (Hz)", linetype = 'Syllable', color = 'Shift direction')
  + coord_cartesian(ylim=c(-550, 350)))

allpatterns = plot_grid(pSymm + theme(legend.position="none"), pAsymm + theme(legend.position="none"), pNo + theme(legend.position="none"), labels = 'AUTO', align = 'h', nrow = 1)

# just for a black and white legend
(pLeg = ggplot(phaseMeans, aes(x= ex_trial, y = F2_normmean, color = stimulus, linetype = stimulus))
  + geom_smooth() 
  + scale_x_continuous(breaks=c(0, 60, 110, 160, 210))
  + geom_vline(xintercept = c(0,60,110,160,210), linetype = 2)
  + annotate("text", x = 25, y = 240, label = "Base", size = 5)
  + annotate("text", x = 85, y = 240, label = "Shift 1", size = 5)
  + annotate("text", x = 135, y = 240, label = "Shift 2", size = 5)
  + annotate("text", x = 185, y = 240, label = "Shift 3", size = 5)
  + geom_hline(yintercept = c(0), linetype = 1)
  + scale_linetype_manual(values = c("solid", "dotted"), labels=c(stimulusIPA2))
  + scale_color_manual(values=c("#000000", "#000000"), labels=c(stimulusIPA2))
  + theme_bw()
  + theme(text = element_text(size=18),
          axis.text = element_text(size=22),
          axis.title = element_text(size=23, vjust=-1),
          legend.text = element_text(size=16),
          axis.title.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  + theme(legend.position = "bottom")
  + guides(linetype=guide_legend(override.aes=list(fill=NA)))
  + ggtitle("Symmetrical (n = 5)")
  + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  + labs(x = "Trial", y = "Norm F2 (Hz)", linetype = 'Syllable', color = 'Syllable')
  + coord_cartesian(ylim=c(-250, 250)))

legend_pattern = get_legend(pLeg + theme(legend.position="bottom"))
(p = plot_grid(allpatterns, legend_pattern, ncol = 1, rel_heights = c(1, .2)))

##################################################
# GAMM models for each formant (F1-F3) (Section 2.3.5)
##################################################

F1mF = bam(F1_normmean ~ pert_direct + s(ex_trial, by = pert_direct, k = 10) +
             + s(ex_trial, subject, by = pert_direct, bs="fs", m=1),
           data = phaseMeans, method = "ML")

F2mF = bam(F2_normmean ~ pert_direct + s(ex_trial, by = pert_direct, k = 10) +
             + s(ex_trial, subject, by = pert_direct, bs="fs", m=1),
           data = phaseMeans, method = "ML")

F3mF = bam(F3_normmean ~ pert_direct + s(ex_trial, by = pert_direct, k = 10) +
             + s(ex_trial, subject, by = pert_direct, bs="fs", m=1),
           data = phaseMeans, method = "ML")

# check for complexity of the smooths and residuals
gam.check(F2mF)

# get auto correlation of the residuals
F2mFacf = acf_resid(F2mF)

# GAMM vizualization

# set up canvas
par(mar=c(10,6,3,1), mfrow=c(1,3))

# F1 model
plot_smooth(F1mF, view = "ex_trial", rug = F,
            shade = T, legend_plot_all = NA, plot_all = c("pert_direct"),
            main = "Response",  xlab = 'Trial',
            ylab = 'Normalized F1 (Hz)', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = T,
            lty = c("dotted", "solid"), lwd = 2, col = "black")
abline(v = 60, lty = 2)
abline(v = 110, lty = 2)
abline(v = 160, lty = 2)
abline(v = 210, lty = 2)
text(x = 30, y = 13, labels = "Base", cex=1.5)
text(x = 85, y = 13, labels = "Shift 1", cex=1.5)
text(x = 135, y = 13, labels = "Shift 2", cex=1.5) 
text(x = 185, y = 13, labels = "Shift 3", cex=1.5)
mtext("B", adj=1.15,line=1, cex=1.5) 

plot_smooth(F1mF, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = NA, se = F,
            main = "Participant smooths", plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized F1 (Hz)',
            cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = gray.colors(length(levels(phaseMeans$subject)), start = 0.3, end = 0.8, gamma = 2.2, alpha = NULL),
            rm.ranef = F, lty = c("dotted", "solid"), lwd = 2)
abline(v = 60, lty = 2)
abline(v = 110, lty = 2)
abline(v = 160, lty = 2)
abline(v = 210, lty = 2)
text(x = 30, y = 75, labels = "Base", cex=1.5)
text(x = 85, y = 75, labels = "Shift 1", cex=1.5)
text(x = 135, y = 75, labels = "Shift 2", cex=1.5) 
text(x = 185, y = 75, labels = "Shift 3", cex=1.5)
mtext("A", adj=-1.5, line=1, cex=1.5) 
mtext("C", adj=1.15, line=1, cex=1.5)

plot_diff(F1mF, view = "ex_trial", shade = T,
          comp = list(pert_direct = c('down', 'up')),
          main = "Upward vs downward",  xlab = 'Trial',
          ylab = 'Difference in F1 (Hz)', cex.lab=2,
          cex.axis=2, cex.main=2, cex.sub=2,  mark.diff = F,
          rm.ranef = T, lwd = 1.5)
abline(v = 60, lty = 2)
abline(v = 110, lty = 2)
abline(v = 160, lty = 2)
abline(v = 210, lty = 2)
text(x = 30, y = 8.5, labels = "Base", cex=1.5)
text(x = 85, y = 8.5, labels = "Shift 1", cex=1.5)
text(x = 135, y = 8.5, labels = "Shift 2", cex=1.5) 
text(x = 185, y = 8.5, labels = "Shift 3", cex=1.5)

# reset the plotting area
par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)

# line types should be altered to match the F2 model
legend("bottom", title="Perturbation direction", cex=1.3, pt.cex=1, legend=c("down", "up"), lty =  c("dotdash", "solid"), lwd = 2, ncol=2, xpd=NA, bty="n")

# set up canvas
par(mar=c(10,6,3,1), mfrow=c(1,3))

# F2 model
plot_smooth(F2mF, view = "ex_trial", rug = F,
            shade = T, legend_plot_all = F, plot_all = c("pert_direct"),
            main = "Response",  xlab = 'Trial',
            ylab = 'Normalized F2 (Hz)', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = T,
            lty = c("dotted", "solid"), lwd = 2, col = "black")
abline(v = 60, lty = 2)
abline(v = 110, lty = 2)
abline(v = 160, lty = 2)
abline(v = 210, lty = 2)
text(x = 30, y = 60, labels = "Base", cex=1.5)
text(x = 85, y = 60, labels = "Shift 1", cex=1.5)
text(x = 135, y = 60, labels = "Shift 2", cex=1.5) 
text(x = 185, y = 60, labels = "Shift 3", cex=1.5)
mtext("B", adj=1.15,line=1, cex=1.5) 

plot_smooth(F2mF, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = NA, se = F,
            main = "Participant smooths", plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized F2 (Hz)',
            cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = gray.colors(length(levels(phaseMeans$subject)), start = 0.3, end = 0.8, gamma = 2.2, alpha = NULL),
            rm.ranef = F, lty = c("dotted", "solid"), lwd = 2)
abline(v = 60, lty = 2)
abline(v = 110, lty = 2)
abline(v = 160, lty = 2)
abline(v = 210, lty = 2)
text(x = 30, y = 300, labels = "Base", cex=1.5)
text(x = 85, y = 300, labels = "Shift 1", cex=1.5)
text(x = 135, y = 300, labels = "Shift 2", cex=1.5) 
text(x = 185, y = 300, labels = "Shift 3", cex=1.5) 
mtext("A", adj=-1.5, line=1, cex=1.5) 
mtext("C", adj=1.15, line=1, cex=1.5)

plot_diff(F2mF, view = "ex_trial", shade = T,
          comp = list(pert_direct = c('down', 'up')),
          main = "Downward vs upward",  xlab = 'Trial',
          ylab = 'Difference in F2 (Hz)', cex.lab=2,
          cex.axis=2, cex.main=2, cex.sub=2,  mark.diff = T,
          rm.ranef = T, lwd = 1.5)
abline(v = 60, lty = 2)
abline(v = 110, lty = 2)
abline(v = 160, lty = 2)
abline(v = 210, lty = 2)
text(x = 30, y = 320, labels = "Base", cex=1.5)
text(x = 85, y = 320, labels = "Shift 1", cex=1.5)
text(x = 135, y = 320, labels = "Shift 2", cex=1.5) 
text(x = 185, y = 320, labels = "Shift 3", cex=1.5)
abline(v = 75, lty = 1, lwd = 2)
abline(v = 213, lty = 1, lwd = 2)
segments(x0=75.5,x1=211,y0=-70,y1=-70, lwd=5)

# reset the plotting area
par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)

legend("bottom", title="Perturbation direction", cex=1.3, pt.cex=1, legend=c("down", "up"), lty =  c("dotdash", "solid"), lwd = 2, ncol=2, xpd=NA, bty="n")

# set up canvas
par(mar=c(10,6,3,1), mfrow=c(1,3))

# F3 model
plot_smooth(F3mF, view = "ex_trial", rug = F,
            shade = T, legend_plot_all = NA, plot_all = c("pert_direct"),
            main = "Response",  xlab = 'Trial',
            ylab = 'Normalized F3 (Hz)', cex.lab=2,
            cex.axis=2, cex.main=2, cex.sub=2,
            rm.ranef = T,
            lty = c("dotted", "solid"), lwd = 2, col = "black")
abline(v = 60, lty = 2)
abline(v = 110, lty = 2)
abline(v = 160, lty = 2)
abline(v = 210, lty = 2)
text(x = 30, y = 50, labels = "Base", cex=1.5)
text(x = 85, y = 50, labels = "Shift 1", cex=1.5)
text(x = 135, y = 50, labels = "Shift 2", cex=1.5) 
text(x = 185, y = 50, labels = "Shift 3", cex=1.5)
mtext("B", adj=1.15,line=1, cex=1.5) 

plot_smooth(F3mF, view = "ex_trial", rug = F,
            shade = F, legend_plot_all = NA, se = F,
            main = "Participant smooths", plot_all = c("pert_direct", "subject"),
            xlab = 'Trial', ylab = 'Normalized F3 (Hz)',
            cex.lab=2, cex.axis=2, cex.main=2,
            cex.sub=2, col = gray.colors(length(levels(phaseMeans$subject)), start = 0.3, end = 0.8, gamma = 2.2, alpha = NULL),
            rm.ranef = F, lty = c("dotted", "solid"), lwd = 2)
abline(v = 60, lty = 2)
abline(v = 110, lty = 2)
abline(v = 160, lty = 2)
abline(v = 210, lty = 2)
text(x = 30, y = 225, labels = "Base", cex=1.5)
text(x = 85, y = 225, labels = "Shift 1", cex=1.5)
text(x = 135, y = 225, labels = "Shift 2", cex=1.5) 
text(x = 185, y = 225, labels = "Shift 3", cex=1.5) 
mtext("A", adj=-1.5, line=1, cex=1.5) 
mtext("C", adj=1.15, line=1, cex=1.5)


plot_diff(F3mF, view = "ex_trial", shade = T,
          comp = list(pert_direct = c('down', 'up')),
          main = "Downward vs upward",  xlab = 'Trial',
          ylab = 'Difference in F3 (Hz)', cex.lab=2,
          cex.axis=2, cex.main=2, cex.sub=2,  mark.diff = T,
          rm.ranef = T, lwd = 1.5)
abline(v = 60, lty = 2)
abline(v = 110, lty = 2)
abline(v = 160, lty = 2)
abline(v = 210, lty = 2)
text(x = 30, y = 160, labels = "Base", cex=1.5)
text(x = 85, y = 160, labels = "Shift 1", cex=1.5)
text(x = 135, y = 160, labels = "Shift 2", cex=1.5) 
text(x = 185, y = 160, labels = "Shift 3", cex=1.5)
abline(v = 79, lty = 1, lwd = 2)
abline(v = 213, lty = 1, lwd = 2)
segments(x0=79.5,x1=211,y0=-65,y1=-65, lwd=5)

# reset the plotting area
par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)

legend("bottom", title="Perturbation direction", cex=1.3, pt.cex=1, legend=c("down", "up"), lty =  c("dotdash", "solid"), lwd = 2, ncol=2, xpd=NA, bty="n")

####################################################
# individual GAMM plots for all fomants
####################################################

saveDirGAMM = 'C:/individual_GAMMs_chap2/'
dir.create(saveDirGAMM)

specModels = c('F1mF', 'F2mF', 'F3mF')
labels = c('F1 (Hz)', 'F2 (Hz)', 'F3 (Hz)')


for (i in 1:length(specModels)){
  currentModel = specModels[i]
  currentLabel = labels[i]
  
  for (j in 1:length(subjectList)){
  
  # set up canvas
  par(mar=c(10,6,3,1), mfrow=c(1,3))
  
  tryCatch({
    plot_smooth(get(currentModel), view = "ex_trial", rug = F,
                shade = T, legend_plot_all = F, plot_all = c("pert_direct"),
                cond = list(subject=subjectList[j]),
                main = "Response",  xlab = 'Trial',
                ylab = paste('Normalized ', currentLabel, collapse = NULL), cex.lab=2,
                cex.axis=2, cex.main=2, cex.sub=2,
                rm.ranef = F,
                lty = c("dotdash", "solid"), lwd = 2, col = "black")}, error=function(e){})
  
  abline(v = 48, lty = 2)
  abline(v = 78, lty = 2)
  abline(v = 108, lty = 2)
  abline(v = 138, lty = 2)
  text(x = 30, y = 390, labels = "Base", cex = 1.5)
  text(x = 65, y = 390, labels = "Shift 1", cex = 1.5)
  text(x = 95, y = 390, labels = "Shift 2", cex = 1.5) 
  text(x = 125, y = 390, labels = "Shift 3", cex = 1.5)
  mtext("B", adj=1.15,line=1, cex=1.5) 
  
  tryCatch({
    p =  plot_diff(get(currentModel), view = "ex_trial", shade = T,
                   comp = list(pert_direct = c('down', 'up')),
                   cond = list(subject=subjectList[j]),
                   main = "Downward vs upward",  xlab = 'Trial',
                   ylab = paste('Difference in ', currentLabel, collapse = NULL), cex.lab=2,
                   cex.axis=2, cex.main=2, cex.sub=2,  mark.diff = T,
                   rm.ranef = F, lwd = 1.5)
    
  }, error=function(e){})
  abline(v = 48, lty = 2)
  abline(v = 78, lty = 2)
  abline(v = 108, lty = 2)
  abline(v = 138, lty = 2)
  text(x = 30, y = 400, labels = "Base", cex = 1.5)
  text(x = 65, y = 400, labels = "Shift 1", cex = 1.5)
  text(x = 95, y = 400, labels = "Shift 2", cex = 1.5) 
  text(x = 125, y = 400, labels = "Shift 3", cex = 1.5)
  mtext("A", adj=-1.5, line=1, cex=1.5) 
  mtext("C", adj=1.15, line=1, cex=1.5)
  
  tryCatch({
    plot_smooth(get(currentModel), view = "ex_trial", rug = F,
                shade = F, legend_plot_all = F, se = F,
                main = "Random smooth", plot_all = c("pert_direct", "subject"),
                cond = list(subject=subjectList[j]),
                xlab = 'Trial', ylab = paste('Normalized ', currentLabel, collapse = NULL),
                cex.lab=2, cex.axis=2, cex.main=2,
                cex.sub=2, col = gray.colors(length(levels(phaseMeans$subject)), start = 0.3, end = 0.8, gamma = 2.2, alpha = NULL),
                rm.ranef = F, lty = c("dotdash", "solid"), lwd = 2)}, error=function(e){})
  
  abline(v = 48, lty = 2)
  abline(v = 78, lty = 2)
  abline(v = 108, lty = 2)
  abline(v = 138, lty = 2)
  text(x = 30, y = 1500, labels = "Base", cex = 1.5)
  text(x = 65, y = 1500, labels = "Shift 1", cex = 1.5)
  text(x = 95, y = 1500, labels = "Shift 2", cex = 1.5) 
  text(x = 125, y = 1500, labels = "Shift 3", cex = 1.5)
  
  # reset the plotting area
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
  
  legend("bottom", title="Perturbation direction", cex=1.3, pt.cex=1, legend=c("down", "up"), lty =  c("dotdash", "solid"), lwd = 2, ncol=2, xpd=NA, bty="n")
  
  dev.copy(png, width = 1100, height = 400, paste(saveDirGAMM, subjectList[j], '_', currentLabel,".jpg", sep = "", collapse = NULL))
  dev.off()
  
  }
}
