library(ggplot2)
library(data.table)
library(scales)


ct.binned <- overp.indiv.ct[, list(confidence = mean(MeanMaxFcast),
                                   hit_rate   = mean(HitRate),
                                   ci_hit_rate = 1.96*sd(HitRate)/sqrt(.N),
                                   count      = .N),
                            by=c("bin,ct")]
ct.binned[, ct2 := ordered(ct, levels = c("1a","1b","4a","4b"), labels = c("Auto No Train", "Auto Trained", "Team No Train", "Team Trained"))]

cac.ct <- 
  ggplot(ct.binned, aes(confidence, hit_rate, shape = ct2, color = ct2)) +
  geom_segment(aes(x = .2, y = .2, xend = 1, yend = 1), linetype=2, color="grey50") +
  geom_line(size=0.9, aes(group = ct)) + 
  geom_point(size=3) +
  scale_shape_manual(values = c(19, 17, 4, 15)) +
  scale_x_continuous(labels=percent, limits=c(.2,1)) +
  scale_y_continuous(labels=percent, limits=c(.2,1)) +
  labs(x="Confidence", 
       y="Accuracy",
       color="Experimental \n Condition",
       shape="Experimental \n Condition") + 
  theme_bw(base_size=16)