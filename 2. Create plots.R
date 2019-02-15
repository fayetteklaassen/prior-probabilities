############# Create tables and plots ################
#### Packages ####
library("xtable")
library("ggplot2")
library("tidyr")
library("plyr")

### Initial settings ####
### Set working directory to source file location ####
setwd("Pilot")
data <- read.csv("RawData.csv", header = T, sep = ";")

cbbPalette <- c("#e66101","#fdb863", "#b2abd2","#5e3c99")
figHeight <- 7.5

## output for evlauation
#### Evaluation table (validity reliability) ####
# Face validity
Face <- rbind(summary(data$Q1.3),summary(data$Q2.3),summary(data$Q3.3))
# Feasibility: able
Feas <- rbind(summary(data$Q1.1),summary(data$Q1.2))
# Feasibility: capable
FeasC <- rbind(summary(data$Q2.2),summary(data$Q3.2))
# Reliability
Reliab <- rbind(summary(data$Q2.4),summary(data$Q3.4))

TAB <- rbind(Face, Feas, FeasC, Reliab)
xtable(TAB[,c(4,1,6)])
##### end #####

#### Figure 2: Demonstrating concepts ####

poss_rescale <- t(apply(data[,c(16:19)], 1, 
                        function(x){total <- sum(x); 
                        sapply(x, function(xx){xx/total})
                        })
                  )
plaus_rescale <- t(apply(data[,c(28:31)], 1, 
                         function(x){total <- sum(x); 
                         sapply(x, function(xx){xx/total})
                         })
                   )
predicted <- (1/poss_rescale)*plaus_rescale
colnames(predicted) <- paste0("OV.", 1:4)
pred_rescale <- t(apply(predicted, 1, 
                        function(x){total <- sum(x, na.rm = T); 
                        sapply(x, function(xx){xx/total})
                        })
                  )

AppleP <- data.frame(cbind(#data[,1],
                           poss_rescale,
                           plaus_rescale,
                           data[,20:23],
                           data[,24:27],
                           pred_rescale,
                           data[,32:35]))

Long <- AppleP %>%
  reshape(c(1:24), direction = "long", 
          timevar = "Hypothesis", idvar = "Participant")%>%
  reshape(varying = c(2:7), direction = "long",
          timevar = "Scenario", idvar = c("Participant", "Hypothesis"),
          times = c("Possibility", 
                    "Plausibility",
                    "Bet 1",
                    "Bet 2",
                    "Expectation",
                    "Bet 3"),
          v.names = "Probability") %>%
  mutate(Hypothesis = factor(Hypothesis),
         Scenario = factor(Scenario, 
                           levels = c("Possibility", 
                                      "Plausibility",
                                      "Bet 1",
                                      "Bet 2",
                                      "Expectation",
                                      "Bet 3")))

g_apple <- ggplot(data = Long, aes(x = Scenario, y = Probability)) + 
  theme_bw() + 
  expand_limits(y=c(0,1))+
  scale_x_discrete(breaks = c("Possibility", 
                              "Plausibility",
                              "Bet 1",
                              "Bet 2",
                              "Expectation",
                              "Bet 3")
                   ) +
  geom_bar(stat = "identity", 
           width = .5, 
           aes(fill = Hypothesis), 
           position = "dodge") +
  scale_y_continuous(breaks = c(0, .5,1)) +
  facet_grid(Participant ~ .) + 
  scale_fill_manual(values = cbbPalette)
g_apple

ggsave("probApple.pdf", width = 6.33, height = figHeight, units = "in")

#### Figure 3: Flanker task ####
data$FS.1 <- .5
data$FS.2 <- .5
data$FS.3 <- 1/6
data$FS.4 <- 1/6

rescales <- t(apply(data[,c(39:42)], 1, 
                   function(x){total <- sum(x); 
                   sapply(x, 
                          function(xx){xx/total})
                   })
             )
rescalep <- t(apply(data[,c(66:69)], 1,
                    function(x){total <- sum(x);
                    sapply(x,
                           function(xx){xx/total})
                    })
)

predictedF <- (1/rescalep)*rescales
colnames(predictedF) <- paste0("PP.", 1:4)
pred_rescale <- t(apply(predictedF, 1, 
                        function(x){total <- sum(x, na.rm = T); 
                        sapply(x, function(xx){xx/total})
                        })
)



Flanker <- data.frame(cbind("id" = data[,1], rescalep, rescales, pred_rescale,data[,43:46]))%>%
  reshape(c(2:17),
          direction = "long",
          timevar = "Hypothesis", 
          idvar = "id")%>%
  reshape(c(3:6),
          direction = "long", 
          timevar = "FlankerTask", 
          idvar = c("id", "Hypothesis"), 
          times = c("Possibility", "Plausibility","Prediction", "Bet"), 
          v.names = "Score") %>%
  mutate(Hypothesis = factor(Hypothesis),
         FlankerTask = factor(FlankerTask,
                              levels = c("Possibility", "Plausibility","Prediction", "Bet"))
  )

gg_flank <- ggplot(data = Flanker, 
            aes(x = FlankerTask, y = Score)) + 
  theme_bw() + 
  xlab("") + 
  ylab("Probability / Bet") +
  geom_bar(stat = "identity", 
           width = .5, 
           aes(fill = Hypothesis), 
           position = "dodge") + 
  scale_y_continuous(breaks = c(0,.5,1)) + 
  facet_grid(id ~ .) + 
  scale_fill_manual(values = cbbPalette)
gg_flank 

ggsave("Flanker.pdf", width = 5.83, height = figHeight, units = "in")

#### Figure 4: Own hypotheses ####
plaus_own <- t(apply(data[,c(50:53)], 1, 
                     function(x){total <- sum(x, na.rm = T); 
                     sapply(x, function(xx){xx/total})
                     })
               )
odds_own <- t(apply(data[,c(54:57)], 1, 
                 function(x){total <- sum((1/x), na.rm = T); 
                 sapply(x, function(xx){(1/xx)/total})
                 })
           )

pred_own <- (1/odds_own)*plaus_own
colnames(pred_own) <- paste0("PP.", 1:4)
pred_rescale <- t(apply(pred_own, 1, 
                        function(x){total <- sum(x, na.rm = T); 
                        sapply(x, function(xx){xx/total})
                        })
)


OwnData <- data.frame(cbind("id" = data[,1], odds_own, plaus_own, pred_rescale,
                            data[,c(58:61)])) %>%
  reshape(c(2:17), direction = "long", 
          timevar = "Hypothesis", idvar = "id") %>%
  reshape(varying = c(3:6), direction = "long", 
          timevar = "OwnHypotheses", idvar = c("id", "Hypothesis"), 
          times = c("Possibility", "Plausibility","Prediction", "Value"), 
          v.names = "Score") %>%
  mutate(Hypothesis = factor(Hypothesis),
         OwnHypotheses = factor(OwnHypotheses,
                                levels = c("Possibility", 
                                           "Plausibility", 
                                           "Prediction",
                                           "Value"))
  ) %>%
  na.omit()

g_own <- ggplot(data = OwnData, 
                aes(x = OwnHypotheses, y = Score, na.rm = T)) + 
  theme_bw() + xlab("") + 
  ylab("Probability/Bet") + 
  geom_bar(stat = "identity", width = .5, 
           aes(fill = Hypothesis), position = "dodge") + 
  scale_y_continuous(breaks = c(0,.5,1)) + 
  facet_grid(id ~ .) + 
  scale_fill_manual(values = cbbPalette)
g_own 

ggsave("Own.pdf", width = 5.83, height = figHeight, units = "in")

setwd("..")
