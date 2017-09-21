### DataFrames
HT = c(h0HT,h1HT,h2HT,h3HT)
WT = c(h0WT,h1WT,h2WT,h3WT)

dataEntropy = data.frame(
  Group = factor(c(rep("HT",4),c(rep("WT",4)))),
  Level = factor(c("H0","H1","H2","H3","H0","H1","H2","H3"), levels=c("H0","H1","H2","H3")),
  Entropy = c(HT,WT))

dataFrameHT = data.frame(Level = factor(c("H0","H1","H2","H3")), Entropy = c(HT))
dataFrameWT = data.frame(Level = factor(c("H0","H1","H2","H3")), Entropy = c(WT))

E_HT = c(c(rep(h0HT, length(TotH1HT))), TotH1HT, TotH2HT, TotH3HT)
LevelHT = c(rep("H0",length(TotH1HT)), rep("H1",length(TotH1HT)),rep("H2",length(TotH2HT)),rep("H3",length(TotH3HT)))
dataFrameHT = data.frame(Level = factor(LevelHT), Entropy = E_HT)

E_WT = c(c(rep(h0WT, length(TotH1WT))), TotH1WT, TotH2WT, TotH3WT)
LevelWT = c(rep("H0",length(TotH1WT)), rep("H1",length(TotH1WT)),rep("H2",length(TotH2WT)),rep("H3",length(TotH3WT)))
dataFrameWT = data.frame(Level = factor(LevelWT), Entropy = E_WT)

### Entropy Plots
install.packages("ggplot2")
require(ggplot2)
ggplot(data=dataEntropy, aes(x=Level, y=Entropy, group=Group, colour=Group)) +
  geom_line() +
  geom_point()

boxplot(dataEntropy$Entropy ~ dataEntropy$Group)
####

### ANOVAs 
entropy.aov = with(dataEntropy, aov(Entropy ~ Group))
summary(entropy.aov)
TukeyHSD(entropy.aov)

HT.aov = with(dataFrameHT, aov(Entropy ~ Level))
summary(HT.aov)
TukeyHSD(HT.aov)

WT.aov = with(dataFrameWT, aov(Entropy ~ Level))
summary(WT.aov)
TukeyHSD(WT.aov)

### t Test
t.test(TotH1HT,TotH1WT)
t.test(TotH2HT,TotH2WT)
t.test(TotH3HT,TotH3WT)

