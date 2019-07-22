library(dplyr)
shotmerge1 <- subset(shotmerge, SHOT_CLOCK <=8)
shotmerge2<- select(shotmerge1, "SHOT_RESULT","BMI","Weight","Height","Experience","Pos","CLOSE_DEF_DIST","Age", "CLOSEST_DEFENDER")
shotof<-select(shotmerge1,-BMI,-Weight,-Height,-Experience,-Pos,-Age, -CLOSEST_DEFENDER,-player_name,-CLOSE_DEF_DIST)
shotof$GAME_CLOCK<-as.numeric(shotof$GAME_CLOCK)
shotof$SHOT_RESULT<-as.factor(shotof$SHOT_RESULT)
shotof$SHOT_RESULT=ifelse(shotof$SHOT_RESULT == "made", "1","0")
summary(shotof)

##Descriptive Analysis
#Correlation plot
library(corrplot)
shotof$SHOT_RESULT<- with(shotof, as.integer(SHOT_RESULT))
corrplot(cor(shotof[,-c(1,3,9)]),type='lower')
shotof$SHOT_RESULT<-as.factor(shotof$SHOT_RESULT)
#Boxplot
library(ggplot2)
p1 <- ggplot(shotof,aes(x = SHOT_RESULT,y = SHOT_DIST))+
  geom_boxplot(colour = "red") +#geom_point(colour = "blue") +
  labs(x = "SHOT RESULT",title = "SHOT_NUMBER") +
  theme(plot.title = element_text(hjust = 0.5))
p2 <- ggplot(shotof,aes(x = SHOT_RESULT,y = SHOT_NUMBER))+
  geom_boxplot(colour = "red") +#geom_point(colour = "blue") +
  labs(x = "SHOT RESULT",title = "SHOT_NUMBER") +
  theme(plot.title = element_text(hjust = 0.5))
p3 <- ggplot(shotof,aes(x = SHOT_RESULT,y = DRIBBLES))+
  geom_boxplot(colour = "red") +#geom_point(colour = "blue") +
  labs(x = "SHOT RESULT",title = "SHOT_NUMBER") +
  theme(plot.title = element_text(hjust = 0.5))
p4 <- ggplot(shotof,aes(x = SHOT_RESULT,y = GAME_CLOCK))+
  geom_boxplot(colour = "red") +#geom_point(colour = "blue") +
  labs(x = "SHOT RESULT",title = "CLOSE_DEF_DIST") +
  theme(plot.title = element_text(hjust = 0.5))
p5 <- ggplot(shotof,aes(x = SHOT_RESULT,y = TOUCH_TIME))+
  geom_boxplot(colour = "red") +#geom_point(colour = "blue") +
  labs(x = "SHOT RESULT",title = "CLOSE_DEF_DIST") +
  theme(plot.title = element_text(hjust = 0.5))
p6 <- ggplot(shotof,aes(x = SHOT_RESULT,y = SHOT_CLOCK))+
  geom_boxplot(colour = "red") +#geom_point(colour = "blue") +
  labs(x = "SHOT RESULT",title = "CLOSE_DEF_DIST") +
  theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,nrow = 2)

#Single variables on target variable
plot(shotof$SHOT_DIST,shotof$SHOT_RESULT,pch=1)
lines(smooth.spline(shotof$SHOT_DIST,shotof$SHOT_RESULT,df=2.5))
plot(shotof$DRIBBLES,shotof$SHOT_RESULT,pch=1)
lines(smooth.spline(shotof$SHOT_DIST,shotof$SHOT_RESULT,df=2.5))
plot(shotof$SHOT_CLOCK,shotof$SHOT_RESULT,pch=1)
lines(smooth.spline(shotof$SHOT_DIST,shotof$SHOT_RESULT,df=2.5))

library(vcd)
pa<-mosaic(SHOT_RESULT~PERIOD,data = shotof, highlighting_fill = c(2,3), highlighting_dir='right')
pb<-mosaic(SHOT_RESULT~LOCATION,data = shotof, highlighting_fill = c(2,3), highlighting_dir='right')
pc<-mosaic(SHOT_RESULT~PTS_TYPE,data = shotof, highlighting_fill = c(2,3), highlighting_dir='right')

#Interaction
ggplot2::ggplot(shotof, aes(x = factor(PERIOD), y = SHOT_DIST,
                          colour = SHOT_RESULT)) +
  geom_boxplot(position = position_dodge(width = .75),notch = T) +
  theme_bw()+
  geom_smooth(aes(group = SHOT_RESULT), method="loess", size = 1, se = T)+
  scale_fill_discrete(name="PERIOD") +
  xlab("Order of Period") +
  ylab("Shot Distance") +
  theme(axis.title.x = element_text(face='plain',size=16,hjust=0.5),
        axis.title.y = element_text(face='plain',size=16,vjust=1),
        axis.text.x = element_text(face='plain',size=14,color='black'),
        axis.text.y = element_text(face='plain',size=14,color='black'),
        legend.title = element_text(face="plain", color="black", size=14),
        legend.text = element_text(face="plain", color="black", size=12)) +
  theme(panel.background = element_rect(fill='light gray'))

ggplot2::ggplot(shotof, aes(x = factor(PTS_TYPE), y = SHOT_CLOCK,
                            colour = SHOT_RESULT)) +
  geom_boxplot(position = position_dodge(width = .75),notch = T) +
  theme_bw()+
  geom_smooth(aes(group = SHOT_RESULT), method="loess", size = 1, se = T)+
  scale_fill_discrete(name="PERIOD") +
  xlab("Order of Period") +
  ylab("Shot Distance") +
  theme(axis.title.x = element_text(face='plain',size=16,hjust=0.5),
        axis.title.y = element_text(face='plain',size=16,vjust=1),
        axis.text.x = element_text(face='plain',size=14,color='black'),
        axis.text.y = element_text(face='plain',size=14,color='black'),
        legend.title = element_text(face="plain", color="black", size=14),
        legend.text = element_text(face="plain", color="black", size=12)) +
  theme(panel.background = element_rect(fill='light gray'))

###Modeling
mod1 <- glm(Y~FlowRate+ProfitRate+FinancialLeverage+TurnoverRate+AssetLiabilityRatio+EquityRatio,
            data = logidata,family=binomial(link=logit))
summary(mod1)
mod2 <- step(mod1)
summary(mod2)

##Prediction
logidata$preY <- predict(mod2,logidata,type="response")
logidata$preYY <- ifelse(logidata$preY>0.5,1,0)
logidata$preY

##Assessment
table(logidata$Y,logidata$preYY)
library(ROCR)
pred <-prediction(logidata$preY,logidata$Y)
tpr <- pred@tp[[1]]/max(pred@tp[[1]])
fpr <- pred@fp[[1]]/max(pred@fp[[1]])
plot(fpr, tpr, type = "1")

g <- roc(Y ~ preY, data = logidata,direction="<")
plot(g,col="yellow", lwd=3, main="The turtle finds its way")   

rocplot(mod2)
rocplot(mod2) +
  ggtitle("模型的ROC曲线")+
  theme(plot.title = element_text(hjust = 0.5))



