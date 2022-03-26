library(modelr)
library(broom)
library(ggplot2)
media = read.csv("A:/R_datasets/Social_Network_Ads.csv")
view(media)
str(media)
sum(is.na(media))
sample = sample(nrow(media),nrow(media)*.7)
train = media[sample,]
test=media[-sample,]
media %>%
  mutate(prob = ifelse(Purchased == 1,1,0)) %>%
  ggplot(aes(Age,prob))+
  geom_point(alpha = 0.15)+
  geom_smooth(method = "glm",method.args = list(family = "binomial"))+
  xlab("Age")+
  ylab("Probability of Purchased")
  
model = glm(Purchased~Age,data = train,family = 'binomial')
summary(model)
m1 = glm(Purchased~Age, data = test,family = 'binomial')
summary(m1)
predict(m1,data.frame(Age = c(48)),type= 'response')
