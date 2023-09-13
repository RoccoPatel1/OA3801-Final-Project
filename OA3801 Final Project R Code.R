dat = read.csv('Master_Data.csv')#Read in the data file and save it as the variable dat
dat$RB#Check to make sure it read in properly with columns I want
lr1=lm(dat$W~dat$RB)#First Linear Regression to look at Wins as a function of RB's
summary(lr1)#Check the coefficients and p value, p = .2922
plot(lr1)#Look at the plots of the model
lr2 = lm(dat$W~dat$WR)#Same as LR1, but for WR
plot(lr2)#plots
summary(lr2)#Check output, p = .1849
plot(dat$RB,dat$W)#look at RB spending against Wins scatterplot
plot(dat$WR,dat$W)#look at WR spending against Wins scatterplot
lr3=lm(dat$W~dat$RB+dat$WR)#now combining WR and RB into a LR model
summary(lr3)#checking, both not significant...p(RB)=.273, p(WR)=.174
plot(dat$W~dat$RB+dat$WR, type='h')#checking individual histograms to see if it's any different than the scatterplots
lr4=lm(dat$PF~dat$RB+dat$WR)#Checking RB&WR against Points For
summary(lr4)#Check, RB not significant with p = .3352, but WR is at the .05 level with p = .0342
plot(lr4)#plots
plot(dat$PF~dat$RB+dat$WR)#checking scatterplots again, can see a slight linear relationship with WR and PF 
lr5=lm(dat$W~dat$RB+dat$WR+dat$QB+dat$TE+dat$OL)#model with all offensive positions
summary(lr5)#QB and OL significant, p(QB)=.0311, p(OL)=.0703
plot(dat$W~dat$RB+dat$WR+dat$QB+dat$TE+dat$OL)#Checking scatterplots
lr6=lm(dat$W~dat$Offense+dat$Defense)#model with defense and offense total spending against Wins
summary(lr6)#check, both significant, p(offense)=.09029, p(defense)=.00634
plot(dat$W~dat$Offense+dat$Defense)#checking scatterplots
lr7=lm(dat$L~dat$RB+dat$WR)#model with RB&WR against losses
summary(lr7)#check, both not significant, p(RB)=.462, p(WR)=.301
plot(dat$L~dat$RB+dat$WR)#look at scatterplots
plot(dat$W~dat$Total.Spending)#Check scatterplot
lr8=lm(dat$W~dat$Total.Spending)#overall model to see if total spending impacts wins
summary(lr8)#total spending is very significant with p =.000189, this intuitively makes sense
lr9=lm(W~RB+WR+TE+QB+OL+Defense, data=dat)#last model looking at all indiv. offense spots then total defense
summary(lr9)#check, QB again significant with p = .02453, and defense significant with p = .00287
plot(W~RB+WR+TE+QB+OL+Defense, data=dat)#checking scatterplots
