#Packages
#This is for the descriptive statistics
library(psych)

#For Levene's test 
#to test for assumption of normality
library(car)

#This helps calculate power
#for ANOVA and Tukey's HSD.
library(pwr)

#This is for (most of) our plots
#like when graphing for critical points, box plots, etc
library(ggplot2)


#This is so that lm objects in the autoplot function
#can output diagnostic plots like the lm method
#in the plot function (plot.lm).
library(ggfortify)

#This helps making multiple comparisons for mean differences and trends 
#like pairwise comparisons like a Tukey correction.
library(emmeans)

library(dplyr)

library(reshape2)

library(tidyr)
#-------------------------------------------------------
#to import sentiment_analysis_results dataset
sentiment_analysis_results="https://raw.githubusercontent.com/ChrisxCross2003/DS4002Project1/refs/heads/main/Data/sentiment_analysis_results.csv"
sentiment=read.csv(sentiment_analysis_results)
#---------------------------------------------------------------
#to make sure every theater group is the same size

#checking each theater group size
test=sentiment%>%
  group_by(Theater.Name)%>%
  count()
test

#changing each group to have 941 reviewers
sentiment=sentiment%>%
  group_by(Theater.Name)%>%
  slice_head(n = 941)%>%
  ungroup()

#double check
test=sentiment%>%
  group_by(Theater.Name)%>%
  count()
test

View(sentiment)
#--------------------------------------
#change column names

# For the original df categories under Theater.Name. Replace space with . (ex. Regal Cinema-> Regal.Cinema)
sentiment<- sentiment%>%
  mutate(Theater.Name = case_when(
    trimws(Theater.Name) == "Alamo Drafthouse" ~ "Alamo.Drafthouse",
    trimws(Theater.Name) == "Regal Cinema" ~ "Regal.Cinema",
    trimws(Theater.Name) == "Violet Crown" ~ "Violet.Crown",
    TRUE ~ Theater.Name  # Keeps other values unchanged
  ))
View(sentiment)
#------------------------------------------------------------------------------
#to make original df into wide df

#Subsetting for only Theater Name, Star Rating, and Author Name columns
wideSentiment=sentiment[,c("Theater.Name", "Sentiment.Polarity", "Author.name")]

View(wideSentiment)

#Group by Author Name and Theater Name and summarize 
wideSentiment=sentiment%>%
  group_by(Author.name, Theater.Name) %>%
  summarise(Sentiment.Polarity = mean(Sentiment.Polarity, na.rm = TRUE))

View(wideSentiment)

wideSentiment<- wideSentiment%>%
  pivot_wider(names_from = Theater.Name, values_from = Sentiment.Polarity)

#long movies
longSentiment=wideSentiment%>% 
  pivot_longer(cols=c('Alamo.Drafthouse', 'Regal.Cinema', 'Violet.Crown'),
               names_to='Theater.Name',
               values_to='Sentiment.Polarity') 
View(longSentiment)
#------------------------------------------------------
#boxplot for every theater 

#Alamo.Drafthouse
ggplot(data=wideSentiment) + geom_histogram(aes(x =Alamo.Drafthouse),
                                         color = "black", fill = "gray", binwidth =1) +
  xlab("Sentiment Score") + 
  ggtitle("Movie Theater Sentiment Score") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title


#Regal.Cinema
ggplot(data=wideSentiment) + geom_histogram(aes(x =Regal.Cinema),
                                         color = "black", fill = "gray", binwidth =1) +
  xlab("Sentiment Score") + 
  ggtitle("Movie Theater Sentiment Score") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title

#Violet Crown
ggplot(data=wideSentiment) + geom_histogram(aes(x =Violet.Crown),
                                         color = "black", fill = "gray", binwidth =1) +
  xlab("Sentiment Score") + 
  ggtitle("Movie Theater Sentiment Score") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title

#---------------------------------------------------------------------------------------------------
#Finding number of participants using the nrow() function (number of rows)
N=nrow(longSentiment)
N
#----------------------------------
#f is just a transformation of R2. It is another way of reporting effect size.
#Hypothetical for medium and large effect size
R2=0.06
correspondingf=sqrt(R2/(1-R2))
pwr.anova.test(k=3, n=NULL, sig.level = .05, f=correspondingf, power= .8) #k=# of groups, pvalue=0.05, power=effect size of 80%
#need 52 reviewers for each theater to get 80% power and medium effect size.


#Large Effect Size (R2=.14)
pwr.anova.test(k=3, n=NULL, sig.level=.05, f=sqrt(.14/(1-.14)), power= .80)
#need 21 reviewers for 80% power and large effect size
#-------------------------------------------------------
#Plot to show power calculations for medium and large effect size
plot(pwr.anova.test(k=3, n=NULL, sig.level=.05, f=sqrt(.06/(1-.06)), power= .8))
plot(pwr.anova.test(k=3, n=NULL, sig.level=.05, f=sqrt(.14/(1-.14)), power= .8))
#----------------------------------------------------
#Violin plots to see visual representation of out data
ggplot(longSentiment,aes(x=Theater.Name,y=Sentiment.Polarity)) +
  geom_violin(aes(fill=Theater.Name), show.legend = FALSE) +
  scale_fill_manual(values = c("#994808","#f79545","#ff65cc")) +
  xlab("Theater") + ylab("Sentiment Score") +
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title
#----------------------------------------------------------------
#Checking normality of the populations by plotting a qq-plot:
autoplot(lm(Sentiment.Polarity~Theater.Name,data=longSentiment),
         which = 2, # which diagnostic plot we want (QQ plot)
         ncol = 1) + # we're only outputting one diagnostic plot, so the number of plot columns is 1
  theme_bw() + 
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title

#Because the points are not all close to the qq-line, 
#the distribution of the number of star ratings is not relatively normal.

#-------------------------------------------------------------------
#Shapiro test to give exactly the pvalue to prove that 
#our data does not havenormal distribution
shapiro.test(wideSentiment$Alamo.Drafthouse)
shapiro.test(wideSentiment$Regal.Cinema)
shapiro.test(wideSentiment$Violet.Crown)
#Every theater has pvalue>0.05, therefore, 
#assumption of normality is violated
#-------------------------------------------------------------------------------------
leveneTest(Sentiment.Polarity~Theater.Name, data=longSentiment)
#significant so does violate normality of variation
#----------------------------------------------------
#Attempt to transform data so that data reflect normal distribution

#Original data histogram
originalDV=ggplot() + geom_histogram(aes(x = longSentiment$Sentiment.Polarity),
                                     color = "black", fill = "gray", bins = 20) +
  xlab("Sentiment Score") + 
  ggtitle("Histogram of Sentiment Score") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title

originalDV
#Checking normality
shapiro.test(longSentiment$Sentiment.Polarity)

#sqrt transformation to see if data becomes normally distributed
SentimentSqrt=sqrt(longSentiment$Sentiment.Polarity)

sqrtDV=ggplot() + geom_histogram(aes(x =SentimentSqrt),
                                 color = "black", fill = "gray", bins = 20) +
  xlab("Sentiment Score") + 
  ggtitle("Histogram of Sentiment Score") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title
sqrtDV

#Checking normality 
shapiro.test(starRatingSqrt)
#-----------
#log transformation to see if data becomes normally distributed
SentimentLog=log10(longSentiment$Sentiment.Polarity)

logDV<- ggplot() + geom_histogram(aes(x = SentimentLog),
                                  color = "black", fill = "gray", bins = 20) +
  xlab("Sentiment Score") + 
  ggtitle("Histogram of Sentiment Score") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title

logDV

# Checking with Shapiro-Wilk test (of DV)
shapiro.test(SentimentLog)


#---------------------------
#Inverse transformation to see if data becomes normally distributed

SentimentInv= 1/((-1)*(longSentiment$Sentiment.Polarity))

invDV<- ggplot() + geom_histogram(aes(x = SentimentInv),
                                  color = "black", fill = "gray", bins = 20) +
  xlab("Sentiment Score") + 
  ggtitle("Histogram of Sentiment Score") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title

invDV

#Check with Shapiro-Wilk test: 
shapiro.test(SentimentInv)

#Out of everything, sqrt transformation does help with out data being normally distributed
originalDV
sqrtDV
logDV
invDV
#--------------------------------------------
#To add a column to our data set with the transformed sentiment scores! 

longSentiment$Sentiment.Polarity.Sqrt<- sqrt(longSentiment$Sentiment.Polarity)
#---------------------------------------------
#Total sample minus number of groups
TheaterDFerror <- 7890

#Number of groups minus 1
TheaterDFbetween <- 2
#----------------------------------------
#To graph out F distribution
x <- seq(0,4,by=.01)
fDist <- df(x, TheaterDFbetween, TheaterDFerror)
fDistData <- data.frame(x, fDist)

ggplot(data = fDistData) + 
  geom_line(aes(x = x, y = fDist), col = "black", lwd = 1) +
  xlab("F-value") + ylab("Density") + 
  ggtitle("Our F(4,789) Distribution") + 
  theme_bw() + theme(panel.grid = element_blank(), # No grid lines
                     plot.title = element_text(hjust = 0.5)) # Center title
#---------------------------------------------------------------
#We know df's necessary for the F distribution, 
#so we can determine our cutoff value for our ANOVA test.  
#This is done with the qf function. 
#Our alpha is 0.05 (from the right tail), then our cutoff value will be:
qf(0.05,TheaterDFbetween, TheaterDFerror, lower.tail=FALSE)
#lower.tail=FALSE BC WE ARE LOOKING AT ONE DIRECTION, WE DO NOT CARE ABOUT LOWER TAIL SO IT IS FALSE

#We can plot this cutoff value using this plot:
colors <- c("black", "blue")
labels <- c("Our F(2,45) Distribution", "Cutoff Value")

# Putting the constant colors inside aes to force a legend to be created,
# Then adjusting it with scale_color_manual
ggplot(data = fDistData) + 
  geom_line(aes(x = x, y = fDist, col = "black"), lwd = 1) +
  # We use show.legend is false here to only inherit geom_line's horizontal
  # lines in the legend - otherwise, we'll see both horizontal and vertical
  # lines in the legend (as a cross)
  geom_vline(aes(xintercept = qf(0.05, 2, 45, lower.tail = FALSE),
                 color = "blue"), show.legend=FALSE) +
  scale_color_manual(name = "",
                     values = colors,# Forces colors
                     breaks = colors,# Forces order
                     labels = labels) +# Gives us our labels
  xlab("F-value") + ylab("Density") + 
  ggtitle("Our F(2,45) Distribution") + 
  theme_bw() + theme(panel.grid = element_blank(), # No grid lines
                     plot.title = element_text(hjust = 0.5)) # Center title

#---------------------------------------------
#ANOVA

TheaterLm <- lm(Sentiment.Polarity.Sqrt~Theater.Name, data=longSentiment)
anova(TheaterLm)
#pvalue=3.361e-05. This is less than 0.05 so our sentiment scores between theater groups is significant. 
#Fvalue is 10.377 which is higher than critical cutoff
#-----------------------------------------------------------------------------------------------------------
#If our results were significant, we would evaluate our final effect size.
#Effect size R^2 = SSbetween / (SSbetween + SSerror)
#SSbetween + SSerror = SStotal for a one-way ANOVA
#SSbetween score found in Theater's Sum sq from ANOVA
#SSerrr score found in Residuals Sum sq from ANOVA
TheaterSSbetween=1.999
TheaterSSerror=81.146
#R squared for the theater
TheaterR2 <- TheaterSSbetween / (TheaterSSbetween+TheaterSSerror)
TheaterR2
#R2=0.024 so it is between small (0.01) and medium effect size (0.06)
#---------------------------------------------------------------------------
#Tukey's HSD 
#realistically to see which groups are statisically significant
library(emmeans)
ComparisonMeans <- emmeans(TheaterLm, ~Theater.Name)
contrast(ComparisonMeans, method="pairwise", adjust="Tukey")
#the pvalues for Alamo.Drafthouse - Regal.Cinema and Regal.Cinema - Violet.Crown
#are less than 0.05 so they have significant differences between sentiment scores
#----------------------------------------------------------------------------------
#Bar chart to double check
colors=c("#f79545","#ff6599","#994998")
labels=c("Regal Cinema","Alamo Drafthouse","Violet Crown")

# Creating dataset of means and standard errors
TheaterDataMeans=sapply(wideSentiment[, sapply(wideSentiment, is.numeric)], mean, na.rm = TRUE)
TheaterDataMeans

#only finding sd of each group
TheaterDataSD <- sapply(wideSentiment[, sapply(wideSentiment, is.numeric)], sd, na.rm = TRUE)
TheaterDataSD

#checking to see if each has the same length
length(labels)
length(TheaterDataMeans)
length(TheaterDataSD)

#Dataset of means and standard errors
meansAndCIs <- data.frame("Group" = factor(labels, levels = labels),
                          "Means" = TheaterDataMeans,
                          # 16 = sample size per group
                          "Std.Errors" = TheaterDataSD/sqrt(16))

# Creating y-axis limits
ylim2SE <- c(0, ceiling(max(meansAndCIs$Means+2*meansAndCIs$Std.Errors))+1)
ylim2SE

# Creating the barplot
ggplot(meansAndCIs, aes(x = Group, y = Means, fill = Group))+
  geom_bar(stat="identity", position = "dodge", col="black") +
  scale_fill_manual(values=colors) +
  xlab("Theater") + ylab("Sentiment Score") + 
  ggtitle("Every Theater Brand Means") + 
  geom_errorbar(aes(ymin=Means-2*Std.Errors, ymax=Means+2*Std.Errors),
                width=.5,
                position=position_dodge(.9)) +
  coord_cartesian(ylim = ylim2SE) +
  theme_bw() + theme(panel.grid = element_blank(), # No grid lines
                     plot.title = element_text(hjust = 0.5)) # Center title

