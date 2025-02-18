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
#to import movies_df dataset
movieTheaterData= "https://raw.githubusercontent.com/ChrisxCross2003/DS4002Project1/refs/heads/main/Data/Movie_Theater.csv"

movies_df = read.csv(movieTheaterData)

View(movies_df)
#---------------------------------------------------------------
#to make sure every theater group is the same size

#checking each theater group size
test=movies_df%>%
  group_by(Theater.Name)%>%
  count()
test

#changing each group to have 941 reviewers
movies_df=movies_df%>%
  group_by(Theater.Name)%>%
  slice_head(n = 941)%>%
  ungroup()

#double check
test=movies_df%>%
  group_by(Theater.Name)%>%
  count()
test

View(movies_df)
#--------------------------------------
#change theater names under Theater.Name column
# For the original df categories under Theater.Name. Replace space with . (ex. Regal Cinema-> Regal.Cinema)
movies_df=movies_df %>%
  mutate(Theater.Name = case_when(
    trimws(Theater.Name) == "Alamo Drafthouse" ~ "Alamo.Drafthouse",
    trimws(Theater.Name) == "Regal Cinema" ~ "Regal.Cinema",
    trimws(Theater.Name) == "Violet Crown" ~ "Violet.Crown",
    TRUE ~ Theater.Name  # Keeps other values unchanged
  ))
#------------------------------------------------------------------------------
#to make original df into wide df

#Subsetting for only Theater Name, Star Rating, and Author Name columns
wideMovies=movies_df[,c("Theater.Name", "Star.rating", "Author.name")]

View(wideMovies)

#movies_df$Theater.Name <- as.character(movies_df$Theater.Name)
#movies_df$Star.rating <- as.numeric(movies_df$Star.rating)

#newMovies_df <- newMovies_df %>% filter(!is.na(Star.rating))

#Group by Author Name and Theater Name and summarize 
wideMovies <- wideMovies%>%
  group_by(Author.name, Theater.Name) %>%
  summarise(Star.rating = mean(Star.rating, na.rm = TRUE))

View(wideMovies)

wideMovies<- wideMovies%>%
  pivot_wider(names_from = Theater.Name, values_from = Star.rating)

wideMovies<- na.omit(wideMovies)
View(wideMovies)

#long movies
longMovies=wideMovies%>% 
  pivot_longer(cols=c('Alamo.Drafthouse', 'Regal.Cinema', 'Violet.Crown'),
                    names_to='Theater.Name',
                    values_to='Star.rating') 
View(longMovies)
#------------------------------------------------------
#For the wide df. keep Auther.Name the same. Replace space with . (ex. Regal Cinema-> Regal.Cinema)
colnames(wideMovies) <- c('Author.name','Alamo.Drafthouse','Regal.Cinema','Violet.Crown') 
#--------------------------------------------------------
#boxplot for every theater 

#Alamo.Drafthouse
ggplot(data=wideMovies) + geom_histogram(aes(x =Alamo.Drafthouse),
                                         color = "black", fill = "gray", binwidth =3) +
  xlab("Star Rating") + 
  ggtitle("Movie Theater Average Star Rating") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title


#Regal.Cinema
ggplot(data=wideMovies) + geom_histogram(aes(x =Regal.Cinema),
                                                 color = "black", fill = "gray", binwidth =3) +
  xlab("Star Rating") + 
  ggtitle("Movie Theater Average Star Rating") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title


#Violet Crown
ggplot(data=wideMovies) + geom_histogram(aes(x =Violet.Crown),
                                                 color = "black", fill = "gray", binwidth =3) +
  xlab("Star Rating") + 
  ggtitle("Movie Theater Average Star Rating") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title

#---------------------------------------------------------------------------------------------------
#Finding number of participants using the nrow() function (number of rows)
N=nrow(longMovies)
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
ggplot(longMovies,aes(x=Theater.Name,y=Star.rating)) +
  geom_violin(aes(fill=Theater.Name), show.legend = FALSE) +
  scale_fill_manual(values = c("#994808","#f79545","#ff65cc")) +
  xlab("Theater") + ylab("Star Rating") +
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title
#----------------------------------------------------------------
#Checking normality of the populations by plotting a qq-plot:
autoplot(lm(Star.rating~Theater.Name,data=longMovies),
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
shapiro.test(wideMovies$Alamo.Drafthouse)
shapiro.test(wideMovies$Regal.Cinema)
shapiro.test(wideMovies$Violet.Crown)
#Every theater has pvalue>0.05, therefore, 
#assumption of normality is violated
#-------------------------------------------------------------------------------------
leveneTest(Star.rating~Theater.Name, data=longMovies)
#not significant so does not violate normality of variations
#----------------------------------------------------
#Attempt to transform data so that data reflect normal distribution

#Original data histogram
originalDV=ggplot() + geom_histogram(aes(x = longMovies$Star.rating),
                                                  color = "black", fill = "gray", bins = 20) +
  xlab("Star Rating") + 
  ggtitle("Histogram of Star Ratings") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title

originalDV
#Checking normality
shapiro.test(longMovies$Star.rating)

#sqrt transformation to see if data becomes normally distributed
starRatingSqrt=sqrt(longMovies$Star.rating)

sqrtDV=ggplot() + geom_histogram(aes(x = starRatingSqrt),
                                              color = "black", fill = "gray", bins = 20) +
  xlab("Square Root of Star Rating") + 
  ggtitle("Histogram - Square Root Transformation of Star Rating") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title

sqrtDV

#Checking normality 
shapiro.test(starRatingSqrt)
#-----------
#log transformation to see if data becomes normally distributed
StarRatingLog=log10(longMovies$Star.rating)

logDV<- ggplot() + geom_histogram(aes(x = StarRatingLog),
                                             color = "black", fill = "gray", bins = 20) +
  xlab("Log of Reaction Time") + 
  ggtitle("Histogram - Log Transformation of Mean Reaction Time") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title

logDV

# Checking with Shapiro-Wilk test (of DV)
shapiro.test(StarRatingLog)


#---------------------------
#Inverse transformation to see if data becomes normally distributed

StarRatingInv= 1/((-1)*(longMovies$Star.rating))

invDV<- ggplot() + geom_histogram(aes(x = StarRatingInv),
                                             color = "black", fill = "gray", bins = 20) +
  xlab("Inverse of Reaction Time") + 
  ggtitle("Histogram - Inverse Transformation of Mean Reaction Time") + 
  theme_bw() +
  theme(panel.grid = element_blank(), # No grid lines
        plot.title = element_text(hjust = 0.5)) # Center title

invDV

#Check with Shapiro-Wilk test: 
shapiro.test(StarRatingInv)

#Out of everything, transformation does not help with out data being normally distributed
originalDV
sqrtDV
logDV
invDV

#---------------------------------------------
#Total sample minus number of groups
TheaterDFerror <- 45

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

TheaterLm <- lm(Star.rating~Theater.Name, data=longMovies)
anova(TheaterLm)
#pvalue=0.7661. This is greater than 0.05 so our star ratings between theater groups is not significant. 
#Fvalue is 0.2681 which is not higher than critical cutoff
#-----------------------------------------------------------------------------------------------------------
#If our results were significant, we would evaluate our final effect size.
#Effect size R^2 = SSbetween / (SSbetween + SSerror)
#SSbetween + SSerror = SStotal for a one-way ANOVA
#TheaterSSbetween=
#TheaterSSerror=
#R squared for the theater
#TheaterR2=TheaterSSbetween / (TheaterSSbetween+TheaterSSerror)
#TheaterR2
#---------------------------------------------------------------------------
#Tukey's HSD 
#realistically to see which groups are statisically significant
library(emmeans)
ComparisonMeans <- emmeans(TheaterLm, ~Theater.Name)
contrast(ComparisonMeans, method="pairwise", adjust="Tukey")
#the pvalues for every group comparison is not less than 0.05
#therefore, none of our groups have significant difference in star ratings
#----------------------------------------------------------------------------------
#Bar chart to double check
colors=c("#f79545","#ff6599","#994998")
labels=c("Alamo.Drafthouse","Regal.Cinema","VioletCrown")

# Creating dataset of means and standard errors
TheaterDataMeans=sapply(wideMovies[, sapply(wideMovies, is.numeric)], mean, na.rm = TRUE)
TheaterDataMeans

#only finding sd of each group
TheaterDataSD <- sapply(wideMovies[, sapply(wideMovies, is.numeric)], sd, na.rm = TRUE)
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
  xlab("Theater") + ylab("Star Rating") + 
  ggtitle("Every Theater Brand Means") + 
  geom_errorbar(aes(ymin=Means-2*Std.Errors, ymax=Means+2*Std.Errors),
                width=.5,
                position=position_dodge(.9)) +
  coord_cartesian(ylim = ylim2SE) +
  theme_bw() + theme(panel.grid = element_blank(), # No grid lines
                     plot.title = element_text(hjust = 0.5)) # Center title

