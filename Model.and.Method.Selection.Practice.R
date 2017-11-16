#Assignment: From just the question and dataset, figure out which method is most appropriate for
#answering the question purposed.


#############################################################################

#Question 1:Determine which factors impact the setiment response. 
#Interest was in seeing whether proximity, contact and norms had an impact on whether the 
#respondent favored segregation or integration

############################################################################

data <- read.table("http://data.princeton.edu/wws509/datasets/housing.dat")
data

names(data) <- c("Location", "Con.Freq", "Norms.Fav", "Num.Fav", "Num.Unfav")
data



#EDA
summary(data)
str(data)
head(data)

#Univariate responses and all ind. variables are factors, response data is count data,  Use glm for this question


#Create model
fit <- glm(cbind(Num.Fav, Num.Unfav) ~ factor(Location) + factor(Con.Freq) + factor(Norms.Fav), family = binomial, data = data)
summary(fit) #All sig except Location
#AIC 46.948

#Diagnostic tests

#Residual deviance:  2.2378  on 4  degrees of freedom
1-pchisq(2.2378,4) #P value is .69, so H): model is adequate

##Look at residuals
plot(fit$residuals) #No pattern so good

#To get probablity of how many want integratoin for each combination of ind variables
fit$fitted.values



#########################################################################################

#Question 2: Interest is in determining the effect of type of diet and gender on weight loss

#########################################################################################


library(readr)
Diet <- read_csv("C:/Users/awong/Downloads/Diet.csv")
View(Diet)

#Create wieght loss column in table
Diet$weightloss <- Diet$pre.weight-Diet$weight6weeks

#EDA

head(Diet)
#weightloss = dbl
#Diet = int but should be factor 
#Gender = int but should be factor

#Since weight loss is continous and Gender and diet are factors we will use
#2 way anova

anova(aov(weightloss~as.factor(gender)*as.factor(Diet), data = Diet))


#or something as commond right above
fit<-aov(weightloss~as.factor(gender)*as.factor(Diet), data = Diet)
summary(fit)
#Result = Diet and interaction between Diet and gender is sig


#Interaction effects looks significant. So need to focus on interactation effect
#Do an interaction plot
interaction.plot(Diet$gender, Diet$Diet, Diet$weightloss) ##This plot shows that 
#as Diets 3,2,1  goes from female(0) to male(1), they intersect and the means they are significant. 


#explore more
dataF <- Diet[which(Diet$gender == "0"),] ##Pulls those rows
dataM <- Diet[which(Diet$gender == "1"),]
##Separates toothgroth by supp type
anova(aov(weightloss~ as.factor(Diet), dataF)) ##Diet is highly signifacnt within females
anova(aov(weightloss~ as.factor(Diet), dataM)) ##Diet is not highly sigificant within males

#Check normaility
qqnorm(dataF$weightloss) #Looks okay
qqnorm(dataM$weightloss) #Looks okay

##Do the same thing by dose instead
data1 <- Diet[which(Diet$Diet == "1"),]
data2 <- Diet[which(Diet$Diet == "2"),]
data3 <- Diet[which(Diet$Diet == "3"),]

anova(aov(weightloss~as.factor(gender), data1)) #not sig
anova(aov(weightloss~as.factor(gender), data2)) #not Sig
anova(aov(weightloss~as.factor(gender), data3)) #Not Sig

#Check normality
qqnorm(data1$weightloss) #okay
qqnorm(data2$weightloss) #okay
qqnorm(data3$weightloss) #okay

##Diets effect weightloss significantly in females. 

##########################################################################################

#Question 3: Which factors affect the distance of the punt?

#########################################################################################


data <- read.table("http://www.statsci.org/data/general/punting.txt", header = T)
data

#one response variable, ind variables are not factors, response is continuous, obs are ind. 
#Use multiple regression

#EDA
summary(data)
#Dist = num
#R_strength = int
#L_strength = int
#R_Flexibility = int
#L_Flexibility = int
#O_strength = Num

#Check normal
qqnorm(data$Distance) #Response variable looks normal

#Check correlation
library(corrplot)
cor(data)
corrplot(cor(data), method = "number")
#There are a lot of correlations between the variables
#R_strength looks to be correlated with L_Strength (.9), R_Flexitbility(.77), and maybe O_Strength(.61)


#Create model
fit <- lm(Distance ~ R_Strength + L_Strength + R_Flexibility + L_Flexibility + O_Strength, data = data)
summary(fit) #Non are significant, probably cause there is a lot of correlation. 

#Now let's try to find the best model using AIC
library(MASS)
step <- stepAIC(fit)
step$anova 
#both step and step$anova tell me that "best" model should be Distance ~ R_strength + O_strength. AIC = 69.69
AICfit <- lm(Distance ~ R_Strength + O_Strength, data = data)
summary(AICfit) #Both are sig
