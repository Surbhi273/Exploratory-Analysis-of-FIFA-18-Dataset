#installing package ggthemes
install.packages("ggthemes", repos = "https://cran.rstudio.com") 
install.packages("DT")
install.packages("shiny")
###Loading all packages
library(data.table)         #  data
library(tidyverse)          # Data manupulation
library(tibble)             # used to create tibbles
library(tidyr)              # used to tidy up data
library(dplyr)              # used for data manipulation
library(DT)                 # used for datatable function for displaying dataset
library("ggthemes")         # for using themes for plots
library("shiny")

#Importing dataset and converting to tibble
fifa18 <- as_tibble(fread("https://raw.githubusercontent.com/ameyaj2910/Data/master/CompleteDataset.csv", showProgress = FALSE))

class(fifa18)               # class of the entire dataset

colnames(fifa18)
dim(fifa18)                 # dimesnions of the data set

datatable(head(fifa18, 6),options = list(scrollX = TRUE, pageLength = 3))
####Converting variables into appropriate data type####
fifa18$Value <- gsub(".*¬", "", fifa18$Value)   # Converting Value to proper format
fifa18$Value <- gsub("M$", "", fifa18$Value)    #removing million character 'M'from player's value
fifa18$Wage <- gsub(".*¬", "", fifa18$Wage)     # Converting Wage to proper format
fifa18$Wage <- gsub("K$", "", fifa18$Wage)      #removing thousand character 'K' from wage

fifa18 <- fifa18 %>%  subset(Value != 0) %>% subset(Wage != 0)      # removing all players' whose Valuation and Wage is 0.

fifa18 <- as.data.frame(fifa18)    #converting into data frame as tibble doesnt give appropriate results for sub function

for (i in 14:47)                   # Converting columns with player attributes to numeric
{
  fifa18[,i] <- sub("\\+.*", "", fifa18[,i])
  fifa18[,i] <- sub("\\-.*", "", fifa18[,i])
  
}
fifa18 <- as_tibble(fifa18)   #Converting back to tibble
colnames(fifa18)[11] <- "Value in Million Euros"
colnames(fifa18)[12] <- "Wage in '000 Euros"

for (i in 11:47) # Converting columns with player attributes to numeric
{
  fifa18[,i] <- as.numeric(unlist(fifa18[,i] ))
}

names(fifa18)[64] <- "PreferedPosition" 
fifa18_v1 <- separate(fifa18, PreferedPosition, c("PreferedPosition1", "PreferedPosition2", "PreferedPosition"), sep = " ") # Splitting a player's prefered positions

###To remove NULL values###
colSums(is.na(fifa18_v1)) #To identify variables containing NULL values

fifa18_v1[is.na(fifa18_v1)] <- 0 #Replace all NA values with 0

colSums(is.na(fifa18_v1)) #Check if all NA values are converted to 0

(is.null(fifa18_v1)) #Check for NULL values

length(unique(fifa18_v1$ID)) #Calculating number of duplicates duplicates

fifa18_v2 <- fifa18_v1[!duplicated(fifa18_v1$ID),] # Removing duplicates
fifa18_final <- fifa18_v2

# Displaying first 6 rows of cleaned dataset
datatable(head(fifa18_final, 6),options = list(scrollX = TRUE, pageLength = 3))
class(fifa18_final$Age)
as.numeric(fifa18$Age)


##Histogram####
hist(fifa18_final$Age, 
     main="Distribution of Age", 
     xlab="Age",
     ylab="Frequency",
     border="blue", 
     col="green",
     las=1, 
     breaks=5)


summary(fifa18_final$Potential)


#Top 11 players based o their Overall FIFA rating#####
Top<-fifa18_final %>% 
  arrange(-Overall) %>% 
  top_n(11,wt = Overall) %>% 
  select( Name, Age,Overall,Club,`Value in Million Euros`,`PreferedPosition1`) %>% datatable(options = list(scrollX = TRUE, pageLength = 11))


####3 Top player in each of the given position##########
fifa18_final %>% group_by(PreferedPosition1) %>%
  arrange(-Overall) %>% 
  top_n(1,wt = Overall) %>% 
  select( `PreferedPosition1`,Name, Overall,Club,Nationality, 
          `Value in Million Euros`,`Wage in '000 Euros`) %>% 
  datatable(options = list(scrollX = TRUE, pageLength = 10))

####Distribution of overall ratings####
fifa18_final  %>% 
  ggplot(aes(x = Overall, fill = factor(Overall))) +
  geom_bar() + guides(fill = guide_legend(title = "Overall rating")) +
  labs(title = "Player Ratings") +
  theme(legend.position = "right", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#####Distribution based on Nationality of Players (Top 10 Countries)#####
countries_count <- count(fifa18_final, Nationality)
top_10_countries <- top_n(countries_count, 10, n)
top_10_country_names <- top_10_countries$Nationality

country <- filter(fifa18_final, Nationality == top_10_country_names)
ggplot(country, aes(x = Nationality)) + 
  geom_bar(col = "orange", aes(fill = ..count..)) + ggtitle("Distribution based on Nationality of Players (Top 10 Countries)")


#### Players' position and age ####
ggplot(fifa18_final, aes(x = Age, fill = (`PreferedPosition1`))) +
  geom_bar(position = 'fill') + 
  scale_fill_brewer(palette = "Green") + theme_solarized_2(light = F) +
  scale_colour_solarized("blue") + 
  guides(fill = guide_legend(title = "Prefered Position")) + 
  labs(title = "PREFERED POSITIONS OF ALL PLAYERS OVER AGE",x = "Age") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

######Wage vs Player Position #####
ggplot(data = fifa18_final, aes(`Wage in '000 Euros`, fill =     factor(`PreferedPosition1`))) + 
  geom_bar() + scale_x_discrete(breaks = c(0,10,20,40,80, 160,320, 640)) +
  guides(fill = guide_legend(title = "")) + 
  labs(title = "COMPARISON OF NUMBER OF PLAYERS AT A WAGE BY POSITION")

####Wage vs Age####
ggplot(data = fifa18_final, aes(x = Age, y = `Wage in '000 Euros`)) +
  geom_line(color = "orange",size = 2) + labs(title = "WAGE vs AGE OF PLAYERS") +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))

#########Position vs Wage by Overall rating########
ggplot(data = fifa18_final, aes(x = `PreferedPosition1`, y = `Wage in '000 Euros`, color = Overall)) + geom_point() + geom_jitter() + labs(title = "") +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))

#####. Overall rating vs Potential######
ggplot(fifa18_final,aes(Overall, Potential)) +
  geom_point( size = 2, alpha = .9) + geom_jitter() + 
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))

#####Variations with Age####
ggplot(fifa18_final) +
  geom_tile(aes(Overall, Potential, fill = Age)) + 
  scale_fill_distiller(palette = "Spectral") + 
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))

#####Regression Model###
Striker_data <- filter(fifa18_final, `PreferedPosition1` == "ST") #filtering fifa data for players who are strikers

model <- lm(Overall ~ 
              Acceleration + Agility + `Ball control` + 
              Finishing + `Long shots`, data = Striker_data ) #creating a regression model
summary(model)     #summarizing the model
anova(model) 