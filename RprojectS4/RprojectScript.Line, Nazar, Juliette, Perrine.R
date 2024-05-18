#Setting up directory with our ‘RprojectS4’ folder; this folder consists of the two csv files we used in our code 

setwd("D:/unilasalle/RprojectS4") 

#loading necessary packages 

library(stats) 

library(ggplot2) 

#First we want to select data to carry out our investigation so we will select a specific production type by answer this question; Which production type has the largest debt? 

#Finding the mean debt for each production type 

#uploading data to R studios of the OTEXE (production type) and TDTE3 (Debt values) values 

fn= "TypevsDebt.csv" 

#the file is read in a data frame and stored in ‘df’ 

df = read.csv(fn, header = TRUE, sep = ",", dec = ".") 

#filtering only OTEXE = 15, a summary is made giving information such as the mean debt etc... 

print(summary(df[df$OTEXE == "15",]))

#process was repeated for each OTEXE(production type) value, and the corresponding means were recorded 

#making a barplot of each production type (OTEXE values ) and there corresponding mean debt  

categories <- c("Specialist cereal, oilseed and protein crop farms ", "Farms specializing in other field crops ", " Specialized vegetable and mushroom farms ",  "Farms specializing in flowers and various horticultural product" , "Farms specializing in quality winegrowing ", "Farms specializing in other wine-growing ", "Farms specializing in fruit and other permanent crops ", "Specialized cattle farms - Dairy-oriented ", "Specialized cattle farms - Livestock and meat orientation ","Cattle farms - dairy, livestock and meat combined ", "Farms with sheep, goats and other herbivores ", "Granivore farms ", "Mixed farming ", "Herbivore-oriented mixed livestock farms ", "Mixed farming operations with granivores ", "Mixed arable and grazing farms ", "Mixed crop and livestock farms  ") 

values <- c(162204,273605 , 248698 , 131431,266099 , 278322 , 210075 , 285511 , 152336, 304323, 158730, 370302, 234568, 339701, 421443 , 272236,  275083 ) 

barplot(values, names.arg = categories, xlab = "Production Type", ylab= "Mean Debt in Euros", main = "Mean Debt in Euros for Farms of Different Production Types") 

#we found that production type = 74 has the largest mean debt, hence we focused the rest of our analysis only on this production type 

#Now following our investigation of how different farm characteristics impact debt on a farm we looked at this question; Does the farm's size (in terms of surface area, production, and revenue) impact the quantity of debt the farm has? 

#To do so we chose to do a Linear Regression Analysis for the slected characteristic vs Debt of Production type = 74 farms. This will tell us if there is a correlation between the two selected variables.

#load necessary data to perform test 

data <- read.csv("Rproject.csv") 

#shapiro wilk test for dependent variable to ensure it follows a normal distribution

data$Debt.in.EurosSW <- rnorm(114) 

print(shapiro.test(data$Debt.in.EurosSW))

#Linear regression analysis for surface area vs debt 

model <- lm(Debt.in.Euros ~ Surface.area.of.the.Farm, data = data) 

print(summary(model)) 

#Line plot of data 

plot(data$Surface.area.of.the.Farm, data$Debt.in.Euros,   xlab = "Surface Area of the Farm", ylab = "Debt in Euros", main = "Surface Area of the Farm vs Debt in Euros") 

#Adding a line of best fit to the plot 

abline(model, col = "red") 

#Linear regression analysis for Revenue of the farm vs Debt and line plot 

model <- lm(Debt.in.Euros ~ Revenues, data = data) 

print(summary(model))

plot(data$Revenues, data$Debt.in.Euros,  xlab = "Revenues", ylab = "Debt in Euros", main = "Revenues vs Debt in Euros") 

abline(model, col = "blue")  

#Linear regression analysis for Revenue:SA (revenues per surface area unit) vs Debt:SA (debt value per surface area unit)and line plot 

model <- lm(Debt.in.Euros...SA ~ Revenues.SA, data = data) 

print(summary(model))

plot(data$Revenues.SA,data$Debt.in.Euros...SA,  xlab = "Revenues per Surface Area Unit", ylab = "Debt in Euros per Surface Area Unit", main = "Revenues per Surface Area Unit vs Debt in Euros per Surface Area Unit") 

abline(model, col = "green") 

#Now we will look at if the sex of the farmer impacts the amount of debt.
# Load the data: Read the CSV file into an R dataframe. 

data <- read.csv("Rproject.csv") 


# Data transformation: Convert 'Sex' column to a factor to handle categorical data (Male, Female). 

# Convert 'Debt in Euros' column to numeric since it should represent quantitative values. 

data <- read.csv("Rproject.csv") 

data <- transform(data, Sex = factor(Sex, levels = c("1", "2"), labels = c("Male", "Female")), Debt.in.Euros = (as.numeric(Debt.in.Euros))) 

# A t-test will be done to compare the mean debt between genders. 

t_test_result <- t.test(Debt.in.Euros ~ Sex, data = data) 

#Displaying the results of the t-test  

print(t_test_result)
# Visualization of the data using a boxplot 

show(ggplot(data, aes(x = Sex, y = Debt.in.Euros, fill = Sex)) + 
  
  geom_boxplot() + 
  
  labs(title = "Boxplot of Debts by Gender", 
       
       x = "Gender", 
       
       y = "Debt (Euros)") + 
  
  theme_minimal())

# Here is a different visualization: density plot to show the distribution of debt amounts by gender. 

show(ggplot(data, aes(x = Debt.in.Euros, fill = Sex, color = Sex)) + 
  
  geom_density(alpha = 0.5) + 
  
  labs(title = "Density Plot of Debts by Gender", 
       
       x = "Debt (Euros)", 
       
       y = "Density") + 
  
  scale_fill_manual(values = c("blue", "pink")) + # Custom colors for visual distinction 
  
  scale_color_manual(values = c("darkblue", "darkred")) + 
  
  theme_minimal()) 
  
#Now we will look at the age of the farmer; Does the age of the farmer impact the quantity of debt the farmer has? 

# import the two parameters from the database. 

data <- read.csv("Rproject.csv") 

#A correlation test between age and debt was done

cor.test(as.numeric(data$Age.range),data$Debt.in.Euros) 

#After this correlation test, we can see that the value of the corr is near of 0 than 1 or -1. Both have not got any relationship. 

#to verify correlation test result, we can also do an additional chi-square test 

print(chisq.test(x = data$Age.range, y= data$Debt.in.Euros)) 

# Creating boxplots to visualize the data 
boxplot(data$Debt.in.Euros ~ data$Age.range, xlab = "Age Category", ylab = "Total Amount of Debt", main = "Boxplot of Total Amount of Debt by Age Category") 

#Lastly, we looked at if farms present on natura 2000 zones have more debt?

# Load the data to R studios 
data <- read.csv("Rproject.csv") 

# Sorting data for farms located in Natura 2000 zones and those not in Natura 2000 zones 

zone_2000 <- data[data$Natura2000zones == 1, "Debt.in.Euros"] 
not_in_zone_2000 <- data[data$Natura2000zones == 0, "Debt.in.Euros "] 

# Performing a t-test to look at the signficance of the difference of mean debt between the two categories 

print(t.test(zone_2000, not_in_zone_2000))

# Perform a Mann-Whitney U test, an alternative test 

print(wilcox.test(Debt.in.Euros~ Natura2000zones, data = data))

# Creating boxplots to display the data 

boxplot(Debt.in.Euros ~ Natura2000zones, data = data, 
        xlab = "Zone", ylab = "Total Debt Amount", 
        main = "Boxplot of Total Debt Amount by Zone") 

# Creating violin plots, to display the data in a different way 

show(ggplot(data, aes(x = factor(Natura2000zones), y =Debt.in.Euros)) + 
  geom_violin(fill = "skyblue", color = "blue") + 
  labs(x = "Zone", y = "Total Debt Amount", 
       title = "Violin Plot of Total Debt Amount by Zone")) 

