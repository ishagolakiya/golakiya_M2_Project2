print("Plotting Basics: Isha Golakiya")
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)
data(BullTroutRML2)
#Load the BullTroutRML2 dataset
BullTroutRML2
#Print the first and last 3 records from the BullTroutRMS2 dataset
head(BullTroutRML2,3)
tail(BullTroutRML2,3)
#Or
mydata <- BullTroutRML2[1:3,]
mydata
# Remove all records except those from Harrison Lake
harrison <- filterD(BullTroutRML2,BullTroutRML2$lake == 'Harrison')
harrison

# Display the first and last 5 records from the filtered BullTroutRML2 dataset
head(harrison,5)
tail(harrison,5)
#Or 
headtail(harrison,n=5)

#Display the structure of the filtered BullTroutRML2dataset
str(harrison)

#Display the summary of the filtered BullTroutRML2dataset
summary(harrison)

#Create a scatterplot for "age" (y variable) and "fl" (x variable) with the following
#specifications:
plot(harrison$fl, harrison$age, 
     xlab = "Fork Length (mm)",
     ylab = "Age (yrs)",
     xlim = c(0,500),
     ylim = c(0,15),
     main = "Plot 1: Harrison Lake Trout"
     ) 
#Plot an "Age" histogram with the following specifications
hist(harrison$age,
     xlab = "Age (yrs)",
     ylab = "Frequency",
     xlim = c(0,15),
     ylim = c(0, 15),
     col = "cadetblue",
     main = "Plot 2: Harrison Fish Age Distribution",
     col.main = "cadetblue")

#Create an overdense plot using the same specifications as the previous scatterplot.
smoothScatter(harrison$fl,harrison$age, 
              ylim=c(0, 15), 
              xlim=c(0,500), 
              ylab="Age (yrs)", 
              xlab="ForkLength (mm)",
              main="Plot 3: Harrison Density Shaded by Era", 
              pch=21, 
              col="green"
              )

#Create a new object called "tmp" that includes the first 3 and last 3 records of the
#BullTroutRML2 data set
tmp<-headtail(harrison, n=3)
tmp

#Display the "era" column (variable) in the new "tmp" object
tmp$era

#Create a pchs vector with the argument values for + and x. 
pchs <- c("+", "x")

#Create a cols vector with the two elements "red" and "gray60"
cols <- c("red", "gray60")

#Convert the tmp era values to numeric values.
new_era<- as.numeric(tmp$era)
class(tmp$era)
tmp$era

#. Initialize the cols vector with the tmp era values
cols[factor(tmp$era)]
cols
harrison
#Create a plot of "Age (yrs)" (y variable) versus "Fork Length (mm)" (x variable) with the
#following specifications:
plot(harrison$fl, harrison$age, main="Plot 4: Symbol & Color by Era",
     xlim=c(0,500), ylim=c(0,15), ylab="Age (yrs)", xlab="Fork Length (mm)",
     pch = ifelse(tmp$era == "1977-80", pchs[1], pchs[2]),
     col = ifelse(tmp$era == "1977-80", cols[1], cols[2])
)
tmp$era
#. Plot a regression line overlay on Plot 4 and title the new graph "Plot 5: Regression
#Overlay
plot(harrison$fl, harrison$age, main="Plot 5: Regression Overlay",
     xlim=c(0,500), ylim=c(0,15), ylab="Age (yrs)", xlab="Fork Length (mm)",
     pch = ifelse(tmp$era == "1977-80", pchs[1], pchs[2]),
     col = ifelse(tmp$era == "1977-80", cols[1], cols[2])
     
   )
abline(lm(harrison$age~harrison$fl), col="blue", main= "Plot 5: Regression Overlay")

#Place a legend of on Plot 5 and call the new graph "Plot 6: :Legend Overlay"
plot(harrison$fl, harrison$age, main="Plot 6: Legend Overlay",
     xlim=c(0,500), ylim=c(0,15), ylab="Age (yrs)", xlab="Fork Length (mm)",
     pch = ifelse(tmp$era == "1977-80", pchs[1], pchs[2]),
     col = ifelse(tmp$era == "1977-80", cols[1], cols[2]))
abline(lm(harrison$age~harrison$fl), col="blue", main= "Plot 6: Legend Overlay")
legend("topleft", 
       inset=0.05, 
       legend=c("1977-80", "1977-01"),
       col= c("red", "black"),
       pch = c(3,4),
       bty="n",
       text.col = cols,
       bg='lightblue',
       box.col="blue")
-------------------------------------------------------------------------------------------
