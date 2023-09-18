
mtcars
?mtcars
head(mtcars)
nrow(mtcars)
mtcars$gear
table(mtcars$gear)
table(mtcars$gear)/length(mtcars$gear)
0.46875+0.37500+0.15625

table(mtcars$vs)
table(mtcars$gear, mtcars$vs)






#?mtcars
barplot(table(mtcars$gear))

tab <- table(mtcars$gear, mtcars$vs)
barplot(tab, beside=TRUE, legend=FALSE,
        main="Barplot comparing gears by type of engine",
        args.legend=list(title="Gears"))


barplot(tab, beside = T, 
        names=c("V-shaped engine", "Straight engine"), 
        legend=TRUE, args.legend=list(title="Gears"), 
        main="Barplot comparing gears by type of engine", xlab="Engine")






# Load libraries
install.packages("ggplot2")
library(ggplot2) # for graphics
library(reshape2) # for "melt" command
library(ggthemes)

# Create data

tab<-table(mtcars$gear, mtcars$vs)
colnames(tab) <- c("V-shaped engine", "Straight engine")
rownames(tab) <- c("3 gears", "4 gears", "5 gears")

tab1 <- melt(tab)
colnames(tab1) <- c("Gear", "Engine", "Value")

# Barplot
ggplot(data = tab1, aes(x=Engine, y=Value, fill=Gear)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_wsj()




