
library(tidyverse)
library(rvest)
library(ggplot2)
library(ineq)

## Data from sports-reference.com

dadata <- read_html("https://www.sports-reference.com/cbb/conferences/big-east/2023-stats.html#conference-stats", header = FALSE)
dat <- html_table(dadata, fill = TRUE)
dataa <- data.frame(dat)
write.csv(dataa, file="~/bigeast.csv")
bigeastplayers <- read.csv(file = "~/bigeast.csv", skip = 1, header = TRUE, as.is = TRUE)

####

dadata <- read_html("https://www.sports-reference.com/cbb/conferences/big-12/2023-stats.html#conference-stats", header = FALSE)
dat <- html_table(dadata, fill = TRUE)
dataa <- data.frame(dat)
write.csv(dataa, file="~/bigeast.csv")
big12players <- read.csv(file = "~/bigeast.csv", skip = 1, header = TRUE, as.is = TRUE)

####

dadata <- read_html("https://www.sports-reference.com/cbb/conferences/sec/2023-stats.html#conference-stats", header = FALSE)
dat <- html_table(dadata, fill = TRUE)
dataa <- data.frame(dat)
write.csv(dataa, file="/bigeast.csv")
secplayers <- read.csv(file = "~/bigeast.csv", skip = 1, header = TRUE, as.is = TRUE)

####

dadata <- read_html("https://www.sports-reference.com/cbb/conferences/big-ten/2023-stats.html#conference-stats", header = FALSE)
dat <- html_table(dadata, fill = TRUE)
dataa <- data.frame(dat)
write.csv(dataa, file="~/bigeast.csv")
bigtenplayers <- read.csv(file = "~/bigeast.csv", skip = 1, header = TRUE, as.is = TRUE)

####

dadata <- read_html("https://www.sports-reference.com/cbb/conferences/pac-12/2023-stats.html#conference-stats", header = FALSE)
dat <- html_table(dadata, fill = TRUE)
dataa <- data.frame(dat)
write.csv(dataa, file="~/bigeast.csv")
pac12players <- read.csv(file = "~/bigeast.csv", skip = 1, header = TRUE, as.is = TRUE)

####

dadata <- read_html("https://www.sports-reference.com/cbb/conferences/acc/2023-stats.html#conference-stats", header = FALSE)
dat <- html_table(dadata, fill = TRUE)
dataa <- data.frame(dat)
write.csv(dataa, file="~/bigeast.csv")
accplayers <- read.csv(file = "~/bigeast.csv", skip = 1, header = TRUE, as.is = TRUE)

##

data <- rbind(accplayers,pac12players,bigtenplayers,big12players,bigeastplayers,secplayers)

## cleaning the data

data[data == ''] <- NA
data[data == 'Player'] <- NA

data <- data %>% drop_na(Player)

cols <- colnames(data)
data <- data %>% mutate_at(cols[7:27], as.numeric)

data <- subset(data, MP > 25)

data$Team <- toupper(substr(data$School,1,3))



## splits

s <- split(data,data$School)

teams <- c(sort(unique(data$School)))

for (i in 1:length(teams)){
  s[[i]] <- s[[i]] %>% arrange(s[[i]][[15]])
  s[[i]][[29]] <- 1:nrow(s[[i]])
  s[[i]][[30]] <- ((s[[i]][[15]])/(sum(s[[i]][[15]])))
  s[[i]][[31]] <- cumsum(s[[i]][[30]])
  
  s[[i]][[32]] <- (s[[i]][[29]]/nrow(s[[i]]))
  
  colnames(s[[i]])[29] = "TMrank"
  colnames(s[[i]])[30] = "PERpts"
  colnames(s[[i]])[31] = "CUMperpts"
  colnames(s[[i]])[32] = "CUMSUM"
  
  assign(teams[i], s[[i]])
}


schools <- c(sort(unique(data$School)))


###### Functional Programming ##

## func for bar dist chart

distplot <- function(team,name){
  namai <- name
  
  print(ggplot(team, mapping = aes(x = reorder(Player, PTS),PTS))+
          geom_bar(stat = "identity", width = .5,fill="royalblue")+
          scale_x_discrete(guide = guide_axis(n.dodge = 3))+
          ylab("Points")+
          xlab("")+
          theme_minimal()+
          labs(title = paste(namai," -  Points Distribution")))
}

## func for Lorenz curve & GINI 

lorenz <- function(team,name){
  
  namai <- name
  
  print(ggplot(team, aes(CUMSUM,CUMperpts))+
          geom_abline(intercept = 0, slope = 1)+
          geom_smooth() + 
          ylab("Quartiles of Points")+
          xlab("Quartiles")+
          theme_minimal()+
          labs(title = paste(namai ," -  Lorenz Curve"))+
          annotate("text", x=.25, y=.82, label = paste("GINI coeff",signif(Gini(team$PTS),4))))
  
  Gini(team$PTS)
}

#### LOOP through teams

for(i in 1:length(s)){
  
  distplot(s[[i]],schools[i])
  lorenz(s[[i]],schools[i])  
}

##



#### school level ANALYSIS

schdata <- data.frame(c(schools))

for (i in 1:length(teams)){
  schdata[i,2] <- sum(s[[i]][["TOV"]])
  schdata[i,3] <- sum(s[[i]][["AST"]])
  schdata[i,4] <- sum(s[[i]][["PTS"]])
  schdata[i,5] <- Gini(s[[i]][["PTS"]])
    
}

colnames(schdata)[1] = "School"
colnames(schdata)[2] = "tov"
colnames(schdata)[3] = "ast"
colnames(schdata)[4] = "pts"
colnames(schdata)[5] = "gini"



## team level data 


biggie <- read_html("https://www.sports-reference.com/cbb/conferences/big-east/2023-ratings.html", header = FALSE)
bigus <- html_table(biggie, fill = TRUE)
biggums <- data.frame(bigus)
write.csv(biggums, file="~/bigeastteams.csv")
bigeast <- read.csv(file = "~/bigeastteams.csv", skip = 1, header = TRUE, as.is = TRUE)

biggie <- read_html("https://www.sports-reference.com/cbb/conferences/big-12/2023-ratings.html", header = FALSE)
bigus <- html_table(biggie, fill = TRUE)
biggums <- data.frame(bigus)
write.csv(biggums, file="~/bigeastteams.csv")
big12 <- read.csv(file = "~/bigeastteams.csv", skip = 1, header = TRUE, as.is = TRUE)

biggie <- read_html("https://www.sports-reference.com/cbb/conferences/big-ten/2023-ratings.html", header = FALSE)
bigus <- html_table(biggie, fill = TRUE)
biggums <- data.frame(bigus)
write.csv(biggums, file="~/bigeastteams.csv")
bigten <- read.csv(file = "~/bigeastteams.csv", skip = 1, header = TRUE, as.is = TRUE)

biggie <- read_html("https://www.sports-reference.com/cbb/conferences/sec/2023-ratings.html", header = FALSE)
bigus <- html_table(biggie, fill = TRUE)
biggums <- data.frame(bigus)
write.csv(biggums, file="~/bigeastteams.csv")
sec <- read.csv(file = "~/bigeastteams.csv", skip = 1, header = TRUE, as.is = TRUE)

biggie <- read_html("https://www.sports-reference.com/cbb/conferences/pac-12/2023-ratings.html", header = FALSE)
bigus <- html_table(biggie, fill = TRUE)
biggums <- data.frame(bigus)
write.csv(biggums, file="~/bigeastteams.csv")
pac12 <- read.csv(file = "~/bigeastteams.csv", skip = 1, header = TRUE, as.is = TRUE)


biggie <- read_html("https://www.sports-reference.com/cbb/conferences/acc/2023-ratings.html", header = FALSE)
bigus <- html_table(biggie, fill = TRUE)
biggums <- data.frame(bigus)
write.csv(biggums, file="~/bigeastteams.csv")
acc <- read.csv(file = "~/bigeastteams.csv", skip = 1, header = TRUE, as.is = TRUE)

####





dateams <- rbind(bigten,big12,bigeast,acc,pac12,sec)

###

schdata <- merge(dateams, schdata, by="School")
schdata$wlperc <- (schdata$W/(schdata$W + schdata$L))

#

schdata %>% arrange(gini)

ggplot(schdata, aes(V25, gini))+
  geom_point()+
  geom_smooth()

##


##

ggplot(Xavier, aes(CUMSUM,CUMperpts))+
  geom_abline(intercept = 0, slope = 1)+
  geom_smooth(data = Xavier, aes(CUMSUM,CUMperpts), color = "royalblue") + 
  geom_smooth(data = Utah, aes(CUMSUM,CUMperpts), color = "red") + 
  geom_smooth(data = `Seton Hall`, aes(CUMSUM,CUMperpts), color = "darkblue") + 
  ylab("Quartiles of Points")+
  xlab("Quartiles")+
  theme_minimal()+
  theme()+
  labs(title = paste("XAVIER, UTAH, SETON HALL -- Lorenz Curves"))+
  labs(
    colour = "names"
  )+
  annotate("text", x=.88, y=.28, label = "UTAH", color = "red")+
  annotate("text", x=.84, y=.20, label = "XAVIER", color = "royalblue")+
  annotate("text", x=.85, y=.13, label = "SETON HALL", color = "darkblue")


hist(schdata$gini)


for (i in 1:length(s)){
  
  schdata[i,25] <- sd(s[[i]][["PTS"]])
  
}


ggplot(schdata, aes(gini))+
  geom_histogram()

