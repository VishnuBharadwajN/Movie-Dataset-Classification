getwd()
library(dplyr)
library(ggplot2)
data=read.csv("IMDb movies.csv")
summary(data)
head(data,5)

#cleaning genre attribute

newgenre=data$genre
size=length(newgenre)
for (i in 1:size) 
{
  str1=""
  str2=""
  str2=newgenre[i]
  s=nchar(str2)
  string_split = strsplit(str2, "")[[1]]
  for (x in string_split) 
  {
    if(x ==',')
    {
      break;
    }
    str1=paste(str1, x, sep ="")
  }
  newgenre[i]=str1
}
newgenre
unique_genre=unique(newgenre)
unique_genre
data$genre=newgenre

#cleaning country attribute

newcountry=data$country
size=length(newcountry)
for (i in 1:size) 
{
  str1=""
  str2=""
  str2=newcountry[i]
  s=nchar(str2)
  string_split = strsplit(str2, "")[[1]]
  for (x in string_split) 
  {
    if(x ==',')
    {
      break;
    }
    str1=paste(str1, x, sep ="")
  }
  newcountry[i]=str1
}
newcountry
unique_country=unique(newcountry)
unique_country
data$country=newcountry
data

# Selecting required attributes

data=select(data, year, genre, country, avg_vote)
summary(data)
sum(is.na(data))

#detecting outliers

boxplot(data$avg_vote,xlab = "VOTES",ylab = "RATING",main = "AVERAGE VOTES")


#Selecting data based on user requirements


temp_year = readline(prompt="Enter the year for where you want to consider movies  -> ")
temp_country =  readline(prompt="Enter the country name -> ")
temp_rating = readline(prompt ="Enter the rating between 1-9 decimal is allowed -> " )
temp=subset(data,country==temp_country & year>temp_year & avg_vote>=temp_rating)
temp


#working on temp data to get results in form of graphs


graph_y=c()
graph_x=c()
for(x in unique_genre)
{
  count=0
  for(y in temp$genre)
  {
    if(x==y)
    {
      count=count+1
    }
  }
  if(count>100)
  {
    graph_x=append(graph_x,x)
    graph_y=append(graph_y,count)
  }
}


#BARPLOT 

barplot(graph_y,names.arg=graph_x,xlab = "Genre",ylab = "Number of films",main = "Movies Data",notch = FALSE,
        varwidth = TRUE,
        col = c("green", "orange", "blue","red"),
        names = graph_x )


#PIECHART

piepercent= round(100 * graph_y / sum(graph_y), 1)
pie(graph_y, labels = c(piepercent,graph_x),
    main = "Genre Pie chart", col = rainbow(length(graph_y)))
legend("bottomright", graph_x,
       cex = 0.8, fill = rainbow(length(graph_y)))



#line chart of genre vs movies

val <-data.frame(Genre=graph_x, Movies=graph_y)
ggplot(data=val, aes(x=Genre, y=Movies, group=1)) +
  geom_line(color="green")+
  geom_point()




#Genre vs year analysis

temp_genre=readline(prompt = "Enter the genre to have year wise graph")


temp_y=c()
temp_x=c(2000:2019)
for(x in temp_x)
{
  temp_movie=subset(data,year==x)
  count=0
  for(y in temp_movie$genre)
  {
    if(y==temp_genre)
    {
      count=count+1
    }
  }
  temp_y=append(temp_y,count)
}


#bar plot of genre vs year

barplot(temp_y,names.arg=temp_x,xlab = "years",ylab = "Number of films",main = paste(temp_genre, "Movies vs Year", sep = " "))


#scatter plot of genre vs year

plot(x = temp_x, y = temp_y,
     xlab = "years",
     ylab = "No of movies",
     col = "red",
     pch=8,
     xlim = c(2000,2019),
     ylim = c(0, max(temp_y)),
     main = paste(temp_genre, "Movie analysis", sep = " ")
     
)

#histogram of Rating vs votes

hist(data$avg_vote,main = "Rating analysis using votes", xlab = "Rating", ylab = "Number of votes", col = "green", 
     border = "black", xlim = c(1, 9), breaks = 8)  

