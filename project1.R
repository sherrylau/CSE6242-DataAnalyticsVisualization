#################################
### Name: Kawing Lau (Sherry) ###
### GTID: klau37 ################
### CSE6242 Project 1 ###########

library(ggplot2)
library(grid)
library(gridExtra)
library(corrplot)
library(dplyr)
library(knitr)
load('movies_merged')

#### Q1
movies = movies_merged[movies_merged$Type=="movie",]
# Count number of rows deleted by differencing before and after removal
print(paste("There's", nrow(movies_merged) - nrow(movies), 
            "rows removed that do not correspond to movies", sep=" "))

#### Q2
# Function to convert Runtime to numeric column
convertRuntimeString = function(data_column){
  minute = NULL
  for(i in 1:length(data_column)){
    if(data_column[i]=="N/A"){
      minute[[i]] = NA
    } else if(!grepl("h", data_column[i])){
      min_pos = unlist(gregexpr('min', data_column[i]))
      minute[[i]] = as.numeric(substr(data_column[i], 1, min_pos-1))
    } else{
      hr_pos = unlist(gregexpr('h', data_column[i]))
      hr = as.numeric(substr(data_column[i], 1, hr_pos-1))
      min_pos = unlist(gregexpr('min', data_column[i]))
      min = as.numeric(substr(data_column[i], hr_pos+1, min_pos-1))
      minute[[i]] = as.numeric(min) + hr*60
    }
  }
  return(minute)
}
# Apply function to column Runtime
movies$Runtime = convertRuntimeString(movies$Runtime)

summary(movies$Runtime)
ggplot(movies, aes(x=Runtime)) + 
  geom_histogram(bins=500) + 
  labs(x = "Run Time", y="Frequency", title = "Movies' RunTime Histogram")

ggplot(movies, aes(x=Year)) + 
  geom_bar(fill = "#3399FF") +
  labs(x = "Year", y = "Frequency", title = "Year Frequency Distribution")

ggplot(movies, aes(x=Year, y=Runtime)) +
  geom_point(color = "#CC0000") + 
  stat_smooth(method = "auto")

movies$YearBin = cut(movies$Year, seq(1880, 2020, by=10), dig.lab=4)
yearSummary = movies %>% group_by(YearBin) %>% 
  summarize(
    freq = length(Runtime),
    min = min(Runtime, na.rm=TRUE),
    q1 = quantile(Runtime, prob=0.25, na.rm=TRUE),
    mean = mean(Runtime, na.rm=TRUE),
    median = quantile(Runtime, prob=0.50, na.rm=TRUE),
    q3 = quantile(Runtime, prob=0.75, na.rm=TRUE),
    max = max(Runtime, na.rm=TRUE),
    std = sd(Runtime, na.rm=TRUE)
  )
yearSummary = data.frame(yearSummary)
kable(yearSummary)

ggplot(movies, aes(x=YearBin, y=Runtime, fill=YearBin)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(x = "Binned 10-Year Interval", y="Movies' Run Time") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

rt_bg_cor = paste("Corr ", 
                  round(cor(movies$Runtime, movies$Budget, use="complete.obs")*100,2),
                  "%",sep="")
rt_bg_grob = grobTree(textGrob(rt_bg_cor , x=0.1,  y=0.95, hjust=0))

ggplot(movies, aes(x=Budget, y=Runtime)) +
  geom_point(color = "#CC0000") +
  annotation_custom(rt_bg_grob) +
  labs(x="Budget", y="Runtime", title="Runtime vs. Budget Scatterplot")

movies$BudgetBin = cut(movies$Budget, 
                       breaks=c(quantile(movies$Budget, probs = seq(0, 1, by = 0.25), 
                                         na.rm=TRUE, include.lowest=TRUE, right=TRUE)),
                       dig.lab=10)
budgetSummary = movies %>% group_by(BudgetBin) %>% 
  summarize(
    freq = length(Runtime),
    min = min(Runtime, na.rm=TRUE),
    q1 = quantile(Runtime, prob=0.25, na.rm=TRUE),
    mean = mean(Runtime, na.rm=TRUE),
    median = quantile(Runtime, prob=0.50, na.rm=TRUE),
    q3 = quantile(Runtime, prob=0.75, na.rm=TRUE),
    max = max(Runtime, na.rm=TRUE),
    std = sd(Runtime, na.rm=TRUE)
  )
budgetSummary = data.frame(budgetSummary)
kable(budgetSummary)

ggplot(movies, aes(x=BudgetBin, y=Runtime, fill=BudgetBin)) +
  geom_boxplot(show.legend=FALSE) +
  labs(x = "Binned 10-Year Interval", y="Movies' Run Time") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

#### Q3
# Split individual genre in a cell by comma and convert into list
genre = unlist(strsplit(movies$Genre, ','))
# Trim whitespace
genre = gsub(' ', '', genre)
# Change to lower case
genre = tolower(genre)
# Remove punctuation
genre = gsub("[[:punct:]]", "", genre)
# Get unique set of genre
genre = unique(genre)
# List all the unique genre
print(genre)
# change Genre column under 
movies$GenreModified = tolower(movies$Genre)
# trim whitespace
movies$GenreModified = gsub(" ", "", movies$GenreModified)
# replace comma by space
movies$GenreModified = gsub(",", " ", movies$GenreModified)
# remove punctuation
movies$GenreModified = gsub("[[:punct:]]", "", movies$GenreModified)
# Create binary vector for each type of genre
genre_binary_matrix = NULL
for (i in 1:length(genre)){
  g = genre[i]
  binary_v = as.numeric(grepl(paste('\\b',g,'\\b', sep=""), movies$GenreModified))
  genre_binary_matrix = cbind(genre_binary_matrix, binary_v)
}
genre_binary_df = data.frame(genre_binary_matrix)
colnames(genre_binary_df) = paste("Genre_", genre, sep="")
movies = cbind(movies, genre_binary_df)
# Check if any movies record has no title
movies$Title[is.na(movies$Title)]
# Count the number of movies in each genre
genreSummary = data.frame(genre=names(genre_binary_df),num_movies=colSums(genre_binary_df))
genreSummary$genre = gsub("Genre_", "", genreSummary$genre)
genreSummary$per_movies = genreSummary$num_movies / nrow(movies)
genreSummary = genreSummary[order(-genreSummary$per_movies),]
genreSummary$rank = seq(1:nrow(genreSummary))
genreSummary$rank2 = sprintf("%02d", genreSummary$rank)
row.names(genreSummary) = NULL

ggplot(genreSummary, aes(x=reorder(genre, per_movies), y=per_movies)) + 
  geom_bar(stat="identity", fill="#ff9933") +
  geom_text(aes(label=round(per_movies*100,1))) + 
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(x="Movies' Genres", y="Proportion of Movies")

kable(head(genreSummary,10))

summary(movies$Gross)

genre = genreSummary$genre
newGenreDF = NULL
for (g in genre){
  # Filter dataset to include 1 if genre present in the record
  genre_data = movies[movies[[paste("Genre_",g,sep="")]]==1,]
  genre_data = genre_data[,c("Title","Gross")]
  genre_data$genre = g
  newGenreDF = rbind(newGenreDF, genre_data)
}

newGenreDF = merge(newGenreDF, genreSummary[,c("genre","rank")], by="genre")

ggplot(newGenreDF, aes(x=reorder(genre, -rank), y=Gross, fill=genre)) + 
  geom_boxplot(show.legend=FALSE) +
  coord_flip() +
  labs(x="Genre", y="Gross Revenue", 
       title="Gross Revenue by Genre Boxplot sorted by Movies' Proportion")

#### Q4
summary(movies$Year)
movies$Released = as.Date(movies$Released, "%Y-%m-%d")
movies$ReleasedYear = as.numeric(format(movies$Released, "%Y"))
summary(movies$ReleasedYear)
movies$YearReleasedDiff = ifelse(movies$Year == movies$ReleasedYear, "Y", "N")
summary(as.factor(movies$YearReleasedDiff))
movies$YearReleasedDiff2 = ifelse((
  (movies$Year) == movies$ReleasedYear |
    (movies$Year+1) == movies$ReleasedYear |
    (movies$Year-1) == movies$ReleasedYear),
  "Y","N")

# Original dataset % Present Gross Records
org_gross = length(movies$Gross[!is.na(movies$Gross)])
print(org_gross)

# Filter out mismatch Year and Released Year using "YearReleasedDiff"
movies_m1 = subset(movies, YearReleasedDiff=="Y")
nrow(movies_m1)
# Method 1 % Present Gross Records
m1_gross = length(movies_m1$Gross[!is.na(movies_m1$Gross)])
print(m1_gross)
print((org_gross-m1_gross)/org_gross)

# Filter out mismatch Year and Released Year using "YearReleasedDiff2"
movies_m2 = subset(movies, YearReleasedDiff2=="Y")
nrow(movies_m2)
# Method 2 % Present Gross Records
m2_gross = length(movies_m2$Gross[!is.na(movies_m2$Gross)])
print(m2_gross)
print((org_gross-m2_gross)/org_gross)

#### Q5
movies_m2$ReleasedMonth = format(movies_m2$Released, "%m")
movies_m2$ReleasedDay = format(movies_m2$Released, "%d")

ggplot(movies_m2, aes(x=ReleasedMonth, y=Gross)) + 
  geom_boxplot() + 
  labs(x="Month of Released", y="Gross Revenue",
       title="Gross Revenue by Month of Released Boxplot")

ggplot(movies_m2, aes(x=ReleasedDay, y=Gross)) +
  geom_boxplot() +
  labs(x="Day of Released", y="Gross Revenue",
       title="Gross Revenue by Day of Released Boxplot")

releasedSummary = movies_m2 %>% group_by(ReleasedMonth, ReleasedDay) %>%
  summarize(
    count = length(Title),
    mean = mean(Gross, na.rm=TRUE)
  )

ggplot(releasedSummary, aes(x=ReleasedMonth, y=ReleasedDay)) +
  geom_tile(aes(fill= mean)) +
  scale_fill_gradient(low = "white", high = "#1a3855") +
  labs(x="Month of Released", y="Day of Released",
       fill="Average Gross Revenue",
       title="Average Gross Revenue by Released Day and Month Heatmap")

# Create new data frame with 1 genre per record
genreSummary$genre2 = paste(genreSummary$rank2, ".", genreSummary$genre, sep="")
genre = genreSummary$genre2
newGenreDF2 = NULL
for (g in genre){
  # Filter dataset to include 1 if genre present in the record
  g_string = paste("Genre_",substr(g,4,nchar(g)),sep="")
  genre_data = movies_m2[movies_m2[[g_string]]==1,]
  genre_data = genre_data[,c("Title","Gross","Released","ReleasedMonth","ReleasedDay")]
  genre_data$genre = g
  newGenreDF2 = rbind(newGenreDF2, genre_data)
}

releasedGenreSummary = newGenreDF2 %>% 
  group_by(genre, ReleasedMonth, ReleasedDay) %>%
  summarize(
    count = length(Title),
    mean = mean(Gross, na.rm=TRUE)
  )

ggplot(releasedGenreSummary, aes(x=ReleasedMonth, y=ReleasedDay)) +
  geom_tile(aes(fill= mean)) +
  scale_fill_gradient(low = "white", high = "#1a3855", guide=FALSE) +
  facet_wrap(~genre) + 
  labs(x="Month of Released", y="Day of Released",
       fill="Average Gross Revenue",
       title="Average Gross Revenue by Released Day and Month Heatmap")

releasedGenreSummary2 = newGenreDF2 %>% 
  group_by(genre) %>%
  summarize(
    count = length(Title),
    count_gross = length(na.omit(Gross))
  )
releasedGenreSummary2 = releasedGenreSummary2[order(-releasedGenreSummary2$count_gross),]

ggplot(releasedGenreSummary2, aes(x=reorder(genre,count_gross), y=count_gross)) +
  geom_bar(stat="identity", fill="#a4cdc9") +
  coord_flip() +
  labs(x="Genre", y="Number of Present Gross Revenue Records",
       title="Number of Present Gross Revenue Rows by Genre")


#### Q6
rating_col = c("imdbRating","imdbVotes", "tomatoMeter","tomatoRating",
               "tomatoReviews","tomatoFresh","tomatoRotten","tomatoUserMeter",
               "tomatoUserRating","tomatoUserReviews")

rating_col_combn = combn(rating_col, 2)

corr_ls = NULL
pairwise_graph = NULL
for (i in 1:ncol(rating_col_combn)){
  rating_var1 = rating_col_combn[1,i]
  rating_var2 = rating_col_combn[2,i]
  corr = round(cor(movies_m2[[rating_var1]], movies_m2[[rating_var2]], use="complete.obs"),4)
  corr_ls[[i]] = corr
  corr_grob = grobTree(textGrob(corr , x=0.1,  y=0.95, hjust=0))
  pairwise_graph[[i]] = ggplot(movies_m2, aes_string(x=rating_var1, y=rating_var2)) +
    geom_point(color="#f37735") +
    geom_smooth() + 
    annotation_custom(corr_grob)
}
do.call(grid.arrange, c(pairwise_graph[1:15], ncol=3))
do.call(grid.arrange, c(pairwise_graph[16:30], ncol=3))
do.call(grid.arrange, c(pairwise_graph[31:45], ncol=3))

pair_cormtx = cor(movies_m2[,rating_col], use="complete.obs")
corrplot(pair_cormtx, method="color", type="upper", 
         addCoef.col=TRUE)

x = data.frame(t(rating_col_combn))
x$Correlation = corr_ls
x = x[order(-x$Correlation),]
row.names(x) = NULL
kable(x[x$Correlation>0.2,])

kable(x[(x$Correlation<=0.2 & x$Correlation>=-0.2),])

kable(x[x$Correlation< -0.2,])

corr_gross_ls = NULL
scatter_gross_ls = NULL
for (i in 1:length(rating_col)){
  rating = rating_col[i]
  corr_gross = round(cor(movies_m2[[rating]], movies_m2$Gross, use="complete.obs"),4)
  corr_gross_ls[[i]] = corr_gross
  corr_gross_grob = grobTree(textGrob(corr_gross , x=0.1,  y=0.95, hjust=0))
  scatter_gross_ls[[i]] = ggplot(movies_m2, aes_string(x=rating, y="Gross")) +
    geom_point(color="#ff5722") + 
    geom_smooth(color="#607d8b") +
    annotation_custom(corr_gross_grob) +
    labs(x=rating, y="Gross Revenue")
}
do.call(grid.arrange, c(scatter_gross_ls, ncol=4))

#### Q7
# Create New column name AwardsModified by lower case Awards
movies_m2$AwardsModified = tolower(movies_m2$Awards)
# Convert values with "n/a" to NA
movies_m2$AwardsModified[movies_m2$AwardsModified=="n/a"] = NA
# Examine all unique values of awards to see pattern
# unique(movies$AwardsModified)
# Remove punctuation from Awards column
movies_m2$AwardsModified = gsub("[[:punct:]]","",movies_m2$AwardsModified)
# Keep all numeric values from Awards and seperate values by comma
movies_m2$AwardsModified = gsub("[^0-9]", ",",movies_m2$AwardsModified)
# Split string by comma for each element in Award column
movies_m2$AwardsModified = strsplit(movies_m2$AwardsModified,',')
# Convert each element under the Awards column to numeric
movies_m2$AwardsModified = sapply(movies_m2$AwardsModified,as.numeric)
# Sum elements within each cell of Awards column to get total nominations/awards count
movies_m2$AwardsModified = sapply(movies_m2$AwardsModified,sum, na.rm=TRUE)

# Create three dimensional binary vector
# Examine descriptive summary for number of awards/nominations
summary(movies_m2$AwardsModified)

# Create binary vector for no nomination or awards
movies_m2$AwardsComp1 = ifelse(movies_m2$AwardsModified==0,1,0)
print(sum(movies_m2$AwardsComp1))

# Create threshold awards/nominations count to achieve 5:1 ratio
len_nonzero = length(movies_m2$AwardsModified[movies_m2$AwardsModified!=0])
percentile_thresh = (len_nonzero*5/6)/len_nonzero
thresh = quantile(movies_m2$AwardsModified[movies_m2$AwardsModified!=0], percentile_thresh)
# Number of Awards/Nominations Cutoff to distinguish some (Comp2) and many (Comp3)
print(thresh)

# Create binary vector for some nominations or awards
movies_m2$AwardsComp2 = ifelse((movies_m2$AwardsModified>0 & movies_m2$AwardsModif<=thresh),1,0)
print(sum(movies_m2$AwardsComp2))

# Create binary vector for many nominations or awards
movies_m2$AwardsComp3 = ifelse(movies_m2$AwardsModified>thresh,1,0)
print(sum(movies_m2$AwardsComp3))

#### Q8
#### > Insight 1
summary(movies_m2$Budget)
summary(movies_m2$Gross)

bgt_gross_cor = paste("Corr ", round(cor(movies_m2$Budget, movies_m2$Gross, use="complete.obs")*100,2), "%", sep="")
corr_bgt_grob = grobTree(textGrob(bgt_gross_cor , x=0.1,  y=0.95, hjust=0))
ggplot(movies_m2, aes(x=Budget, y=Gross)) + 
  geom_point(color="#ff5722") + 
  geom_smooth(color="#607d8b") +
  annotation_custom(corr_bgt_grob) +
  labs(x="Budget",
       y="Gross Revenue",
       title="Scatterplot for Budget vs. Gross Revenue")
#### > Insight 2
rating_col = c("imdbRating","tomatoRating")
scatter_rating_award_ls = NULL
for (i in 1:length(rating_col)){
  rating = rating_col[i]
  corr_rating_award = round(cor(movies_m2[[rating]], movies_m2$AwardsModified, use="complete.obs"),4)
  corr_rating_award_grob = grobTree(textGrob(corr_rating_award , x=0.1,  y=0.95, hjust=0))
  scatter_rating_award_ls[[i]] = ggplot(movies_m2, aes_string(x=rating, y="AwardsModified")) +
    geom_point(color="#ff5722") + 
    geom_smooth(color="#607d8b") +
    annotation_custom(corr_rating_award_grob) +
    labs(x=rating,
         y="Number of Awards/Nominations")
}
do.call(grid.arrange, c(scatter_rating_award_ls, ncol=2))

ggplot(movies_m2, aes(x=Gross, y=AwardsModified)) +
  geom_point(color="#ff5722") + 
  geom_smooth(color="#607d8b") +
  annotation_custom(corr_rating_award_grob) +
  labs(x="Gross Revenue",
       y="Number of Awards/Nominations")

#### > Insight 3
movies_m2$LanguageNum = sapply(strsplit(movies_m2$Language,','),length)
summary(movies_m2$LanguageNum)
ggplot(movies_m2, aes(x=LanguageNum)) +
  geom_bar(fill="#af657a") +
  labs(x="Number of Language per Movie",
       y="Frequency",
       title="Number of Lanugage per Movie Distribution")

movies_m2$LanguageNum = as.factor(movies_m2$LanguageNum)
rating_col = c("imdbRating","tomatoRating")
bp_rating_lang_ls = NULL
for (i in 1:length(rating_col)){
  rating = rating_col[i]
  bp_rating_lang_ls[[i]] = ggplot(movies_m2, aes_string(x="LanguageNum", y=rating)) +
    geom_boxplot(aes(fill=LanguageNum), show.legend = FALSE) + 
    labs(x="Number of Language per Movie",
         y=rating)
}
do.call(grid.arrange, c(bp_rating_lang_ls, ncol=2))

