library(dplyr)
library(ggplot2)

df <- read.csv("FakeNewsNet.csv")

#data variables and their data types
dplyr::glimpse(df)
names(df)
head(df)


#news_url seems unimportant so let us drop it
df = df %>% 
  dplyr::select(- news_url) %>% 
  mutate(news_credibility = if_else(real == 1, "Real", "Fake")) %>%  # set category names
  dplyr::select(- real) %>% 
  mutate(source_domain = as.factor(source_domain))

#missing value pattern
mice::md.pattern(df)


#distribution of real and fake news
df %>% 
  ggplot() + 
  geom_bar(mapping = aes(news_credibility, fill = news_credibility)) + 
  scale_fill_discrete() + 
  labs(
    title = "Distribution of real and fake news", 
    x = "", 
    y = "Number of articles", 
    fill = ""
  ) + 
  theme(
    axis.text.x = element_text(color = 'black', size = 10), 
    plot.title = element_text(size = 18)
  )


#frequency distribution of fake news
prop.table(table(df$news_credibility))


#fake news are more frequent
df %>% 
  dplyr::select(tweet_num, news_credibility) %>% 
  group_by(news_credibility) %>% 
  summarise(retweets = mean(tweet_num)) %>% 
  ggplot2::ggplot(mapping = aes(news_credibility, retweets, fill = retweets)) + 
  geom_col() +
  labs(
    title = "Fake news are retweeted more often",
    x = "News",
    y = "Number of retweets"
  ) + 
  theme(plot.title=element_text(size=18)) + 
  scale_fill_gradient(low = 'pink', high = 'red4')

#the news is more likely to be fake if the source is missing
df %>% 
  filter(is.na(source_domain)) %>%
  ggplot(mapping = aes(x = news_credibility, fill = news_credibility)) + 
  geom_bar() + 
  labs(
    title = "Fake news distribution when source is missing", 
    x = "", 
    y = "Number of articles", 
    fill = ""
  ) + 
  theme(
    plot.title = element_text(size = 18), 
    axis.text.x = element_text(size = 10, color = 'black')
  )

#most popular news sources
df %>% 
  filter(!is.na(source_domain)) %>% 
  group_by(source_domain) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(6) %>% 
  ggplot(mapping = aes(x = as.factor(source_domain), 
                       fill = n)) +
  geom_col(aes(y = n)) +
  labs(
    title = "Most popular sources of news",
    x = "Source domains",
    y = "Popularity", 
    fill = "Number of articles"
  ) + 
  theme(
    axis.text.x = element_text(angle = 20, size = 7), 
    plot.title = element_text(size = 18)
  ) + 
  scale_fill_gradient(low = 'pink', high = 'red4')
