library(twitteR)
library(ROAuth)

api_key <- "your_own_api_key"
api_secret <- "your_own_api_secret"

access_token <- "your_own_token"
access_token_secret <- "your_own_secret"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

tweets_plusdaily = searchTwitter('from:@plustrafik', n=2000)
tweets.dfa <- twListToDF(tweets_plus)
write.csv(tweets.dfa, file="plus.csv", row.names = FALSE)
View(tweets.dfa)