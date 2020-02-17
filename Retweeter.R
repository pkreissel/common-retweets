library(digest)

#Skip to line 89 if you dont want to download the data yourself (takes a loooong time)
library(rtweet)

#Somehow generate rtweet token
token <- create_token()


retweet_colnames = colnames(get_retweets(status_id = "1226863007092944897"))
data = setNames(data.frame(matrix(ncol = 90, nrow = 0)), retweet_colnames)

users = c("KemmerichThL", "FDPFraktionTH", "HGMaassen", "_FriedrichMerz", "cdu_fraktion_th", "bodoramelow", "Gruene_TH", "SPDThueringen", "WTiefensee", "cdu_thueringen", "MikeMohring", "HGCreutzburg", "Ralf_Hoecker", "Martin_Sellner", "Alice_Weidel", "BjoernHoecke", "MoellerAfD", "polenz_r", "MitschAlexander", "PaulZiemiak", "jensspahn", "weberpeterstate")

#This for loop can in theory be run infinite times and it will efficiently download only the stuff thats still missing or that is new.
#You can increase the rate limit for app auth
rate_limit_counter = 75
for(user in users) {
  print(user)
  #Keep in mind that we only get the last 3200 Tweets (including retweets) of an account. So the timeframes of two accounts will be different.
  tryCatch({
    timeline = get_timeline(user, n = 3200)
  }, error = function(e) {
    print(paste0("Please crawl ", user, " again, it failed. "))
    print(e)
  })
  
  elems = nrow(timeline[!timeline$is_retweet & timeline$retweet_count > 10  & !is.element(timeline$status_id, data$retweet_status_id),])
  if(elems == 0) next
  pb = txtProgressBar(min=0, max=elems, style = 3)
  y = 0
  
  for(x in 1:nrow(timeline)) {
    #Setting retweet count threshhold increases speed and sometimes twitter doesnt return anything for lower numbers
    #This doesnt matter that much because most tweets have less than 100 retweets
    #"Viral" Tweets often get retweeted by accounts not from the respective community, so its actually a good thing that we devalue them a little bit.
    if (!timeline$is_retweet[x] & timeline$retweet_count[x] > 10 & !is.element(timeline$status_id[x], data$retweet_status_id)) {
      y = y + 1
      rate_limit_counter = rate_limit_counter - 1
      setTxtProgressBar(pb, y)
      if(rate_limit_counter == 0) {
        print(paste0("rate limit reached, sleeping ", Sys.time()))
        Sys.sleep(15*60)
        rate_limit_counter = 75
      }
      
      #Only last 100 Retweets of any tweet can be downloaded
      err <- tryCatch({
        retweets <- get_retweets(status_id = timeline$status_id[x], n = 100)
        if(nrow(retweets) == 0) stop(paste0("no retweets returned for status ",  timeline$status_id[x], " with ", timeline$retweet_count[x] , " and x: ", x))
      }, error = function(e) {
        print(e)
      })
      if(inherits(err, "error")) next
      
      data <- rbind(data, retweets)
    } else {
      #print("skipping")
    }
  }
}

data = unique(data)
sum(data$is_quote)

#Adding some privacy by pseudonyms to the data. That's really important, because potential political affiliation should be as private as possible (even if these people chose to show it publicly).
data_hashed = data.frame("retweeted_acc"= data$retweet_screen_name[is.element(data$retweet_screen_name, users)], "retweeting_acc" = NA)
data_hashed$retweeting_acc = unlist(lapply(data$user_id[is.element(data$retweet_screen_name, users)], function(item) {
    return(digest(paste0(item, "salt"))) #Secret Salt was added here for more privacy
}))

#Lets see if all these guys are roughly equal size
length(unique(data$user_id[data$retweet_screen_name =="BjoernHoecke"]))
length(unique(data$user_id[data$retweet_screen_name =="polenz_r"]))
length(unique(data$user_id[data$retweet_screen_name =="bodoramelow"]))
length(unique(data$user_id[data$retweet_screen_name =="PaulZiemiak"]))
length(unique(data$user_id[data$retweet_screen_name =="_FriedrichMerz"]))
length(unique(data$user_id[data$retweet_screen_name =="cdu_fraktion_th"]))
length(unique(data$user_id[data$retweet_screen_name =="HGMaassen"]))
length(unique(data$user_id[data$retweet_screen_name =="jensspahn"]))

#These two are quite a bit smaller, so we will have to expect somewhat skewed results
length(unique(data$user_id[data$retweet_screen_name =="MikeMohring"]))
length(unique(data$user_id[data$retweet_screen_name =="KemmerichThL"]))

write.csv(data_hashed, "hashed_data.csv")


data_hashed <- read.csv("hashed_data.csv")


calculateHöckeMaaßenX <- function(user) {
  höcke = sum(is.element(data_hashed$retweeting_acc[data_hashed$retweeted_acc =="HGMaassen"], data_hashed$retweeting_acc[data_hashed$retweeted_acc =="BjoernHoecke"]))
  X = sum(is.element(data_hashed$retweeting_acc[data_hashed$retweeted_acc =="HGMaassen"], data_hashed$retweeting_acc[data_hashed$retweeted_acc ==user]))
  return((X - höcke)/(X+höcke))
}

write.csv(cbind(lapply( c("polenz_r", "bodoramlow", "PaulZiemiak", "_FriedrichMerz", "jensspahn", "MikeMorhing", "KemmerichThL"), calculateHöckeMaaßenX), c("polenz_r", "bodoramlow", "PaulZiemiak", "_FriedrichMerz", "jensspahn", "MikeMorhing", "KemmerichThL")), "HöckeMaaßenX.csv")

#Here we compare the overlap between retweets. I think that best reflects the respective community, because it gives more weight to more active retweeters and reflects what the analysed user might perceive as popular.
#But you could also compare the overlap between retweeting accounts by adding some "unique"s into the formulas. Results are very similar. 

checkHöckrel <- function(name) {
  return(sum(is.element(data_hashed$retweeting_acc[data_hashed$retweeted_acc ==name], data_hashed$retweeting_acc[data_hashed$retweeted_acc =="BjoernHoecke"]))/length(data_hashed$retweeting_acc[data_hashed$retweeted_acc ==name]))
}

checkRamelowrel <- function(name) {
  return(sum(is.element(data_hashed$retweeting_acc[data_hashed$retweeted_acc ==name], data_hashed$retweeting_acc[data_hashed$retweeted_acc =="bodoramelow"]))/length(data_hashed$retweeting_acc[data_hashed$retweeted_acc ==name]))
}


checkPolenzrel <- function(name) {
  return(sum(is.element(data_hashed$retweeting_acc[data_hashed$retweeted_acc ==name], data_hashed$retweeting_acc[data_hashed$retweeted_acc =="polenz_r"]))/length(data_hashed$retweeting_acc[data_hashed$retweeted_acc ==name]))
}

write.csv(cbind(lapply(users, checkHöckrel), users), "höcke_relations.csv")
write.csv(cbind(lapply(users, checkRamelowrel), users), "ramelow_relations.csv")
write.csv(cbind(lapply(users, checkPolenzrel), users), "polenz_relations.csv")


