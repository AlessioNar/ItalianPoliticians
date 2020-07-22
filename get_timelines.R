library(rtweet)

twit_names_pol<-politici_italiani[is.na(politici_italiani$screen_name) == FALSE,]

timelines<-data.frame()

for (i in 1:nrow(twit_names_pol)){
  temp_timelines<-get_timelines(twit_names_pol$screen_name[i], n = 3200, since_id = 1210868619741539830)
  timelines<-rbind(timelines, temp_timelines)
  print(i)
  Sys.sleep(3)
}

save(timelines, file = "ita_timelines.rda")

