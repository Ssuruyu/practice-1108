#install.packages("devtools")
#install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
#install.packages("devtools")
#devtools::install_github("hadley/httr")
library(rjson)
library(httpuv)
library(devtools)
library(httr)
library(Rfacebook)

#fb.oauth <- fbOAuth(app_id="", app_secret="")
#save(fb.oauth, file="fb_oauth")
#load("fb_oauth")

fb.oauth = ""
me = getUsers("me", fb.oauth)

ids <- searchGroup(name="pecuClub", token=fb.oauth)
group <- getGroup(group_id=ids$id[1], token=fb.oauth)
#my_checkins <- getCheckins(user="me", token=fb.oauth)

my_friends <- getFriends(fb.oauth, TRUE)
head(my_friends, n=10)


for(i in 1:10)
{
  user = getUsers(my_friends$id[i], fb.oauth, private_info = TRUE)
  print(user)
}

