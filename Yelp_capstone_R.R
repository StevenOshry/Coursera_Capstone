# data prep for project 
# assign business location to 1 of 9 cities
# illustrate random samples
# test_df <-biz_data[sample(nrow(biz_data), 100, replace=FALSE),c("business_id", "full_address", "latitude","longitude" )]
# illustrate sample of dplyr

# #fires %>%
# #  filter(type == "WF") %>%
#   select(state, area) %>%
#   group_by(state) %>%
#   summarize(total=sum(area, na.rm=TRUE)) %>%
#   arrange(desc(total))


setwd("~/Coursera/Capstone")
# read in data set of 10 mjor cities 
major_cities <- read.csv("city_lat_lon.csv", header = TRUE,stringsAsFactors = FALSE)
major_cities$city_num <- seq(1, nrow(major_cities),1)
biz_data  <- readRDS("./yelp_dataset_challenge_academic_dataset/business_data.rds")

# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}


#assign closest city to each location in biz data based on minimum earth distance

for (data_row in 1:nrow(biz_data)){
major_cities$distance<-earth.dist(biz_data[data_row,"longitude"],biz_data[data_row,"latitude"],
                                  major_cities$longitude, major_cities$latitude )
closest_city <-major_cities[which(major_cities$distance==min(major_cities$distance)),]

biz_data$major_city[data_row]<-closest_city$city

biz_data$major_country[data_row]<-closest_city$Country
biz_data$city_num[data_row]<-closest_city$city_num


}
#biz_data[,c("full_address","major_city","major_country")]
biz_data$full_address<-gsub("\n",",",biz_data$full_address)

# find restaurants and create restaurant indicator

for (data_row in 1:nrow(biz_data))
{
  
  category<-biz_data$categories[[data_row]]
  biz_data$rest_ind[data_row]<-sum(category=="Restaurants")
 
}

# count categories

#create empty data frame
# 
# catgories <- data.frame(business_id=character(),
#                         category=character()
# )
# 
# 
# for (data_row in  1:nrow(biz_data))
# {
#   business_id=rep(biz_data$business_id[data_row],length(biz_data$categories[[data_row]]))
#   category<-biz_data$categories[[data_row]]
#   sub_df<-as.data.frame(cbind(business_id,category))
#   #add this data frame to the already existing cagoroes data frame
#   catgories<-rbind(catgories,sub_df)
# }


#count business by  industry
library(dplyr)

catg_sumry_tbl <- as.data.frame(catgories %>%
                                              select(category, business_id) %>%
                                              group_by (category)  %>%
                                              summarise(biz=n_distinct(business_id) ) %>%
                                  arrange(desc(biz))
)


# create dataframe of only restuarants 
biz_data_rest<- subset(biz_data, rest_ind==1, select=c(business_id,   full_address, name, major_country, major_city, city_num, review_count  ))



# read user data 
user_data  <- readRDS("./yelp_dataset_challenge_academic_dataset/user_data.rds")
names(user_data)


review_data  <- readRDS("./yelp_dataset_challenge_academic_dataset/review_data.rds")
review_data$real_date<-as.Date(review_data$date, format = "%Y-%m-%d")
review_data$l12mon <-review_data$real_date >= '2014-01-09'

most_recent <-max(review_data$real_date)
names(review_data)

review_data_subs<-review_data[, c("user_id" , "review_id",   "stars",  "business_id" ,"real_date","l12mon" )]
# add city to review_data
biz_cities<- biz_data[, c("city_num", "major_city", "business_id", "rest_ind")]
#now merge this to review_data
review_data_w_city <-merge(review_data_subs, biz_cities,by="business_id" )

#  EDA of reviews by restaurant vs non-resaurant industry


reviews_ind_w_factor <-review_data_w_city[,]

reviews_ind_w_factor$ind_type <- as.factor(ifelse(reviews_ind_w_factor$rest_ind==1  ,
                                   "Restaurant", "Oth")
)



# This part makes table of restaurant reviews by city

city_sumry_review_type_tbl <- as.data.frame(reviews_ind_w_factor %>%
  select(major_city, city_num , review_id, ind_type) %>%
  group_by (major_city, city_num ,ind_type)  %>%
    summarise(reviews=n_distinct(review_id) ) %>%
    mutate(Restaurant_pct_tot_Reviews=reviews/sum(reviews)*100)  
  
)
 

 
sumry_review_type_tbl <- as.data.frame(reviews_ind_w_factor %>%
                                         select( review_id, ind_type) %>%
                                         group_by (ind_type)  %>%
                                         summarise(reviews=n_distinct(review_id) ) %>%
                                         mutate(Restaurant_pct_tot_Reviews=reviews/sum(reviews)*100)
)
sumry_review_type_tbl$city_num<-11
sumry_review_type_tbl$major_city="ALL"
sumry_review_type_tbl2<- rbind(city_sumry_review_type_tbl,sumry_review_type_tbl)

sumry_review_type_tbl2<-sumry_review_type_tbl2<-sumry_review_type_tbl2 %>%
  filter(ind_type=="Restaurant") %>%
  select(city=major_city, reviews, Restaurant_pct_tot_Reviews) %>%
    arrange(desc(reviews))

# end make table of restaurant reviews by city


review_data_restr <-merge(review_data_subs, biz_data_rest, by="business_id")



# prep wprk to idenitfy frequent local reviewer
# for each user and city, count reviews and get first and last review date

review_by_user_city_tbl<- as.data.frame(review_data_w_city  %>%
  select(major_city, city_num ,user_id, real_date, review_id) %>%
  group_by(major_city, city_num ,user_id) %>%
  summarize(min_date= min(real_date),max_date= max(real_date), reviews=n_distinct(review_id) )
)
review_by_user_city_tbl$local<- ifelse(review_by_user_city_tbl$max_date-review_by_user_city_tbl$min_date>14,"Y","N")
review_by_user_city_tbl$local_freq<-ifelse(review_by_user_city_tbl$local=="Y"&review_by_user_city_tbl$reviews>2,"Y","N")

#in review by city user table, create class var based on how many reviews

review_by_user_city_tbl$review_grp <-cut(review_by_user_city_tbl$reviews, breaks=c(0,1,2,4,10, Inf),
                                   labels=c("1","2", "3-4", "5-10", ">10"))
review_by_user_city_tbl$review_grp_2 <- ifelse(review_by_user_city_tbl$reviews<5, "0-4", "10+")

user_buckets_df <-as.data.frame(review_by_user_city_tbl  %>%
  select(city=major_city, numb_reviews= review_grp, user_id) %>%
  group_by(city,  numb_reviews ) %>%
  summarize( users=n_distinct(user_id) ) %>%
    mutate( pct=users/sum(users)*100 ) 
  
)




#field "local_freq" was created for each reviewer in each city if there were at least 5 reviews in that city and the time period spanned
# more than 2 weeks


user_city_good_reviewer_tbl<-subset(review_by_user_city_tbl, select= c("user_id", "city_num", "local_freq"))
review_data_restr2<-merge(review_data_restr,user_city_good_reviewer_tbl, by=c("user_id", "city_num")  )
table(review_data_restr2$major_city,review_data_restr2$local_freq)

#loacl data set is Local reviewrs AND ONLY reviews in last 12 months
review_restr_local<-subset(review_data_restr2, local_freq=="Y"  &  l12mon==1)


review_restr_oth<-subset(review_data_restr2, local_freq=="N")
# get review summary by restaurant for all restaurants and cities
# this is what Yelp site looks like

reviews_by_rest_all<- as.data.frame(review_data_restr2  %>%
                                      select(major_city, city_num , business_id, name, stars,  review_id) %>%
                                      group_by(major_city, city_num , business_id, name) %>%
                                      summarize(avg_all=mean(stars), reviews_all=n_distinct(review_id), sd_all=sd(stars) )
)


reviews_by_rest_all_ranked <-as.data.frame(reviews_by_rest_all   %>%
                                                                                  
                                             arrange(city_num, desc(avg_all), desc(reviews_all) ) %>%
                                             group_by ( city_num )  %>%
                                             mutate(rank_all=row_number())
)

# finish creating review data all





#create data frame of major city , restaurant, all reviews, avg_review for local_freq

reviews_by_rest_loc<- as.data.frame(review_restr_local  %>%
  select(major_city, city_num , business_id, name, stars,  review_id) %>%
  group_by(major_city, city_num , business_id, name) %>%
  summarize(avg_l=mean(stars), reviews_l=n_distinct(review_id), sd_l=sd(stars) )
)


reviews_by_rest_loc_ranked <-as.data.frame(reviews_by_rest_loc   %>%
  #use filter to keep only restaurants with 3 or more local frequent reviewers
    
                                             filter(reviews_l>2)     %>%                                      
  arrange(city_num, desc(avg_l), desc(reviews_l) ) %>%
  group_by ( city_num )  %>%
mutate(rank_l=row_number())
)

# create dataframe for restaurants reviewed by non-local_freq reviewers
#then compare differences
reviews_by_rest_oth<- as.data.frame(review_restr_oth  %>%
                                      select(major_city, city_num , business_id, name, stars,  review_id) %>%
                                      group_by(major_city, city_num , business_id, name) %>%
                                      summarize(avg_oth=mean(stars), reviews_oth=n_distinct(review_id), sd_oth=sd(stars))
)


reviews_by_rest_oth_ranked <-as.data.frame(reviews_by_rest_oth   %>%
                                             #use filter to keep only restaurants with 3 or more local frequent reviewers
                                             
                                             filter(reviews_oth>2)     %>%                                      
                                             arrange(city_num, desc(avg_oth), desc(reviews_oth) ) %>%
                                             group_by ( city_num )  %>%
                                             mutate(rank_oth=row_number())
)


#compare ratings and rank for local frequent reviewrs  vs non-local

reviews_oth_subs<-reviews_by_rest_oth_ranked[,c("business_id","avg_oth","reviews_oth","rank_oth", "sd_oth")]
loc_vs_oth_df <- merge(reviews_by_rest_loc_ranked,reviews_oth_subs,by="business_id" )
loc_vs_oth_df2<- as.data.frame( loc_vs_oth_df %>%
  arrange(city_num, rank_l) 
)

# create function for ttest between groups


T_pval <- function (avg_1, avg_2, sd_1, sd_2, reviews_1, reviews_2)
{
  #Welch's t-test for unequal sample sizes, unequal variances using the Welch-Satterwaite equation to calculate degress of freedom
  var_1 <-sd_1^2
  var_2 <-sd_2^2
  t_denom<-sqrt(var_1/reviews_1 + var_2/reviews_2)
  t_numer <-abs(avg_1 - avg_2)
  t_stat_calced<- -1*t_numer/t_denom
  # calculate degrees of freedom
  df_num <- (var_1/reviews_1 + var_2/reviews_2)^2
  # denominator has 2 parts
  df_denom <- ((var_1/reviews_1)^2/(reviews_1-1)) + ((var_2/reviews_2)^2/(reviews_2-1))
  deg_freed_calced<-df_num/df_denom
  pval<-2*pt(t_stat_calced,deg_freed_calced)
  return(pval)
}
loc_vs_oth_df2$sig_test<-T_pval(loc_vs_oth_df2$avg_l,loc_vs_oth_df2$avg_oth,loc_vs_oth_df2$sd_l,
                                loc_vs_oth_df2$sd_oth, loc_vs_oth_df2$reviews_l, loc_vs_oth_df2$reviews_oth
)
loc_vs_oth_df2$sig_test[loc_vs_oth_df2$sd_l==0&loc_vs_oth_df2$sd_oth==0& loc_vs_oth_df2$avg_l==loc_vs_oth_df2$avg_oth] <-1
loc_vs_oth_df2$sig_test[loc_vs_oth_df2$sd_l==0&loc_vs_oth_df2$sd_oth==0& loc_vs_oth_df2$avg_l!=loc_vs_oth_df2$avg_oth] <-0



loc_vs_oth_df2$p_value<-as.numeric(round(loc_vs_oth_df2$sig_test,2))
loc_vs_oth_df2$sig_star<-ifelse(loc_vs_oth_df2$p_value<=0.05,"*", "")


# sds=0 means are same Ek9oZDBw3sC_ZxVyHMPFdg
# sds=0 means are diff m_BnpyGYYMxrcqLALaRwjA

# if any city < 30 restaurants delte them
table(loc_vs_oth_df2$city_num)
# after looking at table, get rid of cities 2 & 4 (Karlshrue and Waterloo)as they have fewer than 30 total restaurants
comparison_df <-subset(loc_vs_oth_df2, city_num!=2 & city_num!=4)
comparison_df$sig_diff<- comparison_df$p_value<=0.05
comparison_df$significant.difference<-ifelse( comparison_df$p_value<=0.05, "Yes", "No")

hidden_gem <-subset(comparison_df, p_value<=0.10 & avg_l>=4 & avg_oth<=3)
over_rated<-subset(comparison_df, p_value<=0.10 & avg_l<=3 & avg_oth>=4)



# create empty data_frame
corr_test_df <- data.frame(
  
  city=character(),
  corr.pearson=double(),
  pvalue.pearson=double(),
  corr.spearman=double(),
  pvalue.spearman=double(),
  restaurants=integer(),
  significant.restaurants=integer(),
  sig.pct=character(),
  stringsAsFactors=FALSE)



unique_city_nums <-as.data.frame(comparison_df   %>%
                                   select(city_num, city=major_city, business_id, sig_diff) %>%
                                   group_by(city_num, city) %>%
                                   summarize(restaurants=n_distinct(business_id), found_diff=sum(sig_diff))  %>%
                                   mutate(sig_pct=paste(round(100*found_diff/restaurants,0),"%",sep = "")) %>%
                                   arrange(desc(restaurants))
                                 
)

unique_city_nums <-arrange(unique_city_nums, desc(restaurants))
unique_city_nums$city_num2 <- seq(1, nrow(unique_city_nums),1)


  
  



for (data_row in 1:nrow(unique_city_nums))
{
# get correlation coeffiecient

corr_test_df[data_row,"corr.pearson"]<- as.numeric(round(cor(comparison_df$avg_l[comparison_df$city_num==unique_city_nums[data_row,"city_num"]],
                                   comparison_df$avg_oth[comparison_df$city_num==unique_city_nums[data_row,"city_num"]],method="pearson"),3))

corr_test_df[data_row,"pvalue.pearson"]<- as.numeric(round(cor.test(comparison_df$avg_l[comparison_df$city_num==unique_city_nums[data_row,"city_num"]],
                                                       comparison_df$avg_oth[comparison_df$city_num==unique_city_nums[data_row,"city_num"]],
                                                       method="pearson", alternative = "two.sided")$p.value,3))


corr_test_df[data_row,"corr.spearman"]<- as.numeric(round(cor(comparison_df$avg_l[comparison_df$city_num==unique_city_nums[data_row,"city_num"]],
                                                             comparison_df$avg_oth[comparison_df$city_num==unique_city_nums[data_row,"city_num"]],
                                                             method="spearman"),3))

corr_test_df[data_row,"pvalue.spearman"]<- as.numeric(round(cor.test(comparison_df$avg_l[comparison_df$city_num==unique_city_nums[data_row,"city_num"]],
                                                                    comparison_df$avg_oth[comparison_df$city_num==unique_city_nums[data_row,"city_num"]],
                                                                    method="spearman", alternative = "two.sided")$p.value,3))
corr_test_df[data_row,"city"]<-unique_city_nums[data_row,"city"]
corr_test_df[data_row,"restaurants"]<-unique_city_nums[data_row,"restaurants"]
corr_test_df[data_row,"city_num"]<-unique_city_nums[data_row,"city_num"]
corr_test_df[data_row,"significant.diff"]<-unique_city_nums[data_row,"found_diff"]


}

#EDA of datta so far

review_by_user_city_tbl$review_grp <-cut(review_by_user_city_tbl$reviews, breaks=c(0,1,2,4,10, Inf),
                                         labels=c("1","2", "3-4", "5-10", ">10"))
review_by_user_city_tbl$review_grp_2 <- ifelse(review_by_user_city_tbl$reviews<5, "0-4", "10+")

user_buckets_df <-as.data.frame(review_by_user_city_tbl  %>%
                                  select(major_city, numb_reviews= review_grp, user_id) %>%
                                  group_by(major_city,  numb_reviews ) %>%
                                  summarize( users=n_distinct(user_id) ) %>%
                                  mutate( pct=users/sum(users)*100 ) 
                                
)

# make rest_rvwr_EDA_tbl
# create qualified_review_ind
review_data_restr2$Qualified_ind<-ifelse(review_data_restr2$local_freq=="N", "N",
                                         ifelse(review_data_restr2$local_freq=="Y" & review_data_restr2$l12mon==1,"Y", "O"   )
)








revwr_bus_EDA_tbl<- as.data.frame(review_data_restr2 %>%
                                    filter(Qualified_ind !="O")     %>%      
                                    select(city=major_city, business_id   ,review_id, Qualified_ind, user_id) %>%
                                    group_by(city,business_id,Qualified_ind  ) %>%
                                    summarize( reviews=n_distinct(review_id) )
)

qualified_bus_tbl<-as.data.frame(revwr_bus_EDA_tbl %>%
                                   filter(Qualified_ind =="Y" & reviews>2)     %>%      
                                   select(city, business_id   ) %>%
                                   group_by(city,business_id)
  
                                   )
Not_qual_bus_tbl<-as.data.frame(revwr_bus_EDA_tbl %>%
                                   filter(Qualified_ind =="N" & reviews>2)     %>%      
                                   select(city, business_id   ) %>%
                                   group_by(city,business_id)
                                 
)

  
  
  
  

business_EDA_tbl<- merge(x=qualified_bus_tbl,y=Not_qual_bus_tbl, by="business_id")
names(business_EDA_tbl)[2]="city"
business_EDA_tbl2<-as.data.frame(business_EDA_tbl %>%
                                   select(city, business_id   ) %>%
                                  group_by(city) %>%
                                   summarize( restaurants=n_distinct(business_id) )%>%
                                 arrange( desc(restaurants))
)




library(ggplot2)

q<- ggplot(sumry_review_type_tbl2,aes(x=city, y=Restaurant_pct_tot_Reviews))+ geom_bar(stat='identity', width=0.5)
q<- q +   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))
q<- q + ggtitle("Restaurant % of reviews by city")
q<- q+ theme(plot.title = element_text(size=10, face="bold"))
q<- q+theme(axis.title.x = element_text(face="bold", colour="black", size=8))
q<- q+theme(axis.title.y = element_text(face="bold", colour="black", size=12))

q<- q+theme(axis.text = element_text(face="bold", colour="black", size=10))
q<- q+ labs(x=NULL,y="Percent of Reviews")

q<- q+ylim(0,100)

w<- ggplot(user_buckets_df,aes(x=major_city, y=pct, fill=numb_reviews))+ geom_bar(stat='identity', colour="black", width=0.5)
w<- w+ guides(fill=guide_legend(reverse=TRUE))
w<- w + scale_fill_brewer(palette=2)
w<- w +   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))
w<- w + ggtitle("Total Review distribution\n Reviewers by city")
w<- w+ theme(plot.title = element_text(size=10, face="bold"))
w<- w+theme(axis.title.x = element_text(face="bold", colour="black", size=8))
w<- w+theme(axis.title.y = element_text(face="bold", colour="black", size=12))
w<- w+ guides(fill=guide_legend(title=NULL))
w<- w +  theme(legend.text=element_text(size=8, face="bold"))
w<- w+theme(axis.text = element_text(face="bold", colour="black", size=10))
w<- w+ labs(x=NULL,y="Percent of Users")


z<- ggplot(business_EDA_tbl2,aes(x=city, y=restaurants))+ geom_bar(stat='identity', width=0.5)
z<- z +   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))
z<- z + ggtitle("Restaurants with at least 3 reviews \n  Qual & Non-qual users \n by city")
z<- z+ theme(plot.title = element_text(size=10, face="bold"))
z<- z+theme(axis.title.x = element_text(face="bold", colour="black", size=8))
z<- z+theme(axis.title.y = element_text(face="bold", colour="black", size=12))

z<- z+theme(axis.text = element_text(face="bold", colour="black", size=10))
z<- z+ labs(x=NULL,y="Restaurants")




library(grid)
pushViewport(viewport(layout = grid.layout(1, 3)))
print(q, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(w, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(z, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))

dev.copy(svg,   width=16, height=5, 'plot21.svg')
dev.off()

setwd("~/Coursera/Capstone")
comparison_df3  <- readRDS("comparison_df.rds")
reviews_by_rest_all_ranked7  <- readRDS("reviews_by_rest_all_ranked.rds")
user_buckets_df7  <- readRDS("user_buckets_df.rds")
sumry_review_type_tbl27  <- readRDS("sumry_review_type_tbl2.rds")
business_EDA_tbl27  <- readRDS("business_EDA_tbl2.rds")



# scatter plots
#comparison_df2<-subset(comparison_df, city_num %in% c(5,8,9))

s<- ggplot(comparison_df , aes(x=avg_oth, y=avg_l,colour=significant.difference))  +geom_point( size=3) 
s<- s+ labs(x="Rating from non local reviewer",y="Rating from Local Frequent reviewers")
s<- s+ scale_color_manual(values = c("darkblue",  "red"))
#s<- s+ stat_smooth(method=lm, formula= y~ x,level=0.95)
s<- s + ggtitle("Local vs. non-local restaurant reviewer ratings")
s<- s + theme(plot.title = element_text(size=20, face="bold"))
s<- s+theme(axis.title = element_text(face="bold", colour="black", size=20))
s<- s+theme(axis.text = element_text(face="bold", colour="black", size=15))
s<- s+ theme(panel.background = element_rect(fill="lightblue"))
s<- s + theme( panel.grid.major = element_line(size = 0.5, linetype = 'dashed',
                                        colour = "black") )
s<- s+  theme( axis.line = element_line(colour = "black", 
                                    size = 1, linetype = "solid"))
s<- s+ facet_wrap( ~major_city, ncol=3) 
s<- s+  theme(strip.text=element_text(face="bold", size=17),
        strip.background=element_rect(fill="white", colour="black", 
                                      size=1))
s<- s+ theme(legend.justification=c(1,0), legend.position=c(1,0))
# Title appearance
s<- s+  theme(legend.title = element_text(colour="black", size=15, face="bold"))

# Label appearance
s<- s+ theme(legend.text = element_text(colour="black", size = 15, face = "bold"))
s<- s+  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
s<- s+ylim(1,5)

#s<- s+ theme(axis.ticks.margin = unit(1, "cm"))

s  
 

# get distribution of reviewers with 1 review


review_cnt_tbl <- as.data.frame(reviews_ind_w_factor %>%
                                    select(user_id, review_id)%>%
                                  group_by(user_id)%>%
                                  summarize(reviews=n_distinct(review_id))


)
one_reviewer_ids <- as.data.frame(review_cnt_tbl %>%
                                    filter(reviews==1)%>%
                                    select(user_id)
                                  
)

one_reviewer_reviews <- merge( reviews_ind_w_factor, one_reviewer_ids, by="user_id")
one_reviewer_rest_reviews<- subset(one_reviewer_reviews, rest_ind==1)
table(one_reviewer_rest_reviews$stars)

# show hidden gems
hidden_gem_phnx <-subset(comparison_df, p_value<=0.10 & avg_l>=4.3 & avg_oth<=3 & city_num==8 &sig_test <=0.05)
all_rating_phnx<- subset(reviews_by_rest_all_ranked, city_num==8, select=c("business_id", "avg_all", "reviews_all", "rank_all"))
hidden_gem_phnx2<-merge(hidden_gem_phnx, all_rating_phnx, by="business_id")
hidden_gem_phnx2$avg_qual<-as.numeric(round(hidden_gem_phnx2$avg_l,2))
hidden_gem_phnx2$avg_oth<-as.numeric(round(hidden_gem_phnx2$avg_oth,2))
hidden_gem_phnx2$avg_all<-as.numeric(round(hidden_gem_phnx2$avg_all,2))
hidden_gem_phoenix<- as.data.frame(hidden_gem_phnx2  %>%
                                     select(name, avg_qual, reviews_qual=reviews_l, avg_oth, reviews_oth, avg_all, reviews_all, p_value) %>%
                                   arrange(desc(avg_qual))
  
  
)
# show hidden gems vegas
hidden_gem_v <-subset(comparison_df, p_value<=0.10 & avg_l>4 & avg_oth<=3 & city_num==9 &sig_test <=0.05)
all_rating_v<- subset(reviews_by_rest_all_ranked, city_num==9, select=c("business_id", "avg_all", "reviews_all", "rank_all"))
hidden_gem_v2<-merge(hidden_gem_v, all_rating_v, by="business_id")
hidden_gem_v2$avg_qual<-as.numeric(round(hidden_gem_v2$avg_l,2))
hidden_gem_v2$avg_oth<-as.numeric(round(hidden_gem_v2$avg_oth,2))
hidden_gem_v2$avg_all<-as.numeric(round(hidden_gem_v2$avg_all,2))
hidden_gem_vegas<- as.data.frame(hidden_gem_v2  %>%
                                   select(name, avg_qual, reviews_qual=reviews_l, avg_oth, reviews_oth, avg_all, reviews_all, p_value) %>%
                                   arrange(desc(avg_qual))
                                 
                                 
)


# show hidden gems montreal
hidden_gem_m <-subset(comparison_df,  avg_l>=4 & avg_oth<=3 & city_num==3 &sig_test <=0.05)
all_rating_m<- subset(reviews_by_rest_all_ranked, city_num==3, select=c("business_id", "avg_all", "reviews_all", "rank_all"))
hidden_gem_m2<-merge(hidden_gem_m, all_rating_m, by="business_id")
hidden_gem_m2$avg_qual<-as.numeric(round(hidden_gem_m2$avg_l,2))
hidden_gem_m2$avg_oth<-as.numeric(round(hidden_gem_m2$avg_oth,2))
hidden_gem_m2$avg_all<-as.numeric(round(hidden_gem_m2$avg_all,2))
hidden_gem_montreal<- as.data.frame(hidden_gem_m2  %>%
                                   select(name, avg_qual, reviews_qual=reviews_l, avg_oth, reviews_oth, avg_all, reviews_all, rank_all, p_value) %>%
                                   arrange(desc(avg_qual))
                                 
                                 
)
# make rdatasets for use in R presentation

saveRDS(comparison_df, "comparison_df.rds")
saveRDS(reviews_by_rest_all_ranked, "reviews_by_rest_all_ranked.rds")
saveRDS(user_buckets_df,"user_buckets_df.rds")

saveRDS(sumry_review_type_tbl2,"sumry_review_type_tbl2.rds")
saveRDS(business_EDA_tbl2,"business_EDA_tbl2.rds")


# to read it back in    mod2 <- readRDS("mymodel.rds")


dev.copy(png,  width = 1500, height = 940, units = "px", 'plot21.png')
dev.off()
# use to control plotting
library(grid)
#pushViewport(viewport(layout = grid.layout(1, 2)))
#print(g, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
#print(r, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))





#create discrete bucket varaible for review_count

reviews_by_rest$review_class <-cut(reviews_by_rest$reviews, breaks=c(0,5,10,20,30, Inf),
                                   labels=c("<=5","5-10","10-20","20-30",">30"))

review_buckets_rest <-reviews_by_rest  %>%
  select(major_city, city_num , review_class, reviews) %>%
  group_by(major_city, city_num , review_class ) %>%
  summarize( reviews=sum(reviews) )











test_df <-subset(review_data_w_city, user_id=="-_1ctLaz3jhPYc12hKXsEQ")
super_user_df <-subset(user_data, user_id=="-_1ctLaz3jhPYc12hKXsEQ")

cities23<-unique(test_df$city_num)


# most recent review 1/8/2015

#create data frame of restaurants, city, and number of reviews

names(biz_data)

names(review_data_restr)

reviews_by_rest<-sqldf("Select major_city, city_num , business_id, name,  review_count, count(distinct review_id) as reviews

                       
                       from
review_data_restr

                  
                       group by
 major_city, city_num , business_id, name,  review_count

                       ")


review_samples <-review_data[sample(nrow(review_data), 100, replace=FALSE),c("user_id" ,"review_id",   "stars", "date","business_id", "real_date" )]
review_samples$date2<-as.Date(review_samples$date)
review_samples$l12mon <-review_samples$date2 >= '2014-01-09'
review_samples$l24mon <-review_samples$date2 >= '2013-01-09'
review_samples$l12_24mon <-review_samples$date2 >= '2013-01-09'&review_samples$date2 < '2014-01-09'

x<-dev.size("px")