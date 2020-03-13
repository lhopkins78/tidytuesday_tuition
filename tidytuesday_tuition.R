library(tidyverse)
library(ggrepel)

#get data
tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 
salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')
historical_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')
diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')

#combine data
diversity_school_wide <- diversity_school %>% pivot_wider(names_from=category, values_from=enrollment)

tuition_merge <- merge(diversity_school_wide, salary_potential)
tuition_merge <- merge(tuition_merge,tuition_cost)
tuition_merge[,3:14] <- sapply(tuition_merge[,3:14],as.numeric)
tuition_merge <- tuition_merge %>% mutate(per_white=White/total_enrollment, per_black=Black/total_enrollment,
                                          per_hispanic=Hispanic/total_enrollment, per_asian=Asian/total_enrollment,
                                          per_women=Women/total_enrollment, per_minority=`Total Minority`/total_enrollment,
                                          in_state_ratio=mid_career_pay/in_state_total, out_of_state_ratio=mid_career_pay/out_of_state_total)
tuition_merge$enrollment_group <- cut(tuition_merge$total_enrollment, breaks=quantile(tuition_merge$total_enrollment, c(0.25,0.5,0.75)))

#cluster analysis - midcareer pay v in state
tuition_dist <- tuition_merge %>% select(mid_career_pay,in_state_total)
rownames(tuition_dist) <- tuition_merge$name
t_dist <- dist(scale(tuition_dist))
t_clust <- hclust(t_dist, method="complete")
cluster_t <- cutree(t_clust, h=3)
tuition_clust <- tuition_merge %>% mutate(cluster=cluster_t)

#scatter plot with clusters
p1 <- ggplot(tuition_clust, aes(x=in_state_total, y=mid_career_pay, size=total_enrollment, shape=type, 
                          col=factor(cluster))) + 
  geom_point(alpha=0.5) +theme_minimal() + geom_text_repel(data=tuition_clust %>% filter(cluster==3 & mid_career_pay>135000),
                                                     aes(x=in_state_total, y=mid_career_pay, label=name),
                                                         col="black", size=3, nudge_x=-6000, nudge_y=1000) +
                                          geom_text_repel(data=tuition_clust %>% filter(mid_career_pay>115000 & cluster==5), 
                                                    aes(x=in_state_total, y=mid_career_pay, label=name), 
                                                    col="black", size=3, nudge_x=-6000, nudge_y=1000) +
  geom_text_repel(data=tuition_clust %>% filter(mid_career_pay<65000 & cluster==1), 
                  aes(x=in_state_total, y=mid_career_pay, label=name), 
                  col="black", size=3, nudge_x=-6000, nudge_y=1000) +
  geom_text_repel(data=tuition_clust %>% filter(mid_career_pay<100000 & in_state_total>60000), 
                  aes(x=in_state_total, y=mid_career_pay, label=name), 
                  col="black", size=3, nudge_x=-6000, nudge_y=1000) +
                                                    scale_color_brewer(palette="Set1", name="Group (cluster)", labels=c("Low pay","Medium pay, high tuition", "Highest pay, high tuition", "High pay, high tuition", "Medium pay, low tuition")) +
                                                    labs(shape="Public / Private", title="Return on investment?", subtitle="Mid-career pay by total annual in-state tuition and board costs", size="Total enrollment",
                                                         x="Average total in-state tuition and board costs (USD)", y="Average mid career pay (USD)", caption="Sources: Chronicle of Higher Education, Payscale.com c/o TidyTuesday @ https://github.com/rfordatascience/tidytuesday")

p2 <- p1 + facet_wrap(~type, nrow=2)

ggsave("tuition1.png", plot=p1, dpi="retina", width=15)
ggsave("tuition2.png", plot=p2, dpi="retina", width=15)

#cluster analysis - midcareer pay v out state
tuition_dist <- tuition_merge %>% select(mid_career_pay,out_of_state_total)
rownames(tuition_dist) <- tuition_merge$name
t_dist <- dist(scale(tuition_dist))
t_clust <- hclust(t_dist, method="complete")
cluster_t <- cutree(t_clust, k=5)
tuition_clust_out <- tuition_merge %>% mutate(cluster=cluster_t)

#scatter plot with clusters
p3 <- ggplot(tuition_clust_out, aes(x=out_of_state_total, y=mid_career_pay, size=total_enrollment, shape=type, 
                          col=factor(cluster))) + 
  geom_point(alpha=0.5) +theme_minimal() + geom_text_repel(data=tuition_clust_out %>% filter(cluster==3 & mid_career_pay>135000),
                                                     aes(x=out_of_state_total, y=mid_career_pay, label=name),
                                                     col="black", size=3, nudge_x=-6000, nudge_y=1000) +
  geom_text_repel(data=tuition_clust_out %>% filter(mid_career_pay>110000 & cluster==4), 
                  aes(x=out_of_state_total, y=mid_career_pay, label=name), 
                  col="black", size=3, nudge_x=-6000, nudge_y=1000) +
  geom_text_repel(data=tuition_clust_out %>% filter(mid_career_pay<65000 & cluster==1), 
                  aes(x=out_of_state_total, y=mid_career_pay, label=name), 
                  col="black", size=3, nudge_x=-6000, nudge_y=1000) +
  geom_text_repel(data=tuition_clust_out %>% filter(mid_career_pay<100000 & out_of_state_total>60000), 
                  aes(x=out_of_state_total, y=mid_career_pay, label=name), 
                  col="black", size=3, nudge_x=-6000, nudge_y=1000) +
  geom_text_repel(data=tuition_clust_out %>% filter(mid_career_pay>80000 & out_of_state_total<10000), 
                  aes(x=out_of_state_total, y=mid_career_pay, label=name), 
                  col="black", size=3, nudge_x=-6000, nudge_y=1000) +
  scale_color_brewer(palette="Dark2", name="Group (cluster)", labels=c("Low pay","Medium pay, high tuition", "Highest pay, high tuition", "Medium pay, low tuition", "High pay, high tuition")) +
  labs(shape="Public / Private", title="Return on investment?", subtitle="Mid-career pay by total annual out-of-state tuition and board costs", size="Total enrollment",
       x="Average total out-of-state tuition and board costs (USD)", y="Average mid career pay (USD)", caption="Sources: Chronicle of Higher Education, Payscale.com c/o TidyTuesday @ https://github.com/rfordatascience/tidytuesday")

p4 <- p3 + facet_wrap(~type, nrow=2)


ggsave("tuition3.png", plot=p3, dpi="retina", width=15)
ggsave("tuition4.png", plot=p4, dpi="retina", width=15)



