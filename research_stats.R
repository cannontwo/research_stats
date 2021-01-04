library(tidyverse)
library(ggTimeSeries)

stats <- read.csv("~/Desktop/research_stats/spr2020stats.csv") %>%
  mutate(date=as.Date(Date, format="%m/%d/%y")) %>% 
  select(date, reading=Reading, writing=Writing, meetings=Meetings, research=Research, misc=Misc., total=Total)

old_stats <- read.csv("~/Desktop/research_stats/spr2019stats.csv") %>%
  mutate(date=as.Date(Date, format="%m/%d/%y")) %>% 
  select(date, reading=Reading, writing=Writing, meetings=Meetings, research=Research, misc=Misc., total=Total)

mid_stats <- read.csv("~/Desktop/research_stats/fall2019stats.csv") %>%
  mutate(date=as.Date(Date, format="%m/%d/%y")) %>% 
  select(date, reading=Reading, writing=Writing, meetings=Meetings, research=Research, misc=Misc., total=Total)

long_stats <- stats %>% gather(attr, count, reading:total) %>%  
  mutate(attr=factor(attr, levels=c("meetings", "reading", "research", "writing", "misc", "total")))
grouped_stats <- long_stats %>% group_by(attr)

long_old_stats <- old_stats %>% gather(attr, count, reading:total) %>% 
  mutate(attr=factor(attr, levels=c("meetings", "reading", "research", "writing", "misc", "total")))
grouped_old_stats <- long_old_stats %>% group_by(attr)

long_mid_stats <- mid_stats %>% gather(attr, count, reading:total) %>% 
  mutate(attr=factor(attr, levels=c("meetings", "reading", "research", "writing", "misc", "total")))
grouped_mid_stats <- long_mid_stats %>% group_by(attr)

ggplot(grouped_stats, aes(date, count, color=attr)) +
  geom_smooth() + 
  geom_line(alpha=0.5) +
  facet_wrap(~attr) + 
  geom_vline(data=filter(stats, total==0), aes(xintercept=date), alpha=0.3) +
  ggtitle("Spring 2020 Research Stats")

ggsave("~/Desktop/research_stats/spr2020stats.png")
  
ggplot(subset(long_stats, attr!="total"), aes(x=date, weight=count, color=attr, fill=attr)) +
  geom_dotplot(stackgroups = TRUE, binwidth=1, binpositions='all') +
  ggtitle("Spring 2020 Research Stats") +
  coord_fixed(ylim = c(0,1 + max(long_old_stats$count)))

ggsave("~/Desktop/research_stats/spr2020stats_stacked_dot.png")

ggplot(grouped_old_stats, aes(date, count, color=attr)) +
  geom_smooth() + 
  geom_line(alpha=0.5) +
  facet_wrap(~attr) + 
  geom_vline(data=filter(old_stats, total==0), aes(xintercept=date), alpha=0.3) +
  ggtitle("Spring 2019 Research Stats")

ggsave("~/Desktop/research_stats/spr2019stats.png")

ggplot(subset(long_old_stats, attr!="total"), aes(x=date, weight=count, color=attr, fill=attr)) +
  geom_dotplot(stackgroups = TRUE, binwidth=1, binpositions='all') +
  ggtitle("Spring 2019 Research Stats") +
  coord_fixed(ylim = c(0,1 + max(long_old_stats$count)))

ggsave("~/Desktop/research_stats/spr2019stats_stacked_dot.png")

ggplot(grouped_mid_stats, aes(date, count, color=attr)) +
  geom_smooth() + 
  geom_line(alpha=0.5) +
  facet_wrap(~attr) + 
  geom_vline(data=filter(mid_stats, total==0), aes(xintercept=date), alpha=0.3) +
  ggtitle("Fall 2019 Research Stats")

ggsave("~/Desktop/research_stats/fall2019stats.png")

ggplot(subset(long_mid_stats, attr!="total"), aes(x=date, weight=count, color=attr, fill=attr)) +
  geom_dotplot(stackgroups = TRUE, binwidth=1, binpositions='all') +
  ggtitle("Fall 2019 Research Stats") +
  coord_fixed(ylim = c(0,1 + max(long_mid_stats$count)))

ggsave("~/Desktop/research_stats/fall2019stats_stacked_dot.png")

