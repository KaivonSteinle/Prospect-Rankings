#Prospect Rankings

#Import the data
Prospect_Rankings <- read_excel("ProspectRankings2007-2020.xlsx")
View(Prospect_Rankings)                                              

#1 or 0 for if they made majors
Prospect_Rankings <- Prospect_Rankings %>% mutate(Made_MLB = case_when(
  HiLvl == "MLB" ~ 1,
  HiLvl != "MLB" ~ 0
))
#Filter for 2010-2018 (time period where players have had enough years to reach majors for the most part)
Prospect_Rankings <- Prospect_Rankings %>% filter(Year >= 2010 & Year <= 2018)

#Filter for hitters and pitchers and then put into excel. I put into excel so I could manually assign a position and remove duplicates
Hitters <- Prospect_Rankings %>% filter(Pos != "P")
View(Hitters)
write.csv(Hitters, "P_Hits.csv")
Pitchers <- Prospect_Rankings %>% filter(Pos == "P")
View(Pitchers)
write.csv(Pitchers, "P_Pitch.csv")

Hitters <- read.csv("P_Hits.csv")
Pitchers <- read.csv("P_Pitch.csv")

#Combine all to show what percent made majors
Hitters <- Hitters %>% mutate(Type = "Hitters")
Pitchers <- Pitchers %>% mutate(Type = "Pitchers")
All <- rbind(Hitters, Pitchers)
majors <- table(All$Type, All$Made_MLB)
majors <- prop.table(majors, 1)
majors <- as.data.frame(majors) %>% filter(Var2 == 1)
#Quick graph of the breakdown
ggplot(majors, aes(fill=Var1, y=round(Freq * 100, 1), x=Var1, label = round(Freq * 100, 1))) + geom_bar(position="dodge", stat="identity") + labs(x = "Player Type", y = "Made Majors Percent") + ggtitle("Percent of Top 100 Prospects That Made the Majors") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face = "bold", size = 14)) + theme( axis.text=element_text(size=10.5), axis.title.x=element_blank()) + theme(axis.title.y = element_text(face = "bold")) + theme(legend.title = element_text(face = "bold")) + theme(axis.text.x = element_text(face="bold"), axis.text.y = element_text(face="bold"), legend.text = element_text(face="bold")) + geom_text(size = 7, parse=TRUE, position = position_dodge(width = 0.7)) + theme(legend.position = "none")  + ylim(0, 100)






#Dealt with data in other R document and can import clean one here
Prospect_Hitters <- read.csv("Prospect_Hitters.csv")
View(Prospect_Hitters)

Prospect_Pitchers <- read.csv("Prospect_Pitchers.csv")
View(Prospect_Pitchers)



#Look at the rookie year relation between WAR and rank

ggplot(Prospect_Hitters, aes(x = BA_Rank, y = WAR)) + geom_point(alpha = 0.6, size = 0.7) + geom_jitter() + geom_smooth() + ggtitle("Batters Rookie Year WAR Based On Prospect Rank") + labs(y = "WAR", x = "Prospect Rank") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face = "bold", size = 13)) + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) +  scale_x_continuous(breaks=seq(0, 100, 10), limits=c(0, 100)) + theme(legend.title = element_blank())
ggplot(Prospect_Pitchers, aes(x = BA_Rank, y = WAR)) + geom_point(alpha = 0.6, size = 0.7) + geom_jitter() + geom_smooth() + ggtitle("Pitchers Rookie Year WAR Based On Prospect Rank") + labs(y = "WAR", x = "Prospect Rank") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face = "bold", size = 13)) + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) +  scale_x_continuous(breaks=seq(0, 100, 10), limits=c(0, 100)) + theme(legend.title = element_blank())

#Relation between second year and WAR
ggplot(Prospect_Hitters, aes(x = BA_Rank, y = WAR_Year2)) + geom_point(alpha = 0.6, size = 0.7) + geom_jitter() + geom_smooth() + ggtitle("Batters Second Year WAR Based On Prospect Rank") + labs(y = "WAR", x = "Prospect Rank") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face = "bold", size = 13)) + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) +  scale_x_continuous(breaks=seq(0, 100, 10), limits=c(0, 100)) + theme(legend.title = element_blank())
ggplot(Prospect_Pitchers, aes(x = BA_Rank, y = WAR_Year2)) + geom_point(alpha = 0.6, size = 0.7) + geom_jitter() + geom_smooth() + ggtitle("Pitchers Second Year WAR Based On Prospect Rank") + labs(y = "WAR", x = "Prospect Rank") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face = "bold", size = 13)) + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) +  scale_x_continuous(breaks=seq(0, 100, 10), limits=c(0, 100)) + theme(legend.title = element_blank())
ggplot(Prospect_Pitchers, aes(x = BA_Rank, y = FIP_Year2)) + geom_point(alpha = 0.6, size = 0.7) + geom_jitter() + geom_smooth() + ggtitle("Pitchers Second Year FIP Based On Prospect Rank") + labs(y = "WAR", x = "Prospect Rank") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face = "bold", size = 13)) + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) +  scale_x_continuous(breaks=seq(0, 100, 10), limits=c(0, 100)) + theme(legend.title = element_blank()) 

#Relation between third year and WAR
ggplot(Prospect_Hitters, aes(x = BA_Rank, y = WAR_Year3)) + geom_point(alpha = 0.6, size = 0.7) + geom_jitter() + geom_smooth() + ggtitle("Batters Third Year WAR Based On Prospect Rank") + labs(y = "WAR", x = "Prospect Rank") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face = "bold", size = 13)) + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) +  scale_x_continuous(breaks=seq(0, 100, 10), limits=c(0, 100)) + theme(legend.title = element_blank())
ggplot(Prospect_Pitchers, aes(x = BA_Rank, y = WAR_Year3)) + geom_point(alpha = 0.6, size = 0.7) + geom_jitter() + geom_smooth() + ggtitle("Pitchers Third Year WAR Based On Prospect Rank") + labs(y = "WAR", x = "Prospect Rank") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face = "bold", size = 13)) + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) +  scale_x_continuous(breaks=seq(0, 100, 10), limits=c(0, 100)) + theme(legend.title = element_blank())
ggplot(Prospect_Pitchers, aes(x = BA_Rank, y = FIP_Year3)) + geom_point(alpha = 0.6, size = 0.7) + geom_jitter() + geom_smooth() + ggtitle("Pitchers Third Year FIP Based On Prospect Rank") + labs(y = "WAR", x = "Prospect Rank") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face = "bold", size = 13)) + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) +  scale_x_continuous(breaks=seq(0, 100, 10), limits=c(0, 100)) + theme(legend.title = element_blank())

#Relation between total WAR
ggplot(Prospect_Hitters, aes(x = BA_Rank, y = Total_WAR)) + geom_point(alpha = 0.6, size = 0.7) + geom_jitter() + geom_smooth() + ggtitle("Batters Total WAR Based On Prospect Rank") + labs(y = "WAR", x = "Prospect Rank") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face = "bold", size = 13)) + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) +  scale_x_continuous(breaks=seq(0, 100, 10), limits=c(0, 100)) + theme(legend.title = element_blank())
ggplot(Prospect_Pitchers, aes(x = BA_Rank, y = Total_WAR)) + geom_point(alpha = 0.6, size = 0.7) + geom_jitter() + geom_smooth() + ggtitle("Pitchers Total WAR Based On Prospect Rank") + labs(y = "WAR", x = "Prospect Rank") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face = "bold", size = 13)) + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) + scale_x_continuous(breaks=seq(0, 100, 10), limits=c(0, 100)) + theme(legend.title = element_blank())


#Groupings for total
Rookie_Hitters <- Prospect_Hitters %>% group_by(Group) %>% filter(!is.na(WAR)) %>% dplyr::summarise(N = n(), Rookie_WAR = mean(WAR))
Y2_Hitters <- Prospect_Hitters %>% group_by(Group) %>% filter(!is.na(WAR_Year2)) %>% dplyr::summarise(N = n(), Y2_WAR = mean(WAR_Year2))
Y3_Hitters <- Prospect_Hitters %>% group_by(Group) %>% filter(!is.na(WAR_Year3)) %>% dplyr::summarise(N = n(), Y3_WAR = mean(WAR_Year3))
Total_Hitters <- Prospect_Hitters %>% group_by(Group) %>% dplyr::summarise(N = n(), Total_WAR = mean(Total_WAR))
Hitter_Groups <- cbind(Rookie_Hitters, Y2_Hitters, Y3_Hitters, Total_Hitters)
View(Hitter_Groups)

Rookie_Pitchers <- Prospect_Pitchers %>% group_by(Group) %>% filter(!is.na(WAR)) %>% dplyr::summarise(N = n(), Rookie_IP = mean(IP), Rookie_FIP = mean(FIP), Rookie_WAR = mean(WAR))
Y2_Pitchers <- Prospect_Pitchers %>% group_by(Group) %>% filter(!is.na(WAR_Year2)) %>% dplyr::summarise(N = n(), Y2_IP = mean(IP_Year2), Y2_FIP = mean(FIP_Year2), Y2_WAR = mean(WAR_Year2))
Y3_Pitchers <- Prospect_Pitchers %>% group_by(Group) %>% filter(!is.na(WAR_Year3)) %>% dplyr::summarise(N = n(), Y3_IP = mean(IP_Year3), Y3_FIP = mean(FIP_Year3), Y3_WAR = mean(WAR_Year3))
Total_Pitchers <- Prospect_Pitchers %>% group_by(Group) %>% dplyr::summarise(N = n(), Total_WAR = mean(Total_WAR))
Pitcher_Groups <- cbind(Rookie_Pitchers, Y2_Pitchers, Y3_Pitchers, Total_Pitchers)
View(Pitcher_Groups)



#Look at position impacts with a variety of groups

Prospect_Hitters <- Prospect_Hitters %>% mutate(Position_Group = case_when(
  Position == "CF" | Position == "SS" ~ "Premium",
  Position == "C" ~ "C",
  Position == "1B" | Position == "LF" | Position == "RF" ~ "Bat First",
  Position == "3B" | Position == "2B" ~ "IF"
), 
Position_Group2 = case_when(
  Position == "CF" | Position == "SS" ~ "Premium",
  Position == "C" ~ "C",
  Position == "1B" | Position == "LF" | Position == "RF" | Position == "3B" | Position == "2B" ~ "Other Bat"
))
Prospect_Hitters$Position_Group3 <- "Batter"
Prospect_Pitchers <- Prospect_Pitchers %>% mutate(Position = case_when(
  Throws == "L" ~ "LHP",
  Throws == "R" ~ "RHP"
)) 
Prospect_Pitchers <- Prospect_Pitchers %>% mutate(Position_Group = case_when(
  Throws == "L" ~ "LHP",
  Throws == "R" ~ "RHP"
), 
Position_Group2 = case_when(
  Throws == "L" ~ "Pitcher",
  Throws == "R" ~ "Pitcher"
))
Prospect_Pitchers$Position_Group3 <- "Pitcher"

Bats <- Prospect_Hitters %>% select("Name", "BA_Rank", "Total_WAR", "Position", "Position_Group", "Position_Group2", "Position_Group3", "Made_MLB")
Pitches <- Prospect_Pitchers %>% select("Name", "BA_Rank", "Total_WAR", "Position" ,"Position_Group", "Position_Group2", "Position_Group3", "Made_MLB")
All_Prospects <- rbind(Bats, Pitches)
View(All_Prospects)

Made <- All_Prospects %>% filter(Made_MLB == 1)
View(Made)

#Since not normally distributed will use Kruskall-Wallis
hist(All_Prospects$Total_WAR)

#Look to see if any significant difference between position groups
kruskal.test(Total_WAR ~ Position_Group2, data = All_Prospects)
kruskal.test(Total_WAR ~ Position_Group2, data = Made)

pairwise.wilcox.test(All_Prospects$Total_WAR, All_Prospects$Position_Group2, p.adjust.method = "BH")
pairwise.wilcox.test(Made$Total_WAR, Made$Position_Group2, p.adjust.method = "BH")

#Significant difference between Premium bats and all positions for people who made it

ggplot(Made, aes(x = as.factor(Position_Group2), y = Total_WAR)) + geom_violin(trim=FALSE, fill="gray") + scale_y_continuous(breaks=seq(-5,30,5), limits=c(-5, 30)) + ggtitle("Total WAR Based On Position Group") + labs(y = "Total WAR", x = "Position Group") + theme_classic() + theme(plot.title = element_text(hjust = 0.5))  + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) + theme(plot.title = element_text(face = "bold", size = 14)) + theme(legend.position='none') + theme(axis.text.x = element_text(face="bold"), axis.text.y = element_text(face="bold")) + geom_boxplot(width=0.25, fill="white", outlier.shape = NA)


#See if position impacts rank
ggplot(All_Prospects, aes(x = BA_Rank, y = as.factor(Position), fill = ..quantile..)) + stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.25, 0.75), scale = 1) + scale_fill_manual(name = "BA Rank", "values" = c("#0000FFA0", "#A0A0A0A0", "#FF0000A0"), labels = c("(0, 0.010]", "(0.010, 0.90]", "(0.90, 1]")) + ggtitle("BA Rank Based On Position") + labs(y = "Position", x = "BA Rank") + theme(plot.title = element_text(hjust = 0.5))  + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) + theme(plot.title = element_text(face = "bold", size = 14)) + theme(legend.position='none') + theme(axis.text.x = element_text(face="bold"), axis.text.y = element_text(face="bold")) + scale_x_continuous(breaks=seq(0,100,10), limits=c(0, 100))

ggplot(All_Prospects, aes(y = BA_Rank, x = as.factor(Position))) + geom_violin(trim=FALSE, fill="gray") + scale_y_continuous(breaks=seq(0,100,10), limits=c(0, 100)) + ggtitle("Prospect Rank Based On Position") + labs(y = "Prospect Rank", x = "Position") + theme_classic() + theme(plot.title = element_text(hjust = 0.5))  + theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold")) + theme(plot.title = element_text(face = "bold", size = 14)) + theme(legend.position='none') + theme(axis.text.x = element_text(face="bold"), axis.text.y = element_text(face="bold")) + geom_boxplot(width=0.25, fill="white", outlier.shape = NA)

