#### libraries ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(maps)
library(mapproj)
library(stringi)


#### read in data ####
setwd("/Users/Palma/Documents/Miami Graduate School/Fall 2016/STA 660/Project 3/data")
entity <- read.csv("1_Entity.csv")                        # relationship information, demographics  
athletics <- read.csv("2_Athletics.csv")                  # athletics information, including dates
giving <- read.csv("3_givingdata.csv")                    # donation information, transactions per customer
degree <- read.csv("4_Degree.csv")                        # degree information (school, year)
contact <- read.csv("5_ContactInformation.csv")           # address information
relationship <- read.csv("6_Relationship_Type.csv")       # relationship type
part <- read.csv("7_ParticipationHistory.csv")            # participation in activities, including dates
customer <- read.csv("15_CustomerSpecific_spur.csv")      # customer information (type of Alumni)
appeals <- read.csv("16_Appeals.csv")                     # appeal codes and names // no need to merge
  appeals <- appeals %>% select(Appeal.Code, Appeal.Name)
contact_res <- read.csv("18_ContactRestrictions.csv")     # restriction information
trans <- read.csv("19_CodeTranslationTable.csv")          # relationship status(married etc.), customer definitions // no need to merge

# put code together 
merge_DS <- merge(giving, entity, by="Entity.ID", all = TRUE)
merge_DS <- merge(merge_DS, athletics, by="Entity.ID", all = TRUE)
merge_DS <- merge(merge_DS, degree, by="Entity.ID", all = TRUE)
merge_DS <- merge(merge_DS, contact, by="Entity.ID", all = TRUE)
merge_DS <- merge(merge_DS, relationship, by="Entity.ID", all = TRUE)
merge_DS <- merge(merge_DS, part, by="Entity.ID", all = TRUE)
merge_DS <- merge(merge_DS, customer, by="Entity.ID", all = TRUE)
merge_DS <- merge(merge_DS, contact_res, by="Entity.ID", all = TRUE)
##########################################################################
#########################  SUMMARIZE GIVING  ##############################
##########################################################################
# summarize by Entity ID
giving.sum <- giving %>% group_by(Entity.ID) %>%
              summarize (n = n(), sum(Credit.Amount), mean(Credit.Amount), 
                         max(Credit.Amount), min(Credit.Amount))
colnames(giving.sum) <- c("Entity.ID","Transactions", "Sum", "Mean", "Max", "Min")
# bin amounts
giving.sum$Bins.sum <- ifelse(giving.sum$Sum < 100, "a < $100", 
                         ifelse(giving.sum$Sum >= 100 & giving.sum$Sum < 1000, "b $100 < Giv < $1K",
                             ifelse(giving.sum$Sum >= 1000 & giving.sum$Sum < 10000, "c $1K < Giv < $10K",
                                 ifelse(giving.sum$Sum >= 10000 & giving.sum$Sum < 100000, "d $10K < Giv < $100K",
                                    ifelse(giving.sum$Sum >= 100000 & giving.sum$Sum < 1000000, "e $100K < Giv < $1MIL",
                                               "f > $1MIL")))))


# summarize by Entity ID and Fiscal Year
giving.sum.fis <- giving %>% group_by(Entity.ID, Fiscal.Year) %>%
  summarize (n = n(), sum(Credit.Amount), mean(Credit.Amount), 
             max(Credit.Amount), min(Credit.Amount))
colnames(giving.sum.fis) <- c("Entity.ID", "Year", "Transactions", "Sum", "Mean", "Max", "Min")

year.giving <- giving %>% group_by(Fiscal.Year) %>%
  summarize (n = n(), sum(Credit.Amount), mean(Credit.Amount), 
             max(Credit.Amount), min(Credit.Amount))
colnames(year.giving) <- c("Year", "Transactions", "Sum", "Mean", "Max", "Min")
year.giving <- year.giving %>%
               filter(Year >= 1986) # 1982-1985 data seems strange so removed them

# time series plot
ggplot() +
  geom_line(aes(x = Year, y = Mean), data = year.giving)


##### Athletics #####
# merge giving and athletics
giv.ath <- merge(giving.sum, athletics, by="Entity.ID", all= TRUE)
giv.ath$Athlete <- ifelse(is.na(giv.ath$Athletics.Activity), "Non-Athlete", "Athlete")
filter.ath <- giv.ath %>% filter(Athlete == "Athlete")

giv.ath.table <- as.data.frame(table(giv.ath$Athlete, giv.ath$Bins.sum))
ath.tot <- sum(filter(giv.ath.table, Var1 == "Athlete")$Freq)
nonath.tot <- sum(filter(giv.ath.table, Var1 == "Non-Athlete")$Freq)
giv.ath.table$con.Freq <- ifelse(giv.ath.table$Var1 =="Athlete", round(giv.ath.table$Freq/ath.tot,4), 
                            round(giv.ath.table$Freq/nonath.tot,4))

# histogram of athletes vs. non athletes
ggplot(aes(x=Var2, y=con.Freq, fill=Var1), data=giv.ath.table)+
  geom_bar(aes(fill=Var1), stat="identity", position="dodge") +
  xlab("$$$") +
  ylab("Conditional Frequency") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5,
        axis.text=element_text(size=8)) +
  guides(fill = guide_legend(title = "Athlete Status", title.position = "top")) +
  geom_text(aes(label= paste(format(100*con.Freq, digits=2, drop0trailing=TRUE),"%",sep =""), y= con.Freq ), 
            position=position_dodge(width=0.9), vjust=-0.25) +
  scale_y_continuous(labels=percent) +
  theme(axis.text.y=element_blank())

#######################
######## Degree #######
#######################

# Note: a lot of NA for entity IDs
giv.deg <- merge(giving.sum, degree, by="Entity.ID", all.x = TRUE)
deg.giv <- merge(giving, degree, by="Entity.ID", all.x = TRUE)

# summarize by Graduation Year
giv.gradyear <- giv.deg %>% group_by(Degree.Year) %>%
                summarize(n = n(), sum(Sum), mean(Sum), 
                          max(Sum), min(Sum))
colnames(giv.gradyear) <- c("Year","N", "Sum", "Mean", "Max", "Min")


# summarize alumni > 10 years and < 10 years
deg.giv$Young <- ifelse(deg.giv$Degree.Year >= 2006, "Alumni < 10 Years", "Alumni > 10 Years")
young <- deg.giv %>%
         group_by(Fiscal.Year, Young) %>%
         summarize(n = n(), sum(Credit.Amount), mean(Credit.Amount), 
                   max(Credit.Amount), min(Credit.Amount))

# issues with alumni < 10 years donating before they graduates :S

colnames(giv.gradyear) <- c("Year","N", "Sum", "Mean", "Max", "Min")
  

# summarize by School of Graduation
giv.school <- giv.deg %>% group_by(School.of.Graduation) %>%
                summarize(n = n(), sum(Sum), mean(Sum), 
                 max(Sum), min(Sum))
colnames(giv.school) <- c("School","N", "Sum", "Mean", "Max", "Min")

ggplot() +
  geom_bar(aes(x=School, y = Mean),stat="identity", data = giv.school)





#######################
#### Donation Maps ####
#######################

giv.loc <- merge(giving.sum, contact, by="Entity.ID", all.x = TRUE)

# summarize by state
giv.state <- giv.loc %>% group_by(State) %>%
              summarize(n = n(), sum(Sum), mean(Sum), 
              max(Sum), min(Sum))
colnames(giv.state) <- c("State","N", "Sum", "Mean", "Max", "Min")
states.outlines <- map_data("state")
head(states.outlines)
states.outlines <- states.outlines %>% mutate(state = state.abb[match(stri_trans_totitle(region),state.name)])

# now we need to merge this states data with the outline data so that it can be used to build a plot layer
states.all <- merge(states.outlines, giv.state, by.x="state", by.y="State")
head(states.all)

# plot by connecting the dots into shapes
ggplot() + 	# plot without a default data set
  geom_path(data=states.outlines, aes(x=long, y=lat, group=group, order=order)) 

#average donations
ggplot() + 
  geom_polygon(data=states.all, aes(x=long, y=lat,
                                    fill=Mean, group=group, order=order)) + 
  geom_path(data=states.all, aes(x=long, y=lat, group=group, order=order), color=I("black"))+
  scale_fill_gradient("Average Donation", low="lightblue", high="darkblue") +
  coord_map() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        legend.position = c(.9, .2),
        plot.title = element_text(size=18)) +
  ggtitle("Map of Average Donations by State")


# sum donations
ggplot() + 
  geom_polygon(data=states.all, aes(x=long, y=lat,
                                    fill=Sum, group=group, order=order)) + 
  geom_path(data=states.all, aes(x=long, y=lat, group=group, order=order), color=I("black"))+
  scale_fill_gradient("Total Donation", low="lightblue", high="darkblue") +
  coord_map() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        legend.position = c(.9, .2),
        plot.title = element_text(size=18)) +
  ggtitle("Map of Total Donations by State")

# number of donators
ggplot() + 
  geom_polygon(data=states.all, aes(x=long, y=lat,
                                    fill=N, group=group, order=order)) + 
  geom_path(data=states.all, aes(x=long, y=lat, group=group, order=order), color=I("black"))+
  scale_fill_gradient2("Donators",low = "white", mid="darkseagreen1", high="forestgreen") +
  coord_map() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        legend.position = c(.9, .2),
        plot.title = element_text(size=18)) +
  ggtitle("Map of Donators by State")

  


