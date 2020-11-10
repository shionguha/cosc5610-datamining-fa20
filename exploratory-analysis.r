library(dplyr)
library(ggplot2)
library(forcats)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(scales)
library(aod)
library(tidyverse)


# Load Data
a <- read.csv("allegations.csv")

#### Simple Metrics ####

# simple numbers
nrow(a)

# simple counts
actionTaken <- count(a, required.corrective.action)
actionTaken

#### Types of allegations ####

# Fado types and counts table of all complaints
complaints_FADO <- count(a, fado.type)
nrow(complaints_FADO)
complaints_FADO_ord <- complaints_FADO[order(-n),]
attach(complaints_FADO_ord)
kbl(head(complaints_FADO_ord, 10), row.names = FALSE, caption = "Table 1. All FADO") %>% kable_classic_2(full_width = F)
detach(complaints_FADO_ord)

# allegation types and counts table of substantiated complaints
substantiated_complaints_FADO <- count(a_substantiated, fado.type)
substantiated_complaints_FADO
attach(substantiated_complaints_FADO)
kbl(head(substantiated_complaints_FADO, 10), row.names = FALSE, caption = "Table 2. Subst. FADO") %>% kable_classic_2(full_width = F)
detach(substantiated_complaints_FADO)


# allegation types and counts table of all complaints
complaints <- count(a, allegation)
head(complaints, 40)
nrow(complaints)
attach(complaints)
complaints_ord <- complaints[order(-n),]
kbl(head(complaints_ord, 10), row.names = FALSE, caption = "Table 3. Top 10 Allegations") %>% kable_classic_2(full_width = F)
detach(complaints)

# allegation types and counts table of substantiated complaints
a_substantiated <- a[a$required.corrective.action == "yes",]
nrow(a_substantiated)
substantiated_complaints <- count(a_substantiated, allegation)
attach(substantiated_complaints)
substantiated_complaints_ord <- substantiated_complaints[order(-n), ]
kbl(head(substantiated_complaints_ord, 10), row.names = FALSE, caption = "Table 4. Top 10 Subst. Allegations") %>% kable_classic_2(full_width = F)
detach(substantiated_complaints)


#### Allegations over time ####
# line chart
yearly_no_split <- count(a, year.rcvd)
yearly_no_split
attach(yearly_no_split)
ggplot(yearly_no_split, aes(x=year.rcvd, y=n)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks=pretty_breaks(n=20)) +
  scale_y_continuous(breaks = pretty(n, n = 8)) +
  labs(caption = "Fig. 1 - Complaints over Time") +
  theme(plot.caption=element_text(hjust=0))
detach(yearly_no_split)


# yearly counts
yearly <- count(a, year.rcvd, required.corrective.action)
ggplot(yearly) + 
  geom_col(aes(x=year.rcvd, y=n, fill = required.corrective.action)) +
  labs(caption = "Fig. 2 Number of Complaints per Year Corrective Action Breakdown")+
  theme(plot.caption=element_text(hjust=0))
scale_fill_manual(values = ecols)


#### Allegations Frequencies ####
# all officer incidents counts
officer_incidents <- count(a, officer.id, required.corrective.action)
colnames(officer_incidents)[colnames(officer_incidents) == "n"] <- "num.complaints"
head(officer_incidents, 10)
attach(officer_incidents)
#ggplot(officer_incidents) + geom_violin(aes(x=n, y=required.corrective.action), outline=FALSE)
officer_incidents_vp <- ggplot(officer_incidents, aes(x=num.complaints, y=required.corrective.action)) + 
  geom_violin(color="black", fill="lightblue") +
  labs(caption = "Fig.3 Average Complaints") +
  #geom_boxplot(color="black", fill="lightblue", alpha=0.8, outlier.fill = NA) + 
  #coord_cartesian(xlim = c(0, 70)) +
  scale_x_continuous(breaks=pretty_breaks(n=30)) 
  #geom_jitter(color="blue", size=0.1, alpha=0.1)
  officer_incidents_vp + geom_boxplot(width=0.03)
detach(officer_incidents)
  
## stats
officer_incidents_all <- count(a, officer.id)
all_summary <- summary(officer_incidents_all)
all_summary <- all_summary[,c(2)]
kbl(all_summary, row.names = FALSE, col.names = "Stats", caption = "Table 5. All Complaints") %>% kable_classic_2(full_width = F)

yes_incidents <- officer_incidents[officer_incidents$required.corrective.action == "yes",]
yes_summary <- summary(yes_incidents)
yes_summary <- yes_summary[,c(3)]
kbl(yes_summary, row.names = FALSE, col.names = "Stats", caption = "Table 6. Corrective Action") %>% kable_classic_2(full_width = F)

no_incidents <- officer_incidents[officer_incidents$required.corrective.action == "no",]
no_summary <- summary(no_incidents)
no_summary <- no_summary[,c(3)]
kbl(no_summary, row.names = FALSE, col.names = "Stats", caption = "Table 7. No Corrective Action") %>% kable_classic_2(full_width = F)





# Required Corrective Action stacked bar chart by officer ethnicity
agg <- count(a,
             officer.ethnicity,
             incident.rank.abbr,
             complainant.ethnicity,
             required.corrective.action)
agg_ord <- mutate(
  agg, 
  officer.ethnicity = reorder(officer.ethnicity, -n, sum),
  complainant.ethnicity = reorder(complainant.ethnicity, -n, sum),
  incident.rank.abbr = reorder(incident.rank.abbr, -n, sum),
  required.corrective.action = reorder(required.corrective.action, -n, sum)
)
ecols <- c(no="green", yes="red")
ggplot(agg_ord) +
  geom_col(aes(x=officer.ethnicity, y = n, fill = required.corrective.action)) +
  scale_fill_manual(values = ecols) +
  labs(caption = "Fig.4 Officer Ethnicity and Complaint Disposition")

# Required Corrective Action stacked bar chart by complainant ethnicity
ggplot(agg_ord) +
  geom_col(aes(x=complainant.ethnicity, y = n, fill = required.corrective.action)) +
  scale_fill_manual(values = ecols) +
  labs(caption = "Fig.5 Complainant Ethnicity and Complaint Disposition")

# Required Corrective Action stacked bar chart by complainant ethnicity
##### DOESN'T WORK #####
subs_agg_ord <- agg_ord[agg_ord$required.corrective.action == "yes",]
ccols <- c(Black="chartreuse4", Hispanic="cadetblue3", White="magenta1", Unknown="khaki1", "Other Race"="sienna1", Asian="dodgerblue3", Refused="seagreen1", "American Indian"="lightsteelblue4", na.value="deeppink1")
ggplot(subs_agg_ord) +
  geom_col(aes(x=officer.ethnicity, y = n, fill = complainant.ethnicity)) +
  scale_fill_continuous(values = ccols) +
  labs(caption = "Fig.6 Officer Ethnicity and Complainant Ethnicity")


#Counts for officer ethnicity vs complaints ethnicity
officer_eth_count <- count(a, officer.ethnicity, complainant.ethnicity, required.corrective.action)
officer_eth_count <- officer_eth_count[officer_eth_count$required.corrective.action == "yes",]
attach(officer_eth_count)
officer_eth_count_ord <- officer_eth_count[order(officer.ethnicity, -n, complainant.ethnicity),]
officer_eth_count_ord <- officer_eth_count_ord[,-c(3)]
kbl(officer_eth_count_ord, row.names = FALSE, caption = "Table 8. Substantiated Officer and Complainant Incidents") %>% kable_classic_2(full_width = F)
detach(officer_eth_count)


#black complainants
officer_and_black_complainants <- officer_eth_count
attach(officer_and_black_complainants)
officer_and_black_complainants <- officer_and_black_complainants[which(complainant.ethnicity == 'Black'),]
officer_and_black_complainants <- officer_and_black_complainants[order(-officer_and_black_complainants$n),]
colnames(officer_and_black_complainants)[colnames(officer_and_black_complainants) == "n"] <- "num.subs.complaints"
officer_and_black_complainants <- select(officer_and_black_complainants, 1, 4)
detach(officer_and_black_complainants)
officer_and_black_complainants

#counts of officers
attach(a)
officers <- select(a, 1,16 )
detach(a)
attach(officers)
officers <- distinct(officers)
officers <- count(officers, officer.ethnicity)
officers <- officers[order(-officers$n),]
detach(officers)
kbl(officers, row.names = FALSE, caption = "Table 9. Count of Officers by Ethnicity") %>% kable_classic_2(full_width = F)


# Calculate percents based on black complainants and officer ethnicity
j <- inner_join(officer_and_black_complainants, officers)
colnames(j)[colnames(j) == "n"] <- "num.officers"
attach(j)
j = mutate(j,
  pct_complaints = num.subs.complaints / sum(num.subs.complaints),
  pct_officers = num.officers /sum(num.officers) 
)
j = mutate(j,
  pct_complaints = scales::percent(pct_complaints,accuracy = 0.001),
  pct_officers = scales::percent(pct_officers,accuracy = 0.001)
)
kbl(j, row.names = FALSE, caption = "Table 10. Black Complainants and Officer Ethnicity") %>% kable_classic_2(full_width = F)
detach(j)

