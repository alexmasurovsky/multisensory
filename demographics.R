# setup -------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(readxl)
library(cowplot)
library(corrplot)
library(ggcorrplot)
library(psych)
library(Hmisc)


# choose your working directory
setwd("~/R/Projects/Multisensory_Lab_IA")

# set behavioral data filename here
#behavioral_file <- "behavioral_data_for_Rx_20200318.xls"

# read in list of included participants
# sheet w/ all included participants in one column
included_participants <- read_excel("behavioral_data_for_Rx_20200318-3.xls", 
                                    sheet = "all IA post cut participants")[,1:2]
  included_participants$id <- substring(included_participants$id, 1, 5) # trim id number to join w/ shorter ID's in other datasets
  names(included_participants) <- c("ID","Group")
  # number of included participants
  n_post_cut <- included_participants %>% group_by(Group) %>% summarise(n())

excluded <- data.frame(ID=c("HJ44274","LT39251","TT44464")) %>% mutate(ID=substring(ID, 1, 5))

# read in behavioral data (change filename here)
behavioral_data <- read_excel("behavioral_data_for_Rx_20200318-3.xls", na = c("NA","?")) %>% 
  rename(antipsy_dose=contains("Antipsychotic")) %>%
  mutate(ID=substring(ID, 1, 5), antipsy_dose=as.numeric(antipsy_dose)) 
  # n subjects, pre-cut
  #n_pre_cut <- behavioral_data %>% group_by(Group) %>% summarise(n())
  behavioral_data_precut <- behavioral_data
  #cut_subjects <- behavioral_data[-which(behavioral_data$ID %in% included_participants$ID),]
  #cut_subjects <- cut_subjects %>% slice(-3, -10) # remove empties
  #cut_subjects <- cut_subjects %>% slice(-5, -6, -9)
  # cut participants that were not included
  behavioral_data <- behavioral_data %>% filter(ID %in% included_participants$ID)

PANSS_data <- read_excel("panss_corr_20200225a.xls", na = c("NA","?")) %>%
  select(1:9) %>% mutate(ID=substring(ID, 1, 5)) %>% filter(ID %in% included_participants$ID)

age_gender_data <- 
  read_excel("IA_age_gender2.xls") %>%
  select(ID, Group=contains("Group"), Sex=contains("Sex"), Age=Alter) %>%
  mutate(ID=substring(ID, 1, 5))
  #filter(ID %in% included_participants$ID)
  # read_excel("Preprocessing_20190131_master.xls", sheet="Behavior") %>% 
  #   select(ID, contains("Group"), Sex, Alter) %>%
  #   mutate(ID=substring(ID, 1, 5)) %>% filter(ID %in% included_participants$ID)
  # merged <- included_participants %>% left_join(age_gender_data, by=c("ID","Group"))
  # missing <- merged[which(is.na(merged$Sex)),"ID"]
  # print(missing)
  age_gender_precut <- age_gender_data
  # filter out excluded subjects
  age_gender_data <- age_gender_data %>% filter(ID %in% included_participants$ID)

trial_data <- read_excel("IA_trials_components_channels.xls") %>% 
  rename(ID=1, ) %>%
  mutate(ID=substring(ID, 1, 5)) %>%
  filter(ID %in% included_participants$ID) %>%
  inner_join(age_gender_data %>% select(ID, Group), by="ID") %>%
  mutate(num_cut=count.fields(textConnection(`Components cut`), sep = ","))



# trials ------------------------------------------------------------------

# t-test trials remaining
hist(trial_data[which(trial_data$Group==0),"Trials remaining"], main="HC trials remaining")
hist(trial_data[which(trial_data$Group==1),"Trials remaining"], main="SCZ trials remaining")
ttest_trials <- t.test(`Trials remaining` ~ Group, data=trial_data)

trial_data_output <- data.frame(
  scz_mean = mean(trial_data[which(trial_data$Group==1),"Trials remaining"][[1]]),
  scz_sd = sd(trial_data[which(trial_data$Group==1),"Trials remaining"][[1]]),
  HC_mean = mean(trial_data[which(trial_data$Group==0),"Trials remaining"][[1]]),
  HC_sd = sd(trial_data[which(trial_data$Group==0),"Trials remaining"][[1]]),
  t = ttest_trials$statistic[[1]],
  p = ttest_trials$p.value,
  row.names="Num trials remaining"
)

# t-test channels remaining
hist(trial_data[which(trial_data$Group==0),"Channels remaining"], main="HC channels remaining")
hist(trial_data[which(trial_data$Group==1),"Channels remaining"], main="SCZ channels remaining")
ttest_channels <- t.test(`Channels remaining` ~ Group, data=trial_data)
trial_data_output <- rbind(trial_data_output, data.frame(
  scz_mean = mean(trial_data[which(trial_data$Group==1),"Channels remaining"][[1]]),
  scz_sd = sd(trial_data[which(trial_data$Group==1),"Channels remaining"][[1]]),
  HC_mean = mean(trial_data[which(trial_data$Group==0),"Channels remaining"][[1]]),
  HC_sd = sd(trial_data[which(trial_data$Group==0),"Channels remaining"][[1]]),
  t = ttest_channels$statistic[[1]],
  p = ttest_channels$p.value,
  row.names="Num channels remaining"
))

# t-test number of channels cut
hist(trial_data[which(trial_data$Group==0),"num_cut"], main="HC num trial cut")
hist(trial_data[which(trial_data$Group==1),"num_cut"], main="SCZ num trial cut")
ttest_chans_cut <- t.test(num_cut ~ Group, data=trial_data)
trial_data_output <- rbind(trial_data_output, data.frame(
  scz_mean = mean(trial_data[which(trial_data$Group==1),"num_cut"][[1]]),
  scz_sd = sd(trial_data[which(trial_data$Group==1),"num_cut"][[1]]),
  HC_mean = mean(trial_data[which(trial_data$Group==0),"num_cut"][[1]]),
  HC_sd = sd(trial_data[which(trial_data$Group==0),"num_cut"][[1]]),
  t = ttest_chans_cut$statistic[[1]],
  p = ttest_chans_cut$p.value,
  row.names="Num channels cut"
))


sink("summary_stats_output.csv")
write.csv(trial_data_output)
cat("\n")
sink()

# age ---------------------------------------------------------------------
age_stats_precut <- age_gender_precut%>% group_by(Group) %>% 
  summarise(n=n(),age_mean=mean(Age, na.rm = TRUE), age_SD=sd(Age, na.rm=TRUE)) %>% ungroup()

age_stats_postcut <- age_gender_data %>% group_by(Group) %>% 
  summarise(n=n(),age_mean=mean(Age, na.rm = TRUE), age_SD=sd(Age, na.rm=TRUE)) %>% ungroup()

age_stats_precut
age_stats_postcut

# summary_stats <- cbind(age_stats_postcut,
#                        data.frame(female = gender_postcut[which(gender_postcut$Sex=="female"),"n"][[1]],
#                                   male = gender_postcut[which(gender_postcut$Sex=="male"),"n"][[1]]))

hist(age_gender_data[which(age_gender_data$Group==0),"Age"][[1]], main="HC Age")
hist(age_gender_data[which(age_gender_data$Group==1),"Age"][[1]], main="SCZ Age")
age_ttest <- t.test(Age ~ Group, data=age_gender_data)

age_stats_output <- data.frame(
  scz_mean = age_stats_postcut[which(age_stats_postcut$Group==1),"age_mean"][[1]],
  scz_sd = age_stats_postcut[which(age_stats_postcut$Group==1),"age_SD"][[1]],
  HC_mean = age_stats_postcut[which(age_stats_postcut$Group==0),"age_mean"][[1]],
  HC_sd = age_stats_postcut[which(age_stats_postcut$Group==0),"age_SD"][[1]],
  t = age_ttest$statistic[[1]],
  p = age_ttest$p.value,
  row.names="Age"
)

#write to file
sink("summary_stats_output.csv", append = TRUE)
write.csv(age_stats_postcut)
cat("\n")
write.csv(age_stats_output)
cat("\n")
sink()


# Cigarettes --------------------------------------------------------------
# df <- behavioral_data %>% group_by(Group) %>% summarise(mean=mean(`Fagerstroem Test`, na.rm=TRUE), sd=sd(`Fagerstroem Test`, na.rm=TRUE))

#normally distributed data should look like this:
qqnorm(qnorm(seq(0.01,0.99,0.01)))

# healthy control
hc_group <- behavioral_data %>%
  select(ID, Group, contains("fager")) %>% filter(Group==0, complete.cases(.))
hc_n <- length(hc_group[[1]])
hist(hc_group[,3][[1]], main="fager - hc")
cat("Shapiro p-value; H0 is that data are normally distributed: ")
round(shapiro.test(hc_group[,3][[1]])$p.value, 4)

# schizophrenia
scz_group <- behavioral_data %>%
  select(ID, Group, contains("fager")) %>% filter(Group==1, complete.cases(.))
scz_n <- length(scz_group[[1]])
hist(scz_group[,3][[1]], main="fager - scz")
qqnorm(scz_group[,3][[1]])
cat("Shapiro p-value; H0 is that data are normally distributed: ")
round(shapiro.test(scz_group[,3][[1]])$p.value, 4)

#qqplot(hc_group[,3][[1]], scz_group[,3][[1]], main="Q-Q plot - fager")

df <- data.frame(row.names = "Cigarettes",
  scz_n = scz_n,
  hc_n = hc_n,
  scz_mean = mean(scz_group[,3][[1]], na.rm=TRUE),
  scz_sd = sd(scz_group[,3][[1]], na.rm=TRUE),
  scz_med = median(scz_group[,3][[1]], na.rm=TRUE),
  hc_mean = mean(hc_group[,3][[1]], na.rm=TRUE),
  hc_sd = sd(hc_group[,3][[1]], na.rm=TRUE),
  hc_med = median(hc_group[,3][[1]], na.rm=TRUE),
  # t_stat = t.test(scz_group[,3][[1]], hc_group[,3][[1]])$statistic[[1]],
  # p_stat = t.test(scz_group[,3][[1]], hc_group[,3][[1]])$p.value,
  # t_test_note = "Assuming unequal variances between groups",
  w_stat = wilcox.test(scz_group[,3][[1]], hc_group[,3][[1]])$statistic,
  w_p_val = wilcox.test(scz_group[,3][[1]], hc_group[,3][[1]])$p.value
  # wilc_test_note = wilcox.test(scz_group[,3][[1]], hc_group[,3][[1]])$method
)

fager_data_output <- df

sink("summary_stats_output.csv", append=TRUE)
write.csv(fager_data_output)
cat("\n")
sink()

#summary_stats <- rbind(summary_stats, df)

# Antipsychotic dose --------------------------------------------------------------
hc_group <- behavioral_data %>%
  select(ID, Group, antipsy_dose) %>% filter(Group==0, complete.cases(.))

hc_n <- length(hc_group[[1]])

scz_group <- behavioral_data %>%
  select(ID, Group, antipsy_dose) %>% filter(Group==1, complete.cases(.))

scz_n <- length(scz_group[[1]])

df <- data.frame(row.names = "Antipsychotic_Dose",
                 scz_n = scz_n,
                 hc_n = hc_n,
                 scz_mean = mean(scz_group[,3][[1]], na.rm=TRUE),
                 scz_sd = sd(scz_group[,3][[1]], na.rm=TRUE),
                 hc_mean = mean(hc_group[,3][[1]], na.rm=TRUE),
                 hc_sd = sd(hc_group[,3][[1]], na.rm=TRUE))

antipsy_dose_output <- df

sink("summary_stats_output.csv", append=TRUE)
write.csv(antipsy_dose_output)
cat("\n")
sink()

#summary_stats <- rbind(summary_stats, df)

# gender  ----------------------------------------------------------

gender_precut <- age_gender_precut %>% mutate(Sex=factor(Sex, labels = c("female","male")), Group=factor(Group)) %>%
  group_by(Group) %>% count(Sex) %>% ungroup()

gender_postcut <- age_gender_data %>% mutate(Sex=factor(Sex, labels = c("female","male")), Group=factor(Group)) %>%
  group_by(Group) %>% count(Sex) %>% ungroup()

gender_precut
gender_postcut

ggplot(gender_postcut, aes(factor(Sex), n, fill=Group))+geom_bar(stat="identity", position="dodge")

# make a table of counts (group on rows, gender on columns)
female <- gender_postcut%>%filter(Sex=="female")%>%select(n)
male <- gender_postcut%>%filter(Sex=="male")%>%select(n)
chi_data <- data.frame(female=female[[1]], male=male[[1]], row.names=c("HC","SCZ"))
#rm(female, male)
chi_test <- chisq.test(chi_data)
t(chi_data)


sink("summary_stats_output.csv", append=TRUE)
write.csv(t(chi_data))
cat("\n")
write.csv(chi_test)
cat("\n")
sink()

# Medication counts -------------------------------------------------------
# # count number of medication uses
# #  list of medications to count (can replace w/ excel input if desired)
#   medication_counts <- data.frame(medications = 
#                                   c("Haloperidol",
#                                   "Amisulpride",
#                                   "Clozapine",
#                                   "Quetiapine",
#                                   "Olanzapine",
#                                   "Aripiprazole",
#                                   "Risperidone",
#                                   "Paliperidone",
#                                   "Martazapine",
#                                   "Escitalopram",
#                                   "Paroxetine"),
#                                   Count = 0,
#                                   stringsAsFactors = FALSE)
#   row.names(medication_counts) <- medication_counts[,1]
#   medication_counts <- select(medication_counts, Count)
#   # for(i in 1:length(medication_counts[,1])){
#   #   medication_counts[i,2] <- length(grep(substr(medication_counts[i,1], 1, 4), 
#   #                                         behavioral_data$Medikamente, ignore.case = TRUE))
#   # }
medication_counts <- data.frame()
# Haloperidol
count <- 0
count <- count + length(grep("halo", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("haldol", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("serenace", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Haloperidol","Count"] <- count
# Amisulpride
count <- 0
count <- count + length(grep("amis", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Amisulpride","Count"] <- count
# Clozapine
count<-0
count <- count + length(grep("cloz", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Clozapine","Count"] <- count
# Quetiapine
count<-0
count <- count + length(grep("quet", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("queit", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("seroq", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("serq", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Quetiapine","Count"] <- count
# Olanzapine
count<-0
count <- count + length(grep("olanz", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("zyprexa", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("zypra", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Olanzapine","Count"] <- count
# Aripiprazole
count<-0
count <- count + length(grep("olanz", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("abilify", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Aripiprazole","Count"] <- count
# Risperidone
count<-0
count <- count + length(grep("risp", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("resp", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Risperidone","Count"] <- count
# Ziprasidone
count<-0
count <- count + length(grep("zipr", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("zypr", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("geodon", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Ziprasidone","Count"] <- count
# Paliperidone
count<-0
count <- count + length(grep("pali", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("invega", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("xeplion", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Paliperidone","Count"] <- count
# ANTIDEPRESSANTS
# Martazapine
count<-0
count <- count + length(grep("marta", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("mirta", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("remeron", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Martazapine","Count"] <- count
# Escitalopram
count<-0
count <- count + length(grep("escit", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("lexap", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Escitalopram","Count"] <- count
# Paroxetine
count<-0
count <- count + length(grep("parox", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("paxil", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("seroxat", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Paroxetine","Count"] <- count
# Venlafaxine
count<-0
count <- count + length(grep("venla", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("paxil", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("seroxat", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Venlafaxine","Count"] <- count
# OTHER PSYCHIATRIC
count<-0
count <- count + length(grep("loraz", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("loras", behavioral_data$Medikamente, ignore.case = TRUE))
count <- count + length(grep("ativan", behavioral_data$Medikamente, ignore.case = TRUE))
medication_counts["Lorazepam","Count"] <- count

sink("summary_stats_output.csv", append=TRUE)
write.csv(medication_counts)
cat("\n")
sink()

# BACS --------------------------------------------------------------------

# run analysis on this data
dataset <- behavioral_data %>% select(ID, Group, contains("BACS Z"), contains("Z-Score"))

bacs_data <- data.frame() # load this up with data

for (i in 3:length(dataset)){ #set row numbers for the BACS
  var_name <- names(dataset[,i])
  print(var_name)
  
  hc_group <- dataset %>% select(ID, Group, i) %>% filter(Group==0, complete.cases(.))
  hc_n <- length(hc_group[[1]])
  hist(hc_group[,3][[1]], main=paste(var_name,"\nshapiro p=",round(shapiro.test(hc_group[,3][[1]])$p.value, 4)))
  cat("HC group - Shapiro p-value (H0 is that data are normally distributed): ")
  print(round(shapiro.test(hc_group[,3][[1]])$p.value, 4))
  
  scz_group <- dataset %>% select(ID, Group, i) %>% filter(Group==1, complete.cases(.))
  scz_n <- length(scz_group[[1]])
  hist(scz_group[,3][[1]], main=paste(var_name,"\nshapiro p=",round(shapiro.test(scz_group[,3][[1]])$p.value, 4)))
  cat("scz group - Shapiro p-value (H0 is that data are normally distributed): ")
  print(round(shapiro.test(scz_group[,3][[1]])$p.value, 4))
  
  # f-test for equal variances
  f <- var.test(scz_group[,3][[1]], hc_group[,3][[1]])
  cat("f-test p value = ", round(f$p.value, 4), "\n\n")
  
  qqplot(hc_group[,3][[1]], scz_group[,3][[1]],
         main=paste("Q-Q plot:",var_name,"\nf-test test p=",round(f$p.value, 4)))
  
  df <- data.frame(row.names = var_name,
                   scz_n = scz_n,
                   hc_n = hc_n,
                   scz_mean = mean(scz_group[,3][[1]], na.rm=TRUE),
                   scz_sd = sd(scz_group[,3][[1]], na.rm=TRUE),
                   hc_mean = mean(hc_group[,3][[1]], na.rm=TRUE),
                   hc_sd = sd(hc_group[,3][[1]], na.rm=TRUE),
                   scz_sw_norm = round(shapiro.test(scz_group[,3][[1]])$p.value, 4),
                   hc_sw_norm = round(shapiro.test(hc_group[,3][[1]])$p.value, 4),
                   equal_var_f_test = round(f$p.value, 4),
                   t_stat = t.test(scz_group[,3][[1]], hc_group[,3][[1]])$statistic[[1]],
                   p_stat = t.test(scz_group[,3][[1]], hc_group[,3][[1]])$p.value,
                   #t_test_note = "Assuming unequal variances between groups",
                   w_stat = wilcox.test(scz_group[,3][[1]], hc_group[,3][[1]])$statistic,
                   w_p_val = wilcox.test(scz_group[,3][[1]], hc_group[,3][[1]])$p.value
                   #wilc_test_note = wilcox.test(scz_group[,3][[1]], hc_group[,3][[1]])$method
  )
  #summary_stats <- rbind(summary_stats, df)
  bacs_data <- rbind(bacs_data, df)
}

sink("summary_stats_output.csv", append=TRUE)
write.csv(bacs_data)
cat("\n")
sink()

# PANSS -------------------------------------------------------------------

pans_data_ouput <- data.frame()

for (i in 5:9){
  var_name <- names(PANSS_data[,i])
  
  # hc_group <- PANSS_data %>%
  #   select(ID, i)
  # 
  # scz_group <- behavioral_data %>%
  #   select(ID, i)
  
  df <- data.frame(row.names = var_name,
                   scz_n = length(which(!is.na(PANSS_data[[1]]))),
                   scz_mean = mean(PANSS_data[,i][[1]], na.rm=TRUE),
                   scz_sd = sd(PANSS_data[,i][[1]], na.rm=TRUE),
                   hc_mean = "-",
                   hc_sd = "-",
                   t_stat = "-",
                   p_stat = "-",
                   note = "-")
  pans_data_ouput <- rbind(pans_data_ouput, df)
}


sink("summary_stats_output.csv", append=TRUE)
cat("\nPANSS")
write.csv(pans_data_ouput)
cat("\n")
sink()

# write to file -----------------------------------------------------------
# sink("summary_stats_output.csv")
#   write.csv(summary_stats)
#   cat("\n")
#   write.csv(medication_counts)
# sink()
