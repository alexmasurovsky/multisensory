# setup -------------------------------------------------------------------

setwd("~/R/Projects/Multisensory_Lab_IA")

library(tidyverse)
library(ggplot2)
library(readxl)
library(cowplot)
library(corrplot)
library(ggcorrplot)
library(psych)
library(Hmisc)
library(FSA)

included_participants <- read_excel("behavioral_data_for_Rx_20200318-3.xls", 
                                    sheet = "all IA post cut participants")[,1:2]
included_participants$id <- substring(included_participants$id, 1, 5) # trim id number to join w/ shorter ID's in other datasets
names(included_participants) <- c("ID","Group")

# read in data
behavioral_data <- read_excel("behavioral_data_for_Rx_20200318-3.xls", na=(c("NA","?"))) %>%
  rename(antipsy_dose=contains("Dose")) %>%
  mutate(ID=substring(ID, 1, 5)) # trim id number to join w/ shorter ID's in other datasets
behavioral_data <- behavioral_data %>% filter(ID %in% included_participants$ID)

# read in EEG data, lose the first 10 rows (except for ID)
eeg_data <- read_excel("panss_corr_20200225a.xls", na = "NA") %>%
  mutate(ID=substring(ID, 1, 5)) %>% select(ID, 10:50) %>% filter(ID %in% included_participants$ID)

# read in source data
source_data <- read_excel("source_corr_20200309.xls", na = "NA") %>%
  mutate(ID=substring(ID, 1, 5)) %>% filter(ID %in% included_participants$ID) #%>% rename(ID=id, Group=group)

### SELECT DV (EEG MEASURE) DATASET
dv_data <- source_data %>% mutate(ID=substring(ID, 1, 5)) %>% filter(ID %in% included_participants$ID)
###

#all_data <- inner_join(behavioral_data, eeg_data, by="ID") 

PANSS_data <- read_excel("panss_corr_20200225a.xls", na = c("NA","?")) %>% mutate(ID=substring(ID, 1, 5)) %>% filter(ID %in% included_participants$ID)


# lm plotting function
ggplotRegression <- function (fit) {
# source: https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = as.name(names(fit$model)[2]), y = as.name(names(fit$model)[1]))) + 
    geom_point() +
    stat_smooth(method = "glm", col = "blue") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

# build dataset by adding each behavioral variable of interest to EEG data

# antipsy_dose (corr) ---------------------------------------------------------------------
data <- behavioral_data %>% filter(Group==1) %>% select(ID, Group, antipsy_dose)# %>% mutate(antipsy_dose=as.double(antipsy_dose))
#temp$antipsy_dose <- as.double(temp$antipsy_dose)
data <- inner_join(data, dv_data, by=c("ID","Group")) 
corr_data <- data %>% select(-ID, -Group) %>% mutate(antipsy_dose=as.numeric(antipsy_dose))
l <- rcorr(as.matrix(corr_data), type = "pearson") # "l" because it creates a list
df <- as.data.frame(l[[1]])[1,] 
df <- rbind(df, as.data.frame(l[[2]])[1,])
df <- rbind(df, as.data.frame(l[[3]])[1,])
row.names(df) <- c(paste0(row.names(df[1,]),"_r"),paste0(row.names(df[1,]),"_n"),paste0(row.names(df[1,]),"_p"))
df <- df %>% select(-1)

r_vals <- df[1,]
p_vals <- df[3,]

# df of p_vals, then correct
# row.names(df) <- c("r","n","p")
p_vals <- as.data.frame(t(p_vals))
colnames(p_vals) <- "p"
#p_vals$dv <- row.names(p_vals)
#row.names(p_vals) <- NULL
#colnames(p_vals) <- c("p","dv")
#p_vals <- select(p_vals, dv, p)

# correct p-values w/ Benjamani-Hochberg method
# (source: https://rcompanion.org/rcompanion/f_01.html)
p_vals <- p_vals %>% mutate(rank = row_number(p)) %>% mutate(q_star = (rank/6)*.05) %>%
  #mutate(adjusted_p = p*(6/rank)) %>%
  arrange(rank)
p_vals$p.adj <- p.adjust(p_vals$p, method="fdr")

# write to file
sink("correlation_p_values.csv")
cat("antipsy_dose\n")
write.csv(p_vals)
cat("\n")
sink()


# for (i in 1:length(p_vals[[1]])) { 
#   #df["p.adj",i] <- p_vals["p.adj", names(df)[i]]} # add adj p-vals
#   if (p_vals$p.adj[i] < .05) {p_vals$sig[i] <- "*"} else if (p_vals$p.adj[i] < .01) {
#     p_vals$sig[i] <- "**"} else if (p_vals$p.adj[i] < .001) {
#       p_vals$sig[i] <- "***"} else
#       {p_vals$sig[i] <- "-"}
# }
# p_vals <- p_vals %>% mutate_at(c(2:5), round, 3)

cat("The following measures yielded FDR-corrected p-value < .05: ")
print(p_vals %>% filter(p.adj<.05) %>% mutate(p.adj = round(p.adj, 4)))

# correlation plot
ggcorrplot(r_vals, title="antipsy_dose", lab=TRUE)#, p.mat=p_vals, pch.cex = 5, pch.col = "grey") # exclude p.mat to leave out x's
# plot the adjusted p-values
ggcorrplot(p_vals %>% select(p.adj), lab=TRUE, title="antipsy_dose", legend.title = "p-val", color=c("white", "orange", "white"))


# store names of DV's with significant effects
sig_vals <- p_vals %>% filter(p.adj<.05)



# plot all
corr_data <- as.data.frame(corr_data)
for (i in 2:length(corr_data)){ # start with first eeg measure
  p<- ggplot(corr_data, aes(corr_data[[1]], corr_data[[i]]))+
    geom_point()+
    theme_minimal()+
    labs(x=names(corr_data[1]), y=names(corr_data[i]),
         title=paste0("r=",(p_vals%>%filter(dv_names==names(corr_data[i])))$r,
                      ", p.adj=",(p_vals%>%filter(dv_names==names(corr_data[i])))$p.adj))+
    geom_smooth(method="lm")
  print(p)
}

# plot significant
for (i in 2:length(corr_data)){ # start with first eeg measure
  if (names(corr_data[i]) %in% row.names(sig_vals)){
    p<- ggplot(corr_data, aes(corr_data[[1]], corr_data[[i]]))+
      geom_point()+
      theme_minimal()+
      labs(x=names(corr_data[1]), y=names(corr_data[i]),
           title=paste0("r=",(p_vals%>%filter(dv_names==names(corr_data[i])))$r,
                        ", p.adj=",(p_vals%>%filter(dv_names==names(corr_data[i])))$p.adj))+
      geom_smooth(method="lm")
    print(p)
  }
}

pairs.panels(corr_data, # select 1 + columns for eeg vars of interest (but not too many)
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#rm(df, temp, l, corr_data, p)



# fagerström (glm) --------------------------------------------------------------
data <- select(behavioral_data, ID, Group, `Fagerstroem Test`)
data <- inner_join(data, dv_data, by=c("ID","Group"))

# run lm (rather than behavioral correlation), as there behavioral data from both groups
# make a list of DV's,
dv_names <- names(select(data, -c(1:3)))

#grp <- sample(c(0,1), length(data[[1]]), replace=TRUE) # imaginary groups for right now
#data$Group <- grp
# then create list of models

m <- lapply(dv_names, function(x) {
  glm(substitute(dv ~ `Fagerstroem Test` + Group + `Fagerstroem Test`:Group, list(dv = as.name(x))), data=data)})

results <- lapply(m, summary) # summarize each lm and put into list "results"
results

p_vals <- data.frame(dv_names) # make a df of p-values for each DV, from dv_names
# add p-value to each dv, from results
for (i in 1:length(p_vals[[1]])) {
  p_vals[i,"p"] <- as.data.frame(results[[i]]$coefficients)[2,4]
}

# correct p-values w/ Benjamani-Hochberg method
# (source: https://rcompanion.org/rcompanion/f_01.html)
p_vals <- p_vals %>% mutate(rank = row_number(p)) %>% mutate(q_star = (rank/6)*.05) %>%
  #mutate(adjusted_p = p*(6/rank)) %>%
  arrange(rank)
p_vals$p.adj <- p.adjust(p_vals$p, method="BH")

sink("correlation_p_values.csv", append = TRUE)
cat("fagerstroem\n")
write.csv(p_vals)
cat("\n")
sink()

# add sig stars
for (i in 1:length(p_vals[[1]])) { 
  #df["p.adj",i] <- p_vals["p.adj", names(df)[i]]} # add adj p-vals
   if (p_vals$p.adj[i] < .05) {p_vals$sig[i] <- "*"} else if (p_vals$p.adj[i] < .01) {
     p_vals$sig[i] <- "**"} else if (p_vals$p.adj[i] < .001) {
       p_vals$sig[i] <- "***"} else
       {p_vals$sig[i] <- "-"}
 }
p_vals

cat("The following measures yielded FDR-corrected p-value < .05: ")
print(p_vals %>% filter(p.adj<.05) %>% select(dv_names, p.adj) %>% mutate(p.adj = round(p.adj, 4)))

# store names of DV's with significant effects
sig_dv_names <- p_vals %>% filter(p.adj<.05) %>% select(dv_names)



# # plot all
# !!! NOT WORKING - COPY FROM BACS IF YOU REALLY NEED THIS !!!
# for (i in 4:length(data)){ # start with first eeg measure
#   #f <- m[[i]]
#   print(ggplotRegression(m[[4]]))
# }
# 
# # plot only significant
# for (i in 4:length(data)){ # start with first eeg measure
#   if (names(m[[i]]$model)[1] %in% sig_dv_names){
#     print(ggplotRegression(m[[i]]))
#   }
# }

# # plots using base functionality - not working
# for (i in 4:length(data)){ # start with first eeg measure
#   plot(data[,3], data[,i], type = "p", pch = 16)
#   abline(m[[i]])
# }


#rm(df, temp, l, corr_data, p)


# BACS (glm) -----------------------------------------------------------------------
data <- select(behavioral_data, ID, Group, contains("BACS Z"), contains("Z-Score"))
data <- inner_join(data, dv_data, by=c("ID","Group"))

# eliminate spaces
for (i in 1:length(names(data))){ 
  names(data)[i] <- gsub(" ", "_", names(data)[i])
  names(data)[i] <- gsub("-", "_", names(data)[i])
}
names(data)

# list of dv's (eeg measures) and iv's (behavioral measures)
dv_names <- names(select(data, -ID, -Group, -contains("BACS")))
iv_names <- names(select(select(data, contains("BACS")), contains("Z")))

# loop through all ivs and dvs, make glm, summarise results
rm(models, results) # clear these variables
models <- list()
results <- list()
for (iv in iv_names){
  for (dv in dv_names){
    models[[iv]][[dv]] <- glm(formula(substitute(y ~ x + Group + x:Group, list(y = as.name(dv), x = as.name(iv)))),
                              data = data)
    results[[iv]][[dv]] <- summary(models[[iv]][[dv]])
  }
}

# make a list of df's of p-values for each DV, from dv_names
p_vals <- NULL
# add p-value to each dv, from results
for (i in iv_names){
  for (d in dv_names) {
    # CHANGE THIS SO THAT IT TAKES ALL P-VALUES (IV, GROUP AND IV:GROUP) AND LABELS APPROPRIATELY
    temp_df <- as.data.frame(results[[i]][[d]]$coefficients)
    temp_p <- data.frame(
                iv = i,
                dv = d,
                measure = row.names(temp_df)[-1], # minus 1 to exclude intercept
                p = temp_df$`Pr(>|t|)`[-1] # minus 1 to exclude intercept
                )
    p_vals <- rbind(p_vals, temp_p)
    #p_vals[[iv]][which(p_vals[[iv]][["dv_names"]]==dv),"p"] <- as.data.frame(results[[iv]][[dv]]$coefficients)[2,4]
  }
}

# adjust p-values w/ Benjamani-Hochberg method
# (source: https://rcompanion.org/rcompanion/f_01.html)
# adjust for all DV's per IV
for (i in iv_names){
  temp_df <- p_vals %>% filter(iv==i)
  temp_df <- temp_df %>% mutate(rank = row_number(p)) %>%
    mutate(q_star = (rank/length(temp_df[[1]]))*.05) %>%
    select(-rank)
  temp_df$p.adj <- p.adjust(temp_df$p, method="BH")
  for (j in 1:length(temp_df[[1]])){
    p_vals[which(p_vals$iv==i & p_vals$dv==temp_df[j,"dv"] & p_vals$measure==temp_df[j,"measure"]),"p.adj"] <- temp_df[j,"p.adj"]
    p_vals[which(p_vals$iv==i & p_vals$dv==temp_df[j,"dv"] & p_vals$measure==temp_df[j,"measure"]),"q_star"] <- temp_df[j,"q_star"]
  }
  #p_vals[which(p_vals$iv==i),"p.adj"] <- temp_df$p.adj
  #p_vals[which(p_vals$iv==i),"q_star"] <- temp_df$q_star
# the above line is somewhat shady with different p.adj values,
# but these all seem to get the same
  #p_vals <- left_join(p_vals, temp_df)
}

# round p-values
p_vals <- p_vals %>% mutate(p=round(p, 5), p.adj=round(p.adj, 5)) %>% arrange(p)

sink("correlation_p_values.csv", append = TRUE)
cat("BACS\n")
write.csv(p_vals)
cat("\n")
sink()
  
# then add significance stars (for potential plotting later)
# for (i in 1:length(p_vals[[1]])){
#   if (p_vals[i,"p.adj"] < .05) {p_vals$sig[i] <- "*"} else if (p_vals[i,"p.adj"] < .01) {
#     p_vals$sig[i] <- "*" <- "**"} else if (p_vals[i,"p.adj"] < .001) {
#       p_vals$sig[i] <- "*" <- "***"} else
#       {p_vals$sig[i] <- "*" <- "-"}
# }
# View(p_vals)

#sig_vals <- NULL #data.frame(iv=NULL, dv=NULL, p.adj=NULL)
if (length((p_vals %>% filter(p.adj < .05))[[1]]) > 0){
  cat("\nThe following measures yielded FDR-corrected p-value < .05: ")
  print(p_vals %>% filter(p.adj < .05))
} else {cat("\nNo significant results after FDR correction\n")}

# plot only significant
# to plot others, fill sig_values with desired variables from p_vals, e.g.:
# sig_vals <- p_vals %>% filter(iv=="BACS...",dv=="multi...")
#sig_vals <- p_vals %>% filter(p < .05) %>% select(iv, dv, p) %>% mutate(adj="none")
sig_vals <- p_vals %>% filter(p.adj < .05) %>% select(iv, dv, p=p.adj) %>% mutate(adj="BH")
sig_vals <- distinct(sig_vals)
#View(sig_vals)
# make df of sig. results
cat("\nPlots of significant results will be plotted here\n")
if (length(sig_vals[[1]]) > 0){
  for (i in 1:length(sig_vals[[1]])){ # FILTER SIG VALUES INTO UNIQUE MEASURES, MEANING IV & DV
    plotdata <- models[[sig_vals[i,"iv"]]][[sig_vals[i,"dv"]]]
    smry <- summary(plotdata)
    p_lab <- sig_vals[i,"p"] #sig_vals[i,"p"] # change here to toggle p/p.adj
    p <- ggplot(plotdata, aes(x = plotdata$model[[2]], y = plotdata$model[[1]])) +
      geom_point() +
      geom_smooth(method = "glm", col = "blue")+
      labs(x = names(plotdata$model[2]), y = names(plotdata$model[1]),
           title = paste(#"Adj R2 = ",signif(smry$ adj.r.squared, 5),
                         "Intercept = ",signif(plotdata$coefficients[[1]],5 ),
                         " Slope =",signif(plotdata$coef[[2]], 5),
                         " P =", signif(p_lab, 5), #(p_vals %>% filter(iv==sig_vals[i,"iv"], dv==sig_vals[i,"dv"], measure==as.character(sig_vals[i,"iv"])))$p.adj, 5))) #smry$coefficients[2,4], 5)))
                         " Adjustment = ", sig_vals[i,"adj"]))
    print(p)
      #print(ggplotRegression(models[[sig_vals[i,"iv"]]][[sig_vals[i,"dv"]]]))
  }
} else {cat("\nNo significant results after FDR correction")}



# PANSS (corr) -------------------------------------------------------------------
data <- PANSS_data %>% select(ID, 5:9) %>% inner_join(dv_data, by=c("ID")) %>% select(-ID, -Group)
iv_names <- names(data[,1:5])
dv_names <- names(data[,-c(1:5)])

# create correlation matrix from data
#l <- rcorr(as.matrix(data %>% select(-ID, -Group)), type = "pearson")
r_vals <- cor(data, method="pearson", use="complete.obs")
r_vals <- r_vals[1:5,-c(1:5)]

p_vals <- cor_pmat(data, method="pearson", use="complete.obs")
p_vals <- p_vals[1:5,-c(1:5)]

# convert p_vals

# # correct p-values w/ Benjamani-Hochberg method
# # (source: https://rcompanion.org/rcompanion/f_01.html)
for (i in 1:nrow(p_vals)){
  temp_vector <- as.vector(p_vals[i,])
  temp_vector <- p.adjust(temp_vector, method="BH")
  p_vals[i,] <- temp_vector
}


# SPQ (glm) -----------------------------------------------------------------------
# temp <- select(behavioral_data, ID, Group, SPQ)
# corr_data <- inner_join(temp, eeg_data)

data <- select(behavioral_data, ID, Group, contains("SPQ"))
data <- inner_join(data, dv_data, by=c("ID","Group"))
View(data)

# eliminate spaces
# for (i in 1:length(names(data))){ 
#   names(data)[i] <- gsub(" ", "_", names(data)[i])
#   names(data)[i] <- gsub("-", "_", names(data)[i])
# }
# names(data)

# list of dv's (eeg measures) and iv's (behavioral measures)
dv_names <- names(select(data, -ID, -Group, -SPQ))
iv_names <- "SPQ"

# loop through all ivs and dvs, make glm, summarise results
models <- list()
results <- list()
for (iv in iv_names){
  for (dv in dv_names){
    models[[iv]][[dv]] <- glm(formula(substitute(y ~ x + Group + x:Group, list(y = as.name(dv), x = as.name(iv)))),
                              data = data)
    results[[iv]][[dv]] <- summary(models[[iv]][[dv]])
  }
}

# make a list of df's of p-values for each DV, from dv_names
p_vals <- NULL
# add p-value to each dv, from results
for (i in iv_names){
  for (d in dv_names) {
    # CHANGE THIS SO THAT IT TAKES ALL P-VALUES (IV, GROUP AND IV:GROUP) AND LABELS APPROPRIATELY
    temp_df <- as.data.frame(results[[i]][[d]]$coefficients)
    temp_p <- data.frame(
      iv = i,
      dv = d,
      measure = row.names(temp_df)[-1], # minus 1 to exclude intercept
      p = temp_df$`Pr(>|t|)`[-1] # minus 1 to exclude intercept
    )
    p_vals <- rbind(p_vals, temp_p)
    #p_vals[[iv]][which(p_vals[[iv]][["dv_names"]]==dv),"p"] <- as.data.frame(results[[iv]][[dv]]$coefficients)[2,4]
  }
}

# adjust p-values w/ Benjamani-Hochberg method
# (source: https://rcompanion.org/rcompanion/f_01.html)
# adjust for all DV's per IV
for (i in iv_names){
  temp_df <- p_vals %>% filter(iv==i)
  temp_df$p.adj <- p.adjust(temp_df$p, method="BH")
  p_vals[which(p_vals$iv==i),"p.adj"] <- temp_df$p.adj
  # the above line is somewhat shady with different p.adj values,
  # but these all seem to get the same
  #p_vals <- left_join(p_vals, temp_df)
}

#sig_vals <- NULL #data.frame(iv=NULL, dv=NULL, p.adj=NULL)
if (length((p_vals %>% filter(p.adj < .05))[[1]]) > 0){
  cat("\nThe following measures yielded FDR-corrected p-value < .05: ")
  print(p_vals %>% filter(p.adj < .05))
} else {cat("\nNo significant results after FDR correction\n")}

# plot only significant
# to plot others, fill sig_values with desired variables from p_vals, e.g.:
# sig_vals <- p_vals %>% filter(iv=="BACS...",dv=="multi...")
#sig_vals <- p_vals %>% filter(p < .05) %>% select(iv, dv, p) %>% mutate(adj="none")
sig_vals <- p_vals %>% filter(p.adj < .05) %>% select(iv, dv, p=p.adj) %>% mutate(adj="BH")
sig_vals <- distinct(sig_vals)
View(sig_vals)
# make df of sig. results
cat("\nPlots of significant results will be plotted here\n")
if (length(sig_vals[[1]]) > 0){
  for (i in 1:length(sig_vals[[1]])){ # FILTER SIG VALUES INTO UNIQUE MEASURES, MEANING IV & DV
    plotdata <- models[[sig_vals[i,"iv"]]][[sig_vals[i,"dv"]]]
    smry <- summary(plotdata)
    p_lab <- sig_vals[i,"p"] #sig_vals[i,"p"] # change here to toggle p/p.adj
    p <- ggplot(plotdata, aes(x = plotdata$model[[2]], y = plotdata$model[[1]])) +
      geom_point() +
      geom_smooth(method = "glm", col = "blue")+
      labs(x = names(plotdata$model[2]), y = names(plotdata$model[1]),
           title = paste(#"Adj R2 = ",signif(smry$ adj.r.squared, 5),
             "Intercept = ",signif(plotdata$coefficients[[1]],5 ),
             " Slope =",signif(plotdata$coef[[2]], 5),
             " P =", signif(p_lab, 5), #(p_vals %>% filter(iv==sig_vals[i,"iv"], dv==sig_vals[i,"dv"], measure==as.character(sig_vals[i,"iv"])))$p.adj, 5))) #smry$coefficients[2,4], 5)))
             " Adjustment = ", sig_vals[i,"adj"]))
    print(p)
    #print(ggplotRegression(models[[sig_vals[i,"iv"]]][[sig_vals[i,"dv"]]]))
  }
} else {cat("\nNothing to plot (perhaps no significant p-values)\n")}


# correlation
# l <- rcorr(as.matrix(data %>% select(-ID, -Group)), type = "pearson")
# df <- as.data.frame(l[[1]])[1,]
# df <- rbind(df, as.data.frame(l[[2]])[1,])
# df <- rbind(df, as.data.frame(l[[3]])[1,])
# row.names(df) <- c(paste0(row.names(df[1,]),"_r"),paste0(row.names(df[2,]),"_n"),paste0(row.names(df[3,]),"_p"))
# SPQ <- df %>% select(-1)
# ggcorrplot(SPQ[1,])




# CQ (corr) -----------------------------------------------------------------------
data <- select(behavioral_data, ID, Group, 27:46) %>% filter(Group==1) # CQ questions
data <- inner_join(data, dv_data)
for (i in 1:length(names(data))){ 
  names(data)[i] <- gsub(" ", "_", names(data)[i])
  names(data)[i] <- gsub("-", "_", names(data)[i])
  names(data)[i] <- gsub("ö", "oe", names(data)[i])
  names(data)[i] <- gsub("ä", "ae", names(data)[i])
  names(data)[i] <- gsub(",", "", names(data)[i])
  names(data)[i] <- gsub("ß", "ss", names(data)[i])
}
#View(data)
iv_names <- names(data[,3:22])
dv_names <- names(dv_data[,-c(1:2)])

# create correlation matrix from data
#l <- rcorr(as.matrix(data %>% select(-ID, -Group)), type = "pearson")
r_vals <- cor(data %>% select(-ID, -Group), method="pearson", use="complete.obs")
r_vals <- r_vals[1:20,-c(1:20)]

p_vals <- cor_pmat(data %>% select(-ID, -Group), method="pearson", use="complete.obs")
p_vals <- p_vals[1:20,-c(1:20)]

# # correct p-values w/ Benjamani-Hochberg method
# # (source: https://rcompanion.org/rcompanion/f_01.html)
for (i in 1:nrow(p_vals)){
  temp_vector <- as.vector(p_vals[i,])
  temp_vector <- p.adjust(temp_vector, method="BH")
  p_vals[i,] <- temp_vector
}

# correlation plot
ggcorrplot(r_vals, lab=TRUE)#, p.mat=p_vals, pch.cex = 5, pch.col = "grey") # exclude p.mat to leave out x's
# plot the adjusted p-values
ggcorrplot(p_vals, lab=TRUE, legend.title = "p-val", color=c("white", "orange", "white"))


cat("The following measures yielded FDR-corrected p-value < .05: ")
sig_vals <- data.frame(iv=NULL, dv=NULL)
for (i in row.names(p_vals)){
  for (j in colnames(p_vals)){
    if (p_vals[i,j] < .05){
      sig_vals <- rbind(sig_vals, data.frame(iv=i, dv=j, p=p_vals[i,j]))
      cat(paste0(i,":",j,", p = ",round(p_vals[i,j], 4),"\n"))
    }
  }
}

# plot (manual entry for now)
  # p<- ggplot(data, aes(CQ_sum1, Vis_230320))+ # put variable names in here
  #   geom_point()+
  #   theme_minimal()+
  #   # #labs(x=names(data[1]), y=names(data[i]),
  #   #      title=paste0("r=",(p_vals%>%filter(dv_names==names(corr_data[i])))$r,
  #   #                   ", p.adj=",(p_vals%>%filter(dv_names==names(corr_data[i])))$p.adj))+
  #   geom_smooth(method="lm")
  # print(p)




