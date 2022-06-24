# BENCHMARKING ANALYSIS OF AWARD CANDIDATES AGAINST PREVIOUS WINNERS
# November 2021

### Load Libraries ###

library(tidyr)
library(dbplyr)
library(ggplot2)
library(plyr)
library(data.table)
library(magrittr)
library(pals)
library(cowplot)


### Prepare Data ###

# Load data of previous winners (compiled from major spreadsheet)
futures_winners <- read.csv("futures_bench.csv")

# Remove irrelevant columns
futures_winners <- futures_winners[-c(2:3)]

# Rename columns
column_names_winners <- c("Year",
                          "Scholary Output",
                          "Citations per Publication",
                          "Cited Publications (%)",
                          "Field-Weighted Citation Impact",
                          "h-index",
                          "International Collaboration (%)",
                          "Output in Top Citation Percentile (%)",
                          "Publications in Top Journal Percentile (%)",
                          "FoR")

colnames(futures_winners) <- column_names_winners

# Change FoR codes to their full names
futures_winners$FoR <- mapvalues(futures_winners$FoR, 
                                 from=c("1","2","3","4","5","6","7","8","9","10","11"),
                                 to=c("Mathematical Sciences",
                                      "Physical Sciences",
                                      "Chemical Sciences",
                                      "Earth Sciences",
                                      "Environmental Sciences",
                                      "Biological Sciences",
                                      "Agricultural Sciences",
                                      "IT and Computing",
                                      "Engineering",
                                      "Technology",
                                      "Medical and Health"))


# Load data of potential candidates, change column names and FoR values

futures_candidates <- read.csv("futures_candidates.csv")

column_names_candidates <- c("Name",
                             "Scholary Output",
                             "Citations per Publication",
                             "Cited Publications",
                             "Field-Weighted Citation Impact",
                             "h-index",
                             "International Collaboration",
                             "Output in Top Percentile",
                             "Publications in Top Journals",
                             "FoR")
colnames(futures_candidates) <- column_names_candidates

futures_candidates$FoR <- mapvalues(futures_candidates$FoR, 
                                    from=c("1","2","3","4","5","6","7","8","9","10","11"),
                                    to=c("Mathematical Sciences",
                                         "Physical Sciences",
                                         "Chemical Sciences",
                                         "Earth Sciences",
                                         "Environmental Sciences",
                                         "Biological Sciences",
                                         "Agricultural Sciences",
                                         "IT and Computing",
                                         "Engineering",
                                         "Technology",
                                         "Medical and Health"))

# List to name variables in the loop

var_names <- c("Year", "ScholOut",
               "CitePerPub",
               "CitePub",
               "FieldImpact",
               "hIndex",
               "IntCollab",
               "OutputTop",
               "PubTop")


### Generate Plots ###

# Select a candidate (row)

candidate <- futures_candidates[11,]

# Generate bar plots

for(i in 2:9){
  assign(
    paste0(var_names[i], "_med"), 
    aggregate(futures_winners[,i], list(futures_winners$FoR), median)
  )  
  data.table::setnames(get(paste0(var_names[i], "_med")),  c("FoR", "med_metric"))
  
  assign(
    paste0(var_names[i], "_min"), 
    aggregate(futures_winners[,i], list(futures_winners$FoR), min)
  ) 
  data.table::setnames(get(paste0(var_names[i], "_min")),  c("FoR", "min_metric")) 
  
  assign(
    paste0(var_names[i], "_max"), 
    aggregate(futures_winners[,i], list(futures_winners$FoR), max)
  ) 
  data.table::setnames(get(paste0(var_names[i], "_max")),  c("FoR", "max_metric")) 
  
  assign(
    paste0(var_names[i], "_sum"), 
    left_join(get(paste0(var_names[i], "_med")), get(paste0(var_names[i], "_min")), by = "FoR") %>% 
      left_join(get(paste0(var_names[i], "_max")), by = "FoR") %>%
      mutate(min_y = 0)
  )
  
  
  # These plots are the summary stats without the benchmarking line
  assign(
    paste0(var_names[i], "_plot2"), 
    ggplot(get(paste0(var_names[i], "_sum")), aes(x=FoR, y=min_metric, fill = FoR)) + 
      geom_crossbar(aes(ymin = min_y, ymax = med_metric)) +
      labs(y= NULL, x=NULL) + 
      scale_fill_manual(values= as.vector(brewer.paired(11))) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none", text = element_text(size=10)) + 
      #geom_hline(yintercept = yintercept, size = 2, color = "gray1") +
      ggtitle(paste0(column_names_winners[i]))
  )
  
  yintercept = candidate[,i]
  
  assign(
    paste0(var_names[i], "_plot"), 
    ggplot(get(paste0(var_names[i], "_sum")), aes(x=FoR, y=min_metric, fill = FoR)) + 
      geom_crossbar(aes(ymin = min_y, ymax = med_metric)) +
      labs(y= NULL, x=NULL) + 
      scale_fill_manual(values= as.vector(brewer.paired(11))) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none", text = element_text(size=10)) + 
      geom_hline(yintercept = yintercept, size = 2.5, color = "gray3") +
      ggtitle(paste0(column_names_winners[i]))
  )
}


# Generate box plots

FieldImpact_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,5], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[5], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + xlab(NULL)+
  ggtitle(paste0(column_names_winners[5])) + coord_cartesian(ylim=c(1, 6)) + geom_hline(yintercept = candidate[,5], size = 2, color = "gray3")


ScholOut_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,2], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[2], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + xlab(NULL)+
  ggtitle(paste0(column_names_winners[2])) + coord_cartesian(ylim=c(1, 100)) + geom_hline(yintercept = candidate[,2], size = 2, color = "gray3")


hIndex_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,6], fill = FoR)) + xlab(NULL)+
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[6], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + xlab(NULL)+
  ggtitle(paste0(column_names_winners[6])) + coord_cartesian(ylim=c(1, 100)) + geom_hline(yintercept = candidate[,6], size = 2, color = "gray3")


CitePub_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,4], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[4], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + xlab(NULL)+ 
  ggtitle(paste0(column_names_winners[4])) + coord_cartesian(ylim=c(1, 100)) + geom_hline(yintercept = candidate[,4], size = 2, color = "gray3")


CitePerPub_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,3], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[3], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + xlab(NULL)+
  ggtitle(paste0(column_names_winners[3])) + coord_cartesian(ylim=c(1, 100)) + geom_hline(yintercept = candidate[,3], size = 2, color = "gray3")


Intcollab_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,7], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[7], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + xlab(NULL)+ 
  ggtitle(paste0(column_names_winners[7])) + coord_cartesian(ylim=c(1, 100)) + geom_hline(yintercept = candidate[,7], size = 2, color = "gray3")


PubTop_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,9], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[9], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + xlab(NULL)+
  ggtitle(paste0(column_names_winners[9])) + coord_cartesian(ylim=c(1, 100)) + geom_hline(yintercept = candidate[,9], size = 2, color = "gray3")

Output_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,8], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[8], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + xlab(NULL)+
  ggtitle(paste0(column_names_winners[8])) + coord_cartesian(ylim=c(1, 100)) + geom_hline(yintercept = candidate[,8], size = 2, color = "gray3")

# Save all plots in a grid
# Change location as necessary

filebar = paste0("U:/Documents/Data/Benchmarking/FF_Plots2/", candidate[,1], " Bar.png")
filebox = paste0("U:/Documents/Data/Benchmarking/FF_Plots2/", candidate[,1], " Box.png")

png(filebar,width=1500, height=800)
plot_grid(FieldImpact_plot, 
          ScholOut_plot, 
          hIndex_plot, 
          IntCollab_plot, 
          CitePerPub_plot, 
          CitePub_plot, 
          OutputTop_plot, 
          PubTop_plot,
          ncol=4)
dev.off()

png(filebox,width=1500, height=800) 
plot_grid(FieldImpact_boxplot, 
          ScholOut_boxplot, 
          hIndex_boxplot, 
          Output_boxplot,
          Intcollab_boxplot, 
          CitePerPub_boxplot, 
          CitePub_boxplot, 
          PubTop_boxplot,
          ncol=4)
dev.off() 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# These plots are the summary stats without the benchmarking line

gridplots <- plot_grid(FieldImpact_plot2, 
                       ScholOut_plot2, 
                       hIndex_plot2, 
                       IntCollab_plot2, 
                       CitePerPub_plot2, 
                       CitePub_plot2, 
                       OutputTop_plot2, 
                       PubTop_plot2,
                       ncol=4)


FieldImpact_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,5], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[5], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + 
  ggtitle(paste0(column_names_winners[5])) + coord_cartesian(ylim=c(1, 6)) 


ScholOut_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,2], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[2], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + 
  ggtitle(paste0(column_names_winners[2])) + coord_cartesian(ylim=c(1, 100)) 

hIndex_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,6], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[6], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + 
  ggtitle(paste0(column_names_winners[6])) + coord_cartesian(ylim=c(1, 100))

CitePub_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,4], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[4], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + 
  ggtitle(paste0(column_names_winners[4])) + coord_cartesian(ylim=c(1, 100))

CitePerPub_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,3], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[3], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + 
  ggtitle(paste0(column_names_winners[3])) + coord_cartesian(ylim=c(1, 100))

Intcollab_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,7], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[7], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + 
  ggtitle(paste0(column_names_winners[7])) + coord_cartesian(ylim=c(1, 100))

PubTop_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,9], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[9], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + 
  ggtitle(paste0(column_names_winners[9])) + coord_cartesian(ylim=c(1, 100))

Output_boxplot <- futures_winners %>% ggplot(aes(x=FoR, y=futures_winners[,8], fill = FoR)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y= column_names_winners[8], x="Field of Research") + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1), legend.position = "none") + 
  ggtitle(paste0(column_names_winners[8])) + coord_cartesian(ylim=c(1, 100)) 

boxplots <- plot_grid(FieldImpact_boxplot, 
                      ScholOut_boxplot, 
                      hIndex_boxplot, 
                      Output_boxplot,
                      Intcollab_boxplot, 
                      CitePerPub_boxplot, 
                      CitePub_boxplot, 
                      PubTop_boxplot,
                      ncol=4)

# For a plot of the number of applications in each field
futures_winners %>% 
  ggplot(aes(x=FoR, fill= FoR)) + 
  geom_bar() + 
  scale_fill_manual(values= as.vector(brewer.paired(11))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust=1, hjust=1),
        legend.position = "none", 
        text = element_text(size=10)) + 
  ylab("Number of Successful Applications") + 
  xlab("Field of Research")