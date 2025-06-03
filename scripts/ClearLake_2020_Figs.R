
# Clear Lake Manuscript

# Author: Jennifer L. Beatty & Brittany P. Stewart
# Last edited: 06/03/2025

# Load packages
library(plyr)
library(tidyverse)
library(reshape2)
library(patchwork)

# Load datasets
key <- read.csv("~/github/ClearLakeOverview/raw-data/clearlake_key.csv", header=T)
key$site=factor(key$site, levels=c("S1", "S2","S3","S4","S5","S6","S7","S8","S9","S10"))
key$arm=factor(key$arm, levels=c("Upper Arm", "Oaks Arm", "Lower Arm"))

# Set parameters for figures
arms_cols<-c("Oaks Arm"="#DDAD4B", "Upper Arm"="#4575b4","Lower Arm"="firebrick4")
hex <- c("#FF0000", "#FFA500", "#FFFF00", "#008000", "#9999ff", "#000066")

##### Figure 3: Ecomapper ####
total_lake = read.csv("~/github/ClearLakeOverview/raw-data/whole_lake_trimmed.csv", header=T)
total_lake$Arm= factor(total_lake$Arm, levels=c("Upper Arm", "Oaks Arm", "Lower Arm"))

total_lake_m <- melt(select(total_lake, c("Longitude","DFS.Depth.m.","Temp.C","Chl.ug.L","ODO.mg.L","Arm")),id.vars = c("Longitude","DFS.Depth.m.","Arm"))
head(total_lake_m)

# split
dfs = split(total_lake_m, f = total_lake_m$variable)
# apply ggplot function and write to list
gg_l = lapply(dfs, function(x) {
  ggplot(x, aes(x = Longitude,y = DFS.Depth.m., colour = value)) + 
    geom_line(linewidth=.8, alpha=0.8)+
    #geom_point(size=4)+
    ylim(16,0)+
    scale_x_continuous(breaks = seq(-122.9,-122.65,0.0250)) +
    scale_color_gradientn(colors=rev(hex))+
    theme_classic()+
    theme(panel.background = element_rect(fill = "grey90"))+
    ylab("Depth (m)")+
    facet_grid(.~Arm, scales="free_x", space="free_x",)+ theme(strip.background = element_blank())
})
# patchwork
wrap_plots(gg_l, ncol = 1)
#ggplot2::ggsave("~/github/ClearLakeOverview/results/Fig3.pdf", width=230, height = 190, units="mm",  dpi=300)


##### Figure 4: Temperature from RBR ####
rbr_df <- read.csv("~/github/ClearLakeOverview/raw-data/Compiled_Clearlake_trimmed_2020_datetime.csv", header=T)
rbr_df$Date = format(ymd(rbr_df$Date), "%m/%d/%Y")
head(rbr_df)
rbr_plot=join(rbr_df,key,type="left",by="site")
rbr_plot$site=factor(rbr_plot$site, levels=c("S1", "S2","S3","S4","S5","S6","S7","S8","S9","S10"))

Fig4 <-
  ggplot(data=rbr_plot, aes(x=pressure, y=temperature,group=site,color=arm))+
  geom_line(na.rm=TRUE,linewidth=1)+
  scale_color_manual(values=arms_cols)+
  coord_flip()+
  scale_x_reverse()+ 
  labs(x="Depth (m)", y="Temperature (C)")+
  theme_bw()+
  theme(panel.grid.minor = element_line(colour="white"),
        legend.position="bottom",
        legend.title=element_blank()) +
  facet_grid(site~Date) + theme(strip.background = element_blank(), strip.text=element_text(face= "bold"))
Fig4
# ggplot2::ggsave("~/github/ClearLakeOverview/results/Fig4.pdf", width=230, height = 190, units="mm",  dpi=300)

SFig3 <-
  ggplot(data=rbr_plot, aes(x=pressure, y=temperature,group=site,color=arm))+
  geom_line(na.rm=TRUE,linewidth=0.5)+
  scale_color_manual(values=arms_cols, name="Arm")+
  coord_flip()+
  scale_x_reverse()+ 
  labs(x="Depth (m)", y="Temperature (C)")+
  theme_bw()+
  theme(panel.grid.minor = element_line(colour="white"))+
  theme(legend.position="bottom",
        legend.title=element_blank()) +
  facet_wrap(~Date, ncol=8) + theme(strip.background = element_blank(), strip.text=element_text(face= "bold"))
SFig3
#ggplot2::ggsave("~/github/ClearLakeOverview/resultsSupFig3.pdf", width=190, height = 140, units="mm",  dpi=300)

##### Figure 5: Dissolved O2 from RBR ####
Fig5<-ggplot(data=rbr_plot, aes(x=pressure, y=dissolvedosaturation,group=site,color=arm))+
  geom_line(na.rm=TRUE,linewidth=1)+
  scale_color_manual(values=arms_cols)+
  coord_flip()+
  scale_x_reverse()+ 
  labs(x="Depth (m)", y="Dissolved Oxygen (% Sat)")+
  theme_bw()+
  theme(panel.grid.minor = element_line(colour="white"),
        legend.position="bottom",
        legend.title=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = .5)) +
  facet_grid(site~Date) + theme(strip.background = element_blank(), strip.text=element_text(face= "bold"))
Fig5
#ggplot2::ggsave("~/github/ClearLakeOverview/results/Fig5.pdf", width=230, height = 190, units="mm",  dpi=300)

SFig4 <-
  ggplot(data=rbr_plot, aes(x=pressure, y=dissolvedosaturation,group=site,color=arm))+
  geom_line(na.rm=TRUE, linewidth=0.5)+
  scale_color_manual(values=arms_cols, name="Arm")+
  coord_flip()+
  scale_x_reverse()+ 
  labs(x="Depth (m)", y="Dissolved Oxygen (% Sat)")+
  theme_bw()+
  theme(panel.grid.minor = element_line(colour="white"))+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = .5)) +
  facet_wrap(~Date, ncol=8) + theme(strip.background = element_blank(), strip.text=element_text(face= "bold"))
SFig4
#ggplot2::ggsave("~/github/ClearLakeOverview/results/SupFig4.pdf", width=190, height = 140, units="mm",  dpi=300)

##### Figure 6: Chlorophyll & Microcystins Discrete####

micro_df <- read.csv("~/github/ClearLakeOverview/raw-data/Clearlake2020_Microcystins_RawData.csv", header=T) # read in the CSV, specify that it has a header
micro_df$date=mdy(micro_df$date) # tells r to read the dates as date objects
micro_df$arm=factor(micro_df$arm, levels=c("Upper Arm", "Oaks Arm", "Lower Arm"))
head(micro_df) # look at the data frame
micro.m <-melt(select(micro_df, c("date","site","total","chla","arm")),id.vars=c("date","site","arm")) %>%
  group_by(date, arm, variable)%>%
  summarise(mean=mean(value), se=sd(value)/sqrt(length(value)), sd= sd(value))
micro.m$variable = factor(micro.m$variable, levels=c("chla", "total"))
head(micro.m)

Fig6 <-
  ggplot(micro.m, aes(x=date, y=mean,ymin = mean-sd, ymax = mean+sd, group=arm,color=arm))+
  geom_line() +
  geom_point() +
  geom_errorbar(width=0.5)+
  scale_color_manual(values=arms_cols, name="Arm")+
  labs(x="Date")+
  #scale_x_date(date_breaks="3 day")+
  theme_bw()+
  theme(panel.grid.minor = element_line(colour="white"))+
  theme(legend.title=element_blank(),
        legend.position="bottom",
        axis.title.y = element_text(angle = 90, hjust = .5, vjust = .5),
        axis.title.x = element_blank(),
        text = element_text(size=9))
Fig6 + facet_wrap(~variable, scales="free", ncol=1, 
                   labeller = as_labeller(c(chla="Chlorophyll", total="Total Microcystins")),
                   strip.position="left")+
  labs(y=NULL)+
  theme(strip.background = element_blank(), strip.placement = "outside")
#ggplot2::ggsave("~/github/ClearLakeOverview/results/Fig6_sd.pdf", width=140, height = 140, units="mm",  dpi=300)

##### Figure 7: Nitrogen and Phosphorous ####
nutrients_df <- read.csv("~/github/ClearLakeOverview/raw-data/WholeLake_Survey_Nutrients.csv", header=TRUE)
nutrients_df$Arm=factor(nutrients_df$Arm, levels=c("Upper Arm", "Oaks Arm", "Lower Arm"))
nutrients.m <- melt(nutrients_df) %>%
  mutate(inter=str_sub(variable, 2), date=mdy(inter))%>%
  na.omit(.) %>%
  dcast(date+Arm+Measure~value_type)

head(nutrients.m)

Fig7 <-
  ggplot(nutrients.m, aes(x=date, y=Average,ymin = Average-S.D., ymax = Average+S.D., group=Arm,color=Arm))+
  geom_line() +
  geom_point() +
  geom_errorbar(width=0.5)+
  scale_color_manual(values=arms_cols, name="Arm")+
  labs(x="Date")+
  #scale_x_date(date_breaks="3 day")+
  theme_bw()+
  theme(panel.grid.minor = element_line(colour="white"))+
  theme(legend.title=element_blank(),
        legend.position="bottom",
        axis.title.y = element_text(angle = 90, hjust = .5, vjust = .5),
        axis.title.x = element_blank(),
        text = element_text(size=9))
Fig7 + facet_wrap(~Measure, scales="free", ncol=1, 
                  labeller = as_labeller(c(TN="Total Nitrogen", TP="Total Phosphate")),
                  strip.position="left")+
  labs(y=NULL)+
  theme(strip.background = element_blank(), strip.placement = "outside")
#ggplot2::ggsave("~/github/ClearLakeOverview/results/Fig7.pdf", width=140, height = 140, units="mm",  dpi=300)

##### Figure 9: 15-18 August WireWalker ####
WW_df <-read.csv("~/github/ClearLakeOverview/raw-data/WW_data_Clearlake_trimmed_2020.csv", header=T)
head(WW_df)
WW_df$date_time = ymd_hms(WW_df$date_time)

fig9_df <-WW_df %>%
  filter(between(date_time, as.Date("2020-08-15 00:00"), as.Date("2020-08-18 00:00")))

ggplot(data=fig9_df,aes(x=as.POSIXct(time),y=depth, color=DO_conc))+
  geom_line(linewidth=2, alpha=0.8, position="jitter")+
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%m-%d %H:%M")+   
  scale_y_reverse()+
  scale_color_gradientn(colours = rev(hex),name="O2 (mg/L)", breaks=c(2,4,6,8,10,12,14)) +
  theme_classic()+
  xlab(bquote('Day'))+
  ylab("Depth (m)")

#ggplot2::ggsave("~/github/ClearLakeOverview/results/Fig9.pdf", width=230, height = 190, units="mm",  dpi=300)

