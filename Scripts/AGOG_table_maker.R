library(ggplot2)
library(cowplot)






p <- ggplot(data=Model.sig, aes(y=Variable, x=OR, xmin=CI2.5, xmax=CI97.5))+ 
  geom_point(size=3,shape=15)+ 
  geom_errorbarh(height=.5,size=1)+
  geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5)+
  scale_x_continuous(limits=c(0,4), breaks = c(0:3))+#,name="Odds Ratio")+
  # labs(y="Risk Factor")+
  scale_y_discrete(limits = rev(levels(factor(Model.sig$Variable))))+
  theme_minimal()+
  theme(text=element_text(family="Times",size=18, color="black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #theme(panel.spacing = unit(1, "lines")) +
  theme(text=element_text(size=16,  family="sans")) + 
  theme(plot.title = element_blank(),axis.text.y=element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())+
  theme(axis.line.y = element_blank()) +
theme(axis.line.x = element_line(color="black"))

data_table <- ggplot(Model.sig, aes(x = 1, y = nrow(Model.sig):1, label = format(Variable, nsmall = 1))) +
  geom_text(size = 4, hjust=0, vjust=0.5) + theme_bw() +
  scale_y_discrete(limits = rev(levels(factor(Model.sig$Variable)))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),#element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"),#element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(1,4.5))
#data_table

#grid.arrange(data_table, p, ncol=2)

plot_grid(data_table, p, align = "h",rel_widths=c(0.3,1))

