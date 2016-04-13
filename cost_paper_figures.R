# Preliminary analysis for paper comparing lifetime hospital costs by IMD
# 
# Author: Miqdad Asaria
# Date: May 2015
###############################################################################

library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(scales)
library(stargazer)
library(xlsx)

load_data_from_file = function(){
	filename = "data/lifetable_and_cost_table.RData"
	load(filename)
	return(lifetable_and_cost_table)	
}

###############################################################################
# Create survival curves from mortality data 
###############################################################################

get_mortality_probability = function(age, sex, imd_quintile){
	prob = lifetable %>% 
			filter(age>=MIN_AGE & min(age,85)<=MAX_AGE & imd_quintile==IMD_QUINTILE & sex==SEX) %>%
			select(PROB_MORT)
	return(prob[1,1])
}

get_survival_probability = function(prob_mort){
	survival = vector("numeric",length(prob_mort))
	survival[1] = 1
	for (i in 1:(length(prob_mort)-1)) {
		survival[i+1] = survival[i]*(1-prob_mort[i])
	}
	return(survival)
}

# calculate the population standard error from the sub sample standard deviation
cost_standard_error = function(cost_table){
	return(sqrt(((cost_table$N/cost_table$POPULATION) * (cost_table$STDDEV_COST^2+(cost_table$TOTAL_COST/cost_table$N)^2) - (cost_table$TOTAL_COST/cost_table$POPULATION)^2))/sqrt(cost_table$POPULATION))
}

db_data = load_data_from_file()

lifetable = db_data[["lifetable"]]
cost_table = db_data[["cost_table"]]
cost_table$SE_COST = cost_standard_error(cost_table)
cost_table_imd = db_data[["cost_table_imd"]]
cost_table_imd$SE_COST = cost_standard_error(cost_table_imd)
cost_table_sex = db_data[["cost_table_sex"]]
cost_table_sex$SE_COST = cost_standard_error(cost_table_sex)
cost_table_overall = db_data[["cost_table_overall"]]
cost_table_overall$SE_COST = cost_standard_error(cost_table_overall)
emergency_cost_table = db_data[["emergency_cost_table"]]
emergency_cost_table$SE_COST = cost_standard_error(emergency_cost_table)
elective_cost_table = db_data[["elective_cost_table"]]
elective_cost_table$SE_COST = cost_standard_error(elective_cost_table)
outpatient_table = db_data[["outpatient_table"]]

discount_rate = 0.035

max_age = 125
mortality_rates = data_frame(AGE=rep(0:max_age,10),SEX=rep(c("M","F"),each=(max_age+1)*5),IMD_QUINTILE=c(rep(1:5,each=(max_age+1)),rep(1:5,each=(max_age+1)))) %>%
		group_by(AGE, SEX, IMD_QUINTILE) %>%
		mutate(PROB_MORT=get_mortality_probability(AGE, SEX, IMD_QUINTILE)) %>%
		group_by(SEX, IMD_QUINTILE) %>%
		arrange(AGE) %>%
		mutate(SURVIVAL=get_survival_probability(PROB_MORT)) %>%
		ungroup()

make_modelled_costs = function(cost_table){
	modelled_costs = mortality_rates %>%
			mutate(DF=1/(1+discount_rate)^AGE) %>%
			mutate(AGE1 = ifelse(AGE<85,AGE,85)) %>%
			inner_join(cost_table,by=c("AGE1"="AGE","IMD_QUINTILE"="IMD_QUINTILE","SEX"="SEX")) %>%
			mutate(EXPECTED_COST=SURVIVAL*AVERAGE_COST) %>%
			group_by(IMD_QUINTILE,SEX) %>%
			arrange(AGE) %>%
			mutate(DISCOUNTED_EXPECTED_COST=SURVIVAL*AVERAGE_COST*DF) %>%
			mutate(CUMULATIVE_COST=cumsum(EXPECTED_COST),DISCOUNTED_CUMULATIVE_COST=cumsum(DISCOUNTED_EXPECTED_COST)) %>%
			ungroup() %>%
			select(AGE,SEX,IMD_QUINTILE,EXPECTED_COST,CUMULATIVE_COST,DISCOUNTED_EXPECTED_COST,DISCOUNTED_CUMULATIVE_COST)
	return(modelled_costs)
}


modelled_costs = make_modelled_costs(cost_table)
modelled_emergency_costs = make_modelled_costs(emergency_cost_table)
modelled_elective_costs = make_modelled_costs(elective_cost_table)
modelled_costs_sex = modelled_costs %>% filter(AGE==125) %>% group_by(SEX) %>% summarise(mean(CUMULATIVE_COST))
###############################################################################
# Functions to make generic deprivation and sex plots 
###############################################################################

imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")


get_legend = function(plot, position){
	g = ggplotGrob(plot + theme(legend.position=position))$grobs
	legend = g[[which(sapply(g, function(x) x$name) == "guide-box")]]
	return(legend)
}

deprivation_plot = function(graph_data,xvar,yvar,xlab,ylab,sex){
	if(grepl("\\.",ylab)){
		ylab = substr(ylab,4,nchar(ylab))
	}
	
	plot = ggplot(subset(graph_data,SEX==sex), aes_string(x=xvar,y=yvar,group="IMD_QUINTILE",colour="IMD_QUINTILE",linetype="IMD_QUINTILE")) + 
			geom_line() + 
			xlab(xlab) +
			ylab(ylab) +
			ggtitle(sex) +
			scale_colour_manual(name="IMD Group", values=c("black","lightblue","lightgreen","lightgrey","darkgrey")) +
			scale_linetype_manual(name="IMD Group", values=c(1,2,2,2,1))
	result = list()
	result[["plot"]] = plot
	result[["limits"]] = c(0,max(subset(graph_data,SEX==sex)[,yvar]))
	return(result)
}		

deprivation_panel_plot = function(graph_data,xvar,yvar,xlab,ylab,filename,caption){
	plot_1_a = deprivation_plot(subset(graph_data,FACET==levels(graph_data$FACET)[1]),xvar,yvar,xlab,levels(graph_data$FACET)[1],"Female")
	plot_1_b = deprivation_plot(subset(graph_data,FACET==levels(graph_data$FACET)[1]),xvar,yvar,xlab,levels(graph_data$FACET)[1],"Male")
	plot_2_a = deprivation_plot(subset(graph_data,FACET==levels(graph_data$FACET)[2]),xvar,yvar,xlab,levels(graph_data$FACET)[2],"Female")
	plot_2_b = deprivation_plot(subset(graph_data,FACET==levels(graph_data$FACET)[2]),xvar,yvar,xlab,levels(graph_data$FACET)[2],"Male")
	plot_3_a = deprivation_plot(subset(graph_data,FACET==levels(graph_data$FACET)[3]),xvar,yvar,xlab,levels(graph_data$FACET)[3],"Female")
	plot_3_b = deprivation_plot(subset(graph_data,FACET==levels(graph_data$FACET)[3]),xvar,yvar,xlab,levels(graph_data$FACET)[3],"Male")
	spacer = rectGrob(gp=gpar(col="white"))

	
	top_ymin = min(plot_1_a[["limits"]],plot_1_b[["limits"]])
	top_ymax = max(plot_1_a[["limits"]],plot_1_b[["limits"]])
	
	middle_ymin = min(plot_2_a[["limits"]],plot_2_b[["limits"]])
	middle_ymax = max(plot_2_a[["limits"]],plot_2_b[["limits"]])	
	
	bottom_ymin = min(plot_3_a[["limits"]],plot_3_b[["limits"]])
	bottom_ymax = max(plot_3_a[["limits"]],plot_3_b[["limits"]])
	
	top_panel =  arrangeGrob(
			main=textGrob(levels(graph_data$FACET)[1],gp=gpar(fontsize=20,fontface="bold")),
			arrangeGrob(plot_1_a[["plot"]] + scale_y_continuous(labels=comma, limits=c(top_ymin,top_ymax)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none"), 
					plot_1_b[["plot"]] + scale_y_continuous(labels=comma, limits=c(top_ymin,top_ymax)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none"), 
					ncol=2),
			nrow=2, heights=c(0.1,0.9))
	
	middle_panel =  arrangeGrob(
			main=textGrob(levels(graph_data$FACET)[2],gp=gpar(fontsize=20,fontface="bold")),
			arrangeGrob(plot_2_a[["plot"]] + scale_y_continuous(labels=comma, limits=c(middle_ymin,middle_ymax)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none"), 
			plot_2_b[["plot"]] + scale_y_continuous(labels=comma, limits=c(middle_ymin,middle_ymax)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none"), 
			ncol=2),
	nrow=2, heights=c(0.1,0.9))
	
	
	bottom_panel =  arrangeGrob(
			main=textGrob(levels(graph_data$FACET)[3],gp=gpar(fontsize=20,fontface="bold")),
			arrangeGrob(plot_3_a[["plot"]] + scale_y_continuous(labels=comma, limits=c(bottom_ymin,bottom_ymax)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none"), 
			plot_3_b[["plot"]] + scale_y_continuous(labels=comma, limits=c(bottom_ymin,bottom_ymax)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none"), 
			ncol=2),
	nrow=2, heights=c(0.1,0.9))
	
	plot = arrangeGrob(
			spacer,
			top_panel,
			spacer,
			middle_panel,
			spacer,
			bottom_panel,
			spacer,
			nrow=7,
			heights=c(0.01,0.3,0.01,0.3,0.01,0.3,0.01))
	return(plot)
}

deprivation_panel2 = function(graph_data,xvar,yvar,xlab,ylab,filename,caption){
	plot = deprivation_panel_plot(graph_data,xvar,yvar,xlab,ylab,filename,caption)
	plot_1_a = deprivation_plot(subset(graph_data,FACET==levels(graph_data$FACET)[1]),xvar,yvar,xlab,levels(graph_data$FACET)[1],"Female")
	legend = get_legend(plot_1_a[["plot"]],"right")
	combined_panel = arrangeGrob(
			arrangeGrob(plot,
			legend,
			ncol = 2,
			widths = unit.c(unit(1, "npc") - sum(legend$width), sum(legend$width))),
	sub = textGrob(caption, x = 0, hjust = -0.1, vjust=0.1,	gp = gpar(fontface = "italic", fontsize = 14)),
	heights=c(0.95,0.05))
	
	ggsave(filename=paste("output/",filename,".png",sep=""),plot=combined_panel,width=37,height=50,units="cm",dpi=300)
}		


deprivation_panel_appendix = function(graph_data,xvar,yvar,xlab,ylab,filename,caption){
	elec_plot = deprivation_panel_plot(subset(graph_data,TYPE==levels(graph_data$TYPE)[1]),xvar,yvar,xlab,ylab,filename,caption)
	emer_plot = deprivation_panel_plot(subset(graph_data,TYPE==levels(graph_data$TYPE)[2]),xvar,yvar,xlab,ylab,filename,caption)
	plot_1_a = deprivation_plot(subset(graph_data,FACET==levels(graph_data$FACET)[1]&TYPE==graph_data$TYPE[1]),xvar,yvar,xlab,levels(graph_data$FACET)[1],"Female")
	legend = get_legend(plot_1_a[["plot"]],"right")
	spacer = rectGrob(gp=gpar(col="white"))
	
	combined_panel = arrangeGrob(
			arrangeGrob(
			arrangeGrob(main=textGrob(levels(graph_data$TYPE)[1],gp=gpar(fontsize=30,fontface="bold")),elec_plot,nrow=2,heights=c(0.025,0.975)),
			spacer,
			arrangeGrob(main=textGrob(levels(graph_data$TYPE)[2],gp=gpar(fontsize=30,fontface="bold")),emer_plot,nrow=2,heights=c(0.025,0.975)),
			legend,
			ncol=4,
			widths=c(0.4,0.02,0.4,0.08)),
			sub=textGrob(caption, x = 0, hjust = -0.1, vjust=0.1,	gp = gpar(fontface = "italic", fontsize = 14)),
			ncol=1,
			heights=c(0.99,0.01))
	ggsave(filename=paste("output/",filename,".png",sep=""),plot=combined_panel,width=67,height=50,units="cm",dpi=300)
}		

deprivation_panel = function(graph_data,xvar,yvar,xlab,ylab,filename,caption){
	plot_1_a = deprivation_plot(subset(graph_data,FACET==levels(graph_data$FACET)[1]),xvar,yvar,xlab,levels(graph_data$FACET)[1],"Female")
	plot_1_b = deprivation_plot(subset(graph_data,FACET==levels(graph_data$FACET)[1]),xvar,yvar,xlab,levels(graph_data$FACET)[1],"Male")
	plot_2_a = deprivation_plot(subset(graph_data,FACET==levels(graph_data$FACET)[2]),xvar,yvar,xlab,levels(graph_data$FACET)[2],"Female")
	plot_2_b = deprivation_plot(subset(graph_data,FACET==levels(graph_data$FACET)[2]),xvar,yvar,xlab,levels(graph_data$FACET)[2],"Male")
	spacer = rectGrob(gp=gpar(col="white"))
	legend = get_legend(plot_1_a[["plot"]],"right")
	
	top_ymin = min(plot_1_a[["limits"]],plot_1_b[["limits"]])
	top_ymax = max(plot_1_a[["limits"]],plot_1_b[["limits"]])

	bottom_ymin = min(plot_2_a[["limits"]],plot_2_b[["limits"]])
	bottom_ymax = max(plot_2_a[["limits"]],plot_2_b[["limits"]])
	
	top_panel =  arrangeGrob(
			main=textGrob(levels(graph_data$FACET)[1],gp=gpar(fontsize=20,fontface="bold")),
			arrangeGrob(plot_1_a[["plot"]] + scale_y_continuous(labels=comma, limits=c(top_ymin,top_ymax)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none"), 
			plot_1_b[["plot"]] + scale_y_continuous(labels=comma, limits=c(top_ymin,top_ymax)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none"), 
			ncol=2),
	nrow=2, heights=c(0.1,0.9))


	bottom_panel =  arrangeGrob(
			main=textGrob(levels(graph_data$FACET)[2],gp=gpar(fontsize=20,fontface="bold")),
			arrangeGrob(plot_2_a[["plot"]] + scale_y_continuous(labels=comma, limits=c(bottom_ymin,bottom_ymax)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none"), 
			plot_2_b[["plot"]] + scale_y_continuous(labels=comma, limits=c(bottom_ymin,bottom_ymax)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none"), 
			ncol=2),
	nrow=2, heights=c(0.1,0.9))
	
	combined_panel = arrangeGrob(
			arrangeGrob(
			arrangeGrob(
					spacer,
					top_panel,
					spacer,
					bottom_panel,
					spacer,
					nrow=5,
					heights=c(0.03,0.45,0.03,0.45,0.03)),
			legend,
			ncol = 2,
			widths = unit.c(unit(1, "npc") - sum(legend$width), sum(legend$width))),
	sub = textGrob(caption, x = 0, hjust = -0.1, vjust=0.1,	gp = gpar(fontface = "italic", fontsize = 14)),
	nrow=2,heights=c(0.95,0.05))
	
	ggsave(filename=paste("output/",filename,".png",sep=""),plot=combined_panel,width=37,height=35,units="cm",dpi=300)
}		

###############################################################################
# Survival plots 
###############################################################################

# survival plot for paper
survival_panel = function(graph_data,xvar,yvar,xlab,ylab,filename,caption){
	plot_1_a = deprivation_plot(graph_data,xvar,yvar,xlab,ylab,"Female")
	plot_1_b = deprivation_plot(graph_data,xvar,yvar,xlab,ylab,"Male")
	spacer = rectGrob(gp=gpar(col="white"))
	legend = get_legend(plot_1_a[["plot"]],"right")
	
	top_ymin = min(plot_1_a[["limits"]],plot_1_b[["limits"]])
	top_ymax = max(plot_1_a[["limits"]],plot_1_b[["limits"]])
	
	top_panel =  arrangeGrob(
			main=textGrob(levels(graph_data$FACET)[1],gp=gpar(fontsize=20,fontface="bold")),
			arrangeGrob(plot_1_a[["plot"]] + scale_y_continuous(labels=comma, limits=c(top_ymin,top_ymax)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none"), 
			plot_1_b[["plot"]] + scale_y_continuous(labels=comma, limits=c(top_ymin,top_ymax)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none"), 
			ncol=2),
	nrows=2,heights=c(0.1,0.9))
	
	combined_panel = arrangeGrob(
			arrangeGrob(
					spacer,
					top_panel,
					spacer,
					nrow=3,
					heights=c(0.05,0.90,0.05)),
			legend,
			ncol = 2,
			sub = textGrob(caption, x = 0, hjust = -0.1, vjust=0.1,	gp = gpar(fontface = "italic", fontsize = 14)),
			widths = unit.c(unit(1, "npc") - sum(legend$width), sum(legend$width)))
	
	ggsave(filename=paste("output/",filename,".png",sep=""),plot=combined_panel,width=37,height=14,units="cm",dpi=300)
}		
survival_data = mortality_rates %>%
		#filter(AGE>20 & AGE<121) %>%
		mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))
		
#survival_panel(survival_data,"AGE","SURVIVAL","Age","Probability of Survival","survival_curves_panel","Figure 2. Survival curves by sex and deprivation")

###############################################################################
# Inpatient utilisation and cost plots 
###############################################################################

make_cost_plots = function(cost_table, prefix, caption){
	cost_graph_data = cost_table %>%
			mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male"))) %>% 
			gather(FACET,VALUE,N,HOSP_EPI_RATE,AVERAGE_COST)
	cost_graph_data$FACET = factor(cost_graph_data$FACET, levels=c("N","HOSP_EPI_RATE","AVERAGE_COST"), 
			labels=c("a. Total Hospital Episodes","b. Hospital Episodes per 100,000 Population","c. Average Annual Cost (\u00A3)"))
	deprivation_panel2(subset(cost_graph_data,AGE<85),"AGE","VALUE","Age","",paste(prefix,"overall_resource_use_panel",sep=""),caption)		
}
caption="" # "Figure 1. All hospital inpatient admissions split by age, sex and deprivation (2011/12)"
make_cost_plots(cost_table,"",caption)

make_cost_plots_appendix = function(elective_cost_table, emergency_cost_table, prefix, caption){
	cost_graph_data_elec = elective_cost_table %>%
			mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male"))) %>% 
			gather(FACET,VALUE,N,HOSP_EPI_RATE,AVERAGE_COST) %>%
			mutate(TYPE="ELEC")
	cost_graph_data_emer = emergency_cost_table %>%
			mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male"))) %>% 
			gather(FACET,VALUE,N,HOSP_EPI_RATE,AVERAGE_COST) %>%
			mutate(TYPE="EMER")
	cost_graph_data = bind_rows(cost_graph_data_elec,cost_graph_data_emer)
	
	cost_graph_data$FACET = factor(cost_graph_data$FACET, levels=c("N","HOSP_EPI_RATE","AVERAGE_COST"), 
			labels=c("a. Total Hospital Episodes","b. Hospital Episodes per 100,000 Population","c. Average Annual Cost (\u00A3)"))
	cost_graph_data$TYPE = factor(cost_graph_data$TYPE, levels=c("ELEC","EMER"), 
			labels=c("1. Elective Hospital Admissions","2. Emergency Hospital Admissions"))
	
	deprivation_panel_appendix(subset(cost_graph_data,AGE<85),"AGE","VALUE","Age","",paste(prefix,"resource_use_panel",sep=""),caption)		
}
caption="" # "Figure A1. Hospital inpatient admissions split by admission type, age, sex and deprivation (2011/12)"
make_cost_plots_appendix(elective_cost_table,emergency_cost_table,"emergency_elective_",caption)

###############################################################################
# Expected cost plots for inpatient care - combining survival and average costs  
###############################################################################

make_expected_cost_plots = function(modelled_costs,prefix,caption){
	modelled_cost_graph_data = modelled_costs %>%
			mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male"))) %>% 
			gather(FACET,VALUE,EXPECTED_COST,CUMULATIVE_COST)
	
	modelled_cost_graph_data$FACET = factor(modelled_cost_graph_data$FACET, levels=c("EXPECTED_COST","CUMULATIVE_COST"), labels=c("Expected Cost (\u00A3)","Cumulative Expected Cost (\u00A3)"))
	deprivation_panel(modelled_cost_graph_data,"AGE","VALUE","Age","",paste(prefix,"lifetime_cost_panel",sep=""),caption)		
}
caption="" # "Figure 3. Hospital inpatient costs broken down by age, sex and deprivation"
#make_expected_cost_plots(modelled_costs,"","Figure 3. Hospital inpatient costs broken down by age, sex and deprivation")

make_lifetime_cost_plots = function(survival_data, modelled_costs, prefix, caption){
	survival_graph_data = survival_data %>% 
			select(AGE,SEX,IMD_QUINTILE,SURVIVAL) %>%
			gather(FACET,VALUE,SURVIVAL)
	modelled_cost_graph_data = modelled_costs %>%
			mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male"))) %>% 
			select(AGE,SEX,IMD_QUINTILE,CUMULATIVE_COST) %>%
			gather(FACET,VALUE,CUMULATIVE_COST)
	
	graph_data = bind_rows(survival_graph_data,modelled_cost_graph_data)
	graph_data$FACET = factor(graph_data$FACET, levels=c("SURVIVAL","CUMULATIVE_COST"), labels=c("a. Survival Curves","b. Cumulative Expected Lifetime Cost (\u00A3)"))
	deprivation_panel(graph_data,"AGE","VALUE","Age","",paste(prefix,"lifetime_cost_panel",sep=""),caption)			
}

caption="" # "Figure 2. Survival curves and cumulative lifetime hospital inpatient costs broken down by age, sex and deprivation"
make_lifetime_cost_plots(survival_data, modelled_costs, "overall_", caption)
	
make_expected_cost_plots_appendix = function(modelled_elective_costs,modelled_emergency_costs,prefix,caption){
	modelled_cost_graph_data_emer = modelled_emergency_costs %>%
			mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male"))) %>% 
			select(AGE,SEX,IMD_QUINTILE,CUMULATIVE_COST_EMER=CUMULATIVE_COST) %>%
			gather(FACET,VALUE,CUMULATIVE_COST_EMER)
	modelled_cost_graph_data_elec = modelled_elective_costs %>%
			mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male"))) %>% 
			select(AGE,SEX,IMD_QUINTILE,CUMULATIVE_COST_ELEC=CUMULATIVE_COST) %>%
			gather(FACET,VALUE,CUMULATIVE_COST_ELEC)
	modelled_cost_graph_data = bind_rows(modelled_cost_graph_data_emer,modelled_cost_graph_data_elec)
	modelled_cost_graph_data$FACET = factor(modelled_cost_graph_data$FACET, levels=c("CUMULATIVE_COST_ELEC","CUMULATIVE_COST_EMER"), labels=c("a. Cumulative Expected Lifetime Costs of Elective Hopitalisations (\u00A3)","b. Cumulative Expected Lifetime Costs of Emergency Hopitalisations (\u00A3)"))
	deprivation_panel(modelled_cost_graph_data,"AGE","VALUE","Age","",paste(prefix,"lifetime_cost_panel",sep=""),caption)		
}
caption="" # "Figure A2. Hospital inpatient costs broken down by admission type age, sex and deprivation"
make_expected_cost_plots_appendix(modelled_elective_costs,modelled_emergency_costs,"elective_emergency_",caption)


###############################################################################
# Outpatient utilisation plots 
###############################################################################

make_outpatient_plots = function(outpatient_table, prefix, caption){
	outpatient_graph_data = outpatient_table %>%
			mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male"))) %>% 
			gather(FACET,VALUE,N,HOSP_APPT_RATE)
	outpatient_graph_data$FACET = factor(outpatient_graph_data$FACET, levels=c("N","HOSP_APPT_RATE"), labels=c("a. Outpatient Appointments","b. Outpatient Appointments per 100,000 Population"))
	deprivation_panel(outpatient_graph_data,"AGE","VALUE","Age","",paste(prefix,"appointments",sep=""),caption)		
}
caption = "" # "Figure A3. Outpatient appointments split by age, sex and deprivation (2011/12)"
make_outpatient_plots(subset(outpatient_table,AGE<85),"outpatient_",caption)


###############################################################################
# Summary table  
###############################################################################

summary = inner_join(
		cost_table %>% group_by(IMD_QUINTILE) %>% summarise(POP=sum(POPULATION),total_N=sum(N)) %>% mutate(total_rate=round((total_N/POP)*100000)),
		inner_join(
				elective_cost_table %>% group_by(IMD_QUINTILE) %>% summarise(POP=sum(POPULATION),elective_N=sum(N)) %>% mutate(elective_rate=round((elective_N/POP)*100000)),
				emergency_cost_table %>% group_by(IMD_QUINTILE) %>% summarise(POP=sum(POPULATION),emergency_N=sum(N)) %>% mutate(emergency_rate=round((emergency_N/POP)*100000))))
write.csv(summary, file="output/summary_table.csv")

###############################################################################
# Cost of inequality table 
###############################################################################

q5_cost = cost_table %>% 
		filter(IMD_QUINTILE==5) %>%
		group_by(SEX) %>%
		summarise(total_cost=sum(AVERAGE_COST*POPULATION))

imd_average_cost = cost_table %>% 
		group_by(IMD_QUINTILE) %>%
		summarise(total_cost=sum(AVERAGE_COST*POPULATION)/sum(POPULATION))

# calculate the total cost of inequality by each subgroup
cost_of_inequality = function(cost_table){
	cost_table_q5 = cost_table %>% 
			filter(IMD_QUINTILE==5) %>%
			select(AGE,SEX,Q5_AVERAGE_COST =AVERAGE_COST)
	cost_table_merged = inner_join(cost_table,cost_table_q5,by=c("AGE","SEX"))
	cost_of_inequality = cost_table_merged %>%
			mutate(INEQ_COST = round(TOTAL_COST - (POPULATION*Q5_AVERAGE_COST))) %>%
			select(AGE,SEX,IMD_QUINTILE,INEQ_COST)		
	return(cost_of_inequality)	
}

total_cost_of_inequality = cost_of_inequality(cost_table)

total_cost_of_ineq_table = total_cost_of_inequality %>% 
		group_by(SEX,IMD_QUINTILE) %>%
		summarise(INEQ_COST=sum(INEQ_COST)) %>%
		ungroup() %>%
		spread(SEX,INEQ_COST) %>%
		mutate(TOTAL=F+M) %>%
		bind_rows(.,summarise_each(.,funs(sum))) %>%
		apply(.,c(1,2),format,big.mark=",",scientific=FALSE)

write.csv(total_cost_of_ineq_table,file="output/total_cost_of_ineq.csv")

sink("output/total_cost_of_ineq.tex")
stargazer(total_cost_of_ineq_table,title="Total cost of inequality - assuming everybody could potentially have the costs of the most affluent",summary=FALSE,rownames=FALSE,font.size="tiny")
sink()

###############################################################################
# Resource use and cost refence tables  
###############################################################################
summary_imd_sex = cost_table %>% 
		select(AGE,SEX,IMD_QUINTILE,AVERAGE_COST,SE_COST) %>%
		reshape( 
				timevar = "IMD_QUINTILE",
				idvar = c("AGE","SEX"),
				direction = "wide")

summary_imd = cost_table_imd  %>% 
		select(AGE,IMD_QUINTILE,AVERAGE_COST,SE_COST) %>%
		reshape( 
				timevar = "IMD_QUINTILE",
				idvar = c("AGE"),
				direction = "wide")

summary_sex = cost_table_sex %>% 
		select(AGE,SEX,AVERAGE_COST,SE_COST) 

summary_overall = cost_table_overall %>%
		select(AGE,AVERAGE_COST,SE_COST) 

save.xlsx = function (file, ...){
	require(xlsx, quietly = TRUE)
	objects = list(...)
	fargs = as.list(match.call(expand.dots = TRUE))
	objnames = as.character(fargs)[-c(1, 2)]
	nobjects = length(objects)
	for (i in 1:nobjects) {
		if (i == 1)
			write.xlsx(objects[[i]], file, sheetName = objnames[i])
		else write.xlsx(objects[[i]], file, sheetName = objnames[i],
					append = TRUE)
	}
	print(paste("Workbook", file, "has", nobjects, "worksheets."))
}


save.xlsx("output/cost_summary.xlsx", summary_imd_sex, summary_imd, summary_sex, summary_overall)