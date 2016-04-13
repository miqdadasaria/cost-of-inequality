# Preliminary analysis for paper comparing lifetime hospital costs by IMD
# 
# Author: Miqdad Asaria
# Date: May 2015
###############################################################################

library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(scales)
library(stargazer)

###############################################################################
# Extract mortality data and cost data from database 
###############################################################################

extract_data_from_db = function(cached=TRUE){
	filename = "data/lifetable_and_cost_table.RData"
	if(cached){
		load(filename)
	} else {
		source("../db_connection.R")
		con = get_db_connection()
		
		hes_missing_data_inflation_factor = 1+(18808903-17149608)/18808903
		
		sql = "SELECT (d.deaths/p.population) prob_mort, d.deaths, p.* FROM
				(SELECT COUNT(*) AS deaths, min_age, max_age, SEX, NVL(IMD_QUINTILE,0) AS IMD_QUINTILE FROM ONS_MORTALITY
				WHERE HES_YEAR = 2011
				GROUP BY min_age, max_age, SEX, IMD_QUINTILE
				ORDER BY  min_age, max_age, SEX, IMD_QUINTILE) d
				INNER JOIN
				(SELECT SUM(population) AS population, min_age, max_age, sex, imd_quintile FROM
				(SELECT 
				population,
				CASE 
				WHEN age BETWEEN 0 AND 0 THEN 0
				WHEN age BETWEEN 1 AND 4 THEN 1
				WHEN age BETWEEN 5 AND 9 THEN 5
				WHEN age BETWEEN 10 AND 14 THEN 10
				WHEN age BETWEEN 15 AND 19 THEN 15
				WHEN age BETWEEN 20 AND 24 THEN 20
				WHEN age BETWEEN 25 AND 29 THEN 25
				WHEN age BETWEEN 30 AND 34 THEN 30
				WHEN age BETWEEN 35 AND 39 THEN 35
				WHEN age BETWEEN 40 AND 44 THEN 40
				WHEN age BETWEEN 45 AND 49 THEN 45
				WHEN age BETWEEN 50 AND 54 THEN 50
				WHEN age BETWEEN 55 AND 59 THEN 55
				WHEN age BETWEEN 60 AND 64 THEN 60
				WHEN age BETWEEN 65 AND 69 THEN 65
				WHEN age BETWEEN 70 AND 74 THEN 70
				WHEN age BETWEEN 75 AND 79 THEN 75
				WHEN age BETWEEN 80 AND 84 THEN 80
				WHEN age > 84 THEN 85
				ELSE NULL
				END min_age,
				CASE 
				WHEN age BETWEEN 0 AND 0 THEN 0
				WHEN age BETWEEN 1 AND 4 THEN 4
				WHEN age BETWEEN 5 AND 9 THEN 9
				WHEN age BETWEEN 10 AND 14 THEN 14
				WHEN age BETWEEN 15 AND 19 THEN 19
				WHEN age BETWEEN 20 AND 24 THEN 24
				WHEN age BETWEEN 25 AND 29 THEN 29
				WHEN age BETWEEN 30 AND 34 THEN 34
				WHEN age BETWEEN 35 AND 39 THEN 39
				WHEN age BETWEEN 40 AND 44 THEN 44
				WHEN age BETWEEN 45 AND 49 THEN 49
				WHEN age BETWEEN 50 AND 54 THEN 54
				WHEN age BETWEEN 55 AND 59 THEN 59
				WHEN age BETWEEN 60 AND 64 THEN 64
				WHEN age BETWEEN 65 AND 69 THEN 69
				WHEN age BETWEEN 70 AND 74 THEN 74
				WHEN age BETWEEN 75 AND 79 THEN 79
				WHEN age BETWEEN 80 AND 84 THEN 84
				WHEN age > 84 THEN 85
				ELSE NULL
				END max_age,
				sex,
				quintile AS imd_quintile
				FROM ONS_POPULATION pop
				INNER JOIN
				IMD_2010 imd
				ON pop.lsoa01cd = imd.lsoa01cd
				WHERE YEAR=2011) 
				GROUP BY min_age, max_age, SEX, IMD_QUINTILE
				ORDER BY  min_age, max_age, SEX, IMD_QUINTILE) p
				ON d.min_age=p.min_age AND p.sex=d.sex AND p.imd_quintile=d.imd_quintile"
		
		lifetable = dbGetQuery(con,sql)
		
		cost_sql = paste("SELECT AGE, SEX, IMD_QUINTILE, POPULATION, N*",hes_missing_data_inflation_factor," AS N, 
					TOTAL_COST*",hes_missing_data_inflation_factor," AS TOTAL_COST, 
					STDDEV_COST, 
				(TOTAL_COST/POPULATION)*",hes_missing_data_inflation_factor," AS AVERAGE_COST, 
				(N/POPULATION)*100000*",hes_missing_data_inflation_factor," AS HOSP_EPI_RATE 
				FROM 
				((SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				COUNT(*) n, 
				age, 
				sex, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE AGE<85
				GROUP BY age, sex, imd_quintile) c
				ON p.age=c.age AND p.sex=c.sex AND p.imd_quintile=c.imd_quintile)
				
				UNION
				
				(SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION pop
				INNER JOIN
				IMD_2010 imd
				ON pop.LSOA01CD=imd.LSOA01CD
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE
				ORDER BY QUINTILE, SEX, AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				COUNT(*) n, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				85 AS age, 
				sex, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE age > 84
				GROUP BY sex, imd_quintile) c
				ON p.sex=c.sex AND p.imd_quintile=c.imd_quintile AND p.age=c.age))
				ORDER BY IMD_QUINTILE, SEX, AGE",sep="")
		
		cost_table = dbGetQuery(con,cost_sql)
		
		cost_sql_imd = paste("SELECT AGE, IMD_QUINTILE, POPULATION, N*",hes_missing_data_inflation_factor," AS N, 
TOTAL_COST*",hes_missing_data_inflation_factor," AS TOTAL_COST, 
STDDEV_COST, 
				(TOTAL_COST/POPULATION)*",hes_missing_data_inflation_factor," AS AVERAGE_COST, 
				(N/POPULATION)*100000*",hes_missing_data_inflation_factor," AS HOSP_EPI_RATE 
				FROM 
				((SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, QUINTILE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				COUNT(*) n, 
				age, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE AGE<85
				GROUP BY age, imd_quintile) c
				ON p.age=c.age AND p.imd_quintile=c.imd_quintile)
				
				UNION
				
				(SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION pop
				INNER JOIN
				IMD_2010 imd
				ON pop.LSOA01CD=imd.LSOA01CD
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, QUINTILE
				ORDER BY QUINTILE, AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				COUNT(*) n, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				85 AS age, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE age > 84
				GROUP BY imd_quintile) c
				ON p.imd_quintile=c.imd_quintile AND p.age=c.age))
				ORDER BY IMD_QUINTILE, AGE", sep="")
		
		cost_table_imd = dbGetQuery(con,cost_sql_imd)
		
		cost_sql_sex = paste("SELECT AGE, SEX, POPULATION, N*",hes_missing_data_inflation_factor," AS N, 
TOTAL_COST*",hes_missing_data_inflation_factor," AS TOTAL_COST, 
STDDEV_COST, 
				(TOTAL_COST/POPULATION)*",hes_missing_data_inflation_factor," AS AVERAGE_COST, 
				(N/POPULATION)*100000*",hes_missing_data_inflation_factor," AS HOSP_EPI_RATE 
				FROM 
				((SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX 
				FROM
				ONS_POPULATION p
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, SEX) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				COUNT(*) n, 
				age, 
				sex 
		 		FROM hes_episode_cost_2011
				WHERE AGE<85
				GROUP BY age, sex) c
				ON p.age=c.age AND p.sex=c.sex)
				
				UNION
				
				(SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX 
				FROM
				ONS_POPULATION pop
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, SEX
				ORDER BY SEX, AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				COUNT(*) n, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				85 AS age, 
				sex 
				FROM hes_episode_cost_2011
				WHERE age > 84
				GROUP BY sex) c
				ON p.sex=c.sex AND p.age=c.age))
				ORDER BY  SEX, AGE", sep="")
		
		cost_table_sex = dbGetQuery(con,cost_sql_sex)
		
		cost_sql_overall = paste("SELECT AGE, POPULATION, N*",hes_missing_data_inflation_factor," AS N, 
TOTAL_COST*",hes_missing_data_inflation_factor," AS TOTAL_COST, 
STDDEV_COST, 
				(TOTAL_COST/POPULATION)*",hes_missing_data_inflation_factor," AS AVERAGE_COST, 
				(N/POPULATION)*100000*",hes_missing_data_inflation_factor," AS HOSP_EPI_RATE 
				FROM 
				((SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE 
				FROM
				ONS_POPULATION p
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				COUNT(*) n, 
				age 
				FROM hes_episode_cost_2011
				WHERE AGE<85
				GROUP BY age) c
				ON p.age=c.age)
				
				UNION
				
				(SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE 
				FROM
				ONS_POPULATION pop
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE
				ORDER BY AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				COUNT(*) n, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				85 AS age
				FROM hes_episode_cost_2011
				WHERE age > 84) c
				ON p.age=c.age))
				ORDER BY AGE", sep="")
		
		cost_table_overall = dbGetQuery(con,cost_sql_overall)
		
		emergency_cost_sql = paste("SELECT AGE, SEX, IMD_QUINTILE, POPULATION, N*",hes_missing_data_inflation_factor," AS N, 
TOTAL_COST*",hes_missing_data_inflation_factor," AS TOTAL_COST, 
STDDEV_COST, 
				(TOTAL_COST/POPULATION)*",hes_missing_data_inflation_factor," AS AVERAGE_COST, 
				(N/POPULATION)*100000*",hes_missing_data_inflation_factor," AS HOSP_EPI_RATE 
				FROM 
				((SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				COUNT(*) n, 
				age, 
				sex, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE AGE<85 AND EMERGENCY=1
				GROUP BY age, sex, imd_quintile) c
				ON p.age=c.age AND p.sex=c.sex AND p.imd_quintile=c.imd_quintile)
				
				UNION
				
				(SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION pop
				INNER JOIN
				IMD_2010 imd
				ON pop.LSOA01CD=imd.LSOA01CD
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE
				ORDER BY QUINTILE, SEX, AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				COUNT(*) n, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				85 AS age, 
				sex, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE age > 84 AND EMERGENCY=1
				GROUP BY sex, imd_quintile) c
				ON p.sex=c.sex AND p.imd_quintile=c.imd_quintile AND p.age=c.age))
				ORDER BY IMD_QUINTILE, SEX, AGE", sep="")
		
		emergency_cost_table = dbGetQuery(con,emergency_cost_sql)
		
		elective_cost_sql = paste("SELECT AGE, SEX, IMD_QUINTILE, POPULATION, N*",hes_missing_data_inflation_factor," AS N, 
TOTAL_COST*",hes_missing_data_inflation_factor," AS TOTAL_COST, 
STDDEV_COST, 
				(TOTAL_COST/POPULATION)*",hes_missing_data_inflation_factor," AS AVERAGE_COST, 
				(N/POPULATION)*100000*",hes_missing_data_inflation_factor," AS HOSP_EPI_RATE 
				FROM 
				((SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				COUNT(*) n, 
				age, 
				sex, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE AGE<85 AND EMERGENCY=0
				GROUP BY age, sex, imd_quintile) c
				ON p.age=c.age AND p.sex=c.sex AND p.imd_quintile=c.imd_quintile)
				
				UNION
				
				(SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION pop
				INNER JOIN
				IMD_2010 imd
				ON pop.LSOA01CD=imd.LSOA01CD
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE
				ORDER BY QUINTILE, SEX, AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				COUNT(*) n, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				85 AS age, 
				sex, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE age > 84 AND EMERGENCY=0
				GROUP BY sex, imd_quintile) c
				ON p.sex=c.sex AND p.imd_quintile=c.imd_quintile AND p.age=c.age))
				ORDER BY IMD_QUINTILE, SEX, AGE", sep="")
		
		elective_cost_table = dbGetQuery(con,elective_cost_sql)
		
		outpatient_sql = "SELECT AGE, SEX, IMD_QUINTILE, POPULATION, N, (N/POPULATION)*100000 AS HOSP_APPT_RATE 
				FROM 
				((SELECT p.*, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE) p
				LEFT JOIN
				(SELECT COUNT(*) n, 
				AGE, 
				SEX, 
				IMD_QUINTILE 
				FROM hes_appointments_2011
				WHERE AGE<85
				GROUP BY AGE, SEX, IMD_QUINTILE) c
				ON p.age=c.age AND p.sex=c.sex AND p.imd_quintile=c.imd_quintile)
				
				UNION
				
				(SELECT p.*, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE) p
				LEFT JOIN
				(SELECT COUNT(*) n, 
				85 AS AGE, 
				SEX, 
				IMD_QUINTILE 
				FROM hes_appointments_2011
				WHERE AGE>84
				GROUP BY SEX, IMD_QUINTILE) c
				ON p.age=c.age AND p.sex=c.sex AND p.imd_quintile=c.imd_quintile))
				ORDER BY IMD_QUINTILE, SEX, AGE"
		
		outpatient_table = dbGetQuery(con,outpatient_sql)

		dbDisconnect(con)
		
		lifetable_and_cost_table = list()
		lifetable_and_cost_table[["lifetable"]]=lifetable
		lifetable_and_cost_table[["cost_table"]]=cost_table
		lifetable_and_cost_table[["cost_table_imd"]]=cost_table_imd
		lifetable_and_cost_table[["cost_table_sex"]]=cost_table_sex
		lifetable_and_cost_table[["cost_table_overall"]]=cost_table_overall
		lifetable_and_cost_table[["emergency_cost_table"]]=emergency_cost_table
		lifetable_and_cost_table[["elective_cost_table"]]=elective_cost_table
		lifetable_and_cost_table[["outpatient_table"]]=outpatient_table
		
		save(lifetable_and_cost_table,file=filename)
	}
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

monte_carlo_integral = function(curve, a, b, n=10){
	set.seed(123)
	return(round(mean(curve[round(runif(n, a+1, b+1))])*(b-a),1))
}

# calculate the population standard error from the sub sample standard deviation
cost_standard_error = function(cost_table){
	return(sqrt(((cost_table$N/cost_table$POPULATION) * (cost_table$STDDEV_COST^2+(cost_table$TOTAL_COST/cost_table$N)^2) - (cost_table$TOTAL_COST/cost_table$POPULATION)^2))/sqrt(cost_table$POPULATION))
}

db_data = extract_data_from_db(cached=TRUE)

lifetable = db_data[["lifetable"]]
cost_table = db_data[["cost_table"]]
cost_table$SE_COST = cost_standard_error(cost_table)
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

###############################################################################
# Functions to make generic deprivation and sex plots 
###############################################################################

imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")

deprivation_plot = function(graph_data,xvar,yvar,xlab,ylab,filename,breaks=NULL,caption=NULL){
	plot = ggplot(graph_data, aes_string(x=xvar,y=yvar,group="IMD_QUINTILE",colour="IMD_QUINTILE",linetype="IMD_QUINTILE")) + 
			geom_line() + 
			facet_grid(. ~ SEX) + 
			xlab(xlab) +
			ylab(ylab) +
			scale_y_continuous(labels = comma) +
			scale_colour_manual(name="IMD Group", values=c("black","lightblue","lightgreen","lightgrey","darkgrey")) +
			scale_linetype_manual(name="IMD Group", values=c(1,2,2,2,1)) +
			theme_minimal(base_size = 14)
	if(!is.null(breaks)){
		plot = plot + scale_x_continuous(breaks=breaks)
	}
	if(!is.null(caption)){
		plot = 
				arrangeGrob(plot, 
						sub = textGrob(caption, x = 0, hjust = -0.1, vjust=0.1,
								gp = gpar(fontface = "italic", fontsize = 14)))
	}
	ggsave(filename=paste("output/",filename,".png",sep=""),plot=plot,width=27,height=10,units="cm",dpi=300)
}		

deprivation_panel = function(graph_data,xvar,yvar,xlab,ylab,filename,caption,breaks=NULL){
	plot = ggplot(graph_data, aes_string(x=xvar,y=yvar,group="IMD_QUINTILE",colour="IMD_QUINTILE",linetype="IMD_QUINTILE")) + 
			geom_line() + 
			facet_grid(FACET ~ SEX, scales="free_y") + 
			xlab(xlab) +
			ylab(ylab) +
			scale_y_continuous(labels = comma) +
			scale_colour_manual(name="IMD Group", values=c("black","lightblue","lightgreen","lightgrey","darkgrey")) +
			scale_linetype_manual(name="IMD Group", values=c(1,2,2,2,1)) +
			theme_minimal(base_size = 14)
	if(!is.null(breaks)){
		plot = plot + scale_x_continuous(breaks=breaks)
	}
	final_plot = 
			arrangeGrob(plot, 
					sub = textGrob(caption, x = 0, hjust = -0.1, vjust=0.1,
							gp = gpar(fontface = "italic", fontsize = 14)))
	
	ggsave(filename=paste("output/",filename,".png",sep=""),plot=final_plot,width=27,height=20,units="cm",dpi=300)
}		

sex_plot = function(graph_data,xvar,yvar,xlab,ylab,filename,breaks=NULL){
	plot = ggplot(graph_data, aes_string(x=xvar,y=yvar,group="SEX",colour="SEX",linetype="SEX")) + 
			geom_line() + 
			facet_wrap(~IMD_QUINTILE, nrow=2) + 
			xlab(xlab) +
			ylab(ylab) +
			scale_y_continuous(labels = comma) +
			scale_colour_manual(name="SEX", values=c("black","darkgrey")) +
			scale_linetype_manual(name="SEX", values=c(1,2)) +
			theme_minimal(base_size = 14)
	if(!is.null(breaks)){
		 plot = plot + scale_x_continuous(breaks=breaks)
	}
	
	ggsave(filename=paste("output/",filename,"2.png",sep=""),plot=plot,width=39,height=20,units="cm",dpi=300)
}		

###############################################################################
# Survival plots 
###############################################################################

survival_graph_data = mortality_rates %>%
		filter(AGE>49) %>%
		mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))

# make survival deprivation and sex plots
deprivation_plot(survival_graph_data,"AGE","SURVIVAL","Age","Probability of Survival","survival_curves")
sex_plot(survival_graph_data,"AGE","SURVIVAL","Age","Probability of Survival","survival_curves")

# survival plot for paper
deprivation_plot(survival_graph_data,"AGE","SURVIVAL","Age","Probability of Survival","survival_curves_panel",NULL,"Figure 2. Survival curves by sex and deprivation")

# make life expectancy plots
#life_expectancy = mortality_rates %>%
#					select(SEX,IMD_QUINTILE,SURVIVAL) %>%
#					group_by(SEX,IMD_QUINTILE) %>%
#					summarise(LIFE_EXPECTANCY=monte_carlo_integral(SURVIVAL,0,max_age,10000000)) %>%
#					ungroup() %>%
#					mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))

life_expectancy = mortality_rates %>% 
		group_by(SEX,IMD_QUINTILE) %>% 
		summarise(LIFE_EXPECTANCY=round(sum(SURVIVAL),1)) %>%
		ungroup() %>%
		mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))

le_dep_plot = ggplot(life_expectancy, aes(x=IMD_QUINTILE,y=LIFE_EXPECTANCY,fill=IMD_QUINTILE)) + 
		geom_bar(stat="identity",position="dodge") +
		geom_text(aes(y=LIFE_EXPECTANCY+1, label=formatC(LIFE_EXPECTANCY, format="f", digits=1)), vjust=0, size=4) +
		facet_grid(. ~ SEX) +  
		scale_fill_manual(name="IMD Group", values=c("black","lightgrey","lightgrey","lightgrey","darkgrey")) +
		scale_y_continuous(labels = comma) +
		xlab("Deprivation Group") +
		ylab("Life Expectancy at Birth") +
		theme_minimal(base_size = 14)

ggsave(filename="output/life_expectancy.png",plot=le_dep_plot,width=39,height=20,units="cm",dpi=300)

le_sex_plot = ggplot(life_expectancy, aes(x=SEX,y=LIFE_EXPECTANCY,fill=SEX)) + 
		geom_bar(stat="identity",position="dodge") +
		geom_text(aes(y=LIFE_EXPECTANCY+1, label=formatC(LIFE_EXPECTANCY, format="f", digits=1)), vjust=0, size=4) +
		facet_grid(. ~ IMD_QUINTILE) + 
		scale_fill_manual(name="Sex", values=c("black","darkgrey"), labels=c("Female","Male")) +
		scale_y_continuous(labels = comma) +
		xlab("Sex") +
		ylab("Life Expectancy at Birth") +
		theme_minimal(base_size = 14)

ggsave(filename="output/life_expectancy2.png",plot=le_sex_plot,width=39,height=20,units="cm",dpi=300)

###############################################################################
# Inpatient utilisation and cost plots 
###############################################################################

make_cost_plots = function(cost_table, prefix, caption){
	cost_graph_data = cost_table %>%
			mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))
	
	# make total hospital episodes deprivation and sex plots
	deprivation_plot(subset(cost_graph_data,AGE<85),"AGE","N","Age","Inpatient Episodes",paste(prefix,"hospitalisations",sep=""))
	sex_plot(subset(cost_graph_data,AGE<85),"AGE","N","Age","Inpatient Episodes",paste(prefix,"hospitalisations",sep=""))
	
	# make hospitalisation rate deprivation and sex plots
	deprivation_plot(cost_graph_data,"AGE","HOSP_EPI_RATE","Age","Inpatient Episodes per 100,000",paste(prefix,"hosp_curves",sep=""))
	sex_plot(cost_graph_data,"AGE","HOSP_EPI_RATE","Age","Inpatient Episodes per 100,000",paste(prefix,"hosp_curves",sep=""))
	
	panel_graph_data = cost_graph_data %>% 
			gather(FACET,VALUE,N,HOSP_EPI_RATE)
	panel_graph_data$FACET = factor(panel_graph_data$FACET, levels=c("N","HOSP_EPI_RATE"), labels=c("Total Hospital Episodes","Hospital Episodes per 100,000 Population"))
	deprivation_panel(subset(panel_graph_data,AGE<85),"AGE","VALUE","Age","",paste(prefix,"resource_use_panel",sep=""),caption)		
	
	# make cost deprivation and sex plots
	deprivation_plot(cost_graph_data,"AGE","AVERAGE_COST","Age","Average Annual Cost (\u00A3)",paste(prefix,"cost_curves",sep=""))
	sex_plot(cost_graph_data,"AGE","AVERAGE_COST","Age","Average Annual Cost (\u00A3)",paste(prefix,"cost_curves",sep=""))
}
make_cost_plots(cost_table,"","Figure 1. Hospital inpatient admissions by age, sex and deprivation (2011/12)")
make_cost_plots(emergency_cost_table,"emergency_","Figure A1. Emergency hospital inpatient admissions by age, sex and deprivation (2011/12)")
make_cost_plots(elective_cost_table,"elective_","Figure A2. Elective hospital inpatient admissions by age, sex and deprivation (2011/12)")

###############################################################################
# Outpatient utilisation plots 
###############################################################################

outpatient_graph_data = outpatient_table %>%
		mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))

# make total outpatient appointment deprivation and sex plots
deprivation_plot(subset(outpatient_graph_data,AGE<85),"AGE","N","Age","Outpatient Appointments",paste("appointments",sep=""))
sex_plot(subset(outpatient_graph_data,AGE<85),"AGE","N","Age","Outpatient Appointments",paste("appointments",sep=""))

# make appointment rate deprivation and sex plots
deprivation_plot(outpatient_graph_data,"AGE","HOSP_APPT_RATE","Age","Outpatient Appointments per 100,000",paste("appt_curves",sep=""))
sex_plot(outpatient_graph_data,"AGE","HOSP_APPT_RATE","Age","Outpatient Appointments per 100,000",paste("appt_curves",sep=""))

###############################################################################
# Expected cost plots for inpatient care - combining survival and average costs  
###############################################################################

make_expected_cost_plots = function(modelled_costs,prefix,caption){
	modelled_cost_graph_data = modelled_costs %>%
			mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))

	# make expected cost deprivation and sex plots
	deprivation_plot(modelled_cost_graph_data,"AGE","EXPECTED_COST","Age","Expected Cost (\u00A3)",paste(prefix,"expected_cost_curves",sep=""))
	sex_plot(modelled_cost_graph_data,"AGE","EXPECTED_COST","Age","Expected Cost (\u00A3)",paste(prefix,"expected_cost_curves",sep=""))

	# make cumulative cost deprivation and sex plots
	deprivation_plot(modelled_cost_graph_data,"AGE","CUMULATIVE_COST","Age","Cumulative Expected Cost (\u00A3)",paste(prefix,"cum_expected_cost_curves",sep=""))
	sex_plot(modelled_cost_graph_data,"AGE","CUMULATIVE_COST","Age","Cumulative Expected Cost (\u00A3)",paste(prefix,"cum_expected_cost_curves",sep=""))
	
	# make panel of both for paper
	panel_graph_data = modelled_cost_graph_data %>% 
			gather(FACET,VALUE,EXPECTED_COST,CUMULATIVE_COST)
	panel_graph_data$FACET = factor(panel_graph_data$FACET, levels=c("EXPECTED_COST","CUMULATIVE_COST"), labels=c("Expected Cost (\u00A3)","Cumulative Expected Cost (\u00A3)"))
	deprivation_panel(panel_graph_data,"AGE","VALUE","Age","",paste(prefix,"lifetime_cost_panel",sep=""),caption)		
	
	
	# make discounted expected cost deprivation and sex plots
	deprivation_plot(modelled_cost_graph_data,"AGE","DISCOUNTED_EXPECTED_COST","Age","Discounted Expected Cost (\u00A3)",paste(prefix,"discounted_expected_cost_curves",sep=""))
	sex_plot(modelled_cost_graph_data,"AGE","DISCOUNTED_EXPECTED_COST","Age","Dicounted Expected Cost (\u00A3)",paste(prefix,"discounted_expected_cost_curves",sep=""))

	# make discounted cumulative cost deprivation and sex plots
	deprivation_plot(modelled_cost_graph_data,"AGE","DISCOUNTED_CUMULATIVE_COST","Age","Discounted Cumulative Expected Cost (\u00A3)",paste(prefix,"discounted_cum_expected_cost_curves",sep=""))
	sex_plot(modelled_cost_graph_data,"AGE","DISCOUNTED_CUMULATIVE_COST","Age","Discounted Cumulative Expected Cost (\u00A3)",paste(prefix,"discounted_cum_expected_cost_curves",sep=""))
}

make_expected_cost_plots(modelled_costs,"","Figure 3. Hospital inpatient costs broken down by age, sex and deprivation")
make_expected_cost_plots(modelled_emergency_costs,"emergency_","Figure A3. Emergency hospitalisation costs broken down by age, sex and deprivation")
make_expected_cost_plots(modelled_elective_costs,"elective_","Figure A4. Elective hospitalisation costs broken down by age, sex and deprivation")

###############################################################################
# Explore population distribution  
###############################################################################
pop_graph_data_5_plus = lifetable %>%
		filter(MAX_AGE>4) %>%
		select(POPULATION,MAX_AGE,SEX,IMD_QUINTILE) %>%
		mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))

pop_graph_data_0_4 = lifetable %>%
		select(POPULATION,MAX_AGE,SEX,IMD_QUINTILE) %>%
		filter(MAX_AGE<5) %>%
		mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male"))) %>%
		group_by(IMD_QUINTILE, SEX) %>%
		summarise(POPULATION=sum(POPULATION)) %>%
		mutate(MAX_AGE=4) %>%
		ungroup()

pop_graph_data = bind_rows(pop_graph_data_0_4,pop_graph_data_5_plus)

population_plot = ggplot(pop_graph_data, aes(x=as.factor(MAX_AGE),y=POPULATION,fill=IMD_QUINTILE,alpha=IMD_QUINTILE)) + 
		geom_bar(stat="identity",position="dodge") +
		facet_grid(. ~ SEX) +  
		scale_fill_manual(name="IMD Group", values=c("black","lightgrey","lightgrey","lightgrey","darkgrey")) +
		scale_alpha_manual(name="IMD Group", values=c(1,0.2,0.2,0.2,1)) +
		scale_y_continuous(labels = comma) +
		xlab("Age Group") +
		ylab("Population") +
		theme_minimal(base_size = 14)

ggsave(filename="output/population.png",plot=population_plot,width=39,height=20,units="cm",dpi=300)

population_plot2 = ggplot(pop_graph_data, aes(x=as.factor(MAX_AGE),y=POPULATION,fill=SEX)) + 
		geom_bar(stat="identity",position="dodge") +
		facet_wrap(~IMD_QUINTILE, nrow=2) + 
		scale_fill_manual(name="Sex", values=c("black","darkgrey"), labels=c("Female","Male")) +
		scale_y_continuous(labels = comma) +
		xlab("Age Group") +
		ylab("Population") +
		theme_minimal(base_size = 14)

ggsave(filename="output/population2.png",plot=population_plot2,width=39,height=20,units="cm",dpi=300)

population_summary = pop_graph_data %>%
		mutate(SEX=as.character(SEX)) %>%
		group_by(IMD_QUINTILE,SEX) %>%
		summarise(POPULATION=sum(POPULATION)) %>%
		spread(IMD_QUINTILE, POPULATION) 

sink("output/population_summary.tex")
stargazer(population_summary,title="Population Summary",summary=FALSE,rownames=FALSE,font.size="tiny")
sink()

###############################################################################
# Population and cost projection   
###############################################################################

project_population = function(pop,p_mort,years){
	new_pop = pop*(1-p_mort)
	new_pop = c(0,new_pop[-length(new_pop)])
	return(new_pop)
}

cost_projection = cost_table %>% 
		group_by(IMD_QUINTILE,SEX) %>% 
		select(AGE,POPULATION,AVERAGE_COST) %>%
		right_join(mortality_rates, by=c("IMD_QUINTILE", "SEX", "AGE")) %>%
		mutate(DF=1/(1+discount_rate)^AGE) %>%
		mutate(POPULATION0=ifelse(is.na(POPULATION),0,POPULATION)) %>%
		mutate(AVERAGE_COST=ifelse(is.na(AVERAGE_COST),max(AVERAGE_COST,na.rm=TRUE),AVERAGE_COST), YEAR0_COST=AVERAGE_COST*POPULATION0) %>%
		mutate(POPULATION1=project_population(POPULATION0,PROB_MORT,1), YEAR1_COST=AVERAGE_COST*POPULATION1) %>%
		mutate(POPULATION2=project_population(POPULATION1,PROB_MORT,2), YEAR2_COST=AVERAGE_COST*POPULATION2) %>%
		mutate(POPULATION3=project_population(POPULATION2,PROB_MORT,3), YEAR3_COST=AVERAGE_COST*POPULATION3) %>%
		mutate(POPULATION4=project_population(POPULATION3,PROB_MORT,4), YEAR4_COST=AVERAGE_COST*POPULATION4) %>%
		mutate(POPULATION5=project_population(POPULATION4,PROB_MORT,5), YEAR5_COST=AVERAGE_COST*POPULATION5) %>%
		mutate(POPULATION6=project_population(POPULATION5,PROB_MORT,6), YEAR6_COST=AVERAGE_COST*POPULATION6) %>%
		mutate(POPULATION7=project_population(POPULATION6,PROB_MORT,7), YEAR7_COST=AVERAGE_COST*POPULATION7) %>%
		mutate(POPULATION8=project_population(POPULATION7,PROB_MORT,8), YEAR8_COST=AVERAGE_COST*POPULATION8) %>%
		mutate(POPULATION9=project_population(POPULATION8,PROB_MORT,9), YEAR9_COST=AVERAGE_COST*POPULATION9) %>%
		mutate(POPULATION10=project_population(POPULATION9,PROB_MORT,10), YEAR10_COST=AVERAGE_COST*POPULATION10) %>%
		mutate(POPULATION11=project_population(POPULATION10,PROB_MORT,11), YEAR11_COST=AVERAGE_COST*POPULATION11) %>%
		mutate(POPULATION12=project_population(POPULATION11,PROB_MORT,12), YEAR12_COST=AVERAGE_COST*POPULATION12) %>%
		mutate(POPULATION13=project_population(POPULATION12,PROB_MORT,13), YEAR13_COST=AVERAGE_COST*POPULATION13) %>%
		mutate(POPULATION14=project_population(POPULATION13,PROB_MORT,14), YEAR14_COST=AVERAGE_COST*POPULATION14) %>%
		mutate(POPULATION15=project_population(POPULATION14,PROB_MORT,15), YEAR15_COST=AVERAGE_COST*POPULATION15) %>%
		mutate(POPULATION16=project_population(POPULATION15,PROB_MORT,16), YEAR16_COST=AVERAGE_COST*POPULATION16) %>%
		mutate(POPULATION17=project_population(POPULATION16,PROB_MORT,17), YEAR17_COST=AVERAGE_COST*POPULATION17) %>%
		mutate(POPULATION18=project_population(POPULATION17,PROB_MORT,18), YEAR18_COST=AVERAGE_COST*POPULATION18) %>%
		mutate(POPULATION19=project_population(POPULATION18,PROB_MORT,19), YEAR19_COST=AVERAGE_COST*POPULATION19)

population_projection_0_84 = cost_projection[,!grepl("COST",names(cost_projection))] %>%
						select(-SURVIVAL,-POPULATION,-PROB_MORT,-DF) %>%
						filter(AGE<85) %>%
						ungroup() %>%
						mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))
				
population_projection_85_plus = cost_projection[,!grepl("COST",names(cost_projection))] %>%
		select(-SURVIVAL,-POPULATION,-PROB_MORT,-DF) %>%
		filter(AGE>84) %>%
		summarise_each(funs(sum)) %>%
		mutate(AGE=85) %>%
		ungroup() %>%
		mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))

population_projection = bind_rows(population_projection_0_84,population_projection_85_plus)								

###############################################################################
# Produce graphs for population projection animation  
###############################################################################
for(i in 0:19){
	pop = paste("POPULATION",i,sep="")
	graph_data = population_projection %>%
					select_("AGE","POP"=pop,"IMD_QUINTILE","SEX")
	total_data = population_projection %>%
			select_("POP"=pop,"IMD_QUINTILE","SEX") %>%
			group_by(IMD_QUINTILE,SEX) %>%
			summarise(total=sum(POP))
	pop_plot = ggplot(graph_data, aes(x=AGE,y=POP)) +
			geom_density(stat="identity", fill=paste("gray",i*4,sep=""), alpha=0.5) +
			geom_text(data=total_data,x=40,y=250000,aes(label=format(round(total), big.mark=",", scientific=FALSE)),size=3) +
			facet_grid(SEX ~ IMD_QUINTILE) + 		
			ylab(paste("Projected Population in ",2011+i,sep="")) +
			xlab(paste("Age in ",2011+i,sep="")) +
			scale_y_continuous(labels = comma, limits=c(0,276000)) +
			theme_minimal()
	ggsave(filename=paste("output/population_projection/projected_population_",i,".pdf",sep=""),plot=pop_plot,width=35,height=25,units="cm",dpi=300)
}

###############################################################################
# Aggregate projection figures to get annual and cumulative estimates  
###############################################################################

annual_cost_projection = cost_projection %>%
		summarise(pop=sum(POPULATION0),year1=sum(YEAR0_COST),year2=sum(YEAR1_COST),year3=sum(YEAR2_COST),year4=sum(YEAR3_COST),year5=sum(YEAR4_COST),year6=sum(YEAR5_COST),year7=sum(YEAR6_COST),year8=sum(YEAR7_COST),year9=sum(YEAR8_COST),year10=sum(YEAR9_COST),
				year11=sum(YEAR10_COST),year12=sum(YEAR11_COST),year13=sum(YEAR12_COST),year14=sum(YEAR13_COST),year15=sum(YEAR14_COST),year16=sum(YEAR15_COST),year17=sum(YEAR16_COST),year18=sum(YEAR17_COST),year19=sum(YEAR18_COST),year20=sum(YEAR19_COST)) %>%
		ungroup() %>%
		gather(year,cost,year1:year20) %>%
		mutate(year=as.numeric(substr(year,5,length(year))), unit_cost=round(cost/pop,2),
				IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))

# make annual cost projection deprivation and sex plots
deprivation_plot(annual_cost_projection,"year","cost","Years from 2011/12","Annual Projected Cost (\u00A3)","annual_projected_cost_curves")
sex_plot(annual_cost_projection,"year","cost","Years from 2011/12","Annual Projected Cost (\u00A3)","annual_projected_cost_curves")

# make annual average cost projection deprivation and sex plots
deprivation_plot(annual_cost_projection,"year","unit_cost","Years from 2011/12","Annual Projected Average Cost (\u00A3)","annual_projected_average_cost_curves")
sex_plot(annual_cost_projection,"year","unit_cost","Years from 2011/12","Annual Projected Average Cost (\u00A3)","annual_projected_average_cost_curves")

discounted_annual_cost_projection = cost_projection %>%
		summarise(pop=sum(POPULATION0),year1=sum(YEAR0_COST*DF),year2=sum(YEAR1_COST*DF),year3=sum(YEAR2_COST*DF),year4=sum(YEAR3_COST*DF),year5=sum(YEAR4_COST*DF),year6=sum(YEAR5_COST*DF),year7=sum(YEAR6_COST*DF),year8=sum(YEAR7_COST*DF),year9=sum(YEAR8_COST*DF),year10=sum(YEAR9_COST*DF),
				year11=sum(YEAR10_COST*DF),year12=sum(YEAR11_COST*DF),year13=sum(YEAR12_COST*DF),year14=sum(YEAR13_COST*DF),year15=sum(YEAR14_COST*DF),year16=sum(YEAR15_COST*DF),year17=sum(YEAR16_COST*DF),year18=sum(YEAR17_COST*DF),year19=sum(YEAR18_COST*DF),year20=sum(YEAR19_COST*DF)) %>%
		ungroup() %>%
		gather(year,cost,year1:year20) %>%
		mutate(year=as.numeric(substr(year,5,length(year))), unit_cost=round(cost/pop,2),
				IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))

# make discounted annual cost projection deprivation and sex plots
deprivation_plot(discounted_annual_cost_projection,"year","cost","Years from 2011/12","Discounted Annual Projected Cost (\u00A3)","discounted_annual_projected_cost_curves")
sex_plot(discounted_annual_cost_projection,"year","cost","Years from 2011/12","Discounted Annual Projected Cost (\u00A3)","discounted_annual_projected_cost_curves")

# make discounted annual average cost projection deprivation and sex plots
deprivation_plot(discounted_annual_cost_projection,"year","unit_cost","Years from 2011/12","Discounted Annual Projected Average Cost (\u00A3)","discounted_annual_projected_average_cost_curves")
sex_plot(discounted_annual_cost_projection,"year","unit_cost","Years from 2011/12","Discounted Annual Projected Average Cost (\u00A3)","discounted_annual_projected_average_cost_curves")

cum_cost_projection = cost_projection %>%
		mutate(cum0=YEAR0_COST,cum1=cum0+YEAR1_COST,cum2=cum1+YEAR2_COST,cum3=cum2+YEAR3_COST,cum4=cum3+YEAR4_COST,cum5=cum4+YEAR5_COST,cum6=cum5+YEAR6_COST,cum7=cum6+YEAR7_COST,cum8=cum7+YEAR8_COST,cum9=cum8+YEAR9_COST,cum10=cum9+YEAR10_COST,cum11=cum10+YEAR11_COST,cum12=cum11+YEAR12_COST,cum13=cum12+YEAR13_COST,cum14=cum13+YEAR14_COST,cum15=cum14+YEAR15_COST,cum16=cum15+YEAR16_COST,cum17=cum16+YEAR17_COST,cum18=cum17+YEAR18_COST,cum19=cum18+YEAR19_COST) %>%
		summarise(pop=sum(POPULATION0),year1=sum(cum0),year2=sum(cum1),year3=sum(cum2),year4=sum(cum3),year5=sum(cum4),year6=sum(cum5),year7=sum(cum6),year8=sum(cum7),year9=sum(cum8),year10=sum(cum9),
				year11=sum(cum10),year12=sum(cum11),year13=sum(cum12),year14=sum(cum13),year15=sum(cum14),year16=sum(cum15),year17=sum(cum16),year18=sum(cum17),year19=sum(cum18),year20=sum(cum19)) %>%
		ungroup() %>%
		gather(year,cost,year1:year20) %>%
		mutate(year=as.numeric(substr(year,5,length(year))), unit_cost=round(cost/pop,2),
				IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))

# make cumulative cost projection deprivation and sex plots
deprivation_plot(cum_cost_projection,"year","cost","Years from 2011/12","Cumulative Projected Cost (\u00A3)","cum_projected_cost_curves")
sex_plot(cum_cost_projection,"year","cost","Years from 2011/12","Cumulative Projected Cost (\u00A3)","cum_projected_cost_curves")

# make cumulative average cost projection deprivation and sex plots
deprivation_plot(cum_cost_projection,"year","unit_cost","Years from 2011/12","Cumulative Projected Average Cost (\u00A3)","cum_projected_average_cost_curves")
sex_plot(cum_cost_projection,"year","unit_cost","Years from 2011/12","Cumulative Projected Average Cost (\u00A3)","cum_projected_average_cost_curves")

discounted_cum_cost_projection = cost_projection %>%
		mutate(cum0=YEAR0_COST*DF,cum1=cum0+YEAR1_COST*DF,cum2=cum1+YEAR2_COST*DF,cum3=cum2+YEAR3_COST*DF,cum4=cum3+YEAR4_COST*DF,cum5=cum4+YEAR5_COST*DF,cum6=cum5+YEAR6_COST*DF,cum7=cum6+YEAR7_COST*DF,cum8=cum7+YEAR8_COST*DF,cum9=cum8+YEAR9_COST*DF,cum10=cum9+YEAR10_COST*DF,cum11=cum10+YEAR11_COST*DF,cum12=cum11+YEAR12_COST*DF,cum13=cum12+YEAR13_COST*DF,cum14=cum13+YEAR14_COST*DF,cum15=cum14+YEAR15_COST*DF,cum16=cum15+YEAR16_COST*DF,cum17=cum16+YEAR17_COST*DF,cum18=cum17+YEAR18_COST*DF,cum19=cum18+YEAR19_COST*DF) %>%
		summarise(pop=sum(POPULATION0),year1=sum(cum0),year2=sum(cum1),year3=sum(cum2),year4=sum(cum3),year5=sum(cum4),year6=sum(cum5),year7=sum(cum6),year8=sum(cum7),year9=sum(cum8),year10=sum(cum9),
				year11=sum(cum10),year12=sum(cum11),year13=sum(cum12),year14=sum(cum13),year15=sum(cum14),year16=sum(cum15),year17=sum(cum16),year18=sum(cum17),year19=sum(cum18),year20=sum(cum19)) %>%
		ungroup() %>%
		gather(year,cost,year1:year20) %>%
		mutate(year=as.numeric(substr(year,5,length(year))), unit_cost=round(cost/pop,2),
				IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))

# make cumulative cost projection deprivation and sex plots
deprivation_plot(discounted_cum_cost_projection,"year","cost","Years from 2011/12","Discounted Cumulative Projected Cost (\u00A3)","discounted_cum_projected_cost_curves")
sex_plot(discounted_cum_cost_projection,"year","cost","Years from 2011/12","Discounted Cumulative Projected Cost (\u00A3)","discounted_cum_projected_cost_curves")

# make cumulative average cost projection deprivation and sex plots
deprivation_plot(discounted_cum_cost_projection,"year","unit_cost","Years from 2011/12","Discounted Cumulative Projected Average Cost (\u00A3)","discounted_cum_projected_average_cost_curves")
sex_plot(discounted_cum_cost_projection,"year","unit_cost","Years from 2011/12","Discounted Cumulative Projected Average Cost (\u00A3)","discounted_cum_projected_average_cost_curves")

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

sink("output/total_cost_of_ineq.tex")
	stargazer(total_cost_of_ineq_table,title="Total cost of inequality - assuming everybody could potentially have the costs of the most affluent",summary=FALSE,rownames=FALSE,font.size="tiny")
sink()
