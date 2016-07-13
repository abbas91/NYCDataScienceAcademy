#Q1---------
#1
library(ggplot2)
library(dplyr)
data(mpg)
#2
data = transmute(mpg,V1=year,V2=cyl,V3=cty,V4=hwy)
#3
by_cylinder = data %>% group_by(V2) %>% summarise(city_mpg=mean(V3),hwy_mpg=mean(V4))
#4
by_city_mpg = mpg %>% group_by(manufacturer)  %>% filter(cty==max(cty))

#Q2----------
#1
mpg = mutate(mpg, ratioHVE= hwy / displ)
#2
mpg = mutate(mpg, ratioCVE= cty / displ)
#3
mpg %>% group_by(year, manufacturer) %>% summarise(avgHVE=mean(ratioHVE), avgCVE=mean(ratioCVE))
#4
mpg %>% group_by(year,drv) %>% summarise(mean(ratioHVE))

#Q3----------
jobs <- read.csv('NYC_Jobs.csv',stringsAsFactors = F)
#1
#function to annualize salaries
Annualize = function (salary, type) {
  ifelse(type == 'Daily', salary*5*52, ifelse(type == 'Hourly', salary*8*5*52, salary))
}
#add new salary columns 
parsed_jobs =  mutate(jobs, Agency, Posting.Type, X..Of.Positions, Level, Civil.Service.Title,
                      Salary_From=Annualize(Salary.Range.From,Salary.Frequency), 
                      Salary_To=Annualize(Salary.Range.To,Salary.Frequency)) 
#group by agency and summarises means of high and low salaries
salaries = parsed_jobs %>% group_by(Agency) %>% summarise(mean_salary_low=mean(Salary_From),
                                                          median_salary_low=median(Salary_From),
                                                          mean_salary_high=mean(Salary_To),
                                                          median_salary_high=median(Salary_To))
#2
arrange(salaries, desc(mean_salary_low))
#3
parsed_jobs %>% group_by(Posting.Type) %>% summarise(avg_salary_range=abs(mean(Salary_From) - mean(Salary_To)))
#4
parsed_jobs %>% group_by(Level) %>% summarise(avg_salary_range=abs(mean(Salary_From) - mean(Salary_To)))
#5
ranges = transmute(parsed_jobs, Agency, Civil.Service.Title, salary_range=(Salary_To-Salary_From),budget_range=(salary_range*X..Of.Positions)) 
ranges %>% group_by(Agency) %>% select(Agency, budget_range)
#6
ranges %>% group_by(Civil.Service.Title) %>% arrange(desc(salary_range)) %>% select(-budget_range)


