## Prep work 
rm(list=ls())
# Set working directory 
# setwd("~/Desktop/ECON32211 Applied Dissertation A/11.22 R Project")
# Load packages
library(haven)
library(dplyr)
library(tidyverse)
library(reporttools)
library(stargazer)
library(ggplot2)
library(extrafont)
library(quantreg)
# Load data 
wage <- read_dta("Project_A.dta")
deflator <- read_dta("Project_A_Deflator.dta")

## A glimpse into the data
# Wage data
# View(wage);str(wage);summary(wage)
subset(wage, is.na(wage$w_hr)) %>% summary(wage)
# Deflator data
str(deflator)


## Tidy the data
# Rename the column and convert some into factors
wage1 <- wage
wage1[,c(1,2,4)] <- lapply(wage1[,c(1,2,4)],as.factor)
colnames(wage1) <- c("Gender", "Race", "Age", "Education", "Year", "Hourly_wage")
# Check and Rearrange the level of factors
levels(wage1$Gender);levels(wage1$Race);levels(wage1$Education)
wage1$Gender <- factor(wage1$Gender, levels=c("Male","Female"))
wage1$Education <- factor(wage1$Education, 
                           levels=c("HS dropout", "HS grad", "Some college/associate degree", "Bachelor's degree or higher"))
# Drop NAs
wage1 <- wage1[!is.na(wage1$Hourly_wage),]



## Q1
# Merge in the price deflator and deflate all the appropriate variables
## Compute the real wages
colnames(deflator) <- c("Year", "deflator09")
wage1 <-  merge(wage1,deflator,by="Year") %>%
        mutate(Real_wage=Hourly_wage/deflator09)
wage1 <- wage1[,-7]
# Q1 Table
# summarize numeric variables
tableContinuous(wage1[,sapply(wage1, is.numeric)], stats=c("n","mean","median","max","min","na"))
# summarize non-numeric variables
tableNominal(wage1[,!sapply(wage1, is.numeric)])



## Q2
# Calculate and report the 90th, 50th, 10th percentile of real wages 2012,
# and 90/10 and 50/10 ratio
options(digits=3)
quantile(wage1[wage1$Year==2012,]$`Real_wage`,c(0.9,0.5,0.1))
q2 <- as.vector(unname(quantile(wage1[wage1$Year==2012,]$Real_wage,c(0.9,0.5,0.1))))
q2_9V1 <- q2[1]/q2[3]
q2_5V1 <- q2[2]/q2[3]


## Q3
# Calculate and report the fraction of workers with at least some university education in 2012
wage1$least_uni<-ifelse(
        wage1$Education=="Bachelor's degree or higher"|
        wage1$Education=="Some college/associate degree",1,0)
sum(wage1[wage1$Year==2012,]$least_uni)/nrow(wage1[wage1$Year==2012,])

## Q4
# Consider the real wage distributions for men and women separately in a table and a figure
# Table
q4t <- wage1%>%
        group_by(Gender) %>%
        summarise(Wage=quantile((Real_wage), c(1,0.9,0.75,0.5,0.25,0.1,0))) %>% 
        as.matrix() %>% t()
q4t1 <- as.data.frame(matrix(rev(q4t[2,]),ncol=7,nrow=2,byrow=T))
colnames(q4t1) <- c("Min","10th","25th","50th","75th","90th","Max")
rownames(q4t1) <- c("Female","Male")
xtable(q4t1)

# Q4Graph
q4p <- ggplot(wage1) +
        aes(x = Real_wage, fill = Gender, colour = Gender) +
        geom_density(alpha=0.4,size=1) +
        scale_fill_manual(values = c(Male = "#076FA1", Female = "#E3120B")) +
        scale_color_manual(values = c(Male = "#076FA1", Female = "#E3120B"))

q4p <- q4p + scale_x_continuous( limits = c(0, 110),expand = c(0, 0),breaks = seq(0,110,10),)+
        scale_y_continuous( limits = c(0, 0.065),expand = c(0, 0),breaks = seq(0,0.065,0.01))+
        labs(x = "Real Wage", y = "Density", title = "1990-2012 Real Wage Distribution by Gender") +
        theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5), 
              axis.title.y = element_text(size = 13L, face = "bold"), 
              axis.title.x = element_text(size = 13L,  face = "bold"))+
        ggthemes::theme_calc()

## Q5
# For each year in the dataset,
# (1) 90, 50, 10 percentile of wages
# (2) Fraction of male and female workers at least some university education

## (1)
q5ta <- wage1 %>% 
        group_by(Year) %>% 
        summarise(Wage=quantile((Real_wage), c(0.9,0.5,0.1))) %>% 
        as.data.frame()
q5ta$Percentile <- rep(c("90th","50th","10th"),23)

# Q5(1)Graph
q5p1 <-  ggplot(q5ta) +aes(x = Year,y = Wage,fill = Year,colour = Percentile,group = Percentile) +
        geom_line(size = 1) +
        scale_color_manual(values = c("90th" = "#076FA1","50th"="#2FC1D3", "10th" = "#E3120B"))
q5p1 <- q5p1+scale_x_continuous( limits = c(1990, 2012),expand = c(0, 0),breaks = seq(1990,2012,2),)+
        scale_y_continuous( limits = c(0, 50),expand = c(0, 0),breaks = seq(0,50,5))+
        labs(x = "Year", y = "Wage", title = "1990-2012 Trend of Wage at Different Percentile") +
        theme(plot.title = element_text(size = 15L,  face = "bold", hjust = 0.5), 
              axis.title.y = element_text(size = 13L, face = "bold"),
              axis.title.x = element_text(size = 13L, face = "bold"))+
        ggthemes::theme_calc()

## (2)
q5tb1 <- wage1[wage1$Gender=="Male",] %>% 
        group_by(Year) %>%
        summarise(Some_uni=mean(least_uni)) %>% 
        as.data.frame()
q5tb2 <- wage1[wage1$Gender=="Female",] %>% 
        group_by(Year) %>%
        summarise(Some_uni=mean(least_uni)) %>% 
        as.data.frame()
q5tb <-rbind(q5tb1,q5tb2)
q5tb$Gender <- c(rep("Male",23),rep("Female",23))

# Q5(2)Graph
q5p2 <-ggplot(q5tb) +aes(x = Year,y = Some_uni,fill = Year,colour = Gender,group = Gender) +
        geom_line(size = 1)+
        scale_color_manual(values = c(Male = "#076FA1", Female = "#E3120B"))
q5p2 <- q5p2+scale_x_continuous( limits = c(1990, 2012),expand = c(0, 0),breaks = seq(1990,2012,2),)+
        scale_y_continuous( limits = c(0.3, 0.8),expand = c(0, 0),breaks = seq(0.3,0.8,0.05))+
        labs(x = "Year", y = "Percentage of people who have some Uni education", 
             title = "1990-2012 Trend of People's University Particpation") +
        theme(plot.title = element_text(size = 15L,  face = "bold", hjust = 0.5), 
              axis.title.y = element_text(size = 13L, face = "bold"),
              axis.title.x = element_text(size = 13L, face = "bold"))+
        ggthemes::theme_calc()

## Q6
# Estimate the gender wage gap controlling for education
# OLS,control for Education
lm6.1 <- lm(I(log(Real_wage)) ~ Gender, data=wage1)
lm6.2 <- lm(I(log(Real_wage)) ~ Gender+least_uni, data=wage1)
lm6.3 <- lm(I(log(Real_wage)) ~ Gender*least_uni, data=wage1)

stargazer(lm6.1,lm6.2,lm6.3,type="text")
stargazer(lm6.1,lm6.2,lm6.3,type="latex")
# Quantile Regression, control for Education
qr6.3.1 <- rq(I(log(Real_wage)) ~ Gender*least_uni, data=wage1, tau=0.1)
qr6.3.2 <- rq(I(log(Real_wage)) ~ Gender*least_uni, data=wage1, tau=0.5)
qr6.3.3 <- rq(I(log(Real_wage)) ~ Gender*least_uni, data=wage1, tau=0.9)

stargazer(qr6.3.1,qr6.3.2,qr6.3.3, type="text")
stargazer(qr6.3.1,qr6.3.2,qr6.3.3, type="latex")

## Q7
# Does the gender wage gap change over time
# OLS control education
q7t1 <- as.data.frame(matrix(rep(0,69),23,3))
for(i in 1990:2012){
        k=i-1989
        q7t1[k,1] <- i
        q7t1[k,2] <- summary(lm(I(log(Real_wage)) ~ Gender+least_uni, data=wage1[wage1$Year==i,]))$coeff[2,1]
        q7t1[k,3] <- summary(lm(I(log(Real_wage)) ~ Gender+least_uni, data=wage1[wage1$Year==i,]))$coeff[3,1]
        
}

#Q7 Graph 1,2
q7p1 <- ggplot(q7t1) +
        aes(x = V1, y = V2) + geom_point(shape = "circle", size = 2.4,color="#076FA1") +
        geom_smooth(span = 1, colour = "#E3120B")
q7p1 <- q7p1+scale_x_continuous( limits = c(1990, 2012),expand = c(0, 0),breaks = seq(1990,2012,2),)+
        scale_y_continuous( limits = c(-0.4, -0.1),expand = c(0, 0),breaks = seq(-0.4, -0.1,0.05))+ 
        labs( x = "Year",y = "Coefficient of Gender",title = "1990-2012 Wage Gap Gender",
              subtitle = "Method: OLS Control for Education") +ggthemes::theme_calc()

q7p2 <- ggplot(q7t1) +
        aes(x = V1, y = V3) + geom_point(shape = "circle", size = 2.4,color="#076FA1") +
        geom_smooth(span = 1, colour = "#E3120B")
q7p2 <- q7p2+scale_x_continuous( limits = c(1990, 2012),expand = c(0, 0),breaks = seq(1990,2012,2),)+
        scale_y_continuous( limits = c(0.2, 0.65),expand = c(0, 0),breaks = seq(0.2, 0.65,0.05))+ 
        labs( x = "Year",y = "Coefficient of Education",title = "1990-2012 Wage Gap Gender",
              subtitle = "Method: OLS Control for Education") +ggthemes::theme_calc()

# Quantile Regression, control for Education
q7t2a <- as.data.frame(matrix(rep(0,161),23,7))
for(i in 1990:2012){
        k=i-1989
        q7t2a[k,1] <- i
        q7t2a[k,2] <- as.numeric(rq(I(log(Real_wage)) ~ Gender+least_uni, data=wage1[wage1$Year==i,], tau=0.1)$coeff[2])
        q7t2a[k,3] <- as.numeric(rq(I(log(Real_wage)) ~ Gender+least_uni, data=wage1[wage1$Year==i ,], tau=0.5)$coeff[2])
        q7t2a[k,4] <-as.numeric(rq(I(log(Real_wage)) ~ Gender+least_uni, data=wage1[wage1$Year==i,], tau=0.9)$coeff[2])
        q7t2a[k,5] <- as.numeric(rq(I(log(Real_wage)) ~ Gender+least_uni, data=wage1[wage1$Year==i,], tau=0.2)$coeff[3])
        q7t2a[k,6] <- as.numeric(rq(I(log(Real_wage)) ~ Gender+least_uni, data=wage1[wage1$Year==i ,], tau=0.5)$coeff[3])
        q7t2a[k,7] <-as.numeric(rq(I(log(Real_wage)) ~ Gender+least_uni, data=wage1[wage1$Year==i,], tau=0.9)$coeff[3])
}
colnames(q7t2a) <- c("Year", rep(c("Coef"),6))
q7t2ag <- rbind(q7t2a[,c(1,2)],q7t2a[,c(1,3)],q7t2a[,c(1,4)])
q7t2ag$Percentile <- c(rep("10th",23),rep("50th",23),rep("90th",23))

# Graph
q7p3 <- ggplot(q7t2ag) +
        aes(x = Year, y = Coef, colour = Percentile) +geom_point(shape = "circle", size = 1.85) +
        geom_smooth(span = 1) + scale_color_manual(values = c("90th" = "#076FA1","50th"="#2FC1D3", "10th" = "#E3120B"))
q7p3 <- q7p3+scale_x_continuous( limits = c(1990, 2012),expand = c(0, 0),breaks = seq(1990,2012,2),)+
        scale_y_continuous(labels = scales::comma)+ ggthemes::theme_calc()+
        labs(x = "Year", y = "Coefficent of Gender",title = "1990-2012 Gender Wage Gap at Different wage Percentile" ,
             subtitle = "Method: Quantile Regression Control for Education")


# Quantile Regression, control for Education by Gender
# Male
q7t3m <- as.data.frame(matrix(rep(0,92),23,4))
wagem <- wage1[wage1$Gender=="Male",]
for(i in 1990:2012){
        k=i-1989
        q7t3m[k,1] <- i
        q7t3m[k,2] <- as.numeric(rq(I(log(Real_wage)) ~ least_uni, data=wagem[wagem$Year==i,], tau=0.1)$coeff[2])
        q7t3m[k,3] <- as.numeric(rq(I(log(Real_wage)) ~ least_uni, data=wagem[wagem$Year==i ,], tau=0.5)$coeff[2])
        q7t3m[k,4] <-as.numeric(rq(I(log(Real_wage)) ~ least_uni, data=wagem[wagem$Year==i,], tau=0.9)$coeff[2])
}
colnames(q7t3m) <- c("Year", rep(c("Coef"),3))
q7t3gm <- rbind(q7t3m[,c(1,2)],q7t3m[,c(1,3)],q7t3m[,c(1,4)])
q7t3gm$Percentile <- c(rep("10th",23),rep("50th",23),rep("90th",23))

# Graph Male
q7p4 <- ggplot(q7t3gm) +
        aes(x = Year, y = Coef, colour = Percentile) +geom_point(shape = "circle", size = 1.85) +
        geom_smooth(span = 1) + scale_color_manual(values = c("90th" = "#076FA1","50th"="#2FC1D3", "10th" = "#E3120B"))
q7p4 <- q7p4+scale_x_continuous( limits = c(1990, 2012),expand = c(0, 0),breaks = seq(1990,2012,2),)+
        scale_y_continuous(limits = c(0, 0.8),expand = c(0, 0),breaks = seq(0, 0.8,0.1),labels = scales::comma)+
        ggthemes::theme_calc()+
        labs(x = "Year", y = "Coefficent of Education",title = "1990-2012 Return to Male Uni Attendence at Different wage Percentile" ,
             subtitle = "Method: Quantile Regression Control for Education(Male only)")

q7t3f <- as.data.frame(matrix(rep(0,92),23,4))

# Female
wagef <- wage1[wage1$Gender=="Female",]
for(i in 1990:2012){
        k=i-1989
        q7t3f[k,1] <- i
        q7t3f[k,2] <- as.numeric(rq(I(log(Real_wage)) ~ least_uni, data=wagef[wagef$Year==i,], tau=0.1)$coeff[2])
        q7t3f[k,3] <- as.numeric(rq(I(log(Real_wage)) ~ least_uni, data=wagef[wagef$Year==i ,], tau=0.5)$coeff[2])
        q7t3f[k,4] <-as.numeric(rq(I(log(Real_wage)) ~ least_uni, data=wagef[wagef$Year==i,], tau=0.9)$coeff[2])
}
colnames(q7t3f) <- c("Year", rep(c("Coef"),3))
q7t3gf <- rbind(q7t3f[,c(1,2)],q7t3f[,c(1,3)],q7t3f[,c(1,4)])
q7t3gf$Percentile <- c(rep("10th",23),rep("50th",23),rep("90th",23))

# Graph Female
q7p5 <- ggplot(q7t3gf) +
        aes(x = Year, y = Coef, colour = Percentile) +geom_point(shape = "circle", size = 1.85) +
        geom_smooth(span = 1) + scale_color_manual(values = c("90th" = "#076FA1","50th"="#2FC1D3", "10th" = "#E3120B"))
q7p5 <- q7p5+scale_x_continuous( limits = c(1990, 2012),expand = c(0, 0),breaks = seq(1990,2012,2),)+
        scale_y_continuous(limits = c(0, 0.8),expand = c(0, 0),breaks = seq(0, 0.8,0.1),labels = scales::comma)+ ggthemes::theme_calc()+
        labs(x = "Year", y = "Coefficent of Education",title = "1990-2012 Return to Female Uni Attendence at Different wage Percentile" ,
             subtitle = "Method: Quantile Regression Control for Education(Female only)") 

# Export the graphs
# q4p,q5p1,q5p2,q7p1,q7p2,q7p3,q7p4,q7p5
# pdf("q4p.png", height=6, width=8)
# print(q4p)
# dev.off()
# 
# pdf("q5p1.pdf", height=6, width=8)
# print(q5p1)
# dev.off()
# 
# pdf("q5p2.pdf", height=6, width=8)
# print(q5p2)
# dev.off()
# 
# pdf("q7p1.pdf", height=2, width=3)
# print(q7p1)
# dev.off()
# 
# pdf("q7p2.pdf", height=2, width=3)
# print(q7p2)
# dev.off()
# 
# pdf("q7p3.pdf", height=6,width=8)
# print(q7p3)
# dev.off()
# 
# pdf("q7p4.pdf", height=6, width=8)
# print(q7p4)
# dev.off()
# 
# pdf("q7p5.pdf", height=6, width=8)
# print(q7p5)
# dev.off()

