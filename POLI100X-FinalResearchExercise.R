#installed package so R can read the excel dataset
install.packages("readxl")
library(readxl)

install.packages("lfe")
library(lfe)

#imported dataset into R
House<- read_excel("CELHouse93to115.xlsx")

summary(House$`Legislative Effectiveness Score (1-5-10)`)

lm(House$`Legislative Effectiveness Score (1-5-10)`~ House$`Congress number`, data=House)


##1: “What affects a member’s legislative effectiveness in Congress?”
fit <-lm(House$`Legislative Effectiveness Score (1-5-10)`~ House$`state_leg * state legislature professionalism` + House$`1 = majority party member` 
      + House$`1 = female` + House$`1 = African American` + House$`1 = Member of Appropriation, Rules, or Ways and Means`, data=House)
summary(fit) 

fit4 <-lm(House$`Legislative Effectiveness Score (1-5-10)`~ House$`1 = served in state legislature` + House$`1 = majority party member` 
         + House$`1 = female` + House$`1 = African American` + House$`1 = Member of Appropriation, Rules, or Ways and Means` + House$`Seniority, number of terms served counting current`, data=House)
summary(fit4)


##2: “How does previous experience in a state legislature affect a member’s legislative effectiveness in their FIRST term in the house?”
fit2 <- felm(House$`Legislative Effectiveness Score (1-5-10)`~ House$`state_leg * state legislature professionalism` + House$`1 = majority party member` 
                      + House$`1 = female` + House$`1 = African American` + House$`1 = Member of Appropriation, Rules, or Ways and Means`
             |factor(House$`Year at start of Congress`) + factor(House$`ICPSR number, according to Poole and Rosenthal`), data=House)

#control for unobserved variables in a specific year

#cont
summary(fit2)

fit3 <- felm(House$`Legislative Effectiveness Score (1-5-10)`~ House$`1 = served in state legislature` + House$`1 = majority party member` 
             + House$`1 = female` + House$`1 = African American` + House$`1 = Member of Appropriation, Rules, or Ways and Means`
             |factor(House$`Year at start of Congress`) + factor(House$`ICPSR number, according to Poole and Rosenthal`), data=House)
summary(fit3)




