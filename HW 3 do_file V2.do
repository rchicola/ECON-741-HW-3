*Call the directory where your file is located. cd command. Replace the portion
*in-quotes with the location you saved the HW file on your computer.
cd "C:\Users\Spong\Desktop\UNR Classes\Fall 2018\Applied Econometrics\HW\HW 3"
use CH6_HW-1

sum

*Summarize and Tab the age category to look at data and seee if anyhthinf stands out.
*summarize age
*tab age
*Per HW directions (see pdf), see are studying people between 16 & 65, so dropping 
*observations outside that age range
drop if age<16
drop if age>65

*Summarize and Tab the incwage category to look at data and seee if anyhthinf stands out.
*summarize incwage
*tab incwage
*Per HW directions,looking at people with positive earnings, so dropping people with no 
* income. drop of 589,440 obs, matches the figure in the tab table
drop if incwage<=0

*Summarize and Tab the incwage category to look at data and seee if anyhthinf stands out.
*summarize uhrswork
*tab uhrswork
* Per HW directions, we wish to look at people working more than 1,000 hrs per year,
* given 1 year=52.1429 weeks, (1000/52.1429) this equates to about 19.178 hours per week
*(We could switch to 52)
drop if uhrswork<19.178

sum


twoway scatter age incwage
reg incwage age 
ovtest
*H0: Model has no omitted variables, p<.05 (it is 0.0000) so it it is likely
* that it have some polynomial variables missing



***********************
*PICK A POLYNOMIAL SPECIFICATION
***********************

gen age2=age^2
gen age3=age^3
gen age4=age^4
gen age5=age^5

*reg incwage age 


reg incwage age age2 age3 age4 age5
ovtest
**Ironically, the ovtest still indicates the that we are omitting variables


***********************
*PART A : PRESENT RESULTS FROM YOUR OUTPUT
***********************
*You need installation files fot regsave (can install from help search regsave 

*Commented out because it replaces original data variables in variable manager.
*I had to clear and load data again.
*regsave
*list

**TEXSAVE saves output for LATEX, need to install package from Help 

*texsave using "table.tex", title(Poly Regression) 



***********************
*PART B: Explain why we choose the specification we did. 
***********************

*Looking at the twoway scatter, it appeared visually that there might be a cubic
*Also looking at the Adjusted R sqr increasing when adding polynomial terms. Finally,
*ovtest to check if we were had omitted any polynomials.

*************************
*PART C: Give AVG marginal Effect & agerage of the marginal effects.
**************************


*Average Marginal Effects 
margins, dydx(age age2 age3 age4 age5)

*This is the marginal effects at the means
margins, dydx(age age2 age3 age4 age5) atmeans

display _b[_cons]+_b[age]+_b[age2]+_b[age3]+_b[age4]+_b[age5]
*WALD test H0 that beta coefficient=0
test _b[_cons]=0
test _b[age]=0
test _b[age2]=0
test _b[age3]=0
test _b[age4]=0
test _b[age5]=0

*test of age variables
test

*. ttest incwage, by (age)
*Stata error: more than 2 groups found, only 2 allowed



***************************
*Part D: Discuss Significance of overall polynomial and individual terms
***************************

*reg incwage age age2 age3 age4 age5 Overall: Adj R2 overall is .0782 which is not 
*very explainatory, so we likely need nore varables since a lot is left unexplained.
* The F-test of our model
*Prob > F =0.0000 tels us that H0 that the R^2=0 (that our model is no good
* explains none of the variation in our dependent variable). Say we coulds say
*it is statistically significant at all confidence levels.

*Individual terms: Looking at Wald test, low p-values indicate 
* the null (that the beta coefficients are 0) should be rejected. That is that 
* they are meaningful additions .

*Looking at the t-statistics, (xbar - mu)/ (s\sqrt(n)) or signal to Noise, we have 
*relatively large t-stat values so combined with the low pvalues tells us the H0 
*(coefficient of variable equals zeros)



*********************************
*Problem 2
*********************************

*Create dummies for never married, currently married, and formerly maried
 tab marst
 tab marst, nolab



gen NevRMar=1 if marst==6
replace NevRMar=0 if marst==5 | marst==5 |marst==3 | marst==2 |marst==1
gen CurrMar=1 if marst==1 | marst==2
replace CurrMar=0 if marst==5 | marst==4 | marst==3 
gen FormMar=1 if marst==3 | marst==4 | marst==5
replace FormMar=0 if marst==1 | marst==2


************
*PART A Put all three indicators in your model
************

reg incwage age age2 age3 age4 age5 NevRMar CurrMar FormMar
***Result is "no observations r(2000)"; We cannot put all three dummies in the 
*model because there would be perfect multicolinearity. In matrix perspective
*the sum of the category dummies for each row equals the intercept value of that row.

****REDO DUMMIES****NEED to fix
*gen NVRMAR=1 if marst==




**************
*Part B: Run a model that will test whether or not married people see wages go up 
*more quickly with age than people who were never married.
*****************************


*reg incwage age age2 age3 age4 age5 NevRMar CurrMar
***Result is "no observations r(2000)" so something wrong with dummies SEE REDO Dummies




*******************
*Problem 3
*******************
*Construct Var. that is years of education
xi [, i noomit] educ
*xi: logistic outcome incwage  i.educ


**********************
*Part A
************************
reg incwage i.educ


************************
*PART B
************************
gen LNincwage=ln(incwage)
reg LNincwage i.educ

************************
*Part C
***************************
gen LNeduc=ln(educ)
xi [,l noomit] LNeduc

reg incwage l.LNeduc


***************************
*PART D
***************************

reg LNincwage  l.LNeduc



************************************************
*Problem 4
************************************************

reg incwage age age2
*********Breuch-Pagan test
estat hettest
*********Low p-value indicates heterosked a problem

*************White Test
imtest, white

twoway scatter age age2

predict e, resid

gen e2=e^2

predict yhat, xb
scatter e2 yhat

***Data has funnel shaped pattern. data has increasing variance as linear predictions increase

********************
*Part B
******************
regress incwage age age2, vce(robust)




