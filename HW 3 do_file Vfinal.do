

*Call the directory where your file is located. cd command. Replace the portion
*in-quotes with the location you saved the HW file on your computer.
*cd "C:\Users\Spong\Desktop\UNR Classes\Fall 2018\Applied Econometrics\HW\HW 3"
cd "/home/appertjt/Documents/Grad School/econometrics/Code/homework3/ECON-741-HW-3"
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


*twoway scatter age incwage
*graph export age_inwage_graph.pdf
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
*We reran the ovtest to check for improvement in omitted polynomials variable bias.
*Stata forums referred to Wooldridge's Intro Econom. Text saying it is testing
*functional form of X's included

*reg incwage age age2
*ovtest
*reg incwage age age2 age3
*ovtest
*reg incwage age age2 age3 age4
*ovtest

* We leave the others commented out, ;eaving the final specification. Since OVtest
*limited, we also looked at how much the beta coefficients were adding age5 is 
*pretty small so we truncated there.


reg incwage age age2 age3 age4 age5
ovtest
**Ironically, the ovtest still indicates the that we are omitting variables, like other 
*variables, interaction terms etc. 



**************************************
*PART A : PRESENT RESULTS FROM YOUR OUTPUT
**************************************

*You need installation files fot regsave (can install from help search regsave 

*Commented out because it replaces original data variables in variable manager.
*I had to clear and load data again.
*regsave
*list

**TEXSAVE saves output for LATEX, need to install package from Help 

*texsave using "table.tex", title(Poly Regression) 
* Comment out because once the command creates the .tex we use the latex code for 
*pasting in our Latex write up.


***************************************
*PART B: Explain why we choose the specification we did. 
***************************************

*Looking at the twoway scatter, it appeared visually that there might be a cubic
*Also looking at the Adjusted R sqr increasing when adding polynomial terms. Finally,
*ovtest to check if we were had omitted any polynomials.

****************************************
*PART C: Give AVG marginal Effect & agerage of the marginal effects.
****************************************


*Average Marginal Effects 
margins, dydx(age age2 age3 age4 age5)

*regsave 
*list
*texsave using "AvgMargFX", title(Average Marginal Effects)


*This is the marginal effects at the means
margins, dydx(age age2 age3 age4 age5) atmeans

*regsave 
*list
*texsave using "MargFXatMean", title(Marginal Effects at the Mean)



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



*******************************
*Part D: Discuss Significance of overall polynomial and individual terms
*******************************

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



*******************************************************************************
*Problem 2
*******************************************************************************

*Create dummies for never married, currently married, and formerly maried
 tab marst
 tab marst, nolab



gen NevRMar=1 if marst==6
replace NevRMar=0 if marst==5 | marst==5 |marst==3 | marst==2 |marst==1
gen CurrMar=1 if marst==1 | marst==2
replace CurrMar=0 if marst==5 | marst==4 | marst==3 
gen FormMar=1 if marst==3 | marst==4 | marst==5
replace FormMar=0 if marst==1 | marst==2


************************************
*PART A Put all three indicators in your model
************************************

reg incwage NevRMar CurrMar FormMar
***Result is "no observations r(2000)"; We cannot put all three dummies in the 
*model because there would be perfect multicolinearity. In matrix perspective
*the sum of the category dummies for each row equals the intercept value of that row.

****REDO DUMMIES****NEED to fix
**inspect

*Going to try to do seperate dummies for each marital status. 
tab marst, g(m)
rename m1 MarSP_prez
rename m2 MarSP_abs
rename m3 Sep
rename m4 Divor
rename m5 Widow
rename m6 Single

*reg incwage age age2 age3 age4 age5 Single MarSP_prez MarSP_abs Divor Widow

gen NM= Single
gen CM= MarSP_prez | MarSP_abs | Sep
gen FM= Divor | Widow

reg incwage age age2 age3 age4 age5 NM CM FM
***NM omitted because of collinearity, having all dummies always gives this problem



**************************************
*Part B: Run a model that will test whether or not married people see wages go up 
*more quickly with age than people who were never married.
**************************************

reg incwage age age2 age3 age4 age5 NevRMar CurrMar
***Result is "no observations r(2000)" so something wrong with dummies SEE REDO Dummies

reg incwage age age2 age3 age4 age5 NM CM 

*regsave 
*list
*texsave using "martialdummies", title(Indicator Variable Regression)

*It looks like coefficient on NM is smaller (2528.0118) than CM (16632.36), so 
*married people seem to have the wage advantage.


***Need interaction term
gen ageNM= age*NM
reg incwage age age2 age3 age4 age5 NM CM ageNM


*regsave 
*list
*texsave using "InteractAgeNMv2", title(Interaction Regression)



*reg incwage age2 age3 age4 age5 i.marst##age




*************************************************************************
*Problem 3
*************************************************************************
*Create a variable edY for years of education

tab educ
tab educ, nolab

gen edY=0 if educ==0
replace edY=4 if educ ==1
replace edY=9 if educ ==2
replace edY=10 if educ ==3
replace edY=11 if educ ==4
replace edY=12 if educ==5
replace edY=13 if educ ==6
replace edY=14 if educ==7
replace edY=15 if educ==8
replace edY=17 if educ==9
replace edY=18 if educ==10


/*
*Construct Var. that is years of education
****If 
xi [, i noomit] educ
*xi: logistic outcome incwage  i.educ
*/

**
**********************
*Part A
************************
reg incwage edY

*regsave 
*list
*texsave using "lin_lin", title(Linear-Linear Regression)

************************
*PART B
************************
gen LNincwage=ln(incwage)
reg LNincwage edY

*regsave 
*list
*texsave using "log_lin", title(Log-Linear Regression)

************************
*Part C
***************************
***Do we have to do some sort of fequency weight?
gen LNedY=ln(edY)
reg incwage LNedY

*regsave 
*list
*texsave using "lin_log", title(Linear-Log Regression)

**Try freq weight ?
*reg incwage [aw=weight]

***************************
*PART D
***************************
reg LNincwage  LNedY

*regsave 
*list
*texsave using "log_log", title(Log-Log Regression)


****************************************************************************
*Problem 4
****************************************************************************

reg incwage age age2


*********Breuch-Pagan test
estat hettest
*********Low p-value indicates heteroskedacity a problem

*************White Test
imtest, white

*twoway scatter age age2

predict e, resid

gen e2=e^2

predict yhat, xb
scatter e2 yhat

***Data has funnel shaped pattern. Data has increasing variance as linear predictions increase

graph export  e2_yhat.pdf

*regsave 
*list
*texsave using "e2_yhat", title(Squared Errors to Fitted)

********************
*Part B
********************
regress incwage age age2, vce(robust)

*regsave 
*list
*texsave using "reg_robust", title(Robust Regression)

log close
