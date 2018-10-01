
*Call the directory where your file is located. cd command. Replace the portion
*in-quotes with the location you saved the HW file on your computer.
cd "C:\Users\Spong\Desktop\UNR Classes\Fall 2018\Applied Econometrics\HW\HW 3"
use CH6_HW-1

*Summarize and Tab the age category to look at data and seee if anyhthinf stands out.
summarize age
tab age
*Per HW directions (see pdf), see are studying people between 16 & 65, so dropping 
*observations outside that age range
drop if age<16
drop if age>65

*Summarize and Tab the incwage category to look at data and seee if anyhthinf stands out.
summarize incwage
tab incwage
*Per HW directions,looking at people with positive earnings, so dropping people with no 
* income. drop of 589,440 obs, matches the figure in the tab table
drop if incwage<=0

*Summarize and Tab the incwage category to look at data and seee if anyhthinf stands out.
summarize uhrswork
tab uhrswork
* Per HW directions, we wish to look at people working more than 1,000 hrs per year,
* given 1 year=52.1429 weeks, (1000/52.1429) this equates to about 19.178 hours per week
*(We could switch to 52)
drop if uhrswork<19.178

* Part A:  Pick a polynomial specification of age to run. I am trying out age^2
*First need to genrate a new variable for age^2 before we can get it in the regress command.
* We could compare the regression with age^2 to a baseline regression with just age. Additionally,
*we can regress eache independent variable by itself.

gen age2=age^2
regress incwage educ marst uhrswork age age2
regress incwage educ marst uhrswork age
regress incwage educ
regress incwage marst
regress incwage uhrswork
regress incwage age
* Part B: 
scatter incwage age || lfit incwage age 
* When looking at the scatter plot of incwage and age, (if you squint hard enough) you can see
* two humps (inverted U's) the first hump around age 20 to 44 and the second hump around 47 to 65.
* Since the two humps are the same direction a quadratic seems reasonable. If one hump faced up and
* the other faced down, we might prefer a cubic then.

*rvfplot, yline(0)
*We can also look at the residuals of age on incwage. Looking at the plot, we see that 
* the residuals trend negative as fitted values increase.
regress incwage age
rvfplot, yline(0)


* Part C:
regress incwage educ marst uhrswork age age2

*Average Marginal Effects 
margins, dydx(educ marst uhrswork age age2)
display _b[_cons]+_b[educ]+_b[marst]+_b[uhrswork]+_b[age]+_b[age2]

*This is the marginal effects at the means
margins, dydx(educ marst uhrswork age age2) atmeans
