*QM3-POSTER
*SHAOQIAN ZHANG
clear
set scheme jab 
set more off
*-------step1: expolre the topics and the data:traffic policy influence the fatality? increase/decrease-----------
*step2: chooose your question(specfic,causal story)
cd "C:\Users\HP\Desktop\QM3\Final poster 2019" 
use traffic_fat.dta, clear

*--------------step3:decide how to answer the question--------------------------------------
*variable transformation
unique year
unique state
encode state, gen(state1)
xtset state1 year
set matsize 1000

*treat variable
gen treatyear=year if intlck1 > 0
bysort state1 : egen mintreatyear=min(treatyear)
gen treat= year >= mintreatyear
gen lfatality= log(totfat)

* margins plot
xtreg lfatality treat i.year, fe i(state1)cluster(region1)
tw lfit lfatality treat, msize(tiny) legend(off) xtitle("treated or not") ytitle("logged fatalities") title("Graph1: Linear Relationship")
graph export "Fit2.png", replace

margins, at(treat=(0(1)1))
marginsplot

gen region= state
replace region = "west" if region=="AK"
replace region = "west" if region=="HI"
replace region = "west" if region=="AZ"
replace region = "west" if region=="CA"
replace region = "west" if region=="CO"
replace region = "west" if region=="ID"
replace region = "west" if region=="MT"
replace region = "west" if region=="NM"
replace region = "west" if region=="NV"
replace region = "west" if region=="OR"
replace region = "west" if region=="UT"
replace region = "west" if region=="WA"
replace region = "west" if region=="WY"

replace region = "midwest" if region=="IA"
replace region = "midwest" if region=="IL"
replace region = "midwest" if region=="IN"
replace region = "midwest" if region=="KS"
replace region = "midwest" if region=="MI"
replace region = "midwest" if region=="MN"
replace region = "midwest" if region=="MO"
replace region = "midwest" if region=="NE"
replace region = "midwest" if region=="OH"
replace region = "midwest" if region=="SD"
replace region = "midwest" if region=="WI"

replace region = "northeast" if region=="CT"
replace region = "northeast" if region=="MA"
replace region = "northeast" if region=="ME"
replace region = "northeast" if region=="ND"
replace region = "northeast" if region=="NH"
replace region = "northeast" if region=="NJ"
replace region = "northeast" if region=="NY"
replace region = "northeast" if region=="PA"
replace region = "northeast" if region=="RI"
replace region = "northeast" if region=="VT"

replace region = "south" if region=="AL"
replace region = "south" if region=="AR"
replace region = "south" if region=="DC"
replace region = "south" if region=="DE"
replace region = "south" if region=="FL"
replace region = "south" if region=="GA"
replace region = "south" if region=="KY"
replace region = "south" if region=="LA"
replace region = "south" if region=="MD"
replace region = "south" if region=="MS"
replace region = "south" if region=="NC"
replace region = "south" if region=="OK"
replace region = "south" if region=="SC"
replace region = "south" if region=="TN"
replace region = "south" if region=="TX"
replace region = "south" if region=="VA"
replace region = "south" if region=="WV"

encode region, gen(region1)


*Form of panel analysis
gen tau = year - mintreatyear
tabulate tau, generate(Dt)

replace Dt29 = 0 
foreach var of varlist Dt* {
	replace `var' = 0 if `var' == . 
}

*parallel trends test-insignificant would be paralleL trends.
*reg D.lfatality Dt* if tau <0


*-------write your model with subscripts:add FE and controls to make it approprite; use interactions

*tw FE
xtreg lfatality Dt* i.year, fe i(state1) cluster(state1)
*controls:income unemprte vcrimerte
gen lincome=log(pcinc)
gen lviolent= log(vcrimetot)
xtreg lfatality Dt* lincome unemprte lviolent i.year, fe i(state1) cluster(state1)
coefplot, vertical yline(0) xline(8.5, lpattern(dash)) keep( Dt24 Dt25 Dt26 Dt26 Dt28 Dt29 Dt30 Dt31 Dt32 Dt33 Dt34 Dt35 Dt36 Dt37 Dt38 Dt39) omitted order(Dt*) ///
		  levels(95) legend(order(1 "95% CI" 2 "Point estimate")) ciopts(recast(rcap) lcolor(navy%50)) mcolor(navy%80) lcolor(navy%80)	///
		  graphregion(color(white))xlabel(1 "-6" 2 "-5" 3 "-4" 4 "-3" 5 "-2" 6 "-1" 7 "0" 8 "1" 9 "2" 10 "3" 11 "4" 12 "5" 13 "6" 14 "7" 15 "8" 16 "9", valuelabel) ///
		  title("Graph2: Parallel trend test") xtitle("Year") ytitle("Estimates") name(graph1, replace)	

*-----------------Run a few regressions(2 or more), make a table--------------------------------------
xi i.year

quietly xtreg lfatality treat _Iyear*, fe i(state1) cluster(region1)
outreg2 using reg.doc, tstat replace
*quietly xtreg lfatality treat lincome _Iyear*, fe i(state1) cluster(region1)
*outreg2 using reg.doc,tstat append
*quietly xtreg lfatality treat lincome lunemploy _Iyear*, fe i(state1) cluster(region1)
*outreg2 using reg.doc,tstat append
quietly xtreg lfatality treat lincome unemprte lviolent _Iyear*, fe i(state1) cluster(region1)
outreg2 using reg.doc,tstat append

*-------------step4: produce a great figure: explain question, causeolity, not only a bar graph.-----------------------------
*margins plot after controls
xtreg lfatality i.treat lincome unemprte lviolent _Iyear*, fe i(state1) cluster(region1)
margins, at(treat=(0(1)1)) by(region1)
marginsplot
* Lowess graph 
*endogenous timing of adoption, we could restrict our sample to the treated units and 
*compute the average outcome for each time relative to treatment and smooth it using a 
*locally weighted regression (e.g. lowess).
preserve 
bysort state1 : egen everTreat = max(treat) 
keep if everTreat 
collapse (mean) lfatality, by(tau) 
tw scatter lfatality tau if tau >-6 & tau <= 9 || /// 
	lowess lfatality tau if tau >-6 & tau <= 0 /// 
	|| lowess lfatality tau if tau > 0 & tau <= 9, /// 
	graphregion(color(white)) xtitle("Year") /// 
	ytitle("Average Outcome") xline(0, lpattern(dash)) 
restore

*-------------write into poster-----------------------------------------------------------------------
clear
set more off
