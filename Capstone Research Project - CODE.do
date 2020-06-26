*2/12/2020
*Immigration Variable
clear
set more off
global dir = "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic"
cd "$dir"
//log using Feb_12_finalpaper, text replace
use "$dir\merge_all_health_gdp_phi_finalreg.dta"
**Exploring the dataset
tab year
tab sex 

***** Immigration
//tab citizen 
//tab citizen, nolabel 

gen immigrant=(citizen==4 | citizen==5)
tabstat immigrant [weight=asecwt], by(year)

gen all=1

*** Explore this immigrant variable
summ immigrant, detail 
//tab year immigrant 
tab year immigrant，row 
//tab nativity

***** Birthplace of Immigrants
tab bpl 

estpost tab bpl, sort
esttab using "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic\summary_table_bpl.csv", replace

tab bpl, sort nolabel

** Make Indicators for Top Countries
gen Mexico=(bpl==20000)
gen Phillippines=(bpl==51500)
gen Puertorico =(bpl==11000)
gen India=(bpl==52100)
gen ElSalvador=(bpl==21030)
gen China=(bpl==50000)
gen Cuba=(bpl==25000)
gen Vietnam=(bpl==51800)
gen Dominicanrepublic=(bpl==26010)
gen Germany=(bpl==45300)

//gen Haiti=(bpl==26020)
//gen Honduras=(bpl==21050)

**** Education Levels************************************
//tab educ 
//tab educ, nolabel
gen college_grad=(educ>100 & educ<.)
//tab immigrant college_grad , row

gen immigrant_college_grad=college_grad*immigrant

*** Age Distributions 
//tab age immigrant, col

*** Gender 
//tab sex immigrant, col
//tab sex
//tab sex, nol
gen male=(sex==1)
gen male_immigrant=male*immigrant

//tab Mexico male [iweight=hwtsupp], row
//tab China male [iweight=hwtsupp], row
//gen male_Mexico=male*Mexico
//gen male_China=male*China

**** Where are Immigrants Concentrated?
tab state if immigrant==1, sort
tab state if Cuba==1, sort
tab state if Mexico==1, sort
tab state if China==1, sort

*** Year of Immigration
//tab yrimmig 

********* Weekly Earnings（outcome from professor)
//keep if age>15 & age<70
//gen log_earnings=log(incwage)
//tabstat log_earnings, by(immigrant)




**********OLS_________________________________________with reverse casuality***

/*
reg healthcare_spending immigrant i.year
outreg2 using table1.doc, tstat append 

quiet reg healthcare_spending immigrant i.year i.sex
outreg2 using table1.doc, tstat append 

quiet reg healthcare_spending immigrant i.year i.sex i.educ
outreg2 using table1.doc, tstat append 

quiet reg healthcare_spending immigrant i.year i.sex i.educ i.age
outreg2 using table1.doc, tstat append 
*/
//PUBLIC HE
xtreg healthcare_spending immigrant
outreg2 using table_ols.doc, replace 

quiet xtreg healthcare_spending immigrant i.year i.educ i.age i.statenum
outreg2 using table_ols.doc, append keep(healthcare_spending immigrant educ age)

quiet xtreg healthcare_spending Mexico i.year i.educ i.age i.statenum 
outreg2 using table_ols.doc, append keep(healthcare_spending Mexico educ age)

//quiet xtreg healthcare_spending Phillippines i.year i.educ i.age i.statenum 
//outreg2 using table_ols.doc, append 

quiet xtreg healthcare_spending Mexico Phillippines Puertorico India ElSalvador China Cuba Vietnam Dominicanrepublic Germany i.year i.educ i.age i.statenum 
outreg2 using table_ols.doc, append keep(healthcare_spending Mexico Phillippines Puertorico India ElSalvador China Cuba Vietnam Dominicanrepublic Germany educ age)



***************** Shift Share Approach********************************
*** ***********************Shift Share -- Mexico -- right kind of numbers
egen baseline_Mexico_2=mean(Mexico) if year==1994, by(state)
egen baseline_Mexico=mean(baseline_Mexico_2), by(state)
egen total_Mexico=mean(Mexico), by(year)

preserve
collapse (mean) Mexico baseline_Mexico total_Mexico healthcare_spending GDPpercap immigrant_college_grad (sum) all, by(statenum year)

gen shift_share=baseline_Mexico * total_Mexico

reg Mexico shift_share i.statenum i.year [weight=all]

//reg log_earnings shift_share i.statenum i.year [weight=all]

xtset statenum year
tab year, gen(y_)
//xtivreg2 healthcare_spending (Mexico=shift_share) y_* [weight=all], fe

quiet xtivreg2 healthcare_spending (Mexico=shift_share) GDPpercap immigrant_college_grad y_*[weight=all], fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 healthcare_spending Mexico GDPpercap immigrant_college_grad using reg_second.xls, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3)

restore



*** Shift Share - El Salvador -- elasticity = +4 and huge numbers
egen baseline_ElSalvador_2=mean(ElSalvador) if year==1994, by(statenum)
egen baseline_ElSalvador=mean(baseline_ElSalvador_2), by(statenum)
egen total_ElSalvador=mean(ElSalvador), by(year)

preserve
collapse (mean) ElSalvador baseline_ElSalvador total_ElSalvador healthcare_spending GDPpercap immigrant_college_grad (sum) all, by(statenum year)

gen shift_share=baseline_ElSalvador * total_ElSalvador

//reg ElSalvador shift_share i.statenum i.year [weight=all]

//reg healthcare_spending shift_share i.statenum i.year [weight=all]

xtset statenum year
tab year, gen(y_)
//xtivreg2 healthcare_spending (ElSalvador=shift_share) y_* [weight=all], fe
quiet xtivreg2 healthcare_spending (ElSalvador=shift_share) GDPpercap immigrant_college_grad y_*[weight=all], fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 healthcare_spending ElSalvador GDPpercap immigrant_college_grad using reg_second.xls, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3)

restore




*** Shift Share - Cuba -- elasticity = -1.7 and huge numbers
egen baseline_Cuba_2=mean(Cuba) if year==1994, by(statenum)
egen baseline_Cuba=mean(baseline_Cuba_2), by(statenum)
egen total_Cuba=mean(Cuba), by(year)

preserve
collapse (mean) Cuba baseline_Cuba total_Cuba healthcare_spending GDPpercap immigrant_college_grad (sum) all, by(statenum year)

gen shift_share=baseline_Cuba * total_Cuba

//reg Cuba shift_share i.statenum i.year [weight=all]

//reg healthcare_spending shift_share i.statenum i.year [weight=all]
//xtivreg2 healthcare_spending (Cuba=shift_share) y_* [weight=all], fe
xtset statenum year
tab year, gen(y_)

quiet xtivreg2 healthcare_spending (Cuba=shift_share) GDPpercap immigrant_college_grad y_*[weight=all], fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 healthcare_spending Cuba GDPpercap immigrant_college_grad using reg_second.xls, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3)

restore


*** Shift Share - Phillippines
egen baseline_Phillippines_2=mean(Phillippines) if year==1994, by(statenum)
egen baseline_Phillippines=mean(baseline_Phillippines_2), by(statenum)
egen total_Phillippines=mean(Phillippines), by(year)

preserve
collapse (mean) Phillippines baseline_Phillippines total_Phillippines healthcare_spending GDPpercap immigrant_college_grad (sum) all, by(statenum year)

gen shift_share=baseline_Phillippines * total_Phillippines

//reg Honduras shift_share i.statenum i.year [weight=all]

//reg healthcare_spending shift_share i.statenum i.year [weight=all]
//xtivreg2 healthcare_spending (Honduras=shift_share) y_* [weight=all], fe
xtset statenum year
tab year, gen(y_)

quiet xtivreg2 healthcare_spending (Phillippines=shift_share) GDPpercap immigrant_college_grad y_*[weight=all], fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 healthcare_spending Phillippines GDPpercap immigrant_college_grad using reg_second.xls, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3)

restore



********************India **********************************
egen baseline_India_2=mean(India) if year==1994, by(statenum)
egen baseline_India=mean(baseline_India_2), by(statenum)
egen total_India=mean(India), by(year)

preserve
collapse (mean) India baseline_India total_India healthcare_spending GDPpercap immigrant_college_grad (sum) all, by(statenum year)

gen shift_share=baseline_India * total_India

//reg Honduras shift_share i.statenum i.year [weight=all]

//reg healthcare_spending shift_share i.statenum i.year [weight=all]
//xtivreg2 healthcare_spending (Honduras=shift_share) y_* [weight=all], fe
xtset statenum year
tab year, gen(y_)

quiet xtivreg2 healthcare_spending (India=shift_share) GDPpercap immigrant_college_grad y_*[weight=all], fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 healthcare_spending Indias GDPpercap immigrant_college_grad using reg_second.xls, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3)

restore




************************ China ****************************************
egen baseline_China_2=mean(China) if year==1994, by(statenum)
egen baseline_China=mean(baseline_China_2), by(statenum)
egen total_China=mean(China), by(year)

preserve
collapse (mean) China baseline_China total_China healthcare_spending GDPpercap immigrant_college_grad (sum) all, by(statenum year)

gen shift_share=baseline_China * total_China

//reg Honduras shift_share i.statenum i.year [weight=all]

//reg healthcare_spending shift_share i.statenum i.year [weight=all]
//xtivreg2 healthcare_spending (Honduras=shift_share) y_* [weight=all], fe
xtset statenum year
tab year, gen(y_)

quiet xtivreg2 healthcare_spending (China=shift_share) GDPpercap immigrant_college_grad y_*[weight=all], fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 healthcare_spending China GDPpercap immigrant_college_grad using reg_second.xls, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3)

restore

**************************Germany******************************************
egen baseline_Germany_2=mean(Germany) if year==1994, by(statenum)
egen baseline_Germany=mean(baseline_Germany_2), by(statenum)
egen total_Germany=mean(Germany), by(year)

preserve
collapse (mean) Germany baseline_Germany total_Germany healthcare_spending GDPpercap immigrant_college_grad (sum) all, by(statenum year)

gen shift_share=baseline_Germany * total_Germany

//reg Honduras shift_share i.statenum i.year [weight=all]

//reg healthcare_spending shift_share i.statenum i.year [weight=all]
//xtivreg2 healthcare_spending (Honduras=shift_share) y_* [weight=all], fe
xtset statenum year
tab year, gen(y_)

quiet xtivreg2 healthcare_spending (Germany=shift_share) GDPpercap immigrant_college_grad y_*[weight=all], fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 healthcare_spending Germany GDPpercap immigrant_college_grad using reg_second.xls, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3)

restore



**** **************selected all Countries***********************************

******************first and second for every country************************
egen baseline_Mexico_2=mean(Mexico) if year==1994, by(state)
egen baseline_Mexico=mean(baseline_Mexico_2), by(state)
egen total_Mexico=mean(Mexico), by(year)

egen baseline_ElSalvador_2=mean(ElSalvador) if year==1994, by(statenum)
egen baseline_ElSalvador=mean(baseline_ElSalvador_2), by(statenum)
egen total_ElSalvador=mean(ElSalvador), by(year)

egen baseline_Cuba_2=mean(Cuba) if year==1994, by(statenum)
egen baseline_Cuba=mean(baseline_Cuba_2), by(statenum)
egen total_Cuba=mean(Cuba), by(year)

egen baseline_Phillippines_2=mean(Phillippines) if year==1994, by(statenum)
egen baseline_Phillippines=mean(baseline_Phillippines_2), by(statenum)
egen total_Phillippines=mean(Phillippines), by(year)

egen baseline_India_2=mean(India) if year==1994, by(statenum)
egen baseline_India=mean(baseline_India_2), by(statenum)
egen total_India=mean(India), by(year)

egen baseline_China_2=mean(China) if year==1994, by(statenum)
egen baseline_China=mean(baseline_China_2), by(statenum)
egen total_China=mean(China), by(year)

egen baseline_Germany_2=mean(Germany) if year==1994, by(statenum)
egen baseline_Germany=mean(baseline_Germany_2), by(statenum)
egen total_Germany=mean(Germany), by(year)

egen baseline_Puertorico_2=mean(Puertorico) if year==1994, by(statenum)
egen baseline_Puertorico=mean(baseline_Puertorico_2), by(statenum)
egen total_Puertorico=mean(Puertorico), by(year)

egen baseline_Vietnam_2=mean(Vietnam) if year==1994, by(statenum)
egen baseline_Vietnam=mean(baseline_Vietnam_2), by(statenum)
egen total_Vietnam=mean(Vietnam), by(year)

egen baseline_Dominicanrepublic_2=mean(Dominicanrepublic) if year==1994, by(statenum)
egen baseline_Dominicanrepublic=mean(baseline_Dominicanrepublic_2), by(statenum)
egen total_Dominicanrepublic=mean(Dominicanrepublic), by(year)


preserve
collapse (mean) Mexico baseline_Mexico total_Mexico ElSalvador baseline_ElSalvador total_ElSalvador Cuba baseline_Cuba total_Cuba Phillippines baseline_Phillippines total_Phillippines India baseline_India total_India Vietnam baseline_Vietnam total_Vietnam China baseline_China total_China Germany baseline_Germany total_Germany Puertorico baseline_Puertorico total_Puertorico Dominicanrepublic baseline_Dominicanrepublic total_Dominicanrepublic healthcare_spending PHI all_countries shift_share (sum) all, by(statenum year)

//first stage
gen shift_share_Mexico=baseline_Mexico * total_Mexico
quiet reghdfe Mexico shift_share i.statenum i.year, absorb(year statenum)
outreg2 using diff_country.doc, replace addtext(State FE, YES, Year FE, YES) keep(Mexico shift_share) 

gen shift_share_Phillippines=baseline_Phillippines * total_Phillippines
quiet reghdfe Phillippines shift_share_Phillippines i.statenum i.year, absorb(year statenum)
outreg2 using diff_country.doc, append addtext(State FE, YES, Year FE, YES) keep(Phillippines shift_share_Phillippines) 

gen shift_share_Puertorico=baseline_Puertorico * total_Puertorico
quiet reghdfe Puertorico shift_share_Puertorico i.statenum i.year , absorb(year statenum)
outreg2 using diff_country.doc, append addtext(State FE, YES, Year FE, YES) keep(Puertorico shift_share_Puertorico) 

gen shift_share_India=baseline_India * total_India
quiet reghdfe India shift_share_India i.statenum i.year, absorb(year statenum) 
outreg2 using diff_country.doc, append addtext(State FE, YES, Year FE, YES) keep(India shift_share_India) 

gen shift_share_ElSalvador=baseline_ElSalvador * total_ElSalvador
quiet reghdfe ElSalvador shift_share_ElSalvador i.statenum i.year , absorb(year statenum)
outreg2 using diff_country.doc, append addtext(State FE, YES, Year FE, YES) keep(ElSalvador shift_share_ElSalvador) 

gen shift_share_China=baseline_China * total_China
quiet reghdfe China shift_share_China i.statenum i.year , absorb(year statenum)
outreg2 using diff_country.doc, append addtext(State FE, YES, Year FE, YES) keep(China shift_share_China) 

gen shift_share_Cuba=baseline_Cuba * total_Cuba
quiet reghdfe Cuba shift_share_Cuba i.statenum i.year, absorb(year statenum)
outreg2 using diff_country.doc, append addtext(State FE, YES, Year FE, YES) keep(Cuba shift_share_Cuba) 

gen shift_share_Vietnam=baseline_Vietnam * total_Vietnam
quiet reghdfe Vietnam shift_share_Vietnam i.statenum i.year, absorb(year statenum) 
outreg2 using diff_country.doc, append addtext(State FE, YES, Year FE, YES) keep(Vietnam shift_share_Vietnam) 

gen shift_share_Dominicanrepublic=baseline_Dominicanrepublic * total_Dominicanrepublic
quiet reghdfe Dominicanrepublic shift_share_Dominicanrepublic i.statenum i.year, absorb(year statenum)
outreg2 using diff_country.doc, append addtext(State FE, YES, Year FE, YES) keep(Dominicanrepublic shift_share_Dominicanrepublic) 

gen shift_share_Germany=baseline_Germany * total_Germany
quiet reghdfe Germany shift_share_Germany i.statenum i.year , absorb(year statenum)
outreg2 using diff_country.doc, append addtext(State FE, YES, Year FE, YES) keep(Germany shift_share_Germany) 


gen shift_share = (baseline_Cuba *total_Cuba) + (baseline_ElSalvador* total_ElSalvador) + (baseline_India * total_India) + (baseline_Cuba * total_Cuba) + (baseline_Vietnam * total_Vietnam) + (baseline_China * total_China) + (baseline_Germany * total_Germany) + (baseline_Puertorico * total_Puertorico) + (baseline_Dominicanrepublic * total_Dominicanrepublic)

//second stage
quiet xtivreg2 healthcare_spending (Puertorico=shift_share_Puertorico)  y_*, fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second_country.doc, replace addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(healthcare_spending Puertorico)

quiet xtivreg2 healthcare_spending (India=shift_share_India)  y_*, fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second_country.doc, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(healthcare_spending India)

quiet xtivreg2 healthcare_spending (ElSalvador=shift_share_ElSalvador)  y_*, fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second_country.doc, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(healthcare_spending ElSalvador)

quiet xtivreg2 healthcare_spending (China=shift_share_China)  y_*, fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second_country.doc, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(healthcare_spending China)

quiet xtivreg2 healthcare_spending (Cuba=shift_share_Cuba)  y_*, fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second_country.doc, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(healthcare_spending Cuba)

quiet xtivreg2 healthcare_spending (Vietnam = shift_share_Vietnam)  y_*, fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second_country.doc, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(healthcare_spending Vietnam)

quiet xtivreg2 healthcare_spending (Dominicanrepublic=shift_share_Dominicanrepublic)  y_*, fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second_country.doc, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(healthcare_spending Dominicanrepublic)

quiet xtivreg2 healthcare_spending (Germany=shift_share_Germany)  y_*, fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second_country.doc, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(healthcare_spending Germany shift_share_Germany)

***********magnitude*******************
summ Puertorico, detail
summ India, detail
summ ElSalvador, detail
summ China, detail
summ Cuba, detail
summ Vietnam, detail
summ Dominicanrepublic, detail
summ Germany, detail


**********************using all counties variables****************************

//use "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic\merge_all_health_gdp_phi_finalreg.dta"
//delete: (baseline_Phillippines * total_Phillippines) +(baseline_Mexico * total_Mexico) +
gen all_countries= ElSalvador+Cuba+India+Vietnam+China+Germany+Puertorico+Dominicanrepublic

gen shift_share = (baseline_Cuba *total_Cuba) + (baseline_ElSalvador* total_ElSalvador) + (baseline_India * total_India) + (baseline_Cuba * total_Cuba) + (baseline_Vietnam * total_Vietnam) + (baseline_China * total_China) + (baseline_Germany * total_Germany) + (baseline_Puertorico * total_Puertorico) + (baseline_Dominicanrepublic * total_Dominicanrepublic)

//OLS***************************
reg healthcare_spending all_countries, robust cluster(statenum)
outreg2 using reg_ols.doc, replace addtext(State FE, NO, Year FE, NO)
xtreg healthcare_spending all_countries i.year i.state, robust cluster(statenum)
outreg2 using reg_ols.doc, append addtext(State FE, YES, Year FE, YES)


//first stage
xtreg all_countries shift_share, robust cluster(statenum)
outreg2 using reg_first.doc, replace addstat(Partial R-squared, pr2, F-Test of ivs, Fstat)  

reg all_countries shift_share i.statenum i.year, robust cluster(statenum)
outreg2 using reg_first.doc, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) keep(all_countries shift_share)

xtset statenum year
tab year, gen(y_)
/*y and z
reg healthcare_spending shift_share i.statenum i.year, robust cluster(statenum)
outreg2 using reg_y_z.doc, replace addtext(State FE, YES, Year FE, YES) keep(healthcare_spending shift_share)

*/
xtreg2 healthcare_spending shift_share y_*, fe first robust cluster(statenum)
outreg2 using y_z.doc, replace se asterisk(se) label bdec(3) sdec(3) keep(healthcare_spending shift_share)


//second stage

xtset statenum year
tab year, gen(y_)
//without fixed effects:
quiet xtivreg2 healthcare_spending (all_countries=shift_share), fe first robust cluster(statenum)
ivreg2
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second.doc, replace addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3)

//with FE
quiet xtivreg2 healthcare_spending (all_countries=shift_share) y_*, fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second.doc, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(healthcare_spending all_countries)

/*
quiet xtivreg2 PHI (all_countries=shift_share), fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second.doc, append addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(PHI all_countries)


quiet xtivreg2 PHI (all_countries=shift_share) y_*, fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second.doc, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(PHI all_countries)
*/
restore







**************************few countries instrument*******************
gen shift_share_Puertorico=baseline_Puertorico * total_Puertorico
gen shift_share_India=baseline_India * total_India
gen shift_share_ElSalvador=baseline_ElSalvador * total_ElSalvador
gen shift_share_China=baseline_China * total_China
gen shift_share_Cuba=baseline_Cuba * total_Cuba
gen shift_share_Vietnam=baseline_Vietnam * total_Vietnam
xtset statenum year
tab year, gen(y_)

quiet xtivreg2 healthcare_spending (Puertorico India ElSalvador China Cuba Vietnam =shift_share_Puertorico shift_share_India shift_share_ElSalvador  shift_share_China shift_share_Cuba shift_share_Vietnam)  y_*, fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second_few.doc, replace addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(Puertorico India ElSalvador China Cuba Vietnam healthcare_spending )














/*
****************************************************************
using all immigrants to create shift share is not a good instrument variable, the collinearities detected. 
******************************All Immigrants ******************************

egen baseline_immigrant_2=mean(immigrant) if year==1994, by(state)
egen baseline_immigrant=mean(baseline_immigrant_2), by(state)
egen total_immigrant=mean(immigrant), by(year)

preserve
collapse (mean) immigrant baseline_immigrant total_immigrant healthcare_spending PHI (sum) all, by(statenum year)

gen shift_share=baseline_immigrant * total_immigrant
//drop if year>2014
//reg immigrant shift_share i.statenum i.year [weight=all]

//reg healthcare_spending shift_share i.statenum i.year [weight=all]
//pwcorr healthcare_spending shift_share, sig

/*correlations is 0.0508, but significant>0.05, not statistically significant, there is no relationship between IV and Y.
 Display correlation matrix or covariance matrix
        correlate [varlist] [if] [in] [weight] [, correlate_options]
 Display all pairwise correlation coefficients
       pwcorr [varlist] [if] [in] [weight] [, pwcorr_options]
*/


//gen log_healthcare= log(1+ healthcare_spending)
//with first stage-fail
//quiet xtivreg2 healthcare_spending (immigrant=shift_share) [weight=all], fe first savefirst robust cluster(statenum)
//est restore first
//outreg2 using reggression_table_1.doc, tstat replace
//est restore second
//outreg2 using reggression_table_1.doc, tstat append

//quiet xtivreg2 healthcare_spending (immigrant=shift_share) y_*[weight=all], fe first robust cluster(statenum)
//outreg2 using reggression_table_1.doc, tstat append addtext(State FE, YES, Year FE, YES)

//first stage
reg immigrant shift_share, robust cluster(statenum)
outreg2 using reg_first_immi.doc, replace addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) 

reg immigrant shift_share i.statenum i.year,robust cluster(statenum)
outreg2 using reg_first_immi.doc, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat)  keep(immigrant shift_share) 

*reghdfe won't show the time or state dummies
*ftools
//only second stage

//tabstat immigrant [weight=asecwt], by(year)
tab year, gen(y_)

xtset statenum year
//xtivreg2
quiet xtivreg2 healthcare_spending (immigrant=shift_share), fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second_immi.doc, replace addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3)


quiet xtivreg2 healthcare_spending (immigrant=shift_share) y_*, fe first robust cluster(statenum)
mat first = e(first)
scalar pr2 = first[3,1]
scalar Fstat = first[4,1]
scalar Fpval = first[6,1]
outreg2 using reg_second_immi.doc, append addtext(State FE, YES, Year FE, YES) addstat(Partial R-squared, pr2, F-Test of ivs, Fstat) se asterisk(se) label bdec(3) sdec(3) keep(healthcare_spending immigrant)

*xtivreghdfe

//weak IV test
quietly ivregress gmm healthcare_spending (immigrant = shift_share),vce(robust)
estat firststage,forcenonrobust

restore
*/





************* Plot the Change in Y vs the Change in X
set scheme s1color
*** Binned Scatter of X vs Y
binscatter healthcare_spending immigrant, n(25) line(lfit) msymbols(O T) title("Graph: The Relationship Between Immigrants and Healthcare Spending in U.S.(1994-2014)") xtitle(Propostion of Immigrants) ytitle(Healthcare Spending(millions of dollars) ) legend(lab(1 White) lab(2 Black))
//graph save "Graph-motivation1" "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic\Graph-1.gph"
graph export graph_scatterbin.png, replace

gen change_healthcare=D20.healthcare_spending if year==2014 
gen change_immigrant = D20.immigrant if year==2014

twoway (scatter change_healthcare change_immigrant) (lfit change_healthcare change_immigrant), legend(off) title("Graph: The Relationship Between Immigrants and Healthcare Spending in U.S.(1994-2014)") xtitle("Change of Propostion of Immigrants") ytitle("Change of Healthcare Spending(millions of dollars)")
//graph save "Graph-motivation" "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic\Graph-tw2.gph"
graph export graph_scatterbin_change.png, replace

// Let's do it in Bins:
egen D_immi_bin=cut(change_immigrant), at(-2000, -1000(100)1500, 4000) 
egen mean_Dlog_immi=mean(change_immigrant), by(D_immi_bin)
egen mean_Dlog_hc=mean(change_healthcare), by(D_immi_bin)

twoway (scatter mean_Dlog_hc mean_Dlog_immi, msymbol(circle_hollow)) (lfit change_healthcare change_immigrant ), legend(off) title("Graph: The Relationship Between Immigrants and Healthcare Spending in U.S.") xtitle("Change in Immigrants") ytitle("Change in Healthcare Spending")
//graph export prettier_D_scatter.png, replace


/*
*************************healthcare_longdata*********************
rename B y1994
rename C y1995
rename D y1996
rename E y1997
rename F y1998
rename G y1999
rename H y2000
rename I y2001
rename J y2002
rename K y2003
rename L y2004
rename M y2005
rename N y2006
rename O y2007
rename P y2008
rename Q y2009
rename R y2010
rename S y2011
rename T y2012
rename U y2013
rename V y2014

//reshape long y,i(state) j(year)

***************************merge healthcare**********************
//master:
decode statecensus, gen(state)
//decode state, gen(state4) 
//rename state statenum
//rename state4 state
merge m:1 state year using "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic\healthcare_long_1994-2014.dta"

   Result                           # of obs.
    -----------------------------------------
    not matched                       536,888
        from master                   536,867  (_merge==1)
        from using                         21  (_merge==2)

    matched                         2,564,018  (_merge==3)
    -----------------------------------------

save "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic\immigrant_healthcare_year_state.dta"

***************************merge GDP**********************

. merge m:1 state year using "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic\GDPpercap.dta"

    Result                           # of obs.
    -----------------------------------------
    not matched                            42
        from master                        21  (_merge==1)
        from using                         21  (_merge==2)

    matched                         2,594,728  (_merge==3)
    -----------------------------------------
. save "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic\immi_health_gdp.dta"
. 
**********************merge PHI*******************

. merge m:1 state year using "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic\PHI_2001_2014.dta"

    Result                           # of obs.
    -----------------------------------------
    not matched                       665,458
        from master                   665,458  (_merge==1)
        from using                          0  (_merge==2)

    matched                         1,929,312  (_merge==3)
    -----------------------------------------

**********merge 'all countries variable' back to original data*******************

. merge m:1 statenum year using "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic\all_countries_immigrants
> _variables_merge.dta"
(label STATECENSUS already defined)

    Result                           # of obs.
    -----------------------------------------
    not matched                             0
    matched                         2,594,770  (_merge==3)
    -----------------------------------------

. drop _merge
*/
*************summary table*******************************************
tsset year
estpost, esttab, putexcel

sum year state statenum age race bpl yrimmig citizen educ healthcare_spending GDPpercap

//estpost 
estpost sum year state statenum age race bpl yrimmig citizen educ healthcare_spending  GDPpercap 
*to csv
esttab using "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic\summary_table.csv", cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") nomtitle onumber replace

*to doc
esttab using "C:\Users\zhang\OneDrive\桌面\Capstone\选题\final topic\summary_table.rtf", cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") nomtitle replace



//how to test parallel trends








