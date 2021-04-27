


***** GNI PER CAPITA *****

use "/Users/CZ/Box Sync/Chenyue Data/master-april-2016.dta", clear
gen deflator=ngdpc_usd*100/cgdpc_usd
gen t1=deflator if year==2014
egen t2=mean(t1), by(wbcode)
gen deflator2014=deflator*100/t2
gen cgdpc_usd_2014=ngdpc_usd*100/deflator2014

drop if year>2014 | year<1992
twoway (scatteri 2500 2011 2500 2014, bcolor(gs15) recast(area) legend(off) ylab(0(500)2500, grid) ytitle("GDP per capita, US$")) ///
(scatter cgdpc_usd_2014 year if wbcode=="LAO", sort xtitle(Year) c(l) msy(S) clwidth(thick) color(red) xlab(1991(3)2015)) ///
(line pov_rate1 pov_rate2 year if wbcode=="LAO" ,  ytitle("Share of population (%)", axis(2)) yaxis(2) ylab(0(15)60, axis(2)) ///
text(600 1996 "LOW" "INCOME" 650 2012.5 "LOWER" "MIDDLE" "INCOME", color(green) size(vsmall)) note("Source: WDI" "Note: GDP per capita in 2014 constant US$")) ///
(pcarrowi 2500 1996.5 2000 1997.5 (12) "$2-a-day poverty (right axis)", lcolor(black) mlabc(black) color(black)) ///
(pcarrowi 2000 1996.5 1700 1997.5 (12) "$1-a-day poverty (right axis)", lcolor(black) mlabc(black) color(black)) 
graph export gdpc.emf, replace



***GDP Growth***

use "/Users/CZ/Box Sync/Chenyue Data/master-april-2016.dta", clear

keep if year>1994 & year<2015

twoway (bar gdpgrowth year if wbcode=="LAO", barwidth(.75) xlab(1995(5)2014) xtitle(Year) ylab(0(2)10, grid) ytitle("Percentage (%)") bcolor(black) fcolor(ltkhaki)) ///
, note("Source: World Development Indicator database") 
graph export gdpgrowth.png, replace



***** INFLATION AND EXCHANGE RATE *****


use "/Users/CZ/Box Sync/Chenyue Data/master-april-2016.dta", clear


drop if year>2014
drop if year<1995

twoway (line inflation year if wbcode=="LAO", clwidth(thick) sort ylab(0(20)160, grid) legend(off) xlab(1995(5)2010 2014) ytitle(Inflation (%))) ///
 (line exrate year if wbcode=="LAO", clwidth(thick) yaxis(2) ytitle("Local currency per US$", axis(2)) xtitle(Year) note("Source: World Development Indicators database"))
graph export ARM-inflation.emf, replace



***** REAL REVENUES AND EXPENDITURES *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\Master\master-oct-2015", clear

drop temp
gen temp=gdp_deflator if year==2013
egen pcpi2013=mean(temp), by(wbcode)
gen realthepc=(thegdp/100)*imf_ngdpc_lcu

gen rev=(revgdp/100)*imf_ngdp_lcu*pcpi2013/gdp_deflator 
gen exp=(expgdp/100)*imf_ngdp_lcu*pcpi2013/gdp_deflator

drop if year>2013 
drop if year<1995

graph bar rev exp if wbcode=="IDN", legend(off) over(year, label(angle(45))) legend(off) b1title(Year) ytitle("IDR Billions") note("Source: World Development Indicators database" "Note: Data are in 2013 constant local currency units")


***** Fiscal deficit and debt ratio *****



use "/Users/CZ/Box Sync/Chenyue Data/master-april-2016.dta", clear


keep if year>=2012 & year<=2014
egen mdebt=mean(ggxwdg_ngdp), by(wbcode)
egen mdeficit=mean(ggxcnl_ngdp), by(wbcode)
bysort wbcode: keep if _n==1
recode mdebt 150/max=.
recode mdeficit 20/max=.

replace wbname="Russia" if wbcode=="RUS"
gen label1=1 if wbcode=="IND" | wbcode=="GHA" | wbcode=="THA" | wbcode=="LKA" | wbcode=="KHM" 
gen label2=1 if wbcode=="RUS" 
gen label3=1 if wbcode=="SLB" 
gen label4=1 if wbcode=="VNM" | wbcode=="NGA" | wbcode=="BRA"
gen label5=1 if wbcode=="ZAF" | wbcode=="PNG" 
gen label6=1 if wbcode=="LAO" | wbcode=="PHL" | wbcode=="CHN" | wbcode=="MYS"
gen label7=1 if wbcode=="IDN"


twoway (scatter mdeficit mdebt if mdebt<200,  mcolor(ltkhaki) legend(off) xlab(0 25 60 100 150) ylab(, grid) xtitle(Debt to GDP ratio (%)) ytitle(Fiscal deficit to GDP ratio (%))) ///
(scatter mdeficit mdebt if mdebt<200 & label1==1, mcolor(black) mlab(wbname) mlabc(black)) ///
(scatter mdeficit mdebt if mdebt<200 & label2==1, mcolor(black) mlab(wbname) mlabc(black) mlabp(12)) ///
(scatter mdeficit mdebt if mdebt<200 & label3==1, mcolor(black) mlab(wbname) mlabc(black) mlabp(12) yline(-3) xline(60)) ///
(scatter mdeficit mdebt if mdebt<200 & label4==1, mcolor(black) mlab(wbname) mlabc(black) mlabp(6)) ///
(scatter mdeficit mdebt if mdebt<200 & label5==1, mcolor(black) mlab(wbname) mlabc(black) mlabp(9)) ///
(scatter mdeficit mdebt if mdebt<200 & label6==1, mcolor(black)) ///
(scatter mdeficit mdebt if mdebt<200 & label7==1, mcolor(red) mlab(wbname) mlabc(red) mlabp(12)), ///
note("Source: IMF World Economic Outlook database") 


***** INFORMALITY *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\Master\master-dec-2015", clear

gen nonwage=100-wagemale
replace nonwage=68 if year==1995 & wbcode=="IDN"
replace nonwage=68 if year==1996 & wbcode=="IDN"

drop if year<1995
drop if year>2013
twoway (bar nonwage year if wbcode=="IDN", barwidth(.75) xlab(1995(5)2010 2013) xtitle(Year) ylab(0(20)80, grid) ytitle("Share of employed population (%)") bcolor(black) fcolor(ltkhaki)) ///
, note("Source: World Development Indicators database")
graph export informal.emf, replace

gen sample=1 if wbcode=="IND" | wbcode=="LAO" | wbcode=="KHM" | wbcode=="CHN" | wbcode=="VNM" | wbcode=="PHL" | wbcode=="ZAF" | wbcode=="THA" | wbcode=="LKA" | wbcode=="RUS" | wbcode=="BRA" | wbcode=="MYS" | wbcode=="IDN"

table wbname if sample==1 & year>=2010, c(m nonwage) 



**** REVENUES AND EXPENDITURES SHARE OF GDP *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\Rawdata\Dec2015\master-dec-2015", clear
keep if year==2013
collapse revgdp expgdp, by(wbregion)
rename wbregion wbname
save temp1, replace

use "C:\Users\wb94976\Desktop\Box Sync\Database\Rawdata\Dec2015\master-dec-2015", clear
keep if year==2013
collapse  revgdp expgdp, by(income)
rename income wbname
save temp2, replace

use "C:\Users\wb94976\Desktop\Box Sync\Database\Rawdata\Dec2015\master-dec-2015", clear
keep if year==2013
keep wbname wbcode revgdp expgdp
append using temp1
append using temp2

replace wbname="Russia" if wbcode=="RUS"

gen sample=1 if wbcode=="IND" | wbcode=="LAO" | wbcode=="KHM" | wbcode=="CHN" | wbcode=="VNM" | wbcode=="PHL" | wbcode=="ZAF" | wbcode=="THA" | wbcode=="LKA" | wbcode=="RUS" | wbcode=="BRA" | wbcode=="MYS" | wbcode=="IDN" ///
| wbname=="East Asia & Pacific" | wbname=="Lower middle income"

graph bar revgdp expgdp if sample==1, legend(off) over(wbname, sort(1) label(angle(45))) ylab(0(10)50, grid) ytitle("Percent of GDP (%)") note("Source: IMF World Economic Outlook database")
graph export revexp.emf, replace


***** FERTILITY AND POPULATION GROWTH *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\Master\master-dec-2015", clear

drop if year>2013

twoway (line tfr year if wbcode=="IDN", sort ylab(, grid) legend(off) xlab(1960(10)2000 2013) ytitle(Average number of children)) /*
*/ (line popgrow year if wbcode=="IDN", yaxis(2) ytitle("Percentage (%)", axis(2)) xtitle(Year) note("Source: World Development Indicators database"))


*****Share of population aged 65 and over (projection)*****

use pop-projection.dta, clear

twoway (line above65 year if country=="Indonesia", clcolor(blue) clwidth(thick) legend(off) ylab(, grid) xlab(1950(20)2070) xtitle(Year) ytitle("Share of total population (%)") note("Source: UN Population projection")) ///
(line above65 year if country=="Thailand", clcolor(green) clpatter(dash)) ///
(line above65 year if country=="Philippines", clcolor(orange) clwidth(thick) clpatter(dash)) ///
(line above65 year if country=="China", color(green)) ///
(line above65 year if country=="Viet Nam", clcolor(red) color(red)), text(27 2040 "China" 19 2020 "Thailand" 20 2060 "Vietnam" 15 2050 "Indonesia" 7 2050 "Philippines", size(small))


***** POPULATION HEALTH OUTCOMES *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\Master\master-dec-2015", clear

drop if year>2013

twoway (line infant year if wbcode=="IDN", sort yscale(log) legend(off) xlab(1960(10)2000 2013)) /*
*/ (line under5 year if wbcode=="IDN", yscale(log) ylab(25 50 100 150 250, grid) ylab(45(5)75, axis(2)) ytitle("Mortality rate per 1,000 live births")) /*
*/ (line life year if wbcode=="IDN", sort yaxis(2) yscale(axis(2) log) clwidth(thick) ytitle(Years, axis(2)) /*
*/ xtitle(Year) text(200 1971 "Under-five mortality (left axis)" 100 1970 "Infant mortality (left axis)" 175 1998 "Life expectancy (right axis)") note("Source: World Development Indicators database" "Note: y-scales logged"))
graph export pophealth.emf, replace


***** LIFE EXPECTANCY VS INCOME *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\Master\master-dec-2015", clear

keep if year==2013
replace income="High income" if income=="High income: OECD"
replace income="High income" if income=="High income: nonOECD"
replace wbname="Russia" if wbcode=="RUS"

gen y=life
gen ly=ln(y)
gen lx=ln(ngnipc_atl)
gen sq=lx*lx
reg ly lx sq
predict ply 
gen py=exp(ply)

gen label1=wbname if wbcode=="RUS" | wbcode=="SLB" | wbcode=="PHL"
gen label2=wbname if wbcode=="NGA" | wbcode=="GHA"
gen label3=wbname if wbcode=="VNM" | wbcode=="KHM" | wbcode=="IND" | wbcode=="LAO"
gen label4=wbname if wbcode=="ZAF" | wbcode=="PNG" | wbcode=="LKA" | wbcode=="CHN" 
gen label5=wbname if wbcode=="THA" | wbcode=="BRA" 
gen label6=wbname if wbcode=="MYS"
gen label7=wbname if wbcode=="IDN"

twoway (scatteri 85 1045 85 4125, bcolor(gs15) recast(area) legend(off) subtitle(Life expectancy) saving(life, replace)) ///
(scatteri 85 12746 85 100000, bcolor(gs15) recast(area)) /// 
(scatter y ngnipc_atl, xscale(log) yscale(log) mcolor(ltkhaki) ylab(45(10)85, grid) xlab(250 500 1000 2500 10000 35000 100000) xtitle("GNI per capita, US$") ytitle("Years")) ///
(scatter y ngnipc_atl if label1!="", mlabc(black) mlab(label1) mlabp(3) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label2!="", mlabc(black) mlab(label2) mlabp(6) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label3!="", mlabc(black) mlab(label3) mlabp(9) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label4!="", mlabc(black) mlab(label4) mlabp(12) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label5!="", mlabc(black) mlab(label5) mlabp(7) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label6!="", mlabc(black) mlab(label6) mlabp(2) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label7!="", mlabc(red) mlab(label7) mfcolor(red) mlabp(9) mcolor(red)) ///
(line py ngnipc_atl, clpattern(dash) sort lcolor(red) text(45.5 500 "LOW INCOME" 46 2000 "LOWER" "MIDDLE" "INCOME" 46 7400 "UPPER" "MIDDLE" "INCOME" 45.5 40000 "HIGH INCOME", color(green) size(vsmall)))

***** INFANT VS INCOME *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\Master\master-dec-2015", clear

keep if year==2013
replace income="High income" if income=="High income: OECD"
replace income="High income" if income=="High income: nonOECD"
replace wbname="Russia" if wbcode=="RUS"

gen y=infant
gen ly=ln(y)
gen lx=ln(ngnipc_atl)
reg ly lx 
predict ply 
gen py=exp(ply)

gen label1=wbname if wbcode=="ZAF" | wbcode=="NGA" | wbcode=="RUS" | wbcode=="BRA" | wbcode=="PNG"
gen label2=wbname if wbcode=="PHL" 
gen label3=wbname if wbcode=="VNM" | wbcode=="KHM" | wbcode=="IND" | wbcode=="LAO" | wbcode=="THA" | wbcode=="SLB" 
gen label4=wbname if wbcode=="GHA" 
gen label5=wbname if wbcode=="CHN" 
gen label6=wbname if wbcode=="MYS" | wbcode=="LKA"
gen label7=wbname if wbcode=="IDN"

twoway (scatteri 125 1045 125 4125, bcolor(gs15) recast(area) legend(off) subtitle(Infant mortality) saving(infant, replace)) ///
(scatteri 125 12746 125 100000, bcolor(gs15) recast(area)) /// 
(scatter y ngnipc_atl, xscale(log) yscale(log) mcolor(ltkhaki) ylab(1 2 5 10 25 50 75 125, grid) xlab(250 500 1000 2500 10000 35000 100000) xtitle("GNI per capita, US$") ytitle("Rate per 1,000 live births")) ///
(scatter y ngnipc_atl if label1!="", mlabc(black) mlab(label1) mlabp(3) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label2!="", mlabc(black) mlab(label2) mlabp(6) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label3!="", mlabc(black) mlab(label3) mlabp(9) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label4!="", mlabc(black) mlab(label4) mlabp(12) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label5!="", mlabc(black) mlab(label5) mlabp(7) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label6!="", mlabc(black) mlab(label6) mlabp(9) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label7!="", mlabc(red) mlab(label7) mfcolor(red) mlabp(12) mcolor(red)) ///
(line py ngnipc_atl, clpattern(dash) sort lcolor(red) text(1.05 500 "LOW INCOME" 1.15 2000 "LOWER" "MIDDLE" "INCOME" 1.15 7400 "UPPER" "MIDDLE" "INCOME" 1.05 40000 "HIGH INCOME", color(green) size(vsmall)))

graph combine life.gph infant.gph, note("Source: World Development Indicators database" "Note: Both y- and x-axes logged")
graph export le-infant-income.emf, replace


***** BURDEN OF DISEASE *****

use master-dec-2015, clear
replace cd=30.2 if wbcode=="IDN" & year==2013
replace ncd=61.2 if wbcode=="IDN" & year==2013
replace injury=8.6 if wbcode=="IDN" & year==2013

graph pie ncd cd injury if year==1990 & wbcode=="IDN", pie(1, explode) plabel(_all percent, format(%9.0f)) legend(off) subtitle("1990") saving(1990, replace)
graph pie ncd cd injury if year==2000 & wbcode=="IDN", pie(1, explode) plabel(_all percent, format(%9.0f)) legend(off) subtitle("2000") saving(2000, replace)
graph pie ncd cd injury if year==2010 & wbcode=="IDN", pie(1, explode) plabel(_all percent, format(%9.0f)) legend(off) subtitle("2010") saving(2010, replace)
graph pie ncd cd injury if year==2013 & wbcode=="IDN", pie(1, explode) plabel(_all percent, format(%9.0f)) legend(off) subtitle("2013") saving(2013, replace)

graph combine 1990.gph 2000.gph 2010.gph 2013.gph, rows(1) note("Source:Institute of Health Metrics and Evaluation database")
graph export gbdidn.emf, replace



***** TOTAL HEALTH EXPENDITURES *****
 
use "C:\Users\wb94976\Desktop\Box Sync\Database\Master\master-dec-2015", clear

keep if year==2013
replace income="High income" if income=="High income: OECD"
replace income="High income" if income=="High income: nonOECD"
replace wbname="Russia" if wbcode=="RUS"

gen y=thegdp
gen ly=ln(y)
gen lx=ln(ngnipc_atl)
reg ly lx 
predict ply 
gen py=exp(ply)

gen label1=wbname if wbcode=="NGA" | wbcode=="RUS" | wbcode=="MYS" | wbcode=="GHA" | wbcode=="THA"
gen label2=wbname if wbcode=="PHL" 
gen label3=wbname if wbcode=="VNM" | wbcode=="KHM" | wbcode=="IND" | wbcode=="LAO" | wbcode=="SLB" | wbcode=="ZAF" | wbcode=="PNG"
gen label4=wbname if wbcode=="BRA"
gen label5=wbname if wbcode=="CHN" 
gen label6=wbname if wbcode=="LKA"
gen label7=wbname if wbcode=="IDN"

twoway (scatteri 20 1045 20 4125, bcolor(gs15) recast(area) legend(off) subtitle(Total health expenditure) saving(total, replace)) ///
(scatteri 20 12746 20 100000, bcolor(gs15) recast(area)) /// 
(scatter y ngnipc_atl, xscale(log) yscale(log) mcolor(ltkhaki) ylab(0.5 1 2 5 10 15 20, grid) xlab(250 500 1000 2500 10000 35000 100000) xtitle("GNI per capita, US$") ytitle("Share of GDP (%)")) ///
(scatter y ngnipc_atl if label1!="", mlabc(black) mlab(label1) mlabp(3) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label2!="", mlabc(black) mlab(label2) mlabp(0.5) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label3!="", mlabc(black) mlab(label3) mlabp(9) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label4!="", mlabc(black) mlab(label4) mlabp(12) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label5!="", mlabc(black) mlab(label5) mlabp(3) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label6!="", mlabc(black) mlab(label6) mlabp(9) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label7!="", mlabc(red) mlab(label7) mfcolor(red) mlabp(3) mcolor(red)) ///
(line py ngnipc_atl, clpattern(dash) sort lcolor(red) text(0.55 500 "LOW INCOME" 0.6 2000 "LOWER" "MIDDLE" "INCOME" 0.6 7400 "UPPER" "MIDDLE" "INCOME" 0.55 40000 "HIGH INCOME", color(green) size(vsmall)))

***** PUBLIC HEALTH EXPENDITURE *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\Master\master-dec-2015", clear

keep if year==2013
replace income="High income" if income=="High income: OECD"
replace income="High income" if income=="High income: nonOECD"
replace wbname="Russia" if wbcode=="RUS"

replace ghegdp=. if ghegdp==0

gen y=ghegdp
gen ly=ln(y)
gen lx=ln(ngnipc_atl)
reg ly lx 
predict ply 
gen py=exp(ply)

gen label1=wbname if wbcode=="NGA" | wbcode=="RUS" | wbcode=="MYS" | wbcode=="GHA" | wbcode=="THA"
gen label2=wbname if wbcode=="PHL" 
gen label3=wbname if wbcode=="VNM" | wbcode=="KHM" | wbcode=="IND" | wbcode=="LAO" | wbcode=="ZAF" | wbcode=="PNG"
gen label4=wbname if wbcode=="BRA" | wbcode=="SLB"
gen label5=wbname if wbcode=="CHN" 
gen label6=wbname if wbcode=="LKA"
gen label7=wbname if wbcode=="IDN"

twoway (scatteri 20 1045 20 4125, bcolor(gs15) recast(area) legend(off) subtitle(Public health expenditure) saving(public, replace)) ///
(scatteri 20 12746 20 100000, bcolor(gs15) recast(area)) /// 
(scatter y ngnipc_atl, xscale(log) yscale(log) mcolor(ltkhaki) ylab(0.5 1 2 5 10 15 20, grid) xlab(250 500 1000 2500 10000 35000 100000) xtitle("GNI per capita, US$") ytitle("Share of GDP (%)")) ///
(scatter y ngnipc_atl if label1!="", mlabc(black) mlab(label1) mlabp(3) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label2!="", mlabc(black) mlab(label2) mlabp(3) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label3!="", mlabc(black) mlab(label3) mlabp(9) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label4!="", mlabc(black) mlab(label4) mlabp(12) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label5!="", mlabc(black) mlab(label5) mlabp(6) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label6!="", mlabc(black) mlab(label6) mlabp(9) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label7!="", mlabc(red) mlab(label7) mfcolor(red) mlabp(3) mcolor(red)) ///
(line py ngnipc_atl, clpattern(dash) sort lcolor(red) text(0.55 500 "LOW INCOME" 0.6 2000 "LOWER" "MIDDLE" "INCOME" 0.6 7400 "UPPER" "MIDDLE" "INCOME" 0.55 40000 "HIGH INCOME", color(green) size(vsmall)))

graph combine total.gph public.gph, ycommon note("Source: World Development Indicators database" "Note: Both y- and x-axes logged")
graph export total-public-income.emf, replace

table income, c(m thegdp m ghegdp)
table wbregion, c(m thegdp m ghegdp)




***** TOTAL AND PUBLIC EXPENDITURES TRENDS *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\Rawdata\Dec2015\master-dec-2015", clear

drop if year>2013 
drop if year<1995

gen temp=gdp_deflator if year==2013
egen pcpi2013=mean(temp), by(wbcode)
gen realthepc=(thegdp/100)*imf_ngdpc_lcu*pcpi2013/gdp_deflator
gen realghepc=(ghegdp/100)*imf_ngdpc_lcu*pcpi2013/gdp_deflator

keep if wbcode=="IDN"
twoway (bar realthepc realghepc year, barwidth(.75 0.75) bcolor(black black) fcolor(ltkhaki orange) ylab(0(300000)1200000, grid) ytitle("IDR per capita")) ///
(line thegdp ghegdp year, clwidth(thick thick) clcolor(green blue) yaxis(2) c(l)  ytitle("Share of GDP (%)", axis(2)) ///
ylab(0(.5)3.5, axis(2)) xlab(1995(5)2010 2013) xtitle(Year) legend(off) note("Source: World Development Indicators database" "Note: Total and Public spending is in 2013 constant IDR") ///
text(800000 2008 "Total per capita (left axis)" 200000 2008 "Public per capita (left axis)" 1000000 2000 "Total as share of GDP (right axis)"  599000 2000 "Public as share of GDP (right axis)", size(small))) 
graph export idn-hexpc.emf, replace



***** Health's share of national budget *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\Master\master-dec-2015", clear
keep if year==2013

drop if ghebud==.
sort ghebud
gen obs=_n

replace wbname="Russia" if wbcode=="RUS"
gen label1=wbname if wbcode=="THA" | wbcode=="CHN" | wbcode=="ZAF" 
gen label2=wbname if wbcode=="LAO"   
gen label3=wbname if wbcode=="MYS" | wbcode=="IND"
gen label4=wbname if wbcode=="PHL" | wbcode=="KHM" | wbcode=="VNM"
gen label5=wbname if wbcode=="LKA" | wbcode=="BRA" | wbcode=="RUS"

twoway (dropline ghebud obs, xlab(-7(25)195, nolabels) ylab(0(5)35, grid) mfcolor(gs13) mcolor(gs13) lcolor(gs13) xtitle(" ") ytitle("Percentage (%)")) ///
(dropline ghebud obs if label1!="", mfcolor(black) mcolor(black) mlab(wbname) mlabc(black) mlabp(12) lcolor(black)) ///
(dropline ghebud obs if label2!="", mfcolor(black) mcolor(black) mlab(wbname) mlabc(black) mlabp(3) lcolor(black) legend(off)) ///
(dropline ghebud obs if label3!="", mfcolor(black) mcolor(black) mlab(wbname) mlabc(black) mlabp(9) lcolor(black) legend(off) note("Source: World Development Indicators database")) ///
(dropline ghebud obs if label4!="", mfcolor(black) mcolor(black) mlab("") mlabc(black) mlabp(3) lcolor(black)) ///
(dropline ghebud obs if label5!="", mfcolor(black) mcolor(black) mlab("") mlabc(black) mlabp(12) lcolor(black)) ///
(dropline ghebud obs if wbcode=="IDN", mfcolor(red) mcolor(red) mlab(wbname) mlabc(red) mlabp(10) lcolor(red)) 
graph export ghebud.emf, replace



***** COVERAGE AND OOP SPENDING ***** 

use master-dec-2015, clear
keep if year>=1995 & year<=2013
 
keep if wbcode=="IDN"
sort wbcode year
merge wbcode year using idn-coverage
keep if _merge==3

gen temp=gdp_deflator if year==2013
egen pcpi2013=mean(temp), by(wbcode)
gen double cooppc=((oopthe/100)*(thegdp/100))*imf_ngdpc_lcu*pcpi2013/gdp_deflator 

twoway (bar cooppc year, bcolor(black) fcolor(ltkhaki) ylab(, grid) ytitle("OOP spending per capita")) ///
(scatter oopthe year, clcolor(orange) yaxis(2) c(l) mcolor(orange) mfcolor(orange) msy(S) ytitle("Percentage (%)", axis(2)) ///
ylab(0(20)60, axis(2)) xlab(1995(5)2010 2013) xtitle(Year) note("Source: World Development Indicators database & SUSENAS (various years)" "Note: OOP spending is in 2013 constant IDR")) ///
(line coverage year, clwidth(thick) clcolor(green) legend(off) yaxis(2) clpattern(dash)) 



***** HF TRANSITION *****

use master-july-2015,clear 

keep if year>=1995 & year<=2013
gen oop1995=oopthe if year==1995
egen moop=mean(oop1995), by(wbcode)
keep if moop>20

gen pooledpc=(100-oopthe)*thepc_usd/100

egen group=group(wbname)
tsset group year

gen grpooled=D.pooledpc*100/L.pooledpc
gen groop=D.ooppc*100/L.ooppc

egen mgrpooled=mean(grpooled), by(wbcode)
egen mgroop=mean(groop), by(wbcode)

recode mgrpooled 20/max=. -10/min=.
recode mgroop 20/max=. -10/min=.
drop if mgroop<-10

bysort wbcode: keep if _n==1

twoway (scatteri 0 0 20 20, bcolor(gs15) recast(area)) ///
(scatteri -10 0 -10 20, bcolor(yellow) recast(area)) ///
(scatter mgroop mgrpooled, mcolor(ltkhaki) xlab(-5(5)20) ylab(-10(5)20, grid)) ///
(scatter mgroop mgrpooled if wbcode=="IDN", mfcolor(red) mlabp(3) mcolor(red) mlab(wbname) mlabc(red)) ///
(scatter mgroop mgrpooled if wbcode=="MYS", mcolor(black)) ///
(scatter mgroop mgrpooled if wbcode=="LAO" | wbcode=="LKA", mfcolor(black) mcolor(black) mlab(wbname) mlabp(3) mlabc(black)) ///
(scatter mgroop mgrpooled if wbcode=="VNM" | wbcode=="THA" | wbcode=="IND" | wbcode=="KHM", mfcolor(black) mcolor(black) mlabp(6) mlab(wbname) mlabc(black)) ///
(scatter mgroop mgrpooled if wbcode=="PHL" | wbcode=="CHN" | wbcode=="ZAF" | wbcode=="BRA", mfcolor(black) mcolor(black) mlab(wbname) mlabp(9) mlabc(black) legend(off)) ///
(pci 20 0 -10 0, lcolor(blue) ytitle("Annual change in OOP/capita health spending (%)") xtitle("Annual change in pooled/capita health spending (%)")) ///
(pci 0 -5 0 20, lcolor(blue) note("Source: World Development Indicators database" "Note: Data are for countries with OOP share>20% in 1995") saving(transition,replace) text(-2.5 17 "RAPID TRANSITION" 2.5 17 "SLOW TRANSITION" 15 2.5 "REGRESSING", size(small) color(green)))  



***** OOP SPENDING AND PUBLIC HEALTH SPENDING *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\master-aug-2015", clear

replace wbname="Russia" if wbcode=="RUS"
drop if ghegdp>15
keep if year==2013

gen label1=wbname if wbcode=="PHL" | wbcode=="RUS" 
gen label2=wbname if wbcode=="IND" | wbcode=="THA" | wbcode=="ZAF" 
gen label3=wbname if wbcode=="VNM" | wbcode=="KHM" | wbcode=="LKA"
gen label4=wbname if wbcode=="LAO"  
gen label5=wbname if wbcode=="BRA" | wbcode=="MYS" | wbcode=="CHN"
gen label6=wbname if wbcode=="IDN"

gen sq=ghegdp*ghegdp
reg oopthe ghegdp sq
predict poopthe if e(sample)
replace poopthe=. if ghegdp>9.5
replace poopthe=8.5 if wbcode=="MHL"

twoway (scatter oopthe ghegdp, xlab(0(3)15) mcolor(ltkhaki) ylab(0(20)80, grid) ytitle("OOP share of total health spending (%)") xtitle("Government health spending share of GDP (%)")) ///
(scatter oopthe ghegdp if label1!="", mlabc(black) mlab(label1) mfcolor(black) mcolor(black) legend(off)) ///
(scatter oopthe ghegdp if label2!="", mlabc(black) mlab(label2) mlabp(9) mfcolor(black) mcolor(black) legend(off)) ///
(scatter oopthe ghegdp if label3!="", mlabc(black) mlab(label3) mlabp(12) mfcolor(black) mcolor(black) legend(off)) ///
(scatter oopthe ghegdp if label4!="", mlabc(black) mlab(label4) mlabp(6) mfcolor(black) mcolor(black) legend(off)) ///
(scatter oopthe ghegdp if label5!="", mlabc(black) mlab(label5) mlabp(6) mfcolor(black) mcolor(black) legend(off)) ///
(scatter oopthe ghegdp if label6!="", mlabc(red) mlab(label6) mlabp(6) mfcolor(red) mcolor(red) legend(off)) ///
(line poopthe ghegdp, clpattern(dash) clcolor(red) sort legend(off)), note("Source: World Development Indicators database") 
graph export oopthe.emf, replace





***** EXTERNAL SHARE OF TOTAL HEALTH EXPENDITURE *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\Master\master-dec-2015", clear

keep if year>=2011 & year<=2013
egen mextthe=mean(extthe), by(wbcode)
replace extthe=mextthe

keep if year==2013
replace income="High income" if income=="High income: OECD"
replace income="High income" if income=="High income: nonOECD"
bysort income: summ extthe
replace extthe=. if extthe==0
replace extthe=. if wbcode=="THA"

gen y=extthe
gen ly=ln(y)
gen lx=ln(ngnipc_atl)
reg ly lx 
predict ply 
gen py=exp(ply)
replace py=. if py<0.5
replace y=0.45 if extthe==. | extthe<0.45

replace wbname="Russia" if wbcode=="RUS"

gen label1=wbname if wbcode=="IND" | wbcode=="LAO" | wbcode=="KHM"
gen label2=wbname if wbcode=="GHA" | wbcode=="NGA" | wbcode=="PHL"
gen label3=wbname if wbcode=="VNM" 
gen label4=wbname if wbcode=="ZAF" | wbcode=="LKA" | wbcode=="SLB" | wbcode=="PNG"
gen label5=wbname if wbcode=="RUS" | wbcode=="BRA" | wbcode=="MYS" | wbcode=="THA" | wbcode=="CHN"
gen label6=wbname if wbcode=="IDN"

replace y=2 if wbcode=="LKA"
twoway (scatteri 100 1045 100 4125, bcolor(gs15) recast(area)) ///
(scatteri 100 12746 100 100000, bcolor(gs15) recast(area)) ///
(scatter y ngnipc_atl, mcolor(ltkhaki) xlab(250 500 1000 2500 10000 35000 100000) ylab(1 2 5 10 20 50 100, grid) yscale(log) xscale(log)) ///
(scatter y ngnipc_atl if label1!="", mlabc(black) mlab(label1) mlabp(9) mcolor(black)) ///
(scatter y ngnipc_atl if label2!="", mlabc(black) mlab(label2) mlabp(12) mcolor(black)) ///
(scatter y ngnipc_atl if label3!="", mlabc(black) mlab(label3) mlabp(12) mcolor(black)) ///
(scatter y ngnipc_atl if label4!="", mlabc(black) mlab(label4) mcolor(black)) ///
(scatter y ngnipc_atl if label5!="", mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label6!="", mlabc(red) mlab(label6) mfcolor(red) mlabp(3) mcolor(red) legend(off)) ///
(line py ngnipc_atl, clpattern(dash) sort lcolor(red) ytitle("Share of total health expenditure (%)") xtitle("GNI per capita, US$") ///
text(0.55 500 "LOW INCOME" 0.6 2000 "LOWER" "MIDDLE" "INCOME" 0.6 7400 "UPPER" "MIDDLE" "INCOME" 0.55 40000 "HIGH INCOME", color(green) size(vsmall)) ///
note("Source: World Development Indicators database") saving(extthe, replace))
graph export extthe.emf, replace




***** EXTERNAL SHARE TRENDS *****

use master-july-2015, clear

replace extthe=4.8 if wbcode=="IDN" & year==1995
replace extthe=1.4 if wbcode=="IDN" & year==1996
replace extthe=5.2 if wbcode=="IDN" & year==1997
replace extthe=11.5 if wbcode=="IDN" & year==1998
replace extthe=11.6 if wbcode=="IDN" & year==1999
replace extthe=10.8 if wbcode=="IDN" & year==2000
replace extthe=4.4 if wbcode=="IDN" & year==2001
replace extthe=3.3 if wbcode=="IDN" & year==2002
replace extthe=3.5 if wbcode=="IDN" & year==2003
replace extthe=3.0 if wbcode=="IDN" & year==2004
replace extthe=4.6 if wbcode=="IDN" & year==2005
replace extthe=2.3 if wbcode=="IDN" & year==2006

drop if year<1995
drop if year>2013
twoway (bar extthe year if wbcode=="IDN", barwidth(.75) xlab(1995(5)2010 2013) xtitle(Year) ylab(0(5)15, grid) ytitle("Share of total health expenditure (%)") bcolor(black) fcolor(ltkhaki)) ///
, note("Source: World Development Indicators database")
graph export extthe-trend.emf, replace


******Population aged 0-1 years, 1995-2030****

use pop0-1
drop if year>2030

gen spop0_1=(pop0_1/totpop)*100
replace pop0_1=pop0_1/1000000

format spop0_1 %2.1f
twoway (bar pop0_1 year if year<2015, fcolor(orange) lcolor(orange)) ///
(bar pop0_1 year if year>2014, fcolor(ltkhaki) lcolor(ltkhaki)) ///
(line spop0_1 year, sort yaxis(2) ylab(1(0.25)2.5, axis(2)) lwidth(thick) ytitle("Percentage (%)", axis(2)) lcolor(green) ) ///
, legend(off) ylab(, grid) ytitle("Millions") xlab(1995(5)2030) xtitle("Year") ///
text(4 2005 "Population" 4 2025 "Population projection" 4.2 2025 "0-1 share of total population (right axis)", size(small) ) ///
note("Source: www.census.gov")




***** IMMUNIZATION COVERAGE VS INCOME *****

use master-july-2015, clear

keep if year==2013
replace wbname="Russia" if wbcode=="RUS"
drop if dpt<60

gen y=dpt
gen ly=ln(y)
gen lx=ln(ngnipc_atl)
reg ly lx 
predict ply 
gen py=exp(ply)

gen label1=wbname if wbcode=="ZAF" | wbcode=="RUS" | wbcode=="BRA" | wbcode=="PHL" 
gen label2=wbname if wbcode=="THA" 
gen label3=wbname if wbcode=="VNM" | wbcode=="KHM" | wbcode=="IND" | wbcode=="LAO"  
gen label4=wbname if wbcode=="MYS" 
gen label5=wbname if wbcode=="CHN" 
gen label6=wbname if wbcode=="LKA"
gen label7=wbname if wbcode=="IDN"

twoway (scatteri 100 1045 100 4125, bcolor(gs15) recast(area) legend(off) subtitle(DPT) saving(dpt, replace)) ///
(scatteri 100 12746 100 100000, bcolor(gs15) recast(area)) /// 
(scatter y ngnipc_atl, xscale(log) yscale(log) mcolor(ltkhaki) ylab(60(10)100, grid) xlab(250 500 1000 2500 10000 35000 100000) xtitle("GNI per capita, US$") ytitle("Percentage (%)")) ///
(scatter y ngnipc_atl if label1!="", mlabc(black) mlab(label1) mlabp(3) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label2!="", mlabc(black) mlab(label2) mlabp(7) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label3!="", mlabc(black) mlab(label3) mlabp(9) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label4!="", mlabc(black) mlab(label4) mlabp(12) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label5!="", mlabc(black) mlab(label5) mlabp(6) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label6!="", mlabc(black) mlab(label6) mlabp(9) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label7!="", mlabc(red) mlab(label7) mfcolor(red) mlabp(12) mcolor(red)) ///
(line py ngnipc_atl, clpattern(dash) sort lcolor(red) text(60 500 "LOW INCOME" 61 2000 "LOWER" "MIDDLE" "INCOME" 61 7400 "UPPER" "MIDDLE" "INCOME" 60 40000 "HIGH INCOME", color(green) size(vsmall)))

use master-july-2015, clear

keep if year==2013
replace wbname="Russia" if wbcode=="RUS"
drop if measles<60

gen y=measles
gen ly=ln(y)
gen lx=ln(ngnipc_atl)
reg ly lx 
predict ply 
gen py=exp(ply)

gen label1=wbname if wbcode=="ZAF" | wbcode=="BRA" | wbcode=="PHL" 
gen label2=wbname if wbcode=="THA" 
gen label3=wbname if wbcode=="KHM" | wbcode=="IND" | wbcode=="LAO"  
gen label4=wbname if wbcode=="MYS" | wbcode=="RUS" | wbcode=="VNM"
gen label5=wbname if wbcode=="CHN" 
gen label6=wbname if wbcode=="LKA"
gen label7=wbname if wbcode=="IDN"

twoway (scatteri 100 1045 100 4125, bcolor(gs15) recast(area) legend(off) subtitle(Measles) saving(measles, replace)) ///
(scatteri 100 12746 100 100000, bcolor(gs15) recast(area)) /// 
(scatter y ngnipc_atl, xscale(log) yscale(log) mcolor(ltkhaki) ylab(60(10)100, grid) xlab(250 500 1000 2500 10000 35000 100000) xtitle("GNI per capita, US$") ytitle("Percentage (%)")) ///
(scatter y ngnipc_atl if label1!="", mlabc(black) mlab(label1) mlabp(3) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label2!="", mlabc(black) mlab(label2) mlabp(7) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label3!="", mlabc(black) mlab(label3) mlabp(9) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label4!="", mlabc(black) mlab(label4) mlabp(6) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label5!="", mlabc(black) mlab(label5) mlabp(6) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label6!="", mlabc(black) mlab(label6) mlabp(9) mfcolor(black) mcolor(black) legend(off)) ///
(scatter y ngnipc_atl if label7!="", mlabc(red) mlab(label7) mfcolor(red) mlabp(12) mcolor(red)) ///
(line py ngnipc_atl, clpattern(dash) sort lcolor(red) text(60 500 "LOW INCOME" 61 2000 "LOWER" "MIDDLE" "INCOME" 61 7400 "UPPER" "MIDDLE" "INCOME" 60 40000 "HIGH INCOME", color(green) size(vsmall)))

graph combine dpt.gph measles.gph, note("Source: World Development Indicators database" "Note: Both y- and x-axes logged")
graph export dpt-measles-income.emf, replace



***** HALE vs TOTAL HEALTH SPENDING *****

use "C:\Users\wb94976\Desktop\Box Sync\Database\Master\master-dec-2015", clear
keep if year==2013 

replace wbname="Russia" if wbcode=="RUS"
gen label1=wbname if wbcode=="PHL" | wbcode=="RUS" 
gen label2=wbname if wbcode=="IND" | wbcode=="THA" | wbcode=="ZAF" 
gen label3=wbname if wbcode=="KHM" | wbcode=="LKA"
gen label4=wbname if wbcode=="LAO" | wbcode=="VNM"  
gen label5=wbname if wbcode=="BRA" | wbcode=="MYS" | wbcode=="CHN"
gen label6=wbname if wbcode=="IDN"

twoway (scatter hale_who thepc_usd, xscale(log) mcolor(ltkhaki) legend(off) ytitle("Health-adjusted life expectancy") ylab(, grid)) ///
(scatter hale_who thepc_usd if label1!="", mcolor(black) mlab(wbname) mlabp(3) mlabc(black)) ///
(scatter hale_who thepc_usd if label2!="", mcolor(black) mlab(wbname) mlabp(3) mlabc(black)) ///
(scatter hale_who thepc_usd if label3!="", mcolor(black) mlab(wbname) mlabp(9) mlabc(black)) ///
(scatter hale_who thepc_usd if label4!="", mcolor(black) mlab(wbname) mlabp(12) mlabc(black)) ///
(scatter hale_who thepc_usd if label5!="", mcolor(black) mlab(wbname) mlabp(9) mlabc(black)) ///
(scatter hale_who thepc_usd if wbcode=="IDN", mlabc(red) mlab(wbname) mlabp(3) mcolor(red) xlab(25 50 100 250 1000 2500 7500) xtitle("Total health expenditure per capita, US$")) ///
, note("Source: World Development Indicators database") 

