log using RunAnalysis.log, replace

ssc install rdrobust,   replace
ssc install rddensity,  replace
ssc install rdplot,     replace
ssc install outreg2,    replace

*******************************************************************************
***	This file contains code to produce the Tables and Figures
***	found in Ericson, Keith. "Consumer Inertia and Firm Pricing in
***	 the Medicare Part D Prescription Drug Insurance Exchange"
***
***	Requires: Data_main.dta, Data_subsidyinfo.dta
***	Produces: Table#.xml (#=2-4) and Figure#.gph (#=1,5)
***	Table 1 in the text is copied from descriptive statistics
***	(this location is noted in this do file)
***
***	Structure: 
***		PART 1: Prepare program and supplemental data
***		PART 2: Prepare main data (create derivative variables used later) 
***		PART 3a: Prepare Tables & Figures
***		PART 3b: Do additional data manipulation for RD results
***		PART 3c: Produce regression discontinuity (RD) Tables & Figures
***	File is most easily viewed using a text editor with indent-based folding
*******************************************************************************
set matsize 400
// local dirName = "Analysis_output"
local dirName = "C:\Users\YWA3857\Downloads\DataFiles"
capture mkdir `dirName'

***PART 1: Prepare program and supplemental data
	***Define a useful program
		capture program drop defineSample
		program define defineSample
			capture drop inLastSampletemp inLastSample
			gen inLastSampletemp =1 if e(sample)
				replace inLastSampletemp = 0 if inLastSampletemp!=1
			egen inLastSample = max(inLastSampletemp),by(uniqueID)
		end
	***Prepare some supplemental data
		cd "C:\Users\YWA3857\Downloads\DataFiles"
		use Data_subsidyinfo.dta, replace
			reshape long s, i(PDPregion) j(year)
			sort PDPregion year
			tempfile theSubsidies
		save `theSubsidies', replace
	
	***We will make frequent use of this plugin to produce clear tables
		ssc install outreg2

***PART 2: Prepare main data (create derivative variables used later)
	cd "C:\Users\YWA3857\Downloads\DataFiles"
	use Data_main.dta
	
	***The var "isin" will tell me whether that plan existed in 200x
		gen isin = 1
	
	***Now, create information about companies
		***Create a numeric version of the text string; useful for stata
		egen firmID = group(orgParentCode)
		egen uniqueIDNum = group(uniqueID)
		
			***Create variables that show when a firm began offering a plan
				egen firstYrExist_F0 = min(year),by(firmID)
				label var firstYrExist_F0 "The first year this plan's company offered a plan"
						
				gen thisPlansExist_F0 = 1 if year >firstYrExist_F0 
				replace thisPlansExist_F0 =0 if thisPlansExist_F0 !=1
				label var thisPlansExist_F0  "=1 if company previously offerred a plan in any state,=0 if else"
				
				***Want to know the first year that company offered a plan in that state.
				egen firstYrExistState_F0 = min(year),by(firmID state)
				label var firstYrExistState_F0 "The first year this plan's company offered a plan, firm def 0"
					
				gen thisPlansExistState_F0 = 1 if year >firstYrExistState_F0 
				replace thisPlansExistState_F0 =0 if thisPlansExistState_F0 !=1
				label var thisPlansExistState_F0  "=1 if company (def 0) previously offerred a plan in any state,=0 if else"
		
	***Generate descriptive cohort variables
		egen minYear = min(year),by(uniqueID)
		
		foreach yr in 2006 2007 2008 2009 2010{
			gen cohort`yr' = 1 if minYear == `yr'
		}
	
		gen cohort = .
		foreach x in 2006 2007 2008 2009 2010{
			replace cohort = `x' if cohort`x'==1
		}
		
		replace cohort2006=0 if cohort2006 !=1
		replace cohort2007=0 if cohort2007 !=1
		replace cohort2008=0 if cohort2008 !=1
		replace cohort2009=0 if cohort2009 !=1
		replace cohort2010=0 if cohort2010 !=1
	
	
		***Create the year of plan's existence
		gen yearOfPlan = year-cohort+1
		replace yearOfPlan = . if yearOfPlan<1 
	
		tab year, generate(Dyear)
		tab yearOfPlan, generate(DOfPlan)
		rename DOfPlan1 _DOfPlan1
		rename Dyear1 _Dyear1
		egen maxYear = max(year),by(uniqueID)
	
		***Indicator for if a plan is basic every year until YR
		gen isBasic = 1 if benefit=="B"
		replace isBasic = 0 if benefit!="B"
		egen minIsBasic = min(isBasic),by(uniqueID)
	
		foreach yr in 2007 2008 2009 2010{
			gen isBasic`yr'= isBasic
			replace isBasic`yr'=1 if year >`yr'
			egen minIsBasic`yr' = min(isBasic`yr'),by(uniqueID)
			drop isBasic`yr'
		} 
	
	***Handle Premium
		label var premium "Monthly Premium"
		gen lnPremium = ln(premium)
	
	***Create Variables Describing plan detail
		***Various types of basic plans
			gen DS = 1 if btypedetail =="DS"
			replace DS = 0 if btypedetail !="DS"
			gen AE = 1 if btypedetail =="AE"
			replace AE = 0 if btypedetail !="AE"
		
		foreach y in 2006 2007 2008 2009 2010{ 
			gen AE`y' = 1 if year == `y' & btypedetail =="AE"
			replace AE`y'= 0 if AE`y'!=1
			gen DS`y' = 1 if year == `y' & btypedetail =="DS"
			replace DS`y'= 0 if DS`y'!=1
		}	
	
		***Create the interaction between BA and deductible amount
		***AE and DS always have the same deductible (varies by year). 
		****But BA may have varying deductibles
		
		***Generate Deductible Groups
			gen cDeduct0 = 1 if deductible ==0
			gen cDeduct1_50 = 1 if deductible >0 & deductible<=50
			gen cDeduct51_100 = 1 if deductible >50 & deductible<=100
			gen cDeduct101_150 = 1 if deductible >100 & deductible<=150
			gen cDeduct151_200 = 1 if deductible >150 & deductible<=200
			gen cDeduct201_250 = 1 if deductible >200 & deductible<=250
			gen cDeduct251_300 = 1 if deductible >250 & deductible<=300
			gen cDeduct301_ = 1 if deductible >300 & deductible !=.
		
			foreach x in cDeduct0  cDeduct1_50   cDeduct51_100  cDeduct101_150 cDeduct151_200 cDeduct201_250 cDeduct251_300 cDeduct301_{
				replace `x' = 0 if `x' ==.
			}
			
			foreach x in cDeduct0  cDeduct1_50   cDeduct51_100  cDeduct101_150 cDeduct151_200 cDeduct201_250 cDeduct251_300 cDeduct301_{
				foreach y in 2006 2007 2008 2009 2010{ 
					gen `x'`y' = `x' if year == `y'
					replace `x'`y'= 0 if  year != `y'
				}
			}
			assert cDeduct0  + cDeduct1_50 +   cDeduct51_100   + cDeduct101_150  + cDeduct151_200 +  cDeduct201_250 + cDeduct251_300+cDeduct301_==1 if deductible !=.
		
			***And focus only on the BA plans
			foreach x in cDeduct0  cDeduct1_50   cDeduct51_100  cDeduct101_150 cDeduct151_200 cDeduct201_250 cDeduct251_300 cDeduct301_{
				foreach y in 2006 2007 2008 2009 2010{ 
					gen BA`x'`y' = `x' if year == `y' & btypedetail=="BA"
					replace BA`x'`y'= 0 if  BA`x'`y'==.
				}
			}
	
			
	****For lagged data analysis, create lagged plan type/deductible groups
		xtset uniqueIDNum year, yearly
		sort uniqueIDNum year
		***Stata lags can't handle string variables lagged. Must do manually
			gen L4btypedetail=btypedetail[_n-4] if uniqueID==uniqueID[_n-4] & year == 2010 
			gen L3btypedetail=btypedetail[_n-3] if uniqueID==uniqueID[_n-3] & (year == 2010 | year == 2009) 
			gen L2btypedetail=btypedetail[_n-2] if uniqueID==uniqueID[_n-2] & (year == 2010 | year == 2009 | year ==2008)
			gen L1btypedetail=btypedetail[_n-1] if uniqueID==uniqueID[_n-1] & (year == 2010 | year == 2009 | year ==2008 | year==2007)
			gen L0btypedetail=btypedetail
			
			gen L4benefit=benefit[_n-4] if uniqueID==uniqueID[_n-4] & year == 2010 
			gen L3benefit=benefit[_n-3] if uniqueID==uniqueID[_n-3] & (year == 2010 | year == 2009) 
			gen L2benefit=benefit[_n-2] if uniqueID==uniqueID[_n-2] & (year == 2010 | year == 2009 | year ==2008)
			gen L1benefit=benefit[_n-1] if uniqueID==uniqueID[_n-1] & (year == 2010 | year == 2009 | year ==2008 | year==2007)
			gen L0benefit=benefit
	
		
		foreach L in 0 1 2 3 4{
			***For basic types only
			gen L`L'DS = 1 if L`L'btypedetail =="DS"
			replace L`L'DS = 0 if L`L'btypedetail !="DS"
			gen L`L'AE = 1 if L`L'btypedetail =="AE"
			replace L`L'AE = 0 if L`L'btypedetail !="AE"
		
			****BUT BA may have varying deductibles
			gen  L`L'BA_0 = 1 if  L`L'.deductible ==0 & L`L'btypedetail =="BA"
			gen  L`L'BA_1_99 = 1 if L`L'.deductible >0 & L`L'.deductible<100 & L`L'btypedetail =="BA"
			gen  L`L'BA_100 = 1 if L`L'.deductible==100 & L`L'btypedetail =="BA"
			gen  L`L'BA_101_99 = 1 if L`L'.deductible >100 & L`L'.deductible<200 & L`L'btypedetail =="BA"
			gen  L`L'BA_200_49 = 1 if L`L'.deductible >=200 & L`L'.deductible<250 & L`L'btypedetail =="BA"
			gen  L`L'BA_250Up = 1 if L`L'.deductible >=250 & L`L'.deductible!=. & L`L'btypedetail =="BA"
	
			foreach x in L`L'BA_0 L`L'BA_1_99 L`L'BA_100 L`L'BA_101_99 L`L'BA_200_49 L`L'BA_250Up{
				replace `x' = 0 if `x' !=1
			}
		
			***Make sure everyone is in a category
			egen x = rowtotal(L`L'DS-L`L'BA_250Up)
			assert x==1 if L`L'benefit =="B" & L`L'btypedetail!=""
			drop x
		}
	
	***Work with Enrollment Data
		egen double stateYrEnroll =sum(enrollment),by(state year)
		gen share = enrollment/stateYrEnroll
		gen double lnS = log(share)
		
		gen enrollmentNonLIS = enrollment-enrollmentLIS
		egen double stateYrEnrollNonLIS =sum(enrollmentNonLIS),by(state year)
		gen shareNonLIS = enrollmentNonLIS/stateYrEnrollNonLIS
		gen double lnSNonLIS = log(shareNonLIS)
		
		***When LIS enrollment is missing b/c <10, it is imputed to be 5
		gen enrollmentNonLISimpute = enrollment-enrollmentLISimpute
		egen double stateYrEnrollNonLISimpute =sum(enrollmentNonLISimpute),by(state year)
		
		gen shareNonLISimpute = enrollmentNonLISimpute/stateYrEnrollNonLISimpute
		gen double lnS_std = log(shareNonLISimpute)
		
		
* Replot figure 3
****************************************************
* SETTINGS
****************************************************
local x LISPremium
local cutoff = 0
local year = 2006

****************************************************
* CREATE LEFT / RIGHT VARIABLES
****************************************************
capture drop LISPremiumNeg LISPremiumPos
gen LISPremiumNeg = `x' if `x' < 0
replace LISPremiumNeg = 0 if `x' >= 0

gen LISPremiumPos = `x' if `x' >= 0
replace LISPremiumPos = 0 if `x' < 0

****************************************************
* POLYNOMIAL TERMS
****************************************************
capture drop LISPremiumNegSq LISPremiumPosSq LISPremiumNegCub LISPremiumPosCub LISPremiumNegQuart LISPremiumPosQuart

gen LISPremiumNegSq = LISPremiumNeg^2
gen LISPremiumPosSq = LISPremiumPos^2
gen LISPremiumNegCub = LISPremiumNeg^3
gen LISPremiumPosCub = LISPremiumPos^3
gen LISPremiumNegQuart = LISPremiumNeg^4
gen LISPremiumPosQuart = LISPremiumPos^4

****************************************************
* GET SUPPORT
****************************************************
summ `x' if year==`year'
local x_l = r(min)
local x_u = r(max)
local x_bar = `cutoff'

****************************************************
* ===== CASE 1: J = 10 =====
****************************************************
local J = 10

capture drop bin_temp bin_id
gen bin_temp = .

* LEFT BINS
forval j = 1/`J' {
    local lower = `x_l' + (`j'-1)*(`x_bar'-`x_l')/`J'
    local upper = `x_l' + `j'*(`x_bar'-`x_l')/`J'
    local mid = (`lower' + `upper')/2
    replace bin_temp = `mid' if `x' >= `lower' & `x' < `upper' & `x' < `x_bar'
}

* RIGHT BINS
forval j = 1/`J' {
    local lower = `x_bar' + (`j'-1)*(`x_u'-`x_bar')/`J'
    local upper = `x_bar' + `j'*(`x_u'-`x_bar')/`J'
    local mid = (`lower' + `upper')/2
    replace bin_temp = `mid' if `x' >= `lower' & `x' < `upper' & `x' >= `x_bar'
}

egen bin_id = group(bin_temp)

****************************************************
* REGRESSIONS
****************************************************
capture drop yhat_bin yhat_ll yhat_poly

reg `y' i.bin_id if year==`year' & benefit=="B", cluster(firmID)
predict yhat_bin if e(sample)

reg `y' belowBench2006 LISPremiumNeg LISPremiumPos if year==`year' & benefit=="B", cluster(firmID)
predict yhat_ll if e(sample)

reg `y' belowBench2006 LISPremiumNeg LISPremiumPos LISPremiumNegSq LISPremiumPosSq LISPremiumNegCub LISPremiumPosCub LISPremiumNegQuart LISPremiumPosQuart if year==`year' & benefit=="B", cluster(firmID)
predict yhat_poly if e(sample)

****************************************************
* PLOT J = 10
****************************************************
twoway (scatter yhat_bin bin_temp if year==`year' & benefit=="B") (line yhat_ll `x' if year==`year' & benefit=="B", sort lpattern(dash)) (line yhat_poly `x' if year==`year' & benefit=="B", sort), legend(order(2 3) label(2 "Local Linear") label(3 "Quartic Polynomial")) xtitle("Monthly Premium - LIS Subsidy, 2006") ytitle("Log Enrollment Share, 2006")

graph save Figure_J10.gph, replace

****************************************************
* ===== CASE 2: J = 30 =====
****************************************************
local J = 30

capture drop bin_temp bin_id
gen bin_temp = .

* LEFT BINS
forval j = 1/`J' {
    local lower = `x_l' + (`j'-1)*(`x_bar'-`x_l')/`J'
    local upper = `x_l' + `j'*(`x_bar'-`x_l')/`J'
    local mid = (`lower' + `upper')/2
    replace bin_temp = `mid' if `x' >= `lower' & `x' < `upper' & `x' < `x_bar'
}

* RIGHT BINS
forval j = 1/`J' {
    local lower = `x_bar' + (`j'-1)*(`x_u'-`x_bar')/`J'
    local upper = `x_bar' + `j'*(`x_u'-`x_bar')/`J'
    local mid = (`lower' + `upper')/2
    replace bin_temp = `mid' if `x' >= `lower' & `x' < `upper' & `x' >= `x_bar'
}

egen bin_id = group(bin_temp)


*****PART 3B: Prepare for Regression Discontinuity Results
	sort PDPregion year
	merge PDPregion year using `theSubsidies'
	assert _merge==3
	drop _merge
	rename s LISsubsidy

	gen LISPremium =  premium - LISsubsidy
	***Not all proposed plans are actually such
	gen proposedBenchmarkPlan = 1 if LISPremium <= 0
	replace proposedBenchmarkPlan = 0 if  proposedBenchmarkPlan != 1
		
	sum LISPremium, detail
	sum LISPremium if LIS==0, detail
	sum LISPremium if LIS==1, detail
	****SOME MISCATEGORIZATION 
	generate ProblemObs =1 if LISPremium < 0 & LIS == 0
	replace ProblemObs =2 if LISPremium > 0 & LIS == 1
	tab ProblemObs
	***Why positive premiums when LIS == 1? DEMINIMUM PROVISION: note that this problem only occurs in 2007+
		sum LISPremium if ProblemObs==2, detail 
		tab ProblemObs year
	***Why Negative premiums when seemingly not eligible? 
		sum LISPremium if ProblemObs==1, detail 
		tab benefit if  ProblemObs==1
		tab btypedetail if  ProblemObs==1
	****FIX: Not eligible for LIS if benefit == E
		replace LISPremium = . if benefit == "E"
		replace proposedBenchmarkPlan =.  if benefit == "E" 
		
	***Polynomials
		gen LISPremiumSq = LISPremium*LISPremium
		gen LISPremiumCub= LISPremium*LISPremium*LISPremium
		gen LISPremiumQuart= LISPremium*LISPremium*LISPremium*LISPremium
		***Interacted with Status
		gen LISPremiumSq_IS = LISPremium*LISPremium*LIS
		gen LISPremiumCub_IS= LISPremium*LISPremium*LISPremium*LIS
		gen LISPremiumQuart_IS= LISPremium*LISPremium*LISPremium*LISPremium*LIS
		
		gen premiumSq= premium*premium
		gen premiumCub= premium*premium*premium
		gen premiumQuart =premium*premium*premium*premium

	***Splitting by either side of the benchmark
		gen LISPremiumNeg =LISPremium if LISPremium<=0
			replace LISPremiumNeg = 0 if LISPremium>0
		gen LISPremiumPos =LISPremium if LISPremium>=0
			replace LISPremiumPos = 0 if LISPremium<0
		foreach x in Neg Pos{
			gen LISPremium`x'Sq = LISPremium`x'*LISPremium`x'
			gen LISPremium`x'Cub = LISPremium`x'*LISPremium`x'*LISPremium`x'
			gen LISPremium`x'Quart = LISPremium`x'*LISPremium`x'*LISPremium`x'*LISPremium`x'
		}	
	***Lags
	xtset uniqueIDNum year, yearly
	foreach x in 1 2 3 4{
		gen L`x'LISPremium = L`x'.LISPremium
		gen L`x'LISPremiumNeg = L`x'.LISPremiumNeg
		gen L`x'LISPremiumPos = L`x'.LISPremiumPos
		
		gen  L`x'LISPremiumNegSq =  L`x'.LISPremiumNegSq
		gen  L`x'LISPremiumNegCub = L`x'.LISPremiumNegCub
		gen  L`x'LISPremiumNegQuart= L`x'.LISPremiumNegQuart

		gen  L`x'LISPremiumPosSq =  L`x'.LISPremiumPosSq
		gen  L`x'LISPremiumPosCub = L`x'.LISPremiumPosCub
		gen  L`x'LISPremiumPosQuart= L`x'.LISPremiumPosQuart
	}
	
	***Variable for attrition (could be merged with another plan)
		foreach x in 2006 2007 2008 2009 {
			gen attritBy`x' =1 if maxYear <=`x'
			replace attritBy`x' =0  if maxYear >`x'
		}
	
	***Do past indicators for LIS status
		gen alwaysLIS07 = 1 if LIS ==1 & L1.LIS==1 & year ==2007
			replace alwaysLIS07 = 0 if alwaysLIS07 !=1
		gen alwaysLIS08 = 1 if LIS ==1 & L1.LIS==1 & L2.LIS==1 & year ==2008
			replace alwaysLIS08 = 0 if alwaysLIS08 !=1
		gen alwaysLIS09 = 1 if LIS ==1 & L1.LIS==1 & L2.LIS==1 & L3.LIS==1 & year ==2009
			replace alwaysLIS09 = 0 if alwaysLIS09 !=1
		gen alwaysLIS10 = 1 if LIS ==1 & L1.LIS==1 & L2.LIS==1 & L3.LIS==1 & L4.LIS==1 & year ==2010
			replace alwaysLIS10 = 0 if alwaysLIS10 !=1              
	***Never LIS again (after 2006)
		gen neverLIS07 = 1 if LIS ==0 & year ==2007
			replace neverLIS07 = 0 if neverLIS07 !=1
		gen neverLIS08 = 1 if LIS ==0 & L1.LIS==0 & year ==2008
			replace neverLIS08 = 0 if neverLIS08 !=1
		gen neverLIS09 = 1 if LIS ==0 & L1.LIS==0 & L2.LIS==0 & year ==2009
			replace neverLIS09 = 0 if neverLIS09 !=1
		gen neverLIS10 = 1 if LIS ==0 & L1.LIS==0 & L2.LIS==0 & L3.LIS==0 & year ==2010
			replace neverLIS10 = 0 if neverLIS10 !=1              
		
	***CREATE RD Windows		
		******Now, split sample above and below the benchmark subsidy: +/- $10
			sum LISPremium
			***Approx One standard deviation
			gen RDwindow = 1 if LISPremium >= -10 & LISPremium <= 10
			replace RDwindow = 0 if RDwindow != 1  	
			
			gen belowBench = 1 if LISPremium <=0 & RDwindow == 1
			replace belowBench = 0 if LISPremium >0 & RDwindow == 1
			
			***Cleanest experiment: Look at plans above/below the benchmark in 2006
			gen belowBench2006Temp = 1 if belowBench==1 & year ==2006
				replace belowBench2006Temp =0 if belowBench2006Temp!=1
			gen RDwindow2006Temp = 1 if RDwindow==1 & year ==2006
				replace RDwindow2006Temp =0 if RDwindow2006Temp!=1
			gen LISsubsidy2006Temp = LISsubsidy if year == 2006
				replace LISsubsidy2006Temp = 0 if year != 2006
		
			egen belowBench2006=max(belowBench2006Temp), by(uniqueID)
			egen RDwindow2006=max(RDwindow2006Temp), by(uniqueID)
			egen LISsubsidy2006=max(LISsubsidy2006Temp), by(uniqueID)
		
		***Now, take a tighter RD: $4 (optimal for lnS) 	
			local x = 2
			***Approx One standard deviation
			gen RDwindow`x' = 1 if LISPremium >= -4 & LISPremium <= 4
			replace RDwindow`x' = 0 if RDwindow`x' != 1  	
			
			gen belowBench`x' = 1 if LISPremium <=0 & RDwindow`x' == 1
			replace belowBench`x' = 0 if LISPremium >0 & RDwindow`x' == 1
			
			***Cleanest experiment: Look at plans above/below the benchmark in 2006
			gen belowBench2006Temp`x' = 1 if belowBench`x'==1 & year ==2006
				replace belowBench2006Temp`x' =0 if belowBench2006Temp`x'!=1
			gen RDwindow2006Temp`x' = 1 if RDwindow`x'==1 & year ==2006
				replace RDwindow2006Temp`x' =0 if RDwindow2006Temp`x'!=1
		
			egen belowBench2006`x'=max(belowBench2006Temp`x'), by(uniqueID)
			egen RDwindow2006`x'=max(RDwindow2006Temp`x'), by(uniqueID)
		
		***$2.50 bandwidth: smaller than is ever optimal	
			local x = 3
			***Approx One standard deviation
			gen RDwindow`x' = 1 if LISPremium >= -2.5 & LISPremium <= 2.5
			replace RDwindow`x' = 0 if RDwindow`x' != 1  	
			
			gen belowBench`x' = 1 if LISPremium <=0 & RDwindow`x' == 1
			replace belowBench`x' = 0 if LISPremium >0 & RDwindow`x' == 1
			
			***Cleanest experiment: Look at plans above/below the benchmark in 2006
			gen belowBench2006Temp`x' = 1 if belowBench`x'==1 & year ==2006
				replace belowBench2006Temp`x' =0 if belowBench2006Temp`x'!=1
			gen RDwindow2006Temp`x' = 1 if RDwindow`x'==1 & year ==2006
				replace RDwindow2006Temp`x' =0 if RDwindow2006Temp`x'!=1
		
			egen belowBench2006`x'=max(belowBench2006Temp`x'), by(uniqueID)
			egen RDwindow2006`x'=max(RDwindow2006Temp`x'), by(uniqueID)
		***$6 bandwidth: optimal for LISPremium	
			local x = 4
			***Approx One standard deviation
			gen RDwindow`x' = 1 if LISPremium >= -6 & LISPremium <= 6
			replace RDwindow`x' = 0 if RDwindow`x' != 1  	
			
			gen belowBench`x' = 1 if LISPremium <=0 & RDwindow`x' == 1
			replace belowBench`x' = 0 if LISPremium >0 & RDwindow`x' == 1
			
			***Cleanest experiment: Look at plans above/below the benchmark in 2006
			gen belowBench2006Temp`x' = 1 if belowBench`x'==1 & year ==2006
				replace belowBench2006Temp`x' =0 if belowBench2006Temp`x'!=1
			gen RDwindow2006Temp`x' = 1 if RDwindow`x'==1 & year ==2006
				replace RDwindow2006Temp`x' =0 if RDwindow2006Temp`x'!=1
		
			egen belowBench2006`x'=max(belowBench2006Temp`x'), by(uniqueID)
			egen RDwindow2006`x'=max(RDwindow2006Temp`x'), by(uniqueID)
			
	***Now, just another variable for largest window (for parallelism)
		gen RDwindow20061 =RDwindow2006

	***Now, examine interactions:
	gen bench0607 = 1 if belowBench2006==1 & LIS ==1 & year ==2007
		replace bench0607  = 0 if bench0607==.  
	gen bench06Not07 = 1 if belowBench2006==1 & LIS ==0 & year ==2007
		replace bench06Not07  = 0 if bench06Not07==.
	gen benchNot06Yes07 = 1 if belowBench2006==0 & LIS ==1 & year ==2007
		replace benchNot06Yes07 = 0 if benchNot06Yes07==.

		foreach yr in 2007 2008 2009 2010{
			gen bench06`yr'= 1 if belowBench2006==1 & LIS ==1 & year ==`yr'
				replace bench06`yr'  = 0 if bench06`yr'==.  
			gen bench06Not`yr' = 1 if belowBench2006==1 & LIS ==0 & year ==`yr'
				replace bench06Not`yr'  = 0 if bench06Not`yr'==.
			gen benchNot06Yes`yr' = 1 if belowBench2006==0 & LIS ==1 & year ==`yr'
				replace benchNot06Yes`yr' = 0 if benchNot06Yes`yr'==.
		}

		

****************************************************
* ===== RUN AFTER PART 3B ONLY =====
****************************************************
****************************************************
* SETTINGS
****************************************************
local y lnS
local x LISPremium
local year 2006
local cutoff 0

* Make sure sample exists
count if year==`year' & benefit=="B" & RDwindow2006==1

****************************************************
* ===== CASE 1: J = 10 =====
****************************************************

****************************************************
* CCT BIN SELECTION
****************************************************
count if year==`year' & benefit=="B" & RDwindow2006==1
local n = r(N)

* Optimal bins ~ n^(1/3)
local J = ceil(`n'^(1/3))
local nBinOver2 = ceil(`J'/2)

display "Sample size = `n'"
display "Total bins = `J'"
display "Bins per side = `nBinOver2'"

****************************************************
* STEP SIZE
****************************************************
local h = 10
local step = `h'/`nBinOver2'

display "Step size = `step'"

****************************************************
* BIN CONSTRUCTION
****************************************************
capture drop theBinAl theBinAlTemp
gen theBinAlTemp = .

* LEFT side
forval j = 1/`nBinOver2' {
    local lower = -`step'*`j'
    local upper = -`step'*(`j'-1)
    local mid   = (`lower' + `upper')/2

    replace theBinAlTemp = `mid' if LISPremium >= `lower' & LISPremium < `upper' & year==`year' & RDwindow2006==1
}

* RIGHT side
forval j = 1/`nBinOver2' {
    local lower = `step'*(`j'-1)
    local upper = `step'*`j'
    local mid   = (`lower' + `upper')/2

    replace theBinAlTemp = `mid' if LISPremium >= `lower' & LISPremium < `upper' & year==`year' & RDwindow2006==1
}

egen theBinAl = max(theBinAlTemp), by(uniqueID)

* Convert to integer bin index (CRITICAL FIX)
capture drop bin_id
egen bin_id = group(theBinAl)

****************************************************
* REGRESSIONS
****************************************************
capture drop lnSHat lnSHatAlt lnSHatAltPoly

* BIN MEANS (scatter)
reg `y' i.bin_id if year==`year' & benefit=="B" & RDwindow2006==1, cluster(firmID)
predict lnSHat if e(sample)

* LOCAL LINEAR (optimal bandwidth window 2)
reg `y' belowBench2006 LISPremiumNeg LISPremiumPos if year==`year' & benefit=="B" & RDwindow20062==1, cluster(firmID)
predict lnSHatAlt if e(sample)

* QUARTIC (same window as scatter)
reg `y' belowBench2006 LISPremiumNeg LISPremiumPos LISPremiumNegSq LISPremiumPosSq LISPremiumNegCub LISPremiumPosCub LISPremiumNegQuart LISPremiumPosQuart if year==`year' & benefit=="B" & RDwindow2006==1, cluster(firmID)
predict lnSHatAltPoly if e(sample)

****************************************************
* PLOT
****************************************************
twoway (scatter lnSHat theBinAl if year==`year' & benefit=="B" & RDwindow2006==1) (line lnSHatAlt LISPremium if year==`year' & benefit=="B" & RDwindow20062==1, sort lpattern(dash) lcolor(gray)) (line lnSHatAltPoly LISPremium if year==`year' & benefit=="B" & RDwindow2006==1, sort lpattern(solid) lcolor(black)), legend(order(2 3) label(2 "Local Linear") label(3 "Quartic Polynomial")) xtitle("Monthly Premium - LIS Subsidy, 2006") ytitle("Log Enrollment Share, 2006")

graph save `dirName'/Figure3_10.gph, replace


****************************************************
* ===== CASE 1: J = 30 =====
****************************************************

local h 30
count if year==`year' & benefit=="B" & abs(`x') <= `h'
local n = r(N)

local J = ceil(`n'^(1/3))
local nBinOver2 = ceil(`J'/2)

display "Sample size = `n'"
display "Total bins = `J'"
display "Bins per side = `nBinOver2'"

capture drop theBinAl theBinAlTemp
gen theBinAlTemp = .

forval j = 1/`nBinOver2' {
    local lower = -`step'*`j'
    local upper = -`step'*(`j'-1)
    local mid = (`lower' + `upper')/2
    replace theBinAlTemp = `mid' if `x' >= `lower' & `x' < `upper' & year==`year' & abs(`x') <= `h'
}

forval j = 1/`nBinOver2' {
    local lower = `step'*(`j'-1)
    local upper = `step'*`j'
    local mid = (`lower' + `upper')/2
    replace theBinAlTemp = `mid' if `x' >= `lower' & `x' < `upper' & year==`year' & abs(`x') <= `h'
}

egen theBinAl = max(theBinAlTemp), by(uniqueID)

capture drop bin_id
egen bin_id = group(theBinAl)

capture drop LISPremiumNeg LISPremiumPos
gen LISPremiumNeg = `x' if `x' < 0
replace LISPremiumNeg = 0 if `x' >= 0

gen LISPremiumPos = `x' if `x' >= 0
replace LISPremiumPos = 0 if `x' < 0

capture drop LISPremiumNegSq LISPremiumPosSq LISPremiumNegCub LISPremiumPosCub LISPremiumNegQuart LISPremiumPosQuart
gen LISPremiumNegSq = LISPremiumNeg^2
gen LISPremiumPosSq = LISPremiumPos^2
gen LISPremiumNegCub = LISPremiumNeg^3
gen LISPremiumPosCub = LISPremiumPos^3
gen LISPremiumNegQuart = LISPremiumNeg^4
gen LISPremiumPosQuart = LISPremiumPos^4

twoway (scatter lnSHat theBinAl if year==`year' & benefit=="B" & abs(`x') <= `h') (line lnSHatAlt `x' if year==`year' & benefit=="B" & RDwindow20062==1, sort lpattern(dash) lcolor(gray)) (line lnSHatAltPoly `x' if year==`year' & benefit=="B" & abs(`x') <= `h', sort lpattern(solid) lcolor(black)), legend(order(2 3) label(2 "Local Linear") label(3 "Quartic Polynomial")) xtitle("Monthly Premium - LIS Subsidy, 2006") ytitle("Log Enrollment Share, 2006")

graph save `dirName'/Figure3_h30.gph, replace




*******************************************************************************
*** QUESTION 4: Optimal ES bin number + recreate binned scatterplots
*******************************************************************************

preserve
keep if year == 2006 & benefit == "B" & LISPremium != . & lnS != .

rdplot lnS LISPremium, c(0) binselect(es) graph_options(title("Figure Q4: Optimal ES Bins (2006, Basic Plans)") xtitle("Monthly Premium - LIS Subsidy, 2006") ytitle("Log Enrollment Share, 2006"))
graph save "Figure_Q4_OptimalES.gph", replace

* rdplot stores J_star in e() — print them
di "Optimal left bins  (J_ES,-): " e(J_star_l)
di "Optimal right bins (J_ES,+): " e(J_star_r)

restore

* ── 4b. J = 10 bins (replicates Q3 left panel) ───────────────────────────────
preserve
keep if year == 2006 & benefit == "B" & LISPremium != . & lnS != .

rdplot lnS LISPremium, c(0) nbins(10 10) graph_options(title("Figure Q4b: J=10 Bins") xtitle("Monthly Premium - LIS Subsidy, 2006") ytitle("Log Enrollment Share, 2006"))
graph save "Figure_Q4b_J10.gph", replace

restore

* ── 4c. J = 30 bins (replicates Q3 right panel) ──────────────────────────────
preserve
keep if year == 2006 & benefit == "B" & LISPremium != . & lnS != .

rdplot lnS LISPremium, c(0) nbins(30 30) graph_options(title("Figure Q4c: J=30 Bins") xtitle("Monthly Premium - LIS Subsidy, 2006") ytitle("Log Enrollment Share, 2006"))
graph save "Figure_Q4c_J30.gph", replace

restore


*******************************************************************************
*** QUESTION 5: Manipulation test — Cattaneo, Jansson & Ma (2018)
*******************************************************************************



net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace


preserve
keep if year == 2006 & benefit == "B" & LISPremium != .

rddensity LISPremium, c(0) all
* Stored results: e(T_q) = test statistic, e(pv_q) = p-value

* Density plot with confidence bands
rddensity LISPremium, c(0) plot graph_opt(title("Figure Q5: Density Test (2006)") xtitle("Monthly Premium - LIS Subsidy") ytitle("Density"))
graph save "Figure_Q5_DensityTest.gph", replace

restore


*******************************************************************************
*** QUESTION 6: Recreate Table 3 Panels A & B — bandwidth $4, no covariates
*******************************************************************************


preserve
keep if year == 2006 & benefit == "B" & RDwindow2 == 1 & LISPremium != .

* --- Panel A: outcome = lnS --------------------------------------------------

reg lnS belowBench2006 LISPremiumNeg LISPremiumPos, cluster(firmID)
outreg2 using "Table3_PanelA.xml", replace ctitle("Local Linear") addtext(Bandwidth, "$4") nocons

reg lnS belowBench2006 LISPremiumNeg LISPremiumPos LISPremiumNegSq LISPremiumPosSq, cluster(firmID)
outreg2 using "Table3_PanelA.xml", append ctitle("Quadratic") nocons

reg lnS belowBench2006 LISPremiumNeg LISPremiumPos LISPremiumNegSq LISPremiumPosSq LISPremiumNegCub LISPremiumPosCub, cluster(firmID)
outreg2 using "Table3_PanelA.xml", append ctitle("Cubic") nocons

reg lnS belowBench2006 LISPremiumNeg LISPremiumPos LISPremiumNegSq LISPremiumPosSq LISPremiumNegCub LISPremiumPosCub LISPremiumNegQuart LISPremiumPosQuart, cluster(firmID)
outreg2 using "Table3_PanelA.xml", append ctitle("Quartic") nocons

* --- Panel B: outcome = LISPremium (checks for jump in running variable) -----

reg LISPremium belowBench2006 LISPremiumNeg LISPremiumPos, cluster(firmID)
outreg2 using "Table3_PanelB.xml", replace ctitle("Local Linear") addtext(Bandwidth, "$4") nocons

reg LISPremium belowBench2006 LISPremiumNeg LISPremiumPos LISPremiumNegSq LISPremiumPosSq, cluster(firmID)
outreg2 using "Table3_PanelB.xml", append ctitle("Quadratic") nocons

reg LISPremium belowBench2006 LISPremiumNeg LISPremiumPos LISPremiumNegSq LISPremiumPosSq LISPremiumNegCub LISPremiumPosCub, cluster(firmID)
outreg2 using "Table3_PanelB.xml", append ctitle("Cubic") nocons

reg LISPremium belowBench2006 LISPremiumNeg LISPremiumPos LISPremiumNegSq LISPremiumPosSq LISPremiumNegCub LISPremiumPosCub LISPremiumNegQuart LISPremiumPosQuart, cluster(firmID)
outreg2 using "Table3_PanelB.xml", append ctitle("Quartic") nocons

restore


*******************************************************************************
*** QUESTION 7: CE-optimal bandwidth via Calonico, Cattaneo & Farrell (2020)
*******************************************************************************

preserve
keep if year == 2006 & benefit == "B" & LISPremium != . & lnS != .

* MSE-optimal (replicates Ericson's implicit approach)
rdrobust lnS LISPremium, c(0) bwselect(mserd) kernel(uniform)
di "--- MSE-optimal ---"
di "Bandwidth (left/right): " e(h_l) " / " e(h_r)
di "RD point estimate:      " e(tau_cl)
di "Robust p-value:         " e(pv_rb)

* CE-optimal (Calonico, Cattaneo & Farrell 2020)
rdrobust lnS LISPremium, c(0) bwselect(cerrd) kernel(uniform)
di "--- CE-optimal ---"
di "Bandwidth (left/right): " e(h_l) " / " e(h_r)
di "RD point estimate:      " e(tau_cl)
di "Robust p-value:         " e(pv_rb)

restore


*******************************************************************************
*** QUESTION 8: IV — LIS benchmark status instruments 2006 market share
***              Outcome: future premium changes
*******************************************************************************

* ── Step 1: generate premium changes (full panel, outside preserve) ───────────
xtset uniqueIDNum year, yearly
gen dLnPremium = lnPremium - L1.lnPremium
label var dLnPremium "YoY change in log monthly premium"

* ── Step 2: build 2006 base cross-section ────────────────────────────────────
preserve
keep if year == 2006 & benefit == "B"
keep uniqueID lnS lnPremium premium belowBench2006 RDwindow2006 LISPremium LISPremiumNeg LISPremiumPos firmID state PDPregion
rename lnS       lnS_2006
rename lnPremium lnPremium_2006
rename premium   premium_2006
tempfile base2006
save `base2006', replace
restore

* ── Step 3: merge future years onto 2006 base ────────────────────────────────
preserve
keep if year > 2006 & benefit == "B"
keep uniqueID year lnPremium dLnPremium
rename lnPremium lnPremium_t

merge m:1 uniqueID using `base2006'
keep if _merge == 3
drop _merge

gen dLnPrem_from06 = lnPremium_t - lnPremium_2006
label var dLnPrem_from06 "Log premium change relative to 2006"

* --- First Stage: belowBench2006 predicts 2006 log market share --------------
di "===== FIRST STAGE ====="
foreach yr in 2007 2008 2009 2010 {
    reg lnS_2006 belowBench2006 LISPremiumNeg LISPremiumPos if year == `yr' & RDwindow2006 == 1, cluster(firmID)
    outreg2 using "Table_IV_FirstStage.xml", append ctitle("`yr'") nocons
}

* --- Reduced Form: benchmark status -> future premium change -----------------
di "===== REDUCED FORM ====="
foreach yr in 2007 2008 2009 2010 {
    reg dLnPrem_from06 belowBench2006 LISPremiumNeg LISPremiumPos if year == `yr' & RDwindow2006 == 1, cluster(firmID)
    outreg2 using "Table_IV_ReducedForm.xml", append ctitle("`yr'") nocons
}

* --- 2SLS: instrument lnS_2006 with belowBench2006 ---------------------------
* Positive beta => plans with higher 2006 share raised premiums more later
* => consistent with inertia giving firms pricing power over captive enrollees
di "===== 2SLS (IV) ====="
foreach yr in 2007 2008 2009 2010 {
    ivregress 2sls dLnPrem_from06 LISPremiumNeg LISPremiumPos (lnS_2006 = belowBench2006) if year == `yr' & RDwindow2006 == 1, cluster(firmID) first
    outreg2 using "Table_IV_2SLS.xml", append ctitle("`yr'") nocons
}

restore









