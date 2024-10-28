clear all
global folder "C:\Users\Nico Voigtländer\Dropbox\dark side social capital\Replication"

cd "$folder"
use Dataset_Bowling_Replication_JPE.dta, clear

set logtype text
log using Log_Bowling_JPE_Paper.txt, replace

********************************************* Generate variables ****************************************************
quietly{
*Log entry rates:
gen lnNSentry_total=ln(1+NSentry_total)
label var lnNSentry_total "log total (Falter) Party entries 1925-01/33"
gen lnNSentry_FU_total=ln(1+NSentry_FU_total)
label var lnNSentry_total "log total (Falter-corrected) Party entries 1925-01/33"
gen lnclubs_all = ln(clubs_all)

*Population:
gen pop2 = (pop25/1000)^2
gen pop3 = (pop25/1000)^3
gen lnpop25=ln(pop25)
label var pop2 "(pop'25)^2"
label var pop3 "(pop'25)^3"
label var lnpop25 "ln(pop) in 1925"

*Dummies for size quintiles
xtile pop25_quintiles = pop25, nq(5)
tab  pop25_quintiles, gen(d_pop_quintile)

*Population density:
gen lnpop_density=ln(pop_density)
gen dummy_maps=1 if area_source =="Maps"
recode dummy_maps .=0
gen i_popden_maps = lnpop_density*dummy_maps

*Government stability 
pca govt_longest_perc party_longest_perc weimar_coalition_perc
predict govt_stability, score

xtile govt_stability_AM = govt_stability if landweimar!="Preußen", nq(2)
replace govt_stability_AM= govt_stability_AM-1 if landweimar!="Preußen"
replace govt_stability_AM= 0 if landweimar=="Preußen"

*Principal Component for IV analysis
pca turnmembers_pc singersfest_members_pc
predict IV_pca
label var IV_pca "First Principal Component of turnmembers_pc singersfest_members_pc"

}
*


///////////////////////////////////////////////////////////////////////////////
//////////////////////////// FIGURES -- Main Paper ////////////////////////////
///////////////////////////////////////////////////////////////////////////////


/////////////////////////////// Figure 1 ////////////////////////////////////

preserve
	gen cumentyear1925=entries_FU25
	forval x=26/32 {
		local i=`x'-1
		gen cumentyear19`x'=cumentyear19`i'+entries_FU`x'
	}

	*keep only the required vars  
	keep cityid cument* clubs_pc_AM pop25

	*reshape into long format
	reshape long cumentyear@, i(cityid) j(year)

	*renaming and relabeling for clarity
	ren cumentyear cum_entry
	la var cum_entry "Cumulative total party entries" 
	la var clubs_pc_AM "Club density above median"

	*take means of pcentries, by tercile and year
	collapse (sum) cum_entry pop25, by(year clubs_pc_AM)

	gen cum_entry_pc=1000*(cum_entry/pop25)

	*generate graph
	twoway (connected cum_entry_pc year if clubs_pc_AM==1, mcolor(black) msize(vlarge) msymbol(circle) lcolor(black) lwidth(medthick) lpattern(solid)) ///
	(connected cum_entry_pc year if clubs_pc_AM==0, msize(large) msymbol(circle_hollow) ///
	mfcolor(white) mlcolor(dkgreen) mlwidth(medthick) lcolor(dkgreen) lwidth(medthick) lpattern(dash)), ytitle(Cumulative entry (per 1,000), size(large)) xtitle(Year, size(large)) ///
	legend(order(0 "Association density:" 1 "above median" 2 "below median") textfirst cols(1) position(9) ring(0) size(medlarge)) scheme(s1color) xlabel(1925(1)1932, labsize(medlarge)) ///
	ylabel(, labsize(medlarge))
restore

	**For discussion in text: "The effect is quantitatively important, corresponding to a 27% difference in Nazi Party entry rates over the period January 1925-January 1933."
	reg pcNSentry_total clubs_pc_AM, r
	display(.1501259/.553916) //coefficient is the extra entry for above-median; constant is average for below-median

	
***************************************************************************************************************************************************************************

/////////////////////////////// Figure 3 ////////////////////////////////////	

reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r beta 
	avplot clubs_all_pc, recast(scatter) mcolor(navy) msize(small) msymbol(diamond) mlabel(city) mlabcolor(navy) mlabsize(small) rlopts(lcolor(black)) ytitle(NSDAP entry rate (residual)) ///
	ytitle(, size(medlarge)) xtitle(Association density (residual)) xtitle(, size(medlarge)) note(, size(medsmall) ring(0)) note("") scheme(s1color)
	

*************** Figure 4: NSDAP votes and assoc density ************************	
*Figure 4a
reg pcNSDAP285 pcNSentry_early_std lnpop25 share_cath25 bcollar25, r 
	avplot pcNSentry_early_std, recast(scatter) mcolor(navy) msize(small) msymbol(diamond) mlabel(city) mlabcolor(navy) mlabsize(small) rlopts(lcolor(black)) ytitle(NSDAP vote share 1928 (residual)) ///
	ytitle(, size(medlarge)) xtitle(NSDAP entry rate until 1928 (residual)) xtitle(, size(medlarge)) note(, size(medium) ring(0) position(5)) note("coeff=2.96 se=0.40") scheme(s1color)
*Figure 4b
reg pcNSDAP309 pcNSentry_pre1930_std lnpop25 share_cath25 bcollar25, r 
	avplot pcNSentry_pre1930_std, recast(scatter) mcolor(navy) msize(small) msymbol(diamond) mlabel(city) mlabcolor(navy) mlabsize(small) rlopts(lcolor(black)) ytitle(NSDAP vote share 1930 (residual)) ///
	ytitle(, size(medlarge)) xtitle(NSDAP entry rate until 1930 (residual)) xtitle(, size(medlarge)) note(, size(medium) ring(0) position(5)) note("coeff=4.30 se=0.59") scheme(s1color)
*Figure 4c
reg pcNSDAP333 pcNSentry_std lnpop25 share_cath25 bcollar25, r 
	avplot pcNSentry_std, recast(scatter) mcolor(navy) msize(small) msymbol(diamond) mlabel(city) mlabcolor(navy) mlabsize(small) rlopts(lcolor(black)) ytitle(NSDAP vote share 1930 (residual)) ///
	ytitle(, size(medlarge)) xtitle(NSDAP entry rate until 1930 (residual)) xtitle(, size(medlarge)) note(, size(medium) ring(0) position(5)) note("coeff=2.84 se=0.53") scheme(s1color)

	
*Figure 5: see below, where Table 7 
	



*********************************************************************************************************************************************************************************		
************************************************************ TABLES - Main Paper *************************************************************************************************	
*********************************************************************************************************************************************************************************	



////////////////////////////////////////////////// Table 1 -- Overview ///////////////////////////////////////////////////	


*****Socio-econ variables:
quietly{
preserve
	use "$folder\ZA8013_Sozialdaten_cleaned.dta", clear  

	gen bcollar25 = C25ARBEI/C25WOHN
	gen agric_share25 = C25BLAND/C25WOHN //% ERWERBST.I.D. LAND  U. FORSTWIRTSCHAFT
	gen ind_share25 = C25BWERK/C25WOHN //% BERUFSZUGEH. INDUSTRIE UND HANDWERK
	gen selfemp25 = C25SELB/C25WOHN //% Self employed overall
	gen share_cath25=C25KATH/C25POP
	gen share_jew25=C25JUDEN/C25POP
	gen unemp33 =  C33ERLOS/C33ERWP
	noi: display("Reich, urban with pop>5,000:")
	noi: sum C25POP bcollar25 share_cath25 share_jew25 unemp33 if (AGGLVL==8 | AGGLVL==4) & C25POP>5000 // & C25POP>2000 // only include individual towns (Gemeinden) and cities(Stadtkreise)
	noi: display("Reich, all:")
	noi: sum  bcollar25 share_cath25 share_jew25 unemp33 if (AGGLVL==5 | AGGLVL==4) [aweight=C25POP] // only include Kreise and Stadtkreise
restore
noi: display("Our Sample:")
noi: sum C25POP bcollar25 share_cath25 share_jew25 unemp33
}

*****Election results:
quietly{
preserve
	use "$folder\ZA8013_Wahldaten_cleaned.dta" , clear  

	gen pcNSDAP333 = N333NSDA/N333GS*100
	gen pcKPD333 = N333KPD/N333GS*100
	gen pcSPD333 = N333SPD/N333GS*100
	gen pcZentrum333 = N333ZX/N333GS*100
	noi: display("Reich, urban with pop>5,000:")
	noi: sum pcNSDAP333 pcSPD333 pcZentrum333 pcKPD333 if (AGGLVL==8 | AGGLVL==4) & N285POP>5000 // only include individual towns (Gemeinden) and cities(Stadtkreise)
	noi: display("Reich, all:")
	noi: sum pcNSDAP333 pcSPD333 pcZentrum333 pcKPD333 if (AGGLVL==5 | AGGLVL==4) [aweight=N333GS] // only include Kreise and Stadtkreise
restore
noi: display("Our Sample:")
noi: sum pcNSDAP333 pcSPD333 pcZentrum333 pcKPD333 
} 
******************************************************************************************************************************************


  
////////////////////////////////////////////////// Table 2 -- Balancedness ///////////////////////////////////////////////////				

**Individual correlations with clubs and NS entry:
*First for baseline controls:
eststo clear
foreach var of varlist lnpop25 share_cath25 bcollar25 {
	eststo: reg clubs_all_pc `var', r beta
	eststo: reg pcNSentry_std `var', r beta
	display("************************************")
	}

*Now for other variables, controlling for baseline controls: 
eststo clear
foreach var of varlist share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop ///
		                hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg {
	eststo: reg clubs_all_pc `var' lnpop25 share_cath25 bcollar25, r beta
	eststo: reg pcNSentry_std `var' lnpop25 share_cath25 bcollar25, r beta
	display("************************************")
	} 
	

*Joint F-test for significance of all variables:
*1) Club density
reg clubs_all_pc lnpop25 share_cath25 bcollar25 share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop ///
				  hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg, r
	*conditional on baseline controls:
	test share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg

*2) Party density				  
reg pcNSentry_std lnpop25 share_cath25 bcollar25 share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop ///
				  hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg, r				
	*conditional on baseline controls:
	test share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg

*Omnibus test suggested by editor -- first for all variables
reg pcNSentry_std lnpop25 share_cath25 bcollar25 share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop ///
				  hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg, r
predict pcentry_hat if e(sample)
eststo: reg pcentry_hat clubs_all_pc, r beta 
eststo: reg pcentry_hat clubs_all_pc lnpop25 share_cath25 bcollar25, r beta
				  
******************************************************************************************************************************************



////////////////////////////////////////////////// Tables 3 -- Main Results and Robustness ///////////////////////////////////////////////////				

*Table 3, Panel A:
eststo clear
/*1*/eststo: reg lnNSentry_total lnclubs_all lnpop25 share_cath25 bcollar25, r beta
listcoef lnclubs_all
/*2*/eststo: reg lnNSentry_FU_total lnclubs_all lnpop25 share_cath25 bcollar25, r beta
listcoef lnclubs_all
/*3*/eststo: reg pcNSentry_std clubs_all_pc, r
/*4*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r beta
/*5*/eststo: reg pcNSentry_std clubs_civic_pc share_cath25 lnpop25 bcollar25, r beta
/*6*/eststo: reg pcNSentry_std clubs_nonCivic_pc share_cath25 lnpop25 bcollar25, r beta
esttab , se beta star compress keep(clubs_all_pc clubs_civic_pc clubs_nonCivic_pc) //view beta coefficients


*Table 3, Panel B:		
*Additional controls: Enikopolov et al data, vote shares in early 1920s, and clustering:
local controls_socioecon "share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop"
local controls_political "hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg"		
eststo clear
/*1*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, cl(landweimar)
/*2*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25 `controls_socioecon', cl(landweimar)
/*3*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political', cl(landweimar)
/*4*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political' dummy_land*, cl(landweimar)
/*5*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political' dummy_land*, cl(landweimar)
/*6*/eststo: reg pcNSentry_std clubs_nonCivic_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political' dummy_land*, cl(landweimar)
esttab , se beta star compress keep(clubs_all_pc clubs_civic_pc clubs_nonCivic_pc) //view beta coefficients


	*For footnote 23: effect of removing Passau: from baseline reg:
	/*4*/reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r, if cityid~=751 //cityid 751 is Passau

	*For footnote 24: Random effects model: estimate model with Weimar-state-level random effects and report the share of the residual variance that is at the state level
	xtset landweimar_num
	/*4*/ xtreg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, re
	
******************************************************************************************************************************************


////////////////////////////////////////////////// TABLE 4: Election Results and Mediation ///////////////////////////////////////////////////	

*Mediation: How much of the 'effect' of clubs on election results goes through party entry?

*Table 4, PANEL A:
eststo clear
*reduced form ("Model with dv regressed on iv"):
/*1*/eststo: reg pcNSDAP285 clubs_all_pc lnpop25 share_cath25 bcollar25, r
/*2*/eststo: reg pcNSDAP309 clubs_all_pc lnpop25 share_cath25 bcollar25, r
/*3*/eststo: reg pcNSDAP333 clubs_all_pc lnpop25 share_cath25 bcollar25, r
*First stage: "Model with mediator regressed on iv":
/*4*/eststo: reg pcNSentry_early_std clubs_all_pc lnpop25 share_cath25 bcollar25, r
/*5*/eststo: reg pcNSentry_pre1930_std clubs_all_pc lnpop25 share_cath25 bcollar25, r
/*6*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r
esttab , se beta star compress keep(clubs_all_pc) //view beta coefficients

*Table 4, PANEL B: (cols 1-3)
eststo clear
*"Model with dv regressed on mediator and iv":
/*1*/eststo: reg pcNSDAP285 pcNSentry_early_std clubs_all_pc lnpop25 share_cath25 bcollar25, r
sum clubs_all_pc pcNSDAP285 if e(sample)
/*2*/eststo: reg pcNSDAP309 pcNSentry_pre1930_std clubs_all_pc lnpop25 share_cath25 bcollar25, r
sum clubs_all_pc pcNSDAP309 if e(sample)
/*3*/eststo: reg pcNSDAP333 pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r
sum clubs_all_pc pcNSDAP333 if e(sample)
esttab , se beta star compress keep(clubs_all_pc pcNSentry_early_std pcNSentry_pre1930_std pcNSentry_std) //view beta coefficients


*PANEL B: (cols 4-6)
sgmediation pcNSDAP285, iv(clubs_all_pc) mv(pcNSentry_early_std) cv(lnpop25 share_cath25 bcollar25)
	disp(1.571497*.48343/4.758155) //standardized indirect effect = std(clubs_all_pc)*Sobel_Coefficient*std(pcNSDAP285)
sgmediation pcNSDAP309, iv(clubs_all_pc) mv(pcNSentry_pre1930_std) cv(lnpop25 share_cath25 bcollar25)
	disp(1.571497*.729729/8.694133) //standardized indirect effect
sgmediation pcNSDAP333, iv(clubs_all_pc) mv(pcNSentry_std) cv(lnpop25 share_cath25 bcollar25)
	disp(1.571497*.422989/9.829142) //standardized indirect effect
	

*************************************************************************************************************************************


////////////////////////////////////////////////// Table 5: Early and Late Entry ///////////////////////////////////////////////////	

*Table 5:
estimates clear
local controls_socioecon "share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop"
local controls_political "hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg" 
/*1*/eststo: reg pcNSentry_early_std clubs_all_pc lnpop25 share_cath25 bcollar25, r 
/*2*/eststo: reg pcNSentry_early_std clubs_all_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political', cl(landweimar) 
/*3*/eststo: reg pcNSentry_late_std clubs_all_pc lnpop25 share_cath25 bcollar25, r 
/*4*/eststo: reg pcNSentry_late_std clubs_all_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political', cl(landweimar) 
/*5*/eststo: reg pcNSentry_late_std clubs_all_pc pcNSentry_early_std lnpop25 share_cath25 bcollar25, r 
/*6*/eststo: reg pcNSentry_late_std clubs_all_pc pcNSentry_early_std lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political', cl(landweimar) 
esttab , se beta star compress keep(clubs_all_pc pcNSentry_early_std) //view beta coefficients

	****Sobel-Goodman test for cols 5 and 6: 
	local controls_socioecon "share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop"
	local controls_political "hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg" 
		sgmediation pcNSentry_late_std, iv(clubs_all_pc) mv(pcNSentry_early_std) cv(lnpop25 share_cath25 bcollar25)
		sgmediation pcNSentry_late_std, iv(clubs_all_pc) mv(pcNSentry_early_std) cv(lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political')

	****Test for significance of difference in coeff in cols 2 and 4:  
		local controls_socioecon "share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop"
		local controls_political "hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg" 
		/*1,3*/reg pcNSentry_early_std clubs_all_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political' 
		estimates store m1
		sum clubs_all_pc pcNSentry_early_std if e(sample)
		/*2,4*/reg pcNSentry_late_std clubs_all_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political' 
		estimates store m2
		sum clubs_all_pc pcNSentry_late_std if e(sample)
		suest m1 m2, vce(robust)
		test [m1_mean]clubs_all_pc*(1.588092/1.011082) = [m2_mean]clubs_all_pc*(1.588092/.9630637)

	****Test for significance of difference in coeff in cols 4 and 6 
		local controls_socioecon "share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop"
		local controls_political "hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg" 
		/*4*/reg pcNSentry_late_std clubs_all_pc lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political' 
		estimates store m1
		sum clubs_all_pc pcNSentry_early_std if e(sample)
		/*6*/reg pcNSentry_late_std clubs_all_pc pcNSentry_early_std lnpop25 share_cath25 bcollar25 `controls_socioecon' `controls_political'
		estimates store m2
		sum clubs_all_pc pcNSentry_late_std if e(sample)
		suest m1 m2, vce(cl landweimar)
		test [m1_mean]clubs_all_pc*(1.588092/1.011082) = [m2_mean]clubs_all_pc*(1.588092/.9630637)

******************************************************************************************************************************************


////////////////////////////////////////////////// Table 7: Political Stability ///////////////////////////////////////////////////	
quietly{			

gen i_assoc_stability=govt_stability*clubs_all_pc
foreach var of varlist share_cath25 lnpop25 bcollar25 {
	gen i_`var'_stab = `var'*govt_stability
	gen i_`var'_stabAM = `var'*govt_stability_AM
	}
gen i_clubs_stability_AM=govt_stability_AM*clubs_all_pc
gen i_clubs_Prussia=Prussia*clubs_all_pc
}
	
	
*Table 7:
eststo clear
*Up to Preussenschlag, no effect in Prussia, but strong effect outside
/*1*/eststo: reg pcNSentry_PRS_std clubs_all_pc share_cath25 lnpop25 bcollar25 if Prussia==1, r
/*2*/eststo: reg pcNSentry_PRS_std clubs_all_pc share_cath25 lnpop25 bcollar25 if Prussia==0, r
*Now focus on "Outside Prussia". First, BELOW median coalition stability
/*3*/eststo: reg pcNSentry_PRS_std clubs_all_pc share_cath25 lnpop25 bcollar25 if Prussia==0 & govt_stability_AM==0, r 
*Then, "Outside Prussia", ABOVE median coalition stability
/*4*/eststo: reg pcNSentry_PRS_std clubs_all_pc lnpop25 share_cath25 bcollar25 if Prussia==0 & govt_stability_AM==1, r
*Finally, systematically, with two interactions, for Prussia and "outside prussia" above-median
/*5*/eststo: reg pcNSentry_PRS_std clubs_all_pc govt_stability_AM i_clubs_stability_AM Prussia i_clubs_Prussia lnpop25 share_cath25 bcollar25 i_lnpop25_stabAM i_bcollar25_stabAM i_share_cath25_stabAM, cl(landweimar)	
/*6*/eststo: areg pcNSentry_PRS_std clubs_all_pc govt_stability_AM i_clubs_stability_AM Prussia i_clubs_Prussia lnpop25 share_cath25 bcollar25 i_lnpop25_stabAM i_bcollar25_stabAM i_share_cath25_stabAM, absorb(landweimar) cl(landweimar)	
esttab , se beta star compress keep(clubs_all_pc) //view beta coefficients

	****Test for significance of difference in coeff in cols 1 and 2:  
		/*1*/eststo: reg pcNSentry_PRS_std clubs_all_pc share_cath25 lnpop25 bcollar25 if Prussia==1
		estimates store m1
		sum clubs_all_pc pcNSentry_PRS_std if e(sample)
		/*2*/eststo: reg pcNSentry_PRS_std clubs_all_pc share_cath25 lnpop25 bcollar25 if Prussia==0
		estimates store m2
		suest m1 m2, vce(robust)
		sum clubs_all_pc pcNSentry_PRS_std if e(sample)
		test [m1_mean]clubs_all_pc*1.567219/.9179966 = [m2_mean]clubs_all_pc*1.571497/1.000449
	****Test for significance of difference in coeff in cols 3 and 4:  
		/*3*/eststo: reg pcNSentry_PRS_std clubs_all_pc share_cath25 lnpop25 bcollar25 if Prussia==0 & govt_stability_AM==0
		estimates store m3
		sum clubs_all_pc pcNSentry_PRS_std if e(sample)
		/*4*/eststo: reg pcNSentry_PRS_std clubs_all_pc lnpop25 share_cath25 bcollar25 if Prussia==0 & govt_stability_AM==1
		estimates store m4
		sum clubs_all_pc pcNSentry_PRS_std if e(sample)
		suest m3 m4, vce(robust)
		test [m3_mean]clubs_all_pc*1.438824/1.140114 = [m4_mean]clubs_all_pc*1.64175/.8312506


	*For text: "Importantly, our stability measure does not simply reflect voter preferences – for example, voting results for the Weimar coalition 
	*           of middle-of-the-road democrats have no predictive power for our stability measure (beta coefficient 0.026; p-value 0.86)."
	reg govt_stability weimar_coal_votes_avg, cl(landweimar) 
	listcoef

	*Note for appendix text: negative corr b/w state stability and entry
	preserve
	collapse (mean) pcNSentry_PRS_std govt_stability clubs_all_pc share_cath25 lnpop25 bcollar25 Prussia weimar_coal_votes_avg, by(landweimar)
	pwcorr pcNSentry_PRS_std govt_stability, sig
	restore


********** Figure 5 **************

	*Prussia:
	/*1*/reg pcNSentry_PRS_std clubs_all_pc share_cath25 lnpop25 bcollar25 if Prussia==1, r
	avplot clubs_all_pc, recast(scatter) mcolor(navy) msize(medsmall) msymbol(diamond) mlabel(city) mlabcolor(navy) mlabsize(small) rlopts(lcolor(black)) ytitle(NSDAP entry rate (residual)) ///
	ytitle(, size(medlarge)) xtitle(Association density (residual)) xtitle(, size(medlarge)) note(, size(medsmall) ring(0)) note(, size(medium) ring(0) position(5)) note("coeff=0.079 se=0.077") scheme(s1color)

	*Outside Prussia, Below-median coalition stability:
	/*3*/reg pcNSentry_PRS_std clubs_all_pc share_cath25 lnpop25 bcollar25 if Prussia==0 & govt_stability_AM==0, r
	avplot clubs_all_pc, recast(scatter) mcolor(navy) msize(medsmall) msymbol(diamond) mlabel(city) mlabcolor(navy) mlabsize(small) rlopts(lcolor(black)) ytitle(NSDAP entry rate (residual)) ///
	ytitle(, size(medlarge)) xtitle(Association density (residual)) xtitle(, size(medlarge)) note(, size(medsmall) ring(0)) note(, size(medium) ring(0) position(5)) note("coeff=0.349 se=0.128") scheme(s1color)

	/*4*/reg pcNSentry_PRS_std clubs_all_pc share_cath25 lnpop25 bcollar25 if Prussia==0 & govt_stability_AM==1, r
	avplot clubs_all_pc, recast(scatter) mcolor(navy) msize(medsmall) msymbol(diamond) mlabel(city) mlabcolor(navy) mlabsize(small) rlopts(lcolor(black)) ytitle(NSDAP entry rate (residual)) ///
	ytitle(, size(medlarge)) xtitle(Association density (residual)) xtitle(, size(medlarge)) note(, size(medsmall) ring(0)) note(, size(medium) ring(0) position(5)) note("coeff=-0.012 se=0.062") scheme(s1color)

	

////////////////////////////////////////////////// Table 8: Subsamples   //////////////////////////////////////////////////

foreach var of varlist lnpop25 bcollar25 share_jew25 {
	xtile `var'_AM = `var', nq(2)
	replace `var'_AM= `var'_AM-1
	}

*Panel A:
eststo clear
/*1*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r, if lnpop25_AM==0
/*2*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r, if lnpop25_AM==1
/*3*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r, if share_cath25<0.5 
/*4*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r, if share_cath25>0.5
/*5*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r, if bcollar25_AM==0
/*6*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25, r, if bcollar25_AM==1
esttab , se beta star compress keep(clubs_all_pc) //view beta coefficients

	****Test for significance of difference in coeff in cols 1 and 2:  
		/*1*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25 if lnpop25_AM==0
		estimates store m1
		sum clubs_all_pc pcNSentry_std if e(sample)
		/*2*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25 if lnpop25_AM==1
		estimates store m2
		sum clubs_all_pc pcNSentry_std if e(sample)
		suest m1 m2, vce(robust)
		test [m1_mean]clubs_all_pc*1.485746/1.127951 = [m2_mean]clubs_all_pc*1.352372/.8574146
	****Test for significance of difference in coeff in cols 3 and 4:  
		/*3*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25 if share_cath25<0.5
		estimates store m3
		sum clubs_all_pc pcNSentry_std if e(sample)
		/*4*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25 if share_cath25>0.5
		estimates store m4
		sum clubs_all_pc pcNSentry_std if e(sample)
		suest m3 m4, vce(robust)
		test [m3_mean]clubs_all_pc*1.6237/1.012137 = [m4_mean]clubs_all_pc*1.440381/.834389
	****Test for significance of difference in coeff in cols 5 and 6:  
		/*5*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25 if bcollar25_AM==0
		estimates store m5
		sum clubs_all_pc pcNSentry_std if e(sample)
		/*6*/eststo: reg pcNSentry_std clubs_all_pc lnpop25 share_cath25 bcollar25 if bcollar25_AM==1
		estimates store m6
		sum clubs_all_pc pcNSentry_std if e(sample)
		suest m5 m6, vce(robust)
		test [m5_mean]clubs_all_pc*1.417522/1.065999 = [m6_mean]clubs_all_pc*1.713378/.895792	
	
*Panel B:
eststo clear
/*1*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25, r, if lnpop25_AM==0
/*2*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25, r, if lnpop25_AM==1
/*3*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25, r, if share_cath25<0.5  
/*4*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25, r, if share_cath25>0.5
/*5*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25, r, if bcollar25_AM==0
/*6*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25, r, if bcollar25_AM==1 
esttab , se beta star compress keep(clubs_civic_pc) //view beta coefficients

	****Test for significance of difference in coeff in cols 1 and 2:  
		/*1*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25 if lnpop25_AM==0
		estimates store m1
		sum clubs_civic_pc pcNSentry_std if e(sample)
		/*2*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25 if lnpop25_AM==1
		estimates store m2
		sum clubs_civic_pc pcNSentry_std if e(sample)
		suest m1 m2, vce(robust)
		test [m1_mean]clubs_civic_pc*.5504577/1.127951 = [m2_mean]clubs_civic_pc*.5243137/.8572721 
	****Test for significance of difference in coeff in cols 3 and 4:  
		/*3*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25 if share_cath25<0.5
		estimates store m3
		sum clubs_civic_pc pcNSentry_std if e(sample)
		/*4*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25 if share_cath25>0.5
		estimates store m4
		sum clubs_civic_pc pcNSentry_std if e(sample)
		suest m3 m4, vce(robust)
		test [m3_mean]clubs_civic_pc*.573253/1.012137 = [m4_mean]clubs_civic_pc*.5460622/.8253549
	****Test for significance of difference in coeff in cols 5 and 6:  
		/*5*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25 if bcollar25_AM==0
		estimates store m5
		sum clubs_civic_pc pcNSentry_std if e(sample)
		/*6*/eststo: reg pcNSentry_std clubs_civic_pc lnpop25 share_cath25 bcollar25 if bcollar25_AM==1
		estimates store m6
		sum clubs_civic_pc pcNSentry_std if e(sample)
		suest m5 m6, vce(robust)
		test [m5_mean]clubs_civic_pc*.4645676/1.068739 = [m6_mean]clubs_civic_pc*.6508457/.895792	


*************************************************************************************************************************************	


////////////////////////////////////////////////// Table 9: Bonding vs bridging ////////////////////////////////////////////////// 

pwcorr clubs_bridging_pc clubs_bonding_pc
local controls_socioecon "share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop"
local controls_political "hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg" 
eststo clear
eststo: reg pcNSentry_std clubs_bridging_pc lnpop25 share_cath25 bcollar25, r beta
eststo: reg pcNSentry_std clubs_bonding_pc lnpop25 share_cath25 bcollar25, r beta
eststo: reg pcNSentry_std clubs_bridging_pc lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon', cl(landweimar)
eststo: reg pcNSentry_std clubs_bonding_pc lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon', cl(landweimar)
eststo: reg pcNSentry_std clubs_bridging_pc lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon' dummy_land*, cl(landweimar)
eststo: reg pcNSentry_std clubs_bonding_pc lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon' dummy_land*, cl(landweimar)
esttab , se beta star compress keep(clubs_bridging_pc clubs_bonding_pc) //view beta coefficients
	
	****Test for significance of difference in coeff in cols 1 and 2 
		/*1*/reg pcNSentry_std clubs_bridging_pc lnpop25 share_cath25 bcollar25 
		estimates store m1
		sum clubs_bridging_pc pcNSentry_std if e(sample)
		/*2*/reg pcNSentry_std clubs_bonding_pc lnpop25 share_cath25 bcollar25 
		estimates store m2
		sum clubs_bonding_pc pcNSentry_std if e(sample)
		suest m1 m2, vce(robust)
		test [m1_mean]clubs_bridging_pc*1.172483/1.000764 = [m2_mean]clubs_bonding_pc*.4150877/1.000764
	****Test for significance of difference in coeff in cols 3 and 4 
		local controls_socioecon "share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop"
		local controls_political "hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg" 
		/*1,2*/reg pcNSentry_std clubs_bridging_pc lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon'
		estimates store m3
		sum clubs_bridging_pc pcNSentry_std if e(sample)
		/*3,4*/reg pcNSentry_std clubs_bonding_pc lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon'
		estimates store m4
		sum clubs_bonding_pc pcNSentry_std if e(sample)
		suest m3 m4, vce(cl landweimar)
		test [m3_mean]clubs_bridging_pc*1.179995/.9835903 = [m4_mean]clubs_bonding_pc*.419409/.9835903
	****Test for significance of difference in coeff in cols 5 and 6
		local controls_socioecon "share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop"
		local controls_political "hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg" 
		/*1,2*/reg pcNSentry_std clubs_bridging_pc lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon' dummy_land*
		estimates store m3
		sum clubs_bridging_pc pcNSentry_std if e(sample)
		/*3,4*/reg pcNSentry_std clubs_bonding_pc lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon' dummy_land*
		estimates store m4
		sum clubs_bonding_pc pcNSentry_std if e(sample)
		suest m3 m4, vce(cl landweimar)
		test [m3_mean]clubs_bridging_pc*1.179995/.9835903 = [m4_mean]clubs_bonding_pc*.419409/.9835903
				
*************************************************************************************************************************************	


////////////////////////////////////////////////// Table 10: Matching estimation ////////////////////////////////////////////////// 
eststo clear
/*1*/eststo: nnmatch pcNSentry_std clubs_pc_AM lnpop25, m(1) robust(1) tc(att)
/*2*/eststo: nnmatch pcNSentry_std clubs_pc_AM lnpop25, m(3) robust(3) tc(att)
/*3*/eststo: nnmatch pcNSentry_std clubs_pc_AM lnpop25 share_cath25 bcollar25, m(3) robust(3) tc(att)
/*4*/eststo: nnmatch pcNSentry_std clubs_pc_AM lnpop25 share_cath25 bcollar25 latitude longitude, m(3) robust(3) tc(att)
*Matching within the same Bundesland 
/*5*/eststo: nnmatch pcNSentry_std clubs_pc_AM lnpop25 share_cath25 bcollar25 latitude longitude, m(3) robust(3) tc(att) exact(pop25_quintiles landweimar_num)
*Heinmueller method of entropy reweighting to create balanced samples 
ebalance clubs_pc_AM lnpop25 share_cath25 bcollar25 latitude longitude, tar(2)
/*6*/eststo: reg pcNSentry_std clubs_pc_AM  [pweight=_webal],r

	/*For text: "To provide a benchmark for comparison, the OLS coefficient on the indicator for above-median association density (with baseline controls) is 0.259, with 
				 a standard error of 0.125, and p-value 0.04. */
	reg pcNSentry_std clubs_pc_AM lnpop25 share_cath25 bcollar25, r beta

*************************************************************************************************************************************	


/////////////////////////////////////  Table 11: IV with Principal component /////////////////////////////////////////////////////////////////////////////////


*Table 11, Panel A: Second Stage
eststo clear
local controls_socioecon "share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop"
local controls_political "hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg" 
/*1*/eststo: ivreg2 pcNSentry_std (clubs_all_pc = IV_pca), r first
/*2*/eststo: ivreg2 pcNSentry_std (clubs_all_pc = IV_pca) lnpop25 share_cath25 bcollar25, r first
/*3*/eststo: ivreg2 pcNSentry_std (clubs_all_pc = IV_pca) lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon', r first	
/*4*/eststo: ivreg2 pcNSentry_std (clubs_all_pc = IV_pca) lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon' dummy_land*, r first	
/*5*/eststo: ivreg2 pcNSentry_std (clubs_civic_pc = IV_pca) lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon', r first
esttab , se beta star compress keep(clubs_all_pc clubs_civic_pc) //view beta coefficients


*Table 11, Panel B: First Stage
eststo clear
local controls_socioecon "share_jew25 unemp33 in_welfare_per1000 war_per1000 sozialrentner_per1000 logtaxpers logtaxprop"
local controls_political "hitler_speech_per1000 DNVP_votes_avg DVP_votes_avg SPD_votes_avg KPD_votes_avg" 
/*1*/eststo: reg clubs_all_pc IV_pca, r first
/*2*/eststo: reg clubs_all_pc IV_pca lnpop25 share_cath25 bcollar25, r 
/*3*/eststo: reg clubs_all_pc IV_pca lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon', r 	
/*4*/eststo: reg clubs_all_pc IV_pca lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon' dummy_land*, r 	
/*5*/eststo: reg clubs_civic_pc IV_pca lnpop25 share_cath25 bcollar25 `controls_political' `controls_socioecon', r 

*********************************************************************************************************************************
	
/////////////////////////////////////  Table 12: Historical anti-Semitism /////////////////////////////////////////////////////////////////////////////////
	
*Table 12: 
eststo clear
/*1*/eststo: reg pog20s pog1349 lnpop25 share_cath25 bcollar25 if exist1349==1, r beta 
/*2*/eststo: nnmatch pog20s pog1349 lnpop25 share_cath25 bcollar25  latitude longitude if exist1349==1, m(3) robust(3) tc(att)
/*3*/eststo: reg pcNSDAP285 pog1349 lnpop25 share_cath25 bcollar25 if exist1349==1, r beta 
/*4*/eststo: nnmatch pcNSDAP285 pog1349 lnpop25 share_cath25 bcollar25  latitude longitude if exist1349==1, m(3) robust(3) tc(att)
/*5*/eststo: reg clubs_all_pc pog1349 lnpop25 share_cath25 bcollar25 if exist1349==1, r beta 
/*6*/eststo: nnmatch clubs_all_pc pog1349 lnpop25 share_cath25 bcollar25 latitude longitude if exist1349==1, m(3) robust(3) tc(att)
esttab , se beta star compress keep(pog1349) //view beta coefficients

*********************************************************************************************************************************



*********************************************************************************************************************************************************************************		
************************************************************ end of main results *************************************************************************************************	
*********************************************************************************************************************************************************************************	

log close
