	***Figure 3: RD-Enrollment
		***Algorithm to computer N bins with range +/- h
			local nBinOver2 `1'
			local h `2'
			local dirName `3'
			capture mkdir `dirName'
			
			local step = `h'/((`nBinOver2'))
			dis "StepSize `step'"
			capture drop theBinAl theBinAlTemp 
			gen  theBinAlTemp= .
			***Only Does this in 2006, then expands to other years
			***Handles Negative Numbers
			forval x = 1/`nBinOver2'{
				replace theBinAlTemp = -`step'*`x' if LISPremium >= (-`step'*(`x')) & LISPremium <(-`step'*(`x'-1)) & year == 2006 
				dis "`x'"
			}
			***Does Positive Numbers
			forval x = 1/`nBinOver2'{
				replace theBinAlTemp = `step'*`x' if LISPremium >= (`step'*(`x')) & LISPremium <(`step'*(`x'+1))  & year == 2006
				dis "`x'"
			}
			egen theBinAl = max(theBinAlTemp),by(uniqueID)
	
				
			local year =2006
			local P = ""
	
			***Select RD Window for scatter 
			local ScatWin =1
			***Select RD Window for local linear regression
			local RegWin  =2
			***Select RD Window for Polynomial regression
			local PolyWin  =1
			
			capture drop lnSHat lnSHatAlt lnSHatAltPoly
			***Regress to get Mean in Each Bin, for scatter
			xi: reg lnS i.theBinAl if year ==`year' & RDwindow2006`ScatWin'==1 & `P'benefit =="B",cluster(firmID)
			predict lnSHat if year ==`year'
			
			***Regress to get local linear line
			xi: reg lnS belowBench2006 `P'LISPremiumNeg `P'LISPremiumPos if year ==`year' & RDwindow2006`RegWin'==1 & `P'benefit =="B",cluster(firmID)
			predict lnSHatAlt if year ==`year'
	
			***Regress to get line: from quartic
			xi: reg lnS belowBench2006 `P'LISPremiumNeg `P'LISPremiumPos `P'LISPremiumNegSq `P'LISPremiumPosSq `P'LISPremiumNegCub `P'LISPremiumPosCub `P'LISPremiumNegQuart `P'LISPremiumPosQuart if year ==`year' & RDwindow2006`PolyWin'==1 & `P'benefit =="B",cluster(firmID)
			predict lnSHatAltPoly if year ==`year'
		
			local ytitle = "Log Enrollment Share, 2006"		
			twoway (scatter lnSHat theBinAl if year ==`year' & RDwindow2006`ScatWin'==1 & `P'benefit =="B") ///
				(line lnSHatAlt `P'LISPremium if year ==`year' & RDwindow2006`RegWin'==1 & `P'benefit =="B",sort lpattern(dash) lcolor(gray)) ///
				 (line lnSHatAltPoly `P'LISPremium if year ==`year' & RDwindow2006`PolyWin'==1 & `P'benefit =="B",sort lpattern(solid) lcolor(black)), ///legend(order(2 3) label(2 "Local Linear") label(3 "Quartic Polynomial")) xtitle("Monthly Premium - LIS Subsidy, 2006") ytitle("`ytitle'") 
			graph export "`dirName'/Figure3.png", replace
	