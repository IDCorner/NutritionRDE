*Program to Evaluate Nutrition in Orissa
*version 14

use "C:\NutritionHH34GH.dta" 

set more off

preserve

gen HHIDA=_n


*Creating a Sex dummy variable
gen nsex=0
replace nsex=1 if SexHH=="Male"
destring Age, replace

*Generating a Caste Dummy
gen ncasteOH=0
replace ncasteOH=1 if CasteHH=="Other Hindus"
gen ncasteOBC=0
replace ncasteOBC=1 if CasteHH=="Other Backward Caste"
gen ncasteSC=0
replace ncasteSC=1 if CasteHH=="Schedule Caste"

*destring MStatus, replace
quietly destring TQ*, replace
quietly destring TE*, replace

*Occupation farm and wage labour and others
gen occu =0
replace occu =1 if OccupationHH==1
replace occu= 1 if OccupationHH==2

*demographics males females children
gen TCh=mchild+fchild
gen TOld=mold+fold
gen TAd=males+females
gen TAdAll=TAd+TOld
gen TAFemale= females+fold
gen TAMale=males+mold
gen DepRatio=TCh/tmember

gen femaleall =females +fchild+fold
gen maleall= males+mchild+mold

*total members in terms of caste
summ tmember
scalar stmember=r(sum)
gen tmemberSC = tmember if CasteHH == "Schedule Caste"
summ tmemberSC
scalar stmemberSC = r(sum)
gen tmemberOH= tmember if CasteHH == "Other Hindus"
summ tmemberOH
scalar stmemberOH= r(sum)
gen tmemberOBC= tmember if CasteHH == "Other Backward Caste"
summ tmemberOBC
scalar stmemberOBC=r(sum)

*frequency in terms of caste
gen ftmemberOBC =tmemberOBC/stmemberOBC
gen ftmemberOH =tmemberOH/stmemberOH
gen ftmemberSC =tmemberSC/stmemberSC

*total member in households by the gender of head of household
gen tmemberF = tmember if SexHH == "Female"
summ tmemberF
scalar stmemberF = r(sum)
gen tmemberM= tmember if SexHH == "Male"
summ tmemberM
scalar stmemberM= r(sum)

**** Table 1
tabstat males female mchild fchild mold fold, by(CasteHH) statistics(sum)
tabstat males female mchild fchild mold fold, by(SexHH) statistics(sum)


*Adult Equivalents
gen AELS=aequiv  // aequiv is calculated based on NSS weights

*Raw consumption across food groups.
replace TQEgg=TQEgg*0.05 /// converting each egg in to gms
replace TQSag=TQSag*0.25 /// converting sag bunch in to gms
gen QCereals = (TQRice+TQWheat+TQOtherC)/tmember
gen QPulses = (TQArhar+TQMoong+TQMasur+TQUrad+TQKesari+TQKolatha+TQOtherP)/tmember
gen QVegetables=(TQPotato+TQOnion+TQTomato+TQBrinjal+TQCauliflower+TQCabbage +(TQSag)+TQRootVeg+TQCarotRadish+TQGJackFruit+TQGPlantains+TQBhendi+TQBeans+TQPotal+TQSeema)/tmember
gen QFruits= (TQBanana+TQMango+TQCoconut+TQGuava+TQPineapple+TQLemon+TQPapaya+TQBaraKoli+TQApple+TQOrange+TQGrapes)/tmember
gen QDryNuts=(TQGroundnut+TQOtherDryF)/tmember
gen QSGS=(TQSugar+TQGur+TQSalt)/tmember
gen QSpices=(TQTurmeric+TQBlackPep+TQDryChillies+TQGreenChillies+TQGarlic+TQTamarind+TQGinger+TQCuCor+TQOtherSp+TQPickle)/tmember
gen QMilkPd=(TQMilk+TQDahi+TQChennaSweets)/tmember
gen QEOil=(TQVanaspati+TQMustard+TQRefinedOil+TQOtherOil)/tmember
gen QMEF=(TQGoatM+TQChicken+(TQEgg)+TQFF+TQDFish)/tmember
gen QBev=(TQTea+TQCoffee+TQOtherB)/tmember
gen QPTIntox=(TQPan+TQTobbaco+TQLiquor+TQOtherAdd)/tmember
gen QTotal=(QCereals+QPulses+QVegetables+QFruits+QDryNuts+QSGS+QSpices+QMilkPd+QEOil+QMEF+QBev+QPTIntox)/tmember

***Table 2
tabstat QCereals QPulses QVegetables QFruits QDryNuts QSGS QSpices QMilkPd QEOil QMEF QBev QPTIntox [fw=tmember], statistics( mean median sd max min)

*Expenditure across food groups
gen ECereals = TERice+TEWheat+TEOtherC
gen EPulses = TEArhar+TEMoong+TEMasur+TEUrad+TEKesari+TEKolatha+TEOtherP
gen EVegetables=TEPotato+TEOnion+ TETomato+ TEBrinjal+TECauliflower+TECabbage +TESag+TERootVeg+TECarotRadish+TEGJackFruit+TEGPlantains+TEBhendi+TEBeans+TEPotal+TESeema
gen EFruits= TEBanana+TEMango+TECoconut+TEGuava+TEPineApple+TELemon+TEPapaya+TEBarakoli+TEApple+TEOrange+TEGrapes
gen EDryNuts=TEGroundnut+TEOtherDryF
gen ESGS=TESugar+TEGur+TESalt
gen ESpices=TETurmeric+TEBlackPep+TEDryChillies+TEGreenChillies+TEGarlic+TETamarind+TEGinger+TECuCor+TEOtherSp+TEPickle
gen EMilkPd=TEMilk+TEDahi+TEChennaSweets
gen EEOil=TEVanaspati+TEMustard+TERefinedOil+TEOtherOil
gen EMEF=TEGoatM+TEChicken+TEEgg+TEFF+TEDFish
gen EBev=TETea+TECoffee+TEOtherB
gen EPTIntox=TEPan+TETobbacco+TELiquor+TEOtherAdd
gen EFTotal=ECereals+EPulses+EVegetables+EFruits+EDryNuts+ESGS+ESpices+EMilkPd+EEOil+EMEF+EBev+EPTIntox


******Imputed value of the PDS rice (subsidy from government)
gen RationSub= 13*pdsq

*Expenditure and Assets
gen ECC = (TEJewelery+TESocialfunctions)/12
gen ENFTotal= NonFoodExpd+TEUtensils+TEHouseholdAppliance+TEOtherPersonalGoods+TEInsurance+ TEVacations
gen ETotal=(EFTotal+ENFTotal)

*Converting to Macro Nutrients by food groups
*Cereals
mkmat TQRice TQWheat TQOtherC, mat(A1)
mkmat Rice100gms WheatFlourWhole100gms Other100gms, mat(B1)
*Pulses
mkmat TQArhar TQMoong TQMasur TQUrad TQKesari TQKolatha TQOtherP, mat(A2)
mkmat Arhar100gms Moong100gms Masur100gms Urad100gms Kesari100gms Kolatha100gms OtherPulses100gms, mat(B2)
*Vegetables
mkmat TQPotato TQOnion TQTomato TQBrinjal TQCauliflower TQSag TQCabbage TQRootVeg TQCarotRadish TQGJackFruit TQGPlantains TQBhendi TQBeans TQPotal TQSeema, mat(A3)
mkmat Potato100gms Onion100gms Tomato100gms Brinjal100gms Cauliflower100gms Sag100gms Cabbage100gms RootVegetables100gms CarotRadish100gms Jackfruit100gms Plantain100gms Bhendi100gms Beans100gms Potal100gms Seema100gms, mat(B3)
*Fruits
mkmat TQBanana TQMango TQCoconut TQGuava TQPineapple TQLemon TQPapaya TQBaraKoli TQApple TQOrange TQGrapes, mat(A4)
mkmat Banana100gms Mango100gms Coconut100gms Guava100gms Pineapple100gms Lemon100gms Papaya100gms BaraKoli100gms Apple100gms Orange100gms Grapes100gms, mat(B4)
*Dry Nuts
mkmat TQGroundnut TQOtherDryF, mat(A5)
mkmat Groundnuts100gms OtherDryfruits100gms, mat(B5)
* Sugar
mkmat TQSugar TQGur TQSalt, mat(A6)
mkmat Sugar100gms Gur100gms Salt100gms, mat(B6)
* Spices
mkmat TQTurmeric TQBlackPep TQDryChillies TQGreenChillies TQGarlic TQTamarind TQGinger TQCuCor TQOtherSp TQPickle, mat(A7)
mkmat Tumeric100gms BlackPepper100gms DryChillie100gms GreenChillie100gms Garlic100gms Tamarind100gms Ginger100gms CuCor100gms OtherSpices100gms Pickle100gms, mat(B7)
*Dairy Products
mkmat TQMilk TQDahi TQChennaSweets, mat(A8)
mkmat Milk100gms Dahi100gms Chennasweets100gms, mat(B8)
*Edible Oils
mkmat TQVanaspati TQMustard TQRefinedOil TQOtherOil, mat(A9)
mkmat Vanaspati100gms Mustard100gms RefinedOil100gms OtherOil100gms, mat(B9)
*MeatEggFish
mkmat TQGoatM TQChicken TQEgg TQFF TQDFish, mat(A10)
mkmat GoatMeat100gms Chicken100gms Egg100gms FFishRohu100gms DFishribbon100gms, mat(B10)
*Beverages
mkmat TQTea TQCoffee TQOtherB, mat(A11)
mkmat Tea100gms Coffee100gms OtherBev100gms, mat(B11)


*Average daily Nutirent Consumption
mat NI2=J(11, 5, 0)
local k 1

		while `k'< =11 {
		     
							mat A`k'100 =A`k'*10 
							mat C`k'=(A`k'100)*(B`k')'
							svmat C`k', names(nutr`k')
							drop nutr`k'10-nutr`k'134
							
							gen Cal`k'LS= nutr`k'1
							gen Prot`k'LS= nutr`k'2
							gen Carbo`k'LS= nutr`k'6
							gen Calcium`k'LS= nutr`k'7
							gen Iron`k'LS= nutr`k'9
							
							quietly summ Cal`k'LS
							matrix NI2[`k',1]=r(sum)/(30*stmember)
							
							quietly summ Prot`k'LS
							matrix NI2[`k',2]=r(sum)/(30*stmember)
							
							quietly summ Carbo`k'LS
							matrix NI2[`k',3]=r(sum)/(30*stmember)
							
							quietly summ Calcium`k'LS
							matrix NI2[`k',4]=r(sum)/(30*stmember)
							
							quietly summ Iron`k'LS
							matrix NI2[`k',5]=r(sum)/(30*stmember)
							
							local k=`k'+1
							drop nutr*
							}
							
				
****Table A2
matlist NI2

				
*Converting to Macro Nutrients
mkmat TQ*, mat(A)
mkmat *100gms, mat(B)

*Converting A (in kg) in to number of units in terms of 100gms
mat A100 =A*10 
mat C=A100*B'
svmat C, names(nutr)
drop nutr10-nutr134

*Expand the sample from headofhousehold (HH) to individuals.
gen v=_n
gen v1=tmember
expand v1, gen(ed)
sort v, stable
gen v2=_n							
merge 1:1 v2 using "C:\nutrition22BGH.dta" /// YOU NEED TO INSTALL THIS DATA FILE IN YOUR DRIVE
*Counting the proportion of those occupied in occupation 1 and 2
gen occuI1=0
replace occuI1=1 if Occupation==1 
replace occuI1=1 if Occupation==2
bysort hhid: egen occuI2=total(occuI1) 
drop if ed==1

gen occuprop=occuI2/tmember
gen sexfprop=sexI2/tmember




*Per capita consumption of nutrition at the household level
gen CalTM= nutr1/(30*tmember)
gen ProtTM= nutr2/(30*tmember)
gen CarboTM= nutr6/(30*tmember)
gen CalciumTM= nutr7/(30*tmember)
gen IronTM= nutr9/(30*tmember)

****Table 3
tabstat CalTM ProtTM CarboTM CalciumTM IronTM [fw=tmember], statistics( mean median sd max min)

*Adult Equivalent Consumption at the household level
gen CalAE= nutr1/(30*AELS)
gen ProtAE= nutr2/(30*AELS)
gen CarboAE= nutr6/(30*AELS)
gen CalciumAE= nutr7/(30*AELS)
gen IronAE= nutr9/(30*AELS)
tabstat CalAE ProtAE CarboAE CalciumAE IronAE [fw=tmember], statistics( mean median sd max min)


*Expenditure and PDS for per capita & AELS
gen ETTM=ETotal/(30*tmember)
gen ETotalLTM=ln(ETTM+1)
gen ETotalTMSq=(ETotalLTM)^2
gen pdsq1=(13*pdsq)/(30*tmember)
gen pdsqLTM = ln(pdsq1+1)


*Expenditure and PDS Adult equiv
gen ETAE = ETotal/(30*AELS)
gen ETotalLAE=ln(ETAE+1)

*gen ETotalAESq=(ETotalAE)*(ETotalAE)
gen pdsqAE=(13*pdsq)/(30*AELS)
gen pdsqLAE= ln(pdsqAE+1)

*IVAssets
gen ETTMh = hhassetst2/(30*tmember)
gen ETotalTMh=ln(ETTMh+1)

****IV 
***bedroom correction in the data
replace bedrooms=1 in 33
replace lrooms=1 in 33

gen TElec=ln((kerosltq/(30*(lrooms+kbroom)*tmember))+1) // kbroom is kitchen and bathroom, lrooms is living rooms
replace TElec =ln((kerosltq/(30*1*tmember))+1) if kbroom==0 & lrooms==0


*Diagnostics
tabstat kerosltq if kerosltq>0, stat(count mean)
tabstat kerosf if kerosf>0, stat(count mean)

tabstat kerosltq if pdsq>0, stat(count mean)
tabstat kerosltq if pdsq==0, stat(count mean)

*ln nutrients TM & AELS
gen CalLTM=ln(CalTM)
gen ProtLTM=ln(ProtTM)
gen CarboLTM=ln(CarboTM)
gen CalciumLTM= ln(CalciumTM)
gen IronLTM=ln(IronTM)

gen Calorie=ln(CalAE)
gen Protein=ln(ProtAE)
gen Carbohydrate=ln(CarboAE)
gen Calcium= ln(CalciumAE)
gen Iron=ln(IronAE)


*Dropping Households with high consumption of nutrients based on quantitites
drop if HouseholdIDA=="80"
drop if HouseholdIDA=="83"
drop if HouseholdIDA=="135"



*** OLS estimates
reg CalLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, r
estimates store m1
test _b[ETotalLTM]=1
test _b[pdsqLTM]=1
vif
reg ProtLTM ETotalLTM  pdsqLTM tmember ncasteOH ncasteOBC nsex AgeHH occuprop, r
estimates store m2
test _b[ETotalLTM]=1
test _b[pdsqLTM]=1
vif
reg CarboLTM ETotalLTM pdsqLTM tmember ncasteOH ncasteOBC nsex AgeHH occuprop , r
estimates store m3
test _b[ETotalLTM]=1
test _b[pdsqLTM]=1
vif
reg CalciumLTM ETotalLTM pdsqLTM tmember ncasteOH ncasteOBC nsex AgeHH occuprop, r
eststo m4
test _b[ETotalLTM]=1
test _b[pdsqLTM]=1
vif
reg IronLTM ETotalLTM pdsqLTM tmember ncasteOH ncasteOBC nsex AgeHH occuprop, r
eststo m5
test _b[ETotalLTM]=1
test _b[pdsqLTM]=1
vif
esttab m1 m2 m3 m4 m5  using "C:\TablesTM.rtf", replace b(3) se(3) star(* .1 ** .05 *** .01) stats(N r2 F) nogap ///Table 4
eststo clear


*****IV with Assets with percapita and kerosene per person per day per room.

ivreg2 CalLTM (ETotalLTM pdsqLTM = ETotalTMh TElec) tmember ncasteOH  ncasteOBC nsex AgeHH  occuprop ,  first endog(ETotalLTM pdsqLTM) savefirst savefprefix(fs) r 
eststo m1
weakiv

ivreg2 ProtLTM (ETotalLTM pdsqLTM =  ETotalTMh TElec) tmember ncasteOH  ncasteOBC nsex AgeHH  occuprop , endog(ETotalLTM pdsqLTM ) r 
eststo m2

ivreg2 CarboLTM ( ETotalLTM pdsqLTM = ETotalTMh TElec) tmember ncasteOH  ncasteOBC nsex AgeHH occuprop ,  endog(ETotalLTM pdsqLTM) r 
eststo m3 

ivreg2 CalciumLTM  ( ETotalLTM pdsqLTM = ETotalTMh TElec) tmember ncasteOH  ncasteOBC nsex AgeHH occuprop ,   endog(ETotalLTM pdsqLTM) r 
eststo m4

ivreg2 IronLTM ( ETotalLTM pdsqLTM = ETotalTMh TElec) tmember ncasteOH  ncasteOBC nsex AgeHH occuprop,  endog(ETotalLTM pdsqLTM) r
eststo m5

esttab m1 m2 m3 m4 m5 using "C:\TablesIVElecTM.rtf", replace b(3) se(3) star(* .1 ** .05 *** .01) stats(N r2 F) nogap /// Table 5
esttab fsETotalLTM	fspdsqLTM using "C:\TablesFirstStageIVElecTM.rtf", replace b(3) se(3) star(* .1 ** .05 *** .01) stats(N Fs) nogap ///Table A3

eststo clear

*Regressions based on ADULT EQUIVALENCE at the Household level
reg Calorie ETotalLAE  pdsqLAE tmember ncasteOH  ncasteOBC nsex AgeHH  occuprop, r
estimates store m1
test _b[ETotalLAE]=1
test _b[pdsqLAE]=1
reg Protein ETotalLAE pdsqLAE tmember ncasteOH ncasteOBC nsex AgeHH occuprop , r
estimates store m2
test _b[ETotalLAE]=1
test _b[pdsqLAE]=1
reg Carbohydrate ETotalLAE pdsqLAE tmember ncasteOH ncasteOBC nsex AgeHH occuprop, r
estimates store m3
test _b[ETotalLAE]=1
test _b[pdsqLAE]=1
reg Calcium ETotalLAE pdsqLAE tmember ncasteOH ncasteOBC nsex AgeHH  occuprop, r
estimates store m4
test _b[ETotalLAE]=1
test _b[pdsqLAE]=1
reg Iron ETotalLAE pdsqLAE tmember ncasteOH ncasteOBC nsex AgeHH occuprop, r
estimates store m5
test _b[ETotalLAE]=1
test _b[pdsqLAE]=1
esttab m1 m2 m3 m4 m5 using "C:\TablesAE.rtf", replace b(3) se(3) star(* .1 ** .05 *** .01) stats(N r2 F) nogap  ///Table 6
eststo clear



*****Quantile regressions****
*50th qreg
qreg CalLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.5) vce(robust) 
eststo m1
qreg ProtLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.5) vce(robust)
eststo m2
qreg CarboLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.5) vce(robust)
eststo m3
qreg CalciumLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.5) vce(robust)
eststo m4
qreg IronLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.5) vce(robust)
eststo m5
esttab m1 m2 m3 m4 m5 using "C:\TablesTMqregQ50.rtf", ///
replace b(3) se(3) star(* .1 ** .05 *** .01) pr2(2) nogap  ///Table A4
eststo clear


*35th qreg
qreg CalLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.35) vce(robust) 
eststo m1
qreg ProtLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.35) vce(robust)
eststo m2
qreg CarboLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.35) vce(robust)
eststo m3
qreg CalciumLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.35) vce(robust)
eststo m4
qreg IronLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.35) vce(robust)
eststo m5
esttab m1 m2 m3 m4 m5 using "C:\TablesTMqregQ35.rtf", ///
replace b(3) se(3) star(* .1 ** .05 *** .01)  pr2 nogap  /// Table 7
eststo clear

*25th qreg
qreg CalLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.25) vce(robust) 
eststo m1
qreg ProtLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.25) vce(robust)
eststo m2
qreg CarboLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.25) vce(robust)
eststo m3
qreg CalciumLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.25) vce(robust)
eststo m4
qreg IronLTM ETotalLTM  pdsqLTM tmember ncasteOH  ncasteOBC nsex AgeHH occuprop, quantile(0.25) vce(robust)
eststo m5
esttab m1 m2 m3 m4 m5 using "C:\TablesTMqregQ25.rtf", /// Table A5
replace b(3) se(3) star(* .1 ** .05 *** .01) pr2 nogap
eststo clear

**** Table A1
tab pdsq

