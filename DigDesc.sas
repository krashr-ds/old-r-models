/* Run IMPORTDIG.sas First. */

proc format;
    value raceF
        1 = 'white'
        2 = 'non-white'; 

run;

proc means data=HCA202.DIGMALES mean median mode min max maxdec=3;
	title "Measures of Central Tendency: Basic Characteristics";
	var age sex race bmi heartrte sysbp diabp functcls chfdur;
run;

proc sort data=hca202.digmales;
	by race;
run;

proc means data=HCA202.DIGMALES mean maxdec=3;
	title "Measures of Central Tendency: Prevalent Conditions";
	var prevmi angina diabetes hyperten;
	class race;
	format race raceF.;
run;

proc means data=HCA202.DIGMALES mean maxdec=3;
	title "Measures of Central Tendency: Treatments";
	var trtmt diuret diuretk aceinhib nitrates hydral vasod;
	class race;
	format race raceF.;
run;

proc means data=HCA202.DIGMALES mean median max min maxdec=3;
	title "CT-Ratio, EF %, NYHA Class and CHF Duration (months) by Race and CHF Etiology";
	class race chfetiol;
	var chestx ejf_per functcls chfdur;
	format race raceF. ;
run;
