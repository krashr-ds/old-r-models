/* SAS Regression Models - HYPERTEN & TIMEHYP
   Proposed Models Based on Exploratory Analysis of Framingham Data
   K Rasku 6/16/2020   Applied Healthcare Statistics
*/

libname hca202 "~/hca202";


/* Define at risk population for period 1 */
data work.frmgham1;
    set hca202.frmgham;
    where PREVHYP = 0 AND PERIOD = 1;
run;

/* Set up */
proc format;
    value sexF
        1 = 'Male'
        2 = 'Female';
    value cursmokeF
        0 = 'Not current smoker'
        1 = 'Current smoker';
    value htnF
        0 = 'No HTN'
        1 = 'Incident HTN';
    value diabetesF
    	0 = 'No Diabetes'
    	1 = 'Diabetes';
run;

/* apply formats */
data work.frmgham2;
    set work.frmgham1;
    format  sex sexF.
            cursmoke cursmokeF.
            hyperten htnF.
            diabetes diabetesF.;
    if bmi > 30.0 then obeseCategory = 1;
    else obeseCategory = 0;

    
    /*bpCategory - diastolic first because most people with high diastolic pressure also have high systolic pressure*/
   
	length bpCategory $ 10;
	length ageGroup $ 10;

    /* R cut values: 0, 60, 80, 90, 120, Inf */
    /* SAS values: LOW/NORM combined for logit regression, HYP2 and CRISIS combined due to low n for CRISIS category */
    if diabp < 80 then bpCategory = "LOW/NORM";
    else if diabp < 90 then bpCategory = "HYP1";
    else bpCategory = "HYP2+";

    
    if diabp < 90 then do;
    	/* R cut values: 0, 91, 121, 131, 141, 181, Inf */
    	if sysbp < 121 then bpCategory = "LOW/NORM";
    	else if sysbp < 131 then bpCategory = "PREH";
    	else if sysbp < 141 then bpCategory = "HYP1";
    	else bpCategory = "HYP2+";

    end;
    
    if      age  < 35               then ageGroup = '0 - 35';
    else if age >= 35 and age <= 44 then ageGroup = '35 - 44';
    else if age >= 45 and age <= 54 then ageGroup = '45 - 54';
    else if age >= 55 and age <= 64 then ageGroup = '55 - 64';
    else if age >= 65 and age <= 74 then ageGroup = '65 - 74';
    else if age >= 75 and age <= 84 then ageGroup = '75 - 84';
    else if age > 85                then ageGroup = '85+';
    
    
    label ageGroup = "Age Category - 10 yr intrvls";
    label bmi = 'Body Mass Index (kg/m**2)';
    label age = 'Age (Years)';
    label sysbp = 'Systolic Blood Pressure (mmHg)';
    label diabp = 'Diastolic Blood Pressure (mmHg)';
    label obeseCategory = 'BMI < or > 30 (kg/m**2)';
    label bpCategory = 'Severity of HTN';
    
run;

%let dat = work.frmgham2;

/* Measures of Central Tendency */
ods graphics / imagemap=on;
proc freq data=&dat order=formatted NLEVELS;
	tables ageGroup sex diabetes obeseCategory cursmoke / plots=FREQPLOT;
	tables bpCategory / plots=freqplot;
run;
ods graphics off;

proc means data=&dat alpha=0.05 clm mean median mode n qrange range;
	var sysbp diabp sex diabetes cursmoke bmi age;
run;

proc summary data=&dat;
	class bpCategory sex cursmoke obeseCategory diabetes;
	var timehyp;
	output out=stats n=ht_n mean=ht_mean std=ht_sd median=ht_median / levels ways;
run;

/* Death from CVD */
proc summary data=&dat(where=(death=1 & timecvd=timedth));
	class bpCategory;
	var timedth;
	output out=stats n=ht_n mean=ht_mean std=ht_sd median=ht_median / levels ways;
run;

/* Days to Official Diagnosis */
proc summary data=&dat;
	class bpCategory hyperten death;
	var timehyp;
	output out=stats n=ht_n mean=ht_mean std=ht_sd median=ht_median / levels ways;
run;

/* Died before or at Diagnosis */
/* 4 died before; 348 died at the time of diagnosis */
proc summary data=&dat(where=(death=1 & timedth=timehyp));
	class bpCategory;
	var timedth;
	output out=stats n=ht_n mean=ht_mean std=ht_sd median=ht_median / levels ways;
run;

/* Alive and hypertensive and prescribed Meds */
/* ZERO individuals out of ZERO */
proc summary data=&dat(where=(death=0 & prevhyp=1));
	class bpCategory bpmeds;
	var timehyp;
	output out=stats n=ht_n mean=ht_mean std=ht_sd median=ht_median / levels ways;
run;
/* ZERO individuals out of 1365 */
proc summary data=&dat(where=(death=0 & hyperten=1));
	class bpCategory bpmeds;
	var timehyp;
	output out=stats n=ht_n mean=ht_mean std=ht_sd median=ht_median / levels ways;
run;

/* HISTOGRAMS */
ods graphics / imagemap=on;
proc univariate data=&dat alpha=0.05 plot normal;
	var timehyp hyperten age sysbp diabp diabetes bmi cursmoke;
	histogram;
run;
ods graphics off;

/* SYSBP graphs */
ods graphics / reset width=8.0in height=8.0in imagemap;
proc sgplot data=&dat;
    scatter x=age y=sysbp / transparency=0.0 name='Scatter';
    reg x=age y=sysbp / nomarkers CLM name='Regression';
    xaxis grid;
    yaxis grid;
run;
ods graphics / reset;

ods graphics / reset width=8.0in height=8.0in imagemap;
proc sgplot data=&dat;
    scatter x=bmi y=sysbp / transparency=0.0 name='Scatter';
    reg x=bmi y=sysbp / nomarkers CLM name='Regression';
    xaxis grid;
    yaxis grid;
run;
ods graphics / reset;

/* DIABP graphs */
/* Only BMI showed a significant effect. */
ods graphics / reset width=8.0in height=8.0in imagemap;
proc sgplot data=&dat;
    scatter x=bmi y=diabp / transparency=0.0 name='Scatter';
    reg x=bmi y=diabp / nomarkers CLM name='Regression';
    xaxis grid;
    yaxis grid;
run;
ods graphics / reset;

/* Basic Linear Regression: Age, Systolic BP, Diastolic BP, Diabetes, BMI, Smoking Status */
/* R-squared 0.29 */
ods graphics / imagemap=on;
proc reg data=&dat plots=all;
    model timehyp = age sysbp diabp diabetes bmi cursmoke ;
run;
ods graphics off;

/* Systolic only */
/* R-squared 0.28 */
ods graphics / imagemap=on;
proc reg data=&dat plots=all;
    model timehyp = age sysbp diabetes bmi ;
run;
ods graphics off;

/* Logistic Regression: Outcome Hypertension */
/* obeseCategory: 0 = BMI < 30, 1 = BMI > 30 

/* c-statistic: 0.723						*/
ods graphics / imagemap=on;
proc logistic data=&dat plots=all;
    class   sex (param = reference ref = 'Male')
            cursmoke (param = reference ref = 'Not current smoker')
            bpCategory (param = reference ref = 'LOW/NORM')
            obeseCategory (param = reference ref = '0') /* BMI < 30 */
            diabetes (param = reference ref = 'No Diabetes');
    model hyperten (event='Incident HTN') = age sex cursmoke bpCategory obeseCategory diabetes ;
run;
ods graphics off;

/* Logistic Regression: Outcome Hypertension */
/* c-statistic: 0.742  						 */
ods graphics / imagemap=on;
proc logistic data=&dat plots=all;
    class   sex (param = reference ref = 'Male')
            cursmoke (param = reference ref = 'Not current smoker')
            diabetes (param = reference ref = 'No Diabetes');
    model hyperten (event='Incident HTN') = sex cursmoke sysbp bmi diabetes ;
run;
ods graphics off;

