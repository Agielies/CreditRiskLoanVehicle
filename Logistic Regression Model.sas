
/* importing data set */
proc import datafile = "C:\Users\migae\OneDrive\Documents\SASprogram\Vehicle\lt-vehicle-loan-default-prediction\train.csv" 
out = vehicle dbms = csv;
run;

/*dropping variables with no predictive ability through judgemental approach */


/*processing AVERAGE_ACCT_AGE CREDIT_HISTORY_LENGTH into numeric inputs as months and Date of birth into Age*/

data vehicle1;
set vehicle;
month = substr(AVERAGE_ACCT_AGE,index(AVERAGE_ACCT_AGE,'m')-2,2);
year = substr(AVERAGE_ACCT_AGE,1,1);
Ave_Acc_Age = year*12 + month;

month1 = substr(CREDIT_HISTORY_LENGTH ,index(CREDIT_HISTORY_LENGTH ,'m')-2,2);
year1 = substr(CREDIT_HISTORY_LENGTH ,1,1);
Cre_His_Len = year1*12 + month1;

Age = year(DisbursalDate)-  year(Date_of_Birth);

drop AVERAGE_ACCT_AGE CREDIT_HISTORY_LENGTH month month1 year year1 Date_of_Birth;
run;




data vehicle2;
set vehicle1  ;
drop UniqueID branch_id  Current_pincode_ID Employee_code_ID DisbursalDate;
run;


proc print data = vehicle2 (obs=10);
run;
/*defining variables into numeric, categorical-ordinal binary*/

%let num = disbursed_amount asset_cost ltv Age PERFORM_CNS_SCORE PRI_NO_OF_ACCTS PRI_ACTIVE_ACCTS PRI_OVERDUE_ACCTS PRI_CURRENT_BALANCE PRI_SANCTIONED_AMOUNT PRI_DISBURSED_AMOUNT SEC_NO_OF_ACCTS SEC_ACTIVE_ACCTS SEC_OVERDUE_ACCTS SEC_CURRENT_BALANCE SEC_SANCTIONED_AMOUNT SEC_DISBURSED_AMOUNT PRIMARY_INSTAL_AMT SEC_INSTAL_AMT NEW_ACCTS_IN_LAST_SIX_MONTHS DELINQUENT_ACCTS_IN_LAST_SIX_MON Ave_Acc_Age Cre_His_Len NO_OF_INQUIRIES ;
%let category = Employment_Type  State_ID PERFORM_CNS_SCORE_DESCRIPTION;
%let binary =  Aadhar_flag PAN_flag VoterID_flag Driving_flag Passport_flag MobileNo_Avl_Flag ;

/* checking for missing variables */
proc means data = vehicle2 n nmiss min max mean median;
var &num;
run; 

proc freq data = vehicle2;
tables &category &binary;
run;

proc freq data = vehicle2;
tables loan_default;
run;

proc freq data = vehicle2;
tables supplier_id;
run;


/* imputing missing values for employment type and dropping mobile vlag since it is one for each one */
data vehicle3;
set vehicle2;
if Employment_Type =  "" then Employment_Type = "Missing";
if Employment_Type =  "Salaried" then Employ_Sal = 1;
else Employ_Sal = 0;
if Employment_Type =  "Self employed" then Employ_Self = 1;
else Employ_Self = 0;
if Employment_Type =  "Missing" then Employ_Mis = 1;
else Employ_Mis= 0;
run;

PROC UNIVARIATE DATA = vehicle3;
var PERFORM_CNS_SCORE ;
HISTOGRAM PERFORM_CNS_SCORE  / NORMAL;
RUN;
proc univariate data =  vehicle3;
var PERFORM_CNS_SCORE ;
where PERFORM_CNS_SCORE >= 300;
run;



/*from the data we can see the lowest score is 300 so all accounts that have scores less than this will
data*/
data vehicle4;
set vehicle3;
if PERFORM_CNS_SCORE < 300then Cns_Score = 400;
if PERFORM_CNS_SCORE < 300then Cred_Hist = 0;
else Cred_hist = 1;
if PERFORM_CNS_SCORE >= 300 then Cns_Score = PERFORM_CNS_SCORE ;
run;

proc freq data = vehicle4;
tables &category &binary;
run;

proc means data = vehicle4 n nmiss min max mean median;
var &num Cns_Score;
run; 



/* investigating categorical variables for association */

/*proc freq data = vehicle4 ;
tables (Aadhar_flag PAN_flag VoterID_flag Driving_flag Passport_flag Employment_Type  State_ID Cred_Hist PERFORM_CNS_SCORE_DESCRIPTION)*loan_default /chisq  nocol nopercent;
run;
/*Cramers V
Aadhar = -0.0416
Pan_flag = 0.0020
VoterID_flag = 0.0437
Driving flag = -0.0058
Passport flag = -0.0076
Employment = 0.0286
State_ID = 0.0835
Credit_Hist = -0.0344
Perform_CNS_Descr = 0.0970
*/

%let num1 = disbursed_amount asset_cost ltv Age PERFORM_CNS_SCORE PRI_NO_OF_ACCTS PRI_ACTIVE_ACCTS PRI_OVERDUE_ACCTS PRI_CURRENT_BALANCE PRI_SANCTIONED_AMOUNT PRI_DISBURSED_AMOUNT  PRIMARY_INSTAL_AMT SEC_INSTAL_AMT NEW_ACCTS_IN_LAST_SIX_MONTHS DELINQUENT_ACCTS_IN_LAST_SIX_MON Ave_Acc_Age Cre_His_Len NO_OF_INQUIRIES;

proc univariate data =vehicle4;
var &num1;
histogram &num1;
run;




data vehicle5;
set vehicle4;
drop  Driving_flag Passport_flag Credit_Hist 
SEC_NO_OF_ACCTS  SEC_ACTIVE_ACCTS  SEC_OVERDUE_ACCTS SEC_NO_OF_ACCTS  
SEC_ACTIVE_ACCTS  SEC_OVERDUE_ACCTS SEC_CURRENT_BALANCE  SEC_SANCTIONED_AMOUNT  SEC_DISBURSED_AMOUNT;
disbursed_amount  =disbursed_amount/100000;
asset_cost  = asset_cost/100000;
PRI_CURRENT_BALANCE = PRI_CURRENT_BALANCE/1000000;
PRI_SANCTIONED_AMOUNT = PRI_SANCTIONED_AMOUNT/10000000 ;
PRI_DISBURSED_AMOUNT  = PRI_DISBURSED_AMOUNT/10000000;
PRIMARY_INSTAL_AMT  = PRIMARY_INSTAL_AMT/1000000;
SEC_INSTAL_AMT = SEC_INSTAL_AMT/100000;
run;


/* splitting data into training and test data sets */
proc sort data = vehicle5 out= vehicle_sort;
by  loan_default;
run;


proc surveyselect noprint data = vehicle_sort
                  samprate = 0.75 stratumseed = restore
				  out = vehicle_sample
				  seed = 1 outall;
strata loan_default;
run;

proc freq data = vehicle_sample;

tables loan_default*selected;
run;

data train(drop = selected SelectionProb SamplingWeight)
     test(drop = selected SelectionProb SamplingWeight);
set vehicle_sample;
if selected then output train;
else output test;
run;

/* using smoothed weights of evidence to collapse nomianl category data with too many levels */

%global rho1;

proc sql noprint;

select mean(loan_default) into :rho1
from train;
quit;


/*State ID woe */
proc means data = train sum nway noprint;
class State_ID;
var loan_default;
output out = counts sum = events;
run;

filename brswoe "C:\Users\migae\OneDrive\Documents\SASprogram\State_ID_swoe.sas";

data _null_;
file brswoe;
set counts end=last;
logit = log((events + &rho1*24)/(_FREQ_ - events + (1-&rho1)*24));
if _n_=1 then put "select (State_ID);" ;
put " when ('" State_ID + (-1) "') State_ID = " logit ";" ;
if last then do;
logit = log(&rho1/(1-&rho1));
put " otherwise State_ID = " logit ";" / "end;";
end;
run;


data train_woe;
set train;
%include brswoe / source2;

/*Perform_CNS*/

proc means data = train sum nway noprint;
class PERFORM_CNS_SCORE_DESCRIPTION;
var loan_default;
output out = counts sum = events;
run;



filename brswoe1 "C:\Users\migae\OneDrive\Documents\SASprogram\CNS_SCORE_DESCRIPTION_swoe.sas";

data _null_;
file brswoe1;
set counts end=last;
logit = log((events + &rho1*24)/(_FREQ_ - events + (1-&rho1)*24));
if _n_=1 then put "select (PERFORM_CNS_SCORE_DESCRIPTION);" ;
put " when ('" PERFORM_CNS_SCORE_DESCRIPTION + (-1) "') PERFORM_CNS_SCORE_DESCRIPTION = " logit ";" ;
if last then do;
logit = log(&rho1/(1-&rho1));
put " otherwise PERFORM_CNS_SCORE_DESCRIPTION = " logit ";" / "end;";
end;
run;


 
data train_woe1;
set train_woe;
%include brswoe1 / source2;
format Per_CSD 9.8;
Per_CSD = PERFORM_CNS_SCORE_DESCRIPTION;
run;

/*manufacturer_id woe */
proc means data = train sum nway noprint;
class manufacturer_id;
var loan_default;
output out = counts sum = events;
run;

filename brswoe2 "C:\Users\migae\OneDrive\Documents\SASprogram\manu_idwoe.sas";

data _null_;
file brswoe2;
set counts end=last;
logit = log((events + &rho1*24)/(_FREQ_ - events + (1-&rho1)*24));
if _n_=1 then put "select (manufacturer_id);" ;
put " when ('" State_ID + (-1) "') manufacturer_id = " logit ";" ;
if last then do;
logit = log(&rho1/(1-&rho1));
put " otherwise manufacturer_id = " logit ";" / "end;";
end;
run;


data train_woe2;
set train_woe1;
%include brswoe3 / source2;


%let input = &num1 Cns_Score State_ID Per_CSD Aadhar_flag PAN_flag VoterID_flag Employ_Sal Employ_Self manufacturer_id ;
/*clustering*/

ods select none;
ods output clusterquality = summary
			rsquare = clusters;
proc varclus data =train_woe1 maxeigen = 0.7 hi;
var &input;
run;

ods select all;

%global nvar;
data _null_;
set summary;

call symput('nvar',compress(NumberOfClusters));
run;

title1 "Variables by Cluster";

proc print data = clusters noobs label split = '*';
where NumberofClusters = &nvar;
var Cluster Variable RSquareRatio;
label RSquareRatio = "1- RSqaure*Ratio";
run;

title1 "Variation Explained by Clusters";

proc print data = summary label;
run;

%global reduced;
%let reduced = PRI_ACTIVE_ACCTS Per_CSD VoterID_flag PRI_DISBURSED_AMOUNT Employ_Sal Ave_Acc_Age asset_cost PRI_OVERDUE_ACCTS disbursed_amount 
NO_OF_INQUIRIES manufacturer_id SEC_INSTAL_AMT PRIMARY_INSTAL_AMT PAN_flag Age State_ID PRI_CURRENT_BALANCE ltv; 

ods output spearmancorr = spearman
			hoeffdingcorr = hoeffding;

 proc corr data = train_woe1 spearman hoeffding;
 var loan_default;
 with &reduced;
 run;

 ods select all;

 proc sort data = spearman;
 by variable;
 run;

 proc sort data = hoeffding;
 by variable;
run;

data correlations;
merge spearman(rename = (loan_default=scorr ploan_default = spvalue))
	  hoeffding(rename = (loan_default=hcorr ploan_default = hpvalue));
	  by variable;
	  scorr_abs = abs(scorr);
	  hcorr_abs = abs(hcorr);
 run;

 proc rank data = correlations out = correlations1 descending;
 var scorr_abs hcorr_abs;
 ranks ranksp rankho;
 run;

 proc sort data = correlations1;
 by ranksp;
 run;

 title1 "Spearman and Hoeffding rank";
 proc print data = correlations1 label split = '*';
 var variable ranksp rankho scorr spvalue hcorr hpvalue;
 label ranksp = 'Spearman rank*of variables'
 		scorr = 'Sperman Correlation'
		spvalue = 'Spearman p-value'
		rankho = 'Hoeffding rank*of variables'
		hcorr = 'Hoeffdin Correlation'
		hpvalue = 'Hoeffding p-value';
  run;

  /*drop asset_cost PRIMARY_INSTAL_AMT SEC_INSTAL_AMT PAN_flag*/





/*testing for interaction*/

%let screened = PRI_ACTIVE_ACCTS Per_CSD VoterID_flag PRI_DISBURSED_AMOUNT Employ_Sal Ave_Acc_Age PRI_OVERDUE_ACCTS disbursed_amount 
NO_OF_INQUIRIES manufacturer_id Age State_ID PRI_CURRENT_BALANCE ltv;

title1 "P-Value for Entry and Retention";

%global sl;

proc sql;
select 1 -probchi(log(sum(loan_default ge 0)),1) into :sl
from train_woe1;
quit;

title1 "Interaction Detection using Forward Selection";
proc logistic data = train_woe1;
model loan_default(event = '1') = &screened PRI_ACTIVE_ACCTS|Per_CSD|VoterID_flag|PRI_DISBURSED_AMOUNT|Employ_Sal|Ave_Acc_Age|PRI_OVERDUE_ACCTS|disbursed_amount 
NO_OF_INQUIRIES|manufacturer_id|Age|State_ID|PRI_CURRENT_BALANCE|ltv
 @2 /include = 14 clodds = pl
selection = forward slentry = &sl;
run;


title1 "Interaction Detection using Backward Selection";
proc logistic data = train_woe1;
model loan_default(event = '1') = &screened
PRI_ACTIVE_ACCTS*Ave_Acc_Age
PRI_ACTIVE_ACCTS*disbursed_amount
PRI_DISBURSED_AMOUNT*PRI_OVERDUE_ACCTS 
PRI_CURRENT_BALANCE*ltv  
Ave_Acc_Age*PRI_OVERDUE_ACCTS 
Ave_Acc_Age*disbursed_amount Age @2 /include = 14 clodds = pl
selection = backward slstay = &sl hier=single fast;
run;

%let inter = PRI_ACTIVE_ACCTS*Ave_Acc_Age
PRI_ACTIVE_ACCTS*disbursed_amount
PRI_DISBURSED_AMOUNT*PRI_OVERDUE_ACCTS 
PRI_CURRENT_BALANCE*ltv  
Ave_Acc_Age*PRI_OVERDUE_ACCTS 
Ave_Acc_Age*disbursed_amount;

title1 "Selecting best models using best models";

proc logistic data = train_woe1;
model loan_default(event = '1') = &screened
&inter  / selection = score best=1;
run;


/* model fit statistics */

%macro fitstat(data=,target=,event=,inputs=,best=,priorevent=);

ods select none;
ods output bestsubsets=score;

proc logistic data=&data namelen=50;
 model &target(event = "&event") = &inputs / selection=score best = &best;
run;

proc sql noprint; 
select variablesinmodel into: inputs1 -
from score;

select NumberOfVariables into : ic1 -
from score;
quit;

%let lastindx = &SQLOBS;

%do model_indx=1 %to &lastindx;

%let im = &&inputs&model_indx;
%let ic = &&ic&model_indx;

ods output scorefitstat = stat&ic;
proc logistic data = &data namelen=50;
	model &target(event = "&event") = &im;
	score data = &data out = scored fitstat
	priorevent = &priorevent;
   run;

   proc datasets
   library=work
   nodetails
   nolist;
   delete scored;
   run;
   quit;

   %end;

   data modelfit;
   set stat1 - stat&lastindx;
   model = _n_;
   run;

%mend fitstat;
 

%fitstat(data = train_woe1,target =loan_default,event=1,inputs = &screened &inter,best=1,priorevent=0.21);

proc sort data= modelfit;
by bic;

title1 "Fit Statistics";
ods select all;

proc print data= modelfit;
var model auc aic bic misclass adjrsquare brierscore;
run;

proc sql;
 select VariablesInModel into :selected
 from score
 where numberofvariables = 20;
 quit;

 %let selected = Per_CSD Employ_Sal 
PRI_OVERDUE_ACCTS 
disbursed_amount NO_OF_INQUIRIES 
manufacturer_id Age State_ID ltv 
PRI_ACTIVE_ACCTS*disbursed_amount 
;

proc logistic data = train_woe1;
model loan_default(event = '1') = &selected / ctable  PPROB = 0.5;
run;
/*validation*/

data valid;
set test;
%include brswoe / source2;
%include brswoe1 / source2;
%include brswoe2 / source2;
format Per_CSD 9.8;
Per_CSD = PERFORM_CNS_SCORE_DESCRIPTION;
run;

ods select roccurve scorefitstat;
proc logistic data = train_woe1;
model loan_default(event = '1') = &selected / ctable  PPROB = 0.5;
score data = valid out =scoval
priorevent = &rho1 outroc = roc fitstat;
run;

data scoval1; 
  set scoval;
  Pred = (P_1 >= 0.35);
  run;

  proc freq data = scoval1;
  tables Pred;
  run;

  proc print data = scoval1 (obs=100);
  run;

  data scoval2;
  set scoval1;
  where Pred = 0 & loan_default =1;
  run;

  options validvarname=any;
proc freq data = scoval1 ;
tables loan_default*Pred / nocol nopercent norow;
label loan_default ='Actual';
label Pred ='Predictive';
run;

proc print data = scoval (obs=100);
where
run;

title "Statistics in the ROC Data Set";
proc print data = roc(obs=10);
var _prob_ _sensit_ _1mspec_;
run;

data roc;
set roc;
cutoff = _PROB_;
specif= 1- _1MSPEC_;
tp = &rho1*_SENSIT_;
fn = &rho1*(1-_SENSIT_);
tn = (1-&rho1)*specif;
fp = (1-&rho1)*_1MSPEC_;
depth = tp + fp;
pospv = tp/depth;
negpv = tn/(1-depth);
acc = tp +tn;
lift = pospv/&rho1;
keep cutoff tn fp fn tp _SENSIT_ _1MSPEC_ specif depth pospv negpv acc lift;
run;

/*confusion Matrix*/
data leke;
set scoval;
Actual = input(F_loan_default,1.);
Predicted = input(I_loan_default,1.);
run;


proc freq data = leke;
tables Actual*Predicted / nocol norow nopercent;
run;


/* Creat a lift chart */

title1 "Life Chart for Validation Data";

proc sgplot data = roc;
where 0.005 <= depth <= 0.5;
series y=lift x=depth;
refline 1/ axis=y;
yaxis values=(0 to 3 by 1);
run; quit;
title1 ;

title "K-S Statistic for the Validation Data Set";
proc npar1way edf data = scoval;
class loan_default;
var p_1;
run;

proc print data = valid (obs=10);
run;

/* Confustion matrix */






































