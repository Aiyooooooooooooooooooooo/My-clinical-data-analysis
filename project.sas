libname L1 "C:\Users\amrut\OneDrive\Documents\SAS";
filename f1 "C:\Users\amrut\OneDrive\Documents\SAS\project\project data1.txt";
data L1.First_study_project1 ;
infile f1 dlm = '09'x dsd missover;
input patient_ID age state$ length_of_stay total_charges ;
run;
proc print data= L1.First_study_project1;
run;
libname L1 "C:\Users\amrut\OneDrive\Documents\SAS";
filename f1 "C:\Users\amrut\OneDrive\Documents\SAS\project\project data2.txt";
data L1.Second_study_project1 ;
infile f1 dlm = '09'x dsd missover;
input patient_ID group$ test_score ;
run;
proc print data= L1.Second_study_project1;
run;
data L1. final;
    merge First_study_project1 (in=a)
          Second_study_project1 (in=b);
    by patient_ID;
    if a=1 and b=1 and not (missing(age) or missing(total_charges)) and not (group = "n/a");
run;

proc print data= L1.final;
run;
proc surveyselect data= L1.final
                   method=srs
                   sampsize=1000
                   seed=8497
                   out= L1.final_sample;
run;

proc print data= L1.final_sample;
run;
proc means data = L1.final_sample;
class group;
run;
proc univariate data= L1.final_sample;
var test_score;
run;
proc univariate data= L1.final_sample;
var test_score;
histogram/normal;
run;
proc sgplot data= L1.final_sample;
   vbox test_score / category= group;
run;
proc GLM data= L1.final_sample;
class group;
model test_score= group;
run;
PROC CORR DATA= L1.final_sample spearman;
var age test_score;
run;
proc corr data= L1.final_sample spearman;
var length_of_stay  test_score;
run;
data L1.test;
set L1.final_sample;
if age<= 40 then age_group= 1;
else if age>40 and age<=65 then age_group= 2;
else age_group= 3;
run; 
proc print data= L1.test;
run;
proc GLM data= L1. test;
class group age_group;
model test_score= group age_group
                  group*age_group;
means group age_group;
run;
proc GLM data= L1.test;
class age_group;
model test_score= age_group;
means age_group/scheffe;
run;
data L1.test2;
set L1.test;
if test_score <= 54 then test_group= 1;
else if test_score>54 and test_score <= 110 then test_group= 2;
else test_group= 3;
run;
proc print data = L1.test2;
run;
proc freq data= L1.test2;
tables group*test_group*age_group;
run;
data L1.test3;
   set L1.final_sample;
   high_blood_sugar_binary = (test_score > 110);
run;
proc print data=L1.test3;
run;
proc logistic data= L1.test3;
   class group;
   model high_blood_sugar_binary(event='1') = group;
run;
proc logistic data= L1.test3;
class group;
model high_blood_sugar_binary(event = '1') = group age;
run;


