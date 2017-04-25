/*************************************************************************************************************/
/*********************************** Loading in data and creating variables **********************************/
/*************************************************************************************************************/

libname add 'F:/Desktop/applied_regression_ii/group_projects/data_analysis/addhealth_data';

data w1data; set add.wave1data; run;
data w1weights; set add.w1weights; run;
proc sort data = w1data; by aid; run;
proc sort data = w1weights; by aid; run;
data w1; merge w1data w1weights; by aid; run;

data w1;
	set w1;

	/***** Creating a composite scale of depth of school health & safety education topics: *****/

	measure1 = .;
	if H1TS1 in (0,1) then do;
		measure1 = H1TS1;
		end;
	measure2 = .;
	if H1TS2 in (0,1) then do;
		measure2 = H1TS2;
		end;
	measure3 = .;
	if H1TS3 in (0,1) then do;
		measure3 = H1TS3;
		end;
	measure4 = .;
	if H1TS4 in (0,1) then do;
		measure4 = H1TS4;
		end;
	measure5 = .;
	if H1TS5 in (0,1) then do;
		measure5 = H1TS5;
		end;
	measure6 = .;
	if H1TS6 in (0,1) then do;
		measure6 = H1TS6;
		end;
	measure7 = .;
	if H1TS7 in (0,1) then do;
		measure7 = H1TS7;
		end;
	measure8 = .;
	if H1TS8 in (0,1) then do;
		measure8 = H1TS8;
		end;
	measure9 = .;
	if H1TS9 in (0,1) then do;
		measure9 = H1TS9;
		end;
	measure10 = .;
	if H1TS10 in (0,1) then do;
		measure10 = H1TS10;
		end;
	measure11 = .;
	if H1TS11 in (0,1) then do;
		measure11 = H1TS11;
		end;
	measure12 = .;
	if H1TS12 in (0,1) then do;
		measure12 = H1TS12;
		end;
	measure13 = .;
	if H1TS13 in (0,1) then do;
		measure13 = H1TS13;
		end;
	measure14 = .;
	if H1TS14 in (0,1) then do;
		measure14 = H1TS14;
		end;
	measure15 = .;
	if H1TS15 in (0,1) then do;
		measure15 = H1TS15;
		end;
	measure16 = .;
	if H1TS16 in (0,1) then do;
		measure16 = H1TS16;
		end;
	measure17 = .;
	if H1TS17 in (0,1) then do;
		measure17 = H1TS17;
		end;

	school_topics = sum(
		measure1, measure2, measure3, measure4, measure5, measure6, measure7, measure8, measure9,
		measure10, measure11, measure12, measure13, measure14, measure15, measure16, measure17);

	* Binning school_topics: ;

	if school_topics >= 14 then topic_bin = 1;
	else if school_topics in (0:13) then topic_bin = 0;
	else topic_bin = .;

	/***** Creating an outcome variable: drinking *****/

	* Drink or not: ;

	anydrink=.;
	if H1TO15 in (1:6) then anydrink=1; * Has had a drink in past 12 months ;
	else if H1TO15=7 or H1TO12=0 then anydrink=0; * Has not had a drink in past 12 months ;

	* At-risk drinking: ;
	* Note: At-risk for men is either 5+ drinks per drinking occasion or 14+ drinks per week on average.
	At-risk for women is either 4+ drinks per drinking occasion or 7+ drinks per week on average. ;

	riskydrink=.; * If anyone drinks like an at-risk male, then they are at-risk: ;
	if H1TO15 in (1:6) then do;
		if H1TO16 in (5:95) or (H1TO15=1 and H1TO16 in (2:95)) then riskydrink=1;
		else riskydrink=0;
		end;
	else if H1TO15=7 or H1TO12=0 then riskydrink=0;

	if BIO_SEX=2 then do; * Females ;
		if H1TO15 in (1:6) then do;
			if H1TO16 in (4:95) or (H1TO15=1 and H1TO16 in (1:95)) or (H1TO15=2 and H1TO16 in (3:95)) then riskydrink=1;
			end;
		end;

	* Making final 3-level outcome: ;

	if riskydrink=1 then drinklevel=2;
	else if anydrink=1 and riskydrink=0 then drinklevel=1;
	else if anydrink=0 then drinklevel=0;
	else drinklevel=.;

	/***** Creating variables for race *****/

	* Race [cat.(4)] ;

	if H1GI8 in (1,2,4) then BestRace=H1GI8;
	else BestRace=99999;
	* Which one category best describe your racial background? ;
					* 1=White, 2=Black/African American, 4=Asian/Pacific Islander ;
	if H1GI4 in (0,1) then His=H1GI4;
	else His=0;
	if H1GI6A in (0,1) then White=H1GI6A;
	else White=0;
	if H1GI6B in (0,1) then Black=H1GI6B;
	else Black=0;
	if H1GI6D in (0,1) then Asian=H1GI6D;
	else Asian=0;

	if His=1 then Race=1; * Hispanic or Latino ;
	else if His^=1 and White=1 then Race=2; * White ;
	else if His^=1 and Black=1 then Race=3; * Black or African American ;
	else if His^=1 and Asian=1 then Race=4; * Asian or Pacific Islander ;
	else Race=.;
	label Race="Race [cat.(4)]";

	if BestRace=1 then do;
		if His=1 then Race=1;
		else Race=2;
		end;
	if BestRace=2 then Race=3;
	if BestRace=4 then Race=4;

	* In variable: 4 major race groups vs. not [bin.] ;

	if Race in (1,2,3,4) then InRace=1;
	else InRace=0;
	label InRace="In variable: 4 major race groups vs. not [bin.]";

	* Dichotomous race dummy variables ;

	if InRace=1 then do;
		if Race=1 then R_His=1;
		else R_His=0;
		end;
	else R_His=.;
	label R_His="Hispanic [bin.]";

	if InRace=1 then do;
		if Race=2 then R_White=1;
		else R_White=0;
		end;
	else R_White=.;
	label R_White="White [bin.]";

	if InRace=1 then do;
		if Race=3 then R_Black=1;
		else R_Black=0;
		end;
	else R_Black=.;
	label R_Black="Black [bin.]";

	if InRace=1 then do;
		if Race=4 then R_Asian=1;
		else R_Asian=0;
		end;
	else R_Asian=.;
	label R_Asian="Asian [bin.]";

	/***** Age *****/

	* W1 - Age [cont.] ;
	if 0<=H1GI1Y<96 then do;
		BirthYearw1=H1GI1Y;
		end;
	else BirthYearw1=.;
	if IYEAR in (94,95) then do;
		InterviewYearw1=IYEAR;
		end;
	else InterviewYearw1=.;
	Agew1=InterviewYearw1-BirthYearw1;
	label Agew1="W1 - Age [cont.]";

	* W1 - Age [cat.(3)] ;
	if 10<=Agew1<=14 then AgeCat3=0;
	else if 15<=Agew1<=17 then AgeCat3=1;
	else if 18<=Agew1<=40 then AgeCat3=2;
	else AgeCat3=.;
	label AgeCat3="W1 - Age [cat.(3)]";

	* Dummy variables for AgeCat3 ;
	if AgeCat3=0 then AgeCat30=1;
	else if AgeCat3 in (1,2) then AgeCat30=0;
	else AgeCat30=.;
	label AgeCat30="Binary variable for AgeCat3=0 [bin.]";

	if AgeCat3=1 then AgeCat31=1;
	else if AgeCat3 in (0,2) then AgeCat31=0;
	else AgeCat31=.;
	label AgeCat31="Binary variable for AgeCat3=1 [bin.]";

	if AgeCat3=2 then AgeCat32=1;
	else if AgeCat3 in (0,1) then AgeCat32=0;
	else AgeCat32=.;
	label AgeCat32="Binary variable for AgeCat3=2 [bin.]";

	/***** Sex *****/

	if BIO_SEX=1 then Sex=1;
	else if BIO_SEX=2 then Sex=0;
	else Sex=.;
	label Sex="Sex [bin.]";

	/***** Parent's Education *****/

	* Responding parent education level [cat.(3)] ;
	if PA12 in (1,2,3,5) then PEduc=0; * Less than high school ;
	else if PA12=4 then PEduc=1; * High school ;
	else if PA12 in (6:9) then PEduc=2; * More than high school ;
	else PEduc=.;
	label PEduc="Responding parent's education level [cat.(3)]";

	* Dummy variables for PEduc ;
	if PEduc=0 then PEduc0=1;
	else if PEduc in (1,2) then PEduc0=0;
	else PEduc0=.;
	label PEduc0="Binary variable for PEduc=0";

	if PEduc=1 then PEduc1=1;
	else if PEduc in (0,2) then PEduc1=0;
	else PEduc1=.;
	label PEduc1="Binary variable for PEduc=1";

	if Peduc=2 then PEduc2=1;
	else if PEduc in (0,1) then PEduc2=0;
	else PEduc2=.;
	label PEduc2="Binary variable for PEduc=2";

	/***** Urbanicity *****/

	* Urban/Rural [cat.(3)] ;
	if H1IR12=1 then Urban=0; * Rural ;
	else if H1IR12=2 then Urban=1; * Suburban ;
	else if H1IR12 in (3:6) then Urban=2; * Urban ;
	else Urban=.;
	label Urban="Urban/Rural [cat.(3)]";

	* Dummy variables for Urban ;
	if Urban=0 then Urban0=1;
	else if Urban in (1,2) then Urban0=0;
	else Urban0=.;
	label Urban0="Binary variable for Urban=0 [bin.]";

	if Urban=1 then Urban1=1;
	else if Urban in (0,2) then Urban1=0;
	else Urban1=.;
	label Urban1="Binary variable for Urban=1 [bin.]";

	if Urban=2 then Urban2=1;
	else if Urban in (0,1) then Urban2=0;
	else Urban2=.;
	label Urban2="Binary variable for Urban=2 [bin.]";

	/***** Income *****/

	* Income [cat.(4)] ;
	if 0<=PA55<=20 then Income=0;
	else if 20<PA55<=35 then Income=1;
	else if 35<PA55<=70 then Income=2;
	else if 70<PA55<=9995 then Income=3;
	else Income=.;
	label Income="Income [cat.(4)]";

	* Dummy variables for Income ;
	if Income=0 then Income0=1;
	else if Income in (1,2,3) then Income0=0;
	else Income0=.;
	label Income0="Binary variable for Income=0 [bin.]";

	if Income=1 then Income1=1;
	else if Income in (0,2,3) then Income1=0;
	else Income1=.;
	label Income1="Binary variable for Income=1 [bin.]";

	if Income=2 then Income2=1;
	else if Income in (0,1,3) then Income2=0;
	else Income2=.;
	label Income2="Binary variable for Income=2 [bin.]";

	if Income=3 then Income3=1;
	else if Income in (0,1,2) then Income3=0;
	else Income3=.;
	label Income3="Binary variable for Income=3 [bin.]";

	/***** Relationships at school: *****/

	rel_teacher = .;
	if H1ED15 in (0:4) then do;
		rel_teacher = H1ED15;
		end;

	rel_students = .;
	if H1ED18 in (0:4) then do;
		rel_students = H1ED18;
		end;

	rel_school = sum(rel_teacher + rel_students);

run;

/*************************************************************************************************************/
/*********************************** Taking a look at general descriptives ***********************************/
/*************************************************************************************************************/

/**** Just to take a look *****/

proc freq data = w1; * Before dummy-coding ;
tables
	topic_bin drinklevel race agecat3 sex peduc urban income / missing;
run;

proc freq data = w1; * After dummy-coding ;
tables
	topic_bin drinklevel /* x & y */
	r_white r_black r_asian r_his /* Race */
	agecat30 agecat31 agecat32 /* Age */
	Sex
	PEduc0 PEduc1 PEduc2 /* Parent's education */
	Urban0 Urban1 Urban2 /* Urban */
	Income0 Income1 Income2 Income3 /* Income */ / missing;
run;

/***** Table 1 - Looking at crosstabs between X & Y: *****/

proc sort data = w1; by topic_bin; run;
proc surveyfreq data = w1; weight gswgt1; by topic_bin; tables drinklevel / chisq; run;quit;

proc sort data = w1; by drinklevel; run;
proc surveymeans data = w1; weight gswgt1; by drinklevel; var rel_school; run;quit;

proc sort data = w1; by race; run;
proc surveyfreq data = w1; weight gswgt1; by race; tables drinklevel / chisq; run;quit;

proc sort data = w1; by agecat3; run;
proc surveyfreq data = w1; weight gswgt1; by agecat3; tables drinklevel / chisq; run;quit;

proc sort data = w1; by sex; run;
proc surveyfreq data = w1; weight gswgt1; by sex; tables drinklevel / chisq; run;quit;

proc sort data = w1; by peduc; run;
proc surveyfreq data = w1; weight gswgt1; by peduc; tables drinklevel / chisq; run;quit;

proc sort data = w1; by urban; run;
proc surveyfreq data = w1; weight gswgt1; by urban; tables drinklevel / chisq; run;quit;

proc sort data = w1; by income; run;
proc surveyfreq data = w1; weight gswgt1; by income; tables drinklevel / chisq; run;quit;

/*************************************************************************************************************/
/********************************************** School curriculum ********************************************/
/*************************************************************************************************************/

/***** Crude OR *****/

proc surveylogistic data = w1;
model drinklevel (ref='0') = topic_bin / link=glogit clparm;
weight GSWGT1;
run;

/***** Adjusted OR *****/

proc surveylogistic data = w1;
model drinklevel (ref='0') = topic_bin
	r_his r_black r_asian
	agecat31 agecat32
	sex
	peduc1 peduc2
	urban1 urban2
	income1 income2 income3
	/ link=glogit clparm;
weight GSWGT1;
run;

/*************************************************************************************************************/
/******************************************** School relationships *******************************************/
/*************************************************************************************************************/

proc freq data = w1;
tables rel_teacher rel_students rel_school / missing;
run;

/***** Crude OR *****/

proc surveylogistic data = w1;
model drinklevel (ref='0') = rel_school / link=glogit clparm;
weight GSWGT1;
run;

/***** Adjusted OR *****/

proc surveylogistic data = w1;
model drinklevel (ref='0') = rel_school
	r_his r_black r_asian
	agecat31 agecat32
	sex
	peduc1 peduc2
	urban1 urban2
	income1 income2 income3
	/ link=glogit clparm;
weight GSWGT1;
run;

/***** Interaction model *****/

proc surveylogistic data = w1;
model drinklevel (ref='0') = topic_bin rel_school topic_bin*rel_school / link=glogit;
weight GSWGT1;
run;



proc surveylogistic data = w1;
model drinklevel (ref='0') = topic_bin | h1ed2 / link=glogit;
weight GSWGT1;
run;
