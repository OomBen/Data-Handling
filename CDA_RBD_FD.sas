FILENAME REFILE '/folders/myshortcuts/My_Folders/SunspotData.xls'; /*Change to relevant directory for own computer*/
PROC IMPORT datafile=REFILE 
DBMS=xls
OUT=Sunspots;
GETNAMES=YES;
RUN;

/* Testing for normality: in UNIVARIATE:
	**HISTOGRAM
	**QQPLOT
	**PROBPLOT
*/

TITLE 'Normality Testing using Solar Cycle data from 1750 - 1849';

PROC UNIVARIATE data=Sunspots;
	HISTOGRAM sunspots / normal; 			/*"normal" option superimposes a normal density curve*/
	QQPLOT sunspots / normal;				/*The QQplot is used to determine whether the set is normally distributed (positive diagonal) or not (curved)*/
	PROBPLOT sunspots / normal;
RUN;

TITLE 'Using tentative order selection methods to determine model parameters';
PROC ARIMA data=Sunspots plots=series(acf pacf) OUT=DataResidual;	
/* to include sample and partial autocorrelations use series(acf pacf)*/

	/*Here is where to add the tentative order stuff */
	/*To request Dickey-Fuller Test use "stationarity=(adf=(0))"*/
	/*To do DF test on AR(5) make adf=(4) as 0 is included*/
	/*To do DF test for p=1,2,3,4 use adf=(0,1,2,3)*/
	IDENTIFY VAR=Sunspots STATIONARITY=(adf=(0))  NLAG=24 SCAN ESACF MINIC;
	ESTIMATE P=1 Q=1 METHOD=ml; 				/*ML, ULS, or CLS options */
	FORECAST LEAD=5;						/*Forecast statement needs prior "Estimate" statement*/
RUN;

/*Equivalent Dickey-Fuller test in PROC AUTOREG:
PROC AUTOREG data=Sunspots;
	MODEL sunspots= /stationarity=(adf=(0 1 2 3));
RUN;*/

PROC ARIMA data=Sunspots;
	IDENTIFY VAR=Sunspots NLAG=6 OUTCOV=This; /*Outcov statement makes a dataset containing just th
												sample covariances */
												
	ESTIMATE P=1 METHOD=cls; 				  /*For estimating conditional least squares*/
											  /* The CLS estimates are: Phi, Mu and Theta0 */
RUN;


/*Working with Non-Stationary Time series*/
PROC ARIMA data=Sunspots;
	IDENTIFY VAR=sunspots(1); 				  /*This requests an ARIMA procedure for the first
												difference. (Second difference would be sunspots(2))
												with 2 being the second order difference*/
	ESTIMATE Q=1 METHOD=ml;
RUN;

/*If there are explanatory variables that want to be analyzed (dummies or sine/cosine)
  add the options "crosscorr=(dummy1 dummy2 sine1 cos1)" and "input=(dummy1 dummy2 sine1 cos1)"
  to get more useful metrics*/
PROC ARIMA data=Sunspots OUT=DataResidual1; /*If you want to use the residual set*/

	/*NOTE: "residual","est" and "forecast" are generated keywords so they don't have
	  to be explicitly declared*/

	IDENTIFY VAR=Sunspots NLAG=0; /*CROSSCORR=(d1 d2 s1 c1 s2 c2) as an option for IDENTIFY statement*/
	ESTIMATE P=1; /*INPUT=(d1 d2 s1 c1 s2 c2) as an option for the ESTIMATE statement*/
	FORECAST LEAD=0;
RUN;
 
/*Doing tests on the residuals using PROC UNIVARIATE*/
PROC UNIVARIATE data=DataResidual;
	HISTOGRAM residual / normal (mu=est sigma=est);
	QQPLOT residual /normal (mu=est sigma=est); /*option "square" displays the graph as a square*/
RUN;

/*The R-square (coefficient of determination) is not given in ARIMA or UNIVARIATE so to obtain
  we use PROC CORR and the forecast from the residuals dataset*/
PROC CORR data=DataResidual;
	VAR sunspots forecast;
RUN;

/* NOTE: 
	**Non-stationary time series forecast diverges
	**Stationary time series forecast converges to mean
*/




/*_______________________________CATEGORICAL DATA ANALYSIS___________________________________*/

TITLE 'Categorical Data Analysis with Manually Entered Detergent Data';

PROC FORMAT;
VALUE group 1 = 'Detergent I'
		 	2 = 'Detergent II'
		 	3 = 'Detergnet III';
RUN;

DATA white;
INPUT group reading@@;
CARDS;
1 77 1 81 1 71 1 76 1 80
2 72 2 58 2 74 2 66 2 70
3 76 3 85 3 82 3 80 3 77
;

PROC GLM data=white;
	CLASS group;
	MODEL reading = group; /* same as model y=x */
	MEANS group / scheffe lines cldiff;	/*Scheffe Testing allows us to determine which pairs are
										significant in relation to each other*/
RUN;

/*The exact same code can be used with PROC ANOVA instead, output is the same */
PROC ANOVA data=white;
	CLASS group;
	MODEL reading = group;
	MEANS group / scheffe lines cldiff;
RUN;


TITLE 'Randomized Block Design: 2 Way ANOVA using Manual Input Data';
DATA estimate;
INPUT engineer job cost@@;
CARDS;
1 1 4.6 1 2 6.2 1 3 5.0 1 4 6.6
2 1 4.9 2 2 6.3 2 3 5.4 2 4 6.8
3 1 4.4 3 2 5.9 3 3 5.4 3 4 6.3
;

/* For testing treatments, use the paramter values from the Type III paramter output*/
PROC GLM data=estimate;
CLASS engineer job; 			/*"class" statement needs to be your explanatory variables*/
	MODEL cost = engineer job;
	MEANS engineer / scheffe lines cldiff;
	MEANS job /scheffe lines cldiff;
RUN;


TITLE 'Dummifying variables for use in the GLM directly from SAS';
DATA estimated;
/*NOTE: Indentation is important when creating dummy variables in SAS*/

INPUT engineer job cost@@;
	x1a = 0; x2a = 0;				/*Treatment Factor dummies*/
	if engineer = 1 then x1a = 1;
	if engineer = 2 then x2a = 1;
	if engineer = 3 then do;
				x1a = -1; x2a = -1;
				end;
	xb1 = 0; xb2 = 0; xb3 = 0;		/*Blocking Factor dummies*/
	if job = 1 then xb1 = 1;
	if job = 2 then xb2 = 1;
	if job = 3 then xb3 = 1;
	if job = 4 then do;
				xb1 = -1; xb2 = -1; xb3 = -1;
				end;
CARDS;		/*Data input using cards happens after declaring dummy variables*/
1 1 4.6		1 2 6.2 	1 3 5.0 	1 4 6.6
2 1 4.9 	2 2 6.3 	2 3 5.4 	2 4 6.8
3 1 4.4 	3 2 5.9 	3 3 5.4 	3 4 6.3
;
PROC PRINT data=estimated;
RUN;

PROC GLM data=estimated;
MODEL cost = x1a x2a xb1 xb2 xb3;
CONTRAST 'engineer' x1a 1, x2a 1; 	/*CONTRAST STATEMENT: Test whether a specific effect is zero */
CONTRAST 'job' xb1 1, xb2 1, xb3 1; 
/*contrast..label...effect...value*/
RUN;

/*			__Randomized Blocked Design vs Factorial Design__

**RBD:  Table contains one observation per cell.
		Compare means of k treatments.

**FD:   Table contains two or more observations per cell.
		Try to detect interaction
*/

/*_________________FACTORIAL DESIGN________________*/
PROC FORMAT;
VALUE aa 1 = 'Machine I'
	     2 = 'Machine II';

VALUE bb 1 = 'Cork'
		 2 = 'Rubber'
		 3 = 'Plastic';
RUN;

DATA items;
INPUT machine material cost@@;
	x1a = 0; x2a = 0;				/*Treatment Factor dummies*/
	if machine = 1 then x1a = 1;
	if machine = 2 then do;
				x1a = -1; x2a = -1;
				end;
	xb1 = 0; xb2 = 0; xb3 = 0;		/*Blocking Factor dummies*/
	if material = 1 then xb1 = 1;
	if material = 2 then xb2 = 1;
	if material = 3 then do;
				xb1 = -1; xb2 = -1; xb3 = -1;
				end;
CARDS;
1 1 4.31 1 2 3.36 1 3 4.01
1 1 4.27 1 2 3.42 1 3 3.94
1 1 4.40 1 2 3.48 1 3 3.89
2 1 3.94 2 2 3.91 2 3 3.48
2 1 3.81 2 2 3.80 2 3 3.53
2 1 3.99 2 2 3.85 2 3 3.42
;

PROC UNIVARIATE data=items;
VAR cost;
RUN;

PROC ANOVA data=items;
CLASS machine material; 		/*Remember to include all explanatory model variables in class statement*/
MODEL cost = machine material machine*material;
FORMAT machine aa. material bb.;
RUN;

/* Ho: No Interaction between factors
   Ha: Interaction between factors
   
the machine*material is a measure of interaction :. If asked whether there is interaction
refer to the p_value of the machine*material parameter
*/

/* Ho: BetaA1 = 0
		or
   Ho: BetaB1 = BetaB2 = 0
   		or
   Ho: BetaAB11 = BetaAB12 = 0
   
   for all these type of tests, use the "contrast statement in PROC GLM"
*/

PROC GLM data=items;
	MODEL cost = x1a x2a xb1 xb2 xb3;
	CONTRAST 'machine' x1a 1;
	CONTRAST 'material' xb1 1, xb2 1;
	FORMAT machine aa. material bb.;
RUN;

/*Definition: Covariate:: a continuous control variable that is observed rather than manipulated
						  but can affect the outcome of a study (example IQ in test scores)*/


