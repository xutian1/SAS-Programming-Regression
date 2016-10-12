ods html close; 
options nodate nonumber;
title;
ods rtf file='C:\BostonHousing.rtf' nogtitle startpage=no;
ods noproctitle;

* Harrison-Rubinfeld data
  CRIM     per capita crime rate by town
  ZN       proportion of residential land zoned for lots over 25,000 sq.ft.
  INDUS    proportion of non-retail business acres per town
  CHAS     Charles River dummy variable (= 1 if tract bounds river or 0 otherwise)
  NOX      nitric oxides concentration (parts per 10 million)
  RM       average number of rooms per dwelling
  AGE      proportion of owner-occupied units built prior to 1940
  DIS      weighted distances to five Boston employment centres
  RAD      index of accessibility to radial highways
  TAX      full-value property-tax rate per $10,000
  PTRATIO  pupil-teacher ratio by town
  B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
  LSTAT    % lower status of the population
  MEDV     Median value of owner-occupied homes in $1000's
  ;
data house;
infile 'C:\Downloads\housing.data' dlm=' ';
input crim zn indus chas nox rm age dis rad tax ptratio b lstat medv;
run;

* Select relevant variables to Nox as indus chas age dis rad medv
  chas and rad as categorical variables 
  others are continuous variables
; 
* Basic Statistics on continuous variables;
proc univariate data=house;  
	var nox indus age dis medv;
	Histogram nox indus age dis medv;
	probplot nox indus age dis medv;
	ods select Moments BasicMeasures Histogram Probplot;
run;
proc tabulate data=house;
class chas rad;
var nox; 
table  chas rad, nox*(mean std n);
run;

* GLM Stepwise selection; 
proc glmselect data=house;
class chas rad;
model nox= indus chas age dis rad medv/selection =stepwise (select=SL) stats=all;
ods select SelectionSummary;
run;

*Fit the GLM model with selected variables;
proc glm data=house ;
class chas rad;
model nox=indus rad chas age dis medv;
OUTPUT OUT = glm predicted = fit residual = resid rstudent = studentized_resid;
run;
* Plot resid;
proc sgplot data=glm;
	scatter y= resid x=fit ;
run;
* Plot studentized_resid;
proc sgplot data=glm;
	scatter y= studentized_resid x=fit ;
run;

* Gamma model;
proc genmod data=house plots=(cooksd stdreschi stdresdev);
class chas rad;
model nox = indus chas age dis rad medv/ dist=gamma link=log type1 type3;
output out=gamma cooksd=cd;
ods select ModelInfo ModelFit ParameterEstimates Type1 Type3 DiagnosticPlot;	
run;
* Identify if any observations have Cook's D larger than .5;
proc print data=gamma;
where cd>.5;
run;
*indus age dis rad  medv were selected and no influential observations were selected;

*Refit Gamma model with selected variables;
proc genmod data=house plots=(cooksd stdreschi stdresdev);
class rad;
model nox = indus age dis rad medv / dist=g link=log type1 type3;
output out=gamma cooksd=cd;
ods select ModelInfo ModelFit ParameterEstimates Type1 Type3 DiagnosticPlot;
run;
* Identify if any observations have Cook's D larger than .5;
proc print data=gamma;
where cd>.5;
run;
proc print data=gamma;
var nox  indus age dis rad medv cd;
run;
*no influential observations were selected;

* Inverse Gaussian model;
proc genmod data=house plots=(cooksd stdreschi stdresdev);
class chas rad;
model nox = indus chas age dis rad  medv / dist=ig link=log type1 type3;
output out = igres cooksd=cd  ;	
ods select ModelInfo ModelFit ParameterEstimates Type1 Type3 DiagnosticPlot;
run;
* Identify if any observations have Cook's D larger than .5;
proc print data=igres;
where cd>.5;
run;
* indus age dis rad medv are significant and no influential observations were selected;
*Refit Inverse Gaussian model with selected variables;
proc genmod data=house plots=(cooksd stdreschi stdresdev);
class rad;
model nox = indus age dis rad medv/ dist=ig link=log type1 type3;
output out = igres cooksd=cd  ;	
ods select ModelInfo ModelFit ParameterEstimates Type1 Type3 DiagnosticPlot;
run;
* Identify if any observations have Cook's D larger than .5;
proc print data=igres;
where cd>.5;
run;
proc print data=igres;
var nox  indus age dis rad medv cd;
run;
*no influential observations were selected;

*ANOVA on rad;
* Add Welch Adjustment to account for the non-constant variance;
proc anova data=house;       
class rad;       
model nox = rad;
means rad/hovtest tukey cldiff welch;
run;

ods rtf close;

