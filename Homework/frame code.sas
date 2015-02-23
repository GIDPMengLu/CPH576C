proc contents data=tmp1.frmgham;
run;

proc univariate data=tmp1.frmgham;
var CVD;
histogram;
run;

/* risk factors:

sex period time age sysbp diabp bpmeds cursmoke cigpday totchol hdlc ldlc
bmi glucose diabetes heartrte prevap prevchd prevmi prevstrk prevhyp

*/

proc freq data = tmp1.frmgham;
table angina*period hospmi*period mi_fchd*period anychd*period stroke*period cvd*period hyperten*period / norow nocol nopercent;
run;

proc freq data = frame;
table period*prevap period*prevmi / nocol norow nopercent;
run;

/**********************************************************/
/* data cleaning for time period 1 */

data frame1;
set tmp1.frmgham;

/*cleaning sex */
if Sex = 2 then sex = 0;

/* only looking at period 0 */
if period = 2 or period = 3 then delete;

/*getting rid of all observations that already came in w/ angina or MID */
if prevap = 1 then delete;
if prevmi = 1 then delete;

bad_heart = 0;
if angina = 1 or hospmi = 1 then bad_heart = 1;

keep bad_heart sex age sysbp diabp bpmeds cursmoke cigpday  totchol hdlc ldlc
bmi glucose diabetes heartrte prevchd prevstrk prevhyp; 

run;

/**********************************************************/

proc corr data = frame1;
var bad_heart sex age sysbp diabp bpmeds cursmoke cigpday  totchol
bmi glucose diabetes heartrte prevchd prevstrk prevhyp;
run;


proc logistic descending data=frame1;
model bad_heart = sex age sysbp diabp bpmeds cigpday totchol 
bmi glucose heartrte prevchd prevstrk prevhyp;
run;

proc glimmix data=frame;
model angina = sex age sysbp diabp bpmeds cigpday totchol 
bmi glucose heartrte prevchd prevstrk prevhyp;
random randid;
run;







