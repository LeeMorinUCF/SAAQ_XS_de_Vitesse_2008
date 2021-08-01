/*******************************************************************************
 *
 * Stata_FE_Checks_High_Pts.do
 * verifies the calculation of fixed effects regressions
 * with cluster-robust standard errors
 * for the subsample with drivers who have had high demerit point balances.
 *
 * Lee Morin, Ph.D.
 * Assistant Professor
 * Department of Economics
 * College of Business
 * University of Central Florida
 *
 * July 31, 2021
 *
 *******************************************************************************
*/

/*******************************************************************************
 * Prepare workspace
 *******************************************************************************
*/


* Declare log file and close any open log file.
capture log close
log using Stata_FE_Checks_High_Pts.log, replace

* Clear memory.
clear all

/*******************************************************************************
 * Load Data
 *******************************************************************************
*/


* Import dataset.
import delimited using saaq_check_FE_M_high_pts.csv , delimiters(",")
* Dataset has the following variables:
* check_var_names <- c('date_time', 'xtseq', 'sex', 'age_grp', 'curr_pts_grp',
*                      'num', 'policy_int', 'events_int')


* Inspect data.

* Integers.
summarize(xtseq)
summarize(num)

* Binary variables (as integers).
summarize(events_int)
tabulate(events_int)
summarize(policy_int)
tabulate(policy_int)

* Categorical variables.
summarize(curr_pts_grp)
tabulate(curr_pts_grp)
summarize(age_grp)
tabulate(age_grp)
summarize(sex)
tabulate(sex)




/*******************************************************************************
 * Set variables for panel data
 *******************************************************************************
*/

* Date is in a string format.
* Must be converted to a date format.
* Days do not work with multiple events per driver day.
* generate xtdate=date(date_time,"YMD")
* generate xtdate=date_time
* For whatever reason, Stata does not recognize the 12-hour format.
* generate xtdate=clock(date_time, "DMY hm AM")
* format xtdate %tCDDmonCCYY_HH:MM_AM
* Try again with date and time up to the second.
generate double xtdate=clock(date_time, "DMY hms")
format xtdate %tCDDmonCCYY_HH:MM:SS


* Declare time and id variables.
xtset xtseq xtdate


/*******************************************************************************
 * Fixed effect regression
 *******************************************************************************
*/


* Run a simple fixed effect regression.
xtreg events_int policy_int [fweight=num], fe

* After all this trouble, Stata responds with this:
* weight must be constant within xtseq

* So, Stata cannot handle panel data with different frequency weights
* across observations within individual driver IDs.

* However, that is the reality in the dataset, a driver has multiple days with
* one observation (e.g. no ticket) and a different number of days with tickets.


* The only option seems to be to expand the dataset into
* individual observations with weight one, by driver day.
* This would result in an enormous dataset, on the order of hundreds of gigabytes,
* which would involve a prohibitively high cost in terms of
* computing time and memory resources.

* That is one more reason that open-source software is superior.



/*******************************************************************************
 * GAME OVER!
 *******************************************************************************
*/


* The following scripts are a sketch and have not been tested.
* Stata is unable to estimate these models with the frequency weights in the dataset.



* Now introduce a categorical variable.
xtreg events_int policy_int curr_pts_grp [fweight=num], fe


* Try the full model.
* xtreg events_int policy_int c.curr_pts_grp c.policy_int#c.curr_pts_grp [fweight=num], fe


/*******************************************************************************
 * Fixed effect regression with cluster-robust standard errors
 *******************************************************************************
*/

* Run with cluster-robust standard errors, clustered on the driver ID.
xtreg events_int policy_int c.curr_pts_grp c.policy_int#c.curr_pts_grp [fweight=num], fe vce(cluster cvar)





/*******************************************************************************
 * End
 *******************************************************************************
*/

* Closing arguments.
log close

/*******************************************************************************
 * end
 *******************************************************************************
*/
