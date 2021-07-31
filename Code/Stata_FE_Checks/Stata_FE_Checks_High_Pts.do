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


* Declare log file and close any open log file.
capture log close
log using Stata_FE_Checks_High_Pts.log, replace

* Clear memory.
clear all

* Import dataset.
import delimited using saaq_check_FE_M_high_pts.csv , delimiters(",")
* Dataset has the following variables:
* check_var_names <- c('date', 'xtseq', 'sex', 'age_grp', 'curr_pts_grp',
*                      'num', 'policy_int', 'events_int')


* Inspect data.
* If necessary.

* Date is in a string format.
* Must be converted to a date format.
generate xtdate=date(date,"YMD")


* Declare time and id variables.
xtset xtseq xtdate


* Run fixed effect regression.
xtreg events_int policy_int c.curr_pts_grp c.policy_int#c.curr_pts_grp [fweight=num], fe


* Run with cluster-robust standard errors, clustered on the driver ID.
xtreg events_int policy_int c.curr_pts_grp c.policy_int#c.curr_pts_grp [fweight=num], fe vce(cluster cvar)







* Closing arguments.
log close

/*******************************************************************************
 * end
 *******************************************************************************
*/
