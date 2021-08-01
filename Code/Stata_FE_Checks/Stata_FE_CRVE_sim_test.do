/*******************************************************************************
 *
 * Stata_FE_CRVE_sim_test.do
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
log using Stata_FE_CRVE_sim_test.log, replace

* Clear memory.
clear all

/*******************************************************************************
 * Load Data
 *******************************************************************************
*/


* Import dataset.
import delimited using Stata_FE_CRVE_sim_data.csv , delimiters(",")
* Dataset has the following variables:
* check_var_names <- c('date_stata', 'seq', 'age_grp', 'curr_pts_grp',
*                      'policy_int', 'events_int')


* Inspect data.

* Integers.
summarize(seq)
* summarize(num)

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
* summarize(sex)
* tabulate(sex)




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
* generate double xtdate=clock(date_time, "DMY hms")
* format xtdate %tCDDmonCCYY_HH:MM:SS
* Simpler version for tests with simulated data.
generate xtdate=date(date_stata,"DMY")
format xtdate %tCDDmonCCYY




* Declare time and id variables.
xtset seq xtdate


/*******************************************************************************
 * Fixed effect regression
 *******************************************************************************
*/


* Run a simple fixed effect regression.
* xtreg events_int policy_int [fweight=num], fe
xtreg events_int policy_int, fe


* Now introduce a categorical variable.
xtreg events_int policy_int curr_pts_grp, fe


* Try the full model.
* xtreg events_int policy_int c.curr_pts_grp c.policy_int#c.curr_pts_grp, fe


/*******************************************************************************
 * Fixed effect regression with cluster-robust standard errors
 *******************************************************************************
*/

* Run with cluster-robust standard errors, clustered on the driver ID.
xtreg events_int policy_int c.curr_pts_grp c.policy_int#c.curr_pts_grp, fe vce(cluster cvar)





/*******************************************************************************
 * Closing arguments
 *******************************************************************************
*/

* Closing arguments.
log close

/*******************************************************************************
 * End
 *******************************************************************************
*/
