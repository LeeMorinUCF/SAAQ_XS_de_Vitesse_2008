/*******************************************************************************
 *
 * Stata_FE_CRVE_sim_test.do
 * verifies the calculation of fixed effects regressions
 * with cluster-robust standard errors
 * for a simulated sample of drivers.
 *
 * Lee Morin, Ph.D.
 * Assistant Professor
 * Department of Economics
 * College of Business
 * University of Central Florida
 *
 * August 1, 2021
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


/*******************************************************************************
 * Inspect data
 *******************************************************************************
*/

* Determine variable types.
* For loops look so rudimentary in Stata:
foreach var of varlist _all {
  display " `var' "  _col(20) "`: type `var''"
}


* Integers.
summarize(seq)
* num not needed, since data are not frequency-weighted.
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
* Sex variable not included, since invariant to fixed effects.
* summarize(sex)
* tabulate(sex)

* Generate new categorical variables because...Stata, that's why.
* generate curr_pts_grp_cat = byte(curr_pts_grp)
encode curr_pts_grp, generate(curr_pts_grp_cat)
* generate age_grp_cat = byte(age_grp)
encode age_grp, generate(age_grp_cat)


tabulate(curr_pts_grp_cat)
tabulate(age_grp_cat)
* This stupidly generates a numerical variable that ignores the meaning
* of the categories.

* Instead, generate dummy variables.
tabulate curr_pts_grp, gen(curr_pts_grp_cat_)
tabulate age_grp, gen(age_grp_cat_)




* Verify new variable types.
* For loops look so rudimentary in Stata:
foreach var of varlist _all {
  display " `var' "  _col(20) "`: type `var''"
}


tabulate(curr_pts_grp_cat_1)
tabulate(age_grp_cat_2)


/*******************************************************************************
 * Set variables for panel data
 *******************************************************************************
*/

* Date is in a string format.
* Must be converted to a date format.
* Days do not work with multiple events per driver day.
* generate xtdate = date(date_time,"YMD")
* generate xtdate = date_time
* For whatever reason, Stata does not recognize the 12-hour format.
* generate xtdate = clock(date_time, "DMY hm AM")
* format xtdate %tCDDmonCCYY_HH:MM_AM
* Try again with date and time up to the second.
* generate double xtdate = clock(date_time, "DMY hms")
* format xtdate %tCDDmonCCYY_HH:MM:SS
* Simpler version for tests with simulated data.
generate xtdate = date(date_stata,"DMY")
format xtdate %tCDDmonCCYY
* This is still more complicated than it should be.



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
* xtreg events_int policy_int curr_pts_grp, fe
* This is an onerous procedure, yes, I know there is an xi trick,
* but, maybe, a trick shouldn't be necessary for this. Ya think?
xtreg events_int policy_int ///
  curr_pts_grp_cat_2 ///
  curr_pts_grp_cat_3 ///
  curr_pts_grp_cat_4 ///
  curr_pts_grp_cat_5 ///
  curr_pts_grp_cat_6 ///
  curr_pts_grp_cat_7 ///
  curr_pts_grp_cat_8 ///
  curr_pts_grp_cat_9 ///
  curr_pts_grp_cat_10 ///
  curr_pts_grp_cat_11 ///
  curr_pts_grp_cat_12 ///
  curr_pts_grp_cat_13 ///
  curr_pts_grp_cat_14, fe

* Try again with a less cumbersome notation.
xtreg events_int policy_int i.curr_pts_grp_cat, fe



* Try the full model.
* xtreg events_int policy_int c.curr_pts_grp c.policy_int#c.curr_pts_grp, fe
xtreg events_int i.curr_pts_grp_cat##policy_int, fe


/*******************************************************************************
 * Fixed effect regression with cluster-robust standard errors
 *******************************************************************************
*/

* Run with cluster-robust standard errors, clustered on the driver ID.
xtreg events_int i.curr_pts_grp_cat##policy_int, fe vce(cluster seq)





/*******************************************************************************
 * Closing arguments
 *******************************************************************************
*/

* Closing arguments.
log close

/*******************************************************************************
 * End Stata_FE_CRVE_sim_test.do
 *******************************************************************************
*/
