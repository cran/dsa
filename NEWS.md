dsa v1.00 (Release date: 2021-06-30)
==============
Breaking changes:
	- Harmonisation of the names of the functions. Now mostly snake case, e.g. makeDummy() is changed to make_dummy().
	- Harmonisation of the names of the options. Now mostly snake case.
	- Some minor functions where moved to internal functions, and can only be accessed via dsa:::.function.name, e.g. dsa:::.drop31. Full list: .drop31, .fill31, .fill_up, .Add, .day_split, .df2HTML.
	- dow, dom and day_of_month functions are not included in the package anymore.
	- The option "Diff" has been removed from dsa(). If the differenced series shall be adjusted, the differencing needs to occur outside of dsa().
	- The returned object by dsa() contains sets of xts time series. A few of these have been renamed, especially those contained in sa_result and sa_result2.

Major changes:
	- vignette: A full vignette has been written with examples from Ollech (2021).
	- dsa(): Optimised the (optional) mean-correction of the seasonal and calendar components. The options s.window1, s.window2 and s.window3 can now be set to NULL so that the respective step of the dsa procedure is skipped.
	- outlier(): Improved outlier detection, fixed bug wrt the estimated t-value.
	- output(): Added seasonality tests, added plot of monthly aggregates, added acf and pacf plots for regression model, improved description.
	- Inclusion of examplary data.
	- Inclusion of holiday regressors.

Further changes:
	- print(): Defined the print behaviour for objects of class daily.
	- multi_xts2ts(): New function for multiple conversion of time series of class xts to ts. Helpful for working with calendar regressors.
	- del_names(): New function for working with xts in the context of calendar regressors. It is helpful, because the default behaviour of the xts names can lead to smaller problems when we are combining several xts time series.
	- dsa(): Time zones of the outputted objects are now fixed by appyling as.Date() when necessary. 
	- dsa(): Now, dsa() correctly handles series that start with one or multiple NA
	- Using a model_span interferred with the mean_correction. This has been changed.
	- The package no longer depends on package extrafont.



dsa v0.76 (Release date: 2020-07-01)
==============
Bug fixes:
	- In output: The outlierplot was misleading. Now the major gridlines instead of minor gridlines are drawn to make sure that the dates on the x-axis are helpful.

Changes:
	- Added options to plot_spectrum

dsa v0.75 (Release date: 2020-04-29)
==============
Changes:
	- Improved outlier detection and adjustment. Set cval higher, if too many outliers are found!
	- s.window2 can now be set to NULL
	- Warning due to regressors / the forecast package now avoided


dsa v0.73.5 (Release date: 2020-03-05)
==============
Changes:
	- Bug fix: Now multiple external regressors can be added correctly
	- fixed bug in xts2ts for leapyears

dsa v073.3 (Release date: 2020-01-20)
==============
Changes: 
	- dow_dummy now has an option delete 29_2 to make it possible to delete the values for the 29th of February
	- Improved treatment of 29th February


dsa v0.72.1 (Release date: 2019-11-01)
==============
Changes: 
	- In drop31, more iterations are allowed, to find the correct length of the dates for the new variables



dsa v0.71.6 (Release date: 2019-10-10)
==============
Changes: 
	- In output: for the graphics cairo-png is no longer used, as on some devices it does not create graphics


dsa v0.70.1 (Release date: 2019-04-04)
==============
Changes: 
	- Bug fixes:  - Some minor code clean-up

Additional Functionality:
	- get_sa, get_trend, get_original: Inclusion of helper function to quickly get the most important series from the seasonal adjustment




dsa v0.64.1 (Release date: 2019-03-26)
==============
Changes: 
		- output(): Now contains additional visualizations of the outlier adjustment


dsa v0.63.3 (Release date: 2019-02-26)
==============
Changes: 
- Bug fixes  
	- Multiple regressors were sometimes not considered correctly
	- In output(): Last graphic of spectrum previously showed original instead of seasonally adjusted series


dsa v0.62.1 (Release date: 2019-02-10)
==============
Changes: 
- Bug fixes (thanks to Fernando Kawaoka and Rodrigo da Silva)
	- outlier estimation in case of too low critical value was not processed correctly
        - Creation of dummy variables mixed up from and to values
	- improved the fill_up function to correctly identify the 29th of February even if there is some mismatch of series length



dsa v0.60.35 (Release date: 2019-01-04)
==============
Changes: 
- Bug fixes
	- descaler: set y=NA as a default. Otherwise when descaler is only used for log-transformations, as then y does not need to be supplied, it lead to an error




dsa v0.60.34 (Release date: 2019-01-04)
==============
Changes: 
- Bug fixes
	- drop31: fixed an error that could lead to faulty transformation
	
	

dsa v0.60.33 (Release date: 2019-01-04)
==============
Changes: 
- Bug fixes
	- fixed the fill_up function to correctly identify the 29th of February



dsa v0.60.32 (Release date: 2019-01-04)
==============

Changes: 
- Changes
	- Minor changes of examples to decrease run time


dsa v0.60.31 (Release date: 2018-10-29)
==============

Changes: 
- Bug fixes
	- previously, setting option reiterate3 > 0 in the dsa function did not work together with diff > 0
	- if scaling the data became active in the dsa function, the data where not propably rescaled at the end
	- fixed display of SI ratios for annual period


dsa v0.60.21 (Release date: 2018-10-19)
==============

Changes:

* First public release of the dsa package