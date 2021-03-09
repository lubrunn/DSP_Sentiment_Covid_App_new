The default option automatically chooses the autoregressive structure
and moving average period for each variable.

The number of autoregressive variables is based on the function
`vars::VARselect`. This function returns the number of lags with the
lowest information criterion. (Each variable is evaluated in relation to
the dependent variable) <br> The period for the moving average is set to
5. This is deducted from the assumption forecasts are computed for a
short horizon.
