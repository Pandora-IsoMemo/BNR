#### Parameter Learning - Fitting Constraints

***

List of the fitting constraints. These constraints are taken into account while solving the associated minimization problem during parameter learning, i.e. minimization of the sum of squared errors.  

Constraints are checked against known input variables and a *RESPONSE* placeholder, which is replaced during the optimization step of each variable separately.

For example, the following constraint:

    RESPONSE >= 0

requires to solve the problem taking into account positive response only.
