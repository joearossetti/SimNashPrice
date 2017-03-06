# SimNashPrice

Tools for working with models of demand based on discrete choice/random utility models with logit errors.  

The main tool is an implementation of Morrow and Skerlos (2010)'s "zeta fixed point" which is a fast fixed point method for computing nash equilibrium prices. Eventually I plan to add other methods for computing these prices, but I am finding the "zeta fixed point" to be so fast and reliable it should probably be used exclusively.  

I organize information about a specific market (in practice a time-location pair) in 'Logit_Demand_Market' objects.  These objects contain all of the information needed to compute, prices, shares, and other conduct variable of interest. Using an object oriented approach unifies the representation of the market objects and makes it easy to build extensions without having to think about how to organize new information.  It also avoids relying on the user specifying important parameters in the computational enviroment. 

There is also a simulation tool which is a simple wrapper around a for loop. The user specifies market primitives and a function for drawing from Berry (1994) type structural errors, and is is returned a list of market instances from wich expected prices, shares and other conduct parameters can be computed.   

	
