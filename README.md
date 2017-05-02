# SimNashPrice

Tools for working with models of demand based on discrete choice/random utility models with logit errors: currently I support simple logit and random coefficient logit.  

The main tool is an implementation of Morrow and Skerlos (2010)'s "zeta fixed point" which is a fast fixed point method for computing nash equilibrium prices. Eventually I plan to add other methods for computing these prices, but I am finding the "zeta fixed point" to be so fast and reliable it should probably be used exclusively.  

I organize information about specific markets (in practice often a time-location observation) in 'Logit_Demand_Market' objects or 'rLogit_Demand_Market' objects.  These objects contain all of the information needed to compute: prices, shares, profits, markups and other conduct variables of interest. 

Using an object oriented approach unifies the representation of the markets and will make it easy to build extensions without having to think about how to organize new information.  It also avoids relying on the user specifying important parameters in the computational enviroment--they just need to call the constructor. 

I use the R6 package as the OOP system. This allows for traditional OOP pass by reference, so that methods never copy on modify which allows for faster computation.  R6 allows for private attributes--which encapsulates the state of each market, preventing shared/mutable state errors. Users concerned about replicability may create clones of the initial object state. R6 is faster and has less memory overhead the the RC system.

Eventually, I will add nested logit demand market classes to the package.

There is also a simulation tool. The user specifies market primitives and functions for drawing from Berry (1994) type structural errors, and is returned the firms expected variable profits (average over the simulation draws). 

	
