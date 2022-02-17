### Bi-fidelity Surrogate Modelling

This repo contains the code used to run and analyse all experiments of "Bi-fidelity Surrogate Modelling: Showcasing the need for new test instances" (2022) by Andres-Thio N, Munoz MA, and Smith-Miles K. 

The code implements Kriging[^1,^2] and Co-Kriging[^3,4] as surrogate models. That is, the implementation
generates a sample of a bi-fidelity function and trains the surrogate model, without the added functionality
of choosing further samples. Multiple literature test instances are implemented here

presents an implementation of Kriging and Co-Kriging as surrogate model
Implementation of the work presented in 






[^1]: Krige DG (1951) "A statistical approach to some basic mine valuation problems on the witwatersrand". Journal of the Southern African Institute of Mining and Metallurgy 52(6):119–139
[^2]: Jones DR (2001) "A taxonomy of global optimization methods based on response surfaces". Journal of global optimization 21(4):345–383
[^3]: Kennedy MC, O’Hagan A (2000) "Predicting the output from a complex computer code when fast approximations are available". Biometrika 87(1):1–13.
[^4]:Forrester AI, S ́obester A, Keane AJ (2007) "Multi-fidelity optimization via surrogate modelling". Proceedings of the royal society a: mathematical, physical and engineering sciences 463(2088):3251–3269.


