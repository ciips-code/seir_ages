# IECS - CIIPS: COVID Vaccines Impact Modeling

This model was developed to assess the epidemiological impact of different vaccination strategies against COVID-19. 
The SEIR model (Susceptible,  Exposed, Infected, Recovered)  has a long tradition in infectious disease epidemiology. 
Kermack and McKendrick initially developed it in 1927 1 as a 
SIR model (Susceptible, Infected, Recovered), which was later expanded with the Exposed (E) compartment that includes a 
latent infection state period.

Our model deploys a compartmental model approach as it considers several compartments to represent the population and its 
different age groups.  It considers flows to represent transitions and progression between different states. This model provides 
a framework in which the number of people in separate compartments (each homogeneous for some specified characteristic) and the 
relationships between those compartments that model population dynamics, can be described in mathematical terms.

This version of the IECS-CIIPS model conyains two main sets of modifications. Firstly, to model the impact of vaccination strategies 
by adding a representation of different immunity states, both related to vaccination and natural immunity. Secondly, to analyze the 
differential impact of NPIs or immunization strategies by incorporating age compartments (and their contact matrices).

## Live model
The live version of this model is available at: https://iecs.shinyapps.io/seir-ages/
