# Roadmap

The current roadmap for pedant include three main stages:

 - Creating a basic language that can do dimensional analysis over calculations to check for missing assumptions
 - Create a language that can do Monte Carlo experiments over these calculations to account for uncertainty
 - Create a web interface to this language to make the calculations more accessible.

The details of these stages are outlined below, as well as thoughts of possible
developments in these stages.

## Stage 1, Basic Language

The duration of this phase is the 25th of November to the 25th of December.

This phase is designed to get down a basic workable language that can check for
dimensional errors within calculations. The features that are included within this
stage are:

- Dimensional checker
- Adding records and lists
- Functions with type inference
- Modules
- A basic language server to check for error quickly
- A testing suite

Things that would be nice but we might not get time for include:

- Representing all of GiveWell's work within Pedant

## Stage 2, Uncertainty

The duration of this phase is from the 25th of December to the 25th of January.

This phase is designed to allow creating distributions of possible input parameters,
and therefore determine how uncertain the results of calculations are. The features
that are to be included in this stage are:

- Writing in distributions instead of numbers for calculations, possibly in a syntax
  similar to [Squiggle](https://squiggle-language.com/).
- Checking uncertainty of the result of calculations

Things that would be nice to have, but we might not get time for include:

- Doing sensitivity analysis over parameters, to determine which parameters need
  to be made more certain.
- Doing calculations over distributions, such as value of information, which could
  be valuable in evaluating research.


## Stage 3, Web interface

The duration of this phase is from the 25th of January to the 25th of February.

This phase is designed to make it easier to access cost effectiveness analysis
by providing a web interface. What is to be included in this phase is:

- Reading pedant files and presenting them in a web interface, like a column
  of excel sheets
- Reading doc comments in pedant files to add to the web interface.
- Including sliders to change around the values of different parameters to calculate
  the final result.

Some things that would be nice to have, but we might not get time for include:

- Being able to write pedant files in a browser.
