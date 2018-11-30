# CHIVES
Contains all of the data prep and analysis code for manuscript XX.

Description of files:
1. Dataprep_v1.R extracts all of the nutrition data from the secure server.  Dataprep_12m.R adds the 12 month data to the extraction.
2. FV_outcome.R calculates the primary outcome, servings of fruit and vegetables.  FV_outcome_12m.R adds the 12 month primary outcome data.
3. Indices.R calculates the composite nutrition quality indices, the health eating index (HEI) and the alternative healthy eating index (AHEI) in the CHIVES data.  Indices_12m.R adds the 12 month HEI and AHEI data.  These files both call NDSR_hei.R and NDSR_ahei.R, which contain the algorithms for calculating HEI and AHEI, respectively, from NDSR data.
4. Analysis_v2.R compiles all of the derived variables from steps 1-3 above into the tables and figures that make it into the manuscript.  Analysis_12m.R adds the analogous table for the 12 month derived variables.  

Please contact Joe Rigdon at jrigdon@stanford.edu with any questions.  
