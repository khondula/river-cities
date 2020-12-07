# Notes on dam data

* Global dam watch list of datasets http://globaldamwatch.org/data/

## NID data

* NID data available in the R package dams (91,457 total records)
* data dictionary with all variables: https://files.hawaii.gov/dbedt/op/gis/data/nid_dams_data_dictionary.htm
* coords are at dam centerline as a single value in decimal degrees, NAD83.
* 18,304 out of 91,457 include flood control as a purpose. 93% of those (17,026) have flood control as primary purpose

* Owner types for flood control w/coordinates:
Local is defined as having taxing authority or is supported by taxes

| Owner type | N |
|------------|---|
| Local      | 10722 |
| Private    | 3444 |
| State      | 2853 |
| Federal    | 976 |
| public utility (U) | 274 |
| not listed (X)  | 35 |

* 10 out of the 18,304 flood control dams dont have coordinates listed
* Most of the flood control dams with coordiantes (16,850 out of 18,294) have an associated city that is most likely to be affected by floods resulting from failure of the dam, but those names don't necessarily correspond to anything in the census

Purposes
Codes indicating the purposes for which the reservoir is used:
* I for Irrigation;
* H for Hydroelectric;
* C for Flood Control and Storm Water Management;
* N for Navigation;
* S for Water Supply;
* R for Recreation;
* P for Fire Protection, Stock, Or Small Farm Pond;
* F for Fish and Wildlife Pond;
* D for Debris Control;
* T for Tailings;
* O for Other.





