# Effective Tax Rates and Firm Size 
## Pierre Bachas, Anne Brockmeyer, Roel Dom and Camille Semelet.

### Abstract (draft coming soon):
This paper provides novel evidence on the relationship between firm size and effective corporate tax rates (ETRs) using full-population administrative tax data from 13 countries. In all countries, small firms face lower ETRs than mid-sized firms due to reduced statutory tax rates and a higher propensity to register losses. In most countries, ETRs fall for the largest firms due to the take-up of tax incentives. As a result, a third of the top 1% of firms face ETRs below the global minimum tax of 15%. The minimum tax could raise corporate tax revenue by 27% in the median sample country.    

### Replication codes:
The study uses confidential administrative tax data that cannot be shared publicly. 
However, we provide the replications codes of the study so that the analysis can be run on another country based on Corporate Income Tax returns by tax administrations or other researchers with access to such data. 
Further, the analysis can still be run on data for Mexico, which has been released as OpenSource data. 

#### Descriptions of the files:
- OpenSourceData contains the raw data for Mexico
- The data is cleaned using the do file MEX_cleaning_CIT.do
- Once the data is cleaned, use the 0_master_one_country.R Script. This script will successively call the other 1_, 2_ and 3_ scripts and produce the output as in the paper. 

#### Questions? <a href="mailto:semelet@ifo.de">Contact here</a>
