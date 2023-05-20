# DAG-ISR-Transportability
R code for replicating Table A5 in Appendix 2 results (generating simulation data based on the DAGs of Figures 4 and 5), in "Transporting Causal Effects Across Populations Using Structural Causal Modeling: An Illustration to Work-From-Home Productivity" by Park, Tafti & Shmueli, available at https://ssrn.com/abstract=3968187

# Introduction to Repository

The data and codes are provided for readers who are interested in the replication of tables in our paper. This replication archive includes the datasets necessary to obtain the results in the main analysis. 'Experimental Data' contains information about our source population, the call-center employees of a major travel agency in China. It is created based on a set of datasets provided by Bloom et al. (2015). We describe how our dataset is generated in the following section. For reference, the original datasets are available on Nicholas Bloom's website (https://nbloom.people.stanford.edu/research). The other data files, 'Observational Data_Main' and 'Observational Data_Alternative,' are observational data on our target population. These two files are constructed based on the American Time Use Survey (ATUS) conducted by the US Bureau of Labor Statistics, and the detailed process by which the datasets are created is described in our paper.  

The replication archive also contains the codes that replicate the results in the simulations in our appendix. The code is self-contained and includes its own simple data simulation, so we do not provide a dataset for it.   

## 1. A process for creating the 'Experimental Data.'
The "Experimental Data" file is created based on two original datasets from Bloom et al. (2015): 
1) performance_during_exper.dta provides information about the results of the experiment, such as treatment status, different measures of employees' performance, some of which are extracted for our analysis: subject id, experimental period (week), treatment status, performance measure 
2) summary_volunteer.dta provides all subjects' demographic information, such as age, gender, and education level. 
By combining the two files, we constructed the dataset that contains both experiment results and subjects' characteristics. 

Lastly, we additionally created a new variable for whether the subject is a volunteer or not, based on Bloom et al. (2015)'s description: 
"The second group was the 190 employees in the Shanghai call center who did not volunteer to participate in the WFH experiment but met the eligibility requirements to work from home."
As a result, our final dataset includes information not only about whether the subject is in the treatment or control condition but also about whether he/she is a volunteer or not. 
