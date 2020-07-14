# decarceration
Analysis of longitudinal census and Uniform Crime Report data in Texas to assess the effect of a reform prosecutor on crime, using differences-in-differences analysis

This analysis aims to examine the impact of elected a reform-minded DA, Craig Watkins, in Dallas County, TX, on violent crime. 
Watkins is one of the first "reform-minded" DAs elected in the US, which translates a larger dataset to work with. In the case of my analysis, was matching and differences-in-differences, this enabled a longer post period. 
Findings: After Watkin's election in 2014, Dallas experienced a notable decline in violent crime on its own, but this decline was particularly significant compared to the violent crime rate within the control group
There are a few caveats to this analysis. The influence of DAs lies mostly in the judicial branch and I was not able to identify specific policies or procedures that would plausibly reduce serious crime. This hinders the internal validity of this analysis. Additionally, differences-in-differences analysis relies to the "parallel trends" assumption--ie, that prior to the intervention, the treated and control group's trend lines move in tandem over time, an indication that both are similar enough to be compared in the post period. The final graph shows a roughly parallel trend that begins to narrow in the early 2000s, implying that some differences begin to emerge during that time. Still, Dallas' decline in crime is stark enough that I think the findings are worthwhile investigating further. 

1. Background
- Reform Prosecution.pptx provides a brief background on the issue and a graph of the final analysis

2. Raw Data
- usa_00004.RData: IPUMS census data for all PUMAs within TX, 2006 (right before Watkins was elected)
- PUMA_Crosswalk.csv: this was used to link Census 2000 PUMAs (in my data) to Census 2010 PUMAs (which I had the geographical mapping for. This was the only way for me to double-check the location of the Census 2000 PUMAs and group them into police departments (typically at a city level), which is the ultimate unit of analysis)
- TX_UCR.xlsx: excel file that I used to manually match the control-level PUMAs to police departments
- TX_UCR.csv: CSV version of TX_UCR, to plot the treated and control groups against each other

3. Code
- cleaning_matching.R: used usa_0004.RData to generate control PUMAs
- plot_diff_in_diff.R: used TX_UCR.csv to plot treated and control groups against each other for differences-in-differences analysis

4. Other assorted .png and .csv files
- Output data, including jitter graphs and histograms for respective matching algorithm results, and lists of the generated control groups and their associated statistics
