# Bold Vision Youth Thriving Survey 2025

<img src="https://www.boldvisionla.org/wp-content/uploads/2022/09/Bold-Vision-Light-Logo.png" alt="Bold Vision Logo">

<br>

<details>
  <summary>Table of Contents</summary>
  <ol>
    <li> <a href="#about-bold-vision">About Bold Vision</a></li>
    <li> <a href="#about-the-bold-vision-youth-thriving-survey">About the Bold Vision Youth Thriving Survey</a></li>
    <li> <a href="#acknowledgement-and-partners">Acknowledgement and Partners</a>
    <li> <a href="#about-the-data">About the Data</a>
    <li> <a href="#about-the-repository">About the Repository</a>  
    <li> <a href="#getting-started">Getting Started</a>
    <li> <a href="#contributors">Contributors</a>
    <li> <a href="#contact-us">Contact Us</a>
    <li> <a href="#citation">Citation </a> 
    <li> <a href="#about-catalyst-california">About Catalyst California</a> 
    <li> <a href="#license">License </a> 
  
  </ol>
</details>

# About Bold Vision

[Bold Vision](https://www.boldvisionla.org/) is a multi-sector, 10-year-plus initiative that aims to fundamentally improve the lives of BIPOC children and youth, creating lasting change in our communities by establishing new paths towards success for young people across Los Angeles County. Thanks to decades of power-building, Los Angeles is reaching a tipping point. This tipping point is an opportunity for youth of color and allies to eliminate existing inequities and transform public systems into ones of support, not suppression. The task before us is to follow the lead of youth of color, and commit to the investments, advocacy, and power-building needed to finally uproot inequities from our public systems. 

Data is a vital tool to measure how youth of color are thriving, or lacking the resources and guidance necessary to reaching their fullest potential. Bold Vision released its first data-driven report measuring how well youth are thriving across Los Angeles in 2022 and a follow up midterm report in 2024 with updated indicators. This prior reports can be viewed at the bottom of this webpage [here](https://www.boldvisionla.org/issue/).

# About the Bold Vision Youth Thriving Survey

The Bold Vision Youth Thriving Survey analyzed responses from over 3,000 youth in LA County ages 15-24. The survey is the first of its kind survey that provides policymakers with insights on the factors influencing the ability of L.A. County youth to thrive—from their own perspectives. The survey aims to uplift the voices of young people and provides a better understanding of the factors influencing their lives—directly from their perspectives. Through collaboration with more than 40 youth-serving community-based organizations (CBOs), the survey reached a diverse and large sample of young people across L.A. County. The survey offers invaluable insight into what youth need to thrive, and where change is most urgently needed. Read the report on the first analysis and findings from the survey data [here](INSERT LINK). 

# Acknowledgement and Partners

We thank and acknowledge everyone who played a role in developing and implementing the BVYTS. [Catalyst California](https://www.catalystcalifornia.org/) has served as the lead community engagement, policy development, and research consultant for the initiative alongside the former Bold Vision Community Council, Bold Vision Youth Council, Bold Vision Steering Committee, Social Justice Learning Institute staff, and members of Bold Vision’s Survey Advisory Group. Feedback from these stakeholders was imperative to Catalyst California creating, conducting, and analyzing BVYTS responses. This work would not have been possible without deep collaboration from our research partners and the community-based organizations that collected surveys. We would like to thank Imoyase Community Support Services and the Community Health Equity Group for their contributions to designing and implementing the Bold Vision Youth Thriving Survey. Cheryl Grills, Ph.D., Sandra Villanueva, Ph.D., Elia De La Cruz Toledo He, Ph.D., Peter Rej, Ph.D., Diane Terry, Ph.D., and other staff at Imoyase Community Support Services conducted the literature review for the survey and led the design of the survey questionnaire and sampling plan. Jason Douglas, Ph.D. (University of California Irvine), Andy Subica, Ph.D. (UC Riverside), Aerika Loyd, Ph.D. (UC Riverside), and UC Irvine (UCI) graduate researchers led the data collection and received Institutional Review Board approval for the survey implementation from UCI. We would also like to thank the 40 youth-serving organizations who distributed the BVYTS and helped this survey represent a broad cross-section of L.A. County youth.  

<p align="right">(<a href="#top">back to top</a>)</p>

# About the Data 

## Indicators Overview
We defined ten components to youth thriving based on our literature review, input from the Survey Advisory Group (SAG), and feedback from youth engagement. They are intended to complement the existing Bold Vision framework and provide a detailed definition of Bold Vision’s key goal – BIPOC youth thriving. The first eight components are from the Ettinger THRIVE framework. We added two more components to encompass a more holistic picture of youth thriving.   

We developed the BVYTS based on these 10 components but prioritized specific items to keep the survey manageable for youth completion. We selected components and questions based on SAG feedback, youth group interviews, ICSS expertise, Bold Vision's priorities, and data gaps not covered in Bold Vision's Midterm report.  

| Components                         | Description |   
| ----------------------------- | -------|
| Strong Minds and Bodies | Youth having positive mental and physical health, which includes but is not limited to positive emotions, health behaviors, and cognitive development. |
| Positive Identity and Self-Worth | Youth having a strong sense of self, purpose, self-worth, and hope for the future. |  
| Caring Families and Relationships | The relational aspects of youth thriving, including friendships, caregiver relationships, and school relationships that provide caring, stable, and positive support. |
| Vibrant Communities | The local resources available to youth, such as open public spaces, safe spaces, and accessible public transportation. | 
| Racial Justice, Equity, and Inclusion | Youth feeling comfortable, accepted, and included in all spaces they enter, regardless of race/ethnicity, gender, sexuality, religion, health status, and appearance, including whether youth interactions with public systems are fair and equitable. | 
| Fun and Happiness | Opportunities for youth to experience enjoyment and happiness. | 
| Healthy Environments | Healthy, accessible physical and social environments, such as clean air and water, quality medical services, mental health resources, social services, and healthy food. |   
| Safety | Youth feeling safe in their neighborhoods, at school, and in their communities, such as physically safe spaces, secure relationships (not fearing bullying or violence), and protected development (free to be children). | 
| Cultural and Ethnic Identity | Youth connecting to their cultural or ethnic identity, which includes healing practices, ethnic pride, and family traditions. |  
| Spirituality | Youth having transcendental awareness or spiritual connection, including the pursuit of higher meaning and purpose in life. | 

## Methodolody 
To learn more about the detailed methodology of our Bold Visiong Youth Thirving Survey data collection and analysis, please see the methodology documentation [here](INSERT LINK).

# About the Repository 

In this repository, you will find five folders corresponding to different aspects for the Bold Vision Midterm Report. 
* Data Prep and Quality | Scripts that were developed to analyze the raw survey data and prep it for anlayis.
* Descriptive and Demographic Analysis | Scripts that run calculations for analysis focused on demographic and descriptive characteristisc of respondents. 
* Factor Analysis and Correlations | Scripts that run correlational and factor analysis on the survey data. 
* Visuals | Includes all visuals included in the Bold Vision Youth Thriving Survey Report. Subfolders are organized by components.
* _Any files not in a folder are central to the overall project. The bv_barchart_function scripts are sourced in numerous other scripts to produce harmonious, stylized charts for the report._

  
# Getting Started

## Prerequisites

We completed the data cleaning, analysis, and visualization using the following software. 
* [R](https://cran.rstudio.com/)
* [RStudio](https://posit.co/download/rstudio-desktop)

We used several R packages to analyze data and perform different functions, including the following.
* dplyr
* sf
* tidyr
* usethis
* RPostgreSQL
* readxl
* stringr
* rpostgis
* ggplot


```
list.of.packages <- c("usethis","dplyr","data.table", "sf", tidyr","RPostgreSQL","readxl","stringr","sf", "ggplot2", "flextable", "ggchicklet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

devtools::install_github("r-lib/usethis")

library(usethis)
library(dplyr)
library(sf)
library(tidyr)
library(RPostgreSQL)
library(readxl)
library(stringr)
library(sf)
library(ggplot2)
library(flextable)
library(ggchicklet)

```

<p align="right">(<a href="#top">back to top</a>)</p>


# Contributors

* [Alexandra Baker](https://github.com/bakeralexan)
* [Hillary Khan](https://github.com/hillaryk-ap)
* [Maria T. Khan](https://github.com/mariatkhan)
* [Elycia Mulholland Graves](https://github.com/elyciamg)
* [Chris Ringewald](https://github.com/cringewald)
* [Alicia Vo](https://github.com/avo)

<p align="right">(<a href="#top">back to top</a>)</p>

# Contact Us

[Elycia Mulholland Graves](https://www.linkedin.com/in/elycia-mulholland-graves-54578258/) - egraves[at]catalystcalifornia.org  <br>

[Maria T. Khan](https://www.linkedin.com/in/mariatkhan/) - mkhan[at]catalystcalifornia.org 

<p align="right">(<a href="#top">back to top</a>)</p>

# Citation
To cite Bold Vision:
Catalyst California; BOLD VISION, boldvisionla.org, 2024.

To cite Bold Vision Youth Thriving Survey Report: 
Catalyst California. [Bold Vision Youth Thriving Survey Report]. Los Angeles, CA: Bold Vision, 2025. [https://www.boldvisionla.org/news-updates/]

To cite Bold Vision Youth Thriving Survey: 
Catalyst California and the Social Justice Learning Institute, Bold Vision Youth Thriving Survey, 2024.

# About Catalyst California

## Our Vision
A world where systems are designed for justice and support equitable access to resources and opportunities for all Californians to thrive.

## Our Mission
[Catalyst California](https://www.catalystcalifornia.org/) advocates for racial justice by building power and transforming public systems. We partner with communities of color, conduct innovative research, develop policies for actionable change, and shift money and power back into our communities. 

[Click here to view Catalyst California's Projects on GitHub](https://github.com/catalystcalifornia)

<p align="right">(<a href="#top">back to top</a>)</p>

# License

Distributed under the General Public Use and Creative Commons Licenses. See `LICENSE.txt` and `CC_LICENSE.md` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>
