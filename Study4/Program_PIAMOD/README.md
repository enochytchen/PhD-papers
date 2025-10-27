## Using this repository
The repository enables the reproduction of analyses from the CML cost study--the prevalence part:
## File Descriptions

| **Filename**                              | **Purpose**                                                                                           |
|-------------------------------------------|-------------------------------------------------------------------------------------------------------|
| `functions` | This folder stores functions defined by Fabrizio Di Mari, which are used subsequently in the analysis.|
|**Prepare data**| |
| `PIAMOD_01_analysisdata_cml.do`           | Define CML incidence data from the Swedish Cancer Registry                                            |
| `PIAMOD_01_analysisdata.R`                | Make analysis data for PIAMOD input, including CML incidence, general population mortality rates, and general population size of Sweden |                                             |
| `PIAMOD_01_popmort2022_proj.do`           | General population mortality rates to 2022 (data from the Human Mortality Database) and projection from 2023 to 2120 (data from the Statistics Sweden) |                                                    |
| `PIAMOD_01_userdef_stdpop.R`              | Define Sweden 2000 population for age-standardisation                                               |
|**Net survival**| |
| `PIAMOD_02_tabRS_strs.do`                   | (Results not used in the analysis, only for reference) Calculate net survival using the Pohar-Perme estimator using strs in Stata |
| `PIAMOD_02a1_NS_pp.R`                       | Calculate net survival using the Pohar-Perme estimator using relsurv in R |
| `PIAMOD_02a2_NS_pp_plot.R`              | Plot output from `PIAMOD_02a_NS_relsurv.R` |                                                         |
| `PIAMOD_02a3_NS_pp_plot_age5564.R`          | Plot output from `PIAMOD_02a_NS_relsurv.R` for age 55-64 years by sex |                                                         |
| `PIAMOD_02a_NS_relsurv.R`                   | Calculate net survival using the Pohar-Perme estimator using relsurv in R | 
| `PIAMOD_02b1_NS_fpm.R`                       | Fit flexible parametric relative models using stpm2 |
| `PIAMOD_02b2_NS_fpm_condage_plot.R`          | Plot FPRSM-based net survival condition on ages by sex |
| `PIAMOD_02b3_NS_fpm_condage_plot_age5564.R`          | Plot FPRSM-based net survival condition for age 55-64 years by sex |
| `PIAMOD_02b6_NS_fpm_pp_vs_condage_plot_age5564.R`    | Compare non-parametric with parametric for age 55-64 years by sex |
| `PIAMOD_02b9_NS_fpm_Bower.do`                | (Results not used in the analysis, only for reference) Fit flexible parametric relative models using the model Bower, JCO, 2016 |  
|**Organise PIAMOD outputs**| |
| `PIAMOD_03_piamod_constantRS_dynINC.R` | Organise PIAMOD's output (Scenario 1: constant relative survival and dynamic incidence rates) |
| `PIAMOD_03_piamod_dynRS_dynINC.R`      | Organise PIAMOD's output (Scenario 2: dynamic relative survival and dynamic incidence rates) |
|**Present PIAMOD outputs**| |
|***Table 1***| |
| `PIAMOD_03a_tab1.R` | Organise four scenarios above for Table 1, including incidence and prevalence |
|***Incidence***| |
| `PIAMOD_04a1_inc_apc_plot.R`| Plot PIAMOD's output for incidence by age group and sex |
| `PIAMOD_04a2_inc_obs_vs_apc_plot.R` | Plot PIAMOD's output to check observed versus model-based estimates for incidence by age group and sex|
| `PIAMOD_04a3_inc_obs_vs_apc_plot_age5564.R` | Plot PIAMOD's output to check observed versus model-based estimates for incidence for age 55-64 years and sex|
|***Prevalence***| |
| `PIAMOD_05a1_prev_CIs.R` | stimate confidence intervals for prevalence cases and rates |
| `PIAMOD_05a2_prev_plot_scenario.R`| Plot PIAMOD's output for prevalence by scenario 1 versus 2|

## License
Copyright © 2025. All rights reserved.<br>
![License](https://img.shields.io/badge/license-Apache%202.0-blue)
![R Version](https://img.shields.io/badge/R-%3E%3D4.4.1-blue)
<br>
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements.  See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License.  You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.