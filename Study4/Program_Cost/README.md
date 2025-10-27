## Using this repository
The repository enables the reproduction of analyses from the CML cost study--the cost part:
## File Descriptions

| **Filename**                                | **Purpose** |
| ------------------------------------------- | ----------- |
| **Prepare study population**                |             |
| `Cost_00_cancer_dx_BPCML.R`                 | Define diagnosed at blastic phase from the Swedish Cancer Register |
| `Cost_00_cancer_dx_CML.R`                   | Define diagnosed with CML from the Swedish Cancer Register |
| `Cost_00_cancer_dx.R`                       | Define diagnosed with CML from the Swedish CML register |
| `Cost_00_death.R`                           | Data on date of death |
| `Cost_00_phase_at_dx_plot.R`                | Plot the distribution of diagnosis phases |
| `Cost_00_phase_at_dx.R`                     | Define diagnosis phase: CP, AP, or BP |
| `Cost_00_popmort2022_proj.do`               | Duplicate popmort file for beyond 2022 |
| `Cost_00_progress.R`                        | Define progression (AP/BP) |
| `Cost_00_sct.R`                             | Define SCT |
| `Cost_00_tfr_cost.R`                        | (Results are not used in the analysis) Simply check what costs incurred during TFR |
| `Cost_00_tfr.R`                             | Define Treatment-free remission (TFR) |
| `Cost_00_tki_sublev.R`                      | Organise the tki data in the substance level            |
| `Cost_00_tki.R`                             | Clean data for TKI |
| `Cost_01a_studypop.R`                       | Define the study population |
| **Cost**                                   |             |
| `Cost_02a_yearly_cpp.R`                     | Calculate yearly cost per patient |
| `Cost_02b_yearly_cpp_plot.R`                | Plot yearly cost per patient |
| `Cost_02b1_yearly_cpp_dist_plot.R`          | Plot the composition of TKI by line over years |
| `Cost_02c_yearly_cpp_tab.R`                 | Calculate average yearly cost per patient for all costs in a tabulated format |
| `Cost_02c1_yearly_cpp_resource_tab.R`       | Calculate average yearly cost per patient for resource use in a tabulated format |                       |
| `Cost_02c2_yearly_cpp_tki_tab.R`            | Calculate average yearly cost per patient for tki costs in a tabulated format |
| **Determine the distribution of prevalent CML patients**| Use Study III's CML natural history model |
| `Cost_03a_survmod.R`                        | Fitted survival function for each transition-specific hazard |
| `Cost_03b_microsim_cml.R`                   | Run the natural history model to obtain the state-occupancy probabilities over time |
| `Cost_03c_microsim_cml_apbp.R`              | Run the natural history model (reduced to only the progression state) to obtain the state-occupancy probabilities over time            |
| **Prevalence distribution**                 |             |
| `Cost_04a_prev_dist.R`                      | Estimate the distribution of prevalent CML cases (for Scenario 1) |
| `Cost_04a1_prev_dist_plot.R`                | Plot the distribution of prevalent CML cases over years (for Scenario 1) |
| `Cost_04a2_prev_dist_tab.R`                 | Tabulate the distribution of prevalent CML cases over years (for Scenario 1) |
| `Cost_04b_prev_dist_dynRS.R`                | Estimate the distribution of prevalent CML cases (for Scenario 2) |
| `Cost_04b1_prev_dist_dynRS_plot.R`          | Plot the distribution of prevalent CML cases over years (for Scenario 2) |
| **Prevalence costs**                     |             |
| `Cost_05a_prev_distcosts.R`                 | Estimate the prevalent costs by state |
| `Cost_05a1_prev_distcosts_plot.R`           | Plot the prevalent costs by state |
| `Cost_05b_prev_costs_tab.R`                 | Tabulate the prevalent costs by state |
| **One-way sensitivity analyses**            |             |
| `Cost_07_sensit_plot.R`                     | One-way sensitivity analyses for various scenarios |
| `Cost_08_prev_avcpp_cost.R`                 | Plot three things together: (i) crude prevalent cases of CML, (ii) average yearly cost per patient, and (iii) total prevalence costs |
| `Cost_08_prevcases.R`                       | Plot total prevalence cases |

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