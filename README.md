<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/github_username/repo_name">
    <img src="images/GP2_logo.png" alt="Logo" width="300" height="70">
  </a>

<h3 align="center">Visualize longitudinal and cross-sectional variant effects</h3>

  <p align="center">
    One of the projects from the 2021 GP2/IPDGC Hackathon. The related manuscript can be found on [biorxiv](https://www.biorxiv.org/content/10.1101/2022.05.04.490670v1) 
    <br />
    Contributers: Michael Ta, Alejandro Martinez-Carrasco, Clodagh Towns, Regina Reynolds, María Teresa Periñán, Nikita Pillay
    <br />
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#quick-description">Quick Description</a></li>
        <li><a href="#background/motivation">Background/motivation</a></li>
        <li><a href="#workflow-summary">Workflow Summary</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

![Project Screen Shot][project-screenshot]

### Quick Description

The goal for this project was to visualize longitudinal and cross-sectional variant effects from GWAS.

### Background/motivation

Longitudinal biomarker data is needed to understand the progression of disease and allow for easier testing and diagnosis. This tool takes results from biomarker GWAS to allow researchers to be able to query for a particular biomarker and get a visualisation of the effect on all cohorts or a set of cohorts, as well as the associated meta-analysis. It also adds the ability to display longitudinal information alongside cross-sectional results. 

### Workflow Summary

1. Download/clone repository. 
2. Install necessary packages.
3. Open app.R in R Studio.
4. Click 'Run App' button in R Studio.
5. Visualize!

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

* [R and R studio](https://posit.co/download/rstudio-desktop/)
* [R Shiny](https://shiny.rstudio.com/)

### Installation

1. Clone the repo
   ```sh
   git clone https://github.com/ipdgc/GP2-visualise-longitudinal-variant-effects.git
   ```

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- USAGE EXAMPLES -->
## Usage

There is already example data from biomarker GWAS conducted with the PPMI cohort. However you can upload your own GWAS statistics using the upload tab on the app!


Make sure the necessary packages are installed, they are listed in app.R. All packages except 'ggforestplot' can be installed with `install.packages()`. 'ggforestplot' can be installed like this:

```
install.packages('devtools')
devtools::install_github("NightingaleHealth/ggforestplot")

```

If you wish to visualize your own variant effects, input data should look like this:

| #CHROM | POS      | ID                  | REF | ALT | A1 | A1_FREQ    | TEST    | OBS_CT | BETA       | SE         | T_STAT | P          | OBS_CT_rep |
|--------|----------|---------------------|-----|-----|----|------------|---------|--------|------------|------------|--------|------------|------------|
| 22     | 15226216 | chr22:15226216:GC:G | GC  | G   | G  | 0.00852273 | ADD_REP | 176    | -0.0723873 | 0.25866336 | -9     | 0.77959159 | 687        |
| 22     | 15279243 | chr22:15279243:A:T  | A   | T   | T  | 0.00568182 | ADD_REP | 176    | 0.11891265 | 0.30542448 | -9     | 0.69702783 | 687        |
| 22     | 15284730 | chr22:15284730:C:G  | C   | G   | G  | 0.00568182 | ADD_REP | 176    | -0.1478226 | 0.31534816 | -9     | 0.63924118 | 687        |
| 22     | 15306199 | chr22:15306199:A:T  | A   | T   | T  | 0.01420455 | ADD_REP | 176    | -0.0959635 | 0.19529632 | -9     | 0.62316195 | 687        |
| 22     | 15308575 | chr22:15308575:A:T  | A   | T   | T  | 0.01136364 | ADD_REP | 176    | -0.2816581 | 0.2177098  | -9     | 0.19575805 | 687        |


<p align="right">(<a href="#readme-top">back to top</a>)</p>


<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

* [PPMI](https://www.ppmi-info.org/)


<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[project-screenshot]: images/project_screenshot.png
