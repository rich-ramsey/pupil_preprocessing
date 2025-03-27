This projects takes pupillometry data and creates a preprocessing pipeline in R using the pupillometry package.

# Aims and context #

The basic preprocessing steps and principles stem from these papers and resources:
[Kiret & Sjak-Shie (2019)](https://link.springer.com/article/10.3758/s13428-018-1075-y)
[Geller et al., (2020)](https://link.springer.com/article/10.3758/s13428-020-01374-8)

This project aims to setup and test a preprocessing pipeline in R, which uses the pupillometry package and approximately follows the functions written in MATLAB by Kiret & Sjak-Shie (2019).


# The Pupillometry package #

The main pupil preprocessing functions in this project all come from the fantastic pupillometry package, which is written in R:
[pupillometry package](https://dr-jt.github.io/pupillometry/index.html)

Many thanks to Jason Tsukahara for developing the package and for responding to my questions over email.
The main additions I made to the pupillometry package's default workflow was to wrap some the of the processing functions around trials and participants, so that an entire dataset can be processed at once.
I also added a way to vary parameter settings and visualise how it impacts preprocessing. 
Before looking at my workflow and this project, I would definitely recommend looking at the very nice processing template that comes with the pupillometry package, as it gives a helpful overview of some of the key functions and the general workflow:
[preprocessing template](https://dr-jt.github.io/pupillometry/articles/preprocess_overview.html)

# Data #



# What is the easiest way to access this project? #

If you want to see and work with the code, then:

1. Clone, fork or download the project from github to your local machine.
See this link for the difference between cloning and forking. https://github.com/orgs/community/discussions/35849

2. Open the pupil_preprocessing_example.Rproj file and renv() will automatically bootstrap itself.

3. Use renv::restore() to install all of the packages. Say yes.

4. At this point, you can use the project with the same package versions that are stored in the renv.lock file.


# Basic structure of the project #

## R project file ##

All files and folders are based within an R project called 'pupil_preprocessing_example.Rproj'

## There are four main R markdown files: ##

**1. preprocess.Rmd**

This file preprocesses and visualises data for one eye.

**2. preprocess_both.Rmd**

This file preprocesses and visualises data when using data for both eyes (left and right).

**3. parameter_tests.Rmd**

This file runs parameter tests on data for one eye.

**4. parameter_tests_both.Rmd**

This file runs parameter tests on data for both eyes (left and right).

**5. simulate_pupil_data.Rmd**

This file simulates pupil size data for one eye and then for both eyes.

## There are four sub-folders, which have largely self-explanatory titles: ##

**1. /figures/**

**2. /data/**

**3. /custom_functions/**


# Acknowledgments #

Many thanks to the following people:

Jenny Imhof @ ETH Zurich for answering a million and one questions about pupil preprocessing.

Jason Tsukahara for developing the pupillometry package.

Jason Geller and the other developers of the gazeR package (https://github.com/dmirman/gazer), as this package was also helpful in ordanising and structuring my thoughts and ideas.

And finally, thanks to Claude (Anthropic), an AI assistant that helped with code organization, optimization, and feature implementation (as well as writing this acknowledgement!).

