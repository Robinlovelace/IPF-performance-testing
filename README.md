Testing the iterative proportional fitting procedure
========

Introduction
----------
This project will rigorously test static spatial microsimulation models in a range of conditions. We will begin with IPF-based approaches, as these are simple, deterministic and readily available. The project is open access: all the code and datasets used to conduct the testing can be downloaded and modified. We encourage others to fork the entire project using the command `git clone` for testing on home computers. 

### Research problem
The research problem tackled is lack of rigorous and repeatable model evaluation and testing. Data and code used in published microsimulation research is not always publicly available. This can make it difficult for readers to reproduce results, test models in new ways and adapt the code for their own purposes. 

### Aim
The project aims to test each IPF model in a range of realistic conditions, changing only one parameter at a time. This will allow factors affecting IPF performance to be isolated and analysed.

### Input data
This data-driven approach requires a variety of input datasets. These will be grouped into scenarios that are designed to be realistic: simple to complex, small to large. The next stage is model preparation, which involves tailoring each to produce small area microdata in each of the scenarios. The final stage is testing: systematically altering specific aspects of each model to observe the factors affecting model performance.

Priorities
----------------
This is the current list of priorities, and will be updated as the project progresses:

* Discuss the modus operandi and structure of project
* Upload publicly available test data scenarios
* Add models, and modify them to generate spatial microdata for each scenario
* Run the models under a variety of scenarios, changing one parameter at a time. These parameters will include:
  * The initial weights
  * Number of iterations
  * Order of constraints
  * Integerisation
  * The use of cross-tabulated vs univariate categories (where available)
* Evaluate the results using quantitative methods

RMarkdown
=======
It is recommended that the initial analysis is written in RMarkdown language (as this introductory page is): it is lightweight, fast to type and, most importantly, allows R commands to be embedded and compiled within the text. For example, let us plot a a simple polynomial:


```r
x = 1:10
x2 = x^2 - 0.1 * x^3
plot(x, x2)
```

![R-generated-image](https://github.com/Robinlovelace/IPF-performance-testing/blob/master/figure/unnamed-chunk-1.png)





