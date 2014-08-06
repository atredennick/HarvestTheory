Effects of Tree Harvest on the Stable-State Dynamics of Savanna and Forest
=============

Code to reproduce figures in Tredennick and Hanan, in review. The core model, ``SFG_ModelFunction``, is an extension of the model presented by Staver et al. 2011, _Ecology_  (http://www.esajournals.org/doi/abs/10.1890/10-1684.1) and Staver and Levin 2012, _American Naturalist_ (http://www.jstor.org/stable/10.1086/666648).

Requirements
------------------------
R (http://www.r-project.org/; we used R 3.1.0, but our code should be compatible across builds) and the following R packges:

* ``ggthemes``
* ``grid``
* ``plyr``
* ``reshape2``
* ``ggplot2``

To install these packages, run the code chunk below in your R console:

```coffee
install.packages("ggplot2", dependencies = TRUE)
install.packages("plyr")
install.packages("ggthemes")
install.packages("reshape2")
install.packages("grid")
```


License
-------------------------
Copyright (c) 2014, Andrew Tredennick

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Contact Information
-------------------------
Andrew Tredennick's email: atredenn [at] gmail.com 
