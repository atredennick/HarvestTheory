Effects of Tree Harvest on the Stable-State Dynamics of Savanna and Forest
=============

Code to reproduce figures in Tredennick and Hanan in press, _American Naturalist_. The core model, ``SFG_ModelFunction``, is an extension of the model presented by Staver et al. 2011, _Ecology_  (http://www.esajournals.org/doi/abs/10.1890/10-1684.1) and Staver and Levin 2012, _American Naturalist_ (http://www.jstor.org/stable/10.1086/666648).

The abstract of our paper:
>Contemporary theory on the maintenance and stability of the savanna biome has
focused extensively on how climate and disturbances interact to affect tree growth and
demography. In particular, the role of fire in reducing tree cover from climatic maxima
is now well appreciated, and in certain cases herbivory also strongly affects tree cover.
However, in African savannas and forests, harvest of trees by humans for cooking and
heating is an oft overlooked disturbance. Thus, we incorporate tree harvest into a 
population dynamic model of grasses, savanna saplings, savanna trees, and forest trees. We
use assumptions about the differential demographic responses of savanna trees and forest
trees to harvest to show how tree harvest influences tree cover, demography, and community
composition. Tree harvest can erode the intrinsic basin of attraction for forest and
make a state transition via fire to savanna more likely. The savanna state is generally
resilient to all but high levels of tree harvest due to the resprouting abilities of savanna
trees. In the absence of active fire suppression our analysis suggests we can expect to see
large and potentially irreversible shifts from forest to savanna as demand increases for
charcoal in sub-Saharan Africa. On the other hand, savanna tree species’ traits promote
savanna stability in the face of low-to-moderate harvest pressure

Requirements
------------------------
For most all of our results, we used R (http://www.r-project.org/; we used R 3.1.0, but our code should be compatible across builds) to simulate the model and the following R packges:

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
However, we used the symbolic math functions in Matlab (http://www.mathworks.com/products/matlab/) to calculate system equilibria (see ``EquilibriumSolutions.m``).

License
-------------------------
Copyright (c) 2014, Andrew Tredennick

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Contact Information
-------------------------
Andrew Tredennick's email: atredenn [at] gmail.com 
