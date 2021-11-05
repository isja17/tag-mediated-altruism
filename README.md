# tag-mediated-altruism

"A long-standing problem in biological and social sciences is to understand the conditions required for the emergence and maintenance of cooperation in evolving populations." (Riolo et al.) In this project, I take ideas from Rolio and expand on them to test the conditions in which altruism evolves.

In Riolo's model, to test altruism, we give each agent a tag and tolerence. Both of these are a random floating # between 0 and 1. Starting with a score of zero, two agents are paired, the difference of their tags are taken and compaired to the tolerance of the potential donor. If the difference is lte the tolerence, then the donation happenes. A small cost (0.1) is deduced from the donors score, and a benefit (1.0) is added to the recipients score. 

By finding the difference of the tag, we are finding the similarity between the two agents. The tolerance represents ones willingness to give. If the difference between the two agents is lower than the potential donors tolerance to give, the agent will donate.

This model seems to miss the mark on what altruism is. In this, someone MUST donate if the agent they are paired with is similar to them. This seems to miss that altruism is a voluntary act, meaning the potential donor must be willing to give to someone before they consider similarity. I take Riolo's model and add a willingness to the agents. If the willingness of the potential donor is greater than the recievers and they are similar, a donation happens. 


## Usage

This Clojure projects requires Gorilla-Repl, as well as gorilla-plot.core. 

There are two seperate relevent files. The first is tag-mediated-altruism.clj, which is the implementation of the base idea of this project described above, as well as plots using gorilla-plot. 

The other file is an interation on this base project to further expand the use of evolution for understanding altruism by adding extra mutating variables. More info is included in the file itself.

Gorilla-Repl is similar to Jupyter notebook, so instead of running the entire .clj file, you execute individual cells. This is also why there is weird html stuff in the two project files.


## Future work

I would like to make this work without gorilla repl and gorilla-plot.

Organize code in a better, clojur-y way.


