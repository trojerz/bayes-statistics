Homeworks for subject Bayesian Statistics, Year 2, Masters on FRI - DataScience.



## Homework 01 - Paper Analysis
 
Analyze the Parasuraman et al.'s paper using the PPDAC investigative cycle. For each step and substep list what the authors explicitly or implicitly did, identify potential sources of error, and critique their choices. For the A & C steps you can skip the criticism, but we'll be more impressed if you can also critically view the choice of model and interpretation of results.

Submit a 1 page, A4, pdf report here. Structure and format your report to make the reader's work easier. You may assume that the reader is familiar with statistics, the paper, and what the homework is.


## Homework 02 - Probabilistic thinking, Stan installation

Write down three questions from your everyday life that you often reply to with an uncertain answer. Answer those questions uncertainly with natural language. Then answer those questions using probabilistic answers that roughly correspond to the natural language questions. At least one of the questions must have uncountably many answers (truths) and at least one of the questions must have infinitely but countably many answers.

Install Stan and run the Bernoulli toy example from the installation instructions (https://mc-stan.org/cmdstanr/articles/cmdstanr.html). You may use CmdRStan (recommended), RStan, PyStan, or any other interface. Plot the posterior distribution of the Bernoulli's distribution parameter (Θ). Calculate P(Θ > 0.3) and P(|Θ - 0.5| < 0.001).


## Homework 03 - Probabilistic programming

We are owners of a startup in the USA and we have to decide how to distribute our resources and where to open our offices. The "50_startups.csv" dataset (you can find in the the official course repository: https://github.com/fri-datascience/course_bs/tree/main/Session_03_Probabilistic_programming/data) contains information about how many resources a startup company used for research, administration or marketing along with the location of the company and their profit. Upgrade the linear regression model that we developed during our lectures into a multiple linear regression model and use it to gain insight into how we should distribute our resources and where we should have our offices to maximise the profit.

Even though this is a relatively simple analysis, you should execute it with everything we learned in the previous two weeks in mind. Submit the solution over e-classroom in the form of a single page A4 PDF.


## Homework 04 - GLM

We want to develop a successful video game. Based on personal preferences, we narrowed down the choice to two genres (shooters and role-playing games) and three platforms (PC, PlayStation/PS and Xbox). For the sake of this homework, assume that there are no tools that allow us simultaneous development for all three platforms. Use the video game sales dataset (https://github.com/fri-datascience/course_bs/blob/main/Session_04_GLM/data/videogame_sales.csv) and a gamma regression model to research how genre and platform influence the game's sales. Submit a 1-page A4 PDF report.

Note: you will probably see some warnings at the beginning of the fitting process. When these warnings only appear at the beginning of the warmup phase, you can most likely just ignore them. If the diagnostics we perform on our fit give us no reasons for concern, we are good to go. If we should be bothered by these warning then subsequent diagnostics will show us that something is amiss.