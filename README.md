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


## Homework 05 - Questionnaire

The purpose of this homework is to find more examples of parts of questionnaires that violate the best practices of questionnaire design. Find at least 5 but no more than 10 such examples. For each example provide a reference to the questionnaire (so we can cite it) and a short comment on what you think is wrong or could be improved.

Submit the homework in a format that you consider the most appropriate (anything goes). Keep in mind that the goal is to make the handout better, so we'll literally include the best examples in the handout. Examples from different questionnaires, more serious mistakes, and examples that are not covered by other submissions will be more appreciated.


## Homework 06 - Cross Validation

The happiness.csv dataset includes data of the World Happiness Report from 2017--2019. The dataset has the following columns:

    The year and country columns are self explanatory.
    The score column is the happiness score, the higher it is more happy the populants of a country are.
    The economy column is GPD per capita.
    The column perceived_coruption denotes how populants of a country think of corruption in it, there 0 denotes no corruption, while 1 denotes maximum corruption.

The goal of this homework is to build at least three models (if you wish you can build more) that estimate how various features influence happiness. Note that you do not have to use all of the features, some might be useless (e.g. year). In the first step of your evaluation use LOOIC to determine the quality of your models. In the second step use Akaike weights and LOOIC to gain additional insight into how your models compare against each other and how you would weigh decisions of models if you were to combine multiple models together.

Submit your solution in the form of a 2-page A4 PDF report.

p. s. Like mentioned, if you are filling adventurous, feel free to use the whole, much richer dataset. It can be found at https://www.kaggle.com/unsdsn/world-happiness and at https://worldhappiness.report.