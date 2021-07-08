# Introduction {#intro}

<img src="C:/Users/gangmin.li/Documents/Books/Bookdown-Do-Data/images/dataScientist.jpeg" width="50%" style="display: block; margin: auto;" />

>
>     
>     "What profession did Harvard call the Sexiest Job of the 21st Century?"
>
>       That’s right. You guessed it,
>
>       The data scientist.
>
>
>                                   -- "Data Scientist"
> The Sexiest Job of the 21st Century. 
> (https://hbr.org/2012/10/data-scientist-the-sexiest-job-of-the-21st-century).  
>
>

Ah yes, the ever-mysterious data scientist. So what exactly is the data scientist’s secret sauce, and what does this “sexy” person actually do at work every day? How they do it?

## What is Data Science?

Data science is a multidisciplinary field. It blends data mining, data analysis, statistics, algorithm development, machine learning, and advanced computing and software technology together in order to solve analytically complex problems. Its ultimate goal is to reveal insight into data and get the data value for the business.

<div class="figure" style="text-align: center">
<img src="Capture/whatisdatascience.jpg" alt="Concept of Data Science" width="80%" />
<p class="caption">(\#fig:unnamed-chunk-2)Concept of Data Science</p>
</div>

### Data Science as Discovery of Data Insight {-}

This aspect of data science is all about uncovering hidden patterns from data. Diving in at a granular level to mine and understand complex patterns, trends, and relations. It's about surfacing hidden insight that can help and enable companies to make smarter business decisions and take appropriate actions to gain competitive advantages in the market. For example:

* Amazon build recommendation system to provide users suggestion on the purchase based on the user's shopping history.
* Netflix data mines movie viewing patterns to understand what drives user interest, and uses that to make decisions on which Netflix original series to produce.
* Target identifies are major customer segments within its base and the unique shopping behaviors within those segments, which helps to guide messaging to different market audiences.
* Proctor & Gamble utilizes time series models to more clearly understand future demand, which helps plan for production levels more optimally.
How do data scientists mine out insights? It starts with data exploration. When given a challenging question, data scientists become detectives. They investigate leads and try to understand patterns or characteristics within the data. This requires a big dose of analytical creativity.

How do data scientists mine data insights? there is a procedure to follow. It generally starts with data description it is called Described data analysis (DDA) to get first sight on the data sets available. DDS will help data scientists to grasp the quantity and quality of the data. so they can decide how to deal with the data. it then generally followed by data cleaning, manipulation, transform and attributes engineering, etc, together called preprocess. Data preprocessing is also generally combined with exploratory data analysis (EDA). When given a challenging question, data scientists normally become detectives. They investigate all the information available and follow any possible leads and try to understand patterns or characteristics within the data. This not only requires a huge amount of tools and techniques but also demands analytical creativity.

Then as needed, data scientists may apply quantitative techniques in order to get a level deeper – e.g. statistical methods, projections, inferential models, segmentation analysis, time series forecasting, synthetic control experiments, etc. The intent is to scientifically piece together a forensic view of what the data is really saying.

This data-driven insight is central to providing strategic guidance. In this sense, data scientists act as consultants, information providers help business stakeholders on how to act on findings.

### Data Science as Development of Data Product {-}

A "data product" is a technical asset that: 

1. utilizes data as input, and 
2. processes that data to return algorithmically-generated results. 

A typical example is the users' scoring system. It takes users' profile or/and behavior data as input and with a complex scoring engine, it produces a credit score of the users for business decision making. 
Another example of a data product is a recommendation engine, which ingests user data, and makes personalized recommendations based on that data. 
Here are some examples of data products:

* Amazon's recommendation engines suggest items for you to buy, determined by their algorithms. 
* Netflix recommends movies to you. Spotify recommends music to you.
* Gmail's spam filter is a data product – an algorithm behind the scenes processes incoming mail and determines if a message is junk or not.
* Computer vision used for self-driving cars is also a data product – machine learning algorithms are able to recognize traffic lights, other cars on the road, pedestrians, etc.

This is different from the "data insights" section above, where the outcome to that is to perhaps provide advice to an executive to make a smarter business decision. In contrast, a data product is a technical functionality that encapsulates an algorithm and is designed to integrate directly into core applications. Respective examples of applications that incorporate data products behind the scenes: Amazon's homepage, Gmail's inbox, and autonomous driving software.

Data scientists play a central role in developing data products. This involves building out algorithms, as well as testing, refinement, and technical deployment into production systems. In this sense, data scientists serve as technical developers, building assets that can be leveraged at a wide scale.

## What is Data Scientist?

Data scientists are a new breed of analytical data experts who has the technical skills to solve complex problems – and the curiosity to explore what problems need to be solved. They are part mathematician, part computer scientist, and part business trend-spotter. They straddle in both the business and IT worlds with mathematical and programming weaponry.  

### The Requisite Skill Set {-}

A data scientist needs a blend of skills in three major areas:


1. Mathematics
2. Computing and Software Engineering
3. Business 

<div class="figure" style="text-align: center">
<img src="Capture/datascientist.jpg" alt="Quality of Data Scientists" width="60%" />
<p class="caption">(\#fig:unnamed-chunk-3)Quality of Data Scientists</p>
</div>

#### Mathematics Narrator {-}

The heart of mining data insight and building data product is the ability to view the data through a quantitative lens. There are textures, dimensions, and correlations in data that can be expressed mathematically. Finding solutions utilizing data becomes a brain teaser of heuristics and quantitative techniques. Solutions to many business problems involve building analytic models grounded in the hard math, where being able to understand the underlying mechanics of those models is key to success in building them.

Also, a misconception is that data science all about **statistics**. While statistics are important, it is not the only type of math utilized. First, there are two branches of statistics – classical statistics and Bayesian statistics. When most people refer to stats they are generally referring to classical statistics, but knowledge of both types is helpful. Furthermore, many inferential techniques and machine learning algorithms lean on the knowledge of **linear algebra**. For example, a popular method to discover hidden characteristics in a data set is SVD, which is grounded in matrix math and has much less to do with classical stats. Overall, it is helpful for data scientists to have breadth and depth in their knowledge of mathematics.

#### Computing and Software Engineer Skills {-}

Data is now collected, stored, and processed with a computer. With the increase of data quantity, as termed as, we are entering the big data era. The conventional way of processing data facing an unprecedented challenge. The personal computer maybe not adequate to handle big data. Distributed storage, cloud computing, and computer clusters become commonly-used platforms for data access and controls. Basic computing environment configuration and settings are common skills need to handle data. 

The data processing tools and languages like R or Python, and a database querying language like SQL is the commonly used languages in data process and data analyzing. It is also important to have strong software engineering knowledge so it can be comfortable to handle a large amount of data logging, and to develop data-driven products.

Data scientists need to utilize new technology in order to wrangle enormous data sets and work with complex algorithms and code or prototype quick solutions, as well as interact and integrate with complex data systems. Core languages associated with data science include SQL, Python, R, and SAS. On the periphery are Java, Scala, Julia, and others. But it is not just knowing language fundamentals. A data scientist is a technical ninja, able to creatively navigate their way through technical challenges in order to make their code work.

Along these lines, a data scientist is a solid algorithmic thinker, having the ability to break down messy problems and recompose them in ways that are solvable. This is critical because data scientists operate within a lot of algorithmic complexity. They need to have a strong mental comprehension of high-dimensional data and tricky data control flows. Full clarity on how all the pieces come together to form a cohesive solution.


#### Strong Business Acumen {-}

It is important for a data scientist to be a tactical business consultant, an operation narrator, and a storyteller. Working so closely with data, data scientists are positioned to learn from data in ways no one else can. They can understand the language the data speak and listen to the story the data tells. That creates the responsibility to translate observations, discovery to shared knowledge, and contribute to strategy on how to solve core business problems. This means a core competency of data science is using data to cogently tell a story. No data present a cohesive narrative of problem and solution, using data insights as supporting pillars, that lead to guidance.

Having this business acumen is just as important as having acumen for technology and math and algorithms. There needs to be a clear alignment between data science projects and business goals. Ultimately, the value doesn't come from data, math, and tech itself. It comes from leveraging all of the above to build valuable capabilities and have a strong business influence.

### How to Become a Data Scientist? {-}

Many people start to Position themselves for a career in data science. Not only for good job opportunities but also for the excitement of work in the technology field with freedom for experimentation and creativity. To get to this position you need solid foundations.

A conventional way of becoming a data scientist is Choosing a university that offers a data science degree. Or register yourself for courses in data science and analytics fields. If you cannot do these, the option left to you is to learn by yourself. 

The knowledge and skills you should have are:

+ **Statistics and machine learning**. A good understanding of statistics is vital as a data scientist. You should be familiar with statistical tests, distributions, maximum likelihood estimators, etc. Statistics knowledge will also help you understand when different techniques are (or aren’t) a valid approach. Machine learning (ML) is a good weapon when you involve a big data project. Algorithms are the core of machine learning, although many implementations with R or Python libraries do exist and convenient to use, It is still needed a thorough understanding of how the algorithms works and when it is appropriate to use different ones. 
+ **Coding languages such as R or Python**. It is essential, a data scientist is competent with a number of computing and data querying languages like R, Python, and SQL. 
+ **Databases such as MySQL and Postgres**. Data is generally stored in a Database. it is important to have the necessary skills for data access and control from a DBMS system. The most commonly used DBMS systems are MySql (https://www.mysql.com/)  and Postgres (https://www.postgresql.org/) in addition to ACCESS and EXCEL. 
+ **Visualization and reporting technologies**. Visualizing and communicating data is incredibly important, especially with companies that are making data-driven decisions, or companies where data scientists are viewed as people who help others make data-driven decisions. When it comes to communicating, this means describing your findings, or the way techniques work to audiences, both technical and non-technical. Visualization can be immensely helpful. Therefore familiar with data visualization tools like matplotlib, ggplot, or d3.js. Tableau and dashboarding have become popular data visualization tools. It is important to not just be familiar with the tools necessary to visualize data, but also the principles behind visually encoding data and communicating information.
+ **Big data platforms like Hadoop**.(https://hadoop.apache.org/) and **Spark** (https://spark.apache.org/). Although a lot of Data Science projects can be tried, or at least prototyped on PC or workstations, it is the reality that most large data analyzing is done on advanced computing platforms like distributed infrastructure or computer clusters. this advanced platform mostly deploy Hadoop ecosystems. 

If you don’t want to learn these skills on your own, take an online course or enroll in a Bootcamp. Like what you do now. It not only provides you the opportunity to gain knowledge quickly but also provides you the chance of networking with other people who has a similar situation as you do. Connect with other people can lead you into an online community. They all will help you gain fine grain and insider knowledge of solving problems. 

## Process of Doing Data Science {#process}

Understand what data science is about is just the start of becoming a data scientist. Once the goal is set. The next task is to select the correct path and work hard to reach your destination. The path is important which can be shorter or longer, or direct and smooth,  or curvy and bumpy. It is vital to follow a short and smooth path. This path is the data science project process. Figure \@ref(fig:process) is the 6 steps process, which is inspired by the CRISP (Cross Industry Standard Process for Data Mining) [@Chapman2000], [@Shearer2000], and KDD (knowledge discovery in databases) process [@Li2018].  

<div class="figure" style="text-align: center">
<img src="Capture/process.png" alt="Process of doing Data Science" width="100%" />
<p class="caption">(\#fig:process)Process of doing Data Science</p>
</div>

### Step 1: Understand the Problem - Define Objectives {#step1 -}

Any data analysis must begin with business issues. With business issues, a number of questions should be asked. These questions have to be the right questions and measurable, clear, and concise. Define analysis question is regarded as define a Data Requirements engineering and to get a data project specification. It starts from a business issue and asking relevant questions, only after you fully understand the problem and the issues you may be able to turn a practical problem into analytical questions. 

For example, start with a business issue: A contractor is experiencing rising costs and is no longer able to submit competitive contract proposals. One of many questions to solve this business problem might include: Can the company reduce its staff without compromising quality? Or, can the company find an alternative supplier on the production chain?

Once you have questions, you can start to think about the data required for analysis. The data required for analysis is based on questions. The data necessary as inputs to the analysis can then be identified (e.g., Staff skills and performance). Specific variables regarding a staff member (e.g., Age and Income) may be specified and obtained. The data type can be defined as numerical or categorical.

After you defined your analytical questions. It is important to set a clear evaluation of your project to measurement how the success of your project. 

This generally breaks down into two sub-steps: A) Decide what to measure, and B) Decide how to measure it.

 **A) What To Measure?**
 
Using the contractor example, consider what kind of data you’d need to answer your key question. In this case, you would need to know the number and cost of current staff and the percentage of time they spend on necessary business functions. This is what is called in business as KPI - Key performance indicators. In answering this question, you likely need to answer many sub-questions (e.g., Are staff currently under-utilized? If so, what process improvements would help?). Finally, in your decision on what to measure, be sure to include any reasonable objections any stakeholders might have (e.g., If staff is reduced, how would the company respond to surges in demand?).

**B) How To Measure? **

Thinking about how you measure the success of your data science project, the deep end is to measure some key performance indicators. They are the data you have chosen to use in the previous step. So measure your data is just as important, especially before the data collection phase, because your measuring process either backs up or discredits your project later on. Key questions to ask for this step include:

+ What is your time frame? (e.g., annual versus quarterly costs)
+ What is your unit of measure? (e.g., USD versus Euro)
+ What factors should be included? (e.g., just annual salary versus annual salary plus the cost of staff benefits)

### Step 2: Understand Data - Knowing your Raw Materials {#step2 -}

The second step is to understand data. It includes **Data collection** and **Data Validation**. With the problem understood and analytical questions defined and your validation criteria and measurements set, It is time to collect data. 

#### Data Collection {-}

Before collect data,  the data source has to be determined based on the relevance. A variety of data sources may be assessed and accessed to get relevant data. These data sources may include existing databases, or organization's file system, or a third-party service, or even open web sources. They could provide redundant, or complementary, sometimes conflicting data. it has to be cautious to select the right data source from the very beginning. sometimes you need to gather data via observation or interviews, then develop an interview template ahead of time to ensure consistency. it is a good idea to Keep your collected data organized in a log with collection dates and add any source notes as you go (including any data normalization performed). This practice validates your data and any conclusions down the road.

Data Collection is the actual process of gathering data on targeted variables identified as data requirements. The emphasis is on ensuring correct and accurate data collection, which means correct procedure was taken and appropriate measurements were adopted. the maximum efforts were spent to ensure the data quality. Remember that data Collection provides both a baseline to measure and a target to improve for a successful data science project.

#### Data Validation {-}

Data validation is the process to Assess data quality. It is to ensure the collected data have reached quality requirements identified in step 1, that is, they are useful and correct. The usefulness is the most important aspect. Regardless of how accurate your data collections can be, if it is not useful, anything that follows is just a waste. It is hard to define the usefulness. depends on the problem at hand and the requirements for the find al delivery. The usefulness can vary from a strong correlation between the raw data and the expected outcomes, to direct prediction power from the raw data to the consequence variables. Generally, data validation can include:

+ Data type validation
+ Range and constraint validation
+ Code and cross-reference validation
+ Structured validation
+ Consistency validation
+ Relevancy validation 

### Step 3: Data Preprocessing - Get your Data Ready {#preprocess -}

Data preprocessing is a step that takes data processing methods and techniques to transforms raw data into a formatted and understandable form and ready for analyzing. Real-world data is often incomplete, inconsistent, and is likely to contain many errors. Data preprocessing is a proven method of resolving such issues. Tasks of data preprocessing may include:

+ **Data cleaning**. The process of detecting and correcting (or removing) corrupt or inaccurate records from a recordset, table, or database. It normally includes identifying incomplete, incorrect, inaccurate, or irrelevant data and then replacing, modifying, or deleting the dirty or coarse data. After cleansing, a data set should be consistent with other data sets. 

+ **Data editing**. The process involves changing and adjusting of collected data. The purpose is to ensure the quality of the collected data. Data editing should be done by fully understand the data collected and the data requirement specification. Editing data without them can be disastrous. 

+ **Data reduction**. The process and methods used to reduce data quantity to fit for analyzing. Raw data set collected or selected for analysis can be huge, then it could drastically slow down the analysis process. Reducing the size of the data set without jeopardizing the data analysis results is often desired. It includes records' number reduction and data attributes reduction. Methods used to reduce data records size include **Sampling** and **Modelings** (e.g., regression or log-linear models or histograms, clusters, etc). Methods used for attributes reduction include **Feature selection** and **Dimension reduction**. Feature selection means the removal of irrelevant and redundant features. such an operation should not lose information the data set has. Data analysis algorithms work better if the dimensionality, which is the number of attributes in a data object is low. Data compression techniques (e.g., wavelet transforms and principal components analysis), attribute subset selection (e.g., removing irrelevant attributes as discussed in the previous paragraph), and attribute construction (e.g., where a small set of more useful attributes is derived from the large numbers of attributes in the original data set) are useful techniques.  

+ **Data transformation** sometimes referred to as **data munging** or **data wrangling**. It is the process of transforming and mapping data from one data form into another format with the intent of making it more appropriate and valuable for downstream analytics. It is often that data analysis method requires data to be analyzed have a certain format or possesses certain attributes. For example, classification algorithms require that the data be in the form of categorical (nominal) attributes; algorithms that find association patterns require that the data be in the form of binary attributes. Thus, it is often necessary to transform a continuous attribute into a categorical attribute, which is called **Discretization**, and both continuous and discrete attributes may need to be transformed into one or more binary attributes, which is called **Banalization**. Other methods include **Scaling** and **normalization**. Scaling changes the bounds of the data and can be useful, for example, when you are working with data in different units. Normalization scales data sets to a smaller range such as [0.0, 1.0].   

+ **Data re-engineering**
Re-engineering data is necessary when raw data come from many different data sources and in a different format. Data re-engineering similar to data transformation can be done at both a record level and an attribute level. Record level re-engineering is also called data **Integration**, which integrates a variety of data into one file or place and in one format for analysis. for predictive analysis with a model, data re-engineering is also including split a given data set into two subsets called "Training" and "Test" Set.

### Step 4: Data Analyese - Building Models {#analyse -}

After your collected data being preprocessed and suitable for analysis. Now you can drill down and attempt to answer your question from [Step 1](#step1) with the actions called Data Analyzing. It is the core activity in the data science project process by writing, executing, and refining computer programs that utilize some analytical methods and algorithms to obtain insights from data sets. There are three broad categories of data analytical methods: **Descriptive data analysis (DDA)**, **Exploratory data analysis (EDA)**, and **Predictive data analysis (PDA)**. DDA and EDA use
 quantitative and statistical methods on data sets and data attribute measurements and their value distributions while DDA focus on numeric summary and EDA emphasis on graphical (plot) mean, PDA involves model building and machine learning. 
In a data science project, data analyzing is generally starting from Descriptive analysis and goes further with Exploratory analysis, and finally end up with a tested and optimized prediction model for predicting. However, it does not necessarily mean that the methods used in a data analysis project have to stick in this order. As matter of fact, the most project involves a recursive process and a mixture of all the three methods. For example, An exploratory analysis can be utilized in a feature engineering step to prepare a predictor of a prediction model, which is used to predict some missing values of a particular attribute in a given dataset for an accurate description of the attribute value distribution.

#### **Descriptive data analysis** {-}

It is the simplest type of analysis. It describes and summarizes data sets quantitatively. Descriptive analysis\index{Descriptive analysis} generally starts with univariate analysis, meaning describing a single variable (can also be called an attribute, column, or field) of the data. The appropriate depends on the level of measurement. For nominal variables, a frequency table and a listing of the modes are sufficient. For ordinal variables, the median can be calculated as a measure of central tendency and the range (and variations of it) as a measure of dispersion. For interval level variables, the arithmetic mean (average) and standard deviation are added to the toolbox and, for ratio level variables, we could add the geometric mean and harmonic mean as measures of central tendency and the coefficient of variation as a measure of dispersion. However, there are many other possible statistics that cover areas such as location (“middle” of the data), dispersion (range or spread of data), and shape of the distribution. Moving up to two variables, descriptive analysis can involve measures of association such as computing a correlation coefficient or covariance. Descriptive analysis’s goal is to describe the key features of the sample numerically. It should shed light on the key numbers that summarize distributions within the data, may describe or show the relationships among variables with metrics that describe association, or by tables that cross-tabulation counts. Descriptive analysis is typically the first step on the data analysis ladder, which only tries to get a sense of the data. 

#### **Explorative data analysis** {-}

Descriptive analysis\index{Descriptive analysis} is very important. However, numerical summaries can only get you so far. One problem is that it can only convert a large number of values down to a few summary numbers. Unsurprisingly, different samples with different distributions, shapes, and properties can result in the same summary statistics. This will cause problems. When you are looking at a simple single summary statistic, the mean of a single variable, there can be a lot of possible “solutions” or samples. The typical example is Anscombe’s quartet [@Anscombe1973], it comprises four datasets that have nearly identical simple statistical properties, yet appear very different when graphed. Most kinds of statistical calculations rest on assumptions about the behavior of the data. Those assumptions may be false, and then the calculations may be misleading. We ought always to try and check whether the assumptions are reasonably correct, and if they are wrong we ought to be able to perceive in what ways they are wrong. Graphs are very valuable for these purposes.

EDA allows us to challenge or confirm our assumptions about the data. It is a good tool to be used in [data preprocess](#preprocess). We often have pretty good expectations of what unclean data might look like, such as outliers, missing data, and other anomalies, perhaps more so than our expectations of what clean data might look like. The more we understood data, we could develop our intuition of what factors and possible relations at are play. EDA, with its broad suite of ways to view the data points and relationships, provides us a range of lenses with which to study the story that data is telling us. That in turn, helps us to come up with new hypotheses of what might be happening. Further,  if we understood which variables we can control, which levers we have to work within a system to drive the metrics such as business revenue or customer conversion in the desired direction. EDA can also highlight gaps in our knowledge and which experiments might make sense to run to fill in those gaps.

The basic tools of EDA are plots, graphs, and summary statistics. Generally speaking, it’s a method of systematically going through the data, plotting distributions of all variables (using box plots), plotting time series of data, transforming variables, looking at all pairwise relationships between variables using scatterplot matrices, and generating summary statistics for all of them or identifying outliers.

#### **Predictive data analysis ** {#predictive -}

Predictive analysis\index{Predictive analysis} builds upon **inferential analysis**, which is to learn about relationships among variables from an existing training data set and develop a model that can predict values of attributes for new, incomplete, or future data points. The inferential analysis is a type of analysis that from a dataset sample in hand infer some information, which might be parameters, distributions, or relationships about the broader population from which the sample came. We typically infer metrics about the population from a sample because data collection is too expensive, impractical, or even impossible to obtain all data. The typical process of inferential analysis includes testing hypotheses and deriving estimates. 
There is a whole slew of approaches and tools in predictive analysis. **Regression** is the broadest family of tools. Within that, however, are a number of variants (lasso, ridge, robust, etc.) to deal with different characteristics of the data. Of particular interest and power is **Logistic Regression** which can be used to predict classes. For instance, spam/not spam used to be mostly predicted with a **Naïve Bayes predictor** but nowadays logistic regression is more common. Other techniques and that come under the term **Machine Learning** include neural networks, tree-based approaches such as classification and regression trees, random forests, support vector machines (SVM), and k-nearest neighbors.

### Step 5: Results Interpretation and Evaluation {-}

After analyzing your data and get some answers to your original questions, it is possible that you need to conduct further research and more analysis. Let us suppose that you are happy with the analysis results you have. It is finally time to interpret your results. As you interpret your analysis, keep in mind that you cannot ever prove a hypothesis true: rather, you can only fail to reject the hypothesis. Meaning that no matter how much data you collect, chance could always interfere with your results. Interpreting the results of the analysis, you should think of how close the results address the original problems by asking yourself these key questions:

+ Does the data answer your original question? How?
+ Does the data help you defend against any objections? How?
+ Are there any limitations on your conclusions, any angles you haven’t considered?

If your interpretation of the data holds up under all of these questions and considerations, then you likely have come to a productive conclusion. However, there could be a chance that you may find you might need to revise your original question or collect more data and you may need to roll the ball from the starting line. Again. Either way, this initial analysis of trends, correlations, variations, and outliers is not completely wasted. They help you focus your data analysis on better answering your question and any objections others might have. That is the next step report and communication. 

### Step 6: Data Report and Communication {-}

Whereas the analysis phase involves programming and runs programs on different computer platforms, the reporting involves narrative the results of the analysis, thinking about how close the results address the original problems, and communicating about the outputs of analyses with interesting parties in many cases in visual formats.
During this step, data analysis tools and software are helpful but visual tools are intuitive and worth a lot of words. Visio, Tableau (https://www.tableau.com/), Minitab (https://www.minitab.com/), and Stata (https://www.stata.com/) are all good software packages for advanced statistical data analysis. There are also plenty of open source data visualization tools available.

It is important to note that the above 6 steps process is not a linear process. Any discovery of useful relationships and valuable patterns is enabled by a set of iterative activities. Iteration can occur in a single step or in a few steps at any point in the process. 

## Tools used in Doing a Data Science Project

Data Scientists use traditional statistical methodologies that form the core backbone of Machine Learning algorithms. They also use Deep Learning algorithms to generate robust predictions. Data Scientists use the following tools and programming languages:

### R {-}

R (https://www.r-project.org/) is a scripting language that is specifically tailored for statistical computing and data. It is widely used for data analysis, statistical modeling, time-series forecasting, clustering, etc. R is mostly used for statistical operations. It also possesses the features of an object-oriented programming language. R is an interpreter-based language and is widely popular across multiple industries particularly for doing data science projects.

### Python {-}

Like R, Python (https://www.python.org/) is an interpreter-based high-level programming language. Python is a versatile language. It is mostly used for Data Science and Software Development. Python has gained popularity due to its ease of use and code readability. As a result, Python is widely used for Data Analysis, Natural Language Processing, and Computer Vision. Python comes with various graphical and statistical packages like Matplotlib, Numpy, SciPy, and more advanced packages for Deep Learning such as TensorFlow, PyTorch, Keras, etc. For the purpose of data mining, wrangling, visualizations, and developing predictive models, we utilize Python. This makes Python a very flexible programming language.

### SQL {-}

SQL stands for Structured Query Language. Data Scientists use SQL for managing and querying data stored in databases. Being able to extract data from databases is the first step towards analyzing the data. Relational Databases are a collection of data organized in tables. We use SQL for extracting, managing, and manipulating the data. For example, A Data Scientist working in the banking industry uses SQL for extracting information from customers. While Relational Databases use SQL, **NoSQL** is a popular choice for non-relational or distributed databases. Recently NoSQL has been gaining popularity due to its flexible scalability, dynamic design, and open-source nature. MongoDB, Redis, and Cassandra are some of the popular NoSQL databases.

### Hadoop {-}

Big data is another trending term that deals with the management and storage of a huge amount of data. Data is either structured or unstructured. A Data Scientist must have a familiarity with complex data and must-know tools that regulate the storage of massive datasets. One such tool is Hadoop (https://hadoop.apache.org/). While being open-source software, Hadoop utilizes a distributed storage system using a model called **MapReduce**. There are several other packages in Hadoop that together formed an Apache ecosystem, such as Apache Pig, Hive, HBase, etc. Due to its ability to process colossal data quickly, its scalable architecture, and low-cost deployment, Hadoop has grown to become the most popular software for Big Data.

### Tableau {-}

Tableau (https://www.tableau.com/) is a Data Visualization software specializing in graphical analysis of data. It allows its users to create interactive visualizations and dashboards. This makes Tableau an ideal choice for showing various trends and insights of the data in the form of interactable charts such as Treemaps, Histograms, Box plots, etc. An important feature of Tableau is its ability to connect with spreadsheets, relational databases, and cloud platforms. This allows Tableau to process data directly, making it easier for the users.

### Weka {-}

For Data Scientists looking forward to getting familiar with Machine Learning in action, Weka (https://www.cs.waikato.ac.nz/ml/weka/) is, can be,  an ideal option. Weka is generally used for Data Mining but also consists of various tools required for Machine Learning operations. It is completely open-source software that uses GUI Interface making it easier for users to interact with, without requiring any line of code.

## Applications of Data Science

Data Science has created a strong foothold in several industries such as Government and education, Healthcare and medicine, banking and commerce, manufacturing and transportation, etc. It has immense applications and has a variety of uses. Some of the applications of Data Science are listed below:

### Data Science in Healthcare {-}

Data Science has been playing a pivotal role in the Healthcare Industry. With the help of classification algorithms, doctors are able to detect cancer and tumors at an early stage using Image Recognition software. Genetic Industries use Data Science for analyzing and classifying patterns of genomic sequences. Various virtual assistants are also helping patients to resolve their physical and mental ailments.

### Data Science in E-commerce {-}

Amazon uses a recommendation system that recommends users various products based on their historical purchases. Data Scientists have developed recommendation systems to predict user preferences using Machine Learning.

### Data Science in Manufacturing {-}

Industrial robots have made taken over mundane and repetitive roles required in the manufacturing unit. These industrial robots are autonomous in nature and use Data Science technologies such as Reinforcement Learning and Image Recognition.

### Data Science as Conversational Agents {-}

Amazon’s Alexa and Siri by Apple use Speech Recognition to understand users. Data Scientists develop this speech recognition system, that converts human speech into textual data. Also, it uses various Machine Learning algorithms to classify user queries and provide an appropriate response.

### Data Science in Transport {-}

Self Driving Cars use autonomous agents that utilize Reinforcement Learning and Detection algorithms. Self-Driving Cars are no longer fiction due to advancements in Data Science.

## Data Science Related Terms 

There is a slew of terms closely related to data science that we hope to add some clarity around.

### DataAnalyst and Data Scientist {-}

Analytics has risen quickly in popular business lingo over the past several years; the term is used loosely, but generally meant to describe critical thinking that is quantitative in nature. Technically, analytics is the "science of analysis" — put another way, the practice of analyzing information to make decisions.

Is "analytics" the same thing as data science? Depends on context. Sometimes it is synonymous with the definition of data science that we have described, and sometimes it represents something else. A data scientist using raw data to build a predictive algorithm falls into the scope of analytics. At the same time, a non-technical business user interpreting pre-built dashboard reports (e.g. GA) is also in the realm of analytics but does not cross into the skill set needed in data science. Analytics has come to have a fairly broad meaning. At the end of the day, as long as you understand beyond the buzzword level, the exact semantics don't matter much.

"Analyst" is somewhat of an ambiguous job title that can represent many different types of roles (data analyst, marketing analyst, operations analyst, financial analyst, etc). What does this mean in comparison to a data scientist?

Data Scientist: Specialty role with abilities in math, technology, and business acumen. Data scientists work at the raw database level to derive insights and build data products.
Analyst: This can mean a lot of things. The common thread is that analysts look at data to try to gain insights. Analysts may interact with data at both the database level or the summarized report level.

Thus, "analyst" and "data scientist" are not exactly synonymous, but also not mutually exclusive. Here is our interpretation of how these job titles map to skills and scope of responsibilities:


### Machine Learning and Data Science {-}

Machine learning is a term closely associated with data science. It refers to a broad class of methods that revolve around data modeling to (1) algorithmically make predictions, and (2) algorithmically decipher patterns in data.

Machine learning for making predictions — The core concept is to use tagged data to train predictive models. Tagged data means observations where ground truth is already known. Training models means automatically characterizing tagged data in ways to predict tags for unknown data points. E.g. a credit card fraud detection model can be trained using a historical record of tagged fraud purchases. The resultant model estimates the likelihood that any new purchase is fraudulent. Common methods for training models range from basic regressions to complex neural nets. All follow the same paradigm known as supervised learning.

Machine learning for pattern discovery — Another modeling paradigm is known as unsupervised learning tries to surface underlying patterns and associations in data when no existing ground truth is known (i.e. no observations are tagged). Within this broad category of methods, the most commonly used are clustering techniques, which algorithmically detect what are the natural groupings that exist in a data set. For example, clustering can be used to programmatically learn the natural customer segments in a company's user base. Other unsupervised methods for mining underlying characteristics include principal component analysis, hidden Markov models, topic models, and more.

Not all machine learning methods fit neatly into the above two categories. For example, collaborative filtering is a type of recommendations algorithm with elements related to both supervised and unsupervised learning. Contextual bandits are a twist on supervised learning where predictions get adaptively modified on-the-fly using live feedback.

This wide-ranging breadth of machine learning techniques comprises an important part of the data science toolbox. It is up to the data scientist to figure out which tool to use in different circumstances (as well as how to use the tool correctly) in order to solve analytically open-ended problems.

### Data Mining and Data Science {-}

Raw data can be unstructured and messy, with information coming from disparate data sources, mismatched or missing records, and a slew of other tricky issues. Data munging is a term to describe the data wrangling to bring together data into cohesive views, as well as the janitorial work of cleaning up data so that it is polished and ready for downstream usage. This requires good pattern-recognition sense and clever hacking skills to merge and transform masses of database-level information. If not properly done, dirty data can obfuscate the 'truth' hidden in the data set and completely mislead results. Thus, any data scientist must be skillful and nimble at data munging in order to have accurate, usable data before applying more sophisticated analytical tactics.

## Summary {-}
While Data Science is a vast subject, being an aggregate of several technologies and disciplines, it is possible to acquire these skills with the right approach. In the end, Data Science is a very robust field that best fits people who have a knack for experimentation and problem-solving. With a large number of applications, Data Science has become the most versatile career.

## Exercises  {-}
1. Explain Data Science in your own term. What is the relation between data science and Data mining, Data Science with Data Analytics? 
2. What a Data Scientist do? What skills a data SCientist should have?
3. How do you interpret the saying that "Data Scientist is a detective. An investigation into datasets may not result in a plausible conclusion"? How do you explain the value of doing a data science project if your efforts resulted in an unwelcome result? 




