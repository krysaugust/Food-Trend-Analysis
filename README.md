# Food-Trend-Analysis
Social Media Analysis of Future Food Trend

### Background
Detection of emerging food trend can translate into great business opportunities. The most potentially valuable and real-time source of it is the posts on social media. To obtain early-mover advantage, food companies and retailers want to see the rise and fall of certain categories of food before competitors do, and launch new products or deliver specific categories to target group. 

### Topic Modeling
Data used in this project is over 4 million Facebook posts from 2011 to 2015. Considering the calculation capability and time consumption of code, and because the most recent trend is critical to predict future trend, I conducted topic modeling of 2015 and examined frequent words used in 2015 instead of the five years.  I applied Latent Dirichlet Allocation (LDA) method because this probabilistic model decomposes document corpus into 2 row-rank matrixes: document-topic distribution and topic-word distribution, enabling me to both check the frequent topics and relevant words within each topic. From the result, in 2015, facebook users mentions more about "cup cake", "cheese", "chicken", "cream", "chocolate" and "sauce". 


### Time Series Analysis
I also defined a function in R to return the monthly percent of posts containing the term I want to explore the in all posts. Percentage is calculated here because it is a more reasonable metric compared to the absolute value, regardless of the size difference in posts during different time periods. I used the forecast and stats packages in R to visualize the yearly and seasonal appearance probability of chosen terms, supporting the trend prediction. 
