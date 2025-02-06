# DS4002Project1

## Impact of Brand Recognition on Movie Theater Reviews
Group Name: C.E.L.L
Christopher Joseph (leader), Yiyang Bu, Elle Park
DS 4002 Prototyping (S25)
January 30th, 2025

### Motivation: 
Online word-of-mouth reviews significantly influence a business’s sales and success, especially for a small local chain. Positive online reviews not only encourage consumers to visit a business’s website but also optimize a business’s ranking in Google Search Engine results and improve brand trust. Reviews play a crucial role in reducing consumers’ initial uncertainty, swaying their buying behavior, and increasing confidence in a business’s products. Therefore, businesses must prioritize the frequency and quality of customer reviews as a strategic investment. After the pandemic, consumer interactions with online reviews increased by 50% from pre-pandemic levels [1]. Additionally, when consumers seek a completely new business, Google Reviews is the most popular site to view and submit reviews, with 65% of consumers utilizing Google Reviews to make their decisions. 

According to R. Pike [2], brand recognition plays a critical role in consumer perception, as it will be the consumer’s understanding, memory, and identification with a business. Through marketing behavior and information transmission, essentially with online reviews, businesses can help consumers establish meaningful brand associations. And in the business selection state, strong brand recognition is a factor that can effectively improve consumers’ perception of product quality. When writing reviews in the post-purchase stage, consumers build a sentimental understanding of the brand, recognize the brand value, and, most importantly, promote repeat purchasing behavior. 

### Goal Statement:  
This project aims to determine if brand recognition correlates with more positive reviews for larger movie theater chains than smaller chains.

### Research Question: 
Does the size of the movie theater chain influence the positivity of their reviews? More specifically, are movie theaters rated more positively if they have larger brand recognition?

### Modeling Approach: 
The text data will be extracted from Google Reviews with a third-party website scraper [3]. The compiled dataset will mainly include review content and star ratings. To approach the research question, the group will conduct Exploratory Data Analysis (EDA) and sentiment analysis, utilizing the VADER package in Python [4]. More specifically, average ratings and review sentiments will be compared across the three selected movie theaters, Violet Crown [5], Alamo Drafthouse [6], and Regal [7], to determine the impact of brand recognition. The results of the sentiment analysis will help determine the attitudes (either positive or negative) of the reviews about the movie theaters. 




### References:
[1]       Zhou. Lichun, “Research on Quantitative Model of Brand Recognition Based on Sentiment Analysis of Big Data,” Frontiers in Psychology, vol 13, May, 2022. [Abstract]. Available:https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2022.915443/full#h5. [Accessed Jan. 30, 2025].
[2]        R. Pike, “What Every Small Business Needs To Know About Google Reviews,” forbes.com, Jul. 27, 2022. [Online]. Available: https://www.forbes.com/sites/allbusiness/2022/07/27/what-every-small-business-needs-to-know-about-google-reviews/ [Accessed Jan. 30, 2022].
[3]	Phantom - GMB Audit Tool Extension, https://chromewebstore.google.com/detail/phantom-gmb-audit-tool/ppjhoknoadhakhhdbbjjinlopefmehik [Accessed Jan. 30, 2025]
[4]	“Welcome to VaderSentiment’s documentation! — VaderSentiment 3.3.1 documentation,” vadersentiment.readthedocs.io. https://vadersentiment.readthedocs.io/en/latest/ [Accessed Jan. 30, 2025]
[5]	Violet Crown Google Reviews, https://www.google.com/search?q=violet+crown+cville&rlz=1C1UEAD_enUS1106US1106&oq=viole&gs_lcrp=EgZjaHJvbWUqDwgAEAAYQxjjAhiABBiKBTIPCAAQABhDGOMCGIAEGIoFMhUIARAuGEMYrwEYxwEYgAQYigUYjgUyBggCEEUYOzIKCAMQABixAxiABDIPCAQQABhDGLEDGIAEGIoFMgwIBRAAGEMYgAQYigUyFQgGEC4YQxivARjHARixAxiABBiKBTIHCAcQABiABNIBBzY3NWowajmoAgCwAgE&sourceid=chrome&ie=UTF-8#lrd=0x89b386244acddd39:0xaecfb9561d2265e,1 [Accessed Jan. 30, 2025]
[6]	Regal Google Reviews, https://www.google.com/search?q=regal+cville&rlz=1C1UEAD_enUS1106US1106&oq=regal+&gs_lcrp=EgZjaHJvbWUqBggAEEUYOzIGCAAQRRg7MhAIARAuGK8BGMcBGIAEGI4FMg0IAhAAGLEDGMkDGIAEMgoIAxAAGLEDGIAEMg0IBBAuGK8BGMcBGIAEMg0IBRAAGJIDGIAEGIoFMg0IBhAuGK8BGMcBGIAEMgYIBxBFGD3SAQc3NzFqMGo3qAIAsAIA&sourceid=chrome&ie=UTF-8#lrd=0x89b387c1e467a4fd:0xec93d7192842e3ba,1 [Accessed Jan. 30, 2025]
[7]	Alamo Drafthouse Google Reviews, https://www.google.com/search?q=alamo+cville&rlz=1C1UEAD_enUS1106US1106&oq=alamo&gs_lcrp=EgZjaHJvbWUqBggAEEUYOzIGCAAQRRg7Mg8IARBFGDkYsQMYyQMYgAQyCggCEAAYsQMYgAQyEwgDEC4YrwEYxwEYkgMYgAQYjgUyBwgEEAAYgAQyEAgFEC4YxwEYsQMY0QMYgAQyBwgGEAAYgAQyBggHEEUYPdIBCDExNzdqMGo3qAIAsAIA&sourceid=chrome&ie=UTF-8#lrd=0x89b38672e1a713bb:0x2359775c4f8044bb,1 [Accessed Jan. 30, 2025]
