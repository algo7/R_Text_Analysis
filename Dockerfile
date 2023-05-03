FROM r-base:4.3.0

COPY ./Full_Code/SentimentAnalysis_Individual_Entry_Advanced.R .

CMD ["Rscript"."./SentimentAnalysis_Individual_Entry_Advanced.R"]