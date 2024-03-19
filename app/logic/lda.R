library(topicmodels)
library(tidytext)
library(LDAvis)
library(quanteda)
library(readxl)
library(Rmpfr)
library(servr)
#str(llis.display)

root <- getwd()
# path_data <- paste(root, "/", "data1", sep="")
# file_path <- "C:/Users/LENOVO/Downloads/incidents_description.xlsx"
path_data <- paste(root, "/", "data1", sep="")
file_path <- paste(path_data, "/incidents_description.xlsx", sep="")
description <- read_excel(file_path)
#desc <- data$description

sotu_corpus <- corpus(description$Description)
corp = corpus_reshape(sotu_corpus, to = "sentences")
#corp = corpus_reshape(data_corpus_inaugural, to = "paragraphs")
dfm = dfm(corp, remove_punct=T, remove=stopwords("english"))
dfm = dfm_trim(dfm, min_docfreq = 5)

dtm = convert(dfm, to = "topicmodels")
set.seed(1)

harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

m = LDA(dtm, method = "Gibbs", k = 6,  control = list(alpha = 0.1))
m

terms(m, 5)

dtm = dtm[slam::row_sums(dtm) > 0, ]
phi = as.matrix(posterior(m)$terms)
theta <- as.matrix(posterior(m)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
          doc.length = doc.length, term.frequency = term.freq)
#widget <- createWidget(json)
route <- paste(path_data,"/Topic_modelling", sep="")
serVis(json, out.dir = route, open.browser = FALSE)

serVis(json, out.dir = 'Topic_modelling', open.browser = FALSE)


# seqk <- seq(2, 50, 1)
# burnin <- 100
# iter <- 100
# keep <- 50
# system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(dtm, k = k,
#                                                                      method = "Gibbs",control = list(burnin = burnin,
#                                                                                                      iter = iter, keep = keep) )))
# logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
# hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
# ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
#   theme(text = element_text(family= NULL),
#         axis.title.y=element_text(vjust=1, size=16),
#         axis.title.x=element_text(vjust=-.5, size=16),
#         axis.text=element_text(size=16),
#         plot.title=element_text(size=20)) +
#   xlab('Number of Topics') +
#   ylab('Harmonic Mean') +
#   annotate("text", x = 25, y = -80000, label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
#   ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of NEN LLIS", atop(italic("How many distinct topics in the abstracts?"), ""))))
# ldaplot
