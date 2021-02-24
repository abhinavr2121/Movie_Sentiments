# LOAD DEPENDENCIES
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(sentimentr)
library(tokenizers)

# DEFINE THE GLOBAL NUMBER OF POINTS IN THE PLOT TO MEASURE
N <- 10

# READ THE DATA
metadata <- read.csv('movies_metadata.csv', stringsAsFactors = F)
data <- read.csv('plots.csv', stringsAsFactors = F) %>% as_tibble()

# TOKENIZE EACH SENTENCE OF THE DATA AND CLEAN IT
trimmed.tokenized.data <- tokenizers::tokenize_sentences(data$Plot) %>% lapply(trimws)

# 'sentiments' is an empty list that will contain the numerical sentiment data 
sentiments <- vector('list', length(trimmed.tokenized.data))

# STEP 01: PERFORM SENTIMENT ANALYSIS ON SENTENCES
for(i in 1 : length(trimmed.tokenized.data)) {
  print(paste(round(100 * i / length(sentiments), 2), '%', sep = ''))
  sent <- sentimentr::sentiment(trimmed.tokenized.data[[i]])
  sentiments[[i]] <- sent$sentiment
}

# STEP 02: NORMALIZE ALL SENTIMENT VECTORS TO N POINTS
# 02.1: find the ten most impactful (positive and negative) points of the plot
# 02.2: remove additional points
# 02.3: if there are fewer than ten points, mark them as unusable for future removal
# 02.4: make sure there are no duplicated points (oddities, mostly)

unusable.i <- c() # this variable stores the indices of unusable plots
large.sentiments <- sentiments[which(lengths(sentiments) >= N)]
abridged.sentiments <- large.sentiments
for (i in 1 : length(large.sentiments)) {
  print(paste(i, ' of ', length(large.sentiments)))
  pass <- c()
  # order all the points by magnitude
  ordered.sentiments <- large.sentiments[[i]][order(abs(large.sentiments[[i]]), decreasing = T)] %>%
    unique()
  # subset the list of sentiment numbers by N
  max.list <- ordered.sentiments[1 : N]
  
  # add the relevant sentiments to the pass variable
  for(j in 1 : length(large.sentiments[[i]])) {
    if(large.sentiments[[i]][j] %in% max.list) {
      pass <- append(pass, large.sentiments[[i]][j])
    }
  }
  
  # zeros are not indicative of meaningful emotion
  # if zeroes are captured in the N most important sentiments, they must be removed
  if(0 %in% pass) {
    while(length(pass) > N) {
      zero.index <- which(pass == 0)[1]
      if(is.na(zero.index)) {
        break
      }
      pass <- pass[pass != 0]
      if(length(pass) < N) {
        # keep the zeros if, without it, there are too few sentiment points
        pass <- append(pass, 0, after = zero.index - 1)
      }
    }
  }
  # checking and correcting for duplicates
  if(length(pass) > N) {
    print('Duplicate in data.')
    pass <- unique(pass)
  }
  
  # dumping inadequate plotlines to the unusable pile
  while(length(pass) < N) {
    hopeless.i <- append(unusable.i, i)
    pass <- append(pass, 0)
  }
  
  # add the sentiment data to 'abridged.sentiments'
  abridged.sentiments[[i]] <- pass
}

# remove any unusable sentiments
real.sentiments <- abridged.sentiments[which(!(abridged.sentiments %in% unusable.i))]
data <- data[usable.sentiments, ]

# STEP 03: BRING REVENUES INTO THE PICTURE
# 03.1: check if the title in the sentiments dataset matches any in the metadata set
# 03.2: choose the maximum revenue if there are multiple matches (for remakes)
# 03.3: remove any NA and negative/zero values
# 03.4: apply max/min scaling on the annual revenues
# 03.5: weight the sentiments according to the scaled values

revs <- c()
for(i in 1 : length(data$Release.Year)) {
  # check if title matches
  if(data$Title[i] %in% metadata$title) {
    value <- max(metadata$revenue[which(metadata$title == data$Title[i])])
    revs <- append(revs, value)
  }
  else {
    # append NA values if there are no matches
    revs <- append(revs, NA)
  }
}
data$Revenue <- revs

# remove rows containing NA revenues
usable.indices <- which(!is.na(data$Revenue))
non.na <- data[usable.indices, ]
non.na.sentiments <- abridged.sentiments[usable.indices]
# remove non-positive values
usable.indices <- which(non.na$Revenue > 0)
non.na.non.zero <- non.na[usable.indices, ]
non.na.non.zero.sentiments <- non.na.sentiments[usable.indices]
# scale the revenues (the largest being set to 1.0)
non.na.non.zero$AnnualRevenue <- non.na.non.zero$Revenue / (2018 - non.na.non.zero$Release.Year)
non.na.non.zero$Weight <- non.na.non.zero$AnnualRevenue / max(non.na.non.zero$AnnualRevenue)

# weight each collection of sentiment points by the scaled revenue weights
modified.sentiments <- non.na.non.zero.sentiments
for(row in 1 : length(modified.sentiments)) {
  modified.sentiments[[row]] <- modified.sentiments[[row]] * non.na.non.zero$Weight[row]
}

# STEP 04: FIND THE AVERAGE PLOTLINE AND THE WEIGHTED AVERAGE PLOTLINE
# 04.1: calculate the average of each of the N weighted points
# 04.2: calculate the average of each of the N points

# finding the weighted average plotline on a point-by-point basis
# all of the rows have N points, so counter will contain the final point averages
counter <- rep(0, 10)
for(row in 1 : length(modified.sentiments)) {
  for(column in 1 : N) {
    counter[column] <- counter[column] + modified.sentiments[[row]][column]
  }
}
# place the results in a dataframe 'final.df'
final <- counter / length(modified.sentiments)
final.df <- data.frame(x = seq(10, 100, N), y = final)

# repeat the above for the non-weighted points
counter <- rep(0, 10)
for(row in 1 : length(abridged.sentiments)) {
  for(column in 1 : N) {
    counter[column] <- counter[column] + abridged.sentiments[[row]][column]
  }
}
# place the results in a dataframe 'final2.df'
final2 <- counter / length(abridged.sentiments)
final2.df <- data.frame(x = seq(10, 100, N), y = final2)

# STEP 05: OUTPUT THE RESULTS
# 05.1: define custom ggplot2 theme
# 05.2: weighted average plotline
# 05.3: normally averaged plotline
# 05.4: Cinderella (example)
# 05.5: Twelve Angry Men (example)

theme_blue <- theme(
  title = element_text(color = 'black', face = 'bold'),
  panel.background = element_rect(fill = '#F0F5F9'),
  plot.background = element_rect(fill = '#F0F5F9'),
  axis.text = element_text(color = '#3E4E50', face = 'bold'),
  axis.line = element_line(colour = '#3E4E50'),
  plot.subtitle = element_text(color = 'black', face = 'plain'),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank()
)

theme_blue_axes <- theme(
  title = element_text(color = 'black', face = 'bold'),
  panel.background = element_rect(fill = '#F0F5F9'),
  plot.background = element_rect(fill = '#F0F5F9'),
  axis.text = element_text(color = '#3E4E50', face = 'bold'),
  axis.line = element_line(colour = '#3E4E50'),
  plot.subtitle = element_text(color = 'black', face = 'plain')
)

p1 <- ggplot(final.df, aes(x, y)) + 
  geom_line(color = '#3E4E50', size = 1, linetype = 'longdash') +
  geom_point(size = 3, color = '#3E4E50') +
  theme_hc() +
  labs(title = 'Averaged Movie Plot Curves',
       subtitle = '(weighted by average annual revenue)') +
  xlab('% Movie Complete') +
  xlim(c(0, 100)) +
  ylab('Preferred Sentiment') +
  ylim(c(-0.003, 0)) + # adjust scale to demonstrate shape
  theme_blue
p1

p2 <- ggplot(final2.df, aes(x, y)) + 
  geom_line(color = '#3E4E50', size = 1, linetype = 'longdash') +
  geom_point(size = 3, color = '#3E4E50') +
  theme_hc() +
  labs(title = 'Average Movie Plot Curves',
          subtitle = '(unweighted average)') +
  xlab('% Movie Complete') +
  xlim(c(0, 100)) +
  ylab('Preferred Sentiment') +
  theme_blue
p2

# CINDERELLA EXAMPLE
test.df <- data.frame(x = 100 * ((1 : 10) / 10), y = non.na.non.zero.sentiments[[4]])
p3 <- ggplot(test.df, aes(x, y)) +
  geom_line(color = '#3E4E50', size = 1, linetype = 'longdash') +
  geom_point(size = 3, color = '#3E4E50') +
  theme_hc() +
  labs(title = 'Cinderella (1950) Plot Curve',
       subtitle = '(74 minutes)') +
  xlab('% Movie Complete') +
  scale_y_continuous(breaks = seq(-0.75, 0.75, 0.25), labels = seq(-0.75, 0.75, 0.25)) +
  xlim(c(0, 100)) +
  ylab('Emotion') +
  geom_label(aes(x = 06, y = -0.30), label = '1') +
  geom_label(aes(x = 46, y = 0.75), label = '2') +
  geom_label(aes(x = 75, y = -0.30), label = '3') +
  geom_label(aes(x = 96, y = 0.70), label = '4') +
  theme_blue_axes
p3

test2.df <- data.frame(x = 100 * ((1 : 10) / 10), y = non.na.non.zero.sentiments[[281]])
p4 <- ggplot(test2.df, aes(x, y)) +
  geom_line(color = '#3E4E50', size = 1, linetype = 'longdash') +
  geom_point(size = 3, color = '#3E4E50') +
  theme_hc() +
  labs(title = '12 Angry Men (1957) Plot Curve',
       subtitle = '(96 minutes)') +
  xlab('% Movie Complete') +
  xlim(c(0, 100)) +
  ylab('Emotion') +
  geom_label(x = 6, y = 0.5, label = '1') +
  geom_label(x = 25, y = -0.65, label = '2') +
  geom_label(x = 37, y = 0.48, label = '3') +
  geom_label(x = 46, y = -0.55, label = '4') +
  geom_label(x = 94, y = -0.42, label = '5') +
  geom_label(x = 96, y = 0.40, label = '6') +
  theme_blue_axes
p4
