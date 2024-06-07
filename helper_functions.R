# Helper functions for LDA analysis

################################################################################
# clean_text: removes unwanted clauses, punctuation, common words, etc. from   #
#     single string using replace and tree-tagger                              #
# in:  x - string of text for cleaning                                         #
#      singularize - flag to singularize words in unknown_plurals.csv          #
# out: y - cleaned list of words                                               #
################################################################################

clean_text = function(x, singularize) {

    if(missing(singularize)) { singularize = TRUE }

    # Remove unncessary text idenitified by punctuation
    x = str_replace_all(x, regex('\\(c\\)', ignore_case=TRUE), '')
    x = str_replace_all(x, regex('\\(Source: journal abstract\\)', ignore_case=TRUE), '')
    x = str_replace_all(x, regex('\\(PsycINFO Database Record  \\d+ APA, all rights reserved\\)', ignore_case=TRUE), '')

    # Remove punctuation
    x = str_replace_all(x, ',', '')
    x = str_replace_all(x, '\\.', '')
    x = str_replace_all(x, '\\?', '')
    x = str_replace_all(x, '!', '')
    x = str_replace_all(x, ':', '')
    x = str_replace_all(x, ';', '')
    x = str_replace_all(x, '\\(', '')
    x = str_replace_all(x, '\\)', '')
    x = str_replace_all(x, '\\[', '')
    x = str_replace_all(x, '\\]', '')
    x = str_replace_all(x, '\'', '')
    x = str_replace_all(x, '\"', '')
    x = str_replace_all(x, '&', '')
    x = str_replace_all(x, '%', '')
    x = str_replace_all(x, '=', ' ')
    x = str_replace_all(x, '\U2019', '')   # apostrophe
    x = str_replace_all(x, '\U2018', '')   # apostrophe
    x = str_replace_all(x, '\U201C', '')   # left quotation
    x = str_replace_all(x, '\U201D', '')   # right quotation
    x = str_replace_all(x, '\U2026', '')   # ellipsis
    x = str_replace_all(x, '\U00AE', '')   # trademark
    x = str_replace_all(x, '\U00b2', '')   # squared
    x = str_replace_all(x, regex(' i+ ', ignore_case=TRUE), ' ')   # roman numerals
    x = str_replace_all(x, ' iv ', ' ')
    x = str_replace_all(x, '/', ' ')       # replace with space
    x = str_replace_all(x, '-', ' ')       # replace with space
    x = str_replace_all(x, '\U2212', ' ')  # minus
    x = str_replace_all(x, '\U2010', ' ')  # hyphen
    x = str_replace_all(x, '\U2011', ' ')  # non-breaking hyphen
    x = str_replace_all(x, '\U2012', ' ')  # figure dash
    x = str_replace_all(x, '\U2013', ' ')  # en dash
    x = str_replace_all(x, '\U2014', ' ')  # em dash

    # Remove numbers
    x = str_replace_all(x, '\\d+', '')

    # Replace acronyms (don't love the space hacks, but want it before splitting
    # and tree-tagger), keep case for this step
    acronyms = read.csv('~/Fellowship/LDA/acronyms.csv', header=FALSE)
    for(i in 1:nrow(acronyms)) {
        # first word
        x = str_replace_all(x,
                            paste('^', acronyms[i, 1], ' ', sep=''),
                            paste(acronyms[i, 2], ' ', sep=''))
        # middle words
        x = str_replace_all(x,
                            paste(' ', acronyms[i, 1], ' ', sep=''),
                            paste(' ', acronyms[i, 2], ' ', sep=''))
        # last word
        x = str_replace_all(x,
                            paste(' ', acronyms[i, 1], '$', sep=''),
                            paste(' ', acronyms[i, 2], sep='')) }

    # Replace words (converts to lower case at this step)
    words = read.csv('~/Fellowship/LDA/replacements.csv', header=FALSE)
    for(i in 1:nrow(words)) {
        # first word
        x = str_replace_all(tolower(x),
                            paste('^', words[i, 1], ' ', sep=''),
                            paste(words[i, 2], ' ', sep=''))
        # middle words
        x = str_replace_all(tolower(x),
                            paste(' ', words[i, 1], ' ', sep=''),
                            paste(' ', words[i, 2], ' ', sep=''))
        # last word
        x = str_replace_all(tolower(x),
                            paste(' ', words[i, 1], '$', sep=''),
                            paste(' ', words[i, 2], sep='')) }

    # Convert to list of words
    x = t(str_split(x, ' ', simplify=TRUE))
    # Remove empty entries
    x = x[x != '']
    # Write to temporary file
    write(x, 'tmp.in')

    # Run tree-tagger
    system(paste(tt.ex.path, tt.par.path, 'tmp.in tmp.out -lemma'))
    # Read in results
    y = read.csv('tmp.out', header=FALSE, sep='\t')
    # Convert all to lower case
    y$V2 = tolower(y$V2)
    # Delete temporary files
    system('rm tmp.in tmp.out')

    # Words not in tree-tagger dictionary will not get singularized. Need to pass
    # through once to generate a list of such words and then pare it down to those
    # that need to be singularized (e.g. microagressions)
    if(singularize) {
        # Singularize unknown plurals
        plurals = read.csv('~/Fellowship/LDA/unknown_plurals.csv', header=FALSE)
        for(i in 1:length(x)) {
            if(x[i] %in% plurals$V1) { y$V2[i] = str_sub(x[i], start=1, end=-2) } }
    } else {
        # Make list of unknown words with 's' at end for singularizing
        plurals = unique(tolower(x[y$V2 == '<unknown>' & str_sub(x, start=-1) == 's']))
        write.table(plurals, '~/Fellowship/LDA/unknown_plurals_tmp.csv', append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE) }

    # Replace unknown words with their original
    y$V2[y$V2 == '<unknown>'] = tolower(x[y$V2 == '<unknown>'])
    # Remove articles, prepositions, numbers, etc.
    y = y %>% dplyr::filter(V1 != 'TO',
                            V1 != 'IN',
                            V1 != 'IN/that',
                            V1 != 'CD',
                            V1 != 'CC',
                            V1 != 'RB',
                            V1 != 'EX',
                            V1 != 'DT',
                            V1 != 'PP',
                            V1 != 'PP$',
                            V1 != 'WRB',
                            V1 != ':')
    # other candidates: WP, WDT
    # Remove single letters
    y = y %>% dplyr::filter(nchar(V2) > 1)

    # Remove specific words
    words = read.csv('~/Fellowship/LDA/word_exclusion_list.csv', header=FALSE)
    for(word in words$V1) {
        y = y %>% dplyr::filter(tolower(V2) != tolower(word)) }

    return(y$V2) }

################################################################################
# write_word_list: write df of words by text object to xlsx file               #
# in:  x - dataframe of words (row = words, columns = text objects)            #
#      fn - filename to write to                                               #
################################################################################

write_word_list = function(x, fn) {

    n = max(sapply(x, length))
    x = lapply(abstract_word_list, pad_with_empty, n=n)
    x = as.data.frame(x, col.names=1:length(x))
    write.xlsx(x, fn, col.names=FALSE) }

################################################################################
# pad_with_empty:  fills vector with empty entries                             #
# in:  x - vector of strings                                                   #
#      n - desired length of vector
# out: x - vector of strings padded to length n                                #
################################################################################

pad_with_empty = function(x, n) {

    if(length(x) < n) {
        x[(length(x) + 1) : n] = '' }
    return(x) }

################################################################################
# convert_to_tidy: converts word by text object df to word per row tibble      #
# in:  x - dataframe of words (row = words, columns = text objects)            #
# out: y - tibble                                                              #
################################################################################

convert_to_tidy = function(x) {

    n.abs = length(x)
    y = tibble(abstract=integer(), word=character())
    for(i in 1:n.abs) {
        n.word = length(x[[i]])
        y = y %>% add_row(abstract=rep(i, n.word), word=x[[i]]) }
    # Add count
    y = y %>% add_count(word, abstract)
    # Reduce to one entry per word
    y = y %>% distinct()
    return(y) }

################################################################################
# create_wordcloud: creates wordcloud for topic from LDA output                #
# in:  x - LDA model                                                           #
#      k - topic number                                                        #
#      t - threshold                                                           #
# out: wc - word cloud                                                         #
################################################################################

create_wordcloud = function(x, k, t) {

    # Convert output to tibble
    y = tidy(x, matrix='beta')
    # Change column names for use with wordcloud2
    names(y)[2:3] = c('word', 'freq')
    # Convert beta to frequency (not sure this is necessary)
    y$freq = round(y$freq / t)
    # Generate word cloud
    wc = y %>%
        dplyr::filter(topic == k, freq > 1) %>%
        dplyr::select(word, freq) %>%
        wordcloud2()

    return(wc)

################################################################################
# plot_topic_charts: creates pie charts showing topic contrib. to each doc.    #
# in:  x - LDA model                                                           #
# out: g - pie chart plot                                                      #
################################################################################

plot_topic_charts = function(x) {

    # Convert output to tibble
    y = tidy(x, matrix='gamma')
    # Define rows and cols for plot
    y$row = NA
    y$col = NA
    y$document = as.numeric(y$document)
    for(i in 1:16) {
        y$row[as.numeric(y$document) > (i-1) * 17 & as.numeric(y$document) <= i * 17] = i
        y$col[as.numeric(y$document) > (i-1) * 17 & as.numeric(y$document) <= i * 17] = 1:17 }
    y$row[as.numeric(y$document) > 16 * 17] = 17
    y$col[as.numeric(y$document) > 16 * 17] = 1:15
    # Convert document and topics to factors
    y$document = as.factor(y$document)
    y$topic = as.factor(y$topic)

    # Plot grid of pie charts
    g = y %>% ggplot(aes(x=" ", y=gamma, group=topic, colour=topic, fill=topic)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start=0) +
        facet_grid(rows=vars(row), col=vars(col)) +theme_void()

    # Summarize topics in table
    cutoffs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.01)
    n = length(unique(y$topic))
    z = matrix(NA, nrow=n, ncol=length(cutoffs) - 1)
    for(i in 1:(length(cutoffs) - 1)) {
        for(j in 1:n) {
            z[j, i] = sum(y$gamma >= cutoffs[i] & y$gamma < cutoffs[i + 1] & y$topic == j) } }
    print(z)

    return(g) }

################################################################################
# topic_summary: calculates prevalence and (weighted) average year for topics  #
# in:  x - LDA model                                                           #
#      years - year of publication for each document                           #
# out: summary table with prevalence and average year of publication           #
################################################################################

topic_summary = function(x, years) {

    # Convert output to tibble
    y = tidy(x, matrix='gamma')

    # calculate prevalence for each topic
    prevalence = sapply(unique(y$topic),
        FUN = function(z) y %>%
            filter(topic == z) %>%
            pull(gamma) %>%
            sum(),
        simplify=TRUE)

    # calculate average year
    avg_yr = sapply(unique(y$topic),
        FUN = function(z) y %>%
            filter(topic == z) %>%
            pull(gamma) %>%
            sum(. * years),
        simplify=TRUE)

    # Normalize
    avg_yr = avg_yr / prevalence
    prevalence = prevalence / length(unique(y$document))

    # Compile into data frame
    summary = data.frame(topic=unique(y$topic), avg_yr=avg_yr, prevalence=prevalence)

    return(summary)

}

################################################################################
# print message or blank line if empty                                         #
################################################################################

print_msg = function(msg) {

    if(missing(msg)) {
        cat('\n')
    } else {
        cat(paste(msg, '\n', sep='')) } }

