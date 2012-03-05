
###################################################################
# Artist filtering with tag balancing
# Version: 1.0
# Data: 29/02/2012
###################################################################

artist_filtering_with_tag_balancing <- function(input, nfolds=3, minpertag=5){
	
	all_folds_artist <- vector("list", nfolds)

	# Initiate a list that will store the folds
	all_folds <- list()
	for(i in 1:nfolds){
		all_folds[[i]] <- data.frame(NULL,NULL,NULL)
	}

	# Read the data set that will be splitted in 'n' folds (nfolds)
	ds <- read.csv(input, header = F, sep = "\t")
	
	# In order to keep the balance of the tags among the folds, we remove tags whose frequency is smaller than the number of folds
	ds <- remove_tags_less_than_nfolds(ds, nfolds)

	# Print the number of artirsts, songs and tags
	print(paste('Dataset: ', length(unique(ds[,1])), ' artists, ', length(unique(ds[,2])), ' songs and ', length(unique(ds[,3])), ' tags.' ,sep=''))

	# Get all tags
	all_tags <- as.vector(unique(ds[,3]))

	# For each tag computes how many tags we should put in each fold
	all_nelement <- NULL
	for(i in all_tags){
		all_nelement <- c(all_nelement, as.integer(length(ds[ds[,3]==i, 2]) / nfolds))
	}
	names(all_nelement) <- all_tags

	print('GENERATING THE FOLDS')
	for(i in all_tags){ # Make the split of the data set by tag, and fill each fold
		nelement <- all_nelement[i] # Number of tags that should be put in each fold
		ds_by_tag <- ds[ds[,3]==i, ]
		n_tag_each_fold <- rep.int(0, nfolds) # Initiate the variable which will count the number of tags we have currently in each fold

		print(paste('Tag to fill the folds: ', i, sep=''))
		print(paste('Number of tags in each fold: ', nelement, sep=''))

		# Define the order that the artists should be inserted in each fold. We rank the artists using the number of songs and tags related to each artist
		artists_by_tag <- unique(as.vector(ds_by_tag[,1])) # Get the artists

		artist_name <- NULL
		artist_score <- NULL
		aritst_used <- NULL
		for(j in artists_by_tag){
			tmp <- ds[ds[,1]==j, ]
			artist_name <- c(artist_name, as.character(j))
			artist_score <- c(artist_score, (length(tmp[,2]) / length(unique(tmp[,3]))) )
			aritst_used <- c(aritst_used, 0)
		}
		artist_rank <- data.frame(artist_name, artist_score, aritst_used)
		artist_rank <- artist_rank[order(artist_rank[,2], decreasing = T),] # Rank of artists

		# First: insert songs from artists already in the folds, following the rank of artists
		for(j in 1:nfolds){
			artist_in_fold <- unique(as.vector(all_folds_artist[[j]]))

			for(k in artist_in_fold){
				data_of_artist <- ds_by_tag[ds_by_tag[,1]==as.character(k),]

				if(nrow(data_of_artist) > 0){
					all_folds[[j]] <- rbind(all_folds[[j]], data_of_artist)
					n_tag_each_fold[j] <- n_tag_each_fold[j] + nrow(data_of_artist)
					artist_rank[artist_rank[,1]==as.character(k),3] <- 1
				}
			}
		}

		# Second: complete the folds with songs from other artists, following the rank of artists
		j <- 1 # Number of the current fold being fullfilled
		increase <- TRUE # Set the direction to fill the folds (bidirectional - TRUE: 1,2,3,... ; FALSE: ...,3,2,1)
		k <- 1 # Number of the artist
		stop <- FALSE # Stop the process if all folds are filled
		fold_filled <- FALSE # Indicate that all folds are filled
		while((k <= nrow(artist_rank)) & (stop == FALSE)){
			if(artist_rank[k,3]==0){
				if(increase){
					if(n_tag_each_fold[j] < nelement){ # Fill the folds in the direction 1,2,3,... (increase <- TRUE)
						data_of_artist <- ds_by_tag[ds_by_tag[,1]==as.character(artist_rank[k,1]),]
						all_folds[[j]] <- rbind(all_folds[[j]], data_of_artist)
						n_tag_each_fold[j] <- n_tag_each_fold[j] + nrow(data_of_artist)
						all_folds_artist[[j]] <- unique(c(all_folds_artist[[j]], as.character(artist_rank[k,1])))
						artist_rank[k,3] <- 1
						k <- k + 1
						fold_filled <- TRUE
					}
					j <- j + 1
					if(j > nfolds){
						if(fold_filled){
							j <- nfolds
							increase <- FALSE
							fold_filled <- FALSE
						}else{
							stop <- TRUE
						}
					}
				}else{
					if(n_tag_each_fold[j] < nelement){ # Fill the folds in the direction ...,3,2,1 increase <- FALSE
						data_of_artist <- ds_by_tag[ds_by_tag[,1]==as.character(artist_rank[k,1]),]
						all_folds[[j]] <- rbind(all_folds[[j]], data_of_artist)
						n_tag_each_fold[j] <- n_tag_each_fold[j] + nrow(data_of_artist)
						all_folds_artist[[j]] <- unique(c(all_folds_artist[[j]], as.character(artist_rank[k,1])))
						artist_rank[k,3] <- 1
						k <- k + 1
						fold_filled <- TRUE
					}
					j <- j - 1
					if(j < 1){
						if(fold_filled){
							j <- 1
							increase <- TRUE
							fold_filled <- FALSE
						}else{
							stop <- TRUE
						}
					}
				}
			}else{
				k <- k + 1
			}
		}
	}

	print('REMOVING TAGS WITH FEW SONG EXAMPLES')
	# Although the code above tries to balance the number of tags in each fold, for some tags we just have a few song examples in each fold. Sometimes this is not desirable. The code below remove tags from the folds, which have a few song examples (smaller than minpertag)
	tags_with_problem <- NULL
	for(i in all_tags){
		for(k in 1:length(all_folds)){
			all_matches <- as.character(all_folds[[k]][,3]) == as.character(i)
			size_all_matches <- length(all_matches[all_matches==TRUE])
			if(!(size_all_matches >= minpertag)){
				tags_with_problem <- c(tags_with_problem, as.character(i))
			}
		}
	}

	# Remove from the folds the tags with few song examples
	tags_with_problem <- unique(tags_with_problem)
	for(i in tags_with_problem){
		for(k in 1:length(all_folds)){
			tmp <- all_folds[[k]]
			all_folds[[k]] <- tmp[as.character(tmp[,3])!=as.character(i),] 
		}
	}

	# Save the list of tags with few song examples
	write.table(as.data.frame(tags_with_problem), file = 'tags_with_few_examples.txt', row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")

	print('COMPUTING STATISTICS AND SAVING THE FOLDS')
	# Compute statistics for each fold and save the folds, the songNames and the tags vocabulary in their respective files
	songNames <- NULL
	vocab <- NULL
	for(k in 1:length(all_folds)){
		songNames <- c(songNames, as.character(all_folds[[k]][,2]))
		vocab <- c(vocab, as.character(all_folds[[k]][,3]))

		# Save each fold in a file
		write.table(all_folds[[k]][,c(1,2,3)], file = paste('dataset_fold_',k,'.csv',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")

		# Save the statistics in their respective files
		f_tmp <- all_folds[[k]]
		n_artist <- length(unique(f_tmp[,1]))
		n_song <- length(unique(f_tmp[,2]))
		song_by_artist_tmp <- table(as.vector(f_tmp[,1]))
		song_by_artist <- data.frame(names(song_by_artist_tmp), as.numeric(song_by_artist_tmp))

		artist_by_genre_tmp <- unique(f_tmp[,c(1,3)])
		artist_by_genre_tmp <- table(as.vector(artist_by_genre_tmp[,2]))
		artist_by_genre <- data.frame(names(artist_by_genre_tmp), as.numeric(artist_by_genre_tmp))

		write.table('-> General Statistics', file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "")
		write.table(paste('\nNumber of artists: ',n_artist,sep=''), append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "")
		write.table(paste('Number of songs: ',n_song,sep=''), append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "")
		write.table('\nNumber of songs per artist:', append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "")
		write.table(song_by_artist, append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ": ")
		write.table('\nNumber of artists per genre:', append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "")
		write.table(artist_by_genre, append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ": ")
		write.table('\n\n-> Statistics Per Tag', append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "")


		# Save the statistics by tag in their respective files
		tags.ok <- setdiff(all_tags, tags_with_problem)
		for(z in tags.ok){
			f_tmp.genre <- f_tmp[f_tmp[,3]==z, ]
			n_artist <- length(unique(f_tmp.genre[,1]))
			n_song <- length(unique(f_tmp.genre[,2]))
			song_by_artist_tmp <- table(as.vector(f_tmp.genre[,1]))
			song_by_artist <- data.frame(names(song_by_artist_tmp), as.numeric(song_by_artist_tmp))

			write.table(paste('\nStatistics for ',z,sep=''), append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "")
			write.table(paste('Size for the fold: ',all_nelement[z],' songs',sep=''), append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "")
			write.table(paste('Number of artists: ',n_artist,sep=''), append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "")
			write.table(paste('Number of songs: ',n_song,sep=''), append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "")
			write.table('Number of songs per artist:', append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "")
			write.table(song_by_artist, append = T, file = paste('statistic_fold_',k,'.txt',sep=''), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ": ")
		}
	}

	# Save the song names
	songNames <- unique(songNames)
	write.table(as.data.frame(songNames), file = 'songFiles.txt', row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")

	# Save the tags vocabulary
	vocab <- unique(vocab)
	write.table(as.data.frame(vocab), file = 'tagVocabulary.txt', row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
}


# In order to keep the balance of the tags among the folds, we remove tags whose frequency is smaller than the number of folds
remove_tags_less_than_nfolds <- function(dataset, nfolds){
	tags <- as.vector(unique(dataset[,3])) # A list of unique tags

	all_nelement <- NULL
	for(i in tags){ # For each tag computes how many tags we should put in each fold. For tags whose frequency is smaller than the number of folds, we assign the value zero
		all_nelement <- c(all_nelement, as.integer(length(dataset[dataset[,3]==i, 2]) / nfolds))
	}
	names(all_nelement) <- tags

	tags_ok <- names(all_nelement[all_nelement > 0]) # Remove tags whose frequency is smaller than the number of folds

	res <- NULL
	for(i in tags_ok){ # Retrieve and return tags whose frequency is higher than the number of folds
		res <- rbind(res, dataset[dataset[,3]==i,])
	}

	res
}

