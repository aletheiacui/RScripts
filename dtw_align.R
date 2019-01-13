# This is a script that applies Dynamic Time Warping to a directory
# of recorded sentences of the exact same utterance. 
# This scrpit output Praat TextGrids of alignment from DTW.

# To run this script, you'll need:
#   -a reference sentence
#   -the manually labeled TextGrid of the reference sentence
#   -a directory of sentences of the same utterance
#   -the write.TextGrid function from: https://rdrr.io/github/jpellman/textgridR/src/R/write.TextGrid.R

library(tuneR)
library(dtw)
library(textgRid)

freq = 11025

sent_dir = "sentences"

ref = readWave('reference.wav')
ref = downsample(ref, freq)
s1 <- ref@left
s1 <- s1 / 2^(ref@bit - 1)

tg <- TextGrid('reference.TextGrid')
new_tg <- TextGrid('reference.TextGrid')

wav_files = list.files(sent_dir, pattern = "*.wav")
tg_files = list.files(sent_dir, pattern = "*.TextGrid")

# check which wav files already have associated TextGrids
done_files = gsub(".TextGrid", ".wav", tg_files)
# only run dtw on files that don't already have TextGrids
wav_files = wav_files[-match(done_files, wav_files)]


updateTimes <- function(tg, new_tg, alignment){
  # A function that updates the TextGrid based on alignment produced
  # by DTW.
  new_tg@endTime = alignment$index1[length(alignment$index1)] / freq
  for (i in 1:length(new_tg)){
    start_times = floor(tg[[i]]@startTimes * freq + 1)
    end_times = floor(tg[[i]]@endTimes * freq + 1)
    start_inds = match(start_times, alignment$index2)
    end_inds = match(end_times, alignment$index2)
    new_tg[[i]]@startTimes = (alignment$index1[start_inds] - 1) / freq
    new_tg[[i]]@endTimes = (alignment$index1[end_inds] - 1) / freq
  }
  return (new_tg)
}

for (f in 1:length(wav_files)){
  print(paste("Matching", wav_files[f]))
  sound_path = file.path(sent_dir, wav_files[f])
  tg_out_path = gsub(".wav$", ".TextGrid", sound_path)
  new_sound = readWave(sound_path)
  new_sound = downsample(new_sound, freq)
  s2 <- new_sound@left
  s2 <- s2 / 2^(new_sound@bit -1)
  alignment <- dtw(s2, s1)
  new_tg = updateTimes(tg, new_tg, alignment)
  
  # get this function from:
  #   https://rdrr.io/github/jpellman/textgridR/src/R/write.TextGrid.R
  write.TextGrid(new_tg, tg_out_path)
}

plot(dtw(test,ref,k=TRUE),type="two",off=1,match.lty=2,match.indices=200)