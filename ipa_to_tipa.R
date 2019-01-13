# functions to replace ipa with tipa for LaTeX

ipa = c("a", "æ", "e", "ɛ", "i", "ɪ", "o", "ɔ", "u", "ʊ", "ʌ", "ɚ",
        "b", "d", "ð", "ʤ", 
        "f", "g", "h", "j", "k", "l", "m", "n", "ŋ", 
        "p", "r", "s", "ʃ", "t", "ʧ", "v", "w", "z", "θ")

# need to change to four backslashes for xtable to output it correctly
tipa = c("\\textscripta", "\\ae", "e", "\\textepsilon", "i",  
         "\\textsci", "o", "\\textopeno", "u", "\\textupsilon", 
         "\\textturnv", "\\textrhookschwa",
         "b", "d", "\\textipa{D}", "\\textdyoghlig",
         "f", "g", "h", "j", "k", "l", "m", "n", "\\textipa{N}",
         "p", "r", "s", "\\textesh", "t", "\\textteshlig", "v", "w", "z",
         "\\texttheta")

# replaces ipa with tipa for a vector of strings
replaceWithTipa <- function(string_vec){
  return (tipa[match(string_vec, ipa)])
}


# replaces the column and row names 
replaceColRowNamesTipa <- function(tbl, ipa, tipa){
  colnames(tbl) = replaceWithTipa(colnames(tbl), ipa, tipa)
  rownames(tbl) = replaceWithTipa(rownames(tbl), ipa, tipa)
  return(tbl)
}

# prints out the xtable with the right number of slashes
printXTableTipa <- function(tbl, ipa, tipa){
  print(xtable(replaceColRowNamesTipa(tbl, ipa, tipa)), 
        sanitize.colnames.function = identity, 
        sanitize.rownames.function = identity)
}


