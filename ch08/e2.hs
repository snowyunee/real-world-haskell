import Glob

main = do
  upperExtention <- namesMatching "./*.HS"
  print $ upperExtention
  lowerExtention <- namesMatching "./*.hs"
  print $ lowerExtention
  allDirectory <- namesMatching "**.hs"
  print $ allDirectory
