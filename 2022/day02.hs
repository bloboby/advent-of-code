import Util

rerange n = 1 + mod (n-1) 3

scoreShape [x]
  | elem x "AX" = 1
  | elem x "BY" = 2
  | elem x "CZ" = 3

scoreRound1 round =
  let [n,m] = map scoreShape round
  in m + case mod (m-n) 3 of
    0 -> 3
    1 -> 6
    2 -> 0

scoreRound2 [abc, xyz] =
  let n = scoreShape abc
  in case xyz of
    "X" -> rerange (n-1)
    "Y" -> 3 + n
    "Z" -> 6 + rerange (n+1)

main = interact $ Util.print . sum . map (scoreRound2 . words) . lines
