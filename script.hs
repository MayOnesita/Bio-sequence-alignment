-- ENLACE A LA PRESENTACION: 
-- https://www.canva.com/design/DAF3Jyf_ZAk/AHZ_z_K-BJkYO79g7g0-_Q/view?utm_content=DAF3Jyf_ZAk&utm_campaign=designshare&utm_medium=link&utm_source=editor


import Data.Array
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)

type ScoreMatrix = Array (Int, Int) Int
type Traceback = Array (Int, Int) Direction

data Direction = Stop 
                 | LeftDir 
                 | Up 
                 | Diag 
                 deriving (Eq)

-- EJEMPLOS
adn1, adn2, arn1, arn2 :: String
adn1 = "TGTTACGG"
adn2 = "GGTTGACTA"

arn1 = "LGPSSKQTGKGSSRIWDN"
arn2 = "LNITKSAGKGAIMRLGDA"

-- Constantes
matchScore, mismatchScore, gapScore :: Int
matchScore = 3
mismatchScore = -3
gapScore = -2

-- Smith-Waterman
smithWaterman :: String -> String -> (ScoreMatrix, Traceback)
smithWaterman s1 s2 = (scoreMatrix, traceback)
  where 
    m = length s1
    n = length s2
    bounds = ((0, 0), (m, n))
    scoreMatrix = array bounds [((i, j), score i j) | (i, j) <- range bounds]
    traceback = array bounds [((i, j), trace i j) | (i, j) <- range bounds]

    score i j
      | i == 0 || j == 0 = 0
      | otherwise = maximum [0, 
                             scoreMatrix!(i-1, j-1) + delta (s1!!(i-1)) (s2!!(j-1)),
                             scoreMatrix!(i-1, j)   + gapScore,
                             scoreMatrix!(i, j-1)   + gapScore]

    trace i j
      | i == 0 || j == 0 = Stop
      | score i j == scoreMatrix!(i-1, j-1) + delta (s1!!(i-1)) (s2!!(j-1)) = Diag
      | score i j == scoreMatrix!(i-1, j) + gapScore = Up
      | score i j == scoreMatrix!(i, j-1) + gapScore = LeftDir
      | otherwise = Stop

    delta a b = if a == b then matchScore else mismatchScore

-- Alineamiento
tracebackAlignment :: ScoreMatrix -> Traceback -> String -> String -> (String, String)
tracebackAlignment scoreMatrix tb s1 s2 = go (maxIndices scoreMatrix) ([], [])
  where
    go (0, 0) al = al
    go (i, j) (as1, as2) =
      case tb!(i, j) of
        Stop    -> (as1, as2)
        Diag    -> go (i-1, j-1) (s1!!(i-1) : as1, s2!!(j-1) : as2)
        Up      -> go (i-1, j) (s1!!(i-1) : as1, '-' : as2)
        LeftDir -> go (i, j-1) ('-' : as1, s2!!(j-1) : as2)

    maxIndices :: ScoreMatrix -> (Int, Int)
    maxIndices sm = fst (maximumBy (comparing snd) (assocs sm))

-- Imprimir matriz de puntuación
-- print2DScoreMatrix :: ScoreMatrix -> IO ()
-- print2DScoreMatrix sm = mapM_ putStrLn [intercalate " " (map (show . (sm!)) [(i, j) | j <- [0..n]]) | i <- [0..m]]
--   where
--     ((_, _), (m, n)) = bounds sm


-- Imprimir matriz de puntuación con caracteres
print2DScoreMatrix :: String -> String -> ScoreMatrix -> IO ()
print2DScoreMatrix s1 s2 sm = do
  putStrLn $ "    " ++ intercalate " " (map (\c -> [c]) s2)
  mapM_ printRow [0..m]
  where
    ((_, _), (m, n)) = bounds sm

    printRow i = do
      let rowLabel = if i == 0 then " " else [s1!!(i-1)]
      putStrLn (rowLabel ++ " " ++ intercalate " " (map (show . (sm!)) [(i, j) | j <- [0..n]]))

-- Programa principal
main :: IO ()
main = do
  putStrLn "Ingrese la primera secuencia:"
  s1 <- getLine
  putStrLn "Ingrese la segunda secuencia:"
  s2 <- getLine

  let (sm, tb) = smithWaterman s1 s2
      (alignedS1, alignedS2) = tracebackAlignment sm tb s1 s2
  
  putStrLn "Matriz de puntuación:"
  print2DScoreMatrix s1 s2 sm

  putStrLn "\nAlineamientos:"
  putStrLn alignedS1
  putStrLn alignedS2