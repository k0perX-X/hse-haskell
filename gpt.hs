import System.Random
import Data.List
import qualified Data.Vector as V
import Data.Maybe (fromJust)
import Codec.Picture (Image(..), PixelRGB8(..), generateImage, writePng)

-- Определение генотипа как вектора из 16 генов
type Genotype = V.Vector Int

-- Генерация случайного генотипа
randomGenotype :: IO Genotype
randomGenotype = V.replicateM 15 (randomRIO (-9, 9)) >>= \g ->
 (\x -> V.snoc g x) <$> randomRIO (2, 12)

-- Функция для мутации генотипа
mutate :: Genotype -> IO Genotype
mutate genotype = do
  index <- randomRIO (0, 14)
  let value = genotype V.! index
  newValue <- if value == -9 then return (-8) else
    if value == 9 then return 8 else
      randomRIO (value - 1, value + 1)
  return $ V.update genotype (V.singleton index) (V.singleton newValue)

-- Генерация биоморфа на основе генотипа
generateBiomorph :: Genotype -> Image PixelRGB8
generateBiomorph genotype = generateImage (\x y -> let segments = generateSegments genotype in
  if abs x < length segments && abs y < length segments then
    let (sx, sy) = segments !! abs x in
      if abs y <= round sx && abs y <= round sy then PixelRGB8 0 0 0 else PixelRGB8 255 255 255
  else PixelRGB8 255 255 255) 150 150

generateSegments :: Genotype -> [(Float, Float)]
generateSegments genotype = map (\(i, g) -> (fromIntegral g * scaleFactor, fromIntegral (length genotype - i) * segmentLength)) $ zip [0..] (V.toList genotype)
  where
    scaleFactor = 5
    segmentLength = 10

-- Оценка подобия биоморфа заданному результату
similarity :: Image PixelRGB8 -> Image PixelRGB8 -> Double
similarity biomorph target = sum $ zipWith (\(PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) -> (fromIntegral (r1 - r2))^2 + (fromIntegral (g1 - g2))^2 + (fromIntegral (b1 - b2))^2) (pixels biomorph) (pixels target) / fromIntegral (length (pixels biomorph))
  where
    pixels :: Image PixelRGB8 -> [PixelRGB8]
    pixels (Image _ _ xs) = V.toList xs

-- Эволюционный процесс
evolution :: Int -> Genotype -> Image PixelRGB8 -> IO ()
evolution generations genotype target = do
  let biomorph = generateBiomorph genotype
  similarityScore <- return $ similarity biomorph target
  putStrLn $ "Generation: " ++ show generations ++ " Similarity: " ++ show similarityScore
  if generations == 0 || similarityScore == 1.0
    then return ()
    else do
      newGenotype <- mutate genotype
      evolution (generations - 1) newGenotype target

main :: IO ()
main = do
  initialGenotype <- randomGenotype
  -- Здесь нужно задать целевое изображение (target)
  let target = generateImage (\_ _ -> PixelRGB8 0 0 0) 150 150 -- Пример целевого изображения
  evolution 100 initialGenotype target
