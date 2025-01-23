
-- Type synonym
type MyTemperature = Int

-- Simple syntax
newtype Temperature' = Temp' Int

toInt' :: Temperature' -> Int
toInt' (Temp' i) = i

-- Record syntax
newtype Temperature = Temp { toInt :: Int}

invert :: Temperature -> Temperature
invert = Temp . negate . toInt
