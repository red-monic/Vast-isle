-- {-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module MyProject where

import Data.Ratio
import Data.Char
import Data.List
import qualified Data.String
import Data.Maybe (listToMaybe, isNothing, isJust)
import qualified Data.Bifunctor
-- import Data.Text

run :: IO ()
run = putStrLn "Hello, world!"


-- TODO:
-- 2. good example (w/ and w/o pedofil)
-- 5. check pedofil is working
-- 6. rollback — if 3 is done
-- 7. refactor

-- DONE:
-- 1. formatting
-- 3. simple matching with reduce
-- 4. fix shitty bug (Kamil do it)


-- | an example of a problem solved with our library
example1 :: IO ()
example1 = do
    -- print Dominant
    -- print Recessive
    -- print $ codeToUpper "S"
    -- print $ codeToUpper "s"
    -- print $ codeToLower "S"
    -- print $ codeToLower "s"
    -- print $ furTrait [recessiveAllele, dominantAllele]
    -- print $ furTrait [dominantAllele, dominantAllele]
    -- print $ furTrait [recessiveAllele, recessiveAllele]
    -- print $ eyeSizeTrait [dominantAllele, recessiveAllele]
    -- print catWithBlueFurAndBigEyes
    print $ "First regular: " ++ show (getGeneration $ next $ fromGenotypes [geno1, geno2])
    -- print $ "Second regular: " ++ show (nextGen 2 (fromGenotypes [geno1, geno2]))
    -- print $ "Second with pedofil: " ++ show (nextWithPedofil 2 Nothing (fromGenotypes [geno1, geno2]))
    print "Given generation is [(DR, DD), (DR, RR)], let's compute rollback from its next gen"
    -- print $ rollback (next sampleGeneration2)

--------------------------------------------------------------------------------
-- |                   Data, newtypes and types declaration                    |
--------------------------------------------------------------------------------

data MyCode = A | B | C
    deriving (Eq, Show, Ord)
data AllelePair = RR | DR | DD
    deriving (Eq, Show, Ord)

-- | Represents a single trait i.e. fur
-- | Has a pair of alleles which corresponds to 
data Trait a = Trait a AllelePair
deriving instance Eq a => Eq (Trait a) 
deriving instance Ord a => Ord (Trait a) 

-- | Represents a single organism 
type Genotype a = [Trait a]

-- | The float type is not the best representation for the probability, 
-- | but it's fine for now
type ProbRatio = Float


-- | Represents an offspring in the generation. It is an organism which
-- | will be produced with some ratio
data Offspring a = Offspring { getType :: Genotype a, prob :: ProbRatio}
deriving instance Eq a => Eq (Offspring a)


-- | A single generation of offsprings
newtype Generation a = Generation {getGeneration :: [Offspring a]}

--------------------------------------------------------------------------------
--                                Class instances                              |
--------------------------------------------------------------------------------

-- | Shows |

instance Show a => Show (Trait a) where
    show (Trait c RR) = stringToLower (show c) ++ stringToLower (show c)
    show (Trait c DR) = stringToUpper (show c) ++ stringToLower (show c)
    show (Trait c DD) = stringToUpper (show c) ++ stringToUpper (show c)

instance (Show a) => Show (Offspring a) where
    show (Offspring geno p) = "Offspring: " ++ concatMap show geno ++ ", " ++ show p

instance Show a => Show (Generation a) where
    show (Generation gen) = "Generation:\n\t" ++ concatMap ((++ "\n\t") . show) gen

-- instance Show a => Show (Genotype a) where
--     show traits = "Genotype: \n\t" ++ concatMap ((++ "\n\t") . show) traits

-- | Ords |

-- instance Ord Trait where
--     compare (Trait c1 a1) (Trait c2 a2) = if c1 == c2
--                                         then compare (sort a1) (sort a2)
--                                         else compare (codeToLower c1) (codeToLower c2)

instance Ord a => Ord (Offspring a) where
    compare (Offspring g1 _) (Offspring g2 _) = compare (sort g1) (sort g2)


-- | Eq |

-- instance Eq a => Eq (Trait a) where
--     (Trait oneCode oneAlleles) == (Trait otherCode otherAlleles)
--         = sort oneAlleles == sort otherAlleles && oneCode == otherCode

--------------------------------------------------------------------------------
--                                   Examples                                  |
--------------------------------------------------------------------------------

-- |      Predefined traits and genotypes aka creatures       |


-- in this example we match "fur" <=> A
furTrait :: AllelePair -> Trait MyCode
furTrait = Trait A

-- in this example we match "eyeSize" <=> B
eyeSizeTrait :: AllelePair -> Trait MyCode
eyeSizeTrait = Trait B

exampleFurTrait1 :: Trait MyCode
exampleFurTrait1 = furTrait DR

exampleFurTrait2 :: Trait MyCode
exampleFurTrait2 = furTrait RR


catWithBlueFurAndBigEyes :: Genotype MyCode
catWithBlueFurAndBigEyes = [
        eyeSizeTrait DR, -- DR
        furTrait DR -- DR
    ]

catWithYellowFurAndBigEyes :: Genotype MyCode
catWithYellowFurAndBigEyes = [
        eyeSizeTrait DR, -- DR
        furTrait RR -- RR
    ]

-- [(DR, DR), (DR, RR)]
sampleGeneration1 :: Generation MyCode
sampleGeneration1 = fromGenotypes [catWithBlueFurAndBigEyes, catWithYellowFurAndBigEyes]

-- [(DR, DD), (DR, RR)]
sampleGeneration2 :: Generation MyCode
sampleGeneration2 = fromGenotypes [geno3, geno5]

-- end of Predefined traits and genotypes aka creatures

geno1 :: Genotype MyCode
geno1 = [eyeSizeTrait DR, furTrait DR]

geno2 :: Genotype MyCode
geno2 = [eyeSizeTrait DD, furTrait DD]

geno3 :: Genotype MyCode
geno3 = [eyeSizeTrait DR, furTrait DD]

geno4 :: Genotype MyCode
geno4 = [eyeSizeTrait DR, furTrait DR]

geno5 :: Genotype MyCode
geno5 = [eyeSizeTrait DR, furTrait RR]

osp1 :: Offspring MyCode
osp1 = Offspring [furTrait DD, eyeSizeTrait RR] (1/3)
osp2 :: Offspring MyCode
osp2 = Offspring [furTrait DR, eyeSizeTrait DR] (2/3)
osp3 :: Offspring MyCode
osp3 = Offspring [furTrait DD, eyeSizeTrait RR] (1/3)
osp4 :: Offspring MyCode
osp4 = Offspring [furTrait DR, eyeSizeTrait DR] (2/3)


--------------------------------------------------------------------------------
--                      Computations and Helper functions                      |
--------------------------------------------------------------------------------


stringToUpper :: String -> String
stringToUpper = map toUpper

stringToLower :: String -> String
stringToLower = map toLower


-- | expects 
listToElemWithLength :: [a] -> [(a, Int)]
listToElemWithLength [] = []
listToElemWithLength (x:xs) = [(x, length xs + 1)]


fromGenotype :: Int -> Genotype a -> Offspring a
fromGenotype n genes = Offspring genes (1.0/n')
    where
        n' = fromIntegral n

fromGenotypes :: [Genotype a] -> Generation a
fromGenotypes xs = Generation $ map (fromGenotype (length xs)) xs


combine :: Eq a => Trait a -> Trait a-> [(Trait a, ProbRatio)]
combine (Trait oneCode oneAlleles) (Trait otherCode otherAlleles) = result
    where
        result = if oneCode == otherCode
                 then map (Data.Bifunctor.first (Trait oneCode)) resultingTraits
                 else []

        resultingTraits = case (oneAlleles, otherAlleles) of
            (RR, RR) -> [(RR, 1.0)]
            (RR, DR) -> [(DR, 0.5), (RR, 0.5)]
            (RR, DD) -> [(DR, 1.0)]
            (DR, RR) -> [(DR, 0.5), (RR, 0.5)]
            (DR, DR) -> [(DD, 0.25), (DR, 0.5), (RR, 0.25)]
            (DR, DD) -> [(DD, 0.5), (DR, 0.5)]
            (DD, RR) -> [(DR, 1.0)]
            (DD, DR) -> [(DD, 0.5), (RR, 0.5)]
            (DD, DD) -> [(DD, 1.0)]


combineGenotypes :: Ord a => Genotype a -> Genotype a -> [(Genotype a, ProbRatio)]
combineGenotypes first second = summingUp folded
    where
        offsprings = filter (/= []) [combine traitX traitY | traitX <- first, traitY <- second]
        castedOffsprings = map (map (\(x,y) -> ([x], y))) offsprings
        g (old, prevProb) (current, currProb) = (old ++ current, prevProb * currProb)
        f one other =  [g x y | x <- one, y <- other]
        folded = foldr f [([], 1.0)] castedOffsprings


combineOffsprings :: forall a. (Eq a, Ord a) => Offspring a -> Offspring a -> Generation a
combineOffsprings one other = if one == other then Generation [] else Generation result
    where
        resultingGeno :: [(Genotype a, ProbRatio)]
        resultingGeno = combineGenotypes (getType one) (getType other)
        summed = summingUp resultingGeno
        result = map (uncurry Offspring) summed


next :: Eq a => Generation a -> Generation a
next = nextGen2 1

summingUp :: (Ord a, Ord b, Num b) => [([a], b)] -> [([a], b)]
summingUp xs = summed
    where
        sorted = sort $ map (\(l, v) -> (sort l, v)) xs
        grouped = groupBy comparator sorted
        comparator (l1, _) (l2, _) = l1 == l2

        summed = concatMap compact grouped

        -- | gets first value of the pair in the list and 
        -- | sumes all second values with the same first value
        compact [] = []
        compact ((x, currProb):xs') = [(x, currProb + sum (map snd xs'))]


nextGen :: Ord a => Int -> Generation a -> Generation a
nextGen bad current | bad <= 0 = current
nextGen 1 current = Generation new
    where
        probability xs
            | length xs <= 1 = 0
            | otherwise =  1.0 / fromIntegral (length xs * (length xs - 1))

        getGen = getGeneration
        offspringsCombination = concat [getGen $ combineOffsprings x y | x <- getGen current, y <- getGen current]
        preprocessedCombination = map (\(Offspring g p) -> (g, p * probability (getGen current))) offspringsCombination
        new = map (\(g, p) -> Offspring g p) (summingUp preprocessedCombination)
nextGen n current = nextGen (n-1) (next current)

-- есть gen1, gen2 - одинаковые по длине и признаку? если признак есть только во втором?
-- нужно combine каждый из 1 с каждым из второго N*N 

-- getUpdated :: Maybe Genotype -> Generation -> Generation
-- getUpdated pedofil current = Generation updated
--     where
--         updated = getGeneration current ++ appendix
--         appendix = case pedofil of
--             Nothing -> []
--             (Just p) -> [fromGenotype (length (getGeneration current) + 1) p]

-- nextWithPedofil :: Int -> Maybe Genotype -> Generation -> Generation
-- nextWithPedofil bad _ current | bad <= 0 = current
-- nextWithPedofil 1 pedofil current = next (getUpdated pedofil current)
-- nextWithPedofil n pedofil current = nextWithPedofil (n-1) newPedofil newGen
--     where
--         newGen = nextGen 1 updated
--         updated = getUpdated pedofil current
--         newPedofil = fmap getType (listToMaybe $ getGeneration newGen)

-- second version of combinations to be able to rollback to parents genotypes

-- combine but without reduce and sort
combine2 :: Eq a => Trait a -> Trait a -> [(Trait a, ProbRatio)]
combine2 (Trait oneCode oneAlleles) (Trait otherCode otherAlleles) = result
    where
        result = if oneCode == otherCode
                 then map (Data.Bifunctor.first (Trait oneCode)) resultingTraits
                 else [] -- [Left "Representing codes don't match"]

        resultingTraits = case (oneAlleles, otherAlleles) of
            (RR, RR) -> [(RR, 0.25), (RR, 0.25), (RR, 0.25), (RR, 0.25)]
            (RR, DR) -> [(DR, 0.25), (DR, 0.25), (RR, 0.25), (RR, 0.25)]
            (RR, DD) -> [(DR, 0.25), (DR, 0.25), (DR, 0.25), (DR, 0.25)]
            (DR, RR) -> [(DR, 0.25), (DR, 0.25), (RR, 0.25), (RR, 0.25)]
            (DR, DR) -> [(DD, 0.25), (DR, 0.25), (DR, 0.25), (RR, 0.25)]
            (DR, DD) -> [(DD, 0.25), (DD, 0.25), (DR, 0.25), (DR, 0.25)]
            (DD, RR) -> [(DR, 0.25), (DR, 0.25), (DR, 0.25), (DR, 0.25)]
            (DD, DR) -> [(DD, 0.25), (DD, 0.25), (RR, 0.25), (RR, 0.25)]
            (DD, DD) -> [(DD, 0.25), (DD, 0.25), (DD, 0.25), (DD, 0.25)]


-- combine genotypes but without reduce and sort
combineGenotypes2 :: Eq a => Genotype a -> Genotype a -> [(Genotype a, ProbRatio)]
combineGenotypes2 first second = folded
    where
        folded = foldr f [([], 1.0)] castedOffsprings
        f one other =  [g x y | x <- one, y <- other]
        g (old, prevProb) (current, currProb) = (old ++ current, prevProb * currProb)
        castedOffsprings = map (map (\(x,y) -> ([x], y))) offsprings
        offsprings = filter (/= []) [combine2 traitX traitY | traitX <- first, traitY <- second]

-- combine offsprings but without reduce and sort
combineOffsprings2 :: forall a. Eq a => Offspring a -> Offspring a -> Generation a
combineOffsprings2 one other = if one == other then Generation [] else Generation result
    where
        resultingGeno :: [(Genotype a, ProbRatio)]
        resultingGeno = combineGenotypes2 (getType one) (getType other)
        result = map (uncurry Offspring) resultingGeno

-- combine generations but without reduce and sort
nextGen2 :: Eq a => Int -> Generation a -> Generation a
nextGen2 bad current | bad <= 0 = current
nextGen2 1 current = Generation new
    where
        probability xs
            | length xs <= 1 = 0
            | otherwise =  2.0 / fromIntegral (length xs * (length xs - 1))

        offspringsCombination = filter ((2 == ) . length) $ subsequences $ getGeneration current
        newGenerations = filter (not . null) [getGeneration $ combineOffsprings2 x y | [x, y] <- offspringsCombination]

        new = concatMap (\generation -> map (\(Offspring g p) -> Offspring g (p * (probability $ getGeneration current))) generation) newGenerations
nextGen2 n current = nextGen2 (n-1) (nextGen2 1 current)
