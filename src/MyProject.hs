-- {-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module MyProject where

import Data.Char
import Data.List
import qualified Data.Bifunctor
-- import Data.Text

run :: IO ()
run = putStrLn "Hello, world!"


-- | an example of a problem solved with our library
example1 :: IO ()
example1 = do
    -- print $ furTrait [recessiveAllele, dominantAllele]
    -- print $ furTrait [dominantAllele, dominantAllele]
    -- print $ furTrait [recessiveAllele, recessiveAllele]
    -- print $ eyeSizeTrait [dominantAllele, recessiveAllele]
    print $ "First regular: " ++ show (getGeneration $ next $ fromGenotypes [geno1, geno2])
    -- print $ "Second regular: " ++ show (nextGen 2 (fromGenotypes [geno1, geno2]))
    print "Given generation is [(DR, DD), (DR, RR)], let's compute rollback from its next gen"

    -- print $ rollback (next sampleGeneration2)

--------------------------------------------------------------------------------
-- |                   Data, newtypes and types declaration                    |
--------------------------------------------------------------------------------

data MyCode = A | B | C
    deriving (Eq, Show, Ord)
data AllelePair = RR | DR | DD
    deriving (Eq, Show, Ord)
data Allele = R | D
    deriving (Eq, Show, Ord)

-- | Represents a single trait i.e. fur
-- | Has a pair of alleles which corresponds to 
data Trait a = Trait a AllelePair
deriving instance Eq a => Eq (Trait a)
deriving instance Ord a => Ord (Trait a)

-- | Represents a single phenotypic trait i.e. fur
-- | Has an allele which corresponds to 
data PhenoTrait a = PhenoTrait a Allele

-- | Represents a single organism 
newtype Genotype a = Genotype {getGenotype :: [Trait a]}
deriving instance Eq a => Eq (Genotype a)

-- | Represents the exteral signs of organism
type Phenotype a = [PhenoTrait a]

-- | The float type is not the best representation for the probability, 
-- | but it's fine for now
type ProbRatio = Float


-- | Represents an offspring in the generation. It is an organism which
-- | will be produced with some ratio
data Offspring a = Offspring { getType :: Genotype a, prob :: ProbRatio }
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

instance Show a => Show (PhenoTrait a) where
    show (PhenoTrait c D) = stringToUpper (show c)
    show (PhenoTrait c R) = stringToLower (show c)

instance (Show a) => Show (Offspring a) where
    show (Offspring geno p) = 
        "Offspring: " 
        ++ concatMap show (getGenotype geno)
        ++ ", " 
        ++ concatMap show (genotypeToPhenotype geno) 
        ++ ", " 
        ++ show p

instance (Show a) => Show (Genotype a) where
     show (Genotype traits) = concatMap show traits

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
    compare (Offspring g1 _) (Offspring g2 _) = compare (sort $ getGenotype g1) (sort $ getGenotype g2)


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

-- [(DR, DR), (DR, RR)]
sampleGeneration1 :: Generation MyCode
sampleGeneration1 = fromGenotypes [geno1, geno5]

-- [(DR, DD), (DR, RR)]
sampleGeneration2 :: Generation MyCode
sampleGeneration2 = fromGenotypes [geno3, geno5]

-- end of Predefined traits and genotypes aka creatures

geno1 :: Genotype MyCode
geno1 = Genotype [eyeSizeTrait DR, furTrait DR]

geno2 :: Genotype MyCode
geno2 = Genotype [eyeSizeTrait DD, furTrait DD]

geno3 :: Genotype MyCode
geno3 = Genotype [eyeSizeTrait DR, furTrait DD]

geno4 :: Genotype MyCode
geno4 = Genotype [eyeSizeTrait DR, furTrait DR]

geno5 :: Genotype MyCode
geno5 = Genotype [eyeSizeTrait DR, furTrait RR]

osp1 :: Offspring MyCode
osp1 = Offspring (Genotype [furTrait DD, eyeSizeTrait RR]) (1/3)
osp2 :: Offspring MyCode
osp2 = Offspring (Genotype [furTrait DR, eyeSizeTrait DR]) (2/3)
osp3 :: Offspring MyCode
osp3 = Offspring (Genotype [furTrait DD, eyeSizeTrait RR]) (1/3)
osp4 :: Offspring MyCode
osp4 = Offspring (Genotype [furTrait DR, eyeSizeTrait DR]) (2/3)


--------------------------------------------------------------------------------
--                      Computations and Helper functions                      |
--------------------------------------------------------------------------------

-- | translates all chars from the given string to the upper case
stringToUpper :: String -> String
stringToUpper = map toUpper

-- | translates all chars from the given string to the lower case
stringToLower :: String -> String
stringToLower = map toLower


-- | for empty list the result is empty
-- | for non-empty lists returns the list with only one element which is the 
-- | first element and the length
listToElemWithLength :: [a] -> [(a, Int)]
listToElemWithLength [] = []
listToElemWithLength (x:xs) = [(x, length xs + 1)]

-- | given the number of creatures in the generation and a creature, returns
-- | the offspring instance. the ratio is computed as 1/N
fromGenotype :: Int -> Genotype a -> Offspring a
fromGenotype n genes = Offspring genes (1.0/n')
    where
        n' = fromIntegral n

-- | creates a generation from the list of genotypes with fromGenotype
fromGenotypes :: [Genotype a] -> Generation a
fromGenotypes xs = Generation $ map (fromGenotype (length xs)) xs

-- | converts trait to phenoTrait
traitToPhenoTrait :: Trait a -> PhenoTrait a
traitToPhenoTrait (Trait c RR) = PhenoTrait c R
traitToPhenoTrait (Trait c _)  = PhenoTrait c D

-- | converts genotype to Phenotype
genotypeToPhenotype :: Genotype a -> Phenotype a
genotypeToPhenotype genotype = map traitToPhenoTrait (getGenotype genotype)

-- | combines two traits 
-- | returns the lists of the traits which can be obtained from the given
combine :: Eq a => Trait a -> Trait a -> [(Trait a, ProbRatio)]
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
            (DD, DR) -> [(DD, 0.5), (DR, 0.5)]
            (DD, DD) -> [(DD, 1.0)]

-- | combines two genotypes 
-- | returns the lists of the genotypes which can be obtained from the given
combineGenotypes :: Ord a => Genotype a -> Genotype a -> [(Genotype a, ProbRatio)]
combineGenotypes first second = map (Data.Bifunctor.first Genotype) (summingUp folded)
    where
        folded = foldr f [([], 1.0)] castedOffsprings
        f one other =  [g x y | x <- one, y <- other]
        g (old, prevProb) (current, currProb) = (old ++ current, prevProb * currProb)
        castedOffsprings = map (map (\(x,y) -> ([x], y))) offsprings
        offsprings = filter (/= []) [combine traitX traitY | traitX <- getGenotype first, traitY <- getGenotype second]


-- | combines two offsprings
-- | returns the lists of the offsprings which can be obtained from the given
combineOffsprings :: forall a. (Eq a, Ord a) => Offspring a -> Offspring a -> Generation a
combineOffsprings one other = if one == other then Generation [] else Generation result
    where
        resultingGeno :: [(Genotype a, ProbRatio)]
        resultingGeno = combineGenotypes (getType one) (getType other)
        summed = map (Data.Bifunctor.first Genotype) $
                summingUp $
                map (Data.Bifunctor.first getGenotype) resultingGeno
        result = map (uncurry Offspring) summed


-- | groups same items and sums up their probabilities
-- | first, it needs to sort the items, in order to successfully group them
-- | returns the uniqie items with their probabilities being summed up
summingUp :: (Ord a, Ord b, Num b) => [([a], b)] -> [([a], b)]
summingUp xs = summed
    where
        sorted = sort $ map (Data.Bifunctor.first sort) xs
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
        new = map (uncurry Offspring) (map (Data.Bifunctor.first Genotype) $
                summingUp $
                map (Data.Bifunctor.first getGenotype) preprocessedCombination)
nextGen n current = nextGen (n-1) (next current)

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
            (DD, DR) -> [(DD, 0.25), (DD, 0.25), (DR, 0.25), (DR, 0.25)]
            (DD, DD) -> [(DD, 0.25), (DD, 0.25), (DD, 0.25), (DD, 0.25)]


-- combine genotypes but without reduce and sort
combineGenotypes2 :: Eq a => Genotype a -> Genotype a -> [(Genotype a, ProbRatio)]
combineGenotypes2 first second = map (Data.Bifunctor.first Genotype) folded
    where
        folded = foldr f [([], 1.0)] castedOffsprings
        f one other =  [g x y | x <- one, y <- other]
        g (old, prevProb) (current, currProb) = (old ++ current, prevProb * currProb)
        castedOffsprings = map (map (\(x,y) -> ([x], y))) filteredOffsprings
        filteredOffsprings = filter (/= []) offsprings
        offsprings = [combine2 traitX traitY | traitX <- getGenotype first, traitY <- getGenotype second]

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

        new = concatMap (map (\(Offspring g p) -> Offspring g (p * (probability $ getGeneration current)))) newGenerations
nextGen2 n current = nextGen2 (n-1) (nextGen2 1 current)


-- | a default implementation for the next function 
-- | returns the plain result for the generation (i.e. w/o without any reduce)
next :: Eq a => Generation a -> Generation a
next = nextGen2 1
