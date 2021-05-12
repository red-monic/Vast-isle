-- {-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}

module MyProject where

import Data.Ratio
import Data.Char
import Data.List
import qualified Data.String
-- import Data.Text

run :: IO ()
run = putStrLn "Hello, world!"


-- temporary = do
--     i <- [1..5]
--     j <- [6..10]
--     -- print 
--     -- return (i, j)
--     return ([1..5] )


example1 :: IO ()
example1 = do
    print Dominant
    print Recessive
    print $ codeToUpper "S"
    print $ codeToUpper "s"
    print $ codeToLower "S"
    print $ codeToLower "s"
    print $ furTrait [recessiveAllele, dominantAllele]
    print $ furTrait [dominantAllele, dominantAllele]
    print $ furTrait [recessiveAllele, recessiveAllele]
    print $ eyeSizeTrait [dominantAllele, recessiveAllele]
    print catWithBlueFurAndBigEyes
    print $ toPhenotype catWithBlueFurAndBigEyes
    print $ toPhenotype catWithYellowFurAndBigEyes

-- Predefined traits and genotypes aka creatures


-- example of fur trait, see code
-- NOTE: dominant is blue, recessive is yellow
furTrait :: [Allele] -> Trait
furTrait = Trait "Fur"

-- example of eye trait, see code
-- NOTE: dominant is big, recessive is small
eyeSizeTrait :: [Allele] -> Trait
eyeSizeTrait = Trait "Eye"


exampleFurTrait1 :: Trait
exampleFurTrait1 = furTrait [dominantAllele, recessiveAllele]

exampleFurTrait2 :: Trait
exampleFurTrait2 = furTrait [recessiveAllele, recessiveAllele]


catWithBlueFurAndBigEyes :: Genotype
catWithBlueFurAndBigEyes = [
    eyeSizeTrait [dominantAllele, recessiveAllele],
    furTrait [dominantAllele, recessiveAllele]
    ]

catWithYellowFurAndBigEyes :: Genotype
catWithYellowFurAndBigEyes = [
    eyeSizeTrait [dominantAllele, recessiveAllele],
    furTrait [recessiveAllele, recessiveAllele]
    ]


firstGeneration :: Generation
firstGeneration = fromGenotypes [catWithBlueFurAndBigEyes, catWithYellowFurAndBigEyes]

-- end of Predefined traits and genotypes aka creatures


-- TODO: donot like naming of Sort 
data Sort = Dominant | Recessive
    deriving (Show, Eq)

type Code = String

-- TODO: shitnaming in these 2 function
codeToUpper :: Code -> Code
codeToUpper = map toUpper

codeToLower :: Code -> Code
codeToLower = map toLower


newtype Allele = Allele {getSort :: Sort}
    deriving (Eq)

instance Show Allele where
    show allele = "Allele: " ++ show (getSort allele)

dominantAllele :: Allele
dominantAllele = Allele Dominant

recessiveAllele :: Allele
recessiveAllele = Allele Recessive

instance Semigroup Allele where
    Allele Dominant <> _ = Allele Dominant
    _ <> Allele Dominant = Allele Dominant
    _ <> _ = Allele Recessive

instance Monoid Allele where
    mempty = Allele Recessive

instance Ord Allele where
    compare (Allele Recessive) (Allele Dominant) = GT
    compare (Allele Dominant) (Allele Recessive) = LT
    compare (Allele Dominant) (Allele Dominant) = EQ
    compare (Allele Recessive) (Allele Recessive) = EQ

data Trait = Trait {
    representingCode :: Code, -- TODO: donot like this name too
    getAlleles :: [Allele]
} deriving (Show)


instance Eq Trait where
    (Trait oneCode oneAlleles) == (Trait otherCode otherAlleles)
        = sort oneAlleles == sort otherAlleles && oneCode == otherCode


-- NOTE: is it really working?
combine :: Trait -> Trait -> [(Trait, Ratio Int)]
combine (Trait oneCode oneAlleles) (Trait otherCode otherAlleles) = result
    where
        result = if oneCode == otherCode
                 then resultingTraits
                 else [] -- [Left "Representing codes don't match"]

        resultingTraits = map transformer countedAllAlleles
        transformer :: ((Allele, Allele), Int) -> (Trait, Ratio Int)
        transformer ((alleleA, alleleB), number) = (newTrait, number % combinationNumber)
            where
                newTrait = Trait oneCode [alleleA, alleleB]

        combinationNumber = sum $ map snd countedAllAlleles

        allAlleles = sort [sortTuple (x, y) | x <- oneAlleles, y <- otherAlleles]
        countedAllAlleles = concatMap mapper $ group allAlleles

        mapper :: [(Allele, Allele)] -> [((Allele, Allele), Int)]
        mapper [] = []
        mapper (x:xs) = [(x, length xs + 1)]

        sortTuple :: Ord a => (a, a) -> (a, a)
        sortTuple (one, other)
            | one <= other = (one, other)
            | otherwise = (other, one)


toCode :: Trait -> String
toCode trait = case mconcat $ getAlleles trait of
    Allele Dominant -> codeToUpper $ representingCode trait
    Allele Recessive -> codeToLower $ representingCode trait

-- newtype Genotype = Genotype { getTraits :: [Trait]}
--     deriving (Show)

type Genotype = [Trait]

toPhenotype :: Genotype -> Phenotype
toPhenotype traits = Phenotype (map toCode traits)


newtype Phenotype = Phenotype {getCodes :: [Code]}
    deriving (Show)


data Offspring = Offspring { getType :: Genotype, prob :: Ratio Int}
    deriving (Show)

fromGenotype :: Genotype -> Offspring
fromGenotype genes = Offspring genes 1.0

newtype Generation = Generation [Offspring]

fromGenotypes :: [Genotype] -> Generation
fromGenotypes = Generation . map fromGenotype


-- next :: Generation -> Either String Generation
-- next (Generation []) = Right $ Generation []
-- next (Generation current) = res
--     where
--         -- current :: [(Offspring (Genotype [Trait]), ratio)]
--         -- matched = [ | x <- current, y <- current]
--         res = Right $ Generation new 
--         combined = combineGenotypes current
--         new = []


-- -- Что делать с ошибками?
-- combineOffsprings :: [Offspring] -> [Offspring] -> [Offspring]
-- combineOffsprings [] _ = []
-- combineOffsprings _ [] = []
-- combineOffsprings (x:xs) (y:ys) = smth
--     where
--         smth = []
--         (Offspring genotypeX probX) = x
--         (Offspring genotypeY probY) = y
--         newGeno = combineGenotypes genotypeX genotypeY
--         r = case newGeno of 
--             Left e -> [] -- NO LOGGING!!!!!!!
--             Right new -> new


geno1 :: Genotype
geno1 = [eyeSizeTrait [recessiveAllele, dominantAllele],furTrait [recessiveAllele, dominantAllele]]

geno2 :: Genotype
geno2 = [eyeSizeTrait [dominantAllele, dominantAllele], furTrait [dominantAllele, dominantAllele]]

combineGenotypes :: Genotype -> Genotype -> [(Genotype, Ratio Int)]
combineGenotypes first second = foldr f [([], 1.0)] castedOffsprings
    where
        f one other =  [g x y | x <- one, y <- other]
        g (old, prevProb) (current, currProb) = (old ++ current, prevProb * currProb)
        castedOffsprings = map (map (\(x,y) -> ([x], y))) offsprings
        offsprings = filter (/= [])[combine traitX traitY | traitX <- first, traitY <- second]
