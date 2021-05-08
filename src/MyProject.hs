-- {-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}

module MyProject where

import Data.Ratio 
import Data.Char
-- import Data.Text

run :: IO ()
run = putStrLn "Hello, world!"


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

catWithBlueFurAndBigEyes :: Genotype
catWithBlueFurAndBigEyes = Genotype [
    eyeSizeTrait [dominantAllele, recessiveAllele],
    furTrait [dominantAllele, recessiveAllele] 
    ]

catWithYellowFurAndBigEyes :: Genotype
catWithYellowFurAndBigEyes = Genotype [
    eyeSizeTrait [dominantAllele, recessiveAllele],
    furTrait [recessiveAllele, recessiveAllele] 
    ]


firstGeneration :: Generation
firstGeneration = fromGenotypes [catWithBlueFurAndBigEyes, catWithYellowFurAndBigEyes]

-- end of Predefined traits and genotypes aka creatures





-- TODO: donot like naming of Sort 
data Sort = Dominant | Recessive 
    deriving (Show)

type Code = String 

-- TODO: shitnaming in these 2 function
codeToUpper :: Code -> Code
codeToUpper = map toUpper

codeToLower :: Code -> Code
codeToLower = map toLower


newtype Allele = Allele {getSort :: Sort} 
    deriving (Show)

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


data Trait = Trait { 
    representingCode :: Code, -- TODO: donot like this name too
    getAlleles :: [Allele]
} deriving (Show)

combine :: Trait -> [(Trait, Ratio Integer)] -> Either String Trait
combine one other = result
    where
        result = case representingCode one == representingCode other of
            True -> resultingTrait
            False -> Left "Representing codes don't match"
        
        -- TODO: left here. think about implementing the Monad instance for the
        -- combination algorithm
        -- resultingTraits :
        -- resultingTraits = ()

toCode :: Trait -> String
toCode trait = case mconcat $ getAlleles trait of 
    Allele Dominant -> codeToUpper $ representingCode trait
    Allele Recessive -> codeToLower $ representingCode trait

newtype Genotype = Genotype { getTraits :: [Trait]}
    deriving (Show)

toPhenotype :: Genotype -> Phenotype
toPhenotype (Genotype traits) = Phenotype (map toCode traits)


newtype Phenotype = Phenotype {getCodes :: [Code]} 
    deriving (Show)


data Offspring = Offspring { getType :: Genotype, prob :: Ratio Integer} 
    deriving (Show)

fromGenotype :: Genotype -> Offspring
fromGenotype genes = Offspring genes 1.0

newtype Generation = Generation [Offspring]

fromGenotypes :: [Genotype] -> Generation
fromGenotypes = Generation . map fromGenotype

computeOffsprings :: Generation -> Generation
computeOffsprings = _