-- {-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}

module MyProject where

import Data.Ratio
import Data.Char
import Data.List
import qualified Data.String
import Data.Maybe (listToMaybe, isNothing, isJust)
-- import Data.Text

run :: IO ()
run = putStrLn "Hello, world!"


-- temporary = do
--     i <- [1..5]
--     j <- [6..10]
--     -- print 
--     -- return (i, j)
--     return ([1..5] )

-- TODO:
-- 1. formatting
-- 2. good example (w/ and w/o pedofil)
-- 3. simple matching with reduce
-- 4. fix shitty bug (Kamil do it)
-- 5. check pedofil is working
-- 6. rollback — if 3 is done


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
    -- print $ toPhenotype catWithBlueFurAndBigEyes
    -- print $ toPhenotype catWithYellowFurAndBigEyes
    print $ "First regular: " ++ show (next (fromGenotypes [geno1, geno2]))
    -- print $ "Second regular: " ++ show (nextGen 2 (fromGenotypes [geno1, geno2]))
    -- print $ "Second with pedofil: " ++ show (nextWithPedofil 2 Nothing (fromGenotypes [geno1, geno2]))

--------------------------------------------------------------------------------
-- |                   Data, newtypes and types declaration                    |
--------------------------------------------------------------------------------

-- TODO: donot like naming of Sort 
data Sort = Dominant | Recessive
    deriving (Show, Eq)

newtype Allele = Allele {getSort :: Sort}
    deriving (Eq)

type Code = String

data Trait = Trait {
    representingCode :: Code, -- TODO: donot like this name too
    getAlleles :: [Allele]
}

newtype Phenotype = Phenotype {getCodes :: [Code]}
    deriving (Show)

type Genotype = [Trait]

-- | TODO: shit, floating!
type ProbRatio = Float

data Offspring = Offspring { getType :: Genotype, prob :: ProbRatio}
    deriving (Eq)

type Generation = [Offspring]

--------------------------------------------------------------------------------
--                                Class instances                              |
--------------------------------------------------------------------------------

-- | Shows |

instance Show Allele where
    show allele = "Allele: " ++ show (getSort allele)

instance Show Trait where
    show (Trait c [x, y]) = getCode c x ++ getCode c y
    show (Trait _ _) = "Trait: empty"

instance Show Offspring where
    show (Offspring geno p) = "(Offspring: " ++ show geno ++ ", " ++ show p ++ ")"

-- |     |

instance Semigroup Allele where
    Allele Dominant <> _ = Allele Dominant
    _ <> Allele Dominant = Allele Dominant
    _ <> _ = Allele Recessive

instance Monoid Allele where
    mempty = Allele Recessive

-- | Ords |

instance Ord Allele where
    compare (Allele Recessive) (Allele Dominant) = GT
    compare (Allele Dominant) (Allele Recessive) = LT
    compare (Allele Dominant) (Allele Dominant) = EQ
    compare (Allele Recessive) (Allele Recessive) = EQ

instance Ord Trait where
    compare (Trait c1 a1) (Trait c2 a2) = if c1 == c2
                                        then compare (sort a1) (sort a2)
                                        else compare (codeToLower c1) (codeToLower c2)

instance Ord Offspring where
    compare (Offspring g1 _) (Offspring g2 _) = compare (sort g1) (sort g2)


-- | Eq |

instance Eq Trait where
    (Trait oneCode oneAlleles) == (Trait otherCode otherAlleles)
        = sort oneAlleles == sort otherAlleles && oneCode == otherCode

--------------------------------------------------------------------------------
--                                   Examples                                  |
--------------------------------------------------------------------------------

-- |      Predefined traits and genotypes aka creatures       |


-- example of fur trait, see code
-- NOTE: dominant is blue, recessive is yellow
furTrait :: [Allele] -> Trait
furTrait = Trait "A"

-- example of eye trait, see code
-- NOTE: dominant is big, recessive is small
eyeSizeTrait :: [Allele] -> Trait
eyeSizeTrait = Trait "B"

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

geno1 :: Genotype
geno1 = [eyeSizeTrait [recessiveAllele, dominantAllele], furTrait [recessiveAllele, dominantAllele]]

geno2 :: Genotype
geno2 = [eyeSizeTrait [dominantAllele, dominantAllele], furTrait [dominantAllele, dominantAllele]]

geno3 :: Genotype
geno3 = [eyeSizeTrait [recessiveAllele, dominantAllele], furTrait [dominantAllele, dominantAllele]]

geno4 :: Genotype
geno4 = [eyeSizeTrait [dominantAllele, recessiveAllele], furTrait [dominantAllele,recessiveAllele]]

osp3 = Offspring [furTrait [dominantAllele, dominantAllele], eyeSizeTrait [recessiveAllele, recessiveAllele]] (1/3)
osp4 = Offspring [furTrait [recessiveAllele, dominantAllele], eyeSizeTrait [recessiveAllele, dominantAllele]] (2/3)


--------------------------------------------------------------------------------
--                      Computations and Helper functions                      |
--------------------------------------------------------------------------------


-- TODO: shitnaming in these 2 function
codeToUpper :: Code -> Code
codeToUpper = map toUpper

codeToLower :: Code -> Code
codeToLower = map toLower

dominantAllele :: Allele
dominantAllele = Allele Dominant

recessiveAllele :: Allele
recessiveAllele = Allele Recessive

getCode :: Code -> Allele -> Code
getCode code trait
    | trait == Allele Dominant = codeToUpper code
    | otherwise = codeToLower code

-- sumUpper :: [(a, a), Int] -> (b, ProbRatio)

listToElemWithLength :: [a] -> [(a, Int)]
listToElemWithLength [] = []
listToElemWithLength (x:xs) = [(x, length xs + 1)]

toCode :: Trait -> String
toCode trait = case mconcat $ getAlleles trait of
    Allele Dominant -> codeToUpper $ representingCode trait
    Allele Recessive -> codeToLower $ representingCode trait

toPhenotype :: Genotype -> Phenotype
toPhenotype traits = Phenotype (map toCode traits)

fromGenotype :: Int -> Genotype -> Offspring
fromGenotype n genes = Offspring genes (1.0/n')
    where
        n' = fromIntegral n

fromGenotypes :: [Genotype] -> Generation
fromGenotypes xs = map (fromGenotype (length xs)) xs


-- NOTE: is it really working?
combine :: Trait -> Trait -> [(Trait, ProbRatio)]
combine (Trait oneCode oneAlleles) (Trait otherCode otherAlleles) = result
    where
        result = if oneCode == otherCode
                 then resultingTraits
                 else [] -- [Left "Representing codes don't match"]

        resultingTraits = map transformer countedAllAlleles
        transformer :: ((Allele, Allele), Int) -> (Trait, ProbRatio)
        transformer ((alleleA, alleleB), number) = (newTrait,
                fromIntegral number / fromIntegral combinationNumber
            )
            where
                newTrait = Trait oneCode [alleleA, alleleB]

        combinationNumber = sum $ map snd countedAllAlleles

        allAlleles = sort [sortTuple (x, y) | x <- oneAlleles, y <- otherAlleles]
        countedAllAlleles = concatMap listToElemWithLength $ group allAlleles

        -- mapper :: [(Allele, Allele)] -> [((Allele, Allele), Int)]
        -- mapper [] = []
        -- mapper (x:xs) = [(x, length xs + 1)]

        sortTuple :: Ord a => (a, a) -> (a, a)
        sortTuple (one, other)
            | one <= other = (one, other)
            | otherwise = (other, one)


combineGenotypes :: Genotype -> Genotype -> [(Genotype, ProbRatio)]
combineGenotypes first second = summingUp folded
    where
        folded = foldr f [([], 1.0)] castedOffsprings
        f one other =  [g x y | x <- one, y <- other]
        g (old, prevProb) (current, currProb) = (old ++ current, prevProb * currProb)
        castedOffsprings = map (map (\(x,y) -> ([x], y))) offsprings
        offsprings = filter (/= []) [combine traitX traitY | traitX <- first, traitY <- second]


-- -- Что делать с ошибками?
combineOffsprings :: Offspring  -> Offspring -> [Offspring]
combineOffsprings one other = if one == other then [] else result
    where
        resultingGeno :: [(Genotype, ProbRatio)]
        resultingGeno = combineGenotypes (getType one) (getType other)
        summed = summingUp resultingGeno
        result = map (uncurry Offspring) summed


next :: Generation -> Generation
next = nextGen 1

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


nextGen :: Int -> Generation -> Generation
nextGen bad current | bad <= 0 = current
nextGen 1 current = new
    where
        probability xs 
            | length xs <= 1 = fromIntegral 0 
            | otherwise =  1.0 / fromIntegral (length xs * (length xs - 1))
        
        offspringsCombination = concat [combineOffsprings x y | x <- current, y <- current]
        preprocessedCombination = map (\(Offspring g p) -> (g, p * (probability current))) offspringsCombination
        new = map (\(g, p) -> Offspring g p) (summingUp preprocessedCombination)
nextGen n current = nextGen (n-1) (next current)

-- есть gen1, gen2 - одинаковые по длине и признаку? если признак есть только во втором?
-- нужно combine каждый из 1 с каждым из второго N*N 

getUpdated :: Maybe Genotype -> Generation -> Generation
getUpdated pedofil current = updated
    where
        updated = current ++ appendix
        appendix = case pedofil of
            Nothing -> []
            (Just p) -> [fromGenotype (length current + 1) p]

nextWithPedofil :: Int -> Maybe Genotype -> Generation -> Generation
nextWithPedofil bad _ current | bad <= 0 = current
nextWithPedofil 1 pedofil current = next (getUpdated pedofil current)
nextWithPedofil n pedofil current = nextWithPedofil (n-1) newPedofil newGen
    where
        newGen = nextGen 1 updated
        updated = getUpdated pedofil current
        newPedofil = fmap getType (listToMaybe newGen)

-- second version of combinations to be able to rollback to parents genotypes

-- combine but without reduce and sort
combine2 :: Trait -> Trait -> [(Trait, ProbRatio)]
combine2 (Trait oneCode oneAlleles) (Trait otherCode otherAlleles) = result
    where
        result = if oneCode == otherCode
                 then resultingTraits
                 else [] -- [Left "Representing codes don't match"]

        resultingTraits = map transformer allAlleles
        transformer :: (Allele, Allele) -> (Trait, ProbRatio)
        transformer (alleleA, alleleB) = (newTrait, 1 / fromIntegral combinationNumber)
            where
                newTrait = Trait oneCode [alleleA, alleleB]

        combinationNumber = length allAlleles
        allAlleles = [(x, y) | x <- oneAlleles, y <- otherAlleles]

-- combine genotypes but without reduce and sort
combineGenotypes2 :: Genotype -> Genotype -> [(Genotype, ProbRatio)]
combineGenotypes2 first second = summingUp folded
    where
        folded = foldr f [([], 1.0)] castedOffsprings
        f one other =  [g x y | x <- one, y <- other]
        g (old, prevProb) (current, currProb) = (old ++ current, prevProb * currProb)
        castedOffsprings = map (map (\(x,y) -> ([x], y))) offsprings
        offsprings = filter (/= []) [combine2 traitX traitY | traitX <- first, traitY <- second]

-- combine offsprings but without reduce and sort
combineOffsprings2 :: Offspring  -> Offspring -> [Offspring]
combineOffsprings2 one other = if one == other then [] else result
    where
        resultingGeno :: [(Genotype, ProbRatio)]
        resultingGeno = combineGenotypes (getType one) (getType other)
        result = map (uncurry Offspring) castedGeno
        castedGeno = map f resultingGeno
        f (geno, genoRatio) = (geno, genoRatio * offstringCoeff)
        offstringCoeff = (otherRatio / (1 - oneRatio)) * (oneRatio / (1 - otherRatio))
        oneRatio = prob one
        otherRatio = prob other

