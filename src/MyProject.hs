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
import qualified Data.Bifunctor
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
-- 5. check pedofil is working
-- 6. rollback — if 3 is done
-- 7. refactor

-- DONE:
-- 3. simple matching with reduce
-- 4. fix shitty bug (Kamil do it)


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
    print $ "First regular: " ++ show (getGeneration $ next $ fromGenotypes [geno1, geno2])
    -- print $ "Second regular: " ++ show (nextGen 2 (fromGenotypes [geno1, geno2]))
    -- print $ "Second with pedofil: " ++ show (nextWithPedofil 2 Nothing (fromGenotypes [geno1, geno2]))
    print "Given generation is [(DR, DD), (DR, RR)], let's compute rollback from its next gen"
    -- print $ rollback (next sampleGeneration2)

--------------------------------------------------------------------------------
-- |                   Data, newtypes and types declaration                    |
--------------------------------------------------------------------------------

data MyCode = A | B | C
data AllelePair = RR | DR | DD
data Trait a = Trait a AllelePair


-- | Represents a single organism 
type Genotype = [Trait]

-- | The float type is not the best representation for the probability, 
-- | but it's fine for now
type ProbRatio = Float

data Offspring = Offspring { getType :: Genotype, prob :: ProbRatio}
    deriving (Eq)

newtype Generation = Generation {getGeneration :: [Offspring]}

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
    show (Offspring geno p) = "Offspring: " ++ concatMap show geno ++ ", " ++ show p

instance Show Generation where
    show (Generation gen) = "Generation:\n\t" ++ concatMap ((++ "\n\t") . show) gen


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
        eyeSizeTrait [dominantAllele, recessiveAllele], -- DR
        furTrait [dominantAllele, recessiveAllele] -- DR
    ]

catWithYellowFurAndBigEyes :: Genotype
catWithYellowFurAndBigEyes = [
        eyeSizeTrait [dominantAllele, recessiveAllele], -- DR
        furTrait [recessiveAllele, recessiveAllele] -- RR
    ]

-- [(DR, DR), (DR, RR)]
sampleGeneration1 :: Generation
sampleGeneration1 = fromGenotypes [catWithBlueFurAndBigEyes, catWithYellowFurAndBigEyes]

-- [(DR, DD), (DR, RR)]
sampleGeneration2 :: Generation
sampleGeneration2 = fromGenotypes [geno3, geno5]

-- end of Predefined traits and genotypes aka creatures

geno1 :: Genotype
geno1 = [eyeSizeTrait [recessiveAllele, dominantAllele], furTrait [recessiveAllele, dominantAllele]]

geno2 :: Genotype
geno2 = [eyeSizeTrait [dominantAllele, dominantAllele], furTrait [dominantAllele, dominantAllele]]

geno3 :: Genotype
geno3 = [eyeSizeTrait [recessiveAllele, dominantAllele], furTrait [dominantAllele, dominantAllele]]

geno4 :: Genotype
geno4 = [eyeSizeTrait [dominantAllele, recessiveAllele], furTrait [dominantAllele,recessiveAllele]]

geno5 :: Genotype
geno5 = [eyeSizeTrait [dominantAllele, recessiveAllele], furTrait [recessiveAllele, recessiveAllele]]

osp1 :: Offspring
osp1 = Offspring [furTrait [dominantAllele, dominantAllele], eyeSizeTrait [recessiveAllele, recessiveAllele]] (1/3)
osp2 :: Offspring
osp2 = Offspring [furTrait [dominantAllele, recessiveAllele], eyeSizeTrait [dominantAllele, recessiveAllele]] (2/3)
osp3 :: Offspring
osp3 = Offspring [furTrait [dominantAllele, dominantAllele], eyeSizeTrait [recessiveAllele, recessiveAllele]] (1/3)
osp4 :: Offspring
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
fromGenotypes xs = Generation $ map (fromGenotype (length xs)) xs


combine :: Trait -> Trait -> [(Trait, ProbRatio)]
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
            (DR, DR) -> [(DD, 0.5), (RR, 0.5)]
            (DR, DD) -> [(DD, 0.5), (DR, 0.5)]
            (DD, RR) -> [(DR, 1.0)]
            (DD, DR) -> [(DD, 0.5), (RR, 0.5)]
            (DD, DD) -> [(DD, 1.0)]


combineGenotypes :: Genotype -> Genotype -> [(Genotype, ProbRatio)]
combineGenotypes first second = summingUp folded
    where
        folded = foldr f [([], 1.0)] castedOffsprings
        f one other =  [g x y | x <- one, y <- other]
        g (old, prevProb) (current, currProb) = (old ++ current, prevProb * currProb)
        castedOffsprings = map (map (\(x,y) -> ([x], y))) offsprings
        offsprings = filter (/= []) [combine traitX traitY | traitX <- first, traitY <- second]



instance Show [Offspring] where
    show = ""
        
-- -- Что делать с ошибками?
combineOffsprings :: Offspring  -> Offspring -> Generation
combineOffsprings one other = if one == other then Generation [] else Generation result
    where
        resultingGeno :: [(Genotype, ProbRatio)]
        resultingGeno = combineGenotypes (getType one) (getType other)
        summed = summingUp resultingGeno
        result = map (uncurry Offspring) summed


next :: Generation -> Generation
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


nextGen :: Int -> Generation -> Generation
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

getUpdated :: Maybe Genotype -> Generation -> Generation
getUpdated pedofil current = Generation updated
    where
        updated = getGeneration current ++ appendix
        appendix = case pedofil of
            Nothing -> []
            (Just p) -> [fromGenotype (length (getGeneration current) + 1) p]

nextWithPedofil :: Int -> Maybe Genotype -> Generation -> Generation
nextWithPedofil bad _ current | bad <= 0 = current
nextWithPedofil 1 pedofil current = next (getUpdated pedofil current)
nextWithPedofil n pedofil current = nextWithPedofil (n-1) newPedofil newGen
    where
        newGen = nextGen 1 updated
        updated = getUpdated pedofil current
        newPedofil = fmap getType (listToMaybe $ getGeneration newGen)

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
combineGenotypes2 first second = folded
    where
        folded = foldr f [([], 1.0)] castedOffsprings
        f one other =  [g x y | x <- one, y <- other]
        g (old, prevProb) (current, currProb) = (old ++ current, prevProb * currProb)
        castedOffsprings = map (map (\(x,y) -> ([x], y))) offsprings
        offsprings = filter (/= []) [combine2 traitX traitY | traitX <- first, traitY <- second]

-- combine offsprings but without reduce and sort
combineOffsprings2 :: Offspring  -> Offspring -> Generation
combineOffsprings2 one other = if one == other then Generation [] else Generation result
    where
        resultingGeno :: [(Genotype, ProbRatio)]
        resultingGeno = combineGenotypes2 (getType one) (getType other)
        result = map (uncurry Offspring) resultingGeno

-- combine generations but without reduce and sort
nextGen2 :: Int -> Generation -> Generation
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
