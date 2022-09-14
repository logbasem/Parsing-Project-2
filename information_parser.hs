{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

--This is one file with code written by me, but it is part of a project with a larger group of programmers.

module OpenInformationParsing
  ( InformationLineUnp (..),
    InformationFileUnp (..),
    ProjectionInput (..),
    InformationListWithNum (..),
    getAllCompanySum,
    getOneCompanyValue,
    getOneInformationValue,
  ) 
where
import InformationFileParsing
  ( InformationLine (..),
    InformationLineDayDiff (..),
    InformationLinesDayDiff,
    getDayDifference,
    rawWithDays,
  )
import Statistics
  ( uniqueCategories )
import KDEGeneration
  ( getProbabilityForCategoryFromInformations,
    getProbabilityForDayAllCompaniesFromInformations, 
  )
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Text
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Data.Time.Calendar as Calendar
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Set as Set
import qualified Data.Either as Either

--GLOBAL VARIABLES

currentWorkingFile :: FilePath
currentWorkingFile = "openInformationsComplete_08_02_2021T11_29_05.csv"

previousWorkingFile :: FilePath
previousWorkingFile = "InformationToValue_All_06_30_2021T11_19_46.csv"

currDate :: (Integer, Int, Int)
currDate = (2021, 8, 2)

char :: Char -> Parser Char
char = Megaparsec.Char.char

--TYPE DEFINITIONS
--Define type Parser
type Parser = Megaparsec.Parsec Void Text.Text
data InformationLineUnp = InformationLineUnp
  { InformationYearUnp :: Integer,
    InformationMonthUnp :: Int,
    InformationDayUnp :: Int,
    InformationNumUnp :: Text.Text,
    amountUnp :: Double,
    customerNameUnp :: Text.Text,
    status :: Text.Text,
    maybeService :: Text.Text
  }
  deriving (Eq, Show, Ord)
data InformationFileUnp = InformationFileUnp
  { filenameUnp :: FilePath,
    topLinesUnp :: [Text.Text],
    InformationLinesUnp :: [InformationLineUnp]
  }
  deriving (Eq, Show, Ord)
data ProjectionInput = ProjectionInput
  { openInformations :: [InformationLineUnp],
    prevFilename :: FilePath,
    basis :: Text.Text,
    projecDay :: (Integer, Int, Int),
    projecInformationNum :: Text.Text
  }
  deriving (Eq, Show, Ord)
data InformationListWithNum = InformationListWithNum
  { InformationNumID :: Text.Text,
    InformationList :: [InformationLineUnp] 
  }
  deriving (Eq, Show, Ord)
--TOP-LEVEL FUNCTIONS
--Gets the sum of expected value for all companies in a given amount 
--of days from current date.
getAllCompanySum :: FilePath
                    -> FilePath
                    -> Integer
                    -> (Integer, Int, Int)
                    -> IO (Either 
                            (Megaparsec.ParseErrorBundle Text.Text Void) 
                            Double)
getAllCompanySum filename prevFilename dayNum todaysDate = do
  allInformationEither <- returnInformationLinesUnp filename
  case allInformationEither of
    (Left e) -> return $ Left $ e
    (Right allInformationLines) -> do
      let companyNameList = Set.toList $ uniqueCategoriesUnp 
                                            customerNameUnp 
                                            allInformationLines
      projectionEither <- getOneCompanyValueFromInformations
                              allInformationLines 
                              prevFilename 
                              dayNum 
                              todaysDate 
                              `traverse` companyNameList
      return $ Right $ sum $ Either.rights projectionEither
--Gets the expected value for one given company in a given amount of
--days from current date.
--Uses FILENAME for individual application
getOneCompanyValue :: FilePath
                      -> FilePath
                      -> Text.Text
                      -> Integer
                      -> (Integer, Int, Int)
                      -> IO (Either 
                            (Megaparsec.ParseErrorBundle Text.Text Void) 
                            Double)
getOneCompanyValue filename prevFilename companyName dayNum todaysDate = do
  allInformationEither <- returnInformationLinesUnp filename
  case allInformationEither of
    (Left e) -> return $ Left $ e
    (Right allInformationLines) -> do
      let companyInformationList = (returnOpenInformationsForCategory
                                  allInformationLines
                                  customerNameUnp
                                  companyName)
      projectionValue <- (projectCompanyValue
                            companyName
                            companyInformationList
                            prevFilename
                            dayNum
                            todaysDate)
      return $ Right $ projectionValue
--Gets the expected value for one given company in a given amount of
--days from current date.
--Uses Information LIST for application within another function
getOneCompanyValueFromInformations :: [InformationLineUnp]
                                -> FilePath
                                -> Integer
                                -> (Integer, Int, Int)
                                -> Text.Text
                                -> IO (Either 
                                      (Megaparsec.ParseErrorBundle Text.Text Void) 
                                      Double)
getOneCompanyValueFromInformations InformationList prevFilename dayNum todaysDate companyName = do
  let companyInformationList = (returnOpenInformationsForCategory
                                  InformationList
                                  customerNameUnp
                                  companyName)
  projectionValue <- (projectCompanyValue
                            companyName
                            companyInformationList
                            prevFilename
                            dayNum
                            todaysDate)
  return $ Right $ projectionValue
--Projects the expected value for a given company for a given amount
--of days from current date.
projectCompanyValue :: Text.Text
                       -> [InformationLineUnp]
                       -> FilePath
                       -> Integer
                       -> (Integer, Int, Int)
                       -> IO Double
projectCompanyValue companyName companyInformationList prevFilename dayNum todaysDate = do
  let InformationNumList = Set.toList $ uniqueCategoriesUnp 
                                      InformationNumUnp 
                                      companyInformationList
  let infInfList = makeInformationListWithNum
                      companyInformationList
                      <$> InformationNumList
  projectinfEither <- (projectInformationValue `mapM` (makeProjectionInput
                                                        prevFilename
                                                        companyName
                                                        dayNum
                                                        todaysDate
                                                        <$> infinfList))
  return $ sum $ Either.rights projectinfEither
--Gets the expected value for an individual Information and Information number
--(Mostly useful for testing)
getOneInformationValue :: FilePath
                      -> FilePath
                      -> Text.Text
                      -> Integer
                      -> (Integer, Int, Int)
                      -> Text.Text
                      -> IO (Either 
                              (Megaparsec.ParseErrorBundle Text.Text Void) 
                              Double)
getOneInformationValue filename prevFilename companyName dayNum todaysDate InformationNum = do
  allInformationsEither <- returnInformationLinesUnp filename
  case allInformationsEither of
    (Left e) -> return $ Left $ e
    (Right allInformationLines) -> do
      let InformationLineNum = (InformationListWithNum
                              InformationNum
                              allInformationLines)
      projectionEither <- (projectInformationValue $ (makeProjectionInput
                                                      prevFilename
                                                      companyName
                                                      dayNum
                                                      todaysDate
                                                      InformationLineNum))
      case projectionEither of
        (Left err) -> return $ Left $ err
        (Right projectionVal) -> return $ Right $ projectionVal
--Projects the expected value of an individual Information of a given 
--company for a given amount of days from current date
projectInformationValue :: ProjectionInput 
                       -> IO (Either
                                (Megaparsec.ParseErrorBundle Text.Text Void) 
                                Double)
projectInformationValue (ProjectionInput { openInformations,
                                       prevFilename,
                                       basis,
                                       projecDay,
                                       projecInformationNum }) = do
  let  (amountOfInformation, daysOutstanding) = lookUpInformation 
                                              openInformations 
                                              projecInformationNum 
                                              projecDay
  probabilityEither <- calculateProbabilityOfvalue 
                                prevFilename 
                                basis 
                                (fromIntegral daysOutstanding)
  case probabilityEither of
    (Left e) -> return $ Left $ e
    (Right probabilityResult) ->
      return $ Right $ amountOfInformation * probabilityResult
--Given a company name (basis) and a number of days from current,
--calculate the probability of value.
calculateProbabilityOfvalue :: FilePath 
                                 -> Text.Text 
                                 -> Double 
                                 -> IO (Either
                                        (Megaparsec.ParseErrorBundle Text.Text Void) 
                                        Double)
calculateProbabilityOfvalue prevFilename basis dayDiff = do
  eitherRawFile <- rawWithDays prevFilename
  case eitherRawFile of
    (Left e) -> return $ Left $ e
    (Right InformationFile) -> do
      let InformationLines = InformationLinesDayDiff InformationFile
      let categories = uniqueCategories customer InformationLines
      if Set.member basis categories
        then do
          coordinate <- ( getProbabilityForCategoryFromInformations
                            InformationLines customer basis dayDiff )
          return $ Right $ snd coordinate
      else do
        allCoordinate <- ( getProbabilityForDayAllCompaniesFromInformations 
                              InformationLines dayDiff )
        return $ Right $ snd allCoordinate
--Given a list of Informations and an Information number (identifier), looks
--up the Information with that number and returns the amount of the Information
--and the day difference.
lookUpInformation :: [InformationLineUnp]
                  -> Text.Text 
                  -> (Integer, Int, Int)
                  -> (Double, Integer)
lookUpInformation openInformations wantedinfNum day =
  let Information = head $ foldr dropNotMatching [] openInformations
                where
                  dropNotMatching line lst =
                    if ((InformationNumUnp line) == wantedinfNum)
                      then line : lst
                      else lst
      (projYear, projMonth, projDay) = day
  in let dayDiff = getDayDifference ( InformationLine
                                      (InformationYearUnp Information)
                                      (InformationMonthUnp Information)
                                      (InformationDayUnp Information)
                                      (InformationNumUnp Information)
                                      (amountUnp Information)
                                      (Text.pack $ "Projected")
                                      projYear
                                      projMonth
                                      projDay
                                      (customerNameUnp Information)
                                      (maybeService Information) )
      in (amountUnp Information, dayDiff)
--TYPE CONVERSION
makeInformationListWithNum :: [InformationLineUnp] 
                          -> Text.Text 
                          -> InformationListWithNum
makeInformationListWithNum companyInformationList InformationNum =
  InformationListWithNum
    InformationNum
    (returnOpenInformationsForCategory
      companyInformationList
      InformationNumUnp
      InformationNum)
makeProjectionInput :: FilePath
                       -> Text.Text
                       -> Integer
                       -> (Integer, Int, Int)
                       -> InformationListWithNum
                       -> ProjectionInput
makeProjectionInput filename companyName dayNum todaysDate listWithNum =
  let infNum = InformationNumID listWithNum
      infList = InformationList listWithNum
  in
    ProjectionInput
        infList
        filename
        companyName
        (projectDay dayNum)
        infNum 
    where
      projectDay num =
        let (todaysYear, todaysMonth, todaysDay) = todaysDate
        in
          Calendar.toGregorian $
            Calendar.addDays
                num
                (Calendar.fromGregorian
                    todaysYear
                    todaysMonth
                    todaysDay)
--OPEN Information FILE PARSING
--Returns all open Informations for a given "category"
returnOpenInformationsForCategory :: Eq a => [InformationLineUnp]
                                 -> (InformationLineUnp -> a)
                                 -> a
                                 -> [InformationLineUnp]
returnOpenInformationsForCategory InformationList argument category =
  Set.toList $ Set.fromList $ foldr dropNotMatching [] InformationList
  where
    dropNotMatching line lst =
      if (\il -> argument il == category) line
        then line : lst
        else lst 
--Returns all unique "categories" for a given "argument"
--(Example: all InformationNumberUnp)
uniqueCategoriesUnp :: (InformationLineUnp -> Text.Text) -> [InformationLineUnp] -> Set.Set Text.Text
uniqueCategoriesUnp InformationLineToText InformationLineList =
  Set.fromList $ (InformationLineToText <$> InformationLineList)
--Returns all unique open Information lines from the open Information file
returnInformationLinesUnp :: FilePath -> IO (Either
                                          (Megaparsec.ParseErrorBundle Text.Text Void)
                                          [InformationLineUnp])
returnInformationLinesUnp filename = do
  eitherRawFile <- readAndParseUnp filename
  case eitherRawFile of
    (Left err) -> return $ Left $ err
    (Right InformationFile) -> 
      return $ Right $
        Set.toList $ Set.fromList $ InformationLinesUnp InformationFile
--Reads and parses Open Information File
readAndParseUnp :: FilePath -> IO (Either
                                (Megaparsec.ParseErrorBundle Text.Text Void) 
                                InformationFileUnp)
readAndParseUnp filename = do
  contents <- Text.fromStrict <$> Text.IO.readFile filename
  let filteredContents = Text.replace "\r\n" "\n" contents
  return $ Megaparsec.runParser (parseUnreducedUnp filename) filename filteredContents
--Parses InformationFile unreduced
parseUnreducedUnp :: FilePath -> Parser InformationFileUnp
parseUnreducedUnp fp =
  InformationFileUnp fp
    <$> parseTopLinesUnp
    <*> parseRestLinesUnp
--Parses top two unnecessary lines in InformationFile
parseTopLinesUnp :: Parser [Text.Text]
parseTopLinesUnp = Megaparsec.count 2 parseHeaderUnp
--Parses one individual line
parseHeaderUnp :: Parser Text.Text
parseHeaderUnp = Text.pack <$> Megaparsec.manyTill Megaparsec.Char.asciiChar Megaparsec.Char.eol
--Parses all InformationLines
parseRestLinesUnp :: Parser [InformationLineUnp]
parseRestLinesUnp = Megaparsec.manyTill parseInformationLineUnp Megaparsec.eof

parseInformationLineUnp :: Parser InformationLineUnp
parseInformationLineUnp = do
  InformationLineUnp <$> (parseInformationYearUnp <* char '-')