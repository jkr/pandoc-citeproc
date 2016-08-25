{-# LANGUAGE PatternGuards, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Text.CSL.Input.MODS ( readModsString
                           , readModsFile
                           ) where

import qualified Text.Pandoc.Builder as PB
-- import Text.XML hiding (Name)
import qualified Text.XML as X
import Text.XML.Cursor
import Text.CSL.Reference (Reference(..), RefType(..), RefDate(..),
                           Literal(..), emptyReference, parseDate)
import Text.CSL.Style (Formatted(..), Agent(..), emptyAgent)
import Text.CSL.Util (toLocale)
import Text.CSL.Input.Bibtex (toAuthor)
import qualified Text.Pandoc.Definition as P
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe, listToMaybe)
import qualified Data.Map as M
import Data.Char (isSpace, toUpper, isUpper)
import Data.Monoid ((<>))
import Data.List (nub)
import qualified Text.Pandoc.UTF8 as UTF8

fromText :: Text -> Formatted
fromText = Formatted . PB.toList . PB.text . T.unpack . T.strip

--  Modeled on listToMaybe
listToMonoid :: Monoid a => [a] -> a
listToMonoid [] = mempty
listToMonoid (x : _) = x

data ModsReference = ModsReference { modsID :: Text
                                   , modsTitleInfo :: [TitleInfo]
                                   , modsNames :: [Name]
                                   , modsGenre :: [Text]
                                   -- , modsRefType :: [RefType]
                                   , modsOriginInfo :: [OriginInfo]
                                   , modsLanguage :: [Language]
                                   , modsAbstract :: [Abstract]
                                   , modsNote :: [Note]
                                   , modsSubject :: [Subject]
                                   , modsRelated :: [RelatedItem]
                                   , modsIdent :: [Identifier]
                                   , modsPart :: [Part]
                                   , modsRecordInfo :: [RecordInfo]
                                   , modsLocation :: [Location]
                                   } deriving (Show, Eq)

data Location = Location { locUrl :: [Text]
                         } deriving (Show, Eq)

data RecordInfo = RecordInfo { recInfoId :: [Text]
                             , recInfoLang :: [Language]
                             } deriving (Show, Eq)

data Range = FromBeginning Text
           | ToEnd Text
           | Delimited Text Text
  deriving (Show, Eq)

rangeToText :: Range -> Text
rangeToText (FromBeginning txt) = "-" <> txt
rangeToText (ToEnd txt) = txt <> "-"
rangeToText (Delimited txt txt') = txt <> "-" <> txt'

data DetailType = VolumeDetail
                | PartDetail
                | IssueDetail
                | ChapterDetail
                deriving (Show, Eq)

data Detail = Detail { detailType :: DetailType
                     , detailNumber :: [Text]
                     } deriving (Show, Eq)

data Part = Part { partPageRange :: [Range]
                 , partDetails :: [Detail]
                 , partDate :: [RefDate]
                 } deriving (Show, Eq)


data TitleInfo = TitleInfo { titleInfoTitle :: [Text]
                           , titleInfoSubTitle :: [Text]
                           , titleInfoAbbreviated :: [Text]
                           } deriving (Show, Eq)

data Name = Name { nName  :: Agent
                 , nRoles :: [Role]
                 } deriving (Show, Eq)

data Issuance = Continuing | Monographic
              deriving (Show, Eq)

data OriginInfo = OriginInfo { origPlace :: [Text]
                             , origPublisher :: [Text]
                             , origIssued :: [RefDate]
                             , origCreated :: [RefDate]
                             , origEdition :: [Text]
                             , origIssuance :: [Issuance]
                             } deriving (Show, Eq)

newtype Language = Language { fromLanguage :: Text }
                 deriving (Show, Eq, Monoid)

newtype Abstract = Abstract { fromAbstract :: Text }
  deriving (Show, Eq, Monoid)

-- We can expand this as need be.
data Note = AnyNote Text
          | Annotation Text
          deriving (Show, Eq)

--  Using a data instead of a newtype here because we might want to
--  add more fields. I'm skipping them now because bibutils seems to.
data Subject = Subject { subjTopic :: [Text]
                       , subjGeographic :: [Text]
                       } deriving (Show, Eq)

data RelatedItem = RelatedItem { relItemType :: RelItemType
                               , relItemModsRef :: ModsReference
                               } deriving (Show, Eq)

data RelItemType = Preceding
                 | Succeeding
                 | Original
                 | Host
                 | Constituent
                 | Series
                 | OtherVersion
                 | OtherFormat
                 | IsReferencedBy
                 deriving (Show, Eq)

data Identifier = Identifier { identType :: IdentifierType
                             , identText :: Text
                             } deriving (Show, Eq)

data IdentifierType = DOI
                    | URI
                    | ISBN
                    | ISSN
                    | CiteKey
                    deriving (Show, Eq)

detailCursToDetails :: Cursor -> [Detail]
detailCursToDetails curs = do
  typText <- attribute (X.Name "type" Nothing Nothing) curs
  let typ = case T.toLower typText of
        "volume" -> VolumeDetail
        "part" -> PartDetail
        "issue" -> IssueDetail
        "chapter" -> ChapterDetail
        _         -> IssueDetail
      number' = child curs >>=
        element (X.Name "number" (Just modsNS) Nothing) >>=
        descendant >>= 
        content 
  return $ Detail typ number'

extentCursToRange :: Cursor -> [Range]
extentCursToRange curs =
  let pageExtent =
        ([curs] >>= attributeIs (X.Name "unit" Nothing Nothing) "page") ++
        ([curs] >>= attributeIs (X.Name "unit" Nothing Nothing) "pages")
      start = pageExtent >>=
        child >>=
        element (X.Name "start" (Just modsNS) Nothing) >>=
        descendant >>=
        content
      start' = listToMonoid start
      end = pageExtent >>=
        child >>=
        element (X.Name "end" (Just modsNS) Nothing) >>=
        descendant >>=
        content
      end' = listToMonoid end
  in case () of
    _ | start' /= mempty && end' /= mempty -> [Delimited start' end']
      | start' /= mempty -> [ToEnd end']
      | end' /= mempty -> [FromBeginning start']
      | otherwise -> []
    
    
partCursToPart :: Cursor -> Part
partCursToPart curs =
  let details = child curs >>=
        element (X.Name "detail" (Just modsNS) Nothing) >>=
        detailCursToDetails
      pageExtent = child curs >>=
        element (X.Name "extent" (Just modsNS) Nothing) >>=
        extentCursToRange
      date = child curs >>=
        element (X.Name "date" (Just modsNS) Nothing) >>=
        descendant >>=
        content >>=
        (parseDate . T.unpack)

  in
    Part pageExtent details date

identCursToIdentifier :: Cursor -> [Identifier]
identCursToIdentifier curs = do
  typ <- attribute (X.Name "type" Nothing Nothing) curs

  identType' <- case T.toLower typ of
    "doi" -> [DOI]
    "uri" -> [URI]
    "isbn" -> [ISBN]
    "issn" -> [ISSN]
    "citekey" -> [CiteKey]
    _ -> []
    
  identText' <- (descendant curs >>= content)
  return $ Identifier identType' identText'

relCursorToRelItem :: Cursor -> [RelatedItem]
relCursorToRelItem curs = do
  typ <- attribute (X.Name "type" Nothing Nothing) curs
  relItemType' <- case T.toLower typ of
        "preceding" -> [Preceding]
        "succeeding" -> [Succeeding]
        "original" -> [Original]
        "host" -> [Host]
        "consituent" -> [Constituent]
        "series" -> [Series]
        "otherVersion" -> [OtherVersion]
        "otherFormat" -> [OtherFormat]
        "isReferencedBy" -> [IsReferencedBy]
        _ -> []

  let modsRef = modsCursToModsReference curs
  return $ RelatedItem relItemType' modsRef

iso60392b :: M.Map Text Text
iso60392b = M.fromList [("aar", "afar")
                       , ("abk", "abkhazian")
                       , ("ace", "achinese")
                       , ("ach", "acoli")
                       , ("ada", "adangme")
                       , ("ady", "adyghe")
                       , ("afa", "afro-asiatic")
                       , ("afh", "afrihili")
                       , ("afr", "afrikaans")
                       , ("ain", "ainu")
                       , ("aka", "akan")
                       , ("akk", "akkadian")
                       , ("ale", "aleut")
                       , ("alg", "algonquian")
                       , ("alt", "southern")
                       , ("amh", "amharic")
                       , ("ang", "english,")
                       , ("anp", "angika")
                       , ("apa", "apache")
                       , ("ara", "arabic")
                       , ("arc", "official")
                       , ("arg", "aragonese")
                       , ("arn", "mapudungun")
                       , ("arp", "arapaho")
                       , ("art", "artificial")
                       , ("arw", "arawak")
                       , ("asm", "assamese")
                       , ("ast", "asturian")
                       , ("ath", "athapascan")
                       , ("aus", "australian")
                       , ("ava", "avaric")
                       , ("ave", "avestan")
                       , ("awa", "awadhi")
                       , ("aym", "aymara")
                       , ("aze", "azerbaijani")
                       , ("bad", "banda")
                       , ("bai", "bamileke")
                       , ("bak", "bashkir")
                       , ("bal", "baluchi")
                       , ("bam", "bambara")
                       , ("ban", "balinese")
                       , ("bas", "basa")
                       , ("bat", "baltic")
                       , ("bej", "beja")
                       , ("bel", "belarusian")
                       , ("bem", "bemba")
                       , ("ben", "bengali")
                       , ("ber", "berber")
                       , ("bho", "bhojpuri")
                       , ("bih", "bihari")
                       , ("bik", "bikol")
                       , ("bin", "bini")
                       , ("bis", "bislama")
                       , ("bla", "siksika")
                       , ("bnt", "bantu")
                       , ("bod", "tibetan")
                       , ("bos", "bosnian")
                       , ("bra", "braj")
                       , ("bre", "breton")
                       , ("btk", "batak")
                       , ("bua", "buriat")
                       , ("bug", "buginese")
                       , ("bul", "bulgarian")
                       , ("byn", "blin")
                       , ("cad", "caddo")
                       , ("cai", "central")
                       , ("car", "galibi")
                       , ("cat", "catalan")
                       , ("cau", "caucasian")
                       , ("ceb", "cebuano")
                       , ("cel", "celtic")
                       , ("ces", "czech")
                       , ("cha", "chamorro")
                       , ("chb", "chibcha")
                       , ("che", "chechen")
                       , ("chg", "chagatai")
                       , ("chk", "chuukese")
                       , ("chm", "mari")
                       , ("chn", "chinook")
                       , ("cho", "choctaw")
                       , ("chp", "chipewyan")
                       , ("chr", "cherokee")
                       , ("chu", "church")
                       , ("chv", "chuvash")
                       , ("chy", "cheyenne")
                       , ("cmc", "chamic")
                       , ("cop", "coptic")
                       , ("cor", "cornish")
                       , ("cos", "corsican")
                       , ("cpe", "creoles")
                       , ("cpf", "creoles")
                       , ("cpp", "creoles")
                       , ("cre", "cree")
                       , ("crh", "crimean")
                       , ("crp", "creoles")
                       , ("csb", "kashubian")
                       , ("cus", "cushitic")
                       , ("cym", "welsh")
                       , ("dak", "dakota")
                       , ("dan", "danish")
                       , ("dar", "dargwa")
                       , ("day", "land")
                       , ("del", "delaware")
                       , ("den", "slave")
                       , ("deu", "german")
                       , ("dgr", "dogrib")
                       , ("din", "dinka")
                       , ("div", "divehi")
                       , ("doi", "dogri")
                       , ("dra", "dravidian")
                       , ("dsb", "lower")
                       , ("dua", "duala")
                       , ("dum", "dutch,")
                       , ("dyu", "dyula")
                       , ("dzo", "dzongkha")
                       , ("efi", "efik")
                       , ("egy", "egyptian")
                       , ("eka", "ekajuk")
                       , ("ell", "greek,")
                       , ("elx", "elamite")
                       , ("eng", "english")
                       , ("enm", "english,")
                       , ("epo", "esperanto")
                       , ("est", "estonian")
                       , ("eus", "basque")
                       , ("ewe", "ewe")
                       , ("ewo", "ewondo")
                       , ("fan", "fang")
                       , ("fao", "faroese")
                       , ("fas", "persian")
                       , ("fat", "fanti")
                       , ("fij", "fijian")
                       , ("fil", "filipino")
                       , ("fin", "finnish")
                       , ("fiu", "finno-ugrian")
                       , ("fon", "fon")
                       , ("fra", "french")
                       , ("frm", "french,")
                       , ("fro", "french,")
                       , ("frr", "northern")
                       , ("frs", "eastern")
                       , ("fry", "western")
                       , ("ful", "fulah")
                       , ("fur", "friulian")
                       , ("gaa", "ga")
                       , ("gay", "gayo")
                       , ("gba", "gbaya")
                       , ("gem", "germanic")
                       , ("gez", "geez")
                       , ("gil", "gilbertese")
                       , ("gla", "gaelic")
                       , ("gle", "irish")
                       , ("glg", "galician")
                       , ("glv", "manx")
                       , ("gmh", "german,")
                       , ("goh", "german,")
                       , ("gon", "gondi")
                       , ("gor", "gorontalo")
                       , ("got", "gothic")
                       , ("grb", "grebo")
                       , ("grc", "greek,")
                       , ("grn", "guarani")
                       , ("gsw", "swiss")
                       , ("guj", "gujarati")
                       , ("gwi", "gwich'in")
                       , ("hai", "haida")
                       , ("hat", "haitian")
                       , ("hau", "hausa")
                       , ("haw", "hawaiian")
                       , ("heb", "hebrew")
                       , ("her", "herero")
                       , ("hil", "hiligaynon")
                       , ("him", "himachali")
                       , ("hin", "hindi")
                       , ("hit", "hittite")
                       , ("hmn", "hmong")
                       , ("hmo", "hiri")
                       , ("hrv", "croatian")
                       , ("hsb", "upper")
                       , ("hun", "hungarian")
                       , ("hup", "hupa")
                       , ("hye", "armenian")
                       , ("iba", "iban")
                       , ("ibo", "igbo")
                       , ("ido", "ido")
                       , ("iii", "sichuan")
                       , ("ijo", "ijo")
                       , ("iku", "inuktitut")
                       , ("ile", "interlingue")
                       , ("ilo", "iloko")
                       , ("ina", "interlingua")
                       , ("inc", "indic")
                       , ("ind", "indonesian")
                       , ("ine", "indo-european")
                       , ("inh", "ingush")
                       , ("ipk", "inupiaq")
                       , ("ira", "iranian")
                       , ("iro", "iroquoian")
                       , ("isl", "icelandic")
                       , ("ita", "italian")
                       , ("jav", "javanese")
                       , ("jbo", "lojban")
                       , ("jpn", "japanese")
                       , ("jpr", "judeo-persian")
                       , ("jrb", "judeo-arabic")
                       , ("kaa", "kara-kalpak")
                       , ("kab", "kabyle")
                       , ("kac", "kachin")
                       , ("kal", "kalaallisut")
                       , ("kam", "kamba")
                       , ("kan", "kannada")
                       , ("kar", "karen")
                       , ("kas", "kashmiri")
                       , ("kat", "georgian")
                       , ("kau", "kanuri")
                       , ("kaw", "kawi")
                       , ("kaz", "kazakh")
                       , ("kbd", "kabardian")
                       , ("kha", "khasi")
                       , ("khi", "khoisan")
                       , ("khm", "central")
                       , ("kho", "khotanese")
                       , ("kik", "kikuyu")
                       , ("kin", "kinyarwanda")
                       , ("kir", "kirghiz")
                       , ("kmb", "kimbundu")
                       , ("kok", "konkani")
                       , ("kom", "komi")
                       , ("kon", "kongo")
                       , ("kor", "korean")
                       , ("kos", "kosraean")
                       , ("kpe", "kpelle")
                       , ("krc", "karachay-balkar")
                       , ("krl", "karelian")
                       , ("kro", "kru")
                       , ("kru", "kurukh")
                       , ("kua", "kuanyama")
                       , ("kum", "kumyk")
                       , ("kur", "kurdish")
                       , ("kut", "kutenai")
                       , ("lad", "ladino")
                       , ("lah", "lahnda")
                       , ("lam", "lamba")
                       , ("lao", "lao")
                       , ("lat", "latin")
                       , ("lav", "latvian")
                       , ("lez", "lezghian")
                       , ("lim", "limburgan")
                       , ("lin", "lingala")
                       , ("lit", "lithuanian")
                       , ("lol", "mongo")
                       , ("loz", "lozi")
                       , ("ltz", "luxembourgish")
                       , ("lua", "luba-lulua")
                       , ("lub", "luba-katanga")
                       , ("lug", "ganda")
                       , ("lui", "luiseno")
                       , ("lun", "lunda")
                       , ("luo", "luo")
                       , ("lus", "lushai")
                       , ("mad", "madurese")
                       , ("mag", "magahi")
                       , ("mah", "marshallese")
                       , ("mai", "maithili")
                       , ("mak", "makasar")
                       , ("mal", "malayalam")
                       , ("man", "mandingo")
                       , ("map", "austronesian")
                       , ("mar", "marathi")
                       , ("mas", "masai")
                       , ("mdf", "moksha")
                       , ("mdr", "mandar")
                       , ("men", "mende")
                       , ("mga", "irish,")
                       , ("mic", "mi'kmaq")
                       , ("min", "minangkabau")
                       , ("mis", "uncoded")
                       , ("mkd", "macedonian")
                       , ("mkh", "mon-khmer")
                       , ("mlg", "malagasy")
                       , ("mlt", "maltese")
                       , ("mnc", "manchu")
                       , ("mni", "manipuri")
                       , ("mno", "manobo")
                       , ("moh", "mohawk")
                       , ("mon", "mongolian")
                       , ("mos", "mossi")
                       , ("mri", "maori")
                       , ("msa", "malay")
                       , ("mul", "multiple")
                       , ("mun", "munda")
                       , ("mus", "creek")
                       , ("mwl", "mirandese")
                       , ("mwr", "marwari")
                       , ("mya", "burmese")
                       , ("myn", "mayan")
                       , ("myv", "erzya")
                       , ("nah", "nahuatl")
                       , ("nai", "north")
                       , ("nap", "neapolitan")
                       , ("nau", "nauru")
                       , ("nav", "navajo")
                       , ("nbl", "ndebele")
                       , ("nde", "ndebele")
                       , ("ndo", "ndonga")
                       , ("nds", "low")
                       , ("nep", "nepali")
                       , ("new", "nepal")
                       , ("nia", "nias")
                       , ("nic", "niger-kordofanian")
                       , ("niu", "niuean")
                       , ("nld", "dutch")
                       , ("nno", "norwegian")
                       , ("nob", "bokm\xc3\xa5l,")
                       , ("nog", "nogai")
                       , ("non", "norse,")
                       , ("nor", "norwegian")
                       , ("nqo", "n'ko")
                       , ("nso", "pedi")
                       , ("nub", "nubian")
                       , ("nwc", "classical")
                       , ("nya", "chichewa")
                       , ("nym", "nyamwezi")
                       , ("nyn", "nyankole")
                       , ("nyo", "nyoro")
                       , ("nzi", "nzima")
                       , ("oci", "occitan")
                       , ("oji", "ojibwa")
                       , ("ori", "oriya")
                       , ("orm", "oromo")
                       , ("osa", "osage")
                       , ("oss", "ossetian")
                       , ("ota", "turkish,")
                       , ("oto", "otomian")
                       , ("paa", "papuan")
                       , ("pag", "pangasinan")
                       , ("pal", "pahlavi")
                       , ("pam", "pampanga")
                       , ("pan", "panjabi")
                       , ("pap", "papiamento")
                       , ("pau", "palauan")
                       , ("peo", "persian,")
                       , ("phi", "philippine")
                       , ("phn", "phoenician")
                       , ("pli", "pali")
                       , ("pol", "polish")
                       , ("pon", "pohnpeian")
                       , ("por", "portuguese")
                       , ("pra", "prakrit")
                       , ("pro", "proven\xc3\xa7al,")
                       , ("pus", "pushto")
                       , ("qaa", "reserved")
                       , ("que", "quechua")
                       , ("raj", "rajasthani")
                       , ("rap", "rapanui")
                       , ("rar", "rarotongan")
                       , ("roa", "romance")
                       , ("roh", "romansh")
                       , ("rom", "romany")
                       , ("ron", "romanian")
                       , ("run", "rundi")
                       , ("rup", "aromanian")
                       , ("rus", "russian")
                       , ("sad", "sandawe")
                       , ("sag", "sango")
                       , ("sah", "yakut")
                       , ("sai", "south")
                       , ("sal", "salishan")
                       , ("sam", "samaritan")
                       , ("san", "sanskrit")
                       , ("sas", "sasak")
                       , ("sat", "santali")
                       , ("scn", "sicilian")
                       , ("sco", "scots")
                       , ("sel", "selkup")
                       , ("sem", "semitic")
                       , ("sga", "irish,")
                       , ("sgn", "sign")
                       , ("shn", "shan")
                       , ("sid", "sidamo")
                       , ("sin", "sinhala")
                       , ("sio", "siouan")
                       , ("sit", "sino-tibetan")
                       , ("sla", "slavic")
                       , ("slk", "slovak")
                       , ("slv", "slovenian")
                       , ("sma", "southern")
                       , ("sme", "northern")
                       , ("smi", "sami")
                       , ("smj", "lule")
                       , ("smn", "inari")
                       , ("smo", "samoan")
                       , ("sms", "skolt")
                       , ("sna", "shona")
                       , ("snd", "sindhi")
                       , ("snk", "soninke")
                       , ("sog", "sogdian")
                       , ("som", "somali")
                       , ("son", "songhai")
                       , ("sot", "sotho,")
                       , ("spa", "spanish")
                       , ("sqi", "albanian")
                       , ("srd", "sardinian")
                       , ("srn", "sranan")
                       , ("srp", "serbian")
                       , ("srr", "serer")
                       , ("ssa", "nilo-saharan")
                       , ("ssw", "swati")
                       , ("suk", "sukuma")
                       , ("sun", "sundanese")
                       , ("sus", "susu")
                       , ("sux", "sumerian")
                       , ("swa", "swahili")
                       , ("swe", "swedish")
                       , ("syc", "classical")
                       , ("syr", "syriac")
                       , ("tah", "tahitian")
                       , ("tai", "tai")
                       , ("tam", "tamil")
                       , ("tat", "tatar")
                       , ("tel", "telugu")
                       , ("tem", "timne")
                       , ("ter", "tereno")
                       , ("tet", "tetum")
                       , ("tgk", "tajik")
                       , ("tgl", "tagalog")
                       , ("tha", "thai")
                       , ("tig", "tigre")
                       , ("tir", "tigrinya")
                       , ("tiv", "tiv")
                       , ("tkl", "tokelau")
                       , ("tlh", "klingon")
                       , ("tli", "tlingit")
                       , ("tmh", "tamashek")
                       , ("tog", "tonga")
                       , ("ton", "tonga")
                       , ("tpi", "tok")
                       , ("tsi", "tsimshian")
                       , ("tsn", "tswana")
                       , ("tso", "tsonga")
                       , ("tuk", "turkmen")
                       , ("tum", "tumbuka")
                       , ("tup", "tupi")
                       , ("tur", "turkish")
                       , ("tut", "altaic")
                       , ("tvl", "tuvalu")
                       , ("twi", "twi")
                       , ("tyv", "tuvinian")
                       , ("udm", "udmurt")
                       , ("uga", "ugaritic")
                       , ("uig", "uighur")
                       , ("ukr", "ukrainian")
                       , ("umb", "umbundu")
                       , ("und", "undetermined")
                       , ("urd", "urdu")
                       , ("uzb", "uzbek")
                       , ("vai", "vai")
                       , ("ven", "venda")
                       , ("vie", "vietnamese")
                       , ("vol", "volap\xc3\xbck")
                       , ("vot", "votic")
                       , ("wak", "wakashan")
                       , ("wal", "wolaitta")
                       , ("war", "waray")
                       , ("was", "washo")
                       , ("wen", "sorbian")
                       , ("wln", "walloon")
                       , ("wol", "wolof")
                       , ("xal", "kalmyk")
                       , ("xho", "xhosa")
                       , ("yao", "yao")
                       , ("yap", "yapese")
                       , ("yid", "yiddish")
                       , ("yor", "yoruba")
                       , ("ypk", "yupik")
                       , ("zap", "zapotec")
                       , ("zbl", "blissymbols")
                       , ("zen", "zenaga")
                       , ("zha", "zhuang")
                       , ("zho", "chinese")
                       , ("znd", "zande")
                       , ("zul", "zulu")
                       , ("zun", "zuni")
                       , ("zza", "zaza")]

-- TODO: ADD CODE
langCursToLanguage :: Cursor -> [Language]
langCursToLanguage curs =
  let langsText = child curs >>=
        element (X.Name "languageTerm" (Just modsNS) Nothing) >>=
        attributeIs (X.Name "type" Nothing Nothing) "text" >>=
        descendant >>=
        content >>=
        (\l -> [T.pack $ toLocale $ T.unpack l])
      langsCode = child curs >>=
        element (X.Name "languageTerm" (Just modsNS) Nothing) >>=
        attributeIs (X.Name "type" Nothing Nothing) "code" >>=
        attributeIs (X.Name "authority" Nothing Nothing) "iso6039-2b" >>=
        descendant >>=
        content >>=
        (\l -> case M.lookup l iso60392b of
            Just v -> [T.pack $ toLocale $ T.unpack v]
            Nothing -> [l])
  in
    map Language $ langsCode ++ langsText
  
subjCursorToSubject :: Cursor -> Subject
subjCursorToSubject subjCurs =
  let topics = child subjCurs >>=
        element (X.Name "topic" (Just modsNS) Nothing) >>=
        descendant >>=
        content
      geographic = child subjCurs >>=
        element (X.Name "geographic" (Just modsNS) Nothing) >>=
        descendant >>=
        content
  in
    Subject { subjTopic = topics
            , subjGeographic = geographic
            }

-- toRefDate :: Text -> RefDate
-- toRefDate txt
--   -- | Just ('c', txt') <- T.uncons txt =
--   --     let rd = toRefDate txt' in rd {circa = True}
--   | T.all isDigit txt =
--       RefDate (Literal $ T.unpack txt) mempty mempty mempty mempty False
--   | otherwise = 
--       RefDate mempty mempty mempty mempty (Literal $ T.unpack txt) False
      
origCursToOriginInfo :: Cursor -> OriginInfo
origCursToOriginInfo origCurs =
  let origPlace' = child origCurs >>=
        element (X.Name "place" (Just modsNS) Nothing) >>=
        child >>=
        element (X.Name "placeTerm" (Just modsNS) Nothing) >>=
        attributeIs (X.Name "type" Nothing Nothing) "text" >>=
        descendant >>=
        content
      origPublisher' = child origCurs >>=
        element (X.Name "publisher" (Just modsNS) Nothing) >>=
        descendant >>=
        content
      origIssued' = child origCurs >>=
        element (X.Name "dateIssued" (Just modsNS) Nothing) >>=
        descendant >>=
        content >>=
        (parseDate . T.unpack)
      origCreated' = child origCurs >>=
        element (X.Name "dateCreated" (Just modsNS) Nothing) >>=
        descendant >>=
        content >>=
        (parseDate . T.unpack)
      origEdition' = child origCurs >>=
        element (X.Name "edition" (Just modsNS) Nothing) >>=
        descendant >>=
        content
      origIssuance' =
        let mkIssuance t = case T.toLower t of
                            "continuing" -> [Continuing]
                            "monographic" -> [Monographic]
                            _             -> []
        in
          child origCurs >>=
          element (X.Name "issuance" (Just modsNS) Nothing) >>=
          descendant >>=
          content >>=
          mkIssuance
        

  in
    OriginInfo { origPlace = origPlace'
               , origPublisher = origPublisher'
               , origIssued = origIssued'
               , origCreated = origCreated'
               , origEdition = origEdition'
               , origIssuance = origIssuance'
               }

-- Note: I'm removing the types that don't have an obvious marcrelator
-- code or name right now. I think they were originally put in with
-- custom types. We can return to that.
data Role = Author
          -- | CollectionEditor
          | Composer
          -- | ContainerAuthor
          | Director
          | Editor
          -- | EditorialDirector
          | Illustrator
          | Interviewer
          -- | OriginalAuthor
          | Recipient
          -- | ReviewedAuthor
          | Translator
          deriving (Show, Eq)

modsNS :: Text
modsNS = "http://www.loc.gov/mods/v3"

codeToRole :: Text -> Maybe Role
codeToRole code = case code of
  "aut" -> Just Author
  "cmp" -> Just Composer
  "drt" -> Just Director
  "edt" -> Just Editor
  "ill" -> Just Illustrator
  "ivr" -> Just Interviewer
  "rcp" -> Just Recipient
  "trl" -> Just Translator
  _     -> Nothing

textToRole :: Text -> Maybe Role
textToRole txt = case T.toLower txt of
  "author"      -> Just Author
  "creator"     -> Just Author
  "composer"    -> Just Composer
  "director"    -> Just Director
  "editor"      -> Just Editor
  "illustrator" -> Just Illustrator
  "interviewer" -> Just Interviewer
  "addressee"   -> Just Recipient
  "translator"  -> Just Translator
  _             -> Nothing

roleCursToRole :: Cursor -> Maybe Role
roleCursToRole roleCurs =
  let roleTerms = child roleCurs >>=
        element (X.Name "roleTerm" (Just modsNS) Nothing)
      roleCodes = roleTerms >>=
        attributeIs (X.Name "type" Nothing Nothing) "code" >>=
        descendant >>=
        content
      roleTexts = roleTerms >>=
        attributeIs (X.Name "type" Nothing Nothing) "text" >>=
        descendant >>=
        content
      roleTexts' = roleTerms >>=
        doesntHaveAttribute (X.Name "type" Nothing Nothing) >>=
        descendant >>=
        content
  in
    listToMaybe $
    mapMaybe codeToRole roleCodes ++
    mapMaybe textToRole (roleTexts ++ roleTexts')

doesntHaveAttribute :: X.Name -> Cursor -> [Cursor]
doesntHaveAttribute nm c = case node c of
  X.NodeElement e | Nothing <- M.lookup nm (X.elementAttributes e) -> [c]
  _ -> []

periodAfterInitial :: Text -> Text
periodAfterInitial txt = case T.uncons txt of
  Just (c, "") | isUpper c -> txt `T.snoc` '.'
  _ -> txt


nameCursToName :: Cursor -> Name
nameCursToName nameCurs =
  let nameParts = child nameCurs >>=
        element (X.Name "namePart" (Just modsNS) Nothing)
      givenName' = nameParts >>=
        attributeIs (X.Name "type" Nothing Nothing) "given" >>=
        descendant >>=
        content
      familyName' = nameParts >>=
        attributeIs (X.Name "type" Nothing Nothing) "family" >>=
        descendant >>=
        content
      literalName' = nameParts >>=
        doesntHaveAttribute (X.Name "type" Nothing Nothing) >>=
        descendant >>=
        content
      agent = emptyAgent{ familyName = case familyName' of
                            [] -> mempty
                            x : _ -> fromText x
                        , givenName = map (fromText . periodAfterInitial) givenName'
                        -- If there's more than one literal, we can
                        -- join them with commas.
                        , literal = fromText $ T.intercalate ", " literalName'
                        }
      roles = child nameCurs >>=
        element (X.Name "role" (Just modsNS) Nothing)
  in Name { nName = agent
          , nRoles = mapMaybe roleCursToRole roles
          }
                    
textToRefType :: Text -> RefType
textToRefType txt = case T.toLower txt of
  "article" -> Article
  "magazine article" -> ArticleMagazine
  "newspaper article" -> ArticleNewspaper
  "journal article" -> ArticleJournal
  "bill" -> Bill
  "book" -> Book
  "broadcast" -> Broadcast
  "dataset" ->   Dataset
  "entry"   -> Entry
  "dictionary entry"   -> EntryDictionary
  "encyclopedia entry"   -> EntryEncyclopedia
  "figure"   -> Figure
  "graphic"   -> Graphic
  "interview"   -> Interview
  "legislation"   -> Legislation
  "legal case"   -> LegalCase
  "manuscript"   -> Manuscript
  "map"   -> Map
  "motion picture"   -> MotionPicture
  "musical score"   -> MusicalScore
  "pamphlet"   -> Pamphlet
  "conference paper" -> PaperConference
  "patent"   -> Patent
  "post"   -> Post
  "weblog post"   -> PostWeblog
  "personal communication"   -> PersonalCommunication
  "report"   -> Report
  "review"   -> Review
  "book review"   -> ReviewBook
  "song"   -> Song
  "speech"   -> Speech
  "thesis"   -> Thesis
  "treaty"   -> Treaty
  "web page" -> Webpage
  _ -> Book

capitalize :: Text -> Text
capitalize txt = case T.uncons txt of
  Just (c, txt') -> toUpper c `T.cons` txt'
  Nothing        -> txt

splitTitle :: Text -> (Text, Text)
splitTitle ti
  | (ti', st) <- T.break (':' ==) ti
  , Just st' <- T.strip <$> T.stripPrefix ": " st = (ti', st')
  | otherwise = (ti, mempty)
  
tiCursToTitleInfo :: Cursor -> TitleInfo
tiCursToTitleInfo ti =
  let titleText = child ti >>=
                  element (X.Name "title" (Just modsNS) Nothing) >>=
                  descendant >>=
                  content
      subTitleText = child ti >>=
                     element (X.Name "subTitle" (Just modsNS) Nothing) >>=
                     descendant >>=
                     content
      abbrevText = attributeIs (X.Name "type" Nothing Nothing) "abbreviated" ti >>=
                   child >>=
                   element (X.Name "title" (Just modsNS) Nothing) >>=
                   descendant >>=
                   content
  in
    case subTitleText of
      [] -> TitleInfo { titleInfoTitle = map fst splits
                      , titleInfoSubTitle = map snd splits
                      , titleInfoAbbreviated = abbrevText
                      }
        where splits = map splitTitle titleText
      _  -> TitleInfo { titleInfoTitle = titleText
                      , titleInfoSubTitle = subTitleText
                      , titleInfoAbbreviated = abbrevText
                      }
         
recInfoCursorToRecInfo :: Cursor -> RecordInfo
recInfoCursorToRecInfo curs =
  let ident = child curs >>=
        element (X.Name "recordIdentifier" (Just modsNS) Nothing) >>=
        child >>=
        descendant >>=
        content
      lang = child curs >>=
        element (X.Name "languageOfCataloging" (Just modsNS) Nothing) >>=
        langCursToLanguage
        -- child >>=
        -- element (X.Name "languageTerm" (Just modsNS) Nothing) >>=
        -- descendant >>=
        -- content
  in
    RecordInfo { recInfoId = ident
               , recInfoLang = lang
               }

locationCursorToLocation :: Cursor -> Location
locationCursorToLocation curs = 
  let url' = child curs >>=
        element (X.Name "url" (Just modsNS) Nothing) >>=
        descendant >>=
        content
  in
    Location {locUrl = url' }

noteCursorToNote :: Cursor -> Note
noteCursorToNote curs =
  let attr = listToMonoid $ attribute (X.Name "type" Nothing Nothing) curs
      cont = listToMonoid $ descendant curs >>= content
  in
    case T.toLower attr of
      "annotation" -> Annotation cont
      _            -> AnyNote cont

modsCursToModsReference :: Cursor -> ModsReference
modsCursToModsReference modsCurs =
  let mID = listToMonoid $ attribute (X.Name "ID" Nothing Nothing) modsCurs

      titles = map tiCursToTitleInfo $ 
        child modsCurs >>=
        element (X.Name "titleInfo" (Just modsNS) Nothing)

      names = map nameCursToName $
        child modsCurs >>=
        element (X.Name "name" (Just modsNS) Nothing)

      genres = child modsCurs >>=
        element (X.Name "genre" (Just modsNS) Nothing) >>=
        descendant >>=
        content

      originInfo = map origCursToOriginInfo $
        child modsCurs >>=
        element (X.Name "originInfo" (Just modsNS) Nothing)

      lang = child modsCurs >>=
        element (X.Name "language" (Just modsNS) Nothing) >>=
        langCursToLanguage

      abstr = child modsCurs >>=
        element (X.Name "abstract" (Just modsNS) Nothing) >>=
        descendant >>=
        content >>=
        (\t -> [Abstract t])

      note' = map noteCursorToNote $ 
        child modsCurs >>=
        element (X.Name "note" (Just modsNS) Nothing)

      subj = map subjCursorToSubject $
        child modsCurs >>=
        element (X.Name "subject" (Just modsNS) Nothing)


      related = child modsCurs >>=
        element (X.Name "relatedItem" (Just modsNS) Nothing) >>=
        relCursorToRelItem

      ident = child modsCurs >>=
        element (X.Name "identifier" (Just modsNS) Nothing) >>=
        identCursToIdentifier

      part = map partCursToPart $
        child modsCurs >>= 
        element (X.Name "part" (Just modsNS) Nothing)

      recInfo = map recInfoCursorToRecInfo $ 
        child modsCurs >>=
        element (X.Name "recordInfo" (Just modsNS) Nothing)

      loc = map locationCursorToLocation $ 
        child modsCurs >>=
        element (X.Name "location" (Just modsNS) Nothing)
        

  in
    ModsReference { modsID = mID
                  , modsTitleInfo = titles
                  , modsNames = names
                  , modsGenre = genres
                  , modsOriginInfo = originInfo
                  , modsLanguage = lang
                  , modsAbstract = abstr
                  , modsNote = note'
                  , modsSubject = subj
                  , modsRelated = related
                  , modsIdent = ident
                  , modsPart = part
                  , modsRecordInfo = recInfo
                  , modsLocation = loc
                  }

docToModsReferences :: X.Document -> [ModsReference]
docToModsReferences doc =
  map modsCursToModsReference $ 
  child (fromDocument doc) >>=
  element (X.Name "mods" (Just modsNS) Nothing)
    
-------------------------------------------------------
{-
The second  part involves transforming  the ModsReference type  into a
Reference type.
--}

-- We follow the bibutils behavior, of taking the first available
-- title and the first available subtitle, even if they are in
-- different titleInfo fields
resolveTitleInfos :: [TitleInfo] -> TitleInfo
resolveTitleInfos tis =
  let title' = dropWhile (mempty==) $
              concatMap titleInfoTitle tis
      subtitle = dropWhile (mempty==) $
                 concatMap titleInfoSubTitle tis
      abbrev = dropWhile (mempty==) $
               concatMap titleInfoAbbreviated tis
                 
  in TitleInfo { titleInfoTitle = title'
               , titleInfoSubTitle = subtitle
               , titleInfoAbbreviated = abbrev
               }


tiToFullTitle :: TitleInfo -> Text
tiToFullTitle ti =
  let t = listToMonoid $ titleInfoTitle ti
      st = listToMonoid $ titleInfoSubTitle ti
  in
    if t == mempty
    then mempty
    else if st == mempty
         then t
         else t <> ": " <> capitalize st

tiToShortTitle :: TitleInfo -> Text
tiToShortTitle ti | null $ titleInfoAbbreviated ti =
  let t = listToMonoid $ titleInfoTitle ti
      st = listToMonoid $ titleInfoSubTitle ti
  in case () of
    _ | t /= mempty && st /= mempty -> t
      | (t', st') <- T.span (':'/=) t
      , t' /= mempty
      , st' /= mempty -> t'
      | otherwise -> mempty
tiToShortTitle ti = listToMonoid $ titleInfoAbbreviated ti

-- For the time being, we're just going to imitate bibutils. We can do
-- better in the future (ie, place 'a', 'b', ..., 'aa', 'ab' on
-- duplicate keys) though. Note that this function might give an empty
-- ref. That's okay -- we'll fill in the refs on a second pass (once
-- we have the author and year available, and can zip in an index
-- number for a fallback).

getId :: ModsReference -> Text
getId modsRef | x <- modsID modsRef
              , not $ T.null x = x
              | ri : _ <- modsRecordInfo modsRef
              , x <- listToMonoid $ recInfoId ri
              , not $ T.null x = x
              | x <- listToMonoid $
                     map identText $
                     filter (\i -> identType i == CiteKey) $
                     modsIdent modsRef
              , not $ T.null x = x
              | otherwise = mempty


-- A function for an improved id (that avoids duplicates), roughly in
-- line with Chicago author-year style.

-- getSuffix :: Int -> Text
-- getSuffix n =
--   let f :: Int -> Text
--       f 0 = mempty
--       f num = T.singleton $ chr $ num + 96
--       (q, r) = quotRem n 26
--   in
--     if q > 26
--     then getSuffix q <> (f r)
--     else f q <> f r

-- If a name has several roles, we split it into one role per name.
individuateNames :: [Name] -> [Name]
individuateNames names = concatMap f names
  where f name = if null $ nRoles name
                 then map (\r -> Name (nName name) [r]) [Author]
                 else map (\r -> Name (nName name) [r]) (nRoles name)

getAgentsByRole :: Role -> [Name] -> [Agent]
getAgentsByRole role names =
  filter (emptyAgent/=) $ map nName $ filter f names
  where f nm = case nRoles nm of
                 [] | role == Author -> True
                 [] -> False
                 r : _ -> r == role

-- For switching Formatted names. We might as well do this right, but
-- we're mainly interested in plain strings, so we're not going to
-- stress about Notes.
ilToString :: P.Inline -> String
ilToString (P.Str s) = s
ilToString (P.Emph ils) = ilsToString ils
ilToString (P.Strong ils) = ilsToString ils
ilToString (P.Strikeout ils) = ilsToString ils
ilToString (P.Superscript ils) = ilsToString ils
ilToString (P.Subscript ils) = ilsToString ils
ilToString (P.SmallCaps ils) = ilsToString ils
ilToString (P.Quoted P.SingleQuote ils) = "‘" ++ ilsToString ils ++ "’"
ilToString (P.Quoted P.DoubleQuote ils) = "“" ++ ilsToString ils ++ "”"
ilToString (P.Cite _ ils) = ilsToString ils
ilToString (P.Code _ s) = s
ilToString P.Space = " "
ilToString P.SoftBreak = "\n"
ilToString P.LineBreak = "\n"
ilToString (P.Math _ s) = s
ilToString (P.RawInline _ s) = s
ilToString (P.Link _ ils _) = ilsToString ils
ilToString (P.Image _ ils _) = ilsToString ils
ilToString (P.Note _) = ""
ilToString (P.Span _ ils) = ilsToString ils

ilsToString :: [P.Inline] -> String
ilsToString = concatMap ilToString


-- If we can, we change a ref to author ++ year ("Rosenthal2016"). If
-- we can't we fall back to RefN. Note we're following bibutils'
-- behavior here.
fixBlankRef :: Int -> Reference -> Reference
fixBlankRef _ ref | refId ref /= mempty = ref
fixBlankRef n ref =
  let authorName = case author ref of
        [] -> mempty
        agnt : _ | (Formatted ils) <- familyName agnt
                 , s <- ilsToString ils
                 , s' <- filter (not . isSpace) s
                 , not $ null s' -> s'
                 | (Formatted ils) : _ <- givenName agnt
                 , s <- ilsToString ils
                 , s' <- filter (not . isSpace) s
                 , not $ null s' -> s'
                 | otherwise -> ""
      yr = case issued ref of
        [] -> mempty
        date : _ | Literal y <- year date, not $ null y -> y
                 | otherwise -> ""
      newRefId = if authorName == "" || yr == ""
                 then Literal $ "ref" ++ (show n)
                 else Literal $ authorName ++ yr
  in ref { refId = newRefId }

fixBlankRefs :: [Reference] -> [Reference]
fixBlankRefs refs = zipWith fixBlankRef [1..] refs


getChildRefType :: ModsReference -> ModsReference -> RefType
getChildRefType hostModsRef childModsRef =
  let childGenre' = T.toLower $ listToMonoid $ modsGenre childModsRef
      hostGenre' = T.toLower $ listToMonoid $ modsGenre hostModsRef
      mChildIssuance = listToMaybe $
                       concatMap origIssuance $
                       modsOriginInfo childModsRef
      mHostIssuance = listToMaybe $
                      concatMap origIssuance $
                      modsOriginInfo hostModsRef
  in case () of
    _ | hostGenre' == "periodical" -> ArticleJournal
      | hostGenre' == "academic journal" -> ArticleJournal
      | hostGenre' == "magazine" -> ArticleMagazine
      | hostGenre' == "newspaper" -> ArticleNewspaper
      | childGenre' == "article" -> ArticleJournal
      | hostGenre' == "collection" -> Chapter
      | childGenre' == "conference publication" -> Chapter
      | hostGenre' == "book" -> Chapter
      | childGenre' == "book" -> Book
      | Just Monographic <- mChildIssuance -> Book
      | Just Monographic <- mHostIssuance -> Chapter
      | otherwise -> Chapter


getHostModsReference :: ModsReference -> Maybe ModsReference
getHostModsReference modsRef =
  listToMaybe $
  map relItemModsRef $ 
  filter (\ri -> relItemType ri `elem` [Host, Series]) $
  modsRelated modsRef

replaceIfNonEmpty :: (Monoid a, Eq a)
                  => (Reference -> a)
                  -> Reference
                  -> Reference
                  -> a
replaceIfNonEmpty f hostRef ref =
  if f hostRef /= mempty
  then f hostRef
  else f ref

isStandAlone :: Reference -> Bool
isStandAlone ref = publisher ref /= mempty

withHostInfo :: ModsReference -> ModsReference -> Reference
withHostInfo hostModsRef childModsRef =
  let childRef = modsRefToReference' childModsRef
      childRefType = getChildRefType hostModsRef childModsRef
      hostRef = modsRefToReference' hostModsRef
      hostGenre = listToMonoid $ modsGenre hostModsRef
      containerToCollection ref =
        if isStandAlone childRef
        then ref { collectionTitle = containerTitle ref
                 , containerTitle = mempty }
        else ref
  in
    containerToCollection $ 
    childRef { containerAuthor = author hostRef
      , title = if childRefType == Chapter && hostGenre /= "collection"
                then title hostRef
                else title childRef
      , titleShort = if childRefType == Chapter && hostGenre /= "collection"
                     then titleShort hostRef
                     else titleShort childRef
      , containerTitle =
          if childRefType == Chapter && hostGenre /= "collection"
          then ""
          else title hostRef
      -- , containerTitleShort =
      --     if childRefType == Chapter && hostGenre /= "collection"
      --     then ""
      --     else titleShort hostRef
      , chapterNumber =
          if childRefType == Chapter && hostGenre /= "collection"
          then title childRef
          else chapterNumber childRef
      , author = author childRef ++ author hostRef 
      , editor = editor childRef ++ editor hostRef
      , translator = translator childRef ++ translator hostRef
      , composer = composer childRef ++ composer hostRef
      , director = director childRef ++ director hostRef
      , illustrator = illustrator childRef ++ illustrator hostRef
      , interviewer = interviewer childRef ++ interviewer hostRef
      , recipient = recipient childRef ++ recipient hostRef
      , publisher = replaceIfNonEmpty publisher hostRef childRef
      , publisherPlace = replaceIfNonEmpty publisherPlace hostRef childRef
      , volume = replaceIfNonEmpty volume hostRef childRef
      , issue = replaceIfNonEmpty issue hostRef childRef
      , page = replaceIfNonEmpty page hostRef childRef
      , issued = replaceIfNonEmpty issued hostRef childRef
      , note = replaceIfNonEmpty note hostRef childRef
      , keyword = replaceIfNonEmpty keyword hostRef childRef

      , isbn = replaceIfNonEmpty isbn hostRef childRef
      , issn = replaceIfNonEmpty issn hostRef childRef
      , url = replaceIfNonEmpty url hostRef childRef

      , refType = childRefType

      -- ...
      }

  

partsToPage :: [Part] -> Text
partsToPage parts = listToMonoid $
                    map rangeToText $
                    concatMap partPageRange parts

partsToDetail :: DetailType -> [Part] -> Text
partsToDetail typ parts = listToMonoid $
                      concatMap detailNumber $ 
                      filter (\d -> detailType d == typ) $
                      concatMap partDetails parts


subjectsToSubjText :: [Subject] -> [Text]
subjectsToSubjText subjs =
  nub $ map T.strip $ concatMap (\s -> subjTopic s ++ subjGeographic s) subjs

getIdentByType :: IdentifierType -> [Identifier] -> Literal
getIdentByType typ ident = Literal $
                     filter ('\n'/=) $
                     T.unpack $
                     T.strip $
                     listToMonoid $
                     map identText $
                     filter (\i -> identType i == typ) ident

modsRefToReference' :: ModsReference -> Reference
modsRefToReference' modsRef =
  let names = individuateNames $ modsNames modsRef
      titleInfo = resolveTitleInfos $ modsTitleInfo modsRef
      genre' = listToMonoid $ modsGenre modsRef
  in
      emptyReference{ refId = Literal $ T.unpack $  getId modsRef
                  , title = fromText $ tiToFullTitle titleInfo
                  , titleShort = fromText $ tiToShortTitle titleInfo 
                  , author = getAgentsByRole Author names
                  , editor = getAgentsByRole Editor names
                  , translator = getAgentsByRole Translator names
                  , composer = getAgentsByRole Composer names
                  , director = getAgentsByRole Director names
                  , illustrator = getAgentsByRole Illustrator names
                  , interviewer = getAgentsByRole Interviewer names
                  , recipient = getAgentsByRole Recipient names
                  , abstract = fromText $
                               T.intercalate "; " $ 
                               map fromAbstract $
                               modsAbstract modsRef
                  , note = fromText $
                           listToMonoid $
                           modsNote modsRef >>=
                           (\n -> case n of
                                    AnyNote txt -> [txt]
                                    _ -> [])
                  , annote = fromText $
                             listToMonoid $
                             modsNote modsRef >>=
                             (\n -> case n of
                                      Annotation txt -> [txt]
                                      _ -> [])

                  , keyword = fromText $
                              T.intercalate "; " $
                              subjectsToSubjText $
                              modsSubject modsRef

                  , issued = nub $ 
                             (concatMap partDate $ modsPart modsRef) ++
                             (concatMap origIssued $ modsOriginInfo modsRef)
                  , publisher = fromText $
                                listToMonoid $
                                concatMap origPublisher $ 
                                modsOriginInfo modsRef
                  , publisherPlace = fromText $
                                     T.intercalate "; " $
                                     concatMap origPlace $ 
                                     modsOriginInfo modsRef
                  , edition = fromText $
                              listToMonoid $
                              concatMap origEdition $ 
                              modsOriginInfo modsRef
                  , refType = textToRefType genre'

                  , page = fromText $
                           partsToPage $
                           modsPart modsRef
                  , volume = fromText $
                             partsToDetail VolumeDetail $
                             modsPart modsRef

                  , issue = fromText $
                             partsToDetail IssueDetail $
                             modsPart modsRef

                  , isbn = getIdentByType ISBN $ modsIdent modsRef
                  , issn = getIdentByType ISSN $ modsIdent modsRef
                  , url =
                      let locUrl' = listToMonoid $
                                    concatMap locUrl $
                                    modsLocation modsRef
                      in
                        if locUrl' == mempty
                        then getIdentByType URI $  modsIdent modsRef
                        else Literal $ T.unpack $ T.strip $ locUrl'

                  , language = Literal $
                               T.unpack $
                               listToMonoid $
                               ((map fromLanguage $ modsLanguage modsRef)
                                ++
                                (map fromLanguage $ concatMap recInfoLang $ modsRecordInfo modsRef))
                  }

modsRefToReference :: ModsReference -> Reference
modsRefToReference modsRef =
  case getHostModsReference modsRef of
    Just hostModsRef -> withHostInfo hostModsRef modsRef
    Nothing          -> modsRefToReference' modsRef
        
docToReferences :: X.Document -> [Reference]
docToReferences doc =
  fixBlankRefs $
  map modsRefToReference $
  docToModsReferences doc

readModsString :: String -> [Reference]
readModsString s = case X.parseLBS X.def $ UTF8.fromStringLazy s of
  Right doc -> docToReferences doc
  Left e -> error $ show e

readModsFile :: FilePath -> IO [Reference]
readModsFile fp = readModsString <$> readFile fp



