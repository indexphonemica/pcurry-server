{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Schema where

import           Data.Text                   (Text)
import           Database.Persist.Postgresql
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
                   Doculect json sql=doculects
                       inventoryId Text
                       languageName Text
                       glottocode Text
                       dialect Text Maybe
                       dialectName Text Maybe
                       notes Text Maybe
                       sourceBibkey Text Maybe
                       sourceUrl Text Maybe
                       sourceAuthor Text Maybe
                       sourceDoi Text Maybe
                       sourceTitle Text Maybe
                       sourceNumber Text Maybe
                       sourceYear Text Maybe
                       sourcePages Text Maybe
                       deriving Show Eq
                       UniqueIID inventoryId

                   Language json sql=languages
                       name Text Maybe
                       glottocode Text
                       iso6393 Text
                       family Text Maybe
                       genus Text Maybe
                       macroarea Text Maybe
                       latitude Text Maybe
                       longitude Text Maybe
                       UniqueGlottocode glottocode

                   Country json sql=countries
                       Id Text maxlen=255
                       name Text

                   Segment json sql=segments
                       phoneme Text Maybe
                       glyphId Text Maybe
                       segmentClass Text Maybe
                       tone Text Maybe
                       stress Text Maybe
                       syllabic Text Maybe
                       short Text Maybe
                       long Text Maybe
                       consonantal Text Maybe
                       sonorant Text Maybe
                       continuant Text Maybe
                       delayedRelease Text Maybe
                       approximant Text Maybe
                       tap Text Maybe
                       trill Text Maybe
                       nasal Text Maybe
                       lateralis Text Maybe
                       labial Text Maybe
                       round Text Maybe
                       labiodental Text Maybe
                       coronal Text Maybe
                       anterior Text Maybe
                       distributed Text Maybe
                       strident Text Maybe
                       dorsal Text Maybe
                       high Text Maybe
                       low Text Maybe
                       front Text Maybe
                       back Text Maybe
                       tense Text Maybe
                       retractedTongueRoot Text Maybe
                       advancedTongueRoot Text Maybe
                       periodicGlottalSource Text Maybe
                       epilaryngealSource Text Maybe
                       spreadGlottis Text Maybe
                       constrictedGlottis Text Maybe
                       fortis Text Maybe
                       ejective Text Maybe
                       implosive Text Maybe
                       click Text Maybe

                   LanguageCountry json sql=languages_countries
                       languageId LanguageId
                       countryId CountryId
                       Primary languageId countryId

                   DoculectSegment json sql=doculect_segments
                       doculectId DoculectId
                       segmentId SegmentId
                       marginal Int
                       loan Bool
                       UniqueDS doculectId segmentId

                   Allophone json sql=allophones
                       doculectSegmentId DoculectSegmentId
                       allophoneId SegmentId
                       variation Bool
                       compound Text
                       environment Text

                   User json
                       userFirstName Text
                       userLastName Text
                   |]
