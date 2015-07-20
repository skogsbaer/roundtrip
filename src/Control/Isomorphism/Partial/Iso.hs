module Control.Isomorphism.Partial.Iso (

    Iso, unsafeMakeIso, unsafeMakeIso', unsafeMakeNamedIso, unsafeMakeNamedIsoL
  , unsafeMakeNamedIsoR, unsafeMakeNamedIsoLR, isoRL, isoLR, isoName
  , isoShowSL, isoShowSR, isoShowL, isoShowR
  , isoFailedErrorMessageL, isoFailedErrorMessageR

) where

data Iso a b = Iso {
      isoLR     :: a -> Maybe b
    , isoRL     :: b -> Maybe a
    , isoName   :: String
    , isoShowSL :: Maybe (a -> ShowS)
    , isoShowSR :: Maybe (b -> ShowS)
    }

unsafeMakeIso :: (alpha -> Maybe beta) -> (beta -> Maybe alpha) -> Iso alpha beta
unsafeMakeIso f g = Iso { isoLR = f
                        , isoRL = g
                        , isoName = "?"
                        , isoShowSL = Nothing
                        , isoShowSR = Nothing }

unsafeMakeIso' :: String -> Maybe (a -> ShowS) -> Maybe (b -> ShowS)
               -> (a -> Maybe b) -> (b -> Maybe a) -> Iso a b
unsafeMakeIso' name showSL showSR f g = Iso { isoLR = f
                                            , isoRL = g
                                            , isoName = name
                                            , isoShowSL = showSL
                                            , isoShowSR = showSR }

unsafeMakeNamedIso :: String -> (alpha -> Maybe beta) -> (beta -> Maybe alpha) -> Iso alpha beta
unsafeMakeNamedIso name f g = Iso { isoLR = f
                                  , isoRL = g
                                  , isoName = name
                                  , isoShowSL = Nothing
                                  , isoShowSR = Nothing }

unsafeMakeNamedIsoL :: Show alpha
                    => String -> (alpha -> Maybe beta) -> (beta -> Maybe alpha) -> Iso alpha beta
unsafeMakeNamedIsoL name f g = Iso { isoLR = f
                                   , isoRL = g
                                   , isoName = name
                                   , isoShowSL = Just shows
                                   , isoShowSR = Nothing }

unsafeMakeNamedIsoR :: Show beta
                    => String -> (alpha -> Maybe beta) -> (beta -> Maybe alpha) -> Iso alpha beta
unsafeMakeNamedIsoR name f g = Iso { isoLR = f
                                   , isoRL = g
                                   , isoName = name
                                   , isoShowSL = Nothing
                                   , isoShowSR = Just shows }

unsafeMakeNamedIsoLR :: (Show alpha, Show beta)
                     => String -> (alpha -> Maybe beta) -> (beta -> Maybe alpha) -> Iso alpha beta
unsafeMakeNamedIsoLR name f g = Iso { isoLR = f
                                    , isoRL = g
                                    , isoName = name
                                    , isoShowSL = Just shows
                                    , isoShowSR = Just shows }

isoShowL :: Iso a b -> Maybe (a -> String)
isoShowL iso = makeShow (isoShowSL iso)

isoShowR :: Iso a b -> Maybe (b -> String)
isoShowR iso = makeShow (isoShowSR iso)

makeShow :: Maybe (a -> ShowS) -> Maybe (a -> String)
makeShow Nothing = Nothing
makeShow (Just f) = Just (\x -> f x "")

isoFailedErrorMessageL :: Iso a b -> a -> String
isoFailedErrorMessageL iso = isoFailedErrorMessage (isoName iso) (isoShowSL iso)

isoFailedErrorMessageR :: Iso a b -> b -> String
isoFailedErrorMessageR iso = isoFailedErrorMessage (isoName iso) (isoShowSR iso)

isoFailedErrorMessage :: String -> Maybe (a -> ShowS) -> a -> String
isoFailedErrorMessage name mf x =
    "Isomorphism " ++ name ++ " failed" ++
    (case mf of
       Nothing -> ""
       Just f -> " on input " ++ f x "")
