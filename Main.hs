{-# LANGUAGE OverloadedStrings, Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core
import System.IO
import Control.Exception
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as C
import Network.Wreq
import qualified Network.Wreq.Session as S

loginURL :: String
loginURL = "https://cas.colgate.edu/cas/login"
regURL = "https://bannersv04.colgate.edu/prod/bwskfreg.P_AltPin"
bannerURL = "http://bannersv04.colgate.edu:10003/ssomanager/c/SSB"
pinURL = "https://bannersv04.colgate.edu/prod/bwskfreg.P_CheckAltPin"

termData = ["term_in" := (201502 :: Int)]
loginData username password lt = ["username"  := (username ::String),
                                  "password"  := (password ::String),
                                  "lt"        := (lt :: String),
                                  "execution" := ("e1s1" :: String),
                                  "_eventId"  := ("submit" ::String)]

getPassword :: IO String
getPassword = do
    putStr "Enter password: "
    hFlush stdout -- clear stdout so letters don't show
    pass <- withEcho False getLine
    putChar '\n'
    return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

main :: IO ()
main = S.withSession $ \sess -> do
    putStr "Enter your username: "
    uname <- getLine
    pass <- getPassword
    putStr "Enter your registration PIN: "
    pin' <- getLine
    let pin = (read pin' :: Int)
    sess <- portal_login sess uname pass
    --r1 <- S.get sess bannerURL
    --S.post sess regURL termData
    --r2 <- S.post sess pinURL ["pin" := (pin :: Int)]
    print $ r1 L.^. responseStatus

atTag tag = deep (isElem >>> hasName tag)

text = getChildren >>> getText

getLtVal = atTag "input" >>> 
    proc x -> do
        lt <- getAttrValue "value" <<< hasAttrValue "name" (=="lt") -< x
        returnA -< lt

portal_login sess username password = do
    loginReq <- S.get sess loginURL
    let sessionID = loginReq L.^. responseCookie "JSESSIONID" . cookieValue
    let page = C.unpack (loginReq L.^. responseBody)
    ltVal <- runX (readString [withValidate no, withParseHTML yes, withWarnings no] page >>> getLtVal)
    let lData = loginData username password (head ltVal)
    let postURL = loginURL ++ ";jsessionid=" ++ (show sessionID)
    req <- S.post sess postURL lData
    C.writeFile "checkLog.html" (req L.^. responseBody)
    return sess
