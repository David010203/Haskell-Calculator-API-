{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import           Data.List.Split
import 		 Prelude

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/operation/#String Oper GET
|]

instance Yesod HelloWorld

getOper :: String -> Handler Html
getOper cal = do 
		 let list = split (oneOf "+:-*") cal
		 let a = read $ list !! 0
		 let sym = list !! 1
		 let b = read $ list !! 2
		 let sum = if(sym == "+") then show $ a + b else if(sym == "*") then show $ a * b else if(sym == "-") then show $ a - b else if(sym==":") then show $ a / b else show 0  
                 
		 defaultLayout [whamlet| 
 
 $forall x <- list
   <li>#{x}
 #{cal} = #{sum}
|]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello Worlwqd!|]

main :: IO ()
main = warp 3000 HelloWorld


