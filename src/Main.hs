import qualified HappyParser
import qualified AlexToken

main :: IO ()
main = do
  input <- getContents
  putStrLn "Input:"
  putStrLn input
  putStrLn "\nUsing Alex:"
  let tokens = AlexToken.scanTokens input
  putStrLn $ "Tokens: " ++ (show tokens)
  putStrLn $ "\nUsing Happy"
  let parse = HappyParser.parse tokens
  putStrLn $ (show parse)
