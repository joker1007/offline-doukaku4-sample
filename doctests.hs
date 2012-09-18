import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "Main.hs"]
