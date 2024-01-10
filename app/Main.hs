module Main where
import Coco

main :: IO ()
main = do
  coco <- readCoco "availability-dataset_v4/annotations/tests.json"
  print coco
