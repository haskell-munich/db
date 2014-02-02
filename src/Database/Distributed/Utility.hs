

module Database.Distributed.Utility where


import System.Console.ANSI

color :: Color -> String
color c = setSGRCode [SetColor Foreground Vivid c]

cyan, yellow, green, red, white :: String
yellow = color Yellow
cyan = color Cyan
green = color Green
red = color Red
white = color White


reset :: String
reset = setSGRCode [Reset]
