{-# LANGUAGE CPP #-}

module Utils where

fgcol :: Color -> String
fgcol col = "\ESC[0" ++ show (30+col) ++ "m"

highlight :: String -> String
highlight s = "\ESC[7m" ++ s ++ normal
-- bold      s = "\ESC[1m" ++ s ++ normal
-- underline s = "\ESC[4m" ++ s ++ normal
normal :: String
normal    = "\ESC[0m"

type Color = Int

color :: Color -> String -> String
#if defined(mingw32_HOST_OS)
color _ s = s
#else
color c s = fgcol c ++ s ++ normal
#endif

red, blue :: Color
-- black = 0
red = 1
-- green = 2
blue = 6