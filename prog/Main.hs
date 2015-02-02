module Main where

import Network.HEdiPlug
import System.Posix.Signals

main = do
   installHandler sigPIPE Ignore Nothing
   print "test"
   print =<< discover
   fakePlug
--    fakePlug2
   fakeProxy
