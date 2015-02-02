{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Network.HEdiPlug.XML where

import qualified Data.Map        as M
import           Prelude         hiding (readFile, writeFile)
import           Text.Hamlet.XML
import           Text.XML
import Data.String.QM

power = [qq|
<?xml version="1.0" encoding="UTF8"?><SMARTPLUG id="edimax"><CMD id="get"><NOW_POWER><Device.System.Power.LastToggleTime>20150131173226</Device.System.Power.LastToggleTime><Device.System.Power.NowCurrent>2.5892</Device.System.Power.NowCurrent><Device.System.Power.NowPower>582.64</Device.System.Power.NowPower><Device.System.Power.NowEnergy.Day>7.747</Device.System.Power.NowEnergy.Day><Device.System.Power.NowEnergy.Week>7.747</Device.System.Power.NowEnergy.Week><Device.System.Power.NowEnergy.Month>7.747</Device.System.Power.NowEnergy.Month></NOW_POWER></CMD></SMARTPLUG>
|]