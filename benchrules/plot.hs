module Main where

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11

import qualified Graphics.Gnuplot.MultiPlot as MultiPlot

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import Data.Monoid (mappend, mconcat, )

import System.Environment
import System.Directory
import Data.List.Utils

histogram2d :: [(String, [Double])] -> Frame.T (Graph2D.T Int Double)
histogram2d dp =
   Frame.cons (
      Opts.title "Passwords cracked per rules" $
      Histogram.clusteredGap 2 $
      Opts.boxwidthAbsolute 0.9 $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      Opts.deflt) $
   mconcat $
   map (\(title,dat) ->
      fmap (Graph2D.lineSpec (LineSpec.title title LineSpec.deflt)) $
      Plot2D.list Graph2D.listLines dat) dp

getstats :: String -> (String, String) -> IO (String, [Double])
getstats dirname (fname, lname) = do
    mdata <- fmap read (readFile ("results/" ++ dirname ++ "/" ++ fname))
    return (lname, mdata)

main :: IO ()
main = do
    (prefixname:dirname:_) <- getArgs
    let fprefix = '-' : prefixname
        fl = length fprefix
        rp = reverse . drop fl . reverse
    mystats <- fmap (map (\x -> (x, rp x)) . filter (endswith fprefix)) (getDirectoryContents ("results/" ++ dirname)) >>= mapM (getstats dirname)
    Plot.plot X11.cons (histogram2d mystats)
    print "done"

