{-# LANGUAGE OverloadedStrings #-}
module Blog.Widgets.TagCloud ( boot
                             , update_model ) where

import qualified Blog.Model.Entry as B
import qualified Blog.FrontEnd.Urls as U
import qualified Blog.Constants as C
import Lucid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List ( maximum, group, sortBy, sort, intersperse ) 

import qualified Blog.BackEnd.Holder as H

boot :: B.Model -> IO (H.Holder String)
boot m = H.newHolder $ TL.unpack $ renderText $ tag_cloud (B.all_posts m) C.tags_to_show

update_model :: (H.Holder String) -> B.Model -> IO ()
update_model fh m = H.put fh $ TL.unpack $ renderText $ tag_cloud (B.all_posts m) C.tags_to_show

tag_cloud :: [B.Item] -> Int -> Html ()
tag_cloud = tag_cloud_ . sort . concat . (map B.tags)

d :: Int -> Double
d = fromIntegral

tag_cloud_ :: [String] -> Int -> Html ()
tag_cloud_ ts n = div_ [class_ "tagcloud"] $
    mconcat $ intersperse (toHtml (" " :: String)) $
        map (tag_atom . tag_tuple) ts''
  where
    ts' = group ts
    ts'' = take n $ (sortBy (flip $ snd >< compare) $ map (\y -> (head y, length y)) ts')
    max_count = maximum $ map snd ts''
    min_count = minimum $ map snd ts''
    relative_color = if max_count == min_count then
                         const "#7f7f7f"
                     else
                         \y -> hotness ((d $ y - min_count) / (d $ max_count - min_count))
    relative_size = if max_count == min_count then
                        const 100
                    else
                        \y -> 75 + ((75 * (y-min_count)) `div` (max_count - min_count))
    tag_tuple = \(x,y) -> (x, relative_color y, y, relative_size y)

(><) :: (x -> y) -> (y -> y -> z) -> x -> x -> z
(><) f g x1 x2 = g (f x1) (f x2)

tag_atom :: (String, String, Int, Int) -> Html ()
tag_atom (nm,c,n,sz) = span_ [class_ "tagcloud"] $
    span_ [style_ (T.pack $ "color: " ++ c ++ "; font-size: " ++ (show sz) ++ "%")] $
        a_ [href_ (T.pack $ U.posts_by_tag nm)] $
            toHtml $ (show n) ++ ":" ++ nm
                            

-- | Function to compute an HTML color value (three 0x00-0xFF hex digits
-- of RGB color space) from a [0,1] temperature.  The implementation is a
-- close approximation of a line connecting (0,100,100) and (270,100,70)
-- in HSB color space where one maps to the bright red (0,100,100) and zero
-- maps to the cool purple.
hotness :: Double -- ^ the hotness
        -> String
hotness a | a < 0 = error "Can't be cooler than zero."
hotness a | a > 1 = error "Can't be hotter than one."
hotness a = rgb_to_htmlcolor ((r x) + (rs x), g x, b x)
    where
      x = round(shape(a) * 270)
      r = lump (-120,-60,60,120) 200
      g = lump (0,60,180,240) 150
      b = lump (120,180,300,360) 120
      rs = lump (240,300,420,480) 200

-- Derived from (1/(5*a+1))-(a/6)
shape :: Double -> Double
shape a = (6-a-5*a*a)/(30*a+6)

-- | Ramp up, hold, ramp down and zero elsewhere function. 
lump :: (Int,Int,Int,Int) -- ^ four-tuple consisting of start of up ramp, end of up ramp, start of down ramp, end of down ramp
     -> Int -- ^ height for the hold portion
     -> Int -- ^ the input to the function
     -> Int
lump (su,eu,sd,ed) h x | x < su || x > ed = 0
                       | x > eu && x < sd = h
                       | x >= su && x <= eu = (h*(x-su)) `div` (eu-su)
                       | x >= sd && x <= ed = (h*(ed-x)) `div` (ed-sd)

digits :: String
digits = "0123456789ABCDEF"

hex :: [String]
hex = [ [digits!!a,digits!!b] | a <- [0..15], b <- [0..15] ]

rgb_to_htmlcolor :: (Int,Int,Int) -> String
rgb_to_htmlcolor (a,b,c) = "#" ++ (hex!!(clip a))
                           ++ (hex!!(clip b)) ++ (hex!!(clip c))
    where
      clip = \x -> min (max x 0) 255
