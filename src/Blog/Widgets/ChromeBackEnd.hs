module Blog.Widgets.ChromeBackEnd where

import Blog.Model.Entry
import qualified Blog.BackEnd.Holder as H
import qualified Blog.Widgets.FlickrCollage as FlickrC
import qualified Blog.Widgets.TagCloud as TagC
import qualified Blog.Widgets.Delicious as Delicious
import Blog.Widgets.StreamOfConsciousness
import qualified Blog.BackEnd.HitTracker as HitT
import Blog.BackEnd.ModelChangeListener
import qualified Blog.Constants as C

data ChromeBackEnd = ChromeBackEnd { flickr_collage :: FlickrC.FlickrPhotos
                                   , tag_cloud_holder :: H.Holder String
                                   , delicious_controller :: Delicious.Controller
                                   , delicious_scheduler :: Delicious.Scheduler
                                   , driver :: Delicious.FixedFrequencyDriver 
                                   , soc :: SoCController
                                   , hit_tracker :: HitT.HitTracker }

instance ModelChangeListener ChromeBackEnd where
    handle_model_change cb m = TagC.update_model (tag_cloud_holder cb) m
                               >> Delicious.update_model (delicious_scheduler cb) m

boot :: Model -> IO ChromeBackEnd
boot m = do { f <- FlickrC.boot C.flickr_user C.flickr_api_key
            ; tc <- TagC.boot m
            ; ht <- HitT.boot
            ; dc <- Delicious.boot_dc
            ; s <- Delicious.boot_s dc m
            ; ffd <- Delicious.start_driver s (10^7)
            ; socc <- start_soc 20
            ; tweet_worker <- start_twitter_tweets socc C.twitter_user C.twitter_pass
            ; reply_worker <- start_twitter_replies socc C.twitter_user C.twitter_pass
            ; start_twitter_nanny socc [(tweet_worker,1), (reply_worker,1)] C.twitter_user C.twitter_pass
            ; start_delicious socc C.delicious_user
--            ; start_identica socc C.identica_user
            ; start_google_reader socc
            ; return $ ChromeBackEnd f tc dc s ffd socc ht }