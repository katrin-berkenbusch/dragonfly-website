{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module WebSite.Collection (
    makeRules,
    getList,
    pageIndexCtx,
    getBubbles,
    CollectionConfig(..),
    Rules()
) where

import           Control.Monad     (liftM)
import           Data.List
import qualified Data.Map          as M
import           Data.Maybe        (fromMaybe, maybeToList)
import           Data.Monoid       ((<>))
import           System.FilePath

import           Text.Pandoc

import           Hakyll

import           WebSite.Compilers
import           WebSite.Context

data CollectionConfig = CollectionConfig
                      { baseName           :: String
                      , indexTemplate      :: FilePath
                      , indexPattern       :: Pattern
                      , collectionPattern  :: Pattern
                      , collectionTemplate :: Identifier
                      , pageTemplate       :: Identifier
                      }

makeRules :: CollectionConfig -> Rules()
makeRules cc = do

    match (indexPattern cc) $ do
        route $ constRoute (indexTemplate cc)
        compile $ do
            base <- baseContext (baseName cc)
            pages <- getList cc 1000
            bubbles <- getBubbles cc Nothing
            let  ctx = base <> pages <> bubbles
            scholmdCompiler
                >>= loadAndApplyTemplate (collectionTemplate cc) ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match (collectionPattern cc) $ version "full" $ do
        compile $ do
            scholmdCompiler
                >>= saveSnapshot "content"

    match (collectionPattern cc) $ do
        route $ setExtension "html"
        compile $ do
            ident <- getUnderlying
            base <- baseContext (baseName cc)
            imageMeta <- loadAll ("**/*.img.md")
            bannerMeta <- bannerImageMeta imageMeta
            pages <- getList cc 1000
            bubbles <- getBubbles cc (Just ident)
            pandoc <- readScholmd
            let ctx = base <> actualbodyField "actualbody" <> pages <> bubbles <> bannerMeta
            writeScholmd pandoc
                >>= loadAndApplyTemplate "templates/append-publications.html" ctx
                >>= loadAndApplyTemplate (pageTemplate cc) ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= imageCredits imageMeta
                >>= relativizeUrls

bannerImageMeta :: [Item String] -> Compiler (Context String)
bannerImageMeta imageMeta = do
  identifier <- getUnderlying
  metadata <- getMetadata identifier
  let bannerInfo = do
        banner <- M.lookup "banner-image" metadata
        getBannerMeta banner imageMeta
  case bannerInfo of
    Nothing -> return $ constField "noBannerImageMeta" "here"
    Just bi -> do
      bm <- getMetadata (itemIdentifier bi)
      let ctx = constField "bannerImageMeta" "here"
      return $ M.foldlWithKey foldBannerMeta ctx bm
    where
      getBannerMeta :: String -> [Item String] -> Maybe (Item String)
      getBannerMeta b =
        let banner = dropDrive $ dropExtension b
        in find (\y -> (dropDrive $ dropExtensions $ toFilePath $ itemIdentifier y) == banner)

      foldBannerMeta :: Context a -> String -> String -> Context a
      foldBannerMeta c k m = c <> constField ("banner-image-" ++ k) m

getList :: CollectionConfig -> Int ->  Compiler (Context String)
getList cc limit = do
    snaps <- loadAllSnapshots (collectionPattern cc .&&. hasVersion "full") "content"
    let sortorder i = liftM (fromMaybe "666") $ getMetadataField i "sortorder"
    snaps' <- sortItemsBy sortorder snaps
    let l = length snaps'
        all = cycle snaps'
        lu = [ (itemIdentifier this, (prev, next))
             | (prev, this, next) <- take l $ drop (l-1) $ zip3 all (drop 1 all) (drop 2 all) ]
    return $ listField (baseName cc) (pageIndexCtx lu)(return $ take limit snaps')

getBubbles :: CollectionConfig -> Maybe Identifier -> Compiler (Context String)
getBubbles cc mident = do
    snaps <- loadAllSnapshots (collectionPattern cc .&&. hasVersion "full") "content"
    let sortorder i = liftM (fromMaybe "666") $ getMetadataField i "sortorder"
    snaps' <- sortItemsBy sortorder snaps
    let l = length snaps'
        all = cycle snaps'
        lu = [ (itemIdentifier this, (prev, next))
             | (prev, this, next) <- take l $ drop (l-1) $ zip3 all (drop 1 all) (drop 2 all) ]
        snaps'' = maybe (take 7 snaps') id $ do
                    ident <- mident
                    let ident' = setVersion (Just "full") ident
                    idx <- findIndex (\i -> ident' == itemIdentifier i) snaps'
                    let (before, after) = splitAt (idx + l) (cycle snaps')
                    return $ reverse (take 3 (reverse before)) ++ take 4 after
        previous = listField "bubbles_prev" (pageIndexCtx lu)(return (take 3 snaps''))
        this     = listField "bubbles_this" (pageIndexCtx lu)(return (take 1 $ drop 3 snaps''))
        next     = listField "bubbles_next" (pageIndexCtx lu)(return (take 3 $ drop 4 snaps''))
    return  $ previous <> this <> next


type PreviousNextMap = [(Identifier, (Item String, Item String))]
pageIndexCtx :: PreviousNextMap -> Context String
pageIndexCtx lu  = listContextWith "tags" tagContext
                <> defaultContext
                <> teaserImage
                <> portholeImage
                <> teaserField "teaser" "content"
                <> pageUrlField "pageurl"
                <> dateField "published" "%B %d . %Y"
                <> previous lu
                <> next lu

previous :: PreviousNextMap -> Context String
previous lu =
    let lup item = return $ fmap fst $ maybeToList $ lookup (itemIdentifier item) lu
    in  listFieldWith "previous" (pageIndexCtx []) lup

next :: PreviousNextMap -> Context String
next lu =
    let lup item = return $ fmap snd $ maybeToList $ lookup (itemIdentifier item) lu
    in  listFieldWith "next" (pageIndexCtx []) lup

teaserImage :: Context String
teaserImage = field "teaserImage" getImagePath
  where
    getImagePath item = do
        let path = toFilePath (itemIdentifier item)
            base = dropExtension path
            ident = fromFilePath $ base </> "teaser.jpg"
        fmap (maybe "" toUrl) (getRoute ident)

portholeImage :: Context String
portholeImage = field "portholeImage" getImagePath
  where
    getImagePath item = do
        let path = toFilePath (itemIdentifier item)
            base = dropExtension path
            ident = fromFilePath $ base </> "porthole.png"
        fmap (maybe "" toUrl) (getRoute ident)

-- Sort items by a monadic ordering function
sortItemsBy :: (Ord b, Monad m) => (Identifier -> m b) -> [Item a] -> m [Item a]
sortItemsBy f = sortByM $ f . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = map fst . sortOn snd <$> mapM (\x -> (x,) <$> f x) xs
