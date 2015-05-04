--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Char   (toLower)
import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc
import Text.Pandoc.Options


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- improve this to actually call pandoc to generate
    -- the slideshow from the markdown file
    match "getting-started/*" $ do
        route idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "pages/*" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Blog" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "home.md" $ compile pandocCompiler

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            homeContent <- loadBody "home.md"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    field "home" (\_ -> return homeContent)  `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss myFeedConfiguration feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "haskell-servant blog"
    , feedDescription = "Posts from the haskell-servant blog"
    , feedAuthorName  = "servant developers"
    , feedAuthorEmail = ""
    , feedRoot        = "http://haskell-servant.github.io"
    }

--------------------------------------------------------------------------------
myPandocCompiler' :: Maybe String -> Compiler (Item String)
myPandocCompiler' withToc = 
    pandocCompilerWith defaultHakyllReaderOptions $
        case withToc of
            Just x | map toLower x `elem` ["true", "yes"] -> writerWithToc
                   | otherwise                            -> writerOpts
            Nothing                                       -> writerOpts

    where writerOpts = defaultHakyllWriterOptions 
                           { writerReferenceLinks = True
                           , writerSectionDivs = True 
                           , writerHtml5 = True
                           , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
                           , writerColumns = 100 
                           }
          writerWithToc = 
            writerOpts { writerTableOfContents = True 
                       , writerTemplate = "$if(toc)$<div id=\"toc\"><h3>Table of contents</h3>$toc$</div>$endif$\n$body$" 
                       , writerStandalone = True 
                       }

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = do
    ident <- getUnderlying
    myPandocCompiler' =<< getMetadataField ident "toc"
