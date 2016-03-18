{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import            Codec.Compression.Zlib
import            Control.Monad
import            Data.IORef
import            Data.Char
import qualified  Data.Function as F
import            Data.List
import qualified  Data.Map as M
import            Data.Maybe
import            Data.Foldable
import            Data.Traversable
import qualified  Data.ByteString.Lazy.Char8 as LBS

import qualified  Graphics.UI.Threepenny       as UI
import            Graphics.UI.Threepenny.Core
import qualified  Graphics.UI.Threepenny.SVG   as SVG

import            System.Directory
import            System.Environment
import            System.FilePath
import            System.Posix.Files

import Debug.Trace

type SHA = String
type ObjectType = String

charCode :: Char -> Int
charCode = fromIntegral . ord

data GitObject = GitObject {
    goSHA :: SHA
  , goType :: ObjectType
  , goSize :: Int
  , goBody :: String
  } deriving (Show)

data GitRef = GitRef {
    grName :: String
  , grSHA :: SHA
  } deriving (Show)

data GitCommit = GitCommit {
    gcSHA :: SHA
  , gcTree :: SHA
  , gcParents :: [SHA]
  , gcAuthor :: String
  , gcCommitter :: String
  , gcComment :: String
  } deriving (Show)

gcSummary :: GitCommit -> String
gcSummary (GitCommit {..}) =
  unlines [ gcSHA
          , gcAuthor
          , ""
          , init gcComment
          ]

data CommitGraph = CommitGraph {
    -- the insert operation ensures that the
    -- strands here are *disjoint* from one-another
    --
    stStrands :: [Strand GitCommit]
  } deriving (Show)

graphFromList :: [GitCommit] -> CommitGraph
graphFromList = foldr insertCommit (CommitGraph [])

insertCommit :: GitCommit -> CommitGraph -> CommitGraph
insertCommit commit graph =
  graph { stStrands = spliceStrands (strand commit) (stStrands graph) }

spliceStrands :: Strand GitCommit -> [Strand GitCommit] -> [Strand GitCommit]
spliceStrands strand [] = [strand]
spliceStrands strand (st:rest) =
  case trySplice strand st of
  Nothing -> st : spliceStrands strand rest
  Just spliced -> spliceStrands spliced rest

trySplice :: Strand GitCommit -> Strand GitCommit -> Maybe (Strand GitCommit)
trySplice s1 s2
  | stLast s1 `isFirstParentOf` stHead s2 = Just (s1 `stJoin` s2)
  | stLast s2 `isFirstParentOf` stHead s1 = Just (s2 `stJoin` s1)
  | otherwise = Nothing

isFirstParentOf :: GitCommit -> GitCommit -> Bool
isFirstParentOf parent child =
  case gcParents child of
  [] -> False
  (pSha:_) -> pSha == gcSHA parent

data Strand a = Strand {
    stHead :: a
  , stFront :: [a]
  , stBack :: [a]
  } deriving (Show)

instance Foldable Strand where
  foldMap f = foldMap f . stList

stLast :: Strand a -> a
stLast (Strand _ _ (l:_)) = l
stLast (Strand h _ []) = h

strand :: a -> Strand a
strand a = Strand a [] []

prepend :: a -> Strand a -> Strand a
prepend a st = st { stHead = a
                  , stFront = stHead st : stFront st
                  }

append :: a -> Strand a -> Strand a
append a st = st { stBack = a : stBack st }

stList :: Strand a -> [a]
stList (Strand h f b) = h : (f ++ reverse b)

stListRev :: Strand a -> [a]
stListRev (Strand h f b) = b ++ reverse (h:f)

stJoin :: Strand a -> Strand a -> Strand a
stJoin s1 s2 = Strand {
      stHead = h
    , stFront = front
    , stBack = stListRev s2
    }
  where
    (h:front) = stList s1


main :: IO ()
main = do
  (root:_) <- getArgs
  startGUI (defaultConfig { jsAddr = Just "0.0.0.0"
                          , jsCustomHTML = Just "index.html"
                          , jsStatic = Just "static"
                          })
           (setup root)

setup :: FilePath -> Window -> UI ()
setup root window = do
  pure window # set UI.title "Git Vis!"

  currentSection <- liftIO $ newIORef loadWelcome
  sectionContainer <- UI.div

  let reload = do section <- liftIO $ readIORef currentSection
                  void $ pure sectionContainer # replaceChildren [section]

      changeSection section = do liftIO $ writeIORef currentSection section
                                 dismissPopover
                                 reload

  getBody window
    #+ [ navBar root changeSection
       , element sectionContainer
       ]
    # evt UI.keyup (bodyKeyHandler reload)

  reload

navBar :: FilePath -> (UI UI.Element -> UI ()) -> UI UI.Element
navBar root handler = do
  UI.div #. "navbar"
         #+ [
            UI.span #. "navitem"
                    # set UI.text "GV"
                    # evt UI.click (\_ -> handler loadWelcome)

         ,  UI.span #. "navitem"
                    # set UI.text "Commits"
                    # evt UI.click (\_ -> handler $ loadCommitGraph root)

         ,  UI.span #. "navitem"
                    # set UI.text "Refs"
                    # evt UI.click (\_ -> handler $ loadRefList root)

         ,  UI.span #. "navitem"
                    # set UI.text "Objects"
                    # evt UI.click (\_ -> handler $ loadObjectList root)
         ]

replaceChildren :: [UI UI.Element] -> UI UI.Element -> UI UI.Element
replaceChildren newKids uiEl = do
  kids <- sequence newKids
  uiEl # set children kids

bodyKeyHandler :: UI () -> UI.KeyCode -> UI ()
bodyKeyHandler reload c
  | c == charCode 'R' = void $ reload
  | c == escape = dismissPopover
  | otherwise = pure ()

escape :: Int
escape = 27

evt :: (UI.Element -> UI.Event a) -> (a -> UI ()) -> UI UI.Element -> UI UI.Element
evt event handler uiEl = evt' event (const handler) uiEl

evt' :: (UI.Element -> UI.Event a) -> (UI.Element -> a -> UI ()) -> UI UI.Element -> UI UI.Element
evt' event handler uiEl = do
  el <- uiEl
  on event el (handler el)
  pure el

loadObjectList :: FilePath -> UI UI.Element
loadObjectList root = do
  objects <- liftIO $ loadAllObjects root
  UI.div #. "list"
         #+ [objectListHeaders]
         #+ (objectListView <$> sortBy (compare `F.on` goSHA) objects)

objectListHeaders :: UI UI.Element
objectListHeaders =
  cellRow [ cell "sha"  # set UI.text "SHA"
          , cell "type" # set UI.text "Type"
          , cell "size" # set UI.text "Size"
          , cell "body" # set UI.text "Body"
          ]
    #. "headers"

objectListView :: GitObject -> UI UI.Element
objectListView (GitObject {..}) =
  cellRow [ cell "sha"  # set UI.text goSHA
          , cell "type" # set UI.text goType
          , cell "size" # set UI.text (show goSize)
          , cell "body" # set UI.text (trunc 16 goBody)
                        # evt UI.hover (popover "code" goBody)
          ]

loadRefList :: FilePath -> UI UI.Element
loadRefList root = do
  refs <- liftIO $ loadGitRefs root
  UI.div #. "list"
         #+ [refListHeaders]
         #+ (refListView <$> refs)

refListHeaders :: UI UI.Element
refListHeaders =
  cellRow [ cell "name" # set UI.text "Name"
          , cell "sha"  # set UI.text "SHA"
          ]
    #. "headers"

refListView :: GitRef -> UI UI.Element
refListView (GitRef {..}) =
  cellRow [ cell "name" # set UI.text grName
          , cell "sha"  # set UI.text grSHA
          ]

groupRefs :: [GitRef] -> M.Map SHA [String]
groupRefs refs =
    M.fromListWith (++) (mkEntry <$> refs)
  where
    mkEntry (GitRef {..}) = (grSHA, [grName])

loadWelcome :: UI UI.Element
loadWelcome = do
  UI.div #. "welcome"
         #+ [
      UI.h1 # set UI.text "Understanding Git"
    , UI.blockquote # set UI.text quote
    , UI.p # set UI.text attribution
    ]

quote :: String
quote =
  "In the moment when I truly understand my enemy, understand him well enough \
  \to defeat him, then in that very moment I also love him. I think it’s \
  \impossible to really understand somebody, what they want, what they believe, \
  \and not love them the way they love themselves. And then, in that very moment \
  \when I love them.... I destroy them."

attribution :: String
attribution =  "- Orson Scott Card, Ender's Game"

loadCommitGraph :: FilePath -> UI UI.Element
loadCommitGraph root = do
  graph <- liftIO $ graphFromList <$> loadCommits root
  refs <- liftIO $ groupRefs <$> loadGitRefs root

  let spacing = 150
      paddingX = 75
      paddingY = 100
      coords (c,r) = (paddingX + spacing * c, paddingY + spacing * r)
      commits = layoutCommits graph
      xys = map (coords . fst) $ M.elems commits
      width = foldl' max 0 (map fst xys) + paddingX + 100 -- a little extra padding for ref names
      height = foldl' max 0 (map snd xys) + paddingY

  context <- SVG.svg #. "commit-graph"
                     # set SVG.width (show width)
                     # set SVG.height (show height)

  for_ commits $ \(cr, commit) -> do
    for_ (zip [1..] $ gcParents commit) $ \(pNum, pSha) -> do
      case M.lookup pSha commits of
        Nothing -> pure ()
        Just (parentCR, _) -> do
          let (x, y) = coords cr
              (px, py) = coords parentCR
              offset = case pNum of
                       2 -> 15
                       3 -> -15
                       _ -> 0

              stroke = case pNum of
                       1 -> "rgba(255,0,0,0.9)"
                       2 -> "rgba(0,255,0,0.9)"
                       3 -> "rgba(0,0,255,0.9)"
                       _ -> "black"


          void $ pure context #+ [
              SVG.line
                # set SVG.x1 (show $ x + offset)
                # set SVG.y1 (show y)
                # set SVG.x2 (show $ px + offset)
                # set SVG.y2 (show py)
                # set SVG.stroke stroke
                # set SVG.stroke_width "5"
            ]


  for_ commits $ \(cr, commit) -> do
    let (x, y) = coords cr

    pure context #+ [
        SVG.circle
          # set SVG.r "50"
          # set SVG.cx (show x)
          # set SVG.cy (show y)
          # set SVG.fill "rgba(100,100,100,0.9)"
          # set SVG.stroke_width "8"
          # evt' UI.hover (\e _ -> do popover "code right" (gcSummary commit) ()
                                      void $ pure e # set SVG.stroke "#9af87d")
          # evt' UI.leave (\e _ -> do void $ pure e # set SVG.stroke "none")
      , SVG.text
          # set SVG.x (show $ x - 25)
          # set SVG.y (show $ y + 6)
          # set SVG.fill "white"
          # set SVG.stroke "white"
          # set UI.text (take 5 $ gcSHA commit)
      ]

  for_ (M.toList refs) $ \(sha, refs) -> do
    case M.lookup sha commits of
      Nothing -> pure ()
      Just (cr, _) -> do
        let (x,y) = coords cr

        void $ pure context #+ [
            SVG.circle
              # set SVG.r "50"
              # set SVG.cx (show x)
              # set SVG.cy (show y)
              # set SVG.stroke "black"
              # set SVG.stroke_width "4"
              # set SVG.fill "none"
          ]

        for_ (zip [0..] refs) $ \(n,ref) -> do
          void $ pure context #+ [
               SVG.text
                 # set SVG.x (show $ x - 65)
                 # set SVG.y (show $ y - 60 - (25 * n))
                 # set SVG.fill "black"
                 # set SVG.stroke "black"
                 # set UI.text ref
            ]

  pure context

type CommitLayout = M.Map SHA ((Int,Int), GitCommit)

layoutCommits :: CommitGraph -> CommitLayout
layoutCommits graph = do
    layoutInColumns M.empty (layoutInRows 0 $ map stList $ stStrands graph)
  where
    layoutInRows :: Int -> [[GitCommit]] -> [(Int, GitCommit)]
    layoutInRows _ [] = []
    layoutInRows y ([]:rows) = layoutInRows (y + 1) rows
    layoutInRows y ((commit:commits):rows) =
      (y, commit) : layoutInRows y (commits:rows)

    layoutInColumns :: CommitLayout -> [(Int, GitCommit)] -> CommitLayout
    layoutInColumns m [] = m
    layoutInColumns m ((y,commit):rest) =
      let parents = flip M.lookup m <$> gcParents commit
          getX = fst . fst
          maxX = foldl' max (-1) $ map (maybe (-1) getX) parents
          anyMissing = any isNothing parents
          m' = M.insert (gcSHA commit) ((maxX+1,y), commit) m

      in if not anyMissing
         then layoutInColumns m' rest
         else case rest of
              [] -> error "Commit layout failed! Couldn't find parent for strand"
              (next:rest') -> layoutInColumns m (rest ++ [(y,commit)])

cellRow :: [UI UI.Element] -> UI UI.Element
cellRow uiKids = do
  kids <- sequence uiKids
  UI.div
    #. "row"
    # set children kids

cell :: String -> UI UI.Element
cell field =
  UI.div #. ("cell " ++ field)

dismissPopover :: UI ()
dismissPopover = do
  window <- askWindow
  popovers <- UI.getElementsByClassName window "popover"
  traverse_ UI.delete popovers

popover :: String -> String -> a -> UI ()
popover clazz contents _ = do
  dismissPopover
  window <- askWindow
  void $ getBody window #+ [
      UI.div #. ("popover " ++ clazz)
             # set UI.text contents
    ]


trunc :: Int -> String -> String
trunc n s
  | length s <= n = s
  | otherwise = take n s ++ ".."

loadAllObjects :: FilePath -> IO [GitObject]
loadAllObjects root =
  traverse (readObject root) =<< loadObjectNames root

loadObjectNames :: FilePath -> IO [SHA]
loadObjectNames root = do
  let objectsDir = root </> "objects"
      otherDirs = ["info", "pack"]

  objectSubdirs <- filter (not . (`elem` otherDirs))
                       <$> listDirectory objectsDir

  fmap concat $ for objectSubdirs $ \objDir -> do
    files <- listDirectory (objectsDir </> objDir)
    pure $ map (objDir ++) files


listDirectory :: FilePath -> IO [FilePath]
listDirectory =
      fmap (filter (not . special))
    . getDirectoryContents
  where
    special s = s `elem` [".",".."]

readObject :: FilePath -> SHA -> IO GitObject
readObject root fullName = do
  let (section,rest) = splitAt 2 fullName
      path = root </> "objects" </> section </> rest

  bytes <- decompress <$> LBS.readFile path

  let (oType, typeRest) = LBS.break (== ' ') bytes
      (size, sizeRest) = LBS.break (== '\0') (LBS.drop 1 typeRest)
      body = LBS.drop 1 sizeRest

  pure $ GitObject {
      goSHA = fullName
    , goType = LBS.unpack oType
    , goSize = read $ LBS.unpack size
    , goBody = LBS.unpack body
    }


loadGitRefs :: FilePath -> IO [GitRef]
loadGitRefs root = do
    loadRefsDir "refs"

  where
    loadRefsDir dirName = do
      let refsDir = root </> dirName

      refDirs <- listDirectory refsDir

      fmap concat $ for refDirs $ \entry -> do
        status <- getFileStatus (refsDir </> entry)

        if isDirectory status
          then loadRefsDir (dirName </> entry)
          else pure <$> readRefFile root (dirName </> entry)

readRefFile :: FilePath -> FilePath -> IO GitRef
readRefFile root path = do
  content <- readFile (root </> path)
  pure $ GitRef {
      grName = drop 5 path -- dref "refs/"
    , grSHA = init content
    }

loadCommits :: FilePath -> IO [GitCommit]
loadCommits root = mapMaybe loadCommitObject <$> loadAllObjects root

loadCommitObject :: GitObject -> Maybe GitCommit
loadCommitObject (GitObject {..}) = do
  case goType of
    "commit" -> Just $ parseCommit goSHA goBody
    _ -> Nothing


parseCommit :: SHA -> String -> GitCommit
parseCommit sha body =
    go blankCommit (lines body)
  where
    blankCommit = GitCommit {
        gcSHA = sha
      , gcTree = ""
      , gcParents = []
      , gcAuthor = ""
      , gcCommitter = ""
      , gcComment = ""
      }

    go commit ("":rest) = commit { gcComment = unlines rest }
    go commit (('t':'r':'e':'e':' ':tree):rest) =
      go (commit { gcTree = tree }) rest

    go commit (('p':'a':'r':'e':'n':'t':' ':parent):rest) =
      go (commit { gcParents = gcParents commit ++ [parent] }) rest

    go commit (('a':'u':'t':'h':'o':'r':' ':author):rest) =
      go (commit { gcAuthor = author}) rest

    go commit (('c':'o':'m':'m':'i':'t':'t':'e':'r':' ':committer):rest) =
      go (commit { gcCommitter = committer}) rest

    go commit (line:_) = error $ "Invalid commit line: " ++ show line


