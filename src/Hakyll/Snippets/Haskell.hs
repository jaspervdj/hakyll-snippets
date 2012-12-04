--------------------------------------------------------------------------------
module Hakyll.Snippets.Haskell
    ( snippetsFromFile
    , snippetsFromString
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Error             (throwError)
import           Data.List                       (isPrefixOf)
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     (mconcat, mempty)
import           Hakyll.Snippets


--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
snippetsFromFile :: FilePath -> IO Snippets
snippetsFromFile filePath = do
    contents <- readFile filePath
    case snippetsFromString contents of
        Left err -> error err
        Right x  -> return x


--------------------------------------------------------------------------------
snippetsFromString :: String -> Either String Snippets
snippetsFromString contents = case H.parseFileContents contents of
    H.ParseOk module' -> snippetsFromModule (lines contents) module'
    err               -> throwError $ show err


--------------------------------------------------------------------------------
snippetsFromModule :: [String] -> H.Module H.SrcSpanInfo
                   -> Either String Snippets
snippetsFromModule lines' (H.Module _ mhead _ _ decls) =
    case mhead of
        Nothing -> throwError "No module name provided"
        Just (H.ModuleHead _ (H.ModuleName _ name) _ _) ->
            return $ mconcat $ map (snippetsFromDecl name lines') decls
snippetsFromModule _                        _          = throwError $
    "Only normal modules supported"


--------------------------------------------------------------------------------
snippetsFromDecl :: String -> [String] -> H.Decl H.SrcSpanInfo -> Snippets
snippetsFromDecl mname ls (H.FunBind loc (H.Match _ name _ _ _ : _)) =
    singleSnippet (mname ++ "." ++ fromName name) (selectLines loc ls)

snippetsFromDecl mname ls (H.TypeSig loc names _) = mconcat
    [ singleSnippet (mname ++ "." ++ fromName name) (selectLines loc ls)
    | name <- names
    ]

snippetsFromDecl mname ls (H.DataDecl loc _ _ dhead _ _) = fromMaybe mempty $ do
    name <- case dhead of
        H.DHead _ name _     -> return $ fromName name
        H.DHInfix _ _ name _ -> return $ fromName name
        _                    -> Nothing

    return $ singleSnippet (mname ++ "." ++ name) (selectLines loc ls)

snippetsFromDecl _ _ _ = mempty


--------------------------------------------------------------------------------
fromName :: H.Name l -> String
fromName (H.Ident _ n)  = n
fromName (H.Symbol _ n) = n


--------------------------------------------------------------------------------
selectLines :: H.SrcSpanInfo -> [String] -> [String]
selectLines (H.SrcSpanInfo ss _) ls =
    let start                            = H.srcSpanStartLine ss - 1
        len
            | H.srcSpanEndColumn ss <= 0 = H.srcSpanEndLine ss - start - 1
            | otherwise                  = H.srcSpanEndLine ss - start
    in trim notEmpty $ filter notComment $ take len $ drop start ls

  where
    notEmpty   = not . null
    notComment = not . ("--" `isPrefixOf`)


--------------------------------------------------------------------------------
trim :: (a -> Bool) -> [a] -> [a]
trim p = reverse . dropWhile (not . p) . reverse . dropWhile (not . p)
