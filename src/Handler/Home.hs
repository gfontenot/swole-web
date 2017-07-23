module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    (form, enctype) <- generateFormPost testForm
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "home/index")

postHomeR :: Handler Html
postHomeR = do
    ((res, _form), _enctype) <- runFormPost testForm
    case res of
      FormSuccess fields -> do
          setMessage $ toHtml $ unwords $ values fields
          redirect HomeR
      _ -> do
          setMessage "Unsuccessful"
          redirect HomeR

data TestFields = TestFields
    { values :: [Text]
    }

multiTextField ::  Field Handler [Text]
multiTextField = Field
    { fieldParse = \ts _fs -> multiTextFieldParse ts
    , fieldView = multiTextFieldView
    , fieldEnctype = UrlEncoded
    }

multiTextFieldParse :: [Text] -> Handler (Either (SomeMessage App) (Maybe [Text]))
multiTextFieldParse = return . Right . Just

multiTextFieldView :: FieldViewFunc Handler [Text]
multiTextFieldView theId name attrs evals isReq = do
    let vals = either (const []) id evals
    $(whamletFile "views/multi-text-field.hamlet")

testForm :: Form TestFields
testForm = renderDivs $
    TestFields
        <$> areq multiTextField "Value" Nothing
