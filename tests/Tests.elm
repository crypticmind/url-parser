module Tests exposing (..)

import UrlParser exposing (..)
import Navigation exposing (Location)
import Test exposing (..)
import Expect


-- TESTS


all : Test
all =
    describe "UrlParser"
        [ describe "Basic Parsing" testParsing
        ]


testParsing : List Test
testParsing =
    [ parserTest "Home" "" <| Ok HomeRoute
    , parserTest "About" "about" <| Ok AboutRoute
    , parserTest "Token" "token/abc" <| Ok (TokenRoute "abc")
    , parserTest "Token (missing ID)" "token/" <| Err "missing token segment"
    , parserTest "Users" "users" <| Ok (UsersRoutes UsersRoute)
    , parserTest "User" "users/2" <| Ok (UsersRoutes (UserRoute 2))
    , parserTest "Edit" "users/2/edit" <| Ok (UsersRoutes (UserEditRoute 2))
    ]


parserTest : String -> String -> Result a MainRoute -> Test
parserTest name path expectedRouteResult =
    case expectedRouteResult of
        Ok expectedRoute ->
            describe name
                [ test (name ++ " in path") <|
                    \() ->
                        Expect.equal
                            (Just expectedRoute)
                            (parsePath routeParser { newLocation | pathname = "/" ++ path })
                , test (name ++ " in hash") <|
                    \() ->
                        Expect.equal
                            (Just expectedRoute)
                            (parseHash routeParser { newLocation | hash = "#/" ++ path })
                , test (name ++ " in hash without leading slash") <|
                    \() ->
                        Expect.equal
                            (Just expectedRoute)
                            (parseHash routeParser { newLocation | hash = "#" ++ path })
                ]

        Err _ ->
            describe name
                [ test (name ++ " in path") <|
                    \() ->
                        Expect.equal
                            Nothing
                            (parsePath routeParser { newLocation | pathname = "/" ++ path })
                , test (name ++ " in hash") <|
                    \() ->
                        Expect.equal
                            Nothing
                            (parseHash routeParser { newLocation | hash = "#/" ++ path })
                , test (name ++ "in hash without leading slash") <|
                    \() ->
                        Expect.equal
                            Nothing
                            (parseHash routeParser { newLocation | hash = "#" ++ path })
                ]



-- ROUTES


type alias UserId =
    Int


type UserRoute
    = UsersRoute
    | UserRoute UserId
    | UserEditRoute UserId


type MainRoute
    = HomeRoute
    | AboutRoute
    | TokenRoute String
    | UsersRoutes UserRoute
    | NotFoundRoute



-- PARSERS


routeParser : Parser (MainRoute -> c) c
routeParser =
    oneOf mainMatchers


usersMatchers : List (Parser (UserRoute -> c) c)
usersMatchers =
    [ map UserEditRoute (int </> s "edit")
    , map UserRoute (int)
    , map UsersRoute top
    ]


mainMatchers : List (Parser (MainRoute -> c) c)
mainMatchers =
    [ map HomeRoute top
    , map AboutRoute (s "about")
    , map TokenRoute (s "token" </> string)
    , map UsersRoutes (s "users" </> (oneOf usersMatchers))
    ]



-- DUMMY LOCATION


newLocation : Location
newLocation =
    { hash = ""
    , host = "example.com"
    , hostname = "example.com"
    , href = ""
    , origin = ""
    , password = ""
    , pathname = ""
    , port_ = ""
    , protocol = "http"
    , search = ""
    , username = ""
    }
