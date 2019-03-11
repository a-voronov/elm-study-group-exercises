{- * Homework Week 2 * -}


module Homework exposing
    ( Address
    , ConvertUser
    , Profile
    , User
    , apply
    , apply2
    , bird
    , bird2
    , bird3
    , buildStatsUrl
    , catMaybes
    , convert
    , convert021
    , convert022
    , convert023
    , convert031
    , convert032
    , flip
    , map
    , mapMaybes
    , setPhone
    )

import Url.Builder exposing (..)



{- Map one structure to another -}


convert : List { name : String, email : String, phone_number : String } -> List { name : String, email : String }
convert =
    List.map (\record -> { name = record.name, email = record.email })



{- Filter elements with non-empty name and email -}
-- I


convert021 : List { name : Maybe String, email : Maybe String } -> List { name : String, email : String }
convert021 =
    List.filterMap
        (\record ->
            case ( record.name, record.email ) of
                ( Just name, Just email ) ->
                    Just { name = name, email = email }

                _ ->
                    Nothing
        )



-- II


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x


map : Maybe a -> (a -> b) -> Maybe b
map x f =
    Maybe.map f x


apply : Maybe a -> Maybe (a -> b) -> Maybe b
apply x func =
    case func of
        Just f ->
            map x f

        Nothing ->
            Nothing


type alias ConvertUser =
    { name : String, email : String }


convert022 : List { name : Maybe String, email : Maybe String } -> List ConvertUser
convert022 =
    List.filterMap
        (\record ->
            ConvertUser
                |> map record.name
                |> apply record.email
        )



-- <^> map   : Maybe a ->       (a -> b) -> Maybe b
-- <*> apply : Maybe a -> Maybe (a -> b) -> Maybe b
--
-- curry(User.init)
--     <^> json <| "name"
--     <*> json <| "email"
--     <*> json <| "age"
{-

   map << .name
    : { c | name : Maybe a } -> (a -> b) -> Maybe b

   apply << .email
    : { c | email : Maybe a } -> Maybe (a -> b) -> Maybe b

   flip (map << .name)
    : (a -> b) -> { c | name : Maybe a } -> Maybe b

   flip (apply << .email)
    : Maybe (a -> b) -> { c | email : Maybe a } -> Maybe b

   flip (map << .name) << flip (apply << .email)
    : Maybe (a -> b)
        -> { d | name : Maybe { c | email : Maybe a } }
        -> Maybe (Maybe b)

-}
-- III


apply2 : Maybe a -> Maybe (a -> b) -> Maybe b
apply2 =
    Maybe.map2 (|>)


convert023 : List { name : Maybe String, email : Maybe String } -> List ConvertUser
convert023 =
    List.filterMap
        (\record ->
            Just ConvertUser
                |> apply2 record.name
                |> apply2 record.email
        )



{- Fill in missing emails with <unspecified>, while removing elements with no name -}
-- I


convert031 : List { name : Maybe String, email : Maybe String } -> List { name : String, email : String }
convert031 =
    List.filterMap
        (\record ->
            case ( record.name, record.email ) of
                ( Just name, email ) ->
                    Just { name = name, email = Maybe.withDefault "<unspecified>" email }

                _ ->
                    Nothing
        )



-- II


convert032 : List { name : Maybe String, email : Maybe String } -> List ConvertUser
convert032 =
    List.filterMap
        (\record ->
            ConvertUser
                |> map record.name
                |> apply (Just (Maybe.withDefault "<unspecified>" record.email))
        )



{- Rewrite bird using <|, then using |> instead of parens (where applicable) -}


bird : Int
bird =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))


bird2 : Int
bird2 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum <| List.filter notThree <| List.map incr [ 1, 2, 3 ]


bird3 : Int
bird3 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.map incr [ 1, 2, 3 ] |> List.filter notThree |> List.sum



{- Implement setPhone -}


type alias User =
    { profile : Profile }


type alias Profile =
    { address : Address }


type alias Address =
    { phone : String }


setPhone : String -> User -> User
setPhone phone user =
    { user | profile = { address = { phone = phone } } }



{- mapMaybes -}


mapMaybes : (a -> Maybe b) -> List a -> List b
mapMaybes f =
    List.filterMap f


mapMaybes2 : (a -> Maybe b) -> List a -> List b
mapMaybes2 f list =
    case list of
        [] ->
            []

        x :: xs ->
            case f x of
                Nothing ->
                    mapMaybes2 f xs

                Just y ->
                    y :: mapMaybes2 f xs



{- catMaybes -}


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.filterMap identity


catMaybes2 : List (Maybe a) -> List a
catMaybes2 list =
    case list of
        [] ->
            []

        x :: xs ->
            []


catMaybes3 : List (Maybe a) -> List a
catMaybes3 =
    mapMaybes2 identity



{- Use package elm/url and its Url.Builder.absolute to build URL from parameters -}


buildStatsUrl : Int -> { startDate : Maybe String, numElems : Maybe Int } -> String
buildStatsUrl itemId ps =
    "https://myapi.com"
        ++ Url.Builder.absolute [ "api", "item", String.fromInt itemId, "stats.json" ]
            (catMaybes
                [ Maybe.map (string "start_date") ps.startDate
                , Maybe.map (int "num_elems") ps.numElems
                ]
            )
