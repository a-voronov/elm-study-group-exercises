{- * Homework Week 3 * -}


module Homework exposing
    ( either
    , find
    , find2
    , find3
    , keepOks
    , keepOks2
    , mapOk
    , mapOk2
    , maybeToList
    , maybeToList2
    , parseDate
    , updateList
    , updateList2
    , updateListKv
    , updateListKv2
    )

import Date exposing (Date)



{- maybeToList -}


maybeToList : Maybe a -> List a
maybeToList x =
    case x of
        Nothing ->
            []

        Just y ->
            List.singleton y


maybeToList2 : Maybe a -> List a
maybeToList2 =
    Maybe.withDefault [] << Maybe.map List.singleton



{- updateList: Change or remove element if it matches the shouldChange test. -}


updateList : (a -> Bool) -> (a -> Maybe a) -> List a -> List a
updateList shouldChange f xs =
    List.filterMap
        (\x ->
            if shouldChange x then
                f x

            else
                Just x
        )
        xs


updateList2 : (a -> Bool) -> (a -> Maybe a) -> List a -> List a
updateList2 shouldChange f list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                y =
                    if shouldChange x then
                        maybeToList (f x)

                    else
                        List.singleton x
            in
            y ++ updateList2 shouldChange f xs



{- find -}


find : (a -> Bool) -> List a -> Maybe a
find f list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if f x then
                Just x

            else
                find f xs


find2 : (a -> Bool) -> List a -> Maybe a
find2 f list =
    List.filter f list |> List.head


find3 : (a -> Bool) -> List a -> Maybe a
find3 f =
    List.head << List.filter f



{- Implement updateListKv -}


updateListKv : List ( k, v ) -> k -> (v -> Maybe v) -> List ( k, v )
updateListKv list k f =
    List.filterMap
        (\( x, y ) ->
            if x == k then
                Maybe.map (Tuple.pair x) <| f y

            else
                Just <| Tuple.pair x y
        )
        list


updateListKv2 : List ( k, v ) -> k -> (v -> Maybe v) -> List ( k, v )
updateListKv2 list k f =
    updateList
        ((==) k << Tuple.first)
        (\( x, y ) -> Maybe.map (Tuple.pair x) <| f y)
        list



{- keepOks -}


keepOks : List (Result a b) -> List b
keepOks =
    List.filterMap Result.toMaybe


keepOks2 : List (Result a b) -> List b
keepOks2 list =
    case list of
        [] ->
            []

        (Ok v) :: xs ->
            v :: keepOks xs

        (Err e) :: xs ->
            keepOks xs



{- mapOk -}


mapOk : (b -> c) -> Result a b -> Result a c
mapOk =
    Result.map


mapOk2 : (b -> c) -> Result a b -> Result a c
mapOk2 f r =
    case r of
        Ok v ->
            Ok (f v)

        Err e ->
            Err e



{- either -}


either : (a -> c) -> (b -> c) -> Result a b -> c
either fa fb r =
    case r of
        Err a ->
            fa a

        Ok b ->
            fb b



{- Implement parseDate

   Do elm install justinmimbs/date to get the package. Use Date.fromIsoString.
-}


parseDate : Maybe String -> Maybe Date
parseDate =
    Maybe.andThen <| Result.toMaybe << Date.fromIsoString
