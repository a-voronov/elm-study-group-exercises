module Main exposing
    ( butLast1
    , butLast2
    , clap1
    , clap2
    , compress1
    , compress2
    , dropEvery
    , elementAt
    , flip
    , increment
    , isPalindrome
    , join
    , last1
    , last2
    , length1
    , length2
    , lfold
    , reverse
    , rfold
    , words
    )

{- * Homework Week 1 * -} {- Just for fun -}


lfold : (a -> b -> b) -> b -> List a -> b
lfold func acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            lfold func (func x acc) xs


rfold : (a -> b -> b) -> b -> List a -> b
rfold func acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            func x (rfold func acc xs)


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x


increment : Int -> Int
increment x =
    x + 1



{- Find last element in a list -}


last1 : List a -> Maybe a
last1 =
    List.reverse >> List.head



-- [1, 2, 3] => [3, 2, 1] => Just 3


last2 : List a -> Maybe a
last2 =
    List.foldl (Just >> always) Nothing



-- [1, 2, 3] => Just 1 => Just 2 => Just 3
{- Note: Just >> always

   Just   : a -> Maybe a
   always : a -> b -> a
   (>>)   : (a -> b) -> (b -> c) -> a -> c

   (a ->       b) -> (b       ->             c ) -> a ->             c
   (a -> Maybe a) -> (Maybe a -> (b -> Maybe a)) -> a -> (b -> Maybe a)

-}
{- Find the last but one element of a list -}


butLast1 : List a -> Maybe a
butLast1 =
    List.reverse >> List.tail >> Maybe.andThen List.head



-- [1, 2, 3] => [3, 2, 1] => [2, 1] => Just 2


butLast2 : List a -> Maybe a
butLast2 list =
    case list of
        [] ->
            Nothing

        x :: [ _ ] ->
            Just x

        x :: xs ->
            butLast2 xs



-- [1, 2, 3] => [2, 3] => Just 2
{- Find the K'th element of a list -}


elementAt : Int -> List a -> Maybe a
elementAt index list =
    let
        iterator pos items =
            case items of
                [] ->
                    Nothing

                x :: xs ->
                    if pos == index then
                        Just x

                    else
                        iterator (pos + 1) xs
    in
    iterator 1 list



-- 2 [1, 2, 3] => Just 2
{- Find the number of elements of a list -}


length1 : List a -> Int
length1 =
    List.foldl (\_ acc -> acc + 1) 0


length2 : List a -> Int
length2 =
    List.foldl (increment |> always) 0



{- Note: increment |> always

   increment : number -> number
   always    : a -> b -> a
   (|>)      : a -> (a -> b) -> b

   a                -> (a                ->                      b ) ->                     b
   number -> number -> (number -> number -> (b -> number -> number)) -> b -> number -> number

-}
{- Reverse a list -}


reverse : List a -> List a
reverse =
    List.foldl (::) []



{- Find out whether a list is a palindrome -}


isPalindrome : List a -> Bool
isPalindrome list =
    let
        reversed =
            List.reverse list

        iterator lhs rhs =
            case ( lhs, rhs ) of
                ( [], [] ) ->
                    True

                ( x :: xs, y :: ys ) ->
                    if x == y then
                        iterator xs ys

                    else
                        False

                -- this will never happen,
                -- but we can't prove to Elm's compiler
                -- that a reversed list is of the same length as the original one
                _ ->
                    False
    in
    iterator list reversed



{- Eliminate consecutive duplicates of string elements -}


compress1 : String -> String
compress1 string =
    let
        unique char str =
            let
                cs =
                    String.fromChar char
            in
            if String.endsWith cs str then
                str

            else
                str ++ cs
    in
    String.foldl unique "" string


compress2 : String -> String
compress2 string =
    let
        unique char str =
            if String.startsWith (String.fromChar char) str then
                str

            else
                String.cons char str
    in
    String.foldr unique "" string



{- Drop every N'th element from a string -}


dropEvery : Int -> String -> String
dropEvery number string =
    let
        iterator char ( pos, str ) =
            ( pos + 1
            , if remainderBy number pos == 0 then
                str

              else
                str ++ String.fromChar char
            )
    in
    String.foldl iterator ( 1, "" ) string |> Tuple.second



{- (optional) Insert the 👏 emoji between words -}


clap1 : String -> String
clap1 =
    String.words >> String.join " 👏 "


clap2 : String -> String
clap2 =
    words >> join " 👏 "


words : String -> List String
words string =
    let
        separators =
            [ ' ', '\t', '\n' ]

        iterator : Maybe ( Char, String ) -> String -> List String -> List String
        iterator headTail buf list =
            case headTail of
                Nothing ->
                    -- don't forget to include last buf if not empty
                    if String.isEmpty buf then
                        list

                    else
                        list ++ [ buf ]

                Just ( char, str ) ->
                    if List.member char separators then
                        -- add buf to list if not empty
                        iterator (String.uncons str)
                            ""
                            (if String.isEmpty buf then
                                list

                             else
                                list ++ [ buf ]
                            )

                    else
                        -- add char to buf
                        iterator (String.uncons str) (buf ++ String.fromChar char) list
    in
    iterator (String.uncons string) "" []


join : String -> List String -> String
join separator list =
    let
        iterator : List String -> String -> String
        iterator strings acc =
            case strings of
                [] ->
                    acc

                x :: y :: xs ->
                    iterator (y :: xs) (acc ++ x ++ separator)

                x :: xs ->
                    iterator xs (acc ++ x)
    in
    iterator list ""
