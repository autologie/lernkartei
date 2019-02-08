module FilterSpec exposing (suite)

import Data.Filter as Filter exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Filter"
        [ describe "parse and stringify valid expressions"
            ([ "t:-3d+2d"
             , "^auf"
             , "chen$"
             , "tag"
             , ":s"
             , "e:Essen"
             , "t:-3d+2d ^auf chen$ tag :s e:Essen"
             , "a b c"
             ]
                |> List.map
                    (\exp ->
                        test ("parse and stringify '" ++ exp ++ "'") <|
                            \_ ->
                                exp
                                    |> Filter.parse
                                    |> Filter.toString
                                    |> Expect.equal exp
                    )
            )
        ]
