module FilterConditionSpec exposing (suite)

import Data.FilterCondition as FilterCondition exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "FilterCondition"
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
                                    |> FilterCondition.parse
                                    |> FilterCondition.toString
                                    |> Expect.equal exp
                    )
            )
        ]
