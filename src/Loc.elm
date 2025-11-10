module Loc exposing (Loc(..), Pos, combine, unknown)


type Loc
    = Loc
        { name : String
        , start : Pos
        , end : Pos
        }


type alias Pos =
    { row : Int
    , col : Int
    }


unknown : Loc
unknown =
    { name = "unknown"
    , start = { row = 0, col = 0 }
    , end = { row = 0, col = 0 }
    }
        |> Loc


combine : Loc -> Loc -> Loc
combine (Loc a) (Loc b) =
    let
        before p1 p2 =
            (p1.row < p2.row)
                || (p1.row == p2.row && p1.col < p2.col)

        start =
            if before a.start b.start then
                a.start

            else
                b.start

        end =
            if before a.end b.end then
                b.end

            else
                a.end
    in
    Loc
        { name = a.name
        , start = start
        , end = end
        }
