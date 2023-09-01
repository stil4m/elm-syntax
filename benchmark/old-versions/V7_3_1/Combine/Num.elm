module V7_3_1.Combine.Num exposing (int)

import Parser as Core
import V7_3_1.Combine as Combine exposing (Parser)


int : Parser s Int
int =
    Combine.fromCore Core.int
