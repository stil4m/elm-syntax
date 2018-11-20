module Combine.Num exposing (int)

import Combine exposing (Parser)
import Parser as Core


int : Parser s Int
int =
    Combine.fromCore Core.int
