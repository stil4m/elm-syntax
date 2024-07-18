module Elm.Processing exposing
    ( ProcessContext
    , init, addFile, addDependency, process
    )

{-| Processing raw files with the context of other files and dependencies.

Since v7.3.3, post-processing is unnecessary.
Use [`Elm.Parser.parseToFile`](Elm-Parser#parseToFile) instead.


## Types

@docs ProcessContext


## Functions

@docs init, addFile, addDependency, process

-}

import Dict exposing (Dict)
import Elm.Dependency exposing (Dependency)
import Elm.Interface as Interface exposing (Interface)
import Elm.Internal.RawFile as InternalRawFile
import Elm.RawFile as RawFile
import Elm.Syntax.File exposing (File)
import Elm.Syntax.ModuleName exposing (ModuleName)


{-| Opaque type to hold context for the processing
-}
type ProcessContext
    = ProcessContext ModuleIndexInner


type alias ModuleIndexInner =
    Dict ModuleName Interface


{-| Initialise an empty context
-}
init : ProcessContext
init =
    ProcessContext Dict.empty


{-| Add a file to the context that may be a dependency for the file that will be processed.
-}
addFile : RawFile.RawFile -> ProcessContext -> ProcessContext
addFile file (ProcessContext context) =
    ProcessContext
        (Dict.insert
            (RawFile.moduleName file)
            (Interface.build file)
            context
        )


{-| Add a whole dependency with its modules to the context.
-}
addDependency : Dependency -> ProcessContext -> ProcessContext
addDependency dep (ProcessContext x) =
    ProcessContext (Dict.union dep.interfaces x)


{-| Process a rawfile with a context.
-}
process : ProcessContext -> RawFile.RawFile -> File
process _ (InternalRawFile.Raw file) =
    file
