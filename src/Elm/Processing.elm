module Elm.Processing exposing
    ( ProcessContext
    , init, addFile, addDependency, process
    )

{-| Processing raw files with the context of other files and dependencies.


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
import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)


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
Operator precedence and documentation will be fixed.
-}
process : ProcessContext -> RawFile.RawFile -> File
process _ (InternalRawFile.Raw file) =
    let
        changes : DeclarationsAndComments
        changes =
            List.foldl
                attachDocumentationAndFixOperators
                { declarations = []
                , previousComments = []
                , remainingComments = file.comments
                }
                file.declarations
    in
    { moduleDefinition = file.moduleDefinition
    , imports = file.imports
    , declarations = List.reverse changes.declarations
    , comments = List.sortWith (\(Node a _) (Node b _) -> Range.compare a b) (changes.remainingComments ++ changes.previousComments)
    }


type alias DeclarationsAndComments =
    { declarations : List (Node Declaration)
    , previousComments : List (Node Comment)
    , remainingComments : List (Node Comment)
    }


attachDocumentationAndFixOperators : Node Declaration -> DeclarationsAndComments -> DeclarationsAndComments
attachDocumentationAndFixOperators declaration context =
    case Node.value declaration of
        FunctionDeclaration function ->
            addDocumentation
                (\doc -> FunctionDeclaration { function | documentation = Just doc })
                (Node (Node.range declaration) (FunctionDeclaration function))
                context

        AliasDeclaration typeAlias ->
            addDocumentation
                (\doc -> AliasDeclaration { typeAlias | documentation = Just doc })
                declaration
                context

        CustomTypeDeclaration typeDecl ->
            addDocumentation
                (\doc -> CustomTypeDeclaration { typeDecl | documentation = Just doc })
                declaration
                context

        PortDeclaration _ ->
            { previousComments = context.previousComments
            , remainingComments = context.remainingComments
            , declarations = declaration :: context.declarations
            }

        InfixDeclaration _ ->
            { previousComments = context.previousComments
            , remainingComments = context.remainingComments
            , declarations = declaration :: context.declarations
            }

        Destructuring _ _ ->
            -- Will never happen. Will be removed in v8
            { previousComments = context.previousComments
            , remainingComments = context.remainingComments
            , declarations = declaration :: context.declarations
            }


addDocumentation : (Node Comment -> Declaration) -> Node Declaration -> DeclarationsAndComments -> DeclarationsAndComments
addDocumentation howToUpdate declaration file =
    let
        ( previous, maybeDoc, remaining ) =
            findDocumentationForRange (Node.range declaration) file.remainingComments []
    in
    case maybeDoc of
        Just doc ->
            { previousComments = previous ++ file.previousComments
            , remainingComments = remaining
            , declarations = Node { start = (Node.range doc).start, end = (Node.range declaration).end } (howToUpdate doc) :: file.declarations
            }

        Nothing ->
            { previousComments = previous ++ file.previousComments
            , remainingComments = remaining
            , declarations = declaration :: file.declarations
            }


findDocumentationForRange : Range -> List (Node String) -> List (Node String) -> ( List (Node String), Maybe (Node String), List (Node String) )
findDocumentationForRange range comments previousComments =
    case comments of
        [] ->
            ( previousComments, Nothing, [] )

        ((Node commentRange commentText) as comment) :: restOfComments ->
            -- Since both comments and declarations are in the order that they appear in the source code,
            -- all the comments we've evaluated until now don't need to be re-evaluated when
            -- trying the find the documentation for later declarations if the current comment is later than the current declaration.
            case compare (commentRange.end.row + 1) range.start.row of
                EQ ->
                    if String.startsWith "{-|" commentText then
                        ( previousComments, Just comment, restOfComments )

                    else
                        -- Aborting because the next comment can't match the next declaration
                        ( previousComments, Nothing, comment :: restOfComments )

                LT ->
                    findDocumentationForRange range restOfComments (comment :: previousComments)

                GT ->
                    -- Aborting because we went too far
                    ( previousComments, Nothing, comment :: restOfComments )
