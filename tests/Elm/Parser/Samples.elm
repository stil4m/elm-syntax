module Elm.Parser.Samples exposing (allSamples)


allSamples : List ( Int, String )
allSamples =
    [ ( 1, sample1 )
    , ( 2, sample2 )
    , ( 3, sample3 )
    , ( 4, sample4 )
    , ( 5, sample5 )
    , ( 6, sample6 )
    , ( 7, sample7 )
    , ( 8, sample8 )
    , ( 9, sample9 )
    , ( 11, sample11 )
    , ( 12, sample12 )
    , ( 14, sample14 )
    , ( 15, sample15 )
    , ( 16, sample16 )
    , ( 17, sample17 )
    , ( 18, sample18 )
    , ( 19, sample19 )
    , ( 20, sample20 )
    , ( 21, sample21 )
    , ( 22, sample22 )
    , ( 23, sample23 )
    , ( 24, sample24 )
    , ( 25, sample25 )
    , ( 26, sample26 )
    , ( 27, sample27 )
    , ( 28, sample28 )
    , ( 29, sample29 )
    , ( 30, sample30 )
    , ( 31, sample31 )
    , ( 32, sample32 )
    , ( 33, sample33 )
    , ( 34, sample34 )
    , ( 35, sample35 )
    , ( 36, sample36 )
    , ( 37, sample37 )
    , ( 38, sample38 )
    , ( 39, sample39 )
    , ( 40, sample40 )
    , ( 41, sample41 )
    , ( 42, sample42 )
    , ( 43, sample43 )
    , ( 44, sample44 )
    , ( 45, sample45 )
    , ( 46, sample46 )
    , ( 47, sample47 )
    , ( 48, sample48 )
    , ( 49, sample49 )
    , ( 50, sample50 )
    , ( 51, sample51 )
    , ( 52, sample52 )
    , ( 53, sample53 )
    , ( 54, sample54 )
    ]


sample51 : String
sample51 =
    """module A exposing (..)

f =
    [case a of
                _ -> ""]
"""


sample50 : String
sample50 =
    """module A exposing (..)

f =
    [case a of
                _ -> ""
               ]
"""


sample49 : String
sample49 =
    """module A exposing (..)

f =
    [case a of
                _ -> ""
                ]
"""


sample48 : String
sample48 =
    """module A exposing (..)

f =
    (\\a -> case a of
                _ -> "")
"""


sample47 : String
sample47 =
    """module A exposing (..)

f =
    (\\a -> case a of
                _ -> ""
               )
"""


sample46 : String
sample46 =
    """module A exposing (..)

f =
    (\\a -> case a of
                _ -> ""
                )
"""


sample45 : String
sample45 =
    """module A exposing (..)

a =
    case b of
        [
         ] ->
            ""
"""


sample44 : String
sample44 =
    """module A exposing (..)

type B a= B

"""


sample43 : String
sample43 =
    """module A exposing (..)

a =
    case b of
        [  ] ->
            ""
"""


sample42 : String
sample42 =
    """module PegTest exposing (..)

a =
    infixr

b =
    infixl

c =
    infix
"""


sample41 : String
sample41 =
    """module A exposing ( infixl , infixr, infix )

a = 0
"""


sample40 : String
sample40 =
    """module A exposing (..)

type alias A msg=
    { a : Int
    }
"""


sample39 : String
sample39 =
    """
module Test exposing (asSomething)

import Import


asSomething : Int
asSomething =
    123
"""


sample38 : String
sample38 =
    """
module B exposing (x)

c =
  Maybe.map
      (\\( p, v, s ) ->
          Operation
      )




"""


sample37 : String
sample37 =
    """
module Elm.Parser.Imports exposing (importDefinition)

importX : A
importX =
    B

"""


sample36 : String
sample36 =
    """module Parser.Advanced exposing( Parser)


-- WHITESPACE


{-| Just like [`Parser.spaces`](Parser#spaces)
-}
spaces : Parser c x ()
spaces =
  chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')

"""


sample35 : String
sample35 =
    """
module Validate exposing (isWhitespaceChar)

isWhitespaceChar : Char -> Bool
isWhitespaceChar char =
    char == ' ' || char == '\\n' || char == '\\t' || char == '\\u{000D}'
"""


sample34 : String
sample34 =
    """
module Foo exposing (..)


{-|-}
type_ : String -> Attribute msg
type_ =
  bar  "type"

"""


sample33 : String
sample33 =
    """
module Lazy.List  exposing (..)

type D = C a B
"""


sample32 : String
sample32 =
    """module SomeModule exposing (..)

x =
  case y of
      z ->
          a

toString num =
    1
"""


sample31 : String
sample31 =
    """
module SomeModule exposing (..)

x =
    {- y -} z
"""


sample30 : String
sample30 =
    """module Hex exposing (fromString, toString)

{-| Convert to and from Hex strings.

@docs fromString, toString
-}


{-| Convert a hexdecimal string such as "abc94f" to a decimal integer.

    Hex.fromString "a5" == Ok 165
    Hex.fromString "hat" == Err "invalid hexadecimal string"
-}
fromString : String -> Result String Int
fromString str =
    if String.isEmpty str then
        Err "Empty strings are not valid hexadecimal strings."
    else
        let
            result =
                if String.startsWith "-" str then
                    let
                        list =
                            str
                                |> String.toList
                                |> List.tail
                                |> Maybe.withDefault []
                    in
                        fromStringHelp (List.length list - 1) list 0
                            |> Result.map negate
                else
                    fromStringHelp (String.length str - 1) (String.toList str) 0

            formatError err =
                String.join " "
                    [ Basics.toString str
                    , "is not a valid hexadecimal string because"
                    , err
                    ]
        in
            Result.mapError formatError result


fromStringHelp : Int -> List Char -> Int -> Result String Int
fromStringHelp position chars accumulated =
    case chars of
        [] ->
            Ok accumulated

        char :: rest ->
            let
                recurse additional =
                    fromStringHelp
                        (position - 1)
                        rest
                        (accumulated + (additional * (16 ^ position)))
            in
                case char of
                    '0' ->
                        recurse 0

                    '1' ->
                        recurse 1

                    '2' ->
                        recurse 2

                    '3' ->
                        recurse 3

                    '4' ->
                        recurse 4

                    '5' ->
                        recurse 5

                    '6' ->
                        recurse 6

                    '7' ->
                        recurse 7

                    '8' ->
                        recurse 8

                    '9' ->
                        recurse 9

                    'a' ->
                        recurse 10

                    'b' ->
                        recurse 11

                    'c' ->
                        recurse 12

                    'd' ->
                        recurse 13

                    'e' ->
                        recurse 14

                    'f' ->
                        recurse 15

                    nonHex ->
                        Err (Basics.toString nonHex ++ " is not a valid hexadecimal character.")


{-| Convert a decimal integer to a hexdecimal string such as "abc94f"

    Hex.toString 165 == Ok "a5"
-}
toString : Int -> String
toString num =
    String.fromList <|
        if num < 0 then
            '-' :: unsafePositiveToDigits [] (negate num)
        else
            unsafePositiveToDigits [] num


{-| ONLY EVER CALL THIS WITH POSITIVE INTEGERS!
-}
unsafePositiveToDigits : List Char -> Int -> List Char
unsafePositiveToDigits digits num =
    if num < 16 then
        unsafeToDigit num :: digits
    else
        unsafePositiveToDigits (unsafeToDigit (modBy num 16) :: digits) (num // 16)

"""


sample29 : String
sample29 =
    """module Bar exposing (..)

type Color = Blue

"""


sample28 : String
sample28 =
    """module Foo exposing (..)

x =
    let
        result : Int
        result =
            1
    in
        result
"""


sample27 : String
sample27 =
    """module Foo exposing (..)

type alias Post = {
  id: Int,
  title: String,
  text: Maybe String
  }

"""


{-| Let with comments and exact indent
-}
sample26 : String
sample26 =
    """module Foo exposing (..)

x b =
  let
      a =
          b

      --time
  in
      1


"""


{-| Trailing whitespace multiline comment
-}
sample25 : String
sample25 =
    String.concat
        [ "module HelloWorld exposing (..)\n\n"
        , "{-| an individual Note (no pitch class implies a rest) -}    \n"
        , "type alias AnInt = Int\n"
        ]


sample1 : String
sample1 =
    """module HelloWorld exposing (..)

import Html exposing (text)

main =
  text "Hello, World!"
"""


sample2 : String
sample2 =
    """-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html
module Buttons exposing (..)

import Html exposing (beginnerProgram, div, button, text)
import Html.Events exposing (onClick)

main =
  beginnerProgram { model = 0, view = view, update = update }


view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]


type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1
"""


{-| Moduless file
-}
sample3 : String
sample3 =
    """module X exposing (..)
import Html

main =
  Html.text <| toString foo

foo = 1
"""


sample4 : String
sample4 =
    """module Operators exposing ((|=), (|.))

infix left 5 (|=) = keeper
infix left 6 (|.) = ignorer
"""


sample5 : String
sample5 =
    """module Realm exposing (updateState)

{-| This library exposes a single helper function to help interface with the Realm npm package.

# updateState
@docs updateState
-}

{-| Given an update function and an outgoing port to send your Elm model into JavaScript, returns a new update function which automatically sends the new model to JavaScript after running the update.
-}

updateState : (msg -> model -> (model, Cmd msg)) -> SendPort msg model -> msg -> model -> (model, Cmd msg)
updateState update sendPort = curry <| (uncurry update) >> batchStateCmds sendPort


batchStateCmds : SendPort msg model -> (model, Cmd msg) -> (model, Cmd msg)
batchStateCmds sendPort nextStateAndCmd =
    1
"""


sample6 : String
sample6 =
    """module Realm exposing (updateState)

type alias SendPort msg model = model -> Cmd msg

"""


sample7 : String
sample7 =
    """port module Store exposing (..)

bar foo =
    foo.bar

"""


sample8 : String
sample8 =
    """port module Store exposing (..)

foo bar =
  { bar | n = 2}
"""


sample9 : String
sample9 =
    """port module Store exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, [])

        Increment ->
            ({ model | value = model}, [])



      """


sample11 : String
sample11 =
    """module Foo exposing (..)

type alias X =
    { foo : Bar.Baz
    }
"""


sample12 : String
sample12 =
    """module Z exposing (..)

x : A -> B
x baz =
    let
        { bar } =
            baz

    in
        bar

"""


sample14 : String
sample14 =
    """module G exposing (..)

e =
    let
        c = d
    in
        [
        ]
"""


sample15 : String
sample15 =
    """module G exposing (..)

e =
    let
        c =
            c
    in
        [ a
        ]
"""


sample16 : String
sample16 =
    """module Foo exposing (..)

bar baz3 =
  Foo1
"""


sample17 : String
sample17 =
    """module Foo exposing (..)
bar =
    '1'"""


sample18 : String
sample18 =
    """port module Ports exposing (..)

import Scroll exposing (Move)

port scroll : (Move -> msg) -> Sub msg"""


sample19 : String
sample19 =
    """module Foo exposing (..)


timeAgo time now =
    if x then
        1
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else if x then
        2
    else
        3

"""


sample20 : String
sample20 =
    """module Baz exposing (..)

foo = bar
-- some

"""


sample21 : String
sample21 =
    """module Elmo8.Layers.Background exposing (..)

{-| Background layer

Used for rendering backgrounds (and maps?)


-}

import Math.Vector2 exposing (Vec2, vec2, fromTuple)
import WebGL


-- From http://blog.tojicode.com/2012/07/sprite-tile-maps-on-gpu.html



tileMapVertextShader : WebGL.Shader { attr | position : Vec2, texture : Vec2 } { uniform | viewOffset : Vec2, viewportSize : Vec2, inverseTileTextureSize : Vec2, inverseTileSize : Float } { pixelCoord : Vec2, texCoord : Vec2 }
tileMapVertextShader =
    [glsl|
    precision mediump float;
    attribute vec2 position;
    attribute vec2 texture;

    varying vec2 pixelCoord;
    varying vec2 texCoord;

    uniform vec2 viewOffset;
    uniform vec2 viewportSize;
    uniform vec2 inverseTileTextureSize;
    uniform float inverseTileSize;

    void main(void) {
        pixelCoord = (texture * viewportSize) + viewOffset;
        texCoord = pixelCoord * inverseTileTextureSize * inverseTileSize;
        gl_Position = vec4(position, 0.0, 1.0);
    }
|]

"""


sample22 : String
sample22 =
    """module Foo exposing (..)

x : a -> b -> a
x y z =
  let
    _ = 1
  in
    z
"""


sample23 : String
sample23 =
    """module Foo exposing (..)

x s =
  let indent = String.length s in
  indent

"""


sample24 : String
sample24 =
    """module Foo exposing (..)

tests =
    \\() -> Expect.equal "'\\\\''" (toString '\\'')
"""


sample52 : String
sample52 =
    """
module Simplify exposing
    ( rule
    , Configuration, defaults, expectNaN, ignoreCaseOfForTypes
    )

{-| Reports when an expression can be simplified.


-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Project exposing (Exposed)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Simplify.AstHelpers as AstHelpers exposing (emptyStringAsString, qualifiedToString)
import Simplify.Evaluate as Evaluate
import Simplify.Infer as Infer
import Simplify.Match as Match exposing (Match(..))
import Simplify.Normalize as Normalize
import Simplify.RangeDict as RangeDict exposing (RangeDict)


{-| Rule to simplify Elm code.
-}
rule : Configuration -> Rule
rule (Configuration config) =
    Rule.newProjectRuleSchema "Simplify" initialContext
        |> Rule.withDirectDependenciesProjectVisitor (dependenciesVisitor (Set.fromList config.ignoreConstructors))
        |> Rule.withModuleVisitor (moduleVisitor config)
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema
"""


sample53 : String
sample53 =
    """
module Simplify exposing
    ( rule
    , Configuration, defaults, expectNaN, ignoreCaseOfForTypes
    )

{-| Reports when an expression can be simplified.

🔧 Running with `--fix` will automatically remove all the reported errors.

    config =
        [ Simplify.rule Simplify.defaults
        ]

@docs rule
@docs Configuration, defaults, expectNaN, ignoreCaseOfForTypes


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplify/example --rules Simplify
```


## Simplifications

Below is the list of all kinds of simplifications this rule applies.


### Booleans

    x || True
    --> True

    x || False
    --> x

    x && True
    --> x

    x && False
    --> False

    not True
    --> False

    not (not x)
    --> x

    -- for `<`, `>`, `<=`, `>=`, `==` and `/=`
    not (a < b)
    --> a >= b

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Project exposing (Exposed)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Simplify.AstHelpers as AstHelpers exposing (emptyStringAsString, qualifiedToString)
import Simplify.Evaluate as Evaluate
import Simplify.Infer as Infer
import Simplify.Match as Match exposing (Match(..))
import Simplify.Normalize as Normalize
import Simplify.RangeDict as RangeDict exposing (RangeDict)


{-| Rule to simplify Elm code.
-}
rule : Configuration -> Rule
rule (Configuration config) =
    Rule.newProjectRuleSchema "Simplify" initialContext
        |> Rule.withDirectDependenciesProjectVisitor (dependenciesVisitor (Set.fromList config.ignoreConstructors))
        |> Rule.withModuleVisitor (moduleVisitor config)
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema

"""


sample54 : String
sample54 =
    -- copied from https://github.com/pdamoc/elm-syntax-sscce
    -- big thanks!
    """port module Main exposing (Msg(..), Natural, main)

{-| The above declaration shows how to:

  - declare a port module
  - expose all the tags of a custom type (Msg)
  - expose only the type (Natural)

-}

-- Shows how to import everthing from a module (Html). It is recommended to avoid this syntax.
-- Shows how to create an alias for a module name (Events)
-- Shows how to import multiple modules into a single namespace (Math). Use this with great care as it can create confusion about the source of a function.

import Browser
import Html exposing (..)
import Html.Events as Events exposing (onClick)
import Math.Matrix4 as Math
import Math.Vector2 as Math
import Math.Vector3 as Math
import WebGL



-- CUSTOM TYPES


{-| Shows how to define a single variant custom type.
Exposing only the type and not the tags ensures that the values of the type are created only through functions that can enforce constrains.
-}
type Natural
    = Natural Int


{-| Shows how to define a function that creates a value for the above custom type
-}
fromInt : Int -> Natural
fromInt intValue =
    -- Tags of the custom types are also acting as functions that create the custom type.
    -- max function is defined in the Basics module from elm/core and is imported by default. See elm/core for a list of default imports.
    Natural (max intValue 0)


{-| Shows how to unpack custom type parameters. Works only if the type has a single variant.
-}
toInt : Natural -> Int
toInt (Natural value) =
    value


{-| Shows how to define a function in a pointfree style by composing two functions.
-}
toString : Natural -> String
toString =
    -- String.fromInt shows how to use a module that is imported by default.
    toInt >> String.fromInt


{-| Shows how to control the operations on the your custom type.
In this case the code makes sure you are not storing negative values inside the custom type
-}
addInt : Natural -> Int -> Natural
addInt natural intValue =
    let
        -- One ca unpack / destructure a custom type inside a let..in too
        (Natural value) =
            natural
    in
    fromInt (value + intValue)


{-| Adds 42 (the value is an Int written with HEX notation).
-}
addMeaning : Natural -> Natural
addMeaning (Natural value) =
    fromInt (value + 0x2A)


{-| Shows how to create a type alias for a type that extends records. This alias will extend any other record with the field `name`.
-}
type alias Named a =
    { a | name : String }


{-| Shows how to use the above type alias.
-}
type alias NamedValue a =
    Named { value : a }


{-| Shows how to use the values from an extensible record alias fields
-}
namedToHtml : Named a -> Html msg
namedToHtml { name } =
    text name


namedNaturalToHtml : NamedValue Natural -> Html msg
namedNaturalToHtml namedValue =
    div []
        [ namedToHtml namedValue
        , text ": "
        , text (toString namedValue.value)
        ]


{-| Shows how to create a phantom type
-}
type Unit a
    = Unit Int


{-| When adding two units, the type parameter must be the same.
-}
addUnit : Unit a -> Unit a -> Unit a
addUnit (Unit first) (Unit second) =
    Unit (first + second)


{-| A type to be used with the above Unit type
-}
type Meter
    = Meter


{-| A second type to be used with the above Unit type
-}
type Gram
    = Gram


twoMeters : Unit Meter
twoMeters =
    Unit 2


threeMeters : Unit Meter
threeMeters =
    Unit 3


fewGrams : Unit Gram
fewGrams =
    Unit 21


someMeters : Unit Meter
someMeters =
    -- This works because the two units match
    addUnit twoMeters threeMeters



{- This value will throw an error if uncommented
   impossibleAdd : Unit Meter
   impossibleAdd =
       -- This doesn't work because the types don't match
       addUnit fewGrams someMeters
-}
-- MODEL


{-| Shows how to tie a name to a record type.
-}
type alias Model =
    { count : Natural
    , namedCount : NamedValue Natural
    }


{-| Shows how to ignore a parameter you are not using
This purposefully shows a function without a type signature although top level functions and values should have type signatures.
-}
init _ =
    ( { count = Natural 0
      , namedCount =
            { name = "Natural", value = Natural 0 }
      }
    , Cmd.none
    )



-- UPDATE


{-| Shows how to give a new name to a more complex type
-}
type alias Naturals =
    List Natural


{-| Shows how to define a custom type with multiple variants
-}
type Msg
    = Increment
    | Decrement
    | AddNextTen
    | OnSubscription (Result String Naturals)


{-| Shows how to unpack a record parameter while still keeping the full parameter.
You can use a subset of the fields in the record if you only need certain fields.
This function type signature has been purpusefully spread over multiple lines to show that complex signatures need not be single line.
-}
update :
    Msg
    -> Model
    -> ( Model, Cmd msg )
update msg ({ count } as model) =
    case msg of
        Increment ->
            ( { model | count = addInt count 1 }, Cmd.none )

        Decrement ->
            -- Shows how to create a new scope with a let..in expression
            let
                -- values and function defined inside let..in can have type signatures although they usually don't
                newValue : Natural
                newValue =
                    addInt count -1

                -- this is how to use the Debug.log to check for a value
                _ =
                    Debug.log "newValue" newValue

                -- this shows that you can declare multiple _ values without the compiler complaining.
                -- attempting to use a named declaration multiple times will result in a compiler error
                _ =
                    newValue
                        -- adding the next line at the end of a declaration with result in it being logged to the JS console
                        |> Debug.log "newValue"
            in
            if newValue == count then
                -- Shows how to call a port
                ( model, reportError "There are no negative Natural numbers" )

            else
                ( { model | count = newValue }, Cmd.none )

        AddNextTen ->
            let
                addIntToCount =
                    -- Shows how to partially apply a function.
                    -- This is very useful in scopes where the first arguments stay the same
                    addInt count

                intCount =
                    toInt count

                addTen =
                    -- Shows how to use an infix operator as a prefix function.
                    -- This is useful when you want to partially apply the operator and use
                    -- the resulting function in a higher order function.
                    (+) 10

                nextTen =
                    -- Shows how to use an infix operator as an argument in a higher order function.
                    List.foldl (+) 0 (List.range intCount (addTen intCount))
            in
            ( { model | count = addIntToCount nextTen }, Cmd.none )

        -- Shows how to unpack a variant by matching against the contained variants
        OnSubscription (Ok naturals) ->
            case naturals of
                -- Shows how to pattern match on a List
                [] ->
                    -- Shows how to recursively call update in order to avoid duplicating code.
                    update (OnSubscription (Err "Received an empty list")) model

                -- Shows how to pattern match on a list with a fixed number of elements
                [ first ] ->
                    ( { model | count = first }, Cmd.none )

                -- Shows how to pattern match on a list with at least two elements.
                first :: second :: _ ->
                    ( { model | count = first }, Cmd.none )

        OnSubscription (Err error) ->
            ( model, reportError error )



-- VIEW


{-| Shows how to declare a String that spans multiple lines
-}
multiline : String
multiline =
    \"\"\"
    This is a multiline string. 
    It will be displayed by spliting the lines into separate paragraphs. 
\"\"\"


{-| Shows how to define a tuple.
-}
initials : ( Char, Char )
initials =
    -- Show how to declare a Char type.
    ( 'J', 'D' )


view : Model -> Html Msg
view model =
    let
        namedCount =
            -- Shows how to access a field from a record by using a field accessor function
            .namedCount model

        -- Shows how to pattern match a tuple
        ( first, last ) =
            initials

        -- a helper function
        named value =
            { name = value }

        -- shows that record field accessors work on expressions too
        nameFromExpression =
            (named "Foo").name
    in
    main_ []
        [ button [ onClick Increment ] [ text "+1" ]

        -- Shows how to avoid parentheses by using the backwards pipe operator
        , div [] [ text <| toString model.count ]

        -- Shows how to used a function from a module without having to expose it in the import section.
        , div [] [ button [ Events.onClick Decrement ] [ text "-1" ] ]
        , button [ onClick AddNextTen ] [ text "Add Next Ten" ]
        , div [] [ namedNaturalToHtml namedCount ]
        , String.lines multiline
            |> List.map (\\line -> p [] [ text line ])
            |> div []
        , footer [] [ text (String.fromChar first), text (String.fromChar last) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Listen to the incomming port only if condition in the model is met
    if toInt model.count < 5 then
        -- Show how to use an anonymous function (lambda expression)
        fromJS
            (\\value ->
                case List.map String.toInt value of
                    [] ->
                        OnSubscription (Err "Received an empty list")

                    (Just int) :: _ ->
                        let
                            output =
                                -- This shows how to prepend an element to a list
                                fromInt int :: []
                        in
                        if int >= 0 then
                            OnSubscription (Ok output)

                        else
                            -- Shows how to structure a complex function application by using the "pipe" operator
                            ("Received a negative number: " ++ String.fromInt int)
                                |> Err
                                |> OnSubscription

                    -- Shows how to catch all remaining variants. Watch out for this pattern as it can create troubles.
                    _ ->
                        OnSubscription (Err "Received a list that started with a non-integer ")
            )

    else
        Sub.none



-- WIRING


{-| Signature for Browser.element is Program flags model msg.
The flags type argument here is the Unit type: ()
-}
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


{-| Shows how to define an outgoing port
-}
port reportError : String -> Cmd msg


{-| Shows how to define an incomming port.
The first parameter is a function that takes the data received from JS and produces a message that the app understands.
-}
port fromJS : (List String -> msg) -> Sub msg



-- ADVANCED SYNTAX


{-| Elm also has special syntax for declaring WebGL shaders. See more about this at: <https://github.com/elm-explorations/webgl/>
-}
vertexShader : WebGL.Shader { a | coord : Math.Vec3, position : Math.Vec3 } { b | view : Math.Mat4 } { vcoord : Math.Vec2 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform   mat4 view;
varying   vec2 vcoord;

void main () {
  gl_Position = view * vec4(position, 1.0);
  vcoord = coord.xy;
}
|]"""
