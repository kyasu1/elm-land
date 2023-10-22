module PageFile exposing
    ( PageFile
    , decoder
    , hasDynamicParameters
    , isAdvancedElmLandPage
    , isAuthProtectedPage
    , isNotFoundPage
    , isSandboxOrElementElmLandPage
    , isTopLevelCatchAllPage
    , sortBySpecificity
    , toList
    , toModuleName
    , toPageModelTypeDeclaration
    , toParamsRecordAnnotation
    , toRouteFromStringBranch
    , toRouteVariant
    , toVariantName
    )

import CodeGen
import CodeGen.Annotation
import CodeGen.Declaration
import CodeGen.Expression
import Extras.String
import Json.Decode
import Util.String


type PageFile
    = PageFile Internals


type alias Internals =
    { filepath : List String
    , contents : String
    , isSandboxOrElementElmLandPage : Bool
    , isAdvancedElmLandPage : Bool
    , isAuthProtectedPage : Bool
    }


decoder : Json.Decode.Decoder PageFile
decoder =
    Json.Decode.oneOf
        [ Json.Decode.field "contents" Json.Decode.string
        , Json.Decode.succeed ""
        ]
        |> Json.Decode.andThen
            (\contents ->
                Json.Decode.map PageFile
                    (Json.Decode.map5 Internals
                        (Json.Decode.field "filepath" (Json.Decode.list Json.Decode.string))
                        (Json.Decode.succeed contents)
                        (Json.Decode.succeed (isSandboxOrElementElmLandPageInternal contents))
                        (Json.Decode.succeed (isAdvancedElmLandPageInternal contents))
                        (Json.Decode.succeed (isAuthProtectedPageInternal contents))
                    )
            )


isSandboxOrElementElmLandPage : PageFile -> Bool
isSandboxOrElementElmLandPage (PageFile page) =
    page.isSandboxOrElementElmLandPage


isSandboxOrElementElmLandPageInternal : String -> Bool
isSandboxOrElementElmLandPageInternal contents =
    String.contains "Page.sandbox" contents || String.contains "Page.element" contents


isAdvancedElmLandPage : PageFile -> Bool
isAdvancedElmLandPage (PageFile page) =
    page.isAdvancedElmLandPage


isAdvancedElmLandPageInternal : String -> Bool
isAdvancedElmLandPageInternal contents =
    String.contains "Page.new" contents


isAuthProtectedPage : PageFile -> Bool
isAuthProtectedPage (PageFile page) =
    page.isAuthProtectedPage


isAuthProtectedPageInternal : String -> Bool
isAuthProtectedPageInternal contents =
    String.contains "page : Auth.User -> Shared.Model" contents


toPageModelTypeDeclaration : List PageFile -> CodeGen.Declaration
toPageModelTypeDeclaration pages =
    let
        toPageModelCustomType : List ( String, List CodeGen.Annotation )
        toPageModelCustomType =
            let
                toCustomType : PageFile -> ( String, List CodeGen.Annotation.Annotation )
                toCustomType page =
                    ( toVariantName page
                    , List.concat
                        [ if hasDynamicParameters page then
                            [ toParamsRecordAnnotation page ]

                          else
                            []
                        , if isSandboxOrElementElmLandPage page || isAdvancedElmLandPage page then
                            [ CodeGen.Annotation.type_ (toModuleName page ++ ".Model")
                            ]

                          else
                            []
                        ]
                    )
            in
            List.concat
                [ List.map toCustomType pages
                , [ ( "Redirecting_", [] )
                  , ( "Loading_", [] )
                  ]
                ]
    in
    CodeGen.Declaration.customType
        { name = "Model"
        , variants = toPageModelCustomType
        }


isNotFoundPage : PageFile -> Bool
isNotFoundPage (PageFile { filepath }) =
    filepath == [ "NotFound_" ]


hasDynamicParameters : PageFile -> Bool
hasDynamicParameters page =
    not (isNotFoundPage page) && not (List.isEmpty (toDynamicParameterList page))


toDynamicParameterList : PageFile -> List String
toDynamicParameterList (PageFile { filepath }) =
    List.filter
        (\piece ->
            not (List.member piece [ "Home_", "NotFound_" ])
                && String.endsWith "_" piece
        )
        filepath


toList : PageFile -> List String
toList (PageFile { filepath }) =
    if List.isEmpty filepath then
        [ "Home_" ]

    else
        filepath


toParamsRecordAnnotation : PageFile -> CodeGen.Annotation
toParamsRecordAnnotation (PageFile { filepath }) =
    let
        isCatchAllRoute : Bool
        isCatchAllRoute =
            filepath |> String.join "/" |> String.endsWith "ALL_"

        addConditionalCatchAllParameters : List ( String, CodeGen.Annotation ) -> List ( String, CodeGen.Annotation )
        addConditionalCatchAllParameters fields =
            if isCatchAllRoute then
                fields
                    ++ [ ( "all_"
                         , CodeGen.Annotation.type_ "List String"
                         )
                       ]

            else
                fields
    in
    filepath
        |> List.filter (\str -> str /= "ALL_" && str /= "NotFound_" && String.endsWith "_" str)
        |> List.map (String.dropRight 1)
        |> List.map Extras.String.fromPascalCaseToCamelCase
        |> List.map (\fieldName -> ( fieldName, CodeGen.Annotation.string ))
        |> addConditionalCatchAllParameters
        |> CodeGen.Annotation.record


isTopLevelCatchAllPage : PageFile -> Bool
isTopLevelCatchAllPage (PageFile { filepath }) =
    filepath == [ "ALL_" ]


toVariantName : PageFile -> String
toVariantName (PageFile { filepath }) =
    String.join "_" filepath


toRouteVariant : PageFile -> ( String, List CodeGen.Annotation )
toRouteVariant page =
    ( toVariantName page
    , if hasDynamicParameters page then
        [ toParamsRecordAnnotation page ]

      else
        []
    )


toRouteFromStringBranch : PageFile -> CodeGen.Expression.Branch
toRouteFromStringBranch page =
    let
        (PageFile { filepath }) =
            page

        branchPattern : List String -> List String
        branchPattern segments =
            case segments of
                [] ->
                    [ "[]" ]

                "Home_" :: [] ->
                    [ "[]" ]

                "ALL_" :: [] ->
                    [ "all_" ]

                "ALL_" :: rest ->
                    branchPattern ("all_" :: rest)

                piece :: remaining ->
                    let
                        segmentPiece : String
                        segmentPiece =
                            if piece /= "NotFound_" && String.endsWith "_" piece then
                                piece
                                    |> Extras.String.fromPascalCaseToCamelCase

                            else
                                piece
                                    |> Extras.String.fromPascalCaseToKebabCase
                                    |> Util.String.quote
                    in
                    segmentPiece :: branchPattern remaining
    in
    { name = branchPattern filepath |> String.join " :: "
    , arguments = []
    , expression =
        if hasDynamicParameters page then
            CodeGen.Expression.pipeline
                [ CodeGen.Expression.multilineFunction
                    { name = toVariantName page
                    , arguments =
                        [ CodeGen.Expression.multilineRecord
                            (toDynamicParameterList page
                                |> List.map
                                    (\str ->
                                        if str == "ALL_" then
                                            ( "all_", CodeGen.Expression.value "all_" )

                                        else
                                            ( str |> String.dropRight 1 |> Extras.String.fromPascalCaseToCamelCase
                                            , CodeGen.Expression.value (str |> Extras.String.fromPascalCaseToCamelCase)
                                            )
                                    )
                            )
                        ]
                    }
                , CodeGen.Expression.value "Just"
                ]

        else
            CodeGen.Expression.function
                { name = "Just"
                , arguments =
                    [ CodeGen.Expression.value (toVariantName page)
                    ]
                }
    }


toModuleName : PageFile -> String
toModuleName (PageFile { filepath }) =
    if List.isEmpty filepath then
        "Pages.Home_"

    else
        "Pages." ++ String.join "." filepath


sortBySpecificity : List PageFile -> List PageFile
sortBySpecificity pages =
    pages
        |> List.sortWith sorter


type Specificity
    = Static
    | Dynamic
    | CatchAll


toSpecificity : String -> Specificity
toSpecificity segment =
    if segment == "ALL_" then
        CatchAll

    else if segment == "NotFound_" then
        CatchAll

    else if String.endsWith "_" segment then
        Dynamic

    else
        Static


sorter : PageFile -> PageFile -> Basics.Order
sorter (PageFile page1) (PageFile page2) =
    let
        segments1 : List String
        segments1 =
            page1.filepath

        segments2 : List String
        segments2 =
            page2.filepath

        compareBySpecificity : List String -> List String -> Basics.Order
        compareBySpecificity list1 list2 =
            case ( list1, list2 ) of
                ( [], [] ) ->
                    Basics.EQ

                ( [], _ ) ->
                    Basics.LT

                ( _, [] ) ->
                    Basics.GT

                ( first1 :: rest1, first2 :: rest2 ) ->
                    case compareSpecificityOfSegment first1 first2 of
                        Basics.LT ->
                            Basics.LT

                        Basics.GT ->
                            Basics.GT

                        Basics.EQ ->
                            compareBySpecificity rest1 rest2

        compareSpecificityOfSegment : String -> String -> Basics.Order
        compareSpecificityOfSegment str1 str2 =
            case ( toSpecificity str1, toSpecificity str2 ) of
                ( Static, Static ) ->
                    Basics.compare str1 str2

                ( Dynamic, Dynamic ) ->
                    Basics.compare str1 str2

                ( CatchAll, CatchAll ) ->
                    Basics.compare str1 str2

                ( Static, _ ) ->
                    Basics.LT

                ( _, Static ) ->
                    Basics.GT

                ( Dynamic, _ ) ->
                    Basics.LT

                ( _, Dynamic ) ->
                    Basics.GT

        isHomepage_ : List String -> Bool
        isHomepage_ segments =
            segments == [ "Home_" ]

        isNotFoundPage_ : List String -> Bool
        isNotFoundPage_ segments =
            segments == [ "NotFound_" ]
    in
    if isHomepage_ segments1 then
        Basics.LT

    else if isHomepage_ segments2 then
        Basics.GT

    else if isNotFoundPage_ segments1 then
        Basics.GT

    else if isNotFoundPage_ segments2 then
        Basics.LT

    else
        compareBySpecificity segments1 segments2
