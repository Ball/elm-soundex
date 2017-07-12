module Style exposing (..)

import Css exposing (..)
import Css.Namespace exposing (namespace)

type CssClasses
    = Cup
    | OrderName
    | Wrapper

css : Stylesheet
css = (stylesheet << namespace "sbux")
    [ class Cup
        [ width (px 615)
        , height (px 369)
        , padding2 (px 200) (px 0)
        , fontSize (em 3)
        , textAlign center
        , textTransform uppercase
        , fontFamilies ["Marker Felt"]
        , backgroundImage (url "http://i2.mirror.co.uk/incoming/article6758362.ece/ALTERNATES/s615b/Starbucks-Cup.jpg")
        ]
    , class OrderName
        [ width (px 610)
        , height (px 40)
        , padding2 (px 10) (px 0)
        , fontSize (em 2)
        , textAlign center
        ]
    , class Wrapper
        [ displayFlex
        , flexDirection column
        , alignItems center
        ]
    ]