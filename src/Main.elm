module Main exposing (..)

import Browser
import Browser.Events
import Date
import Element as E
import Element.Border as EB
import Element.Font as EF
import Html exposing (Html, div, h1, img, input, span, text, video)
import Html.Attributes exposing (autoplay, height, loop, placeholder, src, style, value, width)
import Html.Events as Events
import Json.Decode as Decode
import Material.Button exposing (Config, config, raised, setAttributes, setOnClick, setTouch)
import Task
import Time



---- MODEL ----


type Belonging
    = Default
    | Map
    | Certificate
    | Q1
    | Q2
    | Q3
    | Q4
    | Q5
    | Message


type alias Model =
    { stage : Int
    , step : Int
    , belongings : List Belonging
    , focus : Belonging
    , mapInput : String
    , qInput : String
    , aikotobaInput : String
    , lockNumberInput : String
    , picture : Int
    , date : String
    , searched : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { stage = 0, step = 0, belongings = [], focus = Default, mapInput = "", qInput = "", aikotobaInput = "", picture = 0, lockNumberInput = "", date = "", searched = False }
    , Task.perform NowDate
        (Time.here
            |> Task.andThen
                (\zone ->
                    Task.map
                        (\time ->
                            Date.fromPosix zone time
                        )
                        Time.now
                )
        )
    )



---- UPDATE ----


type Msg
    = NextStage
    | NextStep String
    | FocusOn Belonging
    | MapInput String
    | QInput String
    | AskName String
    | AikotobaInput String
    | NextStepButton
    | PrebStepButton
    | NextPicture
    | PrebPicture
    | LockNumberInput String
    | NowDate Date.Date
    | Search


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextStage ->
            if model.stage == 1 then
                if model.mapInput == "モ" then
                    ( { model | stage = model.stage + 1, step = 0, focus = Default, mapInput = "" }, Cmd.none )

                else
                    ( model, Cmd.none )

            else if model.stage == 3 then
                if model.mapInput == "ヤ" then
                    ( { model | stage = model.stage + 1, step = 0, focus = Default, mapInput = "" }, Cmd.none )

                else
                    ( model, Cmd.none )

            else if model.stage == 4 then
                if model.mapInput == "テ" then
                    ( { model | stage = model.stage + 1, step = 0, focus = Default, mapInput = "" }, Cmd.none )

                else
                    ( model, Cmd.none )

            else if model.stage == 6 then
                if model.aikotobaInput == "せいしゅん" || model.aikotobaInput == "青春" then
                    ( { model | stage = model.stage + 1, step = 0, focus = Default, mapInput = "" }, Cmd.none )

                else
                    ( model, Cmd.none )

            else if model.stage == 7 then
                if model.lockNumberInput == model.date then
                    ( { model | stage = model.stage + 1, step = 0, focus = Default, mapInput = "" }, Cmd.none )

                else
                    ( model, Cmd.none )

            else
                ( { model | stage = model.stage + 1, step = 0, focus = Default, mapInput = "" }, Cmd.none )

        NextStep key ->
            if key == "Enter" || key == "ArrowRight" then
                if model.stage == 1 && model.step == 13 then
                    let
                        belongings =
                            if List.all (\x -> List.member x model.belongings) [ Map, Q1, Certificate ] then
                                model.belongings

                            else
                                [ Certificate, Map, Q1 ]
                    in
                    ( { model | step = model.step + 1, belongings = belongings, focus = Q1 }, Cmd.none )

                else if model.stage == 3 && model.step == 1 then
                    let
                        belongings =
                            if List.member Q2 model.belongings then
                                model.belongings

                            else
                                model.belongings ++ [ Q2 ]
                    in
                    ( { model | step = model.step + 1, belongings = belongings }, Cmd.none )

                else if model.stage == 4 && model.step == 0 then
                    let
                        belongings =
                            if List.member Q3 model.belongings then
                                model.belongings

                            else
                                model.belongings ++ [ Q3 ]
                    in
                    ( { model | step = model.step + 1, belongings = belongings }, Cmd.none )

                else if model.stage == 4 && model.step == 1 then
                    let
                        belongings =
                            if List.member Q4 model.belongings then
                                model.belongings

                            else
                                model.belongings ++ [ Q4 ]
                    in
                    ( { model | step = model.step + 1, belongings = belongings }, Cmd.none )

                else if model.stage == 5 && model.step == 1 then
                    let
                        belongings =
                            if List.all (\x -> List.member x model.belongings) [ Q5, Message ] then
                                model.belongings

                            else
                                model.belongings ++ [ Q5, Message ]
                    in
                    ( { model | step = model.step + 1, belongings = belongings }, Cmd.none )

                else if model.stage == 5 && model.step == 4 then
                    update NextStage model

                else if model.stage == 7 then
                    update NextPicture model

                else
                    ( { model | step = model.step + 1 }, Cmd.none )

            else if key == "ArrowLeft" then
                if model.step /= 0 then
                    ( { model | step = model.step - 1 }, Cmd.none )

                else if model.stage == 7 then
                    update PrebPicture model

                else
                    ( model, Cmd.none )

            else
                ( model, Cmd.none )

        NextStepButton ->
            update (NextStep "Enter") model

        PrebStepButton ->
            update (NextStep "ArrowLeft") model

        FocusOn thing ->
            ( { model | focus = thing }, Cmd.none )

        MapInput input ->
            ( { model | mapInput = input }, Cmd.none )

        QInput input ->
            ( { model | qInput = input }, Cmd.none )

        AikotobaInput input ->
            ( { model | aikotobaInput = input }, Cmd.none )

        AskName name ->
            if name == "イタルさん" then
                ( { model | stage = model.stage + 1, step = 0 }, Cmd.none )

            else
                ( model, Cmd.none )

        NextPicture ->
            if model.picture == 6 then
                ( { model | picture = 0 }, Cmd.none )

            else
                ( { model | picture = model.picture + 1 }, Cmd.none )

        PrebPicture ->
            if model.picture == 0 then
                ( { model | picture = 6 }, Cmd.none )

            else
                ( { model | picture = model.picture - 1 }, Cmd.none )

        LockNumberInput input ->
            ( { model | lockNumberInput = input }, Cmd.none )

        NowDate date ->
            let
                month =
                    String.fromInt <| Date.monthNumber date

                day =
                    String.fromInt <| Date.day date

                month_day =
                    (if String.length month == 1 then
                        "0" ++ month

                     else
                        month
                    )
                        ++ (if String.length day == 1 then
                                "0" ++ day

                            else
                                day
                           )
            in
            ( { model | date = month_day }, Cmd.none )

        Search ->
            ( { model | searched = True }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ style "text-align" "center" ]
        [ case model.focus of
            Default ->
                case model.stage of
                    0 ->
                        viewTitle model

                    1 ->
                        viewIntro model

                    2 ->
                        viewItaruAsk model

                    3 ->
                        viewItaru model

                    4 ->
                        viewCafe model

                    5 ->
                        viewTe model

                    6 ->
                        viewAikotoba model

                    7 ->
                        viewInMuseam model

                    8 ->
                        viewClear model

                    _ ->
                        text ""

            Q1 ->
                viewQ1 model

            Q2 ->
                viewQ2 model

            Q3 ->
                viewQ3 model

            Q4 ->
                viewQ4 model

            Q5 ->
                viewQ5 model

            Map ->
                viewMap model

            Certificate ->
                viewCertificate model

            Message ->
                viewMessage model
        ]


viewTitle : Model -> Html Msg
viewTitle model =
    div viewDivStyle <| viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/img23-e1606056552301.jpg" "" <| buttonTemplate NextStage "スタート"


viewIntro : Model -> Html Msg
viewIntro model =
    let
        sentence =
            case model.step of
                0 ->
                    "おはよう諸君。【焼かれた蝉】に来てくれてありがとう。今日のミッションを説明しよう。"

                1 ->
                    "渋幕現代美術館の特設展示「ダナ・リザ展」が昨日終了した。俺たちはゴトウ・ダ・ヴィンチ作の絵画『ダナ・リザ』を盗み出したい。"

                2 ->
                    "お前たちは運送業者のふりをして、一時間後に運び出される予定の『ダナ・リザ』を回収してくれ。"

                3 ->
                    "煩わしいことに『ダナ・リザ』は二重に保管されている。二重のセキュリティを解除するため、しっかり頑張ってくれ。"

                4 ->
                    "第1のセキュリティは、警備員と運送業者の間にある〈あいことば〉だ。それを解読してくれ。"

                5 ->
                    "〈あいことば〉はメンバーに謎にしてバラバラに渡してある。まずは渋幕校内に散らばっている【焼かれた蝉】のメンバーに会って、情報を集めてこい。"

                6 ->
                    "だが、【焼かれた蝉】のメンバーはみんなポンコツで、情報の意味がわからないらしい。まったく……。"

                7 ->
                    "君たちには彼らがまだ分かっていない謎を解き明かしてあいことばを導いてくれ。"

                8 ->
                    "最初のメンバーの位置は後に渡す地図上のモだ。覚えておけ。"

                9 ->
                    "それ以外のメンバーの位置はそれぞれのメンバーが次のメンバーの位置を示す暗号を持っているから彼らから情報を得てくれ。"

                10 ->
                    "〈あいことば〉を導きだし、警備員をだまして美術館に入ることができたら次は第2のセキュリティだ。"

                11 ->
                    "第2のセキュリティは、『ダナ・リザ』が保管されている部屋にかかった4桁のパスコードによるロックだ。"

                12 ->
                    "美術館内に存在する謎を解いて、『ダナ・リザ』の保管された部屋のパスコードを解明し、部屋に入ることができたらミッション成功だ。"

                13 ->
                    "それでは検討を祈る。参加証と地図を渡しておく。最初の指示はこれだ。"

                _ ->
                    ""

        imgsrc =
            "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/IMG_0058-e1606308779660.jpg"
    in
    div viewDivStyle <|
        viewTemplate imgsrc
            sentence
            [ div [] <|
                [ raised (buttonConfig PrebStepButton) "前へ"
                , raised (buttonConfig NextStepButton) "次へ"
                ]
            , div [] <| belongingsButton model
            ]


viewItaruAsk : Model -> Html Msg
viewItaruAsk model =
    div viewDivStyle <|
        viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/IMG_0105-e1606402081539.jpg"
            "話しかけよう"
            [ div []
                [ raised (buttonConfig <| AskName "テツオさん") "テツオさん"
                , raised (buttonConfig <| AskName "タカハシさん") "タカハシさん"
                , raised (buttonConfig <| AskName "イタルさん") "イタルさん"
                ]
            , div [] <| belongingsButton model
            ]


viewItaru : Model -> Html Msg
viewItaru model =
    let
        sentence =
            case model.step of
                0 ->
                    "こんにちは。イタルです。"

                1 ->
                    "これがQ2です。どうぞ。"

                2 ->
                    "次のメンバーは、何か食べに行っているはずです。"

                _ ->
                    ""
    in
    div viewDivStyle <|
        viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/IMG_0105-e1606402081539.jpg"
            sentence
            [ div [] <|
                [ raised (buttonConfig PrebStepButton) "前へ"
                , raised (buttonConfig NextStepButton) "次へ"
                ]
            , div [] <| belongingsButton model
            ]


viewCafe : Model -> Html Msg
viewCafe model =
    let
        sentence =
            case model.step of
                0 ->
                    "こんにちは。待っていました。Q3をどうぞ。"

                1 ->
                    "次のメンバーの位置は、Q4を解くとわかります。それでは。"

                _ ->
                    ""
    in
    div viewDivStyle <|
        viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/iOS-の画像-1-e1606468931405.jpg"
            sentence
            [ div [] <|
                [ raised (buttonConfig PrebPicture) "前へ"
                , raised (buttonConfig NextPicture) "次へ"
                ]
            , div [] <| belongingsButton model
            ]


viewTe : Model -> Html Msg
viewTe model =
    let
        sentence =
            case model.step of
                0 ->
                    "よくここまで来たな。<あいことば>までもうすぐだ。"

                1 ->
                    "これがQ5だ。それからボスからの伝言も預かっている。"

                2 ->
                    "それから階段広告と英語科研究室についてはすでに調べておいたぞ。俺は有能だからな。"

                3 ->
                    "階段広告が「こんにちは」で、英語科研究室が「しぶやまくはり」だ。メモしておけよ。"

                4 ->
                    "しかしこれが一体何の役に立つのか..."

                _ ->
                    ""
    in
    div viewDivStyle <|
        viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/IMG_0046-e1606472538273.jpg"
            sentence
            [ div [] <|
                [ raised (buttonConfig PrebStepButton) "前へ"
                , raised (buttonConfig NextStepButton) "次へ"
                ]
            , div [] <| belongingsButton model
            ]


viewAikotoba : Model -> Html Msg
viewAikotoba model =
    div viewDivStyle <|
        viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/hogehoge-e1606476240792.jpg"
            "美術館に着いた。<あいことば>を伝え、美術館へ侵入せよ!"
            [ div []
                [ input [ placeholder "<あいことば>を入力", value model.aikotobaInput, Events.onInput AikotobaInput ] []
                , raised (buttonConfig NextStage) "決定"
                ]
            , div [] <| belongingsButton model
            ]


viewInMuseam : Model -> Html Msg
viewInMuseam model =
    let
        imgsrc =
            case model.picture of
                0 ->
                    "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/IMG_0063-e1606484016934.jpg"

                1 ->
                    "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/IMG_0065-e1606484044852.jpg"

                2 ->
                    "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/IMG_0067-e1606484075580.jpg"

                3 ->
                    "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/IMG_0066-e1606484089268.jpg"

                4 ->
                    "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/IMG_0068-e1606484108693.jpg"

                5 ->
                    "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/IMG_0069-e1606484133751.jpg"

                6 ->
                    "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/IMG_0070-e1606484152304.jpg"

                _ ->
                    ""
    in
    div viewDivStyle <|
        if model.picture /= 6 then
            viewTemplate imgsrc
                "暗証番号の謎を解け!"
                [ div [] <|
                    [ raised (buttonConfig PrebPicture) "前へ"
                    , raised (buttonConfig NextPicture) "次へ"
                    ]
                ]

        else
            viewTemplate imgsrc
                "四桁の暗証番号を入力せよ"
                [ div []
                    [ input [ placeholder "暗証番号を入力", value model.lockNumberInput, Events.onInput LockNumberInput ] []
                    , raised (buttonConfig NextStage) "決定"
                    ]
                , div [] <|
                    [ raised (buttonConfig PrebPicture) "前へ"
                    , raised (buttonConfig NextPicture) "次へ"
                    ]
                ]


viewClear : Model -> Html Msg
viewClear model =
    div viewDivStyle <|
        [ video
            [ src "https://drive.google.com/uc?export=view&id=1RYsuAewj1SJ3SJ7ii8iuqZu9ixlkPoB9"
            , autoplay True
            , style "width" "100%"
            , style "max-height" "700px"
            , loop True
            ]
            []
        , E.layout
            [ E.htmlAttribute <| style "text-align" "left"
            , E.padding 3
            , E.htmlAttribute <| style "max-width" "721px"
            , E.htmlAttribute <| style "width" "100%"
            ]
          <|
            E.paragraph
                [ EB.width 3
                , EB.solid
                , EB.rounded 8
                , EB.color <| E.rgb255 0 128 0
                , E.centerY
                , E.alignLeft
                , E.paddingXY 5 10
                ]
                [ E.el [ EF.size 25 ] <| E.text "クリアおめでとう!!" ]
        ]


viewQ1 : Model -> Html Msg
viewQ1 model =
    div viewDivStyle <| viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/2-Do╢e╗i╒-1_page-0004-e1606400532419.jpg" "" [ div [] <| belongingsButton model ]


viewQ2 : Model -> Html Msg
viewQ2 model =
    div viewDivStyle <| viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/2-D文化祭-2_page-0003-e1606404346174.jpg" "" [ div [] <| belongingsButton model ]


viewQ3 : Model -> Html Msg
viewQ3 model =
    div viewDivStyle <| viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/2-D文化祭-2_page-0003-1-e1606404392790.jpg" "" [ div [] <| belongingsButton model ]


viewQ4 : Model -> Html Msg
viewQ4 model =
    div viewDivStyle <| viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/2-D文化祭-2_page-0007-1.jpg" "" [ div [] <| belongingsButton model ]


viewQ5 : Model -> Html Msg
viewQ5 model =
    div viewDivStyle <| viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/2-D文化祭-2_page-0003-2-e1606404430835.jpg" "" [ div [] <| belongingsButton model ]


viewMap : Model -> Html Msg
viewMap model =
    div viewDivStyle <|
        viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/img26-e1606219688612.jpg"
            "移動先を入力して移動しよう"
            [ div []
                [ input [ placeholder "移動先の記号を入力", value model.mapInput, Events.onInput MapInput ] []
                , raised (buttonConfig NextStage) "決定"
                ]
            , div [] <| belongingsButton model
            ]


viewCertificate : Model -> Html Msg
viewCertificate model =
    let
        imgsrc =
            if model.searched then
                "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/2-D文化祭-2_page-0012.jpg"

            else
                "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/615399_s-e1606491537448.jpg"
    in
    div viewDivStyle <|
        viewTemplate imgsrc
            ""
        <|
            (if model.stage == 6 then
                if model.searched then
                    []

                else
                    [ div [] [ raised (buttonConfig Search) "調べてみる" ] ]

             else
                []
            )
                ++ [ div [] <| belongingsButton model
                   ]


viewMessage : Model -> Html Msg
viewMessage model =
    div viewDivStyle <|
        viewTemplate "https://enju2020.shibumaku.jp/wp-content/uploads/2020/11/2-D文化祭-2_page-0004-e1606404532787.jpg"
            ""
            [ div [] <| belongingsButton model
            ]


viewTemplate : String -> String -> List (Html Msg) -> List (Html Msg)
viewTemplate imgsrc sentence content =
    [ img
        [ src imgsrc
        , style "max-width" "721px"
        , style "width" "100%"
        ]
        []
    , E.layout
        [ E.htmlAttribute <| style "text-align" "left"
        , E.padding 3
        , E.htmlAttribute <| style "max-width" "721px"
        , E.htmlAttribute <| style "width" "100%"
        ]
      <|
        if sentence /= "" then
            E.paragraph
                [ EB.width 3
                , EB.solid
                , EB.rounded 8
                , EB.color <| E.rgb255 0 128 0
                , E.centerY
                , E.alignLeft
                , E.paddingXY 5 10
                ]
                [ E.el [ EF.size 25 ] <| E.text sentence ]

        else
            E.none
    ]
        ++ content


buttonTemplate : Msg -> String -> List (Html Msg)
buttonTemplate msg name =
    [ div []
        [ raised (buttonConfig msg) name
        ]
    ]


belongingsButton : Model -> List (Html Msg)
belongingsButton model =
    let
        belongingButton : Belonging -> Html Msg
        belongingButton belonging =
            case belonging of
                Map ->
                    raised (buttonConfig <| FocusOn Map) "地図"

                Q1 ->
                    raised (buttonConfig <| FocusOn Q1) "Q1"

                Q2 ->
                    raised (buttonConfig <| FocusOn Q2) "Q2"

                Q3 ->
                    raised (buttonConfig <| FocusOn Q3) "Q3"

                Q4 ->
                    raised (buttonConfig <| FocusOn Q4) "Q4"

                Q5 ->
                    raised (buttonConfig <| FocusOn Q5) "Q5"

                Certificate ->
                    raised (buttonConfig <| FocusOn Certificate) "参加証"

                Default ->
                    text ""

                Message ->
                    raised (buttonConfig <| FocusOn Message) "伝言"
    in
    (if model.focus /= Default then
        [ raised (buttonConfig <| FocusOn Default) "戻る"
        ]

     else
        []
    )
        ++ List.map belongingButton model.belongings


buttonConfig : msg -> Config msg
buttonConfig msg =
    setOnClick msg
        (setAttributes [ style "background-color" "#008000" ]
            (setTouch
                True
                config
            )
        )


viewDivStyle : List (Html.Attribute msg)
viewDivStyle =
    [ style "display" "inline-block"
    , style "max-width" "721px"
    , style "width" "100%"
    ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown <| keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map NextStep (Decode.field "key" Decode.string)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
