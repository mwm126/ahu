module Ahu exposing (..)

import AhuInstructions exposing (ahuinstructions)
import AhuModel exposing (..)
import AhuText exposing (ahutext)
import AhuUnits exposing (..)
import Char exposing (..)
import Color exposing (..)
import Html exposing (Attribute, div, text, input)
import Html exposing (Html, div, button, text, label)
import Html.Attributes as H exposing (min, max, value)
import Html.Attributes exposing (style, placeholder)
import Html.Events exposing (on, onInput)
import Html.Events exposing (onClick)
import Markdown exposing (..)
import Material
import Material.Toggles as Toggles
import Material.Options as Options exposing (css)
import Material.Scheme
import Material.Tabs as Tabs
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( init_model
    , Cmd.none
    )



-- Duration of animation cycle (Seconds)


cycle : Time
cycle =
    10


time_mod : Time -> Model -> Float
time_mod time model =
    let
        t =
            Time.inSeconds time

        ct =
            cycle
    in
        (t - ct * (toFloat (floor (t / ct)))) / ct


type Msg
    = SetAirflow String
    | SetOap String
    | SetOat String
    | SetOawb String
    | SetSat String
    | SetShf String
    | SetTons String
    | Tick Time
    | SelectTab Int
    | ToggleUnits
    | Mdl (Material.Msg Msg)


get_oa_t : Model -> Float
get_oa_t model =
    inUnits model.system model.outside_air_t


get_oa_wb : Model -> Float
get_oa_wb model =
    inUnits model.system model.outside_air_wb


get_sa_t : Model -> Float
get_sa_t model =
    inUnits model.system model.supply_air.t


get_airflow : Model -> Float
get_airflow model =
    inUnitsAirflow model.system model.airflow


get_load : Model -> Float
get_load model =
    inUnitsPower model.system model.load



-- round x to n decimal places


roundn : Int -> Float -> Float
roundn n x =
    let
        f =
            toFloat (10 ^ n)

        scaled =
            f * x

        fh =
            toFloat (round scaled)
    in
        -- toFloat (round (x*f))/f
        fh / f


toLoad : Model -> String -> Power
toLoad model s =
    let
        d =
            case String.toFloat s of
                Ok f ->
                    f

                Err _ ->
                    0.0
    in
        case model.system of
            Metric ->
                Kilowatts d

            Imperial ->
                Tons d


toAirflow : Model -> String -> AirFlow
toAirflow model s =
    let
        d =
            case String.toFloat s of
                Ok f ->
                    f

                Err _ ->
                    0.0
    in
        case model.system of
            Metric ->
                CubicMetersPerSecond d

            Imperial ->
                CubicFeetPerMinute d


toTemp : Model -> String -> Temperature
toTemp model s =
    let
        d =
            case String.toFloat s of
                Ok f ->
                    f

                Err _ ->
                    0.0
    in
        case model.system of
            Metric ->
                Celsius d

            Imperial ->
                Fahrenheit d


stringToFloat : String -> Float
stringToFloat s =
    case String.toFloat s of
        Ok f ->
            f

        Err m ->
            0.0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        new_model =
            case msg of
                SetOap p ->
                    { model | oa_p = stringToFloat p }

                SetAirflow f ->
                    { model | airflow = toAirflow model f }

                SetShf f ->
                    { model | load_shf = stringToFloat f / 100 }

                SetTons f ->
                    { model | load = toLoad model f }

                SetOawb wb ->
                    { model | outside_air_wb = toTemp model wb }

                SetSat t ->
                    { model | supply_air = { t = toTemp model t, rh = model.supply_air.rh } }

                SetOat t ->
                    { model | outside_air_t = toTemp model t }

                ToggleUnits ->
                    { model
                        | system =
                            if model.system == Metric then
                                Imperial
                            else
                                Metric
                    }

                Tick newTime ->
                    { model
                        | time = time_mod newTime model
                        , building_air = new_building_air model
                    }

                SelectTab idx ->
                    { model | tab = idx }

                Mdl msg_ ->
                    Tuple.first (Material.update Mdl msg_ model)
    in
        ( new_model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (0.1 * second) Tick


control_style : Html.Attribute Msg
control_style =
    Html.Attributes.style
        [ ( "padding", "5px 10px" )
        ]


ctrl_style : Html.Attribute Msg
ctrl_style =
    Html.Attributes.style
        [ ( "display", "inline-block" )
        , ( "text-align", "left" )
        ]


show_style : Html.Attribute Msg
show_style =
    Html.Attributes.style
        [ ( "float", "right" )
        , ( "width", "50%" )
        , ( "display", "inline-block" )
        ]


type alias Range =
    ( Float, Float )


oa_t_range : Model -> Range
oa_t_range model =
    ( inUnits model.system <| Fahrenheit 80.0, inUnits model.system <| Fahrenheit 94.0 )


oawb_range : Model -> Range
oawb_range model =
    ( inUnits model.system <| Fahrenheit 65.0, inUnits model.system <| Fahrenheit 85.0 )


sa_t_range : Model -> Range
sa_t_range model =
    ( inUnits model.system <| Fahrenheit 45.0, inUnits model.system <| Fahrenheit 60.0 )


load_range : Model -> Range
load_range model =
    ( inUnitsPower model.system <| Tons 20.0, inUnitsPower model.system <| Tons 100.0 )


airflow_range : Model -> Range
airflow_range model =
    ( inUnitsAirflow model.system <| CubicFeetPerMinute 20000.0, inUnitsAirflow model.system <| CubicFeetPerMinute 50000.0 )


ahusim : Model -> Html Msg
ahusim model =
    let
        pro_x =
            150

        pro_y =
            150

        r2 x =
            toString <| roundn 20 x

        show name value =
            Html.text (name ++ " = " ++ r2 value)

        q_in =
            inTons <| q_total_in model

        q_sens =
            inTons <| q_sensible model

        shf_in =
            supply_shf model

        building_t =
            inFahrenheit model.building_air.t

        (HPercent building_rh) =
            model.building_air.rh
    in
        Html.section
            [ Html.Attributes.style
                [ ( "width", "100%" )
                , ( "text-align", "center" )
                ]
            ]
            [ div [ ctrl_style ]
                [ Toggles.switch Mdl
                    [ 0 ]
                    model.mdl
                    [ Options.onToggle ToggleUnits
                    , Toggles.ripple
                    , Toggles.value (model.system == Metric)
                    ]
                    [ Html.text "Metric?" ]
                , Html.text "Specify the weather outdoors."
                , div [ redStyle ]
                    [ control model SetOat (oa_t_range model) get_oa_t <| "Outside Air Temp " ++ inUnitsString model.system model.outside_air_t
                    , control model SetOawb (oawb_range model) get_oa_wb <| "Outside Air Wet Bulb " ++ inUnitsString model.system model.outside_air_wb
                    , control model SetOap ( 20.0, 100.0 ) .oa_p <| "Outside Air " ++ toString model.oa_p ++ "%"
                    ]
                , Html.p [] []
                , Html.text "Setup the system by specifying load..."
                , div [ grayStyle ]
                    [ control model SetTons (load_range model) get_load <| "Cooling Load " ++ inUnitsPowerString model.system model.load
                    , control model SetShf ( 40.0, 100.0 ) ((*) 100 << .load_shf) <| "Load Sensible Heat Factor " ++ toString (100 * model.load_shf) ++ "%"
                    ]
                , Html.p [] []
                , Html.text "Now adjust the system to maintain comfort."
                , div [ blueStyle ]
                    [ control model SetSat (sa_t_range model) get_sa_t <| "Supply Air Temp " ++ inUnitsString model.system model.supply_air.t
                    , control model SetAirflow (airflow_range model) get_airflow <| "Airflow " ++ inUnitsAirflowString model.system model.airflow
                    ]
                , Html.text (building_comment model)
                , Html.p [] []
                ]
            , div
                [ Html.Attributes.style
                    [ ( "margin-left", "10px" )
                    , ( "display", "inline-block" )
                    , ( "vertical-align", "top" )
                    , ( "border", "1px solid black" )
                    , ( "width", "50%" )
                    ]
                ]
                [ svg [ viewBox "0 0 600 400", Svg.Attributes.width "100%" ]
                    (List.concat
                        [ (protractor pro_x pro_y model)
                        , duct model
                        , psych_chart model
                        ]
                    )
                ]
            ]


comfort_temp_max : Temperature
comfort_temp_max =
    Fahrenheit 81


comfort_temp_min : Temperature
comfort_temp_min =
    Fahrenheit 69


comfort_rh_max : RelativeHumidity
comfort_rh_max =
    HPercent 60


comfort_rh_min : RelativeHumidity
comfort_rh_min =
    HPercent 30


comfort_hr_max : HumidityRatio
comfort_hr_max =
    MolecularRatio 0.011


comfort_hr_min : HumidityRatio
comfort_hr_min =
    MolecularRatio 0.0055


building_comment : Model -> String
building_comment model =
    let
        (MolecularRatio building_hr) =
            humidity_ratio model.building_air

        building_t =
            inFahrenheit model.building_air.t

        (MolecularRatio hr_max) =
            comfort_hr_max

        (MolecularRatio hr_min) =
            comfort_hr_min

        temp_max =
            inFahrenheit comfort_temp_max

        temp_min =
            inFahrenheit comfort_temp_min
    in
        if building_hr > hr_max then
            "Ugh!  It's too humid."
            -- Humidity:"++(toString <| roundn 2 <| building_hr )
        else if building_hr < hr_min then
            "It's too dry. "
            -- Humidity:"++(toString building_hr )
        else if building_t > temp_max then
            "Whew!  It's too hot in here! "
            -- Temperature:"++(toString <| roundn 2 <| building_t )
        else if building_t < temp_min then
            "Brrr!  It's too cold in here! "
            -- Temperature:"++(toString <| roundn 2 <| building_t )
        else
            ""



-- for drawing the house


air_radius : Float
air_radius =
    10


duct_height : Float
duct_height =
    6 * air_radius


duct_width : Float
duct_width =
    12 * air_radius



-- house offsets


house_x : Temperature
house_x =
    Fahrenheit 50


house_y : Float
house_y =
    30


duct : Model -> List (Svg Msg)
duct model =
    let
        rr =
            air_radius

        dd =
            2 * air_radius

        hh =
            duct_height

        ww =
            duct_width

        xx =
            inFahrenheit house_x

        yy =
            house_y

        ( ax, ay ) =
            .air_location (pie_sprites model)

        ( rx, ry ) =
            .recirc_air_location (pie_sprites model)

        -- points in house icon
        hx =
            toString <| duct_width + xx

        hy =
            toString <| rr + yy

        house_side =
            toString <| 5 * rr

        house =
            Svg.image [ xlinkHref "/home-512.png", x hx, y hy, width house_side, height house_side ] []

        coil_x =
            xx + ww * 0.6

        coil_y =
            yy + hh - rr

        coil x =
            line [ x1 (toString (coil_x + x)), y1 (toString (coil_y - rr)), x2 (toString (coil_x + x)), y2 (toString (coil_y + rr)), stroke "blue" ] []
    in
        [ line [ x1 (toString xx), y1 (toString yy), x2 (toString (xx + ww)), y2 (toString yy), stroke "black" ] []
        , rect [ x (toString xx), y (toString (yy + dd)), width (toString dd), height (toString dd), fill "gray" ] []
        , rect [ x (toString (xx + 2 * dd)), y (toString (yy + dd)), width (toString (ww - 2 * dd)), height (toString dd), fill "gray" ] []
        , line [ x1 (toString xx), y1 (toString (yy + hh)), x2 (toString (xx + ww)), y2 (toString (yy + hh)), stroke "black" ] []
        , house
        , coil 10
        , coil 14
        , coil 18
        , coil 22
        , coil 26
        , Svg.text_ [ x (toString coil_x), y (toString coil_y), dx "15", dy "25", fontSize "10", stroke "blue" ] [ Html.text "coil" ]
        , Svg.text_ [ x (toString xx), y (toString yy), dx "-10", dy "5", fontSize "10", writingMode "tb", stroke "lightgray" ] [ Html.text "outside" ]
        , Svg.text_ [ x (toString <| xx + duct_width), y (toString yy), dx "10", dy "0", fontSize "10", stroke "lightgray" ] [ Html.text "inside" ]

        -- , circle [ cx (toString ax), cy (toString ay), r "10", fill (sprite_states model).air_color ] [ ]
        , pie ax ay 10 (1 - model.oa_p / 100 - 0.25) 0.75 <| asString (pie_sprites model).recirc_air_color
        , pie rx ry 10 -0.25 (1.0 - model.oa_p / 100 - 0.25) <| asString (pie_sprites model).air_color
        ]


protractor : Float -> Float -> Model -> List (Svg msg)
protractor t u model =
    let
        w =
            20

        shf_load =
            model.load_shf

        q_tot_in =
            inTons <| q_total_in model

        load =
            inTons model.load

        shf_supply =
            supply_shf model

        -- θ_supply = atan shf_supply
        -- θ_load = atan shf_load
        -- sensible heat flow in
        x_1 =
            t

        y_1 =
            u

        -- load
        x_2 =
            t - load * sin (shf_load * pi / 2)

        y_2 =
            u + load * cos (shf_load * pi / 2)

        -- supply
        x_3 =
            t - q_tot_in * sin (shf_supply * pi / 2)

        y_3 =
            u + q_tot_in * cos (shf_supply * pi / 2)

        -- x_2 = t - load*sin(θ_load)
        -- y_2 = u + load*cos(θ_load)
        -- x_3 = t - q_in*sin(θ_supply)
        -- y_3 = u + q_in*cos(θ_supply)
    in
        -- [
        [ pieline x_1 y_1 (round load) 0 0.5

        -- building center
        , Svg.text_ [ x (toString x_1), y (toString y_1), dx "5", dy "-5", fontSize "10" ] [ Html.text "Cooling Vectors" ]
        , circle [ cx (toString x_1), cy (toString y_1), r "4", fill "green" ] []
        , Svg.text_ [ x (toString x_1), y (toString y_1), dx "5", dy "5", fontSize "10" ] [ Html.text "building" ]

        -- load vector
        , line [ x1 (toString x_1), y1 (toString y_1), x2 (toString x_2), y2 (toString y_2), stroke "black" ] []
        , circle [ cx (toString x_2), cy (toString y_2), r "4", fill "gray" ] []
        , Svg.text_ [ x (toString x_2), y (toString y_2), dx "-30", dy "5", fontSize "10" ] [ Html.text "load" ]

        -- supply vector
        , line [ x1 (toString x_1), y1 (toString y_1), x2 (toString x_3), y2 (toString y_3), stroke "black" ] []
        , circle [ cx (toString x_3), cy (toString y_3), r "4", fill "blue" ] []
        , Svg.text_ [ x (toString x_3), y (toString y_3), dx "5", dy "5", fontSize "10" ] [ Html.text "supply" ]

        -- protractor
        ]


th_to_xy : ( Temperature, HumidityRatio ) -> ( Float, Float )
th_to_xy ( temp, rel_h ) =
    let
        bottom =
            350

        t =
            inFahrenheit temp

        (MolecularRatio h) =
            rel_h

        x =
            (t - 40) * (toFloat bottom - 100) / (95 - 40) + 100

        y =
            (0.029 - h) * (toFloat bottom - 100) / (0.029 - 0.0052) + 100
    in
        ( x, y )


asString : Color -> String
asString clr =
    let
        t =
            toRgb clr

        hex =
            toRadix 16

        r =
            String.padLeft 2 '0' <| hex t.red

        b =
            String.padLeft 2 '0' <| hex t.blue

        g =
            String.padLeft 2 '0' <| hex t.green
    in
        "#" ++ r ++ g ++ b


air_state : ( Temperature, HumidityRatio, Color ) -> String -> String -> String -> List (Svg msg)
air_state thc label d_x d_y =
    let
        ( t, h, clr ) =
            thc

        ( x_1, y_1 ) =
            th_to_xy ( t, h )
    in
        [ circle [ cx (toString x_1), cy (toString y_1), r "4", fill <| asString clr ] []
        , Svg.text_ [ x (toString x_1), y (toString y_1), dx d_x, dy d_y, fontSize "10" ] [ Html.text label ]

        -- , Svg.text_ [ x (toString x_1), y (toString y_1), dx d_x, dy d_y, fontSize "10" ] [ Html.text <| (toString clr) ++ (asString clr)]
        ]


mixed_thc : Model -> ( Temperature, HumidityRatio, Color )
mixed_thc model =
    let
        ( t, h, c ) =
            avg_thc (building_thc model) (outside_thc model) (model.oa_p / 100)
    in
        ( t, h, Color.yellow )


building_thc : Model -> ( Temperature, HumidityRatio, Color )
building_thc model =
    ( model.building_air.t, humidity_ratio model.building_air, Color.green )


sa_thc : Model -> ( Temperature, HumidityRatio, Color )
sa_thc model =
    ( model.supply_air.t, humidity_ratio model.supply_air, Color.lightBlue )


wetbulb_to_specifichumidity : Temperature -> HumidityRatio
wetbulb_to_specifichumidity temperature =
    let
        t =
            inFahrenheit temperature

        wb =
            0.025 * (t - 65) / (85 - 65) + 0.007
    in
        -- not accurate; just fudged estimate of specifichumidity from wetbulb
        MolecularRatio wb


outside_thc : Model -> ( Temperature, HumidityRatio, Color )
outside_thc model =
    ( model.outside_air_t, wetbulb_to_specifichumidity model.outside_air_wb, Color.red )


avg_thc : ( Temperature, HumidityRatio, Color ) -> ( Temperature, HumidityRatio, Color ) -> Float -> ( Temperature, HumidityRatio, Color )
avg_thc xy1 xy2 t =
    let
        ( t1, MolecularRatio y1, c1 ) =
            xy1

        x1 =
            inFahrenheit t1

        ( t2, MolecularRatio y2, c2 ) =
            xy2

        x2 =
            inFahrenheit t2

        clr =
            avg_color c1 c2 t
    in
        ( Fahrenheit (x1 + (x2 - x1) * t), MolecularRatio (y1 + (y2 - y1) * t), clr )


avg : ( Float, Float ) -> ( Float, Float ) -> Float -> ( Float, Float )
avg xy1 xy2 t =
    let
        ( x1, y1 ) =
            xy1

        ( x2, y2 ) =
            xy2
    in
        ( (x1 + (x2 - x1) * t), y1 + (y2 - y1) * t )


avg_int : Float -> Float -> Float -> Int
avg_int r1 r2 t =
    round (r1 + (r2 - r1) * t)


avg_color : Color -> Color -> Float -> Color
avg_color c1 c2 t =
    let
        rgb1 =
            (Color.toRgb c1)

        rgb2 =
            (Color.toRgb c2)

        f =
            toFloat

        ( r1, g1, b1 ) =
            ( f rgb1.red, f rgb1.green, f rgb1.blue )

        ( r2, g2, b2 ) =
            ( f rgb2.red, f rgb2.green, f rgb2.blue )
    in
        rgb (avg_int r1 r2 t) (avg_int g1 g2 t) (avg_int b1 b2 t)


type alias ThcSprites =
    { recirc_thc : ( Temperature, HumidityRatio, Color )
    , oa_thc : ( Temperature, HumidityRatio, Color )
    }


type alias PieSprites =
    { air_location : ( Float, Float )
    , recirc_air_location : ( Float, Float )
    , air_color : Color
    , recirc_air_color : Color
    }


type AnimationStage
    = EnteringBuilding Float
    | EnteringCooling Float
    | ExitingBuilding Float
    | MixingIntake Float


find_stage : Model -> AnimationStage
find_stage model =
    let
        t =
            model.time
    in
        if model.time < 0.25 then
            -- passing through the building
            ExitingBuilding ((model.time) / 0.25)
        else if model.time < 0.5 then
            -- exiting the building
            MixingIntake ((model.time - 0.25) / 0.25)
        else if model.time < 0.75 then
            -- separate air and recirc
            EnteringCooling ((model.time - 0.5) / 0.25)
        else
            -- entering the building
            EnteringBuilding ((model.time - 0.75) / 0.25)


thc_sprites : Model -> ThcSprites
thc_sprites model =
    let
        xx =
            inFahrenheit house_x

        yy =
            house_y

        stage =
            find_stage model

        building =
            building_thc model

        mixed =
            mixed_thc model
    in
        case stage of
            ExitingBuilding pp ->
                { recirc_thc = building
                , oa_thc = building
                }

            MixingIntake pp ->
                { recirc_thc = avg_thc building mixed pp
                , oa_thc = avg_thc (outside_thc model) mixed pp
                }

            EnteringCooling pp ->
                let
                    th_xy =
                        let
                            ( mix_t, mix_h, _ ) =
                                mixed
                        in
                            if pp < 0.5 then
                                let
                                    ( avg_t, avg_h, c ) =
                                        avg_thc mixed (sa_thc model) pp
                                in
                                    ( avg_t, mix_h, c )
                            else
                                let
                                    ( half_t, _, c ) =
                                        avg_thc mixed (sa_thc model) 0.5
                                in
                                    avg_thc ( half_t, mix_h, c ) (sa_thc model) (2 * pp - 1)
                in
                    { recirc_thc = th_xy
                    , oa_thc = th_xy
                    }

            EnteringBuilding pp ->
                let
                    th_xy =
                        avg_thc (sa_thc model) building pp
                in
                    { recirc_thc = th_xy
                    , oa_thc = th_xy
                    }


pie_sprites : Model -> PieSprites
pie_sprites model =
    let
        xx =
            inFahrenheit house_x

        yy =
            house_y

        rr =
            air_radius

        stage =
            find_stage model
    in
        case stage of
            ExitingBuilding pp ->
                let
                    pie_color =
                        avg_color green green pp
                in
                    { air_location = avg ( xx + duct_width + rr, yy + rr ) ( xx, yy + rr ) pp
                    , recirc_air_location = avg ( xx + duct_width + rr, yy + rr ) ( xx + duct_width * 0.3 + rr, yy + rr ) pp
                    , air_color = pie_color
                    , recirc_air_color = pie_color
                    }

            MixingIntake pp ->
                { air_location = avg ( xx - rr, yy + rr ) ( xx - rr, yy + duct_height - rr ) pp
                , recirc_air_location = avg ( xx + duct_width * 0.3 - rr, yy + rr ) ( xx + duct_width * 0.3 - rr, yy + duct_height - rr ) pp
                , air_color = avg_color green green pp
                , recirc_air_color = avg_color green red pp
                }

            EnteringCooling pp ->
                let
                    pie_color =
                        avg_color green green pp
                in
                    { air_location = avg ( xx, yy + duct_height - rr ) ( xx + duct_width, yy + duct_height - rr ) pp
                    , recirc_air_location = avg ( xx + duct_width * 0.3, yy + duct_height - rr ) ( xx + duct_width, yy + duct_height - rr ) pp
                    , air_color = avg_color yellow blue pp
                    , recirc_air_color = avg_color green green pp
                    }

            EnteringBuilding pp ->
                let
                    pie_xy =
                        avg ( xx + duct_width + rr, yy + duct_height - rr ) ( xx + duct_width + rr, yy + rr ) pp

                    pie_color =
                        avg_color blue green pp
                in
                    { air_location = pie_xy
                    , recirc_air_location = pie_xy
                    , air_color = pie_color
                    , recirc_air_color = pie_color
                    }


psych_chart : Model -> List (Svg msg)
psych_chart model =
    let
        mkTempHr =
            (\( t, x ) -> ( Fahrenheit t, MolecularRatio x ))

        saturation_line : List ( Temperature, HumidityRatio )
        saturation_line =
            List.map mkTempHr
                [ ( 40.0, 0.0052 )
                , ( 45.0, 0.0063 )
                , ( 50.0, 0.0076 )
                , ( 55.0, 0.0092 )
                , ( 60.0, 0.0112 )
                , ( 65.0, 0.0132 )
                , ( 70.0, 0.0158 )
                , ( 75.0, 0.0188 )
                , ( 80.0, 0.0223 )
                , ( 85.0, 0.0264 )
                , ( 90.0, 0.029 )
                , ( 95.0, 0.029 )
                ]

        saturation_line_xy =
            (List.map th_to_xy saturation_line)

        -- make_line takes pairs of (temperature, humidity) and transforms to pixels
        make_line : String -> ( Float, Float ) -> ( Float, Float ) -> Svg msg
        make_line c ( x, y ) ( xx, yy ) =
            line [ x1 (toString x), y1 (toString y), x2 (toString xx), y2 (toString yy), stroke c ] []

        p_horiz ( x, y ) =
            make_line "lightgray" ( x, y ) ( 450, y )

        p_vert ( x, y ) =
            make_line "lightgray" ( x, y ) ( x, 370 )

        r =
            "blue"

        some_temperature =
            50

        -- I don't know what this should be
        c1 =
            th_to_xy ( comfort_temp_min, humidity_ratio { rh = comfort_rh_min, t = comfort_temp_min } )

        c2 =
            th_to_xy ( comfort_temp_min, humidity_ratio { rh = comfort_rh_max, t = comfort_temp_min } )

        c3 =
            th_to_xy ( comfort_temp_max, humidity_ratio { rh = comfort_rh_max, t = comfort_temp_max } )

        c4 =
            th_to_xy ( comfort_temp_max, humidity_ratio { rh = comfort_rh_min, t = comfort_temp_max } )

        x_axis_label =
            Svg.text_ [ x "350", y "370", fontSize "10" ] [ Html.text "Temperature" ]

        y_axis_label =
            Svg.text_ [ x "470", y "100", fontSize "10", writingMode "tb" ] [ Html.text "Humidity Ratio" ]

        comfort_label =
            Svg.text_ [ x "270", y "347", fontSize "10", stroke "blue" ] [ Html.text "Comfort Zone" ]

        -- y_axis_label = Svg.text_ [] [Html.text "Specific Humidity"]
        comfort_zone =
            [ make_line r c1 c2
            , make_line r c2 c3
            , make_line r c3 c4
            , make_line r c4 c1
            ]

        -- Draw the saturation line
        sat_line_points : List ( ( Float, Float ), ( Float, Float ) )
        sat_line_points =
            List.map2 (,) saturation_line_xy (List.drop 1 saturation_line_xy)

        sat_line_lines : List (Svg msg)
        sat_line_lines =
            List.map (\( start, end ) -> make_line "black" start end) sat_line_points
    in
        List.concat
            [ List.map p_horiz saturation_line_xy
            , List.map p_vert saturation_line_xy
            , sat_line_lines
            , List.concat
                [ air_state (outside_thc model) "Outside Air (OA)" "5" "5"
                , air_state (mixed_thc model) "Mixed Air (MA)" "5" "5"
                , air_state (building_thc model) "Return Air (RA)" "5" "5"
                , air_state (sa_thc model) "Supply Air (SA)" "-90" "5"
                , air_state (.oa_thc (thc_sprites model)) "OA" "15" "15"
                , air_state (.recirc_thc (thc_sprites model)) "RA" "15" "-5"
                ]
            , comfort_zone
            , [ x_axis_label
              , y_axis_label
              , comfort_label
              ]
            ]


control : Model -> (String -> Msg) -> Range -> (Model -> Float) -> String -> Html Msg
control model set range get label =
    let
        ( minval, maxval ) =
            range

        val =
            toString << roundn 2 << get <| model
    in
        div [ control_style ]
            [ input
                [ type_ "range"
                , H.min <| toString minval
                , H.max <| toString maxval
                , value val
                , onInput set
                ]
                []
            , Html.text label
            ]


inlineStyle : Html.Attribute Msg
inlineStyle =
    Html.Attributes.style
        [ ( "display", "inline" )
        ]


blueStyle : Html.Attribute Msg
blueStyle =
    Html.Attributes.style
        [ ( "font-family", "-apple-system, system, sans-serif" )
        , ( "background-color", "#9999FF" )
        ]


redStyle : Html.Attribute Msg
redStyle =
    Html.Attributes.style
        [ ( "font-family", "-apple-system, system, sans-serif" )
        , ( "background-color", "#FF9999" )
        ]


grayStyle : Html.Attribute Msg
grayStyle =
    Html.Attributes.style
        [ ( "font-family", "-apple-system, system, sans-serif" )
        , ( "background-color", "#999999" )
        ]


toRadix : Int -> Int -> String
toRadix r n =
    let
        getChr c =
            if c < 10 then
                toString c
            else
                String.fromChar <| Char.fromCode (87 + c)

        getStr b =
            if n < b then
                getChr n
            else
                (toRadix r (n // b)) ++ (getChr (n % b))
    in
        case (r >= 2 && r <= 16) of
            True ->
                getStr r

            False ->
                toString n


pie_points : Float -> Float -> Int -> Float -> Float -> List String
pie_points cx cy r t1 t2 =
    let
        sides =
            30

        sf =
            toFloat sides

        ts =
            List.map (\n -> t1 + (toFloat n) * (t2 - t1) / sf) (List.range 0 sides)

        pts =
            [ ( cx, cy ) ] ++ List.map (\t -> ( cx + (toFloat r) * cos (2 * pi * t), cy + (toFloat r) * sin (2 * pi * t) )) ts ++ [ ( cx, cy ) ]
    in
        List.map (\( x, y ) -> toString x ++ "," ++ toString y) pts


pie : Float -> Float -> Int -> Float -> Float -> String -> Svg msg
pie cx cy r t1 t2 color =
    let
        pt_string =
            pie_points cx cy r t1 t2
    in
        polygon [ points <| String.concat <| List.intersperse " " pt_string, stroke color, fill color ] []


pieline : Float -> Float -> Int -> Float -> Float -> Svg msg
pieline cx cy r t1 t2 =
    let
        pt_string =
            pie_points cx cy r t1 t2
    in
        polyline [ points <| String.concat <| List.intersperse " " pt_string, stroke "gray", fill "white" ] []


view : Model -> Html Msg
view model =
    Material.Scheme.top <|
        Tabs.render Mdl
            [ 1 ]
            model.mdl
            [ Tabs.ripple
            , Tabs.onSelectTab SelectTab
            , Tabs.activeTab model.tab
            ]
            [ Tabs.label
                [ Options.center ]
                [ Options.span [ css "width" "0px" ] []
                , Html.text "Simulation"
                ]
            , Tabs.label
                [ Options.center ]
                [ Options.span [ css "width" "4px" ] []
                , Html.text "Instructions"
                ]
            , Tabs.label
                [ Options.center ]
                [ Options.span [ css "width" "4px" ] []
                , Html.text "Source"
                ]
            , Tabs.label
                [ Options.center ]
                [ Options.span [ css "width" "4px" ] []
                , Html.text "Publication"
                ]
            ]
            [ Options.div
                [ css "margin" "24px auto"
                , css "align-items" "flex-start"
                , Options.center
                , css "overflow-y" "auto"
                ]
                [ case model.tab of
                    0 ->
                        ahusim model

                    1 ->
                        Html.article [] [ Markdown.toHtml [] ahuinstructions ]

                    2 ->
                        Html.article [] [ Markdown.toHtml [] ahusource ]

                    _ ->
                        Html.article [] [ Markdown.toHtml [] ahutext ]
                ]
            ]


ahusource : String
ahusource =
    """
# Source Code

This simulation is written in [Elm](http://elm-lang.org).

The source code is available at: [http://github.com/mwm126/ahu](http://github.com/mwm126/ahu)
"""
