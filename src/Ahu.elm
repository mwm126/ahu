module Ahu exposing (..)

import Html exposing (Attribute, div, text, input)
import Html.Attributes as H exposing (min, max, value)
import Html.Events exposing (on, onInput)
import String

import AhuModel exposing (..)
import AhuUnits exposing (..)
import AhuText exposing (ahutext)
import Char exposing (..)
import Color exposing (..)
import Html exposing (Html, div, button, text, label)
import Html.Attributes exposing (style, placeholder)
import Html.Events exposing (onClick)
import Markdown exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

main: Program Never Model Msg
main =
  Html.program { init = init
               , update = update
               , subscriptions = subscriptions
               , view = view
               }

init : (Model, Cmd Msg)
init = (
        init_model
       , Cmd.none)

time_mod : Time -> Model -> Float
time_mod time model =
    let
        t = Time.inSeconds time
        ct = model.cycle
    in
        (t - ct*(toFloat (floor(t/ct))))/ct

type Msg = SetCfm String
         | SetCycle String
         | SetOap String
         | SetOat String
         | SetOawb String
         | SetSat String
         | SetShf String
         | SetTons String
         | Tick Time

get_oa_t: Model -> Float
get_oa_t model = inFahrenheit model.outside_air.t

get_oa_rh: Model -> Float
get_oa_rh model = let (HPercent x) = model.outside_air.rh in x

get_sa_t: Model -> Float
get_sa_t model = inFahrenheit model.supply_air.t

get_cfm: Model -> Float
get_cfm model = let (CubicFeetPerMinute cfm) = model.cfm in cfm

get_load: Model -> Float
get_load model = inTons model.load

-- round x to n decimal places
roundn : Int -> Float -> Float
roundn n x =
    let
        f = toFloat ( 10^n )
        scaled = f*x
        fh = toFloat (round scaled)
    in
        -- toFloat (round (x*f))/f
        fh/f

toTemp: String -> Temperature
toTemp s = case String.toFloat s of
               Ok f -> Fahrenheit f
               Err m -> Fahrenheit 0.0

stringToFloat: String -> Float
stringToFloat s = case String.toFloat s of
               Ok f -> f
               Err m -> 0.0

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        new_model = case msg of
                        SetOap p -> { model | oa_p = stringToFloat p }
                        SetCfm f -> { model | cfm = toAirflow f }
                        SetShf f -> { model | load_shf = stringToFloat f / 100 }
                        SetTons f -> { model | load = toTons f }
                        SetOawb rh -> { model | outside_air = { t = model.outside_air.t, rh = toHPercent rh }  }
                        SetCycle f -> { model | cycle = toTime f }
                        SetSat t -> { model | supply_air = { t = toTemp t, rh = model.supply_air.rh }  }
                        SetOat t -> { model | outside_air = { t = toTemp t, rh = model.outside_air.rh }  }
                        Tick newTime -> { model | time = time_mod newTime model
                                        , building_air = new_building_air model
                                        }
    in
        (new_model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model = Time.every (0.1 * second) Tick

ctrl_style: Html.Attribute Msg
ctrl_style = Html.Attributes.style
        [ ( "float", "left" )
        , ( "width", "65%" )
        , ( "display", "inline-block" )
        ]

show_style: Html.Attribute Msg
show_style = Html.Attributes.style
        [ ( "float", "right" )
        , ( "width", "50%" )
        , ( "display", "inline-block" )
        ]

view : Model -> Html Msg
view model =
    let
        pro_x = 150
        pro_y = 150
        r2 x = toString <| roundn 20 x
        show name value = Html.text (name ++ " = " ++ r2 value)

        q_in = inTons <| q_total_in model
        q_sens = inTons <| q_sensible model
        shf_in = supply_shf model
        building_t = inFahrenheit model.building_air.t
        (HPercent building_rh) = model.building_air.rh
    in
  Html.article [] [
       Html.section [ Html.Attributes.style [ ( "float", "left")
                                            , ("width", "50%")
                                            , ("position", "fixed")
                                            , ("padding", "1%")
                                            ] ] [
            div [ ctrl_style ]
                -- [ Html.text "Adjust system"
                [ Html.text "Specify the weather outdoors."
                , div [redStyle]
                    [ control SetOat 80.0 94.0 get_oa_t "Outside Air Temp (°F)" model
                    , control SetOawb 65.0 85.0 get_oa_rh "Outside Air Wet Bulb (°F)" model
                    , control SetOap 20.0 100.0 .oa_p "Outside Air %" model
                    ]
                , Html.p [] []
                , Html.text "Setup the system by specifying load..."
                , div [grayStyle]
                    [ control SetTons 40.0 100.0 get_load "Cooling Load (Tons)" model
                    , control SetShf 0.0 100.0 ((*) 100 << .load_shf) "Load Sensible Heat Factor (%)" model -- TODO: limit precision
                    -- , control SetCycle 0.0 30.0 .cycle "sim cycle (seconds)" model
                    ]
                , Html.p [] []
                , Html.text "Now adjust the system to maintain comfort."
                , div [blueStyle]
                    [ control SetSat 45.0 60.0 get_sa_t "Supply Air Temp" model
                    , control SetCfm 20000.0 40000.0 get_cfm "CFM" model
                    ]
                , Html.text (building_comment model), Html.p [] []
                ]
           , div [ show_style ]
                [
                Html.text "", Html.p [] []
                 -- Html.text "The results are:", Html.p [] []
                -- , show "saturation vapor pressure" (inPSI <| saturation_vapor_pressure (Fahrenheit 53) atm), Html.p [] []
                -- , show "load shf" (model.load_shf), Html.p [] []
                -- , show "building relative humidity" (inPercent model.building_air.rh), Html.p [] []
                -- , show "building air partial pressure" (inPSI atm), Html.p [] []
                -- , show "building h2o partial pressure" (water_pp  model.building_air), Html.p [] []
                -- , show "building h2o saturation pressure" (inPSI <| h2o_saturation_vapor_pressure model.building_air.t), Html.p [] []
                -- , show "building air humidityratio aka specifichumidity" (inHumidityRatio <| humidity_ratio model.building_air), Html.p [] []
                -- , show "rahhahaha" (rahhahaha model timestep), Html.p [] []
                -- , show "delta_lat" (roundn 6 <| inTons <| latent_energy_change model timestep), Html.p [] []
                -- , show "water_in (lb/s)" (roundn 6 <| (inLb <| new_water_in model timestep)/(inSeconds timestep)), Html.p [] []
                -- , show "water_out (lb/s)" (roundn 6 <| (inLb <| new_water_out model timestep)/(inSeconds timestep)), Html.p [] []
                -- , show "water_out" (roundn 6 <| inLb <| new_water_out model timestep), Html.p [] []
                -- , show "h_h2o" (roundn 6 <| inBtusPerLb <| enthalpy_h2o model.supply_air), Html.p [] []
                -- , show "timestep: seconds " (roundn 6 <| inSeconds timestep), Html.p [] []
                -- , show "water_from_building (lb/s)" (roundn 6 <| (inLb <| new_water_from_building model timestep)/(inSeconds timestep)), Html.p [] []
                -- , show "h20 saturation vapor pressure" (inPSI <| h2o_saturation_vapor_pressure (Fahrenheit 80)), Html.p [] []
                -- , show "building temperature" (roundn 6 <| building_t), Html.p [] []
                -- , show "building relative humidity" (roundn 1 <| inPercent model.building_air.rh), Html.p [] []
                -- , show "supply sensible heat factor:" (roundn 5 <| shf_in), Html.p [] []
                -- , show "q_total_in :" q_in, Html.p [] []
                -- , show "q_sensible :" q_sens, Html.p [] []
                -- , show "building_abs_hum" <| building_rh, Html.p [] []
                -- , show "time" <| model.time, Html.p [] []
                ]
           , div [ Html.Attributes.style [ ( "margin-left", "100px")] ]
                [ svg [viewBox "0 0 600 400", Svg.Attributes.width "100%" ]
                      (List.concat [ (protractor pro_x pro_y model)
                                   , house model
                                   , psych_chart model])
                ]
           ]
      , Html.section [ Html.Attributes.style [ ( "float", "right"), ("width", "50%"), ("overflow-y", "scroll")] ]
           [ Html.article [] [ Markdown.toHtml [] ahutext ]
           ]
      ]

comfort_temp_max: Temperature
comfort_temp_max = Fahrenheit 81
comfort_temp_min: Temperature
comfort_temp_min = Fahrenheit 69

comfort_rh_max: RelativeHumidity
comfort_rh_max = HPercent 60
comfort_rh_min: RelativeHumidity
comfort_rh_min = HPercent 30

comfort_hr_max: HumidityRatio
comfort_hr_max = MolecularRatio 0.011
comfort_hr_min: HumidityRatio
comfort_hr_min = MolecularRatio 0.0055

building_comment: Model -> String
building_comment model =
    let
        (MolecularRatio building_hr) = humidity_ratio model.building_air
        building_t = inFahrenheit model.building_air.t
        -- (HPercent rh_max) = comfort_rh_max
        -- (HPercent rh_min) = comfort_rh_min
        (MolecularRatio hr_max) = comfort_hr_max
        (MolecularRatio hr_min) = comfort_hr_min
        temp_max = inFahrenheit comfort_temp_max
        temp_min = inFahrenheit comfort_temp_min
    in
      if building_hr > hr_max then
          "Ugh!  It's too humid. Humidity:"++(toString <| roundn 2 <| building_hr )
      else if building_hr < hr_min then
          "It's too dry. Humidity:"++(toString building_hr )
      else if building_t > temp_max then
          "Whew!  It's too hot in here! Temperature:"++(toString <| roundn 2 <| building_t )
      else if building_t < temp_min then
          "Brrr!  It's too cold in here! Temperature:"++(toString <| roundn 2 <| building_t )
      else
          ""

-- for drawing the house
air_radius: Float
air_radius = 10
duct_height: Float
duct_height = 3*air_radius
duct_width: Float
duct_width = 8*air_radius
-- house offsets
house_x: Temperature
house_x = Fahrenheit 10
house_y: Float
house_y = 10

house: Model -> List (Svg Msg)
house model =
    let
        roof_width = 10
        roof_height = 10
        rr = air_radius
        hh = duct_height
        ww = duct_width
        rw = roof_width
        rh = roof_height
        xx = inFahrenheit house_x
        yy = house_y
        (ax, ay) = .air_location (sprite_states model)
        (rx, ry) = .recirc_air_location (sprite_states model)
                   -- points in house icon
        xs = [ 0, rw, 2*rw/3, 2*rw/3, -2*rw/3, -2*rw/3, -rw ]
        ys = [ 0, rh, rh,       2*rh,    2*rh, rh,       rh ]
        house_points = List.map2 (\ x y -> toString (xx+ww+rr+x) ++ "," ++ toString (yy+hh/3+y)) xs ys
        coil_x = xx+ww*0.6
        coil_y = yy+hh
        coil x = line [ x1 (toString (coil_x+x)), y1 (toString (coil_y-rr)), x2 (toString (coil_x+x)), y2 (toString coil_y), stroke "blue" ] []
    in
        [ line [ x1 (toString xx), y1 (toString yy), x2 (toString (xx+ww)), y2 (toString yy), stroke "black" ] []
        , rect [ Svg.Attributes.x (toString xx), Svg.Attributes.y (toString (yy + rr)), width (toString (2*rr)), height (toString (rr)), fill "gray" ] [ ]
        , rect [ Svg.Attributes.x (toString (xx+3*rr)), Svg.Attributes.y (toString (yy + rr)), width (toString (ww-(xx+3*rr))), height (toString (rr)), fill "gray" ] [ ]
        , line [ x1 (toString xx), y1 (toString (yy+hh)), x2 (toString (xx+ww)), y2 (toString (yy+hh)), stroke "black" ] []
        , polygon [ points (String.concat (List.intersperse " " house_points)), stroke "black" ] []
        , coil 10
        , coil 12
        , coil 14
        , coil 16
        , coil 18
        , Svg.text_ [ x (toString coil_x), y (toString coil_y), dx "15", dy "15", fontSize "10", stroke "blue" ] [ Html.text "coil" ]
        -- , circle [ cx (toString ax), cy (toString ay), r "10", fill (sprite_states model).air_color ] [ ]
        , pie ax ay 10 (1-model.oa_p/100-0.25) 0.75 (sprite_states model).recirc_air_color
        , pie rx ry 10 -0.25 (1.0-model.oa_p/100-0.25) (sprite_states model).air_color
        ]


protractor : Float -> Float -> Model -> List ( Svg msg)
protractor t u model =
    let
        w = 20
        shf_load = model.load_shf
        q_tot_in = inTons <| q_total_in model
        load = inTons model.load
        shf_supply = supply_shf model
        -- θ_supply = atan shf_supply
        -- θ_load = atan shf_load
-- sensible heat flow in
        x_1 = t
        y_1 = u

        -- load
        x_2 = t - load*sin(shf_load*pi/2)
        y_2 = u + load*cos(shf_load*pi/2)

        -- supply
        x_3 = t - q_tot_in*sin(shf_supply*pi/2)
        y_3 = u + q_tot_in*cos(shf_supply*pi/2)
        -- x_2 = t - load*sin(θ_load)
        -- y_2 = u + load*cos(θ_load)
        -- x_3 = t - q_in*sin(θ_supply)
        -- y_3 = u + q_in*cos(θ_supply)
    in
        -- [
        [ pieline x_1 y_1 (round load) 0 0.5
            -- building center
        , Svg.text_ [ x (toString x_1), y (toString y_1), dx "5", dy "-5", fontSize "10"   ] [ Html.text "Cooling Vectors (BTU/Hr)" ]
        , circle [ cx (toString x_1), cy (toString y_1), r "4", fill "green" ] [ ]
        , Svg.text_ [ x (toString x_1), y (toString y_1), dx "5", dy "5", fontSize "10"   ] [ Html.text "building" ]
            -- load vector
        , line [ x1 (toString x_1), y1 (toString y_1), x2 (toString x_2), y2 (toString y_2), stroke "black" ] []
        , circle [ cx (toString x_2), cy (toString y_2), r "4", fill "gray" ] [ ]
        , Svg.text_ [ x (toString x_2), y (toString y_2), dx "-30", dy "5", fontSize "10"   ] [ Html.text "load" ]
            -- supply vector
        , line [ x1 (toString x_1), y1 (toString y_1), x2 (toString x_3), y2 (toString y_3), stroke "black" ] []
        , circle [ cx (toString x_3), cy (toString y_3), r "4", fill "blue" ] [ ]
        , Svg.text_ [ x (toString x_3), y (toString y_3), dx "5", dy "5", fontSize "10"   ] [ Html.text "supply" ]
-- protractor
        ]

th_to_xy : (Temperature, HumidityRatio) -> (Float, Float)
th_to_xy (temp,rel_h) =
    let
        bottom = 350
        t = inFahrenheit temp
        (MolecularRatio h) = rel_h
        x = (t - 40)*(toFloat bottom-100)/(95-40) + 100
        y = (0.029-h)*(toFloat bottom-100)/(0.029-0.0052) + 100
    in
        (x, y)


air_state : (Temperature, HumidityRatio) -> String -> String -> String -> String -> List (Svg msg)
air_state th clr label d_x d_y =
    let
        (t, h) = th
        (x_1, y_1) = th_to_xy (t,h)
    in
        [ circle [ cx (toString x_1), cy (toString y_1), r "4", fill clr ] [ ]
        , Svg.text_ [ x (toString x_1), y (toString y_1), dx d_x, dy d_y, fontSize "10" ] [ Html.text label ]
        ]

mixed_th: Model -> (Temperature, HumidityRatio)
mixed_th model = avg_th (building_th model) (outside_th model) (model.oa_p/100)

building_th: Model -> (Temperature, HumidityRatio)
building_th model = (model.building_air.t, humidity_ratio model.building_air )

sa_th: Model -> (Temperature, HumidityRatio)
sa_th model = (model.supply_air.t, humidity_ratio model.supply_air )

outside_th: Model -> (Temperature, HumidityRatio)
outside_th model = (model.outside_air.t, humidity_ratio model.outside_air )

avg_th : (Temperature,HumidityRatio) -> (Temperature,HumidityRatio) -> Float -> (Temperature,HumidityRatio)
avg_th xy1 xy2 t =
    let
        (t1, MolecularRatio y1) = xy1
        x1 = inFahrenheit t1
        (t2, MolecularRatio y2) = xy2
        x2 = inFahrenheit t2
    in
        (Fahrenheit (x1 + (x2-x1)*t), MolecularRatio (y1 + (y2-y1)*t))

avg : (Float,Float) -> (Float,Float) -> Float -> (Float,Float)
avg xy1 xy2 t =
    let
        (x1, y1) = xy1
        (x2, y2) = xy2
    in
        ((x1 + (x2-x1)*t), y1 + (y2-y1)*t)


avg_int : Float -> Float -> Float -> String
avg_int r1 r2 t = toRadix 16 (round (r1 + (r2-r1)*t))


avg_color : Color -> Color -> Float -> String
avg_color c1 c2 t =
    let
        r1 = toFloat (Color.toRgb c1).red
        g1 = toFloat (Color.toRgb c1).green
        b1 = toFloat (Color.toRgb c1).blue
        r2 = toFloat (Color.toRgb c2).red
        g2 = toFloat (Color.toRgb c2).green
        b2 = toFloat (Color.toRgb c2).blue
    in
        "#" ++ avg_int r1 r2 t ++ avg_int g1 g2 t ++ avg_int b1 b2 t

type alias Sprites = { recirc_th : (Temperature,HumidityRatio)
                     , oa_th : (Temperature,HumidityRatio)
                     , air_location : (Float,Float)
                     , recirc_air_location : (Float,Float)
                     , air_color : String
                     , recirc_air_color : String
                     }


sprite_states : Model -> Sprites
sprite_states model =
    let
        xx = inFahrenheit house_x
        yy = house_y
    in
     if model.time < 0.25 then
         -- passing through the building
         let
             pp = ((model.time)/0.25)
         in
             { recirc_th = building_th model
             , oa_th = building_th model
             , air_location = avg (xx+duct_width, yy) (xx, yy) pp
             , recirc_air_location = avg (xx+duct_width, yy) (xx+duct_width*0.3, yy) pp
             , air_color = avg_color green green pp
             , recirc_air_color = avg_color green green pp
             }
     else if model.time < 0.5 then
         -- exiting the building
              let
                  pp = ((model.time-0.25)/0.25)
              in
                  { recirc_th = avg_th (building_th model) (mixed_th model) pp
                  , oa_th = avg_th (outside_th model) (mixed_th model) pp
                  , air_location = avg (xx, yy) (xx, yy+duct_height) pp
                  , recirc_air_location = avg (xx+duct_width*0.3, yy) (xx+duct_width*0.3, yy+duct_height) pp
                  , air_color = avg_color green red pp
                  , recirc_air_color = avg_color green green pp
                  }

     else if model.time < 0.75 then
        -- separate air and recirc
              let
                  pp = ((model.time-0.5)/0.25)
              in
                  { recirc_th = avg_th (mixed_th model) (sa_th model) pp
                  , oa_th = avg_th (mixed_th model) (sa_th model) pp
                  , air_location = avg (xx, yy+duct_height) (xx+duct_width, yy+duct_height) pp
                  , recirc_air_location = avg (xx+duct_width*0.3, yy+duct_height) (xx+duct_width, yy+duct_height) pp
                  , air_color = avg_color yellow blue pp
                  , recirc_air_color = avg_color green green pp
              }
     else
         -- entering the building
              let
                  pp = ((model.time-0.75)/0.25)
              in
                  { recirc_th = avg_th (sa_th model) (building_th model) pp
                  , oa_th = avg_th (sa_th model) (building_th model) pp
                  , air_location = avg (xx+duct_width, yy+duct_height) (xx+duct_width, yy) pp
                  , recirc_air_location = avg (xx+duct_width, yy+duct_height) (xx+duct_width, yy) pp
                  , air_color = avg_color blue green pp
                  , recirc_air_color = avg_color blue green pp
                  }



psych_chart : Model -> List (Svg msg)
psych_chart model =
    let
        mkTempHr = (\(t,x) -> (Fahrenheit t, MolecularRatio x))
        saturation_line : List (Temperature, HumidityRatio)
        saturation_line = List.map mkTempHr [ (40.0, 0.0052)
                                            , (45.0, 0.0063)
                                            , (50.0, 0.0076)
                                            , (55.0, 0.0092)
                                            , (60.0, 0.0112)
                                            , (65.0, 0.0132)
                                            , (70.0, 0.0158)
                                            , (75.0, 0.0188)
                                            , (80.0, 0.0223)
                                            , (85.0, 0.0264)
                                            , (90.0, 0.029)
                                            , (95.0, 0.029)
                                            ]
        -- make_line takes pairs of (temperature, humidity) and transforms to pixels
        make_line c (x,y) (xx,yy) = line [ x1 (toString x), y1 (toString y), x2 (toString xx), y2 (toString yy), stroke c ] []
        p_horiz (x,y) = make_line "lightgray" (x,y) (450,y)
        p_vert (x,y) = make_line "lightgray" (x,y) (x,370)
        r = "blue"
        some_temperature = 50 -- I don't know what this should be
        c1 = th_to_xy (comfort_temp_min, humidity_ratio { rh=comfort_rh_min, t=comfort_temp_min})
        c2 = th_to_xy (comfort_temp_min, humidity_ratio { rh=comfort_rh_max, t=comfort_temp_min})
        c3 = th_to_xy (comfort_temp_max, humidity_ratio { rh=comfort_rh_max, t=comfort_temp_max})
        c4 = th_to_xy (comfort_temp_max, humidity_ratio { rh=comfort_rh_min, t=comfort_temp_max})
        x_axis_label = Svg.text_ [x "350", y "370", fontSize "10"] [Html.text "Temperature"]
        y_axis_label = Svg.text_ [x "470", y "100", fontSize "10", writingMode "tb"] [Html.text "Specific Humidity"]
        comfort_label = Svg.text_ [x "270", y "350", fontSize "10", stroke "blue" ] [Html.text "Comfort Zone"]
        -- y_axis_label = Svg.text_ [] [Html.text "Specific Humidity"]
        comfort_zone = [ make_line r c1 c2
                       , make_line r c2 c3
                       , make_line r c3 c4
                       , make_line r c4 c1
                       ]
    in
        List.concat [ List.map p_horiz (List.map th_to_xy saturation_line)
                    , List.map p_vert  (List.map th_to_xy saturation_line)
                    , List.concat [ air_state (outside_th model) "red" "Outside Air (OA)" "5" "5"
                                  , air_state (mixed_th model) "yellow" "Mixed Air (MA)" "5" "5"
                                  , air_state (building_th model) "green" "Return Air (RA)" "5" "5"
                                  , air_state (sa_th model) "blue" "Supply Air (SA)" "5" "5"
                                  , air_state (.oa_th (sprite_states model)) "black" "OA" "15" "15"
                                  , air_state (.recirc_th (sprite_states model)) "black" "RA" "15" "-5"
                                  ]
                    , comfort_zone
                    , [ x_axis_label
                      , y_axis_label
                      , comfort_label
                      ]
                    ]

control: (String -> Msg) -> Float -> Float -> (Model -> Float) -> String -> Model -> (Html Msg)
control set minval maxval get label model =
    let
        val = toString << roundn 2 << get <| model
    in
    div []
        [ input
              [ type_ "range"
              , H.min <| toString minval
              , H.max <| toString maxval
              , value val
              , onInput set
              ] []
        , Html.text (label ++ " " ++ val)
        ]

inlineStyle: Html.Attribute Msg
inlineStyle = Html.Attributes.style
        [ ( "display", "inline" )
        ]

blueStyle: Html.Attribute Msg
blueStyle = Html.Attributes.style
        [ ( "font-family", "-apple-system, system, sans-serif" )
        , ( "background-color", "#9999FF" )
        ]

redStyle: Html.Attribute Msg
redStyle = Html.Attributes.style
        [ ( "font-family", "-apple-system, system, sans-serif" )
        , ( "background-color", "#FF9999" )
        ]

grayStyle: Html.Attribute Msg
grayStyle = Html.Attributes.style
        [ ( "font-family", "-apple-system, system, sans-serif" )
        , ( "background-color", "#999999" )
        ]
toRadix : Int -> Int -> String
toRadix r n =
  let
    getChr c = if c < 10 then toString c else String.fromChar <| Char.fromCode (87+c)

    getStr b = if n < b then getChr n else (toRadix r (n//b)) ++  (getChr (n%b))

  in
    case (r>=2 && r<=16) of
      True -> getStr r
      False -> toString n

pie_points : Float -> Float -> Int -> Float -> Float -> List (String)
pie_points cx cy r t1 t2 =
    let
        sides = 30
        sf = toFloat sides
        ts = List.map (\n -> t1 + (toFloat n)*(t2-t1)/sf) (List.range 0 sides)
        pts = [(cx,cy)] ++ List.map (\t -> (cx + (toFloat r)*cos(2*pi*t), cy + (toFloat r)*sin(2*pi*t))) ts ++ [(cx,cy)]
    in
        List.map (\ (x,y) -> toString x ++ "," ++ toString y) pts

pie : Float -> Float -> Int -> Float -> Float -> String -> Svg msg
pie cx cy r t1 t2 color =
    let
        pt_string = pie_points cx cy r t1 t2
    in
        polygon [ points (String.concat (List.intersperse " " pt_string)), stroke color, fill color ] []

pieline : Float -> Float -> Int -> Float -> Float -> Svg msg
pieline cx cy r t1 t2 =
    let
        pt_string = pie_points cx cy r t1 t2
    in
        polyline [ points (String.concat (List.intersperse " " pt_string)), stroke "gray", fill "white" ] []
