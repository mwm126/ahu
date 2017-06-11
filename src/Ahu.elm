module Ahu exposing (..)

import Html exposing (Attribute, div, text, input)
import Html.Attributes as H exposing (min, max, value)
import Html.Events exposing (on, onInput)
import String

import AhuModel exposing (..)
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
get_oa_t model = let (Fahrenheit t) = model.outside_air.t in t

get_oa_rh: Model -> Float
get_oa_rh model = let (HPercent x) = model.outside_air.rh in x

get_sa_t: Model -> Float
get_sa_t model = let (Fahrenheit t) = model.supply_air.t in t

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
                        SetShf f -> { model | shf = stringToFloat f / 100 }
                        SetTons f -> { model | load = toTons f }
                        SetOawb rh -> { model | outside_air = { t = model.outside_air.t, rh = toHPercent rh }  }
                        SetCycle f -> { model | cycle = toTime f }
                        SetSat t -> { model | supply_air = { t = toTemp t, rh = model.supply_air.rh }  }
                        SetOat t -> { model | outside_air = { t = toTemp t, rh = model.outside_air.rh }  }
                        Tick newTime -> { model | time = time_mod newTime model
                                        , room_air = new_room_air model
                                        }
    in
        (new_model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model = Time.every (0.1 * second) Tick

ctrl_style = Html.Attributes.style
        [ ( "float", "left" )
        , ( "width", "50%" )
        , ( "display", "inline-block" )
        ]

show_style = Html.Attributes.style
        [ ( "float", "right" )
        , ( "width", "50%" )
        , ( "display", "inline-block" )
        ]

view : Model -> Html Msg
view model =
    let
        pro_x = 250.0
        pro_y = 30
        r2 x = toString <| roundn 2 x
        show name value = Html.text (name ++ " = " ++ r2 value)

        q_in = inTons <| q_inflow model
        shf_in = shf_inflow model
        (Fahrenheit room_t) = model.room_air.t
        (HPercent room_rh) = model.room_air.rh
    in
  Html.article [] [
       Html.section [ Html.Attributes.style [ ( "float", "left")
                                            , ("width", "50%")
                                            , ("position", "fixed")
                                            , ("padding", "1%")
                                            ] ] [
            div [ ctrl_style ]
                -- [ Html.text "Adjust system"
                [ Html.text "Setup the system by specifying weather..."
                , div [redStyle]
                    [ control SetOat 65.0 94.0 get_oa_t "Outside Air Temp" model
                    , control SetOawb 65.0 94.0 get_oa_rh "Outside Air Wet Bulb" model
                    ]
                , Html.p [] []
                , Html.text "Setup the system by specifying load..."
                , div [grayStyle]
                    [ control SetTons 40.0 100.0 get_load "Tons" model
                    , control SetShf 0.0 100.0 ((*) 100 << .shf) "SHF" model -- TODO: limit precision
                    , control SetCycle 0.0 30.0 .cycle "sim cycle (seconds)" model
                    ]
                , Html.p [] []
                , Html.text "Now adjust the system to maintain comfort."
                , div [blueStyle]
                    [ control SetOap 20.0 100.0 .oa_p "Outside Air %" model
                    , control SetSat 45.0 60.0 get_sa_t "Supply Air Temp" model
                    , control SetCfm 20000.0 40000.0 get_cfm "CFM" model
                    ]
                ]
           , div [ show_style ]
                [ Html.text "The results are:", Html.p [] []
                , Html.text (room_comment model), Html.p [] []
                , show "temperature inside the building" room_t, Html.p [] []
                , show "sensible heat factor of the building:" shf_in, Html.p [] []
                , show "Heat entering the building:" q_in, Html.p [] []
                , show "room_abs_hum" <| room_rh, Html.p [] []
                , show "time" <| model.time, Html.p [] []
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

comfort_temp_max = Fahrenheit 80
comfort_temp_min = Fahrenheit 70
comfort_rh_max = HPercent 60
comfort_rh_min = HPercent 30

room_comment: Model -> String
room_comment model =
    let
        (HPercent room_rh) = model.room_air.rh
        (Fahrenheit room_t) = model.room_air.t
        (HPercent rh_max) = comfort_rh_max
        (HPercent rh_min) = comfort_rh_min
        (Fahrenheit temp_max) = comfort_temp_max
        (Fahrenheit temp_min) = comfort_temp_min
    in
      if room_rh > rh_max then
          "Ugh!  It's too humid. "++(toString <| roundn 2 <| room_rh )
      else if room_rh < rh_min then
              "It's too dry. "++(toString room_rh )
          else if room_t > temp_max then
                    "Whew!  It's too hot in here! "++(toString <| roundn 2 <| room_t )
                else if room_t < temp_min then
                        "Brrr!  It's too cold in here! "++(toString <| roundn 2 <| room_t )
                    else
                        ""

-- for drawing the house
air_radius = 10
duct_height = 3*air_radius
duct_width = 8*air_radius
-- house offsets
house_x = Fahrenheit 10
house_y = 10

house model =
    let
        roof_width = 10
        roof_height = 10
        rr = air_radius
        hh = duct_height
        ww = duct_width
        rw = roof_width
        rh = roof_height
        (Fahrenheit xx) = house_x
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
        shf = model.shf
        q_in = inTons <| q_inflow model
        load = inTons model.load
        shf_in = shf_inflow model
-- sensible heat flow in
        x_1 = t
        y_1 = u
        x_2 = t - load*sin(shf*pi/2)
        y_2 = u + load*cos(shf*pi/2)
        x_3 = t - q_in*sin(shf_in*pi/2)
        y_3 = u + q_in*cos(shf_in*pi/2)
    in
        -- [
        [ pieline x_1 y_1 (round load) 0 0.5
            -- room center
        , Svg.text_ [ x (toString x_1), y (toString y_1), dx "5", dy "-5", fontSize "10"   ] [ Html.text "Cooling Vectors (BTU/Hr)" ]
        , circle [ cx (toString x_1), cy (toString y_1), r "4", fill "green" ] [ ]
        , Svg.text_ [ x (toString x_1), y (toString y_1), dx "5", dy "5", fontSize "10"   ] [ Html.text "room" ]
            -- load vector
        , line [ x1 (toString x_1), y1 (toString y_1), x2 (toString x_2), y2 (toString y_2), stroke "black" ] []
        , circle [ cx (toString x_2), cy (toString y_2), r "4", fill "gray" ] [ ]
        , Svg.text_ [ x (toString x_2), y (toString y_2), dx "5", dy "5", fontSize "10"   ] [ Html.text "load" ]
            -- supply vector
        , line [ x1 (toString x_1), y1 (toString y_1), x2 (toString x_3), y2 (toString y_3), stroke "black" ] []
        , circle [ cx (toString x_3), cy (toString y_3), r "4", fill "blue" ] [ ]
        , Svg.text_ [ x (toString x_3), y (toString y_3), dx "5", dy "5", fontSize "10"   ] [ Html.text "supply" ]
-- protractor
        ]

th_to_xy : (Temperature, RelativeHumidity) -> (Float, Float)
th_to_xy (temp,rel_h) =
    let
        bottom = 400
        (Fahrenheit t) = temp
        (HPercent h) = rel_h
    in
        ((t - 40)*(toFloat bottom-100)/(95-40) + 100, (0.029-h)*(toFloat bottom-100)/(0.029-0.0052) + 100)


air_state : (Temperature, RelativeHumidity) -> String -> String -> String -> String -> List (Svg msg)
air_state th clr label d_x d_y =
    let
        (t, h) = th
        (x_1, y_1) = th_to_xy (t,h)
    in
        [ circle [ cx (toString x_1), cy (toString y_1), r "4", fill clr ] [ ]
        , Svg.text_ [ x (toString x_1), y (toString y_1), dx d_x, dy d_y, fontSize "10" ] [ Html.text label ]
        ]

mixed_th: Model -> (Temperature, RelativeHumidity)
mixed_th model = avg_th (room_th model) (outside_th model) (model.oa_p/100)

room_th: Model -> (Temperature, RelativeHumidity)
room_th model = (model.room_air.t, absToRh <| abs_humidity model.room_air )

sa_th: Model -> (Temperature, RelativeHumidity)
sa_th model = (model.supply_air.t, absToRh <| abs_humidity model.supply_air )

outside_th: Model -> (Temperature, RelativeHumidity)
outside_th model = (model.outside_air.t, absToRh <| abs_humidity model.outside_air )

avg_th : (Temperature,RelativeHumidity) -> (Temperature,RelativeHumidity) -> Float -> (Temperature,RelativeHumidity)
avg_th xy1 xy2 t =
    let
        (Fahrenheit x1, HPercent y1) = xy1
        (Fahrenheit x2, HPercent y2) = xy2
    in
        (Fahrenheit (x1 + (x2-x1)*t), HPercent (y1 + (y2-y1)*t))

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

type alias Sprites = { recirc_th : (Temperature,RelativeHumidity)
                     , oa_th : (Temperature,RelativeHumidity)
                     , air_location : (Float,Float)
                     , recirc_air_location : (Float,Float)
                     , air_color : String
                     , recirc_air_color : String
                     }


sprite_states : Model -> Sprites
sprite_states model =
    let
        (Fahrenheit xx) = house_x
        yy = house_y
    in
     if model.time < 0.25 then
         -- passing through the building
         let
             pp = ((model.time)/0.25)
         in
             { recirc_th = room_th model
             , oa_th = room_th model
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
                  { recirc_th = avg_th (room_th model) (mixed_th model) pp
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
                  { recirc_th = avg_th (sa_th model) (room_th model) pp
                  , oa_th = avg_th (sa_th model) (room_th model) pp
                  , air_location = avg (xx+duct_width, yy+duct_height) (xx+duct_width, yy) pp
                  , recirc_air_location = avg (xx+duct_width, yy+duct_height) (xx+duct_width, yy) pp
                  , air_color = avg_color blue green pp
                  , recirc_air_color = avg_color blue green pp
                  }



psych_chart : Model -> List (Svg msg)
psych_chart model =
    let
        mkTempRH = (\(t,x) -> (Fahrenheit t,HPercent x))
        saturation_line : List (Temperature, RelativeHumidity)
        saturation_line = List.map mkTempRH [ (40.0, 0.0052)
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
        p_horiz (x,y) = make_line "black" (x,y) (1000,y)
        p_vert (x,y) = make_line "black" (x,y) (x,1000)
        r = "blue"
        some_temperature = 50 -- I don't know what this should be
        c1 = th_to_xy (comfort_temp_min, absToRh <| abs_humidity { rh=comfort_rh_min, t=comfort_temp_min})
        c2 = th_to_xy (comfort_temp_min, absToRh <| abs_humidity { rh=comfort_rh_max, t=comfort_temp_min})
        c3 = th_to_xy (comfort_temp_max, absToRh <| abs_humidity { rh=comfort_rh_max, t=comfort_temp_max})
        c4 = th_to_xy (comfort_temp_max, absToRh <| abs_humidity { rh=comfort_rh_min, t=comfort_temp_max})
        comfort_zone = [ make_line r c1 c2
                       , make_line r c2 c3
                       , make_line r c3 c4
                       , make_line r c4 c1
                       ]
    in
        List.concat [ List.map p_horiz (List.map th_to_xy saturation_line)
                    , List.map p_vert  (List.map th_to_xy saturation_line)
                    , List.concat [ air_state (outside_th model) "red" "outside" "5" "5"
                                  , air_state (mixed_th model) "yellow" "mixed" "5" "5"
                                  , air_state (room_th model) "green" "room" "5" "5"
                                  , air_state (sa_th model) "blue" "supply" "5" "5"
                                  , air_state (.oa_th (sprite_states model)) "black" "OA" "15" "15"
                                  , air_state (.recirc_th (sprite_states model)) "black" "recirc" "15" "-5"
                                  ]
                    , comfort_zone
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

inlineStyle = Html.Attributes.style
        [ ( "display", "inline" )
        ]

blueStyle = Html.Attributes.style
        [ ( "font-family", "-apple-system, system, sans-serif" )
        , ( "background-color", "#9999FF" )
        ]

redStyle = Html.Attributes.style
        [ ( "font-family", "-apple-system, system, sans-serif" )
        , ( "background-color", "#FF9999" )
        ]

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
