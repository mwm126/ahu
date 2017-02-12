module Ahu exposing (..)

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


time_mod : Time -> Model -> Float
time_mod time model =
    let
        t = Time.inSeconds time
        ct = model.cycle
    in
        (t - ct*(toFloat (floor(t/ct))))/ct


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        new_model = case msg of
                        IncrementOap f dd -> { model | oa_p = (Basics.max 20.0 (Basics.min 100.0 (f model + dd)))}
                        IncrementSat f dd -> { model | sa_t = (Basics.max 45.0 (Basics.min 60.0 (f model + dd)))}
                        IncrementCfm f dd -> { model | cfm = (Basics.max 20000.0 (Basics.min 40000.0 (f model + dd)))}
                        IncrementOat f dd -> { model | oa_t = (Basics.max 65.0 (Basics.min 94.0 (f model + dd)))}
                        IncrementOawb f dd -> { model | oa_wb = (Basics.max 65.0 (Basics.min 94.0 (f model + dd)))}
                        IncrementTons f dd -> { model | tons = (Basics.max 40.0 (Basics.min 100.0 (f model + dd)))}
                        IncrementShf f dd -> { model | shf = (Basics.max 0.0 (Basics.min 1.0 (f model + dd)))}
                        IncrementCycle f dd -> { model | cycle = f model + dd}
                        Tick newTime -> { model | time = time_mod newTime model
                                              , room_rh = new_room_rel_humidity model
                                              , room_t = new_room_t model}
    in
        (new_model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model = Time.every (0.1 * second) Tick

ctrl_style = Html.Attributes.style
        [ ( "float", "left" )
        , ( "width", "200px" )
        , ( "display", "inline-block" )
        ]

view : Model -> Html Msg
view model =
    let
        pro_x = 250.0
        pro_y = 30
        r2 x = toString <| roundn 2 x
        show name value = Html.text (name ++ " = " ++ r2 value)
    in
  div [] [
  div [ ctrl_style ]
      [ Html.text "Adjust system"
      , div [blueStyle]
          [ control IncrementOap .oa_p 5 "Outside Air %" model
          , control IncrementSat .sa_t 1 "Supply Air Temp" model
          , control IncrementCfm .cfm 1000 "CFM" model
          ]
      , Html.text "Adjust weather"
      , div [redStyle]
          [ control IncrementOat .oa_t 1 "Outside Air Temp" model
          , control IncrementOawb .oa_wb 3 "Outside Air Wet Bulb" model
          ]
      , Html.text "Adjust load"
      , div [grayStyle]
          [ control IncrementTons .tons 5 "Tons" model
          , control IncrementShf .shf 0.05 "SHF" model -- TODO: limit precision
          , control IncrementCycle .cycle 1 "sim cycle (seconds)" model
          , control IncrementShf .time 0.05 "Time" model
          ]
      , Html.text (room_comment model), Html.p [] []
      -- , show "room_t" model.room_t, Html.p [] []
      -- , show "shf_in" shf_in, Html.p [] []
      -- , show "q_in" q_in, Html.p [] []
      -- , show "room_abs_hum" room_abs_hum, Html.p [] []
      ]
      , div [ Html.Attributes.style [ ( "margin-left", "200px")] ]
      [ svg [viewBox "0 0 600 400", Svg.Attributes.width "600px" ]
            (List.concat [ (protractor pro_x pro_y model)
                         , house model
                         , psych_chart model])
      ]
      , div [ Html.Attributes.style [ ( "float", "right"), ("display", "inline-block")] ]
      [ Markdown.toHtml [] ahutext
      ]
      ]

-- TODO: DRAW THE BOX WITH THE COMFORT ZONE

-- for drawing the house
air_radius = 10
duct_height = 3*air_radius
duct_width = 8*air_radius
-- house offsets
xx = 10
yy = 10

house model =
    let
        roof_width = 10
        roof_height = 10
        rr = air_radius
        hh = duct_height
        ww = duct_width
        rw = roof_width
        rh = roof_height
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

debug model shf_in q_in room_abs_hum =
    let
        r2 x = toString <| roundn 2 x
        show name value = Html.text (name ++ r2 value)
    in
        div [grayStyle] [
             show "room_t" model.room_t
            ]
    -- Html.text ("room_t = " ++ r2 model.room_t ++
    --                " shf_in="++r2 shf_in ++
    --                " q_in="++r2 q_in ++
    --                " room_abs_hum =" ++ r2 room_abs_hum )

protractor : Float -> Float -> Model -> List ( Svg msg)
protractor t u model =
    let
        w = 20
        shf = model.shf
        room_abs_hum = abs_humidity model.room_rh model.room_t pressure
        supply_rel_hum = 0.95
        supply_abs_hum = abs_humidity supply_rel_hum model.sa_t pressure
        q_in = total_heat_flow model.cfm room_abs_hum model.room_t supply_abs_hum model.sa_t
-- sensible heat flow in
        shf_in = cool_supply model / q_in
        x_1 = t
        y_1 = u
        x_2 = t - model.tons*sin(shf*pi/2)
        y_2 = u + model.tons*cos(shf*pi/2)
        x_3 = t - q_in*sin(shf_in*pi/2)
        y_3 = u + q_in*cos(shf_in*pi/2)
    in
        -- [
        [ pieline x_1 y_1 (round model.tons) 0 0.5
            -- room center
        -- , Svg.text_ [ x (toString x_1), y (toString y_1), dx "5", dy "-5", fontSize "10"   ] [ Html.text "Cooling Vectors (BTU/Hr)" ]
        , Svg.text_ [ x (toString x_1), y (toString y_1), dx "5", dy "-5", fontSize "10"   ] [ debug model shf_in q_in room_abs_hum ]
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

th_to_xy : (Float,Float) -> (Float,Float)


th_to_xy (t,h) =
    let
        bottom = 400
    in
        ((t - 40)*(toFloat bottom-100)/(95-40) + 100, (0.029-h)*(toFloat bottom-100)/(0.029-0.0052) + 100)


air_state : (Float,Float) -> String -> String -> String -> String -> List (Svg msg)
air_state th clr label d_x d_y =
    let
        (t, h) = th
        (x_1, y_1) = th_to_xy (t,h)
    in
        [ circle [ cx (toString x_1), cy (toString y_1), r "4", fill clr ] [ ]
        , Svg.text_ [ x (toString x_1), y (toString y_1), dx d_x, dy d_y, fontSize "10" ] [ Html.text label ]
        ]

mixed_th model = avg (room_th model) (outside_th model) (model.oa_p/100)

room_th model = (model.room_t, abs_humidity  model.room_rh model.room_t pressure )

sa_th model = (model.sa_t, abs_humidity (supply_rel_humidity model.sa_t) model.sa_t pressure)

outside_th model = (model.oa_t, abs_humidity model.oa_wb model.oa_t pressure )

avg : (Float,Float) -> (Float,Float) -> Float -> (Float,Float)
avg xy1 xy2 t =
    let
        (x1, y1) = xy1
        (x2, y2) = xy2
    in
        (x1 + (x2-x1)*t, y1 + (y2-y1)*t)


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

type alias Sprites = { recirc_th : (Float,Float)
                     , oa_th : (Float,Float)
                     , air_location : (Float,Float)
                     , recirc_air_location : (Float,Float)
                     , air_color : String
                     , recirc_air_color : String
                     }


sprite_states : Model -> Sprites
sprite_states model =
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
                  { recirc_th = avg (room_th model) (mixed_th model) pp
                  , oa_th = avg (outside_th model) (mixed_th model) pp
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
                  { recirc_th = avg (mixed_th model) (sa_th model) pp
                  , oa_th = avg (mixed_th model) (sa_th model) pp
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
                  { recirc_th = avg (sa_th model) (room_th model) pp
                  , oa_th = avg (sa_th model) (room_th model) pp
                  , air_location = avg (xx+duct_width, yy+duct_height) (xx+duct_width, yy) pp
                  , recirc_air_location = avg (xx+duct_width, yy+duct_height) (xx+duct_width, yy) pp
                  , air_color = avg_color blue green pp
                  , recirc_air_color = avg_color blue green pp
                  }



psych_chart : Model -> List (Svg msg)
psych_chart model =
    let
        saturation_line : List (Float, Float)
        saturation_line = [ (40.0, 0.0052)
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
        p_horiz (x,y) = line [ x1 (toString x), y1 (toString y), x2 (toString 1000), y2 (toString y), stroke "black" ] []
        p_vert (x,y) = line [ x1 (toString x), y1 (toString y), x2 (toString x), y2 (toString 1000), stroke "black" ] []
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
                    ]

control inc get diff label model = div []
                             [ button [ onClick (inc get -diff) ] [ Html.text "-" ]
                             , div [inlineStyle] [ Html.text (label ++ " " ++ (toString (roundn 2 (get model)))) ]
                             , button [ onClick (inc get diff) ] [ Html.text "+" ]
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
