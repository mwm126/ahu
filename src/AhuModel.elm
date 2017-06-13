module AhuModel exposing (..)
import Time exposing (Time, second)

-- Properties and Units

-- type AbsoluteHumidity = AirDensity Float
type HumidityRatio = MolecularRatio Float
type Enthalpy = BTUsPerPound Float
type Pressure = PSI Float
type RelativeHumidity = HPercent Float
type RelativeHumidityRate = HPercentPerHour Float
type Temperature = Fahrenheit Float
type TemperatureRate = FahrenheitPerHour Float
type AirFlow = CubicFeetPerMinute Float
type alias Percent = Float
type Density = LbPerCubicFoot Float
type SpecificHeat = BtuPerLbF Float
type Power = BtuPerHour Float | Tons Float
type alias Air = { t : Temperature
                 , rh : RelativeHumidity
                 }

-- Constants and Conversions
atm: Pressure
atm = PSI 14.696 -- standard atmosphere

specific_heat_air: SpecificHeat
specific_heat_air = BtuPerLbF 0.241

air_density: Density
air_density = LbPerCubicFoot (1/13.2)

inTons: Power -> Float
inTons p = case p of
               (BtuPerHour b) -> (b/12000)
               (Tons t) -> t

inBtusPerHour: Power -> Float
inBtusPerHour p = case p of
               (BtuPerHour b) -> b
               (Tons t) -> t*12000

toTime: String -> Time
toTime s = case String.toFloat s of
               Ok f -> f
               Err m -> 0.0

toTons: String -> Power
toTons s = case String.toFloat s of
               Ok f -> Tons f
               Err m -> Tons 0.0

toAirflow: String -> AirFlow
toAirflow s = case String.toFloat s of
               Ok f -> CubicFeetPerMinute f
               Err m -> CubicFeetPerMinute 0.0

toHPercent: String -> RelativeHumidity
toHPercent s = case String.toFloat s of
               Ok f -> HPercent f
               Err m -> HPercent 0.0

-- Quantities for the model of the system

type alias Model = { supply_air : Air
                   , oa_p : Percent --outside air percentage
                   , cfm : AirFlow -- supply air flow rate
                   , outside_air : Air
                   , load : Power -- building cooling load
                   , shf : Float -- sensible heat factor qsense/qtotal, dimensionless from 0.0 to 1.0
                   , cycle : Time -- Duration of animation cycle (Seconds)
                   , time : Float -- Fraction of animation cycle complete (between 0.0 and 1.0)
                   , room_air : Air
                   }


-- Initial values for quantities for the system model

-- init_model: { supply_air: Temperature, oa_p: Percent }
init_model = { supply_air = { t = Fahrenheit 62
                            , rh = supply_rel_humidity (Fahrenheit 62)
                            }
             , oa_p = 30
             , cfm = CubicFeetPerMinute 30000
             , outside_air = { t = Fahrenheit 90
                             , rh = wetBulbToRelativeHumidity 84
                             }
             , load = Tons 65
             , shf = 0.90
             , cycle = 10
             , time = 0
             , room_air = { t = Fahrenheit 80
                          , rh = HPercent 50
                          }
             }


supply_rel_humidity: Temperature -> RelativeHumidity
supply_rel_humidity supply_t =
    let
        (Fahrenheit t) = supply_t
    in
        if t<60 then
            HPercent 95
        else
            HPercent (95 - (t - 60)*3)

-- FIXME: this is probably wrong
wetBulbToRelativeHumidity x = HPercent x

-- FIXME: this is probably wrong
absToRh: HumidityRatio -> RelativeHumidity
absToRh (MolecularRatio ah) = HPercent ah

-- Thermodynamic Equations

h2o_saturation_vapor_pressure: Temperature -> Pressure
h2o_saturation_vapor_pressure (Fahrenheit t_fah) =
    let
        t = t_fah + 459.67 -- convert to Rankine
        p_sat = e^(-10440.4/t - 11.29465
                  - 0.027022355*t
                      + 1.289036e-5*t*t
                          - 2.4780681e-9*t*t*t+6.5459673*logBase e t)
    in
        -- sat vapor pressure
        PSI p_sat


enthalpy: HumidityRatio -> Temperature -> Enthalpy
enthalpy absolute_humidity temperature =
    let
        (Fahrenheit t) = temperature
        (MolecularRatio w) = absolute_humidity
    in
        BTUsPerPound ((0.24+0.444*w)*t + 1061*w)


-- mass of water vapor per mass of air; dimensionless
humidity_ratio: Air -> HumidityRatio
humidity_ratio air =
    let
        t = air.t

        -- partial pressure of water
        (PSI p_h2o_sat) = h2o_saturation_vapor_pressure t
        (HPercent rh) = air.rh
        h2o_partial_pressure = p_h2o_sat * rh/100
        p_h2o = h2o_partial_pressure
        (PSI p_air) = atm
    in
        MolecularRatio (0.62198*(p_h2o/(p_air - p_h2o)))


-- total heat flow in
q_inflow: Model -> Power
q_inflow model =
    let
        room_t = model.room_air.t
        supply_t = model.supply_air.t

        room_ah = humidity_ratio model.room_air
        supply_ah = humidity_ratio model.supply_air

        (BTUsPerPound room_h) = enthalpy room_ah room_t
        (BTUsPerPound supply_h) = enthalpy supply_ah supply_t
        delta_h = room_h - supply_h

        (LbPerCubicFoot rho) = air_density
        (CubicFeetPerMinute cfm) = model.cfm
        sa_lb_per_hour = 60 * cfm * rho
        qinflow = sa_lb_per_hour*delta_h
    in
        BtuPerHour qinflow


-- sensible cool supply rate in tons per Hour
cool_supply: Model -> Power
cool_supply model =
    let
        (LbPerCubicFoot rho) = air_density
        (CubicFeetPerMinute cfm) = model.cfm
        lb_air_per_hour = 60 * cfm * rho
        (Fahrenheit room_t) = model.room_air.t
        (Fahrenheit supply_t) = model.supply_air.t
        delta_t = room_t - supply_t
        (BtuPerLbF cp) = specific_heat_air
        load = lb_air_per_hour*cp*delta_t
    in
        BtuPerHour load


-- sensible heat factor is the ratio of cooling supply to heat flow in
shf_inflow: Model -> Float
shf_inflow model =
    let
        supply = inBtusPerHour <| cool_supply model
        flow = inBtusPerHour <| q_inflow model
    in
        supply/flow


change_room_t: Model -> TemperatureRate
change_room_t model =
    let
        (BtuPerLbF cp) = specific_heat_air
        load = inBtusPerHour model.load
        supply = inBtusPerHour <| cool_supply model
    in
        FahrenheitPerHour (((load*model.shf - supply)/cp))

-- FIXME: This looks wrong
change_room_rel_humidity: Model -> RelativeHumidityRate
change_room_rel_humidity model =
    let
        q = inBtusPerHour model.load
        shf = model.shf
        (CubicFeetPerMinute sa_cfm) = model.cfm
        (MolecularRatio room_w) = humidity_ratio model.room_air
        (MolecularRatio supply_w) = humidity_ratio model.supply_air
        (Fahrenheit supply_t) = model.supply_air.t
        something = (1093 - 0.444*supply_t)/(13.2*12000)
    in
        HPercentPerHour ((q*(1-shf) - (room_w - supply_w)*sa_cfm*60*something)/100)


-- new room state after one hour
new_room_air: Model -> Air
new_room_air model =
    let
        (FahrenheitPerHour t_dot) = change_room_t model
        (HPercentPerHour delta_rh) = change_room_rel_humidity model
        (HPercent rel_h) = model.room_air.rh
        (Fahrenheit room_t) = model.room_air.t
        delta_t = t_dot -- change in temperature in one hour
        scale = 0.000000001
    in
        { t = Fahrenheit (room_t + delta_t*scale)
        , rh = HPercent (rel_h + delta_rh*scale)
        }
