module AhuModel exposing (..)
import Time exposing (Time, second)

-- Properties and Units

-- type AbsoluteHumidity = AirDensity Float
type HumidityRatio = MolecularRatio Float
type Enthalpy = BTUsPerPound Float
type Pressure = PSI Float | HectoPascals Float
type RelativeHumidity = HPercent Float
type RelativeHumidityRate = HPercentPerHour Float
type Temperature = Fahrenheit Float | Celsius Float
type TemperatureRate = FahrenheitPerHour Float
type AirFlow = CubicFeetPerMinute Float
type alias Percent = Float
type Density = LbPerCubicFoot Float
type DensityRate = LbPerCubicFootPerHour Float
type MassPerTime = LbPerHour Float
type Mass = Lb Float
type SpecificHeat = BtuPerLbF Float
type SpecificHeatExtensive = BtuPerF Float
type Power = BtuPerHour Float | Tons Float
type Volume = CubicFeet Float
type alias Air = { t : Temperature
                 , rh : RelativeHumidity
                 }

-- Constants and Conversions
atm: Pressure
atm = PSI 14.696 -- standard atmosphere

building_volume: Volume
building_volume = CubicFeet 100000

specific_heat_air: SpecificHeat
specific_heat_air = BtuPerLbF 0.241

building_heat_capacitance: SpecificHeatExtensive
building_heat_capacitance = BtuPerF 100000

air_density: Density
air_density = LbPerCubicFoot (1/13.2)

inPercent: RelativeHumidity -> Float
inPercent (HPercent p) = p

inPSI: Pressure -> Float
inPSI p = case p of
              (HectoPascals hp) -> hp*0.0145038
              (PSI psi) -> psi

inHectoPascals: Pressure -> Float
inHectoPascals p = case p of
               (HectoPascals hp) -> hp
               (PSI psi) -> psi*68.9476
inCelsius: Temperature -> Float
inCelsius t = case t of
               (Fahrenheit d) -> d
               (Celsius d) -> (d-32)*5/9

inFahrenheit: Temperature -> Float
inFahrenheit t = case t of
               (Fahrenheit d) -> d
               (Celsius d) -> d*9/5 + 32

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

init_model: Model
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
        t = inFahrenheit supply_t
    in
        if t<60 then
            HPercent 95
        else
            HPercent (95 - (t - 60)*3)

-- FIXME: this is probably wrong
wetBulbToRelativeHumidity: Float -> RelativeHumidity
wetBulbToRelativeHumidity x = HPercent x

-- Thermodynamic Equations

h2o_saturation_vapor_pressure: Temperature -> Pressure
h2o_saturation_vapor_pressure temperature =
    let
        t_fah = inFahrenheit temperature
        t = t_fah + 459.67 -- convert to Rankine
        p_sat = e^(-10440.4/t - 11.29465
                  - 0.027022355*t
                      + 1.289036e-5*t*t
                          - 2.4780681e-9*t*t*t+6.5459673*logBase e t)
    in
        -- sat vapor pressure
        PSI p_sat

-- total enthalpy of the air
enthalpy: HumidityRatio -> Temperature -> Enthalpy
enthalpy absolute_humidity temperature =
    let
        t = inFahrenheit temperature
        (MolecularRatio w) = absolute_humidity
    in
        BTUsPerPound ((0.24+0.444*w)*t + 1061*w)


-- mass of water vapor per mass of air; dimensionless
humidity_ratio: Air -> HumidityRatio
humidity_ratio air =
    let
        t = air.t

        -- partial pressure of water
        p_h2o_sat = h2o_saturation_vapor_pressure t
        p_h2o_sat_psi = inPSI p_h2o_sat
        (HPercent rh) = air.rh
        h2o_partial_pressure = p_h2o_sat_psi * rh/100
        p_h2o = h2o_partial_pressure
        p_air = inPSI atm
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
        room_t = inFahrenheit model.room_air.t
        supply_t = inFahrenheit model.supply_air.t
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
        (BtuPerF cp) = building_heat_capacitance
        load = inBtusPerHour model.load
        supply = inBtusPerHour <| cool_supply model
        shf = model.shf
        (LbPerCubicFoot rho) = air_density
    in
        FahrenheitPerHour (((load*shf - supply)/cp))

-- -- h2o in from supply air
-- delta_water_in: Model -> Time -> Mass
-- delta_water_in model dt =
--     let
--         (CubicFeetPerMinute supply_air_cfm) = model.cfm
--         (LbPerCubicFoot rho) = air_density
--         (MolecularRatio w) = humidity_ratio model.supply_air
--         supply_abs_humidity = w * rho * supply_air_cfm
--     in
--         Lb (60*dt*supply_air_cfm*supply_abs_humidity)

-- -- h2o flowing out
-- delta_water_out: Model -> Time -> Mass
-- delta_water_out model dt =
--     let
--         (CubicFeetPerMinute outflow_air_cfm) = model.cfm
--         (LbPerCubicFoot rho) = air_density
--         (MolecularRatio w) = humidity_ratio model.room_air
--         room_abs_humidity = w * rho * outflow_air_cfm
--     in
--         Lb (60*dt*outflow_air_cfm*room_abs_humidity)

-- -- enthalpy of water vapor
-- enthalpy_h2o: Air -> Enthalpy
-- enthalpy_h2o air =
--     let
--         t = inFahrenheit air.t
--     in
--         BTUsPerPound (1061 + 0.444*t)

-- -- h2o generate by load
-- delta_water_from_building: Model -> Time -> Mass
-- delta_water_from_building model dt =
--     let
--         load = inBtusPerHour model.load
--         shf = model.shf
--         (BTUsPerPound h) = enthalpy_h2o model.room_air
--     in
--         Lb (dt*load*(1-shf)/h)

-- -- rate of change of water in building
-- delta_room_h2o: Model -> Time -> Mass
-- delta_room_h2o model dt =
--     let
--         (Lb h2o_supply) = delta_water_in model dt
--         (Lb h2o_building) = delta_water_from_building model dt
--         (Lb h2o_outflow) = delta_water_out model dt
--     in
--         Lb (h2o_supply + h2o_building - h2o_outflow)


-- -- rate of change of absolute humidity in building
-- delta_room_abs_humidity: Model -> Time -> Density
-- delta_room_abs_humidity model dt =
--     let
--         (Lb h2o_dot) = delta_room_h2o model dt
--         (CubicFeet v) = building_volume
--     in
--         LbPerCubicFoot (h2o_dot/v)

-- rate of change of relative humidity in building
delta_room_rel_humidity: Model -> Time -> RelativeHumidity
delta_room_rel_humidity model dt =
    let
        (MolecularRatio w) = humidity_ratio model.room_air
        p = inPSI atm
        p_w = p*w/(0.62198 + w)
        p_h2o_sat = inPSI <| h2o_saturation_vapor_pressure model.room_air.t
    in
        HPercent (p_w / p_h2o_sat)


-- FIXME: This looks wrong
change_room_rel_humidity: Model -> RelativeHumidityRate
change_room_rel_humidity model =
    let
        shf = model.shf
        load = inBtusPerHour model.load
        supply = inBtusPerHour <| cool_supply model
        (CubicFeetPerMinute sa_cfm) = model.cfm
        (MolecularRatio room_w) = humidity_ratio model.room_air
        room_t = inFahrenheit model.room_air.t
        -- new_room_t = room_t + change_room_t model
        (MolecularRatio supply_w) = humidity_ratio model.supply_air
        (LbPerCubicFoot rho) = air_density
        supply_t = inFahrenheit model.supply_air.t
        supply_h_g = (1061 + 0.444*supply_t) -- BTUsPerPound of water
        something = (1093 - 0.444*supply_t)/(13.2*12000)
        q_total = inBtusPerHour model.load
        q_sensible = shf * supply
        delta_q_latent = q_total - q_sensible
        delta_humidity_ratio = delta_q_latent / (sa_cfm * 60 * rho * supply_h_g)
        w =delta_humidity_ratio
        p = atm
        -- p_w = p*w/(0.622 + w)
        -- p_ws = h2o_saturation_vapor_pressure (room_t)
        -- absolute_humidity = p_w/ p_ws
    in
        HPercentPerHour ((delta_q_latent - (room_w - supply_w)*sa_cfm*60*something)/100)


-- new room state after one hour
new_room_air: Model -> Air
new_room_air model =
    let
        (FahrenheitPerHour t_dot) = change_room_t model
        (HPercentPerHour delta_rh) = change_room_rel_humidity model
        (HPercent rel_h) = model.room_air.rh
        room_t = inFahrenheit model.room_air.t
        delta_t = t_dot -- change in temperature in one hour
        scale = 0.0000001
    in
        { t = Fahrenheit (room_t + delta_t*scale)
        , rh = HPercent (rel_h + delta_rh*scale)
        }
