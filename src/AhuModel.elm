module AhuModel exposing (..)
import Time exposing (Time, second)

import AhuUnits exposing (..)

-- Properties and Units
type alias Air = { t : Temperature
                 , rh : RelativeHumidity
                 }

-- Constants and Conversions

timestep: Duration
timestep = Seconds 1

atm: Pressure
atm = PSI 14.696 -- standard atmosphere

building_volume: Volume
building_volume = CubicFeet 250000

specific_heat_air: SpecificHeat
specific_heat_air = BtuPerLbF 0.241

building_heat_capacitance: SpecificHeatExtensive
building_heat_capacitance = BtuPerF 1000000

air_density: Density
air_density = LbPerCubicFoot (1/13.2)

specific_gas_constant_h2o: SpecificGasConstant
specific_gas_constant_h2o = JoulePerKgPerK 461.5

-- Quantities for the model of the system

type alias Model = { supply_air : Air
                   , oa_p : Percent --outside air percentage
                   , cfm : AirFlow -- supply air flow rate
                   , outside_air : Air
                   , load : Power -- building cooling load
                   , load_shf : Float -- sensible heat factor qsense/qtotal, dimensionless from 0.0 to 1.0
                   , cycle : Time -- Duration of animation cycle (Seconds)
                   , time : Float -- Fraction of animation cycle complete (between 0.0 and 1.0)
                   , building_air : Air
                   }


-- Initial values for quantities for the system model

init_model: Model
init_model = { supply_air = { t = Fahrenheit 62
                            -- , rh = supply_rel_humidity (Fahrenheit 50)
                            , rh = HPercent 95
                            }
             , oa_p = 30
             , cfm = CubicFeetPerMinute 30000
             , outside_air = { t = Fahrenheit 90
                             , rh = wetBulbToRelativeHumidity 84
                             }
             , load = Tons 65
             , load_shf = 0.90
             , cycle = 10
             , time = 0
             , building_air = { t = Fahrenheit 80
                          , rh = HPercent 50
                          }
             }


-- supply_rel_humidity: Temperature -> RelativeHumidity
-- supply_rel_humidity supply_t =
--     let
--         t = inFahrenheit supply_t
--     in
--         if t<60 then
--             HPercent 95
--         else
--             HPercent (95 - (t - 60)*3)

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

relative_humidity: Temperature -> Density -> RelativeHumidity
relative_humidity temperature absolute_humidity =
    let
        e_w = inPascals <| h2o_saturation_vapor_pressure temperature
        ah = inKgPerCubicMeter absolute_humidity
        (JoulePerKgPerK r_v) = specific_gas_constant_h2o
        t = inKelvin temperature
        e = ah * r_v*t
    in
        HPercent (100 * e/e_w)


absolute_humidity: Air -> Density
absolute_humidity air =
    let
        e_w = inPascals <| h2o_saturation_vapor_pressure air.t
        (HPercent rh) = air.rh
        e = e_w * rh/100
        t = inKelvin air.t
        (JoulePerKgPerK r_v) = specific_gas_constant_h2o
    in
        KgPerCubicMeter (e / (r_v*t))

-- total enthalpy of the air
enthalpy: HumidityRatio -> Temperature -> Enthalpy
enthalpy absolute_humidity temperature =
    let
        t = inFahrenheit temperature
        (MolecularRatio w) = absolute_humidity
    in
        BTUsPerPound ((0.24+0.444*w)*t + 1061*w)

water_pp: Air -> Float
water_pp air =
    let
        t = air.t
        p_h2o_sat = h2o_saturation_vapor_pressure t
        p_h2o_sat_psi = inPSI p_h2o_sat
        (HPercent rh) = air.rh
        h2o_partial_pressure = p_h2o_sat_psi * rh/100
        p_h2o = h2o_partial_pressure
    in
        p_h2o


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
        xxx = (0.62198*(p_h2o/(p_air - p_h2o)))
    in
        -- if p_air > p_h2o
        --     then
                MolecularRatio (0.62198*(p_h2o/(p_air - p_h2o)))
            -- else
            --     MolecularRatio -111111


-- total heat flow in
q_total_in: Model -> Power
q_total_in model =
    let
        building_t = model.building_air.t
        supply_t = model.supply_air.t

        building_ah = humidity_ratio model.building_air
        supply_ah = humidity_ratio model.supply_air

        (BTUsPerPound building_h) = enthalpy building_ah building_t
        (BTUsPerPound supply_h) = enthalpy supply_ah supply_t
        delta_h = building_h - supply_h

        rho = inLbPerCubicFoot air_density
        (CubicFeetPerMinute cfm) = model.cfm
        sa_lb_per_hour = 60 * cfm * rho
        qinflow = sa_lb_per_hour*delta_h
    in
        BtuPerHour qinflow


-- sensible cool supply rate in tons per Hour
q_sensible: Model -> Power
q_sensible model =
    let
        rho = inLbPerCubicFoot air_density
        (CubicFeetPerMinute cfm) = model.cfm
        lb_air_per_hour = 60 * cfm * rho
        building_t = inFahrenheit model.building_air.t
        supply_t = inFahrenheit model.supply_air.t
        delta_t = building_t - supply_t
        (BtuPerLbF cp) = specific_heat_air
        load = lb_air_per_hour*cp*delta_t
    in
        BtuPerHour load

-- sensible heat factor is the ratio of cooling supply to heat flow in
supply_shf: Model -> Float
supply_shf model =
    let
        supply = inBtusPerHour <| q_sensible model
        flow = inBtusPerHour <| q_total_in model
    in
        if supply < 0 then
            -111111
                else if flow < 0 then
                         -2222222
                             else
        supply/flow

new_building_t: Model -> Duration -> Temperature
new_building_t model dt =
    let
        current_building_t = inFahrenheit model.building_air.t
        (BtuPerF cp) = building_heat_capacitance
        load = inBtusPerHour model.load
        supply = inBtusPerHour <| q_sensible model
        shf = model.load_shf
        rho = inLbPerCubicFoot air_density
        s = inSeconds dt
    in
        Fahrenheit (current_building_t + s*(load*shf - supply)/cp)

-- enthalpy of water vapor
enthalpy_h2o: Air -> Enthalpy
enthalpy_h2o air =
    let
        t = inFahrenheit air.t
    in
        BTUsPerPound (1061 + 0.444*t)

-- h2o generate by load
new_water_from_building: Model -> Duration -> Mass
new_water_from_building model dt =
    let
        load = inBtusPerHour model.load
        shf = model.load_shf
        (BTUsPerPound h_h2o) = enthalpy_h2o model.building_air
        time = inHours dt
    in
        Lb (time*load*(1-shf)/h_h2o)

-- h2o in from supply air
supply_abs_humidity: Model -> Density
supply_abs_humidity model =
    let
        (CubicFeetPerMinute supply_air_cfm) = model.cfm
        rho = inLbPerCubicFoot air_density
        (MolecularRatio w) = humidity_ratio model.supply_air
        supply_abs_humidity = w * rho * supply_air_cfm
    in
         LbPerCubicFoot supply_abs_humidity

-- h2o in from supply air
new_water_in: Model -> Duration -> Mass
new_water_in model dt =
    let
        (CubicFeetPerMinute supply_air_cfm) = model.cfm
        rho = inLbPerCubicFoot air_density
        (MolecularRatio w) = humidity_ratio model.supply_air
        s = inMinutes dt
    in
        Lb (w*rho*supply_air_cfm*s)

-- h2o flowing out
new_water_out: Model -> Duration -> Mass
new_water_out model dt =
    let
        (CubicFeetPerMinute outflow_air_cfm) = model.cfm
        rho = inLbPerCubicFoot air_density
        (MolecularRatio w) = humidity_ratio model.building_air
        s = inMinutes dt
    in
        Lb (w*rho*outflow_air_cfm*s)

-- rate of change of water in building
-- net water flowing into building
new_building_h2o: Model -> Duration -> Mass
new_building_h2o model dt =
    let
        (Lb h2o_supply) = new_water_in model dt
        (Lb h2o_building) = new_water_from_building model dt
        (Lb h2o_outflow) = new_water_out model dt
    in
        Lb (h2o_supply + h2o_building - h2o_outflow)

latent_energy_change: Model -> Duration -> Power
latent_energy_change model dt =
    let
        -- (CubicFeetPerMinute cfm) = model.cfm
        s = inHours dt
        -- volume_of_air_cf = cfm*s/60
        (Lb lb_water_out) = new_water_out model dt
        (Lb lb_water_in) = new_water_in model dt
        lb_water_net = lb_water_in - lb_water_out

        (BTUsPerPound h_h2o) = enthalpy_h2o model.supply_air

        delta_lat = lb_water_net*h_h2o/s
    in
        BtuPerHour delta_lat

-- rate of change of absolute humidity in building
new_building_abs_humidity: Model -> Duration -> Density
new_building_abs_humidity model dt =
    let
        (Lb delta_h2o) = new_building_h2o model dt
        (CubicFeet v) = building_volume
        abs_humidity = inLbPerCubicFoot <| absolute_humidity model.building_air
        new_h2o = v*abs_humidity + delta_h2o
    in
        LbPerCubicFoot (new_h2o/v)

-- rate of change of relative humidity in building
new_building_rel_humidity: Model -> Duration -> RelativeHumidity
new_building_rel_humidity model dt =
    let
        new_ah = new_building_abs_humidity model dt
        temperature = model.building_air.t
        -- temperature = new_building_t model dt
        new_rh = relative_humidity temperature new_ah
    in
        new_rh


-- new building state after one hour
new_building_air: Model -> Air
new_building_air model =
    let
        dt = timestep
        new_t = new_building_t model dt
        new_rh = new_building_rel_humidity model dt
    in
        { t = new_t
        , rh = new_rh
        }
