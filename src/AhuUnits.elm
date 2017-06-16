module AhuUnits exposing (..)

import Time exposing (Time, second)

type AbsoluteHumidity = AirDensity Float
type AirFlow = CubicFeetPerMinute Float | CubicMetersPerSecond Float
type Density = LbPerCubicFoot Float | KgPerCubicMeter Float
type Enthalpy = BTUsPerPound Float
type Energy = BTU Float
type HumidityRatio = MolecularRatio Float
type Mass = Lb Float
type Power = BtuPerHour Float | Tons Float | Kilowatts Float
type Pressure = PSI Float | HectoPascal Float | Pascal Float
type RelativeHumidity = HPercent Float
type RelativeHumidityRate = HPercentPerHour Float
type SpecificGasConstant = JoulePerKgPerK Float
type SpecificHeat = BtuPerLbF Float
type SpecificHeatExtensive = BtuPerF Float
type Temperature = Fahrenheit Float | Celsius Float | Kelvin Float
type Volume = CubicFeet Float
type Duration = Seconds Float | Minutes Float | Hours Float
type alias Percent = Float

inCubicMetersPerSecond: AirFlow -> Float
inCubicMetersPerSecond d = case d of
               (CubicMetersPerSecond cms) -> cms
               (CubicFeetPerMinute cfm) -> cfm*0.000471947

inCubicFeetPerMinute: AirFlow -> Float
inCubicFeetPerMinute d = case d of
               (CubicMetersPerSecond cms) -> cms*2118.88
               (CubicFeetPerMinute cfm) -> cfm

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

inPercent: RelativeHumidity -> Float
inPercent (HPercent p) = p

inKgPerCubicMeter: Density -> Float
inKgPerCubicMeter d = case d of
               (KgPerCubicMeter k) -> k
               (LbPerCubicFoot l) -> l/0.062428

inLbPerCubicFoot: Density -> Float
inLbPerCubicFoot d = case d of
               (KgPerCubicMeter k) -> k*0.062428
               (LbPerCubicFoot l) -> l

inHumidityRatio: HumidityRatio -> Float
inHumidityRatio t = case t of
             (MolecularRatio s) -> s

inSeconds: Duration -> Float
inSeconds t = case t of
             (Seconds s) -> s
             (Minutes s) -> s*60
             (Hours s) -> s*3600

inMinutes: Duration -> Float
inMinutes t = case t of
             (Seconds s) -> s/60
             (Minutes s) -> s
             (Hours s) -> s*60

inHours: Duration -> Float
inHours t = case t of
             (Seconds s) -> s/3600
             (Minutes s) -> s/60
             (Hours s) -> s

inLb: Mass -> Float
inLb p = case p of
             (Lb lb) -> lb

inPSI: Pressure -> Float
inPSI p = case p of
              (HectoPascal hp) -> hp*0.0145038
              (Pascal p) -> p*0.000145038
              (PSI psi) -> psi

inPascals: Pressure -> Float
inPascals p = case p of
               (Pascal p) -> p
               (HectoPascal hp) -> hp*100
               (PSI psi) -> psi*6894.76

inHectoPascals: Pressure -> Float
inHectoPascals p = case p of
               (HectoPascal hp) -> hp
               (Pascal p) -> p/100
               (PSI psi) -> psi*68.9476

inKelvin: Temperature -> Float
inKelvin t = case t of
               (Fahrenheit d) -> (d-32)*5/9 + 273.15
               (Celsius d) -> d + 273.15
               (Kelvin d) -> d

inCelsius: Temperature -> Float
inCelsius t = case t of
               (Fahrenheit d) -> (d-32)*5/9
               (Celsius d) -> d
               (Kelvin d) -> d - 273.15

inFahrenheit: Temperature -> Float
inFahrenheit t = case t of
               (Fahrenheit d) -> d
               (Celsius d) -> d*9/5 + 32
               (Kelvin d) -> (d-273.15)*9/5 + 32

type SystemOfUnits = Metric | Imperial

inUnits: SystemOfUnits -> Temperature -> Float
inUnits sys temperature = case sys of
                               Metric -> inCelsius temperature
                               Imperial -> inFahrenheit temperature

inUnitsString: SystemOfUnits -> Temperature -> String
inUnitsString sys temperature = case sys of
                               Metric -> (inCelsius temperature |> roundn 1 |> toString) ++ "(°C)"
                               Imperial -> (inFahrenheit temperature |> roundn 1 |> toString) ++ "(°F)"

inUnitsPower: SystemOfUnits -> Power -> Float
inUnitsPower sys power = case sys of
                               Metric -> inKilowatts power
                               Imperial -> inTons power

inUnitsPowerString: SystemOfUnits -> Power -> String
inUnitsPowerString sys power = case sys of
                               Metric -> (inKilowatts power |> roundn 1 |> toString) ++ "(kW)"
                               Imperial -> (inTons power |> roundn 1 |> toString) ++ " (tons)"

inUnitsAirflow: SystemOfUnits -> AirFlow -> Float
inUnitsAirflow sys airflow = case sys of
                               Metric -> inCubicMetersPerSecond airflow
                               Imperial -> inCubicFeetPerMinute airflow

inUnitsAirflowString: SystemOfUnits -> AirFlow -> String
inUnitsAirflowString sys airflow = case sys of
                               Metric -> (inCubicMetersPerSecond airflow |> roundn 1 |> toString) ++ "(cubic m/s)"
                               Imperial -> (inCubicFeetPerMinute airflow |> roundn 1 |> toString) ++ " (cfm)"
inKilowatts: Power -> Float
inKilowatts p = case p of
               (BtuPerHour b) -> b*0.00029307107
               (Kilowatts kw) -> kw
               (Tons t) -> t*3.5168

inTons: Power -> Float
inTons p = case p of
               (BtuPerHour b) -> (b/12000)
               (Kilowatts kw) -> kw/3.5168525
               (Tons t) -> t

inBtusPerHour: Power -> Float
inBtusPerHour p = case p of
               (BtuPerHour b) -> b
               (Kilowatts kw) -> kw*3412.142
               (Tons t) -> t*12000

inBtusPerLb: Enthalpy -> Float
inBtusPerLb p = case p of
               (BTUsPerPound b) -> b

toTime: String -> Time
toTime s = case String.toFloat s of
               Ok f -> f
               Err m -> 0.0

toTons: String -> Power
toTons s = case String.toFloat s of
               Ok f -> Tons f
               Err m -> Tons 0.0

toHPercent: String -> RelativeHumidity
toHPercent s = case String.toFloat s of
               Ok f -> HPercent f
               Err m -> HPercent 0.0
