module AhuUnits exposing (..)

import Time exposing (Time, second)

type AbsoluteHumidity = AirDensity Float
type AirFlow = CubicFeetPerMinute Float
type Density = LbPerCubicFoot Float | KgPerCubicMeter Float
type Enthalpy = BTUsPerPound Float
type Energy = BTU Float
type HumidityRatio = MolecularRatio Float
type Mass = Lb Float
type Power = BtuPerHour Float | Tons Float
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

inTons: Power -> Float
inTons p = case p of
               (BtuPerHour b) -> (b/12000)
               (Tons t) -> t

inBtusPerLb: Enthalpy -> Float
inBtusPerLb p = case p of
               (BTUsPerPound b) -> b

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
