
#I __SOURCE_DIRECTORY__

#r "nuget: MathNet.Numerics"
#r "nuget: Informedica.Utils.Lib"
#r "nuget: FParsec"

#load @"../ValueUnit.fs"
#load @"../Api.fs"

open MathNet.Numerics

open Informedica.GenUnits.Lib
open Informedica.Utils.Lib.BCL

Api.eval "1 mg[Mass] / 1 piece[General]"
|> (/) (Api.eval "1 mg[Mass] / 1 mg[Mass]")
|> ValueUnit.toString ValueUnit.Units.English ValueUnit.Units.Verbal.Short

Api.eval "100 mg[Mass] * 200 mg[Mass] / 5 ml[Volume] / 10 times[Count]"
|> ValueUnit.toString ValueUnit.Units.English ValueUnit.Units.Verbal.Short


