namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Informedica.GenUnits.lib")>]
[<assembly: AssemblyProductAttribute("Informedica.GenUnits.Lib")>]
[<assembly: AssemblyCompanyAttribute("halcwb")>]
[<assembly: AssemblyDescriptionAttribute("Basic units of measure library enabling calculating with values that have units")>]
[<assembly: AssemblyVersionAttribute("1.0.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0.0"
    let [<Literal>] InformationalVersion = "1.0.0"
